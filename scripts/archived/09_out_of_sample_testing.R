#############################################################################
# Badminton: Out-of-Sample Testing for All Four Momentum Definitions
#
# PURPOSE:
# This script evaluates how well the controlled logistic regression models
# predict matches that were not used to fit the models.
#
# DATA:
#   data/processed/ws_with_elo.csv
#   data/processed/ms_with_elo.csv
#
# MODELS:
# The script evaluates both Game 1 -> Game 2 and Game 2 -> Game 3 for:
#   1. maximum-streak difference;
#   2. square-root momentum;
#   3. score-difference-weighted momentum; and
#   4. time-weighted momentum.
#
# Every model controls for pre-match Elo difference, tournament type, and
# round. There are 16 models total:
#   2 disciplines x 2 game transitions x 4 momentum definitions.
#
# OUT-OF-SAMPLE METHOD:
# Within each discipline and game transition, matches are randomly divided
# into 80% training data and 20% test data. The split is stratified jointly by
# the next-game result and tournament type, and it is reproducible because a
# seed is set below. The script also checks that every round category in the
# test data appears in the training data. All four momentum definitions use
# the same matches and split for a fair comparison.
#
# Following the approach in the example tennis paper, performance is summarized
# using:
#   1. RMSE between predicted win probabilities and actual results (0 or 1);
#   2. Pearson correlation between predicted probabilities and actual results.
#
# Lower RMSE and higher positive correlation indicate better predictions.
#
# OUTPUTS:
#   data/oos/oos_performance_summary.csv
#   data/oos/oos_predictions.csv
#############################################################################

library(tidyverse)

# Settings used in Scripts 05-08
d_weight <- 0.25
c_weight <- 0.5

# Settings for the out-of-sample split
test_proportion <- 0.20
random_seed <- 123

data_files <- c(
  WS = "data/processed/ws_with_elo.csv",
  MS = "data/processed/ms_with_elo.csv"
)

dir.create("data/oos", recursive = TRUE, showWarnings = FALSE)


# ---------------------------------------------------------------------------
# 1. Helper functions for the four momentum definitions
# ---------------------------------------------------------------------------

# Turn "['0-0', '1-0', '1-1']" into c("0-0", "1-0", "1-1").
parse_score_list <- function(x) {
  if (is.na(x) || x == "" || x == "[]") {
    return(character(0))
  }
  
  str_extract_all(x, "\\d+-\\d+")[[1]]
}

# Calculate all four momentum definitions in one pass through a game.
# Every measure is from Team 1's perspective, so positive values favor Team 1.
compute_all_momentum <- function(score_vec, d, c) {
  if (length(score_vec) < 2) {
    return(c(
      max_streak = NA_real_,
      sqrt = NA_real_,
      score_diff = NA_real_,
      time_weighted = NA_real_
    ))
  }
  
  score_parts <- str_split_fixed(score_vec, "-", 2)
  p1_score <- as.integer(score_parts[, 1])
  p2_score <- as.integer(score_parts[, 2])
  
  p1_streak <- 0L
  p2_streak <- 0L
  p1_max_streak <- 0L
  p2_max_streak <- 0L
  
  sqrt_momentum <- 0
  score_diff_momentum <- 0
  time_momentum <- 0
  
  for (i in 2:length(p1_score)) {
    
    if (p1_score[i] > p1_score[i - 1] &&
        p2_score[i] == p2_score[i - 1]) {
      
      # Team 1 won this rally.
      p1_streak <- p1_streak + 1L
      p2_streak <- 0L
      sign <- 1
      streak_length <- p1_streak
      
    } else if (p2_score[i] > p2_score[i - 1] &&
               p1_score[i] == p1_score[i - 1]) {
      
      # Team 2 won this rally.
      p2_streak <- p2_streak + 1L
      p1_streak <- 0L
      sign <- -1
      streak_length <- p2_streak
      
    } else {
      # Skip a malformed score transition.
      next
    }
    
    p1_max_streak <- max(p1_max_streak, p1_streak)
    p2_max_streak <- max(p2_max_streak, p2_streak)
    
    base_gain <- 1 + sqrt(streak_length)
    
    # Definition 2: square-root momentum
    sqrt_momentum <- sqrt_momentum + sign * base_gain
    
    # Definition 3: score-difference weighting
    score_gap <- abs(p1_score[i] - p2_score[i])
    closeness_weight <- 1 + d / (1 + score_gap)
    score_diff_momentum <- score_diff_momentum +
      sign * closeness_weight * base_gain
    
    # Definition 4: time weighting
    points_completed <- p1_score[i] + p2_score[i]
    game_progress <- min((points_completed - 1) / 40, 1)
    time_weight <- 1 + c * game_progress
    time_momentum <- time_momentum + sign * time_weight * base_gain
  }
  
  return(c(
    max_streak = p1_max_streak - p2_max_streak,
    sqrt = sqrt_momentum,
    score_diff = score_diff_momentum,
    time_weighted = time_momentum
  ))
}


# ---------------------------------------------------------------------------
# 2. Prepare one discipline and one game transition
# ---------------------------------------------------------------------------

prepare_analysis_data <- function(raw_data, focal_game, next_game) {
  
  point_column <- paste0("game_", focal_game, "_scores")
  outcome_column <- paste0("game_", next_game, "_score")
  
  analysis_data <- raw_data %>%
    mutate(original_row = row_number()) %>%
    filter(tolower(retired) != "true")
  
  # For Game 2 -> Game 3, require evidence that Game 3 was actually played.
  if (next_game == 3) {
    analysis_data <- analysis_data %>%
      filter(
        !is.na(game_3_score), game_3_score != "",
        !is.na(game_3_scores), game_3_scores != "[]"
      )
  }
  
  analysis_data <- analysis_data %>%
    rowwise() %>%
    mutate(
      point_list = list(parse_score_list(.data[[point_column]])),
      momentum_values = list(
        compute_all_momentum(point_list, d_weight, c_weight)
      ),
      momentum_max_streak = momentum_values[["max_streak"]],
      momentum_sqrt = momentum_values[["sqrt"]],
      momentum_score_diff = momentum_values[["score_diff"]],
      momentum_time_weighted = momentum_values[["time_weighted"]]
    ) %>%
    ungroup()
  
  # Read the next game's final score and define whether Team 1 won.
  next_scores <- str_split_fixed(analysis_data[[outcome_column]], "-", 2)
  analysis_data$next_score_one <- as.numeric(next_scores[, 1])
  analysis_data$next_score_two <- as.numeric(next_scores[, 2])
  
  analysis_data <- analysis_data %>%
    mutate(
      won_next_game = ifelse(
        !is.na(next_score_one) &
          !is.na(next_score_two) &
          next_score_one != next_score_two,
        as.integer(next_score_one > next_score_two),
        NA
      ),
      elo_difference_100 =
        (team_one_pre_match_elo - team_two_pre_match_elo) / 100
    ) %>%
    filter(
      !is.na(won_next_game),
      !is.na(momentum_max_streak),
      !is.na(momentum_sqrt),
      !is.na(momentum_score_diff),
      !is.na(momentum_time_weighted),
      !is.na(elo_difference_100),
      !is.na(tournament_type), tournament_type != "",
      !is.na(round), round != ""
    ) %>%
    mutate(
      tournament_type = factor(tournament_type),
      round = factor(round)
    ) %>%
    select(
      original_row, date, team_one_players, team_two_players,
      won_next_game, elo_difference_100, tournament_type, round,
      starts_with("momentum_")
    )
  
  return(analysis_data)
}


# ---------------------------------------------------------------------------
# 3. Make one shared, stratified training/test split
# ---------------------------------------------------------------------------

make_oos_split <- function(data, seed) {
  set.seed(seed)
  
  test_rows <- data %>%
    group_by(won_next_game, tournament_type) %>%
    slice_sample(prop = test_proportion) %>%
    ungroup() %>%
    pull(original_row)
  
  data %>%
    mutate(sample = ifelse(original_row %in% test_rows, "test", "train"))
}


# ---------------------------------------------------------------------------
# 4. Out-of-sample evaluation measures
# ---------------------------------------------------------------------------

calculate_metrics <- function(actual, predicted_probability) {
  tibble(
    rmse = sqrt(mean((predicted_probability - actual)^2)),
    pearson_correlation = cor(predicted_probability, actual)
  )
}


# ---------------------------------------------------------------------------
# 5. Fit and test all 16 controlled models
# ---------------------------------------------------------------------------

momentum_columns <- c(
  max_streak = "momentum_max_streak",
  sqrt = "momentum_sqrt",
  score_diff = "momentum_score_diff",
  time_weighted = "momentum_time_weighted"
)

all_performance <- list()
all_predictions <- list()
result_number <- 1

for (discipline in names(data_files)) {
  
  raw_data <- read.csv(
    data_files[[discipline]],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  for (focal_game in c(1, 2)) {
    
    next_game <- focal_game + 1
    
    model_data <- prepare_analysis_data(
      raw_data = raw_data,
      focal_game = focal_game,
      next_game = next_game
    )
    
    # Use a different reproducible split for each discipline/transition.
    split_seed <- random_seed +
      ifelse(discipline == "MS", 100, 0) +
      focal_game
    
    model_data <- make_oos_split(model_data, split_seed)
    
    training_data <- model_data %>% filter(sample == "train")
    test_data <- model_data %>% filter(sample == "test")
    
    # Prediction requires every test-set category to have been seen in
    # training. Tournament type is protected by the stratified split above;
    # here, check the round categories without making the split too complex.
    missing_rounds <- setdiff(
      unique(as.character(test_data$round)),
      unique(as.character(training_data$round))
    )
    
    if (length(missing_rounds) > 0) {
      stop(
        discipline, ": Game ", focal_game, " -> Game ", next_game,
        " has round categories in the test data but not the training data: ",
        paste(missing_rounds, collapse = ", "),
        ". Try a different random_seed."
      )
    }
    
    cat(
      "\n", discipline, ": Game ", focal_game, " -> Game ", next_game,
      "\nTraining matches: ", nrow(training_data),
      "\nTest matches: ", nrow(test_data), "\n",
      sep = ""
    )
    
    for (momentum_name in names(momentum_columns)) {
      
      momentum_column <- momentum_columns[[momentum_name]]
      
      # Rename the selected definition so all models can use one formula.
      training_model_data <- training_data %>%
        mutate(momentum = .data[[momentum_column]])
      
      test_model_data <- test_data %>%
        mutate(momentum = .data[[momentum_column]])
      
      controlled_model <- glm(
        won_next_game ~ momentum + elo_difference_100 +
          tournament_type + round,
        data = training_model_data,
        family = binomial
      )
      
      predicted_probability <- predict(
        controlled_model,
        newdata = test_model_data,
        type = "response"
      )
      
      performance <- calculate_metrics(
        actual = test_model_data$won_next_game,
        predicted_probability = predicted_probability
      ) %>%
        mutate(
          discipline = discipline,
          focal_game = focal_game,
          next_game = next_game,
          momentum_definition = momentum_name,
          n_train = nrow(training_model_data),
          n_test = nrow(test_model_data),
          .before = 1
        )
      
      predictions <- test_model_data %>%
        transmute(
          discipline = discipline,
          focal_game = focal_game,
          next_game = next_game,
          momentum_definition = momentum_name,
          original_row,
          date,
          team_one_players,
          team_two_players,
          actual_result = won_next_game,
          predicted_probability
        )
      
      all_performance[[result_number]] <- performance
      all_predictions[[result_number]] <- predictions
      result_number <- result_number + 1
    }
  }
}


# ---------------------------------------------------------------------------
# 6. Combine and save results
# ---------------------------------------------------------------------------

performance_summary <- bind_rows(all_performance) %>%
  arrange(discipline, focal_game, momentum_definition)

oos_predictions <- bind_rows(all_predictions) %>%
  arrange(
    discipline, focal_game, momentum_definition, original_row
  )

write.csv(
  performance_summary,
  "data/oos/oos_performance_summary.csv",
  row.names = FALSE
)

write.csv(
  oos_predictions,
  "data/oos/oos_predictions.csv",
  row.names = FALSE
)

cat("\n=== OUT-OF-SAMPLE PERFORMANCE SUMMARY ===\n")
print(performance_summary)

cat(
  "\nResults saved in:\n",
  "  data/oos/oos_performance_summary.csv\n",
  "  data/oos/oos_predictions.csv\n"
)
