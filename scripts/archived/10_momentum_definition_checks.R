#############################################################################
# Badminton: Checks of the Four Momentum Definitions
#
# PURPOSE:
# This script investigates why the momentum definitions may produce similar
# results. It performs three checks:
#
#   1. Calculate correlations among the three point-based momentum measures:
#      square-root, score-difference-weighted, and time-weighted momentum.
#
#   2. Compare all four momentum definitions with the focal game's ordinary
#      final score differential (Team 1 score - Team 2 score), using both
#      correlations and scatterplots.
#
#   3. Repeat the out-of-sample testing from Script 09 while adding a
#      controls-only model:
#
#        won_next_game ~ elo_difference_100 + tournament_type + round
#
#      This shows whether adding momentum improves prediction beyond the
#      control variables alone.
#
# DATA:
#   data/processed/ws_with_elo.csv
#   data/processed/ms_with_elo.csv
#
# The script examines women's singles (WS) and men's singles (MS), for both
# Game 1 -> Game 2 and Game 2 -> Game 3.
#
# OUTPUTS:
#   data/momentum_checks/point_based_momentum_correlations.csv
#   data/momentum_checks/momentum_vs_final_score_diff.csv
#   data/momentum_checks/controls_only_comparison.csv
#   data/momentum_checks/momentum_check_values.csv
#   data/momentum_checks/plots/*.png
#
# INTERPRETING THE CONTROLS-ONLY COMPARISON:
#   - Lower RMSE is better.
#   - Higher positive Pearson correlation is better.
#   - A negative RMSE change means that adding momentum improved RMSE.
#   - A positive correlation change means that adding momentum improved the
#     correlation.
#############################################################################

library(tidyverse)

# Settings used in Scripts 05-08
d_weight <- 0.25
c_weight <- 0.5

# Use the same out-of-sample settings as Script 09
test_proportion <- 0.20
random_seed <- 123

data_files <- c(
  WS = "data/processed/ws_with_elo.csv",
  MS = "data/processed/ms_with_elo.csv"
)

output_folder <- "data/momentum_checks"
plot_folder <- file.path(output_folder, "plots")

dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_folder, recursive = TRUE, showWarnings = FALSE)


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
      time_weighted = NA_real_,
      final_score_diff = NA_real_
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
      point_sign <- 1
      streak_length <- p1_streak
      
    } else if (p2_score[i] > p2_score[i - 1] &&
               p1_score[i] == p1_score[i - 1]) {
      
      # Team 2 won this rally.
      p2_streak <- p2_streak + 1L
      p1_streak <- 0L
      point_sign <- -1
      streak_length <- p2_streak
      
    } else {
      # Skip a malformed score transition.
      next
    }
    
    p1_max_streak <- max(p1_max_streak, p1_streak)
    p2_max_streak <- max(p2_max_streak, p2_streak)
    
    base_gain <- 1 + sqrt(streak_length)
    
    # Definition 2: square-root momentum
    sqrt_momentum <- sqrt_momentum + point_sign * base_gain
    
    # Definition 3: score-difference weighting
    score_gap <- abs(p1_score[i] - p2_score[i])
    closeness_weight <- 1 + d / (1 + score_gap)
    score_diff_momentum <- score_diff_momentum +
      point_sign * closeness_weight * base_gain
    
    # Definition 4: time weighting
    points_completed <- p1_score[i] + p2_score[i]
    game_progress <- min((points_completed - 1) / 40, 1)
    time_weight <- 1 + c * game_progress
    time_momentum <- time_momentum +
      point_sign * time_weight * base_gain
  }
  
  return(c(
    max_streak = p1_max_streak - p2_max_streak,
    sqrt = sqrt_momentum,
    score_diff = score_diff_momentum,
    time_weighted = time_momentum,
    final_score_diff =
      p1_score[length(p1_score)] - p2_score[length(p2_score)]
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
      momentum_time_weighted = momentum_values[["time_weighted"]],
      final_score_diff = momentum_values[["final_score_diff"]]
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
      !is.na(final_score_diff),
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
      final_score_diff,
      momentum_max_streak,
      momentum_sqrt,
      momentum_score_diff,
      momentum_time_weighted
    )
  
  return(analysis_data)
}


# ---------------------------------------------------------------------------
# 3. Make the same type of 80/20 split used in Script 09
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
# 5. Run the three checks
# ---------------------------------------------------------------------------

momentum_columns <- c(
  max_streak = "momentum_max_streak",
  sqrt = "momentum_sqrt",
  score_diff = "momentum_score_diff",
  time_weighted = "momentum_time_weighted"
)

# These are the three measures that use information from every rally.
point_based_columns <- c(
  sqrt = "momentum_sqrt",
  score_diff = "momentum_score_diff",
  time_weighted = "momentum_time_weighted"
)

all_values <- list()
all_point_correlations <- list()
all_score_diff_correlations <- list()
all_performance <- list()

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
    
    model_data <- model_data %>%
      mutate(
        discipline = discipline,
        focal_game = focal_game,
        next_game = next_game,
        .before = 1
      )
    
    all_values[[result_number]] <- model_data
    
    
    # CHECK 1: correlations among the three point-based measures
    point_correlation_matrix <- cor(
      model_data[, point_based_columns],
      use = "complete.obs"
    )
    
    point_correlations <- as.data.frame(as.table(point_correlation_matrix)) %>%
      as_tibble() %>%
      rename(
        momentum_definition_1 = Var1,
        momentum_definition_2 = Var2,
        correlation = Freq
      ) %>%
      mutate(
        discipline = discipline,
        focal_game = focal_game,
        next_game = next_game,
        .before = 1
      ) %>%
      # Keep each pair once and remove correlations of a variable with itself.
      filter(
        as.numeric(momentum_definition_1) <
          as.numeric(momentum_definition_2)
      )
    
    all_point_correlations[[result_number]] <- point_correlations
    
    
    # CHECK 2: relationship between each definition and final score margin
    score_diff_correlations <- map_dfr(
      names(momentum_columns),
      function(momentum_name) {
        momentum_column <- momentum_columns[[momentum_name]]
        
        tibble(
          momentum_definition = momentum_name,
          correlation_with_final_score_diff = cor(
            model_data[[momentum_column]],
            model_data$final_score_diff,
            use = "complete.obs"
          )
        )
      }
    ) %>%
      mutate(
        discipline = discipline,
        focal_game = focal_game,
        next_game = next_game,
        n_matches = nrow(model_data),
        .before = 1
      )
    
    all_score_diff_correlations[[result_number]] <-
      score_diff_correlations
    
    plot_data <- model_data %>%
      # Remove the helper vector's labels so select() keeps the original
      # momentum_* column names instead of renaming them.
      select(final_score_diff, all_of(unname(momentum_columns))) %>%
      pivot_longer(
        cols = all_of(unname(momentum_columns)),
        names_to = "momentum_definition",
        values_to = "momentum_value"
      ) %>%
      mutate(
        momentum_definition = recode(
          momentum_definition,
          momentum_max_streak = "Maximum streak",
          momentum_sqrt = "Square-root",
          momentum_score_diff = "Score-difference weighted",
          momentum_time_weighted = "Time weighted"
        )
      )
    
    momentum_plot <- ggplot(
      plot_data,
      aes(x = final_score_diff, y = momentum_value)
    ) +
      geom_point(alpha = 0.20, color = "steelblue") +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      facet_wrap(~ momentum_definition, scales = "free_y") +
      labs(
        title = paste0(
          discipline, ": Game ", focal_game,
          " momentum vs. final score differential"
        ),
        x = "Final score differential (Team 1 - Team 2)",
        y = "Momentum value"
      ) +
      theme_minimal()
    
    ggsave(
      filename = file.path(
        plot_folder,
        paste0(
          tolower(discipline), "_game", focal_game,
          "_momentum_vs_final_score_diff.png"
        )
      ),
      plot = momentum_plot,
      width = 10,
      height = 7,
      dpi = 300
    )
    
    
    # CHECK 3: compare momentum models with a controls-only model
    split_seed <- random_seed +
      ifelse(discipline == "MS", 100, 0) +
      focal_game
    
    split_data <- make_oos_split(model_data, split_seed)
    
    training_data <- split_data %>% filter(sample == "train")
    test_data <- split_data %>% filter(sample == "test")
    
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
    
    controls_only_model <- glm(
      won_next_game ~ elo_difference_100 + tournament_type + round,
      data = training_data,
      family = binomial
    )
    
    controls_only_predictions <- predict(
      controls_only_model,
      newdata = test_data,
      type = "response"
    )
    
    controls_only_performance <- calculate_metrics(
      actual = test_data$won_next_game,
      predicted_probability = controls_only_predictions
    ) %>%
      mutate(
        discipline = discipline,
        focal_game = focal_game,
        next_game = next_game,
        model = "controls_only",
        n_train = nrow(training_data),
        n_test = nrow(test_data),
        .before = 1
      )
    
    comparison_performance <- list(controls_only_performance)
    
    for (momentum_name in names(momentum_columns)) {
      
      momentum_column <- momentum_columns[[momentum_name]]
      
      training_model_data <- training_data %>%
        mutate(momentum = .data[[momentum_column]])
      
      test_model_data <- test_data %>%
        mutate(momentum = .data[[momentum_column]])
      
      momentum_model <- glm(
        won_next_game ~ momentum + elo_difference_100 +
          tournament_type + round,
        data = training_model_data,
        family = binomial
      )
      
      momentum_predictions <- predict(
        momentum_model,
        newdata = test_model_data,
        type = "response"
      )
      
      comparison_performance[[length(comparison_performance) + 1]] <-
        calculate_metrics(
          actual = test_model_data$won_next_game,
          predicted_probability = momentum_predictions
        ) %>%
        mutate(
          discipline = discipline,
          focal_game = focal_game,
          next_game = next_game,
          model = momentum_name,
          n_train = nrow(training_model_data),
          n_test = nrow(test_model_data),
          .before = 1
        )
    }
    
    comparison_performance <- bind_rows(comparison_performance) %>%
      mutate(
        rmse_change_vs_controls_only =
          rmse - controls_only_performance$rmse,
        correlation_change_vs_controls_only =
          pearson_correlation -
          controls_only_performance$pearson_correlation
      )
    
    all_performance[[result_number]] <- comparison_performance
    
    cat(
      "\n", discipline, ": Game ", focal_game, " -> Game ", next_game,
      "\nMatches: ", nrow(model_data),
      "\nTraining matches: ", nrow(training_data),
      "\nTest matches: ", nrow(test_data), "\n",
      sep = ""
    )
    
    result_number <- result_number + 1
  }
}


# ---------------------------------------------------------------------------
# 6. Combine and save the results
# ---------------------------------------------------------------------------

momentum_check_values <- bind_rows(all_values) %>%
  arrange(discipline, focal_game, original_row)

point_based_momentum_correlations <- bind_rows(all_point_correlations) %>%
  arrange(
    discipline, focal_game,
    momentum_definition_1, momentum_definition_2
  )

momentum_vs_final_score_diff <- bind_rows(
  all_score_diff_correlations
) %>%
  arrange(discipline, focal_game, momentum_definition)

controls_only_comparison <- bind_rows(all_performance) %>%
  arrange(discipline, focal_game, model)

write.csv(
  point_based_momentum_correlations,
  file.path(output_folder, "point_based_momentum_correlations.csv"),
  row.names = FALSE
)

write.csv(
  momentum_vs_final_score_diff,
  file.path(output_folder, "momentum_vs_final_score_diff.csv"),
  row.names = FALSE
)

write.csv(
  controls_only_comparison,
  file.path(output_folder, "controls_only_comparison.csv"),
  row.names = FALSE
)

write.csv(
  momentum_check_values,
  file.path(output_folder, "momentum_check_values.csv"),
  row.names = FALSE
)

cat("\n=== CORRELATIONS AMONG POINT-BASED MOMENTUM DEFINITIONS ===\n")
print(point_based_momentum_correlations)

cat("\n=== MOMENTUM VS. FINAL SCORE DIFFERENTIAL ===\n")
print(momentum_vs_final_score_diff)

cat("\n=== CONTROLS-ONLY COMPARISON ===\n")
print(controls_only_comparison)

cat(
  "\nResults saved in:\n",
  "  data/momentum_checks/point_based_momentum_correlations.csv\n",
  "  data/momentum_checks/momentum_vs_final_score_diff.csv\n",
  "  data/momentum_checks/controls_only_comparison.csv\n",
  "  data/momentum_checks/momentum_check_values.csv\n",
  "  data/momentum_checks/plots/\n"
)
