#############################################################################
# Badminton: New Momentum Definitions
#
# PURPOSE:
# The earlier point-based momentum definitions were almost identical to the
# focal game's final score differential. This script separates ordinary game
# dominance from rally-sequence momentum.
#
# For women's singles (WS) and men's singles (MS), and for both Game 1 -> Game 2
# and Game 2 -> Game 3, the script compares:
#
#   1. Controls only
#   2. Controls + final score differential
#   3. Controls + final score differential + maximum-streak differential
#   4. Controls + final score differential + streak-bonus momentum
#   5. Controls + final score differential + adjusted late-game momentum
#
# All momentum variables use Team 1 - Team 2, so positive values favor Team 1.
# The outcome is 1 when Team 1 wins the next game and 0 otherwise.
#
# CONTROLS:
#   - Pre-match Elo difference, measured per 100 Elo points
#   - Tournament type
#   - Round
#
# NEW MOMENTUM DEFINITIONS:
#
#   Final score differential:
#     Team 1's final score - Team 2's final score.
#
#   Maximum-streak differential:
#     Team 1's longest streak - Team 2's longest streak.
#
#   Streak-bonus momentum:
#     For each rally, the winning team receives sqrt(r) - 1, where r is the
#     current length of its streak. The first point in a streak receives zero,
#     so isolated points do not automatically count as momentum.
#
#   Adjusted late-game momentum:
#     Team 1 - Team 2 point differential in the final k rallies, minus the
#     expected late differential based on the whole game's point differential:
#
#       late differential - (k / total rallies) * final score differential
#
#     By default, k = 10. A positive value means Team 1 performed better late
#     in the game than its overall performance would suggest.
#
# QUICK CHECKS:
#   - Correlations among the four focal-game predictors
#   - Out-of-sample RMSE and Pearson correlation for the five models
#
# OUTPUTS:
#   data/new_momentum/model_coefficients.csv
#   data/new_momentum/model_comparison.csv
#   data/new_momentum/predictor_correlations.csv
#   data/new_momentum/oos_model_comparison.csv
#   data/new_momentum/analysis_values.csv
#   data/new_momentum/model_summaries/*.txt
#   data/new_momentum/models/*.rds
#############################################################################

library(tidyverse)


# ---------------------------------------------------------------------------
# 1. Settings
# ---------------------------------------------------------------------------

# Number of rallies used for adjusted late-game momentum
late_game_rallies <- 10

# Settings for the quick out-of-sample check
test_proportion <- 0.20
random_seed <- 123

data_files <- c(
  WS = "data/processed/ws_with_elo.csv",
  MS = "data/processed/ms_with_elo.csv"
)

output_folder <- "data/new_momentum"
summary_folder <- file.path(output_folder, "model_summaries")
model_folder <- file.path(output_folder, "models")

dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(summary_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(model_folder, recursive = TRUE, showWarnings = FALSE)


# ---------------------------------------------------------------------------
# 2. Helper functions for the four focal-game predictors
# ---------------------------------------------------------------------------

# Turn "['0-0', '1-0', '1-1']" into c("0-0", "1-0", "1-1").
parse_score_list <- function(x) {
  if (is.na(x) || x == "" || x == "[]") {
    return(character(0))
  }
  
  str_extract_all(x, "\\d+-\\d+")[[1]]
}


# Calculate all four predictors in one pass through a game.
compute_new_momentum <- function(score_vec, number_late_rallies) {
  if (length(score_vec) < 2) {
    return(c(
      final_score_diff = NA_real_,
      max_streak_diff = NA_real_,
      streak_bonus = NA_real_,
      adjusted_late = NA_real_
    ))
  }
  
  score_parts <- str_split_fixed(score_vec, "-", 2)
  p1_score <- as.integer(score_parts[, 1])
  p2_score <- as.integer(score_parts[, 2])
  
  p1_streak <- 0L
  p2_streak <- 0L
  p1_max_streak <- 0L
  p2_max_streak <- 0L
  
  streak_bonus_momentum <- 0
  rally_signs <- numeric(0)
  
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
    
    # The first point of a streak receives sqrt(1) - 1 = 0.
    streak_bonus_momentum <- streak_bonus_momentum +
      point_sign * (sqrt(streak_length) - 1)
    
    rally_signs <- c(rally_signs, point_sign)
  }
  
  if (length(rally_signs) == 0) {
    return(c(
      final_score_diff = NA_real_,
      max_streak_diff = NA_real_,
      streak_bonus = NA_real_,
      adjusted_late = NA_real_
    ))
  }
  
  final_score_diff <- sum(rally_signs)
  k <- min(number_late_rallies, length(rally_signs))
  late_score_diff <- sum(tail(rally_signs, k))
  
  adjusted_late_momentum <- late_score_diff -
    (k / length(rally_signs)) * final_score_diff
  
  return(c(
    final_score_diff = final_score_diff,
    max_streak_diff = p1_max_streak - p2_max_streak,
    streak_bonus = streak_bonus_momentum,
    adjusted_late = adjusted_late_momentum
  ))
}


# ---------------------------------------------------------------------------
# 3. Prepare one discipline and one game transition
# ---------------------------------------------------------------------------

prepare_analysis_data <- function(raw_data, focal_game, next_game) {
  
  point_column <- paste0("game_", focal_game, "_scores")
  outcome_column <- paste0("game_", next_game, "_score")
  
  analysis_data <- raw_data %>%
    mutate(original_row = row_number()) %>%
    filter(tolower(retired) != "true")
  
  # For Game 2 -> Game 3, require evidence that Game 3 was played.
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
      new_momentum_values = list(
        compute_new_momentum(point_list, late_game_rallies)
      ),
      final_score_diff =
        new_momentum_values[["final_score_diff"]],
      max_streak_diff =
        new_momentum_values[["max_streak_diff"]],
      streak_bonus_momentum =
        new_momentum_values[["streak_bonus"]],
      adjusted_late_game_momentum =
        new_momentum_values[["adjusted_late"]]
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
      !is.na(final_score_diff),
      !is.na(max_streak_diff),
      !is.na(streak_bonus_momentum),
      !is.na(adjusted_late_game_momentum),
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
      final_score_diff, max_streak_diff,
      streak_bonus_momentum, adjusted_late_game_momentum
    )
  
  return(analysis_data)
}


# ---------------------------------------------------------------------------
# 4. Model formulas
# ---------------------------------------------------------------------------

model_formulas <- list(
  controls_only =
    won_next_game ~ elo_difference_100 + tournament_type + round,
  
  final_score_diff =
    won_next_game ~ final_score_diff + elo_difference_100 +
      tournament_type + round,
  
  max_streak_diff =
    won_next_game ~ final_score_diff + max_streak_diff +
      elo_difference_100 + tournament_type + round,
  
  streak_bonus =
    won_next_game ~ final_score_diff + streak_bonus_momentum +
      elo_difference_100 + tournament_type + round,
  
  adjusted_late_game =
    won_next_game ~ final_score_diff + adjusted_late_game_momentum +
      elo_difference_100 + tournament_type + round
)

model_labels <- c(
  controls_only = "Controls only",
  final_score_diff = "Controls + final score differential",
  max_streak_diff =
    "Controls + final score differential + maximum-streak differential",
  streak_bonus =
    "Controls + final score differential + streak-bonus momentum",
  adjusted_late_game =
    "Controls + final score differential + adjusted late-game momentum"
)


# ---------------------------------------------------------------------------
# 5. Helpers for model outputs
# ---------------------------------------------------------------------------

# Convert summary(model) coefficients into an ordinary data frame.
extract_coefficients <- function(model, model_name) {
  coefficient_table <- as.data.frame(summary(model)$coefficients)
  coefficient_table$term <- rownames(coefficient_table)
  rownames(coefficient_table) <- NULL
  
  as_tibble(coefficient_table) %>%
    rename(
      estimate = Estimate,
      standard_error = `Std. Error`,
      z_value = `z value`,
      p_value = `Pr(>|z|)`
    ) %>%
    mutate(
      model = model_name,
      model_description = unname(model_labels[[model_name]]),
      odds_ratio = exp(estimate),
      .before = 1
    )
}


# Compare two nested logistic regression models.
get_lrt_p_value <- function(smaller_model, larger_model) {
  test_result <- anova(smaller_model, larger_model, test = "Chisq")
  as.numeric(test_result$`Pr(>Chi)`[2])
}


# Avoid errors if a variable happens to have no variation.
safe_cor <- function(x, y) {
  if (length(x) < 2 || sd(x) == 0 || sd(y) == 0) {
    return(NA_real_)
  }
  
  cor(x, y, use = "complete.obs")
}


# ---------------------------------------------------------------------------
# 6. Helpers for the quick out-of-sample check
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


calculate_metrics <- function(actual, predicted_probability) {
  tibble(
    rmse = sqrt(mean((predicted_probability - actual)^2)),
    pearson_correlation = safe_cor(predicted_probability, actual)
  )
}


# ---------------------------------------------------------------------------
# 7. Fit the models and run the quick checks
# ---------------------------------------------------------------------------

predictor_columns <- c(
  final_score_diff = "final_score_diff",
  max_streak_diff = "max_streak_diff",
  streak_bonus = "streak_bonus_momentum",
  adjusted_late_game = "adjusted_late_game_momentum"
)

all_analysis_values <- list()
all_coefficients <- list()
all_model_comparisons <- list()
all_predictor_correlations <- list()
all_oos_results <- list()

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
    ) %>%
      mutate(
        discipline = discipline,
        focal_game = focal_game,
        next_game = next_game,
        .before = 1
      )
    
    all_analysis_values[[result_number]] <- model_data
    
    
    # -----------------------------------------------------------------------
    # A. Fit the five recommended models using the full analysis sample
    # -----------------------------------------------------------------------
    
    fitted_models <- map(
      model_formulas,
      ~ glm(.x, data = model_data, family = binomial)
    )
    
    all_coefficients[[result_number]] <- map_dfr(
      names(fitted_models),
      ~ extract_coefficients(fitted_models[[.x]], .x)
    ) %>%
      mutate(
        discipline = discipline,
        focal_game = focal_game,
        next_game = next_game,
        .before = 1
      )
    
    # Controls + final score differential is compared with controls only.
    # Each sequence-momentum model is compared with final score differential.
    comparison_table <- tibble(
      model = names(fitted_models),
      model_description = unname(model_labels[names(fitted_models)]),
      compared_with = c(
        NA,
        "controls_only",
        "final_score_diff",
        "final_score_diff",
        "final_score_diff"
      ),
      n_matches = map_int(fitted_models, nobs),
      aic = map_dbl(fitted_models, AIC),
      log_likelihood = map_dbl(
        fitted_models,
        ~ as.numeric(logLik(.x))
      ),
      likelihood_ratio_p_value = c(
        NA_real_,
        get_lrt_p_value(
          fitted_models$controls_only,
          fitted_models$final_score_diff
        ),
        get_lrt_p_value(
          fitted_models$final_score_diff,
          fitted_models$max_streak_diff
        ),
        get_lrt_p_value(
          fitted_models$final_score_diff,
          fitted_models$streak_bonus
        ),
        get_lrt_p_value(
          fitted_models$final_score_diff,
          fitted_models$adjusted_late_game
        )
      )
    ) %>%
      mutate(
        discipline = discipline,
        focal_game = focal_game,
        next_game = next_game,
        .before = 1
      )
    
    all_model_comparisons[[result_number]] <- comparison_table
    
    
    # Save the fitted models so individual summary(model) calls can be run.
    model_file <- file.path(
      model_folder,
      paste0(
        tolower(discipline), "_game", focal_game,
        "_to_game", next_game, "_models.rds"
      )
    )
    
    saveRDS(fitted_models, model_file)
    
    
    # Save easy-to-read summary(model) output in one text file.
    summary_lines <- c(
      paste0(
        discipline, ": Game ", focal_game, " -> Game ", next_game
      ),
      paste0("Number of matches: ", nrow(model_data)),
      ""
    )
    
    for (model_name in names(fitted_models)) {
      summary_lines <- c(
        summary_lines,
        paste0("============================================================"),
        unname(model_labels[[model_name]]),
        paste0("============================================================"),
        capture.output(summary(fitted_models[[model_name]])),
        ""
      )
    }
    
    writeLines(
      summary_lines,
      file.path(
        summary_folder,
        paste0(
          tolower(discipline), "_game", focal_game,
          "_to_game", next_game, "_model_summaries.txt"
        )
      )
    )
    
    
    # -----------------------------------------------------------------------
    # B. Quick check 1: correlations among the four predictors
    # -----------------------------------------------------------------------
    
    correlation_matrix <- cor(
      model_data[, unname(predictor_columns)],
      use = "complete.obs"
    )
    
    predictor_correlations <- as.data.frame(
      as.table(correlation_matrix)
    ) %>%
      as_tibble() %>%
      rename(
        predictor_1 = Var1,
        predictor_2 = Var2,
        correlation = Freq
      ) %>%
      mutate(
        predictor_1_number = as.numeric(predictor_1),
        predictor_2_number = as.numeric(predictor_2)
      ) %>%
      filter(predictor_1_number < predictor_2_number) %>%
      select(-predictor_1_number, -predictor_2_number) %>%
      mutate(
        discipline = discipline,
        focal_game = focal_game,
        next_game = next_game,
        n_matches = nrow(model_data),
        .before = 1
      )
    
    all_predictor_correlations[[result_number]] <-
      predictor_correlations
    
    
    # -----------------------------------------------------------------------
    # C. Quick check 2: held-out prediction for the five models
    # -----------------------------------------------------------------------
    
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
    
    training_models <- map(
      model_formulas,
      ~ glm(.x, data = training_data, family = binomial)
    )
    
    oos_table <- map_dfr(
      names(training_models),
      function(model_name) {
        predicted_probability <- predict(
          training_models[[model_name]],
          newdata = test_data,
          type = "response"
        )
        
        calculate_metrics(
          actual = test_data$won_next_game,
          predicted_probability = predicted_probability
        ) %>%
          mutate(
            model = model_name,
            model_description = unname(model_labels[[model_name]]),
            .before = 1
          )
      }
    )
    
    controls_rmse <- oos_table$rmse[
      oos_table$model == "controls_only"
    ]
    controls_correlation <- oos_table$pearson_correlation[
      oos_table$model == "controls_only"
    ]
    score_diff_rmse <- oos_table$rmse[
      oos_table$model == "final_score_diff"
    ]
    score_diff_correlation <- oos_table$pearson_correlation[
      oos_table$model == "final_score_diff"
    ]
    
    oos_table <- oos_table %>%
      mutate(
        discipline = discipline,
        focal_game = focal_game,
        next_game = next_game,
        n_train = nrow(training_data),
        n_test = nrow(test_data),
        rmse_change_vs_controls_only = rmse - controls_rmse,
        correlation_change_vs_controls_only =
          pearson_correlation - controls_correlation,
        rmse_change_vs_final_score_diff = ifelse(
          model == "controls_only",
          NA,
          rmse - score_diff_rmse
        ),
        correlation_change_vs_final_score_diff = ifelse(
          model == "controls_only",
          NA,
          pearson_correlation - score_diff_correlation
        ),
        .before = 1
      )
    
    all_oos_results[[result_number]] <- oos_table
    
    
    # Print the most important full-sample results to the console.
    cat(
      "\n============================================================\n",
      discipline, ": Game ", focal_game, " -> Game ", next_game,
      "\nMatches: ", nrow(model_data),
      "\n============================================================\n",
      sep = ""
    )
    
    print(comparison_table)
    
    result_number <- result_number + 1
  }
}


# ---------------------------------------------------------------------------
# 8. Combine and save all results
# ---------------------------------------------------------------------------

analysis_values <- bind_rows(all_analysis_values) %>%
  arrange(discipline, focal_game, original_row)

model_coefficients <- bind_rows(all_coefficients) %>%
  arrange(discipline, focal_game, model, term)

model_comparison <- bind_rows(all_model_comparisons) %>%
  arrange(discipline, focal_game, model)

predictor_correlations <- bind_rows(all_predictor_correlations) %>%
  arrange(discipline, focal_game, predictor_1, predictor_2)

oos_model_comparison <- bind_rows(all_oos_results) %>%
  arrange(discipline, focal_game, model)

write.csv(
  model_coefficients,
  file.path(output_folder, "model_coefficients.csv"),
  row.names = FALSE
)

write.csv(
  model_comparison,
  file.path(output_folder, "model_comparison.csv"),
  row.names = FALSE
)

write.csv(
  predictor_correlations,
  file.path(output_folder, "predictor_correlations.csv"),
  row.names = FALSE
)

write.csv(
  oos_model_comparison,
  file.path(output_folder, "oos_model_comparison.csv"),
  row.names = FALSE
)

write.csv(
  analysis_values,
  file.path(output_folder, "analysis_values.csv"),
  row.names = FALSE
)

cat("\n=== MODEL COMPARISON ===\n")
print(model_comparison)

cat("\n=== PREDICTOR CORRELATIONS ===\n")
print(predictor_correlations)

cat("\n=== QUICK OUT-OF-SAMPLE COMPARISON ===\n")
print(oos_model_comparison)

cat(
  "\nResults saved in:\n",
  "  data/new_momentum/model_coefficients.csv\n",
  "  data/new_momentum/model_comparison.csv\n",
  "  data/new_momentum/predictor_correlations.csv\n",
  "  data/new_momentum/oos_model_comparison.csv\n",
  "  data/new_momentum/analysis_values.csv\n",
  "  data/new_momentum/model_summaries/\n",
  "  data/new_momentum/models/\n"
)

