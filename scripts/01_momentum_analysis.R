###############################################################################
# Badminton momentum analysis
#
# This script assumes that 00_compute_elo.R has already been run.
#
# For women's singles (WS) and men's singles (MS), it studies:
#   - Game 1 momentum -> winning Game 2
#   - Game 2 momentum -> winning Game 3
#
# For each transition, it compares five logistic regression models:
#   1. Controls only
#   2. Controls + final score differential
#   3. Controls + final score differential + maximum-streak differential
#   4. Controls + final score differential + streak-bonus momentum
#   5. Controls + final score differential + adjusted late-game momentum
#
# The controls are:
#   - Team 1 pre-match Elo minus Team 2 pre-match Elo, per 100 Elo points
#   - Tournament type
#   - Round
#
# Positive momentum values favor Team 1. The outcome is 1 if Team 1 wins the
# next game and 0 if Team 2 wins it.
#
# The script:
#   - Prints every model summary in the Console
#   - Saves result tables in data/momentum_results/
#   - Saves figures in figures/
#   - Performs the same one-time 80% training / 20% test comparison
###############################################################################

library(tidyverse)


# =============================================================================
# 1. Settings
# =============================================================================

# The adjusted late-game measure uses the final 10 rallies.
late_game_rallies <- 10

# Use 20% of the matches as the test set.
test_proportion <- 0.20

# This makes the random train/test split reproducible.
random_seed <- 123

# Processed datasets created by 00_compute_elo.R
data_files <- c(
  WS = "data/processed/ws_with_elo.csv",
  MS = "data/processed/ms_with_elo.csv"
)

# Folders for saved results
results_folder <- "data/momentum_results"
figures_folder <- "figures"

dir.create(results_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_folder, recursive = TRUE, showWarnings = FALSE)


# =============================================================================
# 2. Functions used to calculate momentum
# =============================================================================

# Turn a score history such as "['0-0', '1-0', '1-1']" into a vector of scores.
get_score_vector <- function(score_text) {
  if (is.na(score_text) || score_text == "" || score_text == "[]") {
    return(character(0))
  }

  score_vector <- str_extract_all(score_text, "\\d+-\\d+")[[1]]
  return(score_vector)
}


# Calculate the four predictors from one game's score history.
calculate_momentum <- function(score_vector, number_late_rallies) {

  # A valid score history needs at least two recorded scores.
  if (length(score_vector) < 2) {
    return(c(
      final_score_diff = NA,
      max_streak_diff = NA,
      streak_bonus_momentum = NA,
      adjusted_late_game_momentum = NA
    ))
  }

  # Separate each score into Team 1's score and Team 2's score.
  score_parts <- str_split_fixed(score_vector, "-", 2)
  team_one_score <- as.numeric(score_parts[, 1])
  team_two_score <- as.numeric(score_parts[, 2])

  team_one_streak <- 0
  team_two_streak <- 0
  team_one_max_streak <- 0
  team_two_max_streak <- 0

  streak_bonus <- 0
  rally_results <- c()

  # Start at 2 because each score must be compared with the previous score.
  for (i in 2:length(team_one_score)) {

    if (team_one_score[i] > team_one_score[i - 1] &&
        team_two_score[i] == team_two_score[i - 1]) {

      # Team 1 won the rally.
      team_one_streak <- team_one_streak + 1
      team_two_streak <- 0
      rally_winner <- 1
      current_streak <- team_one_streak

    } else if (team_two_score[i] > team_two_score[i - 1] &&
               team_one_score[i] == team_one_score[i - 1]) {

      # Team 2 won the rally.
      team_two_streak <- team_two_streak + 1
      team_one_streak <- 0
      rally_winner <- -1
      current_streak <- team_two_streak

    } else {
      # Skip a score if it does not show exactly one team winning one rally.
      next
    }

    team_one_max_streak <- max(team_one_max_streak, team_one_streak)
    team_two_max_streak <- max(team_two_max_streak, team_two_streak)

    # The first point in a streak gets sqrt(1) - 1 = 0 bonus.
    streak_bonus <- streak_bonus +
      rally_winner * (sqrt(current_streak) - 1)

    rally_results <- c(rally_results, rally_winner)
  }

  if (length(rally_results) == 0) {
    return(c(
      final_score_diff = NA,
      max_streak_diff = NA,
      streak_bonus_momentum = NA,
      adjusted_late_game_momentum = NA
    ))
  }

  # Team 1 points minus Team 2 points over the entire game.
  final_score_diff <- sum(rally_results)

  # If a game has fewer than 10 rallies, use all of its rallies.
  k <- min(number_late_rallies, length(rally_results))
  late_score_diff <- sum(tail(rally_results, k))

  # Compare late-game performance with what we would expect from the whole game.
  adjusted_late_game_momentum <- late_score_diff -
    (k / length(rally_results)) * final_score_diff

  momentum_values <- c(
    final_score_diff = final_score_diff,
    max_streak_diff = team_one_max_streak - team_two_max_streak,
    streak_bonus_momentum = streak_bonus,
    adjusted_late_game_momentum = adjusted_late_game_momentum
  )

  return(momentum_values)
}


# =============================================================================
# 3. Function used to prepare one analysis dataset
# =============================================================================

prepare_data <- function(raw_data, focal_game, next_game) {

  focal_score_history <- paste0("game_", focal_game, "_scores")
  next_game_score <- paste0("game_", next_game, "_score")

  analysis_data <- raw_data %>%
    mutate(original_row = row_number()) %>%
    filter(tolower(retired) != "true")

  # Game 2 -> Game 3 only includes matches in which Game 3 was played.
  if (next_game == 3) {
    analysis_data <- analysis_data %>%
      filter(
        !is.na(game_3_score),
        game_3_score != "",
        !is.na(game_3_scores),
        game_3_scores != "[]"
      )
  }

  # Calculate the four predictors one match at a time.
  momentum_rows <- lapply(
    analysis_data[[focal_score_history]],
    function(score_text) {
      score_vector <- get_score_vector(score_text)
      calculate_momentum(score_vector, late_game_rallies)
    }
  )

  momentum_data <- as.data.frame(do.call(rbind, momentum_rows))
  analysis_data <- bind_cols(analysis_data, momentum_data)

  # Split the next game's final score into the two teams' scores.
  next_scores <- str_split_fixed(analysis_data[[next_game_score]], "-", 2)
  analysis_data$next_team_one_score <- as.numeric(next_scores[, 1])
  analysis_data$next_team_two_score <- as.numeric(next_scores[, 2])

  # Create the outcome, Elo control, and factor variables.
  analysis_data <- analysis_data %>%
    mutate(
      won_next_game = ifelse(
        !is.na(next_team_one_score) &
          !is.na(next_team_two_score) &
          next_team_one_score != next_team_two_score,
        as.numeric(next_team_one_score > next_team_two_score),
        NA
      ),
      elo_difference_100 =
        (team_one_pre_match_elo - team_two_pre_match_elo) / 100,
      tournament_type = factor(tournament_type),
      round = factor(round)
    )

  # Keep complete rows so every model uses exactly the same matches.
  analysis_data <- analysis_data %>%
    filter(
      !is.na(won_next_game),
      !is.na(final_score_diff),
      !is.na(max_streak_diff),
      !is.na(streak_bonus_momentum),
      !is.na(adjusted_late_game_momentum),
      !is.na(elo_difference_100),
      !is.na(tournament_type),
      tournament_type != "",
      !is.na(round),
      round != ""
    ) %>%
    select(
      original_row,
      date,
      team_one_players,
      team_two_players,
      won_next_game,
      elo_difference_100,
      tournament_type,
      round,
      final_score_diff,
      max_streak_diff,
      streak_bonus_momentum,
      adjusted_late_game_momentum
    )

  return(analysis_data)
}


# =============================================================================
# 4. The five logistic regression models
# =============================================================================

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


# =============================================================================
# 5. Small functions used for the result tables
# =============================================================================

# Put the coefficient table from summary(model) into a data frame.
get_coefficients <- function(model, model_name) {

  coefficient_table <- as.data.frame(summary(model)$coefficients)
  coefficient_table$term <- rownames(coefficient_table)
  rownames(coefficient_table) <- NULL

  coefficient_table <- coefficient_table %>%
    rename(
      estimate = Estimate,
      standard_error = `Std. Error`,
      z_value = `z value`,
      p_value = `Pr(>|z|)`
    ) %>%
    mutate(
      model = model_name,
      model_description = model_labels[model_name],
      .before = 1
    )

  return(coefficient_table)
}


# Calculate Pearson correlation without stopping if one variable is constant.
safe_correlation <- function(x, y) {
  if (length(x) < 2 || sd(x) == 0 || sd(y) == 0) {
    return(NA)
  }

  return(cor(x, y))
}


# Calculate the two out-of-sample prediction measures.
get_prediction_measures <- function(actual, predicted) {
  rmse <- sqrt(mean((predicted - actual)^2))
  pearson_correlation <- safe_correlation(predicted, actual)

  return(c(
    rmse = rmse,
    pearson_correlation = pearson_correlation
  ))
}


# =============================================================================
# 6. Run all four analyses
# =============================================================================

all_analysis_values <- list()
all_coefficients <- list()
all_predictor_correlations <- list()
all_oos_results <- list()

result_number <- 1

for (discipline in names(data_files)) {

  raw_data <- read.csv(
    data_files[discipline],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  for (focal_game in c(1, 2)) {

    next_game <- focal_game + 1

    model_data <- prepare_data(
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


    # -------------------------------------------------------------------------
    # A. Fit and print the five models using all available matches
    # -------------------------------------------------------------------------

    fitted_models <- list()

    for (model_name in names(model_formulas)) {
      fitted_models[[model_name]] <- glm(
        model_formulas[[model_name]],
        data = model_data,
        family = binomial
      )
    }

    cat(
      "\n\n============================================================\n",
      discipline, ": Game ", focal_game, " -> Game ", next_game, "\n",
      "Number of matches: ", nrow(model_data), "\n",
      "============================================================\n",
      sep = ""
    )

    # Print every summary(model) result in the Console.
    for (model_name in names(fitted_models)) {
      cat(
        "\n------------------------------------------------------------\n",
        model_labels[model_name], "\n",
        "------------------------------------------------------------\n",
        sep = ""
      )

      print(summary(fitted_models[[model_name]]))
    }

    # Save every coefficient in one combined table.
    coefficient_tables <- list()

    for (model_name in names(fitted_models)) {
      coefficient_tables[[model_name]] <- get_coefficients(
        fitted_models[[model_name]],
        model_name
      )
    }

    coefficients_this_analysis <- bind_rows(coefficient_tables) %>%
      mutate(
        discipline = discipline,
        focal_game = focal_game,
        next_game = next_game,
        .before = 1
      )

    all_coefficients[[result_number]] <- coefficients_this_analysis


    # -------------------------------------------------------------------------
    # B. Calculate correlations among the four game-level predictors
    # -------------------------------------------------------------------------

    predictor_names <- c(
      "final_score_diff",
      "max_streak_diff",
      "streak_bonus_momentum",
      "adjusted_late_game_momentum"
    )

    correlation_matrix <- cor(model_data[, predictor_names])

    correlation_table <- as.data.frame(as.table(correlation_matrix)) %>%
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

    all_predictor_correlations[[result_number]] <- correlation_table


    # -------------------------------------------------------------------------
    # C. Perform the 80% training / 20% test comparison
    # -------------------------------------------------------------------------

    # Use a different but reproducible seed for each of the four analyses.
    split_seed <- random_seed

    if (discipline == "MS") {
      split_seed <- split_seed + 100
    }

    split_seed <- split_seed + focal_game
    set.seed(split_seed)

    # Sample within outcome and tournament type, as in the archived script.
    test_rows <- model_data %>%
      group_by(won_next_game, tournament_type) %>%
      slice_sample(prop = test_proportion) %>%
      ungroup() %>%
      pull(original_row)

    training_data <- model_data %>%
      filter(!(original_row %in% test_rows))

    test_data <- model_data %>%
      filter(original_row %in% test_rows)

    # Prediction requires every test-set category to appear in the training set.
    missing_rounds <- setdiff(
      unique(as.character(test_data$round)),
      unique(as.character(training_data$round))
    )

    if (length(missing_rounds) > 0) {
      stop(
        discipline, ": Game ", focal_game, " -> Game ", next_game,
        " has round categories in the test set but not the training set: ",
        paste(missing_rounds, collapse = ", "),
        ". Try changing random_seed."
      )
    }

    training_models <- list()

    for (model_name in names(model_formulas)) {
      training_models[[model_name]] <- glm(
        model_formulas[[model_name]],
        data = training_data,
        family = binomial
      )
    }

    oos_rows <- list()

    for (model_name in names(training_models)) {

      predicted_probability <- predict(
        training_models[[model_name]],
        newdata = test_data,
        type = "response"
      )

      measures <- get_prediction_measures(
        actual = test_data$won_next_game,
        predicted = predicted_probability
      )

      oos_rows[[model_name]] <- tibble(
        model = model_name,
        model_description = model_labels[model_name],
        rmse = measures["rmse"],
        pearson_correlation = measures["pearson_correlation"]
      )
    }

    oos_table <- bind_rows(oos_rows)

    controls_rmse <- oos_table$rmse[oos_table$model == "controls_only"]
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

    cat("\nOut-of-sample results:\n")
    print(oos_table)

    all_oos_results[[result_number]] <- oos_table
    result_number <- result_number + 1
  }
}


# =============================================================================
# 7. Combine and save the result tables
# =============================================================================

analysis_values <- bind_rows(all_analysis_values) %>%
  arrange(discipline, focal_game, original_row)

model_coefficients <- bind_rows(all_coefficients) %>%
  arrange(discipline, focal_game, model, term)

predictor_correlations <- bind_rows(all_predictor_correlations) %>%
  arrange(discipline, focal_game, predictor_1, predictor_2)

oos_model_comparison <- bind_rows(all_oos_results) %>%
  arrange(discipline, focal_game, model)

write.csv(
  analysis_values,
  file.path(results_folder, "analysis_values.csv"),
  row.names = FALSE
)

write.csv(
  model_coefficients,
  file.path(results_folder, "model_coefficients.csv"),
  row.names = FALSE
)

write.csv(
  predictor_correlations,
  file.path(results_folder, "predictor_correlations.csv"),
  row.names = FALSE
)

write.csv(
  oos_model_comparison,
  file.path(results_folder, "oos_model_comparison.csv"),
  row.names = FALSE
)


# =============================================================================
# 8. Make and save figures
# =============================================================================

# These figures show the simple relationship between each predictor and
# winning the next game. The model summaries above should be used for the
# regression results that include Elo, tournament type, and round controls.

predictor_names <- c(
  "final_score_diff",
  "max_streak_diff",
  "streak_bonus_momentum",
  "adjusted_late_game_momentum"
)

predictor_labels <- c(
  final_score_diff =
    "Final Score Differential (Team 1 - Team 2)",
  max_streak_diff =
    "Maximum-Streak Difference (Team 1 - Team 2)",
  streak_bonus_momentum =
    "Streak-Bonus Momentum (Team 1 - Team 2)",
  adjusted_late_game_momentum =
    "Adjusted Late-Game Momentum (Team 1 - Team 2)"
)

predictor_title_labels <- c(
  final_score_diff = "Final Score Differential",
  max_streak_diff = "Maximum-Streak Difference",
  streak_bonus_momentum = "Streak-Bonus Momentum",
  adjusted_late_game_momentum = "Adjusted Late-Game Momentum"
)

discipline_labels <- c(
  WS = "Women's Singles",
  MS = "Men's Singles"
)


# Make a separate figure for each discipline, game transition, and predictor.
for (discipline_name in c("WS", "MS")) {
  
  for (focal_game_number in c(1, 2)) {
    
    next_game_number <- focal_game_number + 1
    
    figure_data <- analysis_values %>%
      filter(
        discipline == discipline_name,
        focal_game == focal_game_number
      )
    
    for (predictor_name in predictor_names) {
      
      momentum_figure <- ggplot(
        figure_data,
        aes(
          x = .data[[predictor_name]],
          y = won_next_game
        )
      ) +
        geom_point(
          color = "gray30",
          alpha = 0.08,
          size = 1.5
        ) +
        geom_smooth(
          method = "glm",
          method.args = list(family = binomial),
          formula = y ~ x,
          color = "#159D82",
          fill = "gray75",
          linewidth = 1.2
        ) +
        geom_hline(
          yintercept = 0.5,
          color = "gray50",
          linetype = "dashed"
        ) +
        scale_y_continuous(
          limits = c(0, 1),
          breaks = seq(0, 1, 0.1)
        ) +
        labs(
          title = paste0(
            discipline_labels[discipline_name],
            ": Game ", next_game_number,
            " Win Probability Based on Game ",
            focal_game_number, " ",
            predictor_title_labels[predictor_name]
          ),
          x = paste0(
            "Game ", focal_game_number, " ",
            predictor_labels[predictor_name]
          ),
          y = paste0("Game ", next_game_number, " Win Probability")
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(
            face = "bold",
            hjust = 0.5
          ),
          panel.grid.minor = element_blank()
        )
      
      figure_filename <- paste0(
        tolower(discipline_name),
        "_game", focal_game_number,
        "_", predictor_name,
        "_vs_game", next_game_number,
        "_win.png"
      )
      
      ggsave(
        filename = file.path(figures_folder, figure_filename),
        plot = momentum_figure,
        width = 10,
        height = 7,
        dpi = 300
      )
    }
  }
}

# =============================================================================
# 9. Print final combined tables and file locations
# =============================================================================

cat("\n\n=== COMBINED PREDICTOR CORRELATIONS ===\n")
print(predictor_correlations)

cat("\n=== COMBINED OUT-OF-SAMPLE COMPARISON ===\n")
print(oos_model_comparison)

cat(
  "\nFinished. Tables were saved in data/momentum_results/.\n",
  "Figures were saved in figures/.\n"
)
