###############################################################################
# Badminton momentum analysis
#
# Run 00_compute_elo.R before running this script.
#
# This script studies:
#   - Game 1 momentum -> winning Game 2
#   - Game 2 momentum -> winning Game 3
# for women's singles (WS) and men's singles (MS).
#
# Final score differential measures overall performance in the focal game.
# The three momentum predictors measure the ORDER in which points were won:
#
#   1. Excess maximum-streak momentum
#   2. Excess square-root streak-bonus momentum
#   3. Excess late-game momentum
#
# All three are compared with random LEGAL reorderings of the same game's
# points. Each random order:
#   - Has exactly the same final point totals
#   - Ends when a team first wins under badminton's scoring rules
#   - Therefore cannot include rallies after the game should have ended
#
# The late-game measure uses the final 25% of rallies and subtracts the average
# late-game differential across the legal random orders.
#
# Positive values favor Team 1. The outcome is 1 if Team 1 wins the next game.
#
# The script:
#   - Removes retired matches
#   - Runs and prints five ordinary logistic regression models
#   - Saves coefficient, correlation, and analysis-value tables
#   - Performs one reproducible 80% training / 20% test comparison
#   - Saves simple predicted-probability figures
###############################################################################

library(tidyverse)


# =============================================================================
# 1. Settings
# =============================================================================

# Number of legal random point orders used for all three momentum measures.
# Increasing this gives a more stable average but makes the script slower.
number_random_orders <- 250

# The final 25% of rallies are treated as the late-game period.
late_game_fraction <- 0.25

# Use 20% of the matches as the test set.
test_proportion <- 0.20

# These make the random calculations reproducible.
momentum_seed <- 2026
test_seed <- 123

# Processed datasets created by 00_compute_elo.R.
data_files <- c(
  WS = "data/processed/ws_with_elo.csv",
  MS = "data/processed/ms_with_elo.csv"
)

results_folder <- "data/momentum_results"
figures_folder <- "figures"

dir.create(results_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_folder, recursive = TRUE, showWarnings = FALSE)


# =============================================================================
# 2. Functions used to calculate momentum
# =============================================================================

# Turn a score history such as "['0-0', '1-0', '1-1']" into rally winners.
# Team 1 winning a rally is recorded as 1; Team 2 winning is recorded as -1.
get_rally_results <- function(score_text) {

  if (is.na(score_text) || score_text == "" || score_text == "[]") {
    return(numeric(0))
  }

  score_vector <- str_extract_all(score_text, "\\d+-\\d+")[[1]]

  if (length(score_vector) < 2) {
    return(numeric(0))
  }

  score_parts <- str_split_fixed(score_vector, "-", 2)
  team_one_score <- as.numeric(score_parts[, 1])
  team_two_score <- as.numeric(score_parts[, 2])

  rally_results <- numeric(0)

  for (i in 2:length(team_one_score)) {

    team_one_change <- team_one_score[i] - team_one_score[i - 1]
    team_two_change <- team_two_score[i] - team_two_score[i - 1]

    if (team_one_change == 1 && team_two_change == 0) {
      rally_results <- c(rally_results, 1)
    } else if (team_one_change == 0 && team_two_change == 1) {
      rally_results <- c(rally_results, -1)
    }
  }

  return(rally_results)
}


# Calculate maximum-streak difference and square-root streak bonus for one
# particular ordering of rallies.
get_streak_measures <- function(rally_results) {

  team_one_streak <- 0
  team_two_streak <- 0
  team_one_max_streak <- 0
  team_two_max_streak <- 0
  streak_bonus <- 0

  for (rally_winner in rally_results) {

    if (rally_winner == 1) {
      team_one_streak <- team_one_streak + 1
      team_two_streak <- 0
      current_streak <- team_one_streak
    } else {
      team_two_streak <- team_two_streak + 1
      team_one_streak <- 0
      current_streak <- team_two_streak
    }

    team_one_max_streak <- max(team_one_max_streak, team_one_streak)
    team_two_max_streak <- max(team_two_max_streak, team_two_streak)

    # The first point in a streak gets sqrt(1) - 1 = 0 bonus.
    # Longer streaks receive increasingly larger total bonuses.
    streak_bonus <- streak_bonus +
      rally_winner * (sqrt(current_streak) - 1)
  }

  return(c(
    max_streak_diff = team_one_max_streak - team_two_max_streak,
    streak_bonus = streak_bonus
  ))
}


# Check whether a score means that a badminton game is over.
# A game ends at 21 or more with a two-point lead, or when either team reaches
# the 30-point cap.
game_is_over <- function(team_one_score, team_two_score) {

  reached_30 <- team_one_score == 30 || team_two_score == 30

  won_by_two <- max(team_one_score, team_two_score) >= 21 &&
    abs(team_one_score - team_two_score) >= 2

  return(reached_30 || won_by_two)
}


# Check that a rally order ends exactly when the game first becomes complete.
is_legal_game_order <- function(rally_results) {

  if (length(rally_results) == 0) {
    return(FALSE)
  }

  team_one_score <- 0
  team_two_score <- 0

  for (i in seq_along(rally_results)) {

    if (rally_results[i] == 1) {
      team_one_score <- team_one_score + 1
    } else {
      team_two_score <- team_two_score + 1
    }

    if (game_is_over(team_one_score, team_two_score)) {
      return(i == length(rally_results))
    }
  }

  return(FALSE)
}


# Make one random legal ordering with the same final point totals.
get_legal_random_order <- function(rally_results) {

  team_one_points <- sum(rally_results == 1)
  team_two_points <- sum(rally_results == -1)

  if (team_one_points == team_two_points) {
    stop("A completed game cannot have tied final scores.")
  }

  # The team with the larger final score must win the final rally.
  final_rally_winner <- ifelse(team_one_points > team_two_points, 1, -1)

  remaining_rallies <- c(
    rep(1, team_one_points - as.numeric(final_rally_winner == 1)),
    rep(-1, team_two_points - as.numeric(final_rally_winner == -1))
  )

  maximum_attempts <- 100000

  for (attempt in 1:maximum_attempts) {

    random_order <- c(
      sample(remaining_rallies, replace = FALSE),
      final_rally_winner
    )

    if (is_legal_game_order(random_order)) {
      return(random_order)
    }
  }

  stop(
    "Could not generate a legal random order for a ",
    team_one_points, "-", team_two_points, " game."
  )
}


# Games with the same point totals have the same legal random comparison.
# Saving each comparison avoids repeating the same simulation many times.
expected_momentum_cache <- new.env()

get_expected_momentum <- function(
    rally_results,
    random_orders,
    late_fraction,
    base_seed
) {

  team_one_points <- sum(rally_results == 1)
  team_two_points <- sum(rally_results == -1)
  cache_name <- paste(
    team_one_points,
    team_two_points,
    late_fraction,
    sep = "_"
  )

  if (exists(
    cache_name,
    envir = expected_momentum_cache,
    inherits = FALSE
  )) {
    return(get(
      cache_name,
      envir = expected_momentum_cache,
      inherits = FALSE
    ))
  }

  # Use a reproducible seed based only on the two point totals.
  set.seed(base_seed + 1000 * team_one_points + team_two_points)

  number_late_rallies <- ceiling(late_fraction * length(rally_results))

  random_results <- replicate(random_orders, {

    legal_order <- get_legal_random_order(rally_results)
    streak_measures <- get_streak_measures(legal_order)

    c(
      max_streak_diff = unname(streak_measures["max_streak_diff"]),
      streak_bonus = unname(streak_measures["streak_bonus"]),
      late_score_diff = sum(tail(legal_order, number_late_rallies))
    )
  })

  expected_momentum <- rowMeans(random_results)
  assign(cache_name, expected_momentum, envir = expected_momentum_cache)

  return(expected_momentum)
}


# Calculate the baseline score measure and the three revised momentum measures.
calculate_momentum <- function(
    rally_results,
    random_orders,
    late_fraction,
    base_seed
) {

  if (length(rally_results) == 0) {
    return(c(
      focal_team_one_points = NA_real_,
      focal_team_two_points = NA_real_,
      final_score_diff = NA_real_,
      raw_max_streak_diff = NA_real_,
      raw_streak_bonus_momentum = NA_real_,
      excess_max_streak_momentum = NA_real_,
      excess_streak_bonus_momentum = NA_real_,
      excess_late_game_momentum = NA_real_
    ))
  }

  # Exclude any score history that is not a legally completed game.
  if (!is_legal_game_order(rally_results)) {
    return(c(
      focal_team_one_points = NA_real_,
      focal_team_two_points = NA_real_,
      final_score_diff = NA_real_,
      raw_max_streak_diff = NA_real_,
      raw_streak_bonus_momentum = NA_real_,
      excess_max_streak_momentum = NA_real_,
      excess_streak_bonus_momentum = NA_real_,
      excess_late_game_momentum = NA_real_
    ))
  }

  focal_team_one_points <- sum(rally_results == 1)
  focal_team_two_points <- sum(rally_results == -1)
  final_score_diff <- focal_team_one_points - focal_team_two_points
  observed_streaks <- get_streak_measures(rally_results)

  # Randomly reorder the same points, retaining only legal completed games.
  expected_momentum <- get_expected_momentum(
    rally_results = rally_results,
    random_orders = random_orders,
    late_fraction = late_fraction,
    base_seed = base_seed
  )

  excess_max_streak <- observed_streaks["max_streak_diff"] -
    expected_momentum["max_streak_diff"]

  excess_streak_bonus <- observed_streaks["streak_bonus"] -
    expected_momentum["streak_bonus"]

  # Use the final 25% of rallies, rounded up to a whole rally.
  number_late_rallies <- ceiling(late_fraction * length(rally_results))
  late_score_diff <- sum(tail(rally_results, number_late_rallies))

  excess_late_game <-
    late_score_diff - expected_momentum["late_score_diff"]

  return(c(
    focal_team_one_points = focal_team_one_points,
    focal_team_two_points = focal_team_two_points,
    final_score_diff = final_score_diff,
    raw_max_streak_diff =
      unname(observed_streaks["max_streak_diff"]),
    raw_streak_bonus_momentum =
      unname(observed_streaks["streak_bonus"]),
    excess_max_streak_momentum = unname(excess_max_streak),
    excess_streak_bonus_momentum = unname(excess_streak_bonus),
    excess_late_game_momentum = unname(excess_late_game)
  ))
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

  # Game 2 -> Game 3 includes only matches in which Game 3 was played.
  if (next_game == 3) {
    analysis_data <- analysis_data %>%
      filter(
        !is.na(game_3_score),
        game_3_score != "",
        !is.na(game_3_scores),
        game_3_scores != "[]"
      )
  }

  # Calculate the predictors one match at a time.
  momentum_rows <- lapply(
    analysis_data[[focal_score_history]],
    function(score_text) {
      rally_results <- get_rally_results(score_text)

      calculate_momentum(
        rally_results = rally_results,
        random_orders = number_random_orders,
        late_fraction = late_game_fraction,
        base_seed = momentum_seed
      )
    }
  )

  momentum_data <- as.data.frame(do.call(rbind, momentum_rows))
  analysis_data <- bind_cols(analysis_data, momentum_data)

  # Split the next game's final score into the two teams' scores.
  next_scores <- str_split_fixed(analysis_data[[next_game_score]], "-", 2)
  analysis_data$next_team_one_score <- as.numeric(next_scores[, 1])
  analysis_data$next_team_two_score <- as.numeric(next_scores[, 2])

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
      !is.na(excess_max_streak_momentum),
      !is.na(excess_streak_bonus_momentum),
      !is.na(excess_late_game_momentum),
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
      focal_team_one_points,
      focal_team_two_points,
      final_score_diff,
      raw_max_streak_diff,
      raw_streak_bonus_momentum,
      excess_max_streak_momentum,
      excess_streak_bonus_momentum,
      excess_late_game_momentum
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

  excess_max_streak =
    won_next_game ~ final_score_diff + excess_max_streak_momentum +
      elo_difference_100 + tournament_type + round,

  excess_streak_bonus =
    won_next_game ~ final_score_diff + excess_streak_bonus_momentum +
      elo_difference_100 + tournament_type + round,

  excess_late_game =
    won_next_game ~ final_score_diff + excess_late_game_momentum +
      elo_difference_100 + tournament_type + round
)

model_labels <- c(
  controls_only = "Controls only",
  final_score_diff = "Controls + final score differential",
  excess_max_streak =
    "Controls + final score differential + excess maximum streak",
  excess_streak_bonus =
    "Controls + final score differential + excess streak bonus",
  excess_late_game =
    "Controls + final score differential + excess late-game momentum"
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


safe_correlation <- function(x, y) {
  if (length(x) < 2 || sd(x) == 0 || sd(y) == 0) {
    return(NA_real_)
  }

  return(cor(x, y))
}


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

    for (model_name in names(fitted_models)) {
      cat(
        "\n------------------------------------------------------------\n",
        model_labels[model_name], "\n",
        "------------------------------------------------------------\n",
        sep = ""
      )

      print(summary(fitted_models[[model_name]]))
    }

    coefficient_tables <- list()

    for (model_name in names(fitted_models)) {
      coefficient_tables[[model_name]] <- get_coefficients(
        fitted_models[[model_name]],
        model_name
      )
    }

    all_coefficients[[result_number]] <- bind_rows(coefficient_tables) %>%
      mutate(
        discipline = discipline,
        focal_game = focal_game,
        next_game = next_game,
        .before = 1
      )


    # -------------------------------------------------------------------------
    # B. Calculate correlations among the four main predictors
    # -------------------------------------------------------------------------

    predictor_names <- c(
      "final_score_diff",
      "excess_max_streak_momentum",
      "excess_streak_bonus_momentum",
      "excess_late_game_momentum"
    )

    correlation_matrix <- cor(model_data[, predictor_names])

    all_predictor_correlations[[result_number]] <-
      as.data.frame(as.table(correlation_matrix)) %>%
      rename(
        predictor_1 = Var1,
        predictor_2 = Var2,
        correlation = Freq
      ) %>%
      mutate(
        predictor_1_number = as.numeric(predictor_1),
        predictor_2_number = as.numeric(predictor_2),
        predictor_1 = as.character(predictor_1),
        predictor_2 = as.character(predictor_2)
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


    # -------------------------------------------------------------------------
    # C. Perform the 80% training / 20% test comparison
    # -------------------------------------------------------------------------

    split_seed <- test_seed

    if (discipline == "MS") {
      split_seed <- split_seed + 100
    }

    split_seed <- split_seed + focal_game
    set.seed(split_seed)

    test_rows <- model_data %>%
      group_by(won_next_game, tournament_type) %>%
      slice_sample(prop = test_proportion) %>%
      ungroup() %>%
      pull(original_row)

    training_data <- model_data %>%
      filter(!(original_row %in% test_rows))

    test_data <- model_data %>%
      filter(original_row %in% test_rows)

    missing_rounds <- setdiff(
      unique(as.character(test_data$round)),
      unique(as.character(training_data$round))
    )

    if (length(missing_rounds) > 0) {
      stop(
        discipline, ": Game ", focal_game, " -> Game ", next_game,
        " has round categories in the test set but not the training set: ",
        paste(missing_rounds, collapse = ", "),
        ". Try changing test_seed."
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
# 8. Make and save simple figures
# =============================================================================

# These figures show the unadjusted relationship between each focal-game
# predictor and winning the next game. The model summaries above should be used
# for results that control for Elo, tournament type, round, and final score.

figure_predictors <- c(
  "final_score_diff",
  "excess_max_streak_momentum",
  "excess_streak_bonus_momentum",
  "excess_late_game_momentum"
)

predictor_labels <- c(
  final_score_diff = "Final Score Differential (Team 1 - Team 2)",
  excess_max_streak_momentum =
    "Excess Maximum-Streak Momentum (Team 1 - Team 2)",
  excess_streak_bonus_momentum =
    "Excess Streak-Bonus Momentum (Team 1 - Team 2)",
  excess_late_game_momentum =
    "Excess Late-Game Momentum (Team 1 - Team 2)"
)

predictor_title_labels <- c(
  final_score_diff = "Final Score Differential",
  excess_max_streak_momentum = "Excess Maximum-Streak Momentum",
  excess_streak_bonus_momentum = "Excess Streak-Bonus Momentum",
  excess_late_game_momentum = "Excess Late-Game Momentum"
)

discipline_labels <- c(
  WS = "Women's Singles",
  MS = "Men's Singles"
)

for (discipline_name in c("WS", "MS")) {

  for (focal_game_number in c(1, 2)) {

    next_game_number <- focal_game_number + 1

    figure_data <- analysis_values %>%
      filter(
        discipline == discipline_name,
        focal_game == focal_game_number
      )

    for (predictor_name in figure_predictors) {

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
          plot.title = element_text(face = "bold", hjust = 0.5),
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

cat(
  "\nFinished. Results were saved in data/momentum_results/",
  " and figures were saved in figures/.\n",
  sep = ""
)

