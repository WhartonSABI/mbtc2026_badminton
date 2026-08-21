# Steps: calculate badminton momentum, fit logistic regression models, 
# adjust the 12 focal p-values, and compare out-of-sample predictions.

library(tidyverse)

number_random_orders <- 250
late_game_fraction <- 0.25
test_proportion <- 0.20
momentum_seed <- 2026
test_seed <- 123
data_files <- c(WS = "data/processed/ws_with_elo.csv", MS = "data/processed/ms_with_elo.csv")
results_folder <- "data/momentum_results"
figures_folder <- "figures"
dir.create(results_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_folder, recursive = TRUE, showWarnings = FALSE)

# Convert a cumulative score history into rally winners: 1 for Team 1 and -1 for Team 2.
get_rally_results <- function(score_text) {
  if (is.na(score_text) || score_text == "" || score_text == "[]") return(numeric(0))
  scores <- str_extract_all(score_text, "\\d+-\\d+")[[1]]
  if (length(scores) < 2) return(numeric(0))
  score_parts <- str_split_fixed(scores, "-", 2)
  team_one_score <- as.numeric(score_parts[, 1])
  team_two_score <- as.numeric(score_parts[, 2])
  team_one_change <- diff(team_one_score)
  team_two_change <- diff(team_two_score)
  ifelse(team_one_change == 1 & team_two_change == 0, 1, ifelse(team_one_change == 0 & team_two_change == 1, -1, NA_real_)) %>% na.omit() %>% as.numeric()
}

# Calculate the maximum-streak difference and square-root streak bonus for one rally order.
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
    streak_bonus <- streak_bonus + rally_winner * (sqrt(current_streak) - 1)
  }
  c(max_streak_diff = team_one_max_streak - team_two_max_streak, streak_bonus = streak_bonus)
}

# Check whether the current score legally ends a badminton game.
game_is_over <- function(team_one_score, team_two_score) {
  reached_30 <- team_one_score == 30 || team_two_score == 30
  won_by_two <- max(team_one_score, team_two_score) >= 21 && abs(team_one_score - team_two_score) >= 2
  reached_30 || won_by_two
}

# Check that a rally order ends exactly when a player first wins the game.
is_legal_game_order <- function(rally_results) {
  if (length(rally_results) == 0) return(FALSE)
  team_one_score <- 0
  team_two_score <- 0
  for (i in seq_along(rally_results)) {
    if (rally_results[i] == 1) team_one_score <- team_one_score + 1 else team_two_score <- team_two_score + 1
    if (game_is_over(team_one_score, team_two_score)) return(i == length(rally_results))
  }
  FALSE
}

# Generate one legal random order with the observed final score.
get_legal_random_order <- function(rally_results) {
  team_one_points <- sum(rally_results == 1)
  team_two_points <- sum(rally_results == -1)
  if (team_one_points == team_two_points) stop("A completed game cannot have tied final scores.")
  final_rally_winner <- ifelse(team_one_points > team_two_points, 1, -1)
  remaining_rallies <- c(rep(1, team_one_points - as.numeric(final_rally_winner == 1)), rep(-1, team_two_points - as.numeric(final_rally_winner == -1)))
  for (attempt in seq_len(100000)) {
    random_order <- c(sample(remaining_rallies, replace = FALSE), final_rally_winner)
    if (is_legal_game_order(random_order)) return(random_order)
  }
  stop("Could not generate a legal random order for a ", team_one_points, "-", team_two_points, " game.")
}

# Cache random-order expectations because games with the same final score share the same comparison.
expected_momentum_cache <- new.env()

get_expected_momentum <- function(rally_results) {
  team_one_points <- sum(rally_results == 1)
  team_two_points <- sum(rally_results == -1)
  cache_name <- paste(team_one_points, team_two_points, late_game_fraction, sep = "_")
  if (exists(cache_name, envir = expected_momentum_cache, inherits = FALSE)) return(get(cache_name, envir = expected_momentum_cache, inherits = FALSE))
  set.seed(momentum_seed + 1000 * team_one_points + team_two_points)
  number_late_rallies <- ceiling(late_game_fraction * length(rally_results))
  random_results <- replicate(number_random_orders, {
    legal_order <- get_legal_random_order(rally_results)
    streak_measures <- get_streak_measures(legal_order)
    c(max_streak_diff = unname(streak_measures["max_streak_diff"]), streak_bonus = unname(streak_measures["streak_bonus"]), late_score_diff = sum(tail(legal_order, number_late_rallies)))
  })
  expected_momentum <- rowMeans(random_results)
  assign(cache_name, expected_momentum, envir = expected_momentum_cache)
  expected_momentum
}

empty_momentum <- function() {
  c(focal_team_one_points = NA_real_, focal_team_two_points = NA_real_, final_score_diff = NA_real_, excess_max_streak_momentum = NA_real_, excess_streak_bonus_momentum = NA_real_, excess_late_game_momentum = NA_real_)
}

# Calculate final score differential and the three excess-momentum measures.
calculate_momentum <- function(rally_results) {
  if (length(rally_results) == 0 || !is_legal_game_order(rally_results)) return(empty_momentum())
  team_one_points <- sum(rally_results == 1)
  team_two_points <- sum(rally_results == -1)
  observed_streaks <- get_streak_measures(rally_results)
  expected_momentum <- get_expected_momentum(rally_results)
  number_late_rallies <- ceiling(late_game_fraction * length(rally_results))
  c(
    focal_team_one_points = team_one_points,
    focal_team_two_points = team_two_points,
    final_score_diff = team_one_points - team_two_points,
    excess_max_streak_momentum = unname(observed_streaks["max_streak_diff"] - expected_momentum["max_streak_diff"]),
    excess_streak_bonus_momentum = unname(observed_streaks["streak_bonus"] - expected_momentum["streak_bonus"]),
    excess_late_game_momentum = unname(sum(tail(rally_results, number_late_rallies)) - expected_momentum["late_score_diff"])
  )
}

# Prepare one discipline and game-sequence sample.
prepare_data <- function(raw_data, focal_game, next_game) {
  focal_score_history <- paste0("game_", focal_game, "_scores")
  next_game_score <- paste0("game_", next_game, "_score")
  analysis_data <- raw_data %>% mutate(original_row = row_number()) %>% filter(tolower(retired) != "true")
  if (next_game == 3) analysis_data <- analysis_data %>% filter(!is.na(game_3_score), game_3_score != "", !is.na(game_3_scores), game_3_scores != "[]")
  momentum_rows <- lapply(analysis_data[[focal_score_history]], function(score_text) calculate_momentum(get_rally_results(score_text)))
  momentum_data <- as.data.frame(do.call(rbind, momentum_rows))
  analysis_data <- bind_cols(analysis_data, momentum_data)
  next_scores <- str_split_fixed(analysis_data[[next_game_score]], "-", 2)
  analysis_data$next_team_one_score <- as.numeric(next_scores[, 1])
  analysis_data$next_team_two_score <- as.numeric(next_scores[, 2])
  analysis_data %>%
    mutate(
      won_next_game = ifelse(!is.na(next_team_one_score) & !is.na(next_team_two_score) & next_team_one_score != next_team_two_score, as.numeric(next_team_one_score > next_team_two_score), NA_real_),
      elo_difference_100 = (team_one_pre_match_elo - team_two_pre_match_elo) / 100,
      tournament_type = factor(tournament_type),
      round = factor(round)
    ) %>%
    filter(!is.na(won_next_game), !is.na(final_score_diff), !is.na(excess_max_streak_momentum), !is.na(excess_streak_bonus_momentum), !is.na(excess_late_game_momentum), !is.na(elo_difference_100), !is.na(tournament_type), tournament_type != "", !is.na(round), round != "") %>%
    select(original_row, won_next_game, elo_difference_100, tournament_type, round, focal_team_one_points, focal_team_two_points, final_score_diff, excess_max_streak_momentum, excess_streak_bonus_momentum, excess_late_game_momentum)
}

# Fit the baseline and three momentum models.
model_formulas <- list(
  controls_only = won_next_game ~ elo_difference_100 + tournament_type + round,
  final_score = won_next_game ~ final_score_diff + elo_difference_100 + tournament_type + round,
  maximum_streak = won_next_game ~ final_score_diff + excess_max_streak_momentum + elo_difference_100 + tournament_type + round,
  streak_bonus = won_next_game ~ final_score_diff + excess_streak_bonus_momentum + elo_difference_100 + tournament_type + round,
  late_game = won_next_game ~ final_score_diff + excess_late_game_momentum + elo_difference_100 + tournament_type + round
)

model_labels <- c(controls_only = "Controls only", final_score = "Final score", maximum_streak = "Maximum streak", streak_bonus = "Streak bonus", late_game = "Late-game momentum")
focal_terms <- c(maximum_streak = "excess_max_streak_momentum", streak_bonus = "excess_streak_bonus_momentum", late_game = "excess_late_game_momentum")

get_coefficients <- function(model, model_name) {
  as.data.frame(summary(model)$coefficients) %>%
    rownames_to_column("term") %>%
    rename(estimate = Estimate, standard_error = `Std. Error`, z_value = `z value`, p_value = `Pr(>|z|)`) %>%
    filter(term %in% c("final_score_diff", "elo_difference_100", unname(focal_terms))) %>%
    mutate(model = model_name, model_description = model_labels[model_name], is_focal_momentum = model_name %in% names(focal_terms) & term == focal_terms[model_name], .before = 1)
}

get_prediction_measures <- function(actual, predicted) {
  correlation <- if (length(actual) < 2 || sd(actual) == 0 || sd(predicted) == 0) NA_real_ else cor(predicted, actual)
  c(rmse = sqrt(mean((predicted - actual)^2)), pearson_correlation = correlation)
}

all_analysis_values <- list()
all_regression_results <- list()
all_oos_results <- list()
result_number <- 1

# Run the four samples and retain only results used in the paper.
for (discipline in names(data_files)) {
  raw_data <- read.csv(data_files[discipline], stringsAsFactors = FALSE, check.names = FALSE)
  for (focal_game in c(1, 2)) {
    next_game <- focal_game + 1
    model_data <- prepare_data(raw_data, focal_game, next_game) %>% mutate(discipline = discipline, focal_game = focal_game, next_game = next_game, .before = 1)
    all_analysis_values[[result_number]] <- model_data
    
    fitted_models <- lapply(model_formulas, function(formula) glm(formula, data = model_data, family = binomial))
    regression_results <- bind_rows(lapply(names(fitted_models), function(model_name) get_coefficients(fitted_models[[model_name]], model_name))) %>%
      mutate(discipline = discipline, focal_game = focal_game, next_game = next_game, n_matches = nrow(model_data), .before = 1)
    all_regression_results[[result_number]] <- regression_results
    
    # Use a reproducible stratified 80/20 split for out-of-sample comparisons.
    set.seed(test_seed + ifelse(discipline == "MS", 100, 0) + focal_game)
    test_rows <- model_data %>% group_by(won_next_game, tournament_type) %>% slice_sample(prop = test_proportion) %>% ungroup() %>% pull(original_row)
    training_data <- model_data %>% filter(!(original_row %in% test_rows))
    test_data <- model_data %>% filter(original_row %in% test_rows)
    missing_rounds <- setdiff(unique(as.character(test_data$round)), unique(as.character(training_data$round)))
    if (length(missing_rounds) > 0) stop(discipline, ": Game ", focal_game, " -> Game ", next_game, " has round categories in the test set but not the training set. Change test_seed.")
    
    training_models <- lapply(model_formulas, function(formula) glm(formula, data = training_data, family = binomial))
    oos_results <- bind_rows(lapply(names(training_models), function(model_name) {
      predicted <- predict(training_models[[model_name]], newdata = test_data, type = "response")
      measures <- get_prediction_measures(test_data$won_next_game, predicted)
      tibble(model = model_name, model_description = model_labels[model_name], rmse = unname(measures["rmse"]), pearson_correlation = unname(measures["pearson_correlation"]))
    })) %>% mutate(discipline = discipline, focal_game = focal_game, next_game = next_game, n_train = nrow(training_data), n_test = nrow(test_data), .before = 1)
    all_oos_results[[result_number]] <- oos_results
    result_number <- result_number + 1
  }
}

# Apply one Holm correction across the 12 focal momentum p-values.
regression_results <- bind_rows(all_regression_results) %>% arrange(discipline, focal_game, model, term) %>% mutate(p_value_holm = NA_real_, significant_holm_05 = NA)
focal_rows <- which(regression_results$is_focal_momentum)
if (length(focal_rows) != 12) stop("Expected 12 focal momentum tests, but found ", length(focal_rows), ".")
regression_results$p_value_holm[focal_rows] <- p.adjust(regression_results$p_value[focal_rows], method = "holm")
regression_results$significant_holm_05[focal_rows] <- regression_results$p_value_holm[focal_rows] < 0.05

oos_model_comparison <- bind_rows(all_oos_results) %>% arrange(discipline, focal_game, model)
write.csv(regression_results, file.path(results_folder, "regression_results_holm.csv"), row.names = FALSE)
write.csv(oos_model_comparison, file.path(results_folder, "oos_model_comparison.csv"), row.names = FALSE)

# Print the 12 corrected focal tests for a quick check.
momentum_tests <- regression_results %>% filter(is_focal_momentum) %>% select(discipline, focal_game, next_game, model_description, estimate, standard_error, p_value, p_value_holm, significant_holm_05)
print(momentum_tests)

# Recreate the four representative figures used for men's singles, Game 1 to Game 2.
analysis_values <- bind_rows(all_analysis_values)
figure_data <- analysis_values %>% filter(discipline == "MS", focal_game == 1)
figure_predictors <- c("final_score_diff", "excess_max_streak_momentum", "excess_streak_bonus_momentum", "excess_late_game_momentum")
predictor_labels <- c(final_score_diff = "Final Score Differential (Team 1 - Team 2)", excess_max_streak_momentum = "Excess Maximum-Streak Momentum (Team 1 - Team 2)", excess_streak_bonus_momentum = "Excess Streak-Bonus Momentum (Team 1 - Team 2)", excess_late_game_momentum = "Excess Late-Game Momentum (Team 1 - Team 2)")
predictor_titles <- c(final_score_diff = "Final Score Differential", excess_max_streak_momentum = "Excess Maximum-Streak Momentum", excess_streak_bonus_momentum = "Excess Streak-Bonus Momentum", excess_late_game_momentum = "Excess Late-Game Momentum")

for (predictor_name in figure_predictors) {
  momentum_figure <- ggplot(figure_data, aes(x = .data[[predictor_name]], y = won_next_game)) +
    geom_point(color = "gray30", alpha = 0.08, size = 1.5) +
    geom_smooth(method = "glm", method.args = list(family = binomial), formula = y ~ x, color = "#159D82", fill = "gray75", linewidth = 1.2) +
    geom_hline(yintercept = 0.5, color = "gray50", linetype = "dashed") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    labs(title = paste0("Men's Singles: Game 2 Win Probability Based on Game 1 ", predictor_titles[predictor_name]), x = paste0("Game 1 ", predictor_labels[predictor_name]), y = "Game 2 Win Probability") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5), panel.grid.minor = element_blank())
  figure_filename <- paste0("ms_game1_", predictor_name, "_vs_game2_win.png")
  ggsave(file.path(figures_folder, figure_filename), momentum_figure, width = 10, height = 7, dpi = 300)
}

cat("\nFinished. Holm-adjusted regression results and out-of-sample results were saved in data/momentum_results/.\n")
