###############################################################################
# Checks for the revised badminton momentum definitions
#
# Run 01_legal_order_momentum_analysis.R before running this script.
#
# This script checks:
#   1. Pearson and Spearman correlations among the revised predictors
#   2. Basic summaries of the revised predictors
#   3. Correlations with Elo difference and winning the next game
#   4. Overlap with final score differential
#   5. Whether random adjustment reduced the score overlap of streak measures
#   6. Whether excess momentum is centered near zero within exact-score groups
#
# These are descriptive checks. Script 01 contains the main logistic models.
###############################################################################

library(tidyverse)


# =============================================================================
# 1. Read the analysis dataset created by Script 01
# =============================================================================

results_folder <- "data/momentum_results"

analysis_values <- read.csv(
  file.path(results_folder, "analysis_values.csv"),
  stringsAsFactors = FALSE
)

revised_predictors <- c(
  "final_score_diff",
  "excess_max_streak_momentum",
  "excess_streak_bonus_momentum",
  "excess_late_game_momentum"
)

momentum_predictors <- c(
  "excess_max_streak_momentum",
  "excess_streak_bonus_momentum",
  "excess_late_game_momentum"
)

predictor_labels <- c(
  final_score_diff = "Final score differential",
  raw_max_streak_diff = "Raw maximum-streak differential",
  raw_streak_bonus_momentum = "Raw streak-bonus momentum",
  excess_max_streak_momentum = "Excess maximum-streak momentum",
  excess_streak_bonus_momentum = "Excess streak-bonus momentum",
  excess_late_game_momentum = "Excess late-game momentum"
)


# =============================================================================
# 2. Function for making a correlation table
# =============================================================================

make_correlation_table <- function(data, method_name) {

  correlation_matrix <- cor(
    data[, revised_predictors],
    method = method_name,
    use = "complete.obs"
  )

  correlation_table <- as.data.frame(as.table(correlation_matrix)) %>%
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
    select(-predictor_1_number, -predictor_2_number)

  return(correlation_table)
}


# =============================================================================
# 3. Run checks separately for each discipline and game transition
# =============================================================================

pearson_results <- list()
spearman_results <- list()
summary_results <- list()
outside_results <- list()
score_overlap_results <- list()
old_new_results <- list()
conditional_mean_results <- list()

result_number <- 1

for (discipline_name in c("WS", "MS")) {

  for (focal_game_number in c(1, 2)) {

    next_game_number <- focal_game_number + 1

    group_data <- analysis_values %>%
      filter(
        discipline == discipline_name,
        focal_game == focal_game_number
      )

    # -------------------------------------------------------------------------
    # A. Pearson and Spearman correlations among revised predictors
    # -------------------------------------------------------------------------

    pearson_results[[result_number]] <-
      make_correlation_table(group_data, "pearson") %>%
      mutate(
        discipline = discipline_name,
        focal_game = focal_game_number,
        next_game = next_game_number,
        n_matches = nrow(group_data),
        .before = 1
      )

    spearman_results[[result_number]] <-
      make_correlation_table(group_data, "spearman") %>%
      mutate(
        discipline = discipline_name,
        focal_game = focal_game_number,
        next_game = next_game_number,
        n_matches = nrow(group_data),
        .before = 1
      )


    # -------------------------------------------------------------------------
    # B. Basic predictor summaries
    # -------------------------------------------------------------------------

    summary_results[[result_number]] <- group_data %>%
      select(all_of(revised_predictors)) %>%
      pivot_longer(
        cols = everything(),
        names_to = "predictor",
        values_to = "value"
      ) %>%
      group_by(predictor) %>%
      summarise(
        n_matches = sum(!is.na(value)),
        mean = mean(value, na.rm = TRUE),
        standard_deviation = sd(value, na.rm = TRUE),
        minimum = min(value, na.rm = TRUE),
        maximum = max(value, na.rm = TRUE),
        percent_equal_to_zero = mean(value == 0, na.rm = TRUE) * 100,
        .groups = "drop"
      ) %>%
      mutate(
        discipline = discipline_name,
        focal_game = focal_game_number,
        next_game = next_game_number,
        .before = 1
      )


    # -------------------------------------------------------------------------
    # C. Correlations with Elo and the next-game outcome
    # -------------------------------------------------------------------------

    outside_results[[result_number]] <- group_data %>%
      select(
        won_next_game,
        elo_difference_100,
        all_of(revised_predictors)
      ) %>%
      pivot_longer(
        cols = all_of(revised_predictors),
        names_to = "predictor",
        values_to = "value"
      ) %>%
      group_by(predictor) %>%
      summarise(
        n_matches = sum(
          complete.cases(value, elo_difference_100, won_next_game)
        ),
        correlation_with_elo = cor(
          value,
          elo_difference_100,
          use = "complete.obs"
        ),
        correlation_with_next_game_win = cor(
          value,
          won_next_game,
          use = "complete.obs"
        ),
        .groups = "drop"
      ) %>%
      mutate(
        discipline = discipline_name,
        focal_game = focal_game_number,
        next_game = next_game_number,
        .before = 1
      )


    # -------------------------------------------------------------------------
    # D. Correlation of each revised momentum measure with final score
    # -------------------------------------------------------------------------

    overlap_rows <- list()

    for (predictor_name in momentum_predictors) {

      score_correlation <- cor(
        group_data$final_score_diff,
        group_data[[predictor_name]],
        use = "complete.obs"
      )

      overlap_rows[[predictor_name]] <- tibble(
        discipline = discipline_name,
        focal_game = focal_game_number,
        next_game = next_game_number,
        predictor = predictor_name,
        n_matches = nrow(group_data),
        correlation_with_final_score_diff = score_correlation,
        r_squared = score_correlation^2
      )
    }

    score_overlap_results[[result_number]] <- bind_rows(overlap_rows)


    # -------------------------------------------------------------------------
    # E. Compare raw streak measures with their excess versions
    # -------------------------------------------------------------------------

    old_new_results[[result_number]] <- tibble(
      discipline = discipline_name,
      focal_game = focal_game_number,
      next_game = next_game_number,
      measure = c("Maximum streak", "Square-root streak bonus"),
      raw_correlation_with_final_score = c(
        cor(
          group_data$raw_max_streak_diff,
          group_data$final_score_diff
        ),
        cor(
          group_data$raw_streak_bonus_momentum,
          group_data$final_score_diff
        )
      ),
      adjusted_correlation_with_final_score = c(
        cor(
          group_data$excess_max_streak_momentum,
          group_data$final_score_diff
        ),
        cor(
          group_data$excess_streak_bonus_momentum,
          group_data$final_score_diff
        )
      )
    )


    # -------------------------------------------------------------------------
    # F. Means within exact final-score groups
    # -------------------------------------------------------------------------

    # Because the new measures adjust for exact point totals, their average
    # should be reasonably close to zero among games with the same two final
    # scores. Keep groups with at least 20 matches so very small groups are not
    # overread.

    conditional_mean_results[[result_number]] <- group_data %>%
      select(
        focal_team_one_points,
        focal_team_two_points,
        final_score_diff,
        all_of(momentum_predictors)
      ) %>%
      pivot_longer(
        cols = all_of(momentum_predictors),
        names_to = "predictor",
        values_to = "value"
      ) %>%
      group_by(
        focal_team_one_points,
        focal_team_two_points,
        final_score_diff,
        predictor
      ) %>%
      summarise(
        n_matches = n(),
        mean_momentum = mean(value),
        .groups = "drop"
      ) %>%
      filter(n_matches >= 20) %>%
      mutate(
        discipline = discipline_name,
        focal_game = focal_game_number,
        next_game = next_game_number,
        .before = 1
      )

    result_number <- result_number + 1
  }
}


# =============================================================================
# 4. Combine the check tables and add readable labels
# =============================================================================

pearson_correlations <- bind_rows(pearson_results) %>%
  mutate(
    predictor_1_label =
      unname(predictor_labels[as.character(predictor_1)]),
    predictor_2_label =
      unname(predictor_labels[as.character(predictor_2)])
  )

spearman_correlations <- bind_rows(spearman_results) %>%
  mutate(
    predictor_1_label =
      unname(predictor_labels[as.character(predictor_1)]),
    predictor_2_label =
      unname(predictor_labels[as.character(predictor_2)])
  )

predictor_summaries <- bind_rows(summary_results) %>%
  mutate(
    predictor_label =
      unname(predictor_labels[as.character(predictor)])
  )

outside_correlations <- bind_rows(outside_results) %>%
  mutate(
    predictor_label =
      unname(predictor_labels[as.character(predictor)])
  )

overlap_with_final_score <- bind_rows(score_overlap_results) %>%
  mutate(
    predictor_label =
      unname(predictor_labels[as.character(predictor)])
  )

raw_vs_adjusted_overlap <- bind_rows(old_new_results)

conditional_means <- bind_rows(conditional_mean_results) %>%
  mutate(
    predictor_label =
      unname(predictor_labels[as.character(predictor)])
  )


# =============================================================================
# 5. Save the check tables
# =============================================================================

write.csv(
  pearson_correlations,
  file.path(results_folder, "02_pearson_correlations.csv"),
  row.names = FALSE
)

write.csv(
  spearman_correlations,
  file.path(results_folder, "02_spearman_correlations.csv"),
  row.names = FALSE
)

write.csv(
  predictor_summaries,
  file.path(results_folder, "02_predictor_summaries.csv"),
  row.names = FALSE
)

write.csv(
  outside_correlations,
  file.path(results_folder, "02_correlations_with_elo_and_next_game.csv"),
  row.names = FALSE
)

write.csv(
  overlap_with_final_score,
  file.path(results_folder, "02_overlap_with_final_score.csv"),
  row.names = FALSE
)

write.csv(
  raw_vs_adjusted_overlap,
  file.path(results_folder, "02_raw_vs_adjusted_overlap.csv"),
  row.names = FALSE
)

write.csv(
  conditional_means,
  file.path(results_folder, "02_conditional_means.csv"),
  row.names = FALSE
)


# =============================================================================
# 6. Print the most useful checks in the Console
# =============================================================================

cat("\n\n=== PEARSON CORRELATIONS AMONG REVISED PREDICTORS ===\n")
print(pearson_correlations)

cat("\n\n=== SPEARMAN CORRELATIONS AMONG REVISED PREDICTORS ===\n")
print(spearman_correlations)

cat("\n\n=== CORRELATIONS WITH ELO AND WINNING THE NEXT GAME ===\n")
print(outside_correlations)

cat("\n\n=== OVERLAP OF REVISED MOMENTUM WITH FINAL SCORE ===\n")
print(overlap_with_final_score)

cat("\n\n=== RAW VERSUS ADJUSTED SCORE OVERLAP ===\n")
print(raw_vs_adjusted_overlap)

cat("\n\n=== MEAN MOMENTUM WITHIN EXACT-SCORE GROUPS ===\n")
print(conditional_means)

cat(
  "\nFinished. Seven check tables were saved in data/momentum_results/.\n"
)
