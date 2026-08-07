###############################################################################
# Exploratory data analysis
#
# Run 01_momentum_analysis.R before running this script.
#
# This script makes:
#   1. Histograms of the numeric predictors
#   2. Bar charts of tournament type and round
#   3. A correlation matrix for the numeric predictors
#
# Separate figures are made for women's and men's singles and for
# Game 1 -> Game 2 and Game 2 -> Game 3.
###############################################################################

library(tidyverse)


# =============================================================================
# 1. Read the analysis data
# =============================================================================

analysis_data <- read.csv(
  "data/momentum_results/analysis_values.csv",
  stringsAsFactors = FALSE
)

eda_folder <- "figures/eda"
dir.create(eda_folder, recursive = TRUE, showWarnings = FALSE)


# =============================================================================
# 2. Variable names and labels
# =============================================================================

numeric_predictors <- c(
  "elo_difference_100",
  "final_score_diff",
  "excess_max_streak_momentum",
  "excess_streak_bonus_momentum",
  "excess_late_game_momentum"
)

numeric_labels <- c(
  elo_difference_100 = "Pre-Match Elo Difference (100 Points)",
  final_score_diff = "Final Score Differential",
  excess_max_streak_momentum = "Excess Maximum-Streak Momentum",
  excess_streak_bonus_momentum = "Excess Streak-Bonus Momentum",
  excess_late_game_momentum = "Excess Late-Game Momentum"
)

categorical_controls <- c(
  "tournament_type",
  "round"
)

categorical_labels <- c(
  tournament_type = "Tournament Type",
  round = "Tournament Round"
)


# =============================================================================
# 3. Make figures for each of the four analyses
# =============================================================================

for (discipline_name in c("WS", "MS")) {

  for (focal_game_number in c(1, 2)) {

    next_game_number <- focal_game_number + 1

    figure_data <- analysis_data %>%
      filter(
        discipline == discipline_name,
        focal_game == focal_game_number
      )

    if (discipline_name == "WS") {
      discipline_title <- "Women's Singles"
    } else {
      discipline_title <- "Men's Singles"
    }

    figure_title <- paste0(
      discipline_title,
      ": Game ", focal_game_number,
      " to Game ", next_game_number
    )

    file_prefix <- paste0(
      tolower(discipline_name),
      "_game", focal_game_number,
      "_to_game", next_game_number
    )


    # -------------------------------------------------------------------------
    # A. Histograms of the numeric predictors
    # -------------------------------------------------------------------------

    histogram_data <- figure_data %>%
      select(all_of(numeric_predictors)) %>%
      pivot_longer(
        cols = everything(),
        names_to = "predictor",
        values_to = "value"
      ) %>%
      mutate(
        predictor = factor(
          predictor,
          levels = numeric_predictors,
          labels = numeric_labels
        )
      )

    histogram_figure <- ggplot(histogram_data, aes(x = value)) +
      geom_histogram(
        bins = 30,
        fill = "#159D82",
        color = "white"
      ) +
      facet_wrap(~ predictor, scales = "free", ncol = 2) +
      labs(
        title = paste(figure_title, "Predictor Distributions"),
        x = NULL,
        y = "Number of Matches"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid.minor = element_blank()
      )

    ggsave(
      filename = file.path(
        eda_folder,
        paste0(file_prefix, "_histograms.png")
      ),
      plot = histogram_figure,
      width = 11,
      height = 8,
      dpi = 300
    )


    # -------------------------------------------------------------------------
    # B. Bar charts of tournament type and round
    # -------------------------------------------------------------------------

    control_data <- figure_data %>%
      select(all_of(categorical_controls)) %>%
      pivot_longer(
        cols = everything(),
        names_to = "control",
        values_to = "category"
      ) %>%
      mutate(
        control = factor(
          control,
          levels = categorical_controls,
          labels = categorical_labels
        )
      )

    control_figure <- ggplot(control_data, aes(x = category)) +
      geom_bar(fill = "#159D82") +
      facet_wrap(~ control, scales = "free_x", ncol = 1) +
      labs(
        title = paste(figure_title, "Control-Variable Distributions"),
        x = NULL,
        y = "Number of Matches"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank()
      )

    ggsave(
      filename = file.path(
        eda_folder,
        paste0(file_prefix, "_categorical_controls.png")
      ),
      plot = control_figure,
      width = 11,
      height = 8,
      dpi = 300
    )


    # -------------------------------------------------------------------------
    # C. Correlation matrix of the numeric predictors
    # -------------------------------------------------------------------------

    correlation_matrix <- cor(
      figure_data[, numeric_predictors],
      use = "complete.obs"
    )

    correlation_data <- as.data.frame(as.table(correlation_matrix)) %>%
      rename(
        predictor_1 = Var1,
        predictor_2 = Var2,
        correlation = Freq
      ) %>%
      mutate(
        predictor_1 = factor(
          predictor_1,
          levels = numeric_predictors,
          labels = numeric_labels
        ),
        predictor_2 = factor(
          predictor_2,
          levels = numeric_predictors,
          labels = numeric_labels
        )
      )

    correlation_figure <- ggplot(
      correlation_data,
      aes(x = predictor_1, y = predictor_2, fill = correlation)
    ) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(correlation, 2)), size = 3.5) +
      scale_fill_gradient2(
        low = "#B2182B",
        mid = "white",
        high = "#2166AC",
        midpoint = 0,
        limits = c(-1, 1)
      ) +
      labs(
        title = paste(figure_title, "Predictor Correlations"),
        x = NULL,
        y = NULL,
        fill = "Correlation"
      ) +
      coord_equal() +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
      )

    ggsave(
      filename = file.path(
        eda_folder,
        paste0(file_prefix, "_correlation_matrix.png")
      ),
      plot = correlation_figure,
      width = 10,
      height = 9,
      dpi = 300
    )
  }
}

cat("Finished. EDA figures were saved in figures/eda/.\n")
