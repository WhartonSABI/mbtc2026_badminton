#############################################################################
# Badminton: Time-Weighted Game 1 Momentum -> Game 2 Win
#
# PURPOSE:
# This script tests whether Player 1's time-weighted momentum in Game 1
# is associated with whether Player 1 wins Game 2.
#
# DATA AND MODELS:
# Choose either men's singles ("MS") or women's singles ("WS") below.
# The processed data must contain the pre-match Elo columns created by
# 00_compute_elo.R.
#
# The script fits:
#   1. an uncontrolled logistic regression; and
#   2. a controlled logistic regression that adjusts for pre-match Elo
#      difference, tournament type, and round.
#
# MOMENTUM DEFINITION:
# Each rally contributes:
#
#   sign * (1 + sqrt(streak length)) * time weight
#
# The sign is positive when Player 1 wins the rally and negative when
# Player 2 wins it. The time weight is:
#
#   1 + c * min((number of rallies completed - 1) / 40, 1)
#
# We use c = 0.5, so the weight rises from 1 at the first rally to at
# most 1.5 late in the game. This version does NOT use score-difference
# weighting.
#
# The final measure is one signed value per match:
#
#   Player 1 weighted momentum - Player 2 weighted momentum
#
# Positive values favor Player 1, while negative values favor Player 2.
#############################################################################

library(tidyverse)

# Choose "MS" for men's singles or "WS" for women's singles.
discipline_to_use <- "MS"

# c = 0 removes time weighting. The proposed main value is c = 0.5.
c_weight <- 0.5

if (discipline_to_use == "MS") {
  data_file <- "data/processed/ms_with_elo.csv"
} else if (discipline_to_use == "WS") {
  data_file <- "data/processed/ws_with_elo.csv"
} else {
  stop('discipline_to_use must be either "MS" or "WS".')
}

df <- read.csv(data_file, stringsAsFactors = FALSE)

# Retired matches may not contain complete, comparable games.
df <- df %>%
  filter(tolower(retired) != "true")

# Turn a string such as "['0-0', '1-0', '1-1']" into a vector of scores.
parse_score_list <- function(x) {
  if (is.na(x) || x == "" || x == "[]") return(character(0))
  str_extract_all(x, "\\d+-\\d+")[[1]]
}

# Walk through one game's point-by-point scores and calculate Player 1's
# cumulative momentum relative to Player 2.
compute_time_momentum <- function(score_vec, c) {
  if (length(score_vec) < 2) {
    return(NA_real_)
  }
  
  score_parts <- str_split_fixed(score_vec, "-", 2)
  p1_score <- as.integer(score_parts[, 1])
  p2_score <- as.integer(score_parts[, 2])
  
  p1_streak <- 0L
  p2_streak <- 0L
  momentum_difference <- 0
  
  # The score list normally begins at 0-0, so i = 2 is the first rally.
  for (i in 2:length(p1_score)) {
    
    # ti is the total number of rallies completed after rally i.
    t_points <- p1_score[i] + p2_score[i]
    q_i <- min((t_points - 1) / 40, 1)
    time_weight <- 1 + c * q_i
    
    if (p1_score[i] > p1_score[i - 1] &&
        p2_score[i] == p2_score[i - 1]) {
      
      # Player 1 won this rally.
      p1_streak <- p1_streak + 1L
      p2_streak <- 0L
      
      contribution <- time_weight * (1 + sqrt(p1_streak))
      momentum_difference <- momentum_difference + contribution
      
    } else if (p2_score[i] > p2_score[i - 1] &&
               p1_score[i] == p1_score[i - 1]) {
      
      # Player 2 won this rally.
      p2_streak <- p2_streak + 1L
      p1_streak <- 0L
      
      contribution <- time_weight * (1 + sqrt(p2_streak))
      momentum_difference <- momentum_difference - contribution
    }
  }
  
  return(momentum_difference)
}

# Calculate game 1 momentum for every match
results <- df %>%
  rowwise() %>%
  mutate(
    game_1_points = list(parse_score_list(game_1_scores)),
    game_1_time_momentum =
      compute_time_momentum(game_1_points, c_weight)
  ) %>%
  ungroup()

# Determine if player 1 won game 2
results <- results %>%
  separate(
    game_2_score,
    into = c("g2_p1", "g2_p2"),
    sep = "-",
    remove = FALSE,
    convert = TRUE,
    fill = "right"
  ) %>%
  mutate(
    valid_game_2 = !is.na(g2_p1) & !is.na(g2_p2) & g2_p1 != g2_p2,
    p1_won_game_2 =
      ifelse(valid_game_2, as.integer(g2_p1 > g2_p2), NA)
  )

# Keep one row per match
# the momentum variable is already Player 1 minus Player 2
analysis_data <- results %>%
  filter(!is.na(game_1_time_momentum), !is.na(p1_won_game_2)) %>%
  transmute(
    game_1_time_momentum,
    p1_won_game_2,
    elo_difference_100 =
      (team_one_pre_match_elo - team_two_pre_match_elo) / 100,
    tournament_type,
    round
  )

model_data <- analysis_data %>%
  filter(
    !is.na(elo_difference_100),
    !is.na(tournament_type), tournament_type != "",
    !is.na(round), round != ""
  )

cat("Number of matches in both models:", nrow(model_data), "\n")

# Logistic regression models
model_g1_time_uncontrolled <- glm(
  p1_won_game_2 ~ game_1_time_momentum,
  data = model_data,
  family = binomial
)

cat("\n=== UNCONTROLLED MODEL ===\n")
print(summary(model_g1_time_uncontrolled))

model_g1_time_controlled <- glm(
  p1_won_game_2 ~ game_1_time_momentum + elo_difference_100 +
    factor(tournament_type) + factor(round),
  data = model_data,
  family = binomial
)

cat("\n=== CONTROLLED MODEL ===\n")
print(summary(model_g1_time_controlled))

# Plot for uncontrolled regression model
regression_plot <- ggplot(
  model_data,
  aes(x = game_1_time_momentum, y = p1_won_game_2)
) +
  geom_jitter(height = 0.03, width = 0, alpha = 0.15, color = "gray40") +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    color = "#2ca25f",
    linewidth = 1.2,
    fill = "gray80",
    se = TRUE
  ) +
  coord_cartesian(ylim = c(-0.05, 1.05)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  labs(
    title = "Game 2 Win Probability Based on Game 1 Momentum",
    x = "Game 1 Time-Weighted Momentum (Player 1 - Player 2)",
    y = "Probability Player 1 Wins Game 2"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(regression_plot)

dir.create("figures", showWarnings = FALSE)

ggsave(
  filename = paste0(
    "figures/", tolower(discipline_to_use),
    "_game1_time_weighted_momentum_vs_game2_win.png"
  ),
  plot = regression_plot,
  width = 9,
  height = 6,
  dpi = 150
)

