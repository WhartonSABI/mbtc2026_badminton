# Badminton: Score-Difference-Weighted Game 1 Momentum -> Game 2 Win
#
# PURPOSE:
# This script tests whether Player 1's momentum in Game 1 is associated
# with whether Player 1 wins Game 2.
#
# MOMENTUM AND OUTPUTS:
# The script gives each rally more weight when the score is close.
# It calculates one momentum value per match:
#
#   Player 1 weighted momentum - Player 2 weighted momentum
#
# Positive values favor Player 1, while negative values favor Player 2.
# The outputs are a graph and a logistic regression summary.
# This definition includes score-difference weighting only; it does not
# include time weighting.
#
# Idea: instead of just counting a player's longest streak, we build a
# running "momentum difference" across every rally of Game 1.
# Each rally contributes a gain/loss based on:
#   (a) how many CONSECUTIVE points that player has just won/lost
#       (the "streak" -> momentum table you provided), and
#   (b) how close the score is at that moment (a "closeness weight" that
#       amplifies momentum swings in tight rallies and dampens them in
#       lopsided ones).
# ---------------------------------------------------------------------------
# MOMENTUM FORMULA
# ---------------------------------------------------------------------------
# For a rally that extends a player's current winning streak to n
# consecutive points:
#     base_gain  =  1 + sqrt(n)
#     base_loss  = -(1 + sqrt(n))     (applied to their opponent)
#
# Score-closeness weight, using the score AFTER that rally (a_i, b_i):
#     g   = |a_i - b_i|
#     Ci  = 1 + d / (1 + g)
#
# Actual change in Player 1 minus Player 2 momentum:
#     Player 1 wins: add      Ci * base_gain
#     Player 2 wins: subtract Ci * base_gain
#
# Example: Player 1 wins the first point,
# score becomes 1-0, streak n = 1, base_gain = 1 + sqrt(1) = 2,
# g = |1-0| = 1, Ci = 1 + 0.25/(1+1) = 1.125.
# -> Player 1 minus Player 2 momentum increases by 2.25


d_weight <- 0.25


library(tidyverse)

# Choose "MS" for men's singles or "WS" for women's singles.
discipline_to_use <- "WS"

if (discipline_to_use == "MS") {
  data_file <- "data/processed/ms_with_elo.csv"
} else if (discipline_to_use == "WS") {
  data_file <- "data/processed/ws_with_elo.csv"
} else {
  stop('discipline_to_use must be either "MS" or "WS".')
}
raw <- read.csv(data_file, stringsAsFactors = FALSE)
raw <- raw %>% filter(tolower(retired) != "true")

df <- raw %>%
  select(game_1_scores, game_2_score,
         team_one_pre_match_elo, team_two_pre_match_elo,
         tournament_type, round)


parse_score_list <- function(x) {
  if (is.na(x) || x == "" || x == "[]") return(character(0))
  str_extract_all(x, "\\d+-\\d+")[[1]]
}

compute_momentum <- function(score_vec, d) {
  if (length(score_vec) < 2) {
    return(NA_real_)
  }
  
  parts <- str_split_fixed(score_vec, "-", 2)
  a <- as.integer(parts[, 1])   # team_one running score
  b <- as.integer(parts[, 2])   # team_two running score
  
  cur_a <- 0L
  cur_b <- 0L
  momentum_difference <- 0
  
  for (i in 2:length(a)) {
    if (a[i] > a[i - 1]) {                 # team_one won  
      cur_a <- cur_a + 1L
      cur_b <- 0L
      n  <- cur_a
      g  <- abs(a[i] - b[i])
      Ci <- 1 + d / (1 + g)
      gain <- Ci * (1 + sqrt(n))
      momentum_difference <- momentum_difference + gain
      
    } else if (b[i] > b[i - 1]) {          # team_two won  
      cur_b <- cur_b + 1L
      cur_a <- 0L
      n  <- cur_b
      g  <- abs(a[i] - b[i])
      Ci <- 1 + d / (1 + g)
      gain <- Ci * (1 + sqrt(n))
      momentum_difference <- momentum_difference - gain
    }
  }
  
  return(momentum_difference)
}

results <- df %>%
  rowwise() %>%
  mutate(
    g1_points = list(parse_score_list(game_1_scores)),
    game_1_momentum_difference = compute_momentum(g1_points, d_weight)
  ) %>%
  ungroup()

results <- results %>%
  separate(game_2_score, into = c("g2_one", "g2_two"),
           sep = "-", remove = FALSE, convert = TRUE, fill = "right") %>%
  mutate(
    has_valid_g2 = !is.na(g2_one) & !is.na(g2_two) & g2_one != g2_two,
    won_g2_one   = ifelse(has_valid_g2, as.integer(g2_one > g2_two), NA)
  )

plot_data <- results %>%
  filter(!is.na(game_1_momentum_difference), has_valid_g2,
         !is.na(team_one_pre_match_elo), !is.na(team_two_pre_match_elo),
         !is.na(tournament_type), !is.na(round)) %>%
  transmute(
    game_1_momentum_difference,
    won_game_2 = won_g2_one,
    elo_difference_100 =
      (team_one_pre_match_elo - team_two_pre_match_elo) / 100,
    tournament_type,
    round
  )

cat("Number of matches:", nrow(plot_data), "\n")

p <- ggplot(plot_data, aes(x = game_1_momentum_difference, y = won_game_2)) +
  geom_point(alpha = 0.03, size = 1.6, color = "black") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              color = "#1B9E77", fill = "grey70", linewidth = 1.1) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(-0.05, 1.05)) +
  labs(
    title = "Next Game Win Probability Based on Weighted Game 1 Momentum",
    x = "Game 1 Momentum (Player 1 - Player 2)",
    y = "Player 1 Game 2 Win Probability"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )


print(p)

ggsave(paste0("figures/", tolower(discipline_to_use),
              "_game1_score_diff_momentum_vs_game2_win.png"),
       plot = p, width = 9, height = 6, dpi = 150)

model_uncontrolled <- glm(won_game_2 ~ game_1_momentum_difference,
                          data = plot_data, family = binomial)

cat("\n=== UNCONTROLLED MODEL ===\n")
print(summary(model_uncontrolled))

model_controlled <- glm(
  won_game_2 ~ game_1_momentum_difference + elo_difference_100 +
    factor(tournament_type) + factor(round),
  data = plot_data, family = binomial
)
cat("\n=== CONTROLLED MODEL ===\n")
print(summary(model_controlled))

