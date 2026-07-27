#############################################################################
# Badminton: Score-Difference-Weighted Game 2 Momentum -> Game 3 Win
#
# PURPOSE:
# This script tests whether Player 1's momentum in Game 2 is associated
# with whether Player 1 wins Game 3.
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
# Only matches that actually went to a deciding third game are included
# (i.e. game_3_score is not blank/NA, and the point-by-point game_3_scores
# list is not just "[]").
#
# Idea: instead of just counting a player's longest streak, we build a
# running "momentum difference" across every rally of Game 2.
# Each rally contributes a gain/loss based on:
#   (a) how many CONSECUTIVE points that player has just won/lost
#       (the "streak" -> momentum table you provided), and
#   (b) how close the score is at that moment (a "closeness weight" that
#       amplifies momentum swings in tight rallies and dampens them in
#       lopsided ones).
#
# We then ask whether Player 1's momentum relative to Player 2 at the end
# of Game 2 predicts whether Player 1 wins Game 3.
#
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
# Example check: Player 1 wins the first point, score becomes 1-0,
# streak n = 1, base_gain = 1 + sqrt(1) = 2, g = |1-0| = 1,
# Ci = 1 + 0.25/(1+1) = 1.125.
# -> Player 1 minus Player 2 momentum increases by 2.25
#############################################################################

# ---------------------------------------------------------------------------
# TUNABLE PARAMETER: change this to adjust how much score-closeness affects
# momentum. d = 0 turns the closeness weight off entirely (Ci is always 1).
# ---------------------------------------------------------------------------
d_weight <- 0.25

# ---------------------------------------------------------------------------
# 0. Packages
# ---------------------------------------------------------------------------
library(tidyverse)

# ---------------------------------------------------------------------------
# 1. Load data
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# 2. Keep only matches that actually had a real, decisive Game 3.
#
#    NOTE: `game_3_score` (e.g. "21-9") is the final score, while
#    `game_3_scores` (plural) is the point-by-point list, e.g.
#    "['0-0', '1-0', ...]" or "[]" when no third game was played. We check
#    both -- a handful of rows are inconsistent between the two columns
#    (final score present but point list empty, or vice versa), so
#    requiring both to be populated gives the cleanest set of matches.
# ---------------------------------------------------------------------------

raw <- raw %>%
  filter(
    tolower(retired) != "true",
    !is.na(game_3_score), game_3_score != "",
    !is.na(game_3_scores), game_3_scores != "[]"
  )

cat("Matches that went to a deciding Game 3:", nrow(raw), "\n")

# Keep only the two columns we need for the analysis
df <- raw %>%
  select(game_2_scores, game_3_score,
         team_one_pre_match_elo, team_two_pre_match_elo,
         tournament_type, round)

# ---------------------------------------------------------------------------
# 3. Helper: turn a "['0-0', '1-0', '2-0', ...]" style string into a
#    character vector of "a-b" score strings
# ---------------------------------------------------------------------------
parse_score_list <- function(x) {
  if (is.na(x) || x == "" || x == "[]") return(character(0))
  str_extract_all(x, "\\d+-\\d+")[[1]]
}

# ---------------------------------------------------------------------------
# 4. Core function: walk through Game 2's point-by-point scores and
#    compute Player 1 minus Player 2 cumulative weighted momentum
# ---------------------------------------------------------------------------
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
    if (a[i] > a[i - 1]) {                 # team_one won this rally
      cur_a <- cur_a + 1L
      cur_b <- 0L
      n  <- cur_a
      g  <- abs(a[i] - b[i])
      Ci <- 1 + d / (1 + g)
      gain <- Ci * (1 + sqrt(n))
      momentum_difference <- momentum_difference + gain
      
    } else if (b[i] > b[i - 1]) {          # team_two won this rally
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

# ---------------------------------------------------------------------------
# 5. Apply to every match
# ---------------------------------------------------------------------------
results <- df %>%
  rowwise() %>%
  mutate(
    g2_points = list(parse_score_list(game_2_scores)),
    game_2_momentum_difference = compute_momentum(g2_points, d_weight)
  ) %>%
  ungroup()

# ---------------------------------------------------------------------------
# 6. Determine the Game 3 winner from the final score, e.g. "21-9"
# ---------------------------------------------------------------------------
results <- results %>%
  separate(game_3_score, into = c("g3_one", "g3_two"),
           sep = "-", remove = FALSE, convert = TRUE, fill = "right") %>%
  mutate(
    has_valid_g3 = !is.na(g3_one) & !is.na(g3_two) & g3_one != g3_two,
    won_g3_one   = ifelse(has_valid_g3, as.integer(g3_one > g3_two), NA)
  )

# ---------------------------------------------------------------------------
# 7. Keep one row per MATCH: Player 1's relative momentum and result
# ---------------------------------------------------------------------------
plot_data <- results %>%
  filter(!is.na(game_2_momentum_difference), has_valid_g3,
         !is.na(team_one_pre_match_elo), !is.na(team_two_pre_match_elo),
         !is.na(tournament_type), !is.na(round)) %>%
  transmute(
    game_2_momentum_difference,
    won_game_3 = won_g3_one,
    elo_difference_100 =
      (team_one_pre_match_elo - team_two_pre_match_elo) / 100,
    tournament_type,
    round
  )

cat("Number of matches:", nrow(plot_data), "\n")

# ---------------------------------------------------------------------------
# 8. Plot: Game 2 momentum (x) vs. Game 3 win probability (y)
#
#    Momentum is continuous now (not restricted to small integers like a
#    streak count), so a plain low-alpha scatter naturally builds up into
#    a heatmap-like density -- darker where lots of matches land on a
#    similar momentum value, lighter where they're rare. A logistic trend
#    line is layered on top.
# ---------------------------------------------------------------------------
p <- ggplot(plot_data, aes(x = game_2_momentum_difference, y = won_game_3)) +
  geom_point(alpha = 0.03, size = 1.6, color = "black") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              color = "#1B9E77", fill = "grey70", linewidth = 1.1) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(-0.05, 1.05)) +
  labs(
    title = "Next Game Win Probability Based on Weighted Game 2 Momentum",
    x = "Game 2 Momentum (Player 1 - Player 2)",
    y = "Player 1 Game 3 Win Probability"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

print(p)

ggsave(paste0("figures/", tolower(discipline_to_use),
              "_game2_score_diff_momentum_vs_game3_win.png"),
       plot = p, width = 9, height = 6, dpi = 150)

model_uncontrolled <- glm(won_game_3 ~ game_2_momentum_difference,
                          data = plot_data, family = binomial)

cat("\n=== UNCONTROLLED MODEL ===\n")
print(summary(model_uncontrolled))

model_controlled <- glm(
  won_game_3 ~ game_2_momentum_difference + elo_difference_100 +
    factor(tournament_type) + factor(round),
  data = plot_data, family = binomial
)
cat("\n=== CONTROLLED MODEL ===\n")
print(summary(model_controlled))
