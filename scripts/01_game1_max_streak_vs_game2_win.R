# Uses one row per match and defines momentum as Team 1's maximum streak
# minus Team 2's maximum streak. A positive value favors Team 1, and the
# outcome equals 1 when Team 1 wins Game 2. Using one row per match avoids
# treating the two players from the same match as independent observations.

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
df <- read.csv(data_file, stringsAsFactors = FALSE)

# Filter out retired matches before calculating momentum.
df <- df %>%
  filter(tolower(retired) != "true")

parse_score_list <- function(x) {
  if (is.na(x) || x == "" || x == "[]") return(character(0))
  str_extract_all(x, "\\d+-\\d+")[[1]]
}

max_streaks <- function(score_vec) {
  if (length(score_vec) < 2) return(c(team_one = NA_integer_, team_two = NA_integer_))
  
  parts <- str_split_fixed(score_vec, "-", 2)
  a <- as.integer(parts[, 1])
  b <- as.integer(parts[, 2])
  cur_a <- 0L
  cur_b <- 0L
  max_a <- 0L
  max_b <- 0L
  
  for (i in 2:length(a)) {
    if (a[i] > a[i - 1]) {
      cur_a <- cur_a + 1L
      cur_b <- 0L
    } else if (b[i] > b[i - 1]) {
      cur_b <- cur_b + 1L
      cur_a <- 0L
    }
    max_a <- max(max_a, cur_a)
    max_b <- max(max_b, cur_b)
  }
  c(team_one = max_a, team_two = max_b)
}

results <- df %>%
  rowwise() %>%
  mutate(
    g1_points = list(parse_score_list(game_1_scores)),
    streaks = list(max_streaks(g1_points)),
    streak_one = streaks[["team_one"]],
    streak_two = streaks[["team_two"]]
  ) %>%
  ungroup()

results <- results %>%
  separate(game_2_score, into = c("g2_one", "g2_two"),
           sep = "-", remove = FALSE, convert = TRUE, fill = "right") %>%
  mutate(
    has_valid_g2 = !is.na(g2_one) & !is.na(g2_two) & g2_one != g2_two,
    won_g2_one = ifelse(has_valid_g2, as.integer(g2_one > g2_two), NA),
    won_g2_two = ifelse(has_valid_g2, as.integer(g2_two > g2_one), NA)
  )

plot_data <- results %>%
  filter(!is.na(streak_one), !is.na(streak_two), has_valid_g2,
         !is.na(team_one_pre_match_elo), !is.na(team_two_pre_match_elo),
         !is.na(tournament_type), !is.na(round)) %>%
  mutate(streak_difference = streak_one - streak_two,
         won_next_game = won_g2_one,
         elo_difference_100 =
           (team_one_pre_match_elo - team_two_pre_match_elo) / 100)

cat("Number of matches:", nrow(plot_data), "\n")

# Non-controlled model
model_g1_streak <- glm(won_next_game ~ streak_difference,
                       data = plot_data, family = binomial)
print(summary(model_g1_streak))

# Controlled model: adjust for skill, tournament type, and round.
model_g1_streak_controlled <- glm(
  won_next_game ~ streak_difference + elo_difference_100 +
    factor(tournament_type) + factor(round),
  data = plot_data, family = binomial
)
cat("\n=== CONTROLLED MODEL ===\n")
print(summary(model_g1_streak_controlled))

# Plot for uncontrolled model
p <- ggplot(plot_data, aes(x = streak_difference, y = won_next_game)) +
  geom_count(aes(size = after_stat(n)), alpha = 0.35, color = "black") +
  scale_size_area(max_size = 14, name = "Count") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              color = "#1B9E77", fill = "grey70", linewidth = 1.1) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(-0.05, 1.05)) +
  labs(title = "Game 2 Win Probability Based on Game 1 Momentum",
       x = "Game 1 Maximum-Streak Difference (Team 1 - Team 2)",
       y = "Game 2 Win Probability") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid.minor = element_blank())

print(p)
ggsave(paste0("figures/", tolower(discipline_to_use),
              "_game1_max_streak_vs_game2_win.png"), plot = p,
       width = 9, height = 6, dpi = 150)
