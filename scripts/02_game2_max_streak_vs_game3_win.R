# Uses one row per match and defines momentum as Team 1's maximum streak
# minus Team 2's maximum streak in Game 2. A positive value favors Team 1,
# and the outcome equals 1 when Team 1 wins Game 3. Using one row per match
# avoids treating the two players from the same match as independent.

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
processed_data <- read.csv(data_file, stringsAsFactors = FALSE)

processed_data <- processed_data %>%
  filter(tolower(retired) != "true",
         !is.na(game_3_score), game_3_score != "")

cat("Matches that went to a deciding Game 3:", nrow(processed_data), "\n")

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

results <- processed_data %>%
  rowwise() %>%
  mutate(
    g2_points = list(parse_score_list(game_2_scores)),
    streaks = list(max_streaks(g2_points)),
    streak_one = streaks[["team_one"]],
    streak_two = streaks[["team_two"]]
  ) %>%
  ungroup()

results <- results %>%
  separate(game_3_score, into = c("g3_one", "g3_two"),
           sep = "-", remove = FALSE, convert = TRUE, fill = "right") %>%
  mutate(
    has_valid_g3 = !is.na(g3_one) & !is.na(g3_two) & g3_one != g3_two,
    won_g3_one = ifelse(has_valid_g3, as.integer(g3_one > g3_two), NA),
    won_g3_two = ifelse(has_valid_g3, as.integer(g3_two > g3_one), NA)
  )

plot_data <- results %>%
  filter(!is.na(streak_one), !is.na(streak_two), has_valid_g3,
         !is.na(team_one_pre_match_elo), !is.na(team_two_pre_match_elo),
         !is.na(tournament_type), !is.na(round)) %>%
  mutate(streak_difference = streak_one - streak_two,
         won_next_game = won_g3_one,
         elo_difference_100 =
           (team_one_pre_match_elo - team_two_pre_match_elo) / 100)

cat("Number of matches:", nrow(plot_data), "\n")

# Uncontrolled model
model_g2_streak <- glm(won_next_game ~ streak_difference,
                       data = plot_data, family = binomial)
print(summary(model_g2_streak))

# Controlled model: adjust for skill, tournament type, and round.
model_g2_streak_controlled <- glm(
  won_next_game ~ streak_difference + elo_difference_100 +
    factor(tournament_type) + factor(round),
  data = plot_data, family = binomial
)
cat("\n=== CONTROLLED MODEL ===\n")
print(summary(model_g2_streak_controlled))

# Plot for uncontrolled model
p <- ggplot(plot_data, aes(x = streak_difference, y = won_next_game)) +
  geom_count(aes(size = after_stat(n)), alpha = 0.35, color = "black") +
  scale_size_area(max_size = 14, name = "Count") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              color = "#1B9E77", fill = "grey70", linewidth = 1.1) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(-0.05, 1.05)) +
  labs(title = "Game 3 Win Probability Based on Game 2 Momentum",
       x = "Game 2 Maximum-Streak Difference (Team 1 - Team 2)",
       y = "Game 3 Win Probability") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid.minor = element_blank())

print(p)

ggsave(paste0("figures/", tolower(discipline_to_use),
              "_game2_max_streak_vs_game3_win.png"), plot = p,
       width = 9, height = 6, dpi = 150)
