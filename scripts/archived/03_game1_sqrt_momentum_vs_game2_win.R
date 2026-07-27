# Uses one row per match. Momentum is calculated from Team 1's perspective:
# Team 1's square-root momentum gains minus Team 2's gains. Thus, positive
# momentum favors Team 1. The outcome equals 1 when Team 1 wins Game 2.
# Using one row per match avoids duplicating the match with a mirrored row.

library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)

# Choose "MS" for men's singles or "WS" for women's singles.
discipline_to_use <- "WS"

if (discipline_to_use == "MS") {
  data_file <- "data/processed/ms_with_elo.csv"
} else if (discipline_to_use == "WS") {
  data_file <- "data/processed/ws_with_elo.csv"
} else {
  stop('discipline_to_use must be either "MS" or "WS".')
}
ms <- read.csv(data_file, stringsAsFactors = FALSE)
ms <- ms %>% filter(tolower(retired) != "true")

ms$game_1_scores <- gsub("[", "", ms$game_1_scores, fixed = TRUE)
ms$game_1_scores <- gsub("]", "", ms$game_1_scores, fixed = TRUE)
ms$game_1_scores <- gsub("'", "", ms$game_1_scores, fixed = TRUE)
ms$game_1_scores <- gsub("\\s+", "", ms$game_1_scores)

max_points <- max(str_count(ms$game_1_scores, ","), na.rm = TRUE) + 1
point_cols <- paste0("point_", 1:max_points)

ms_split <- ms %>%
  separate_wider_delim(cols = game_1_scores, delim = ",", names = point_cols,
                       too_few = "align_start", cols_remove = FALSE)

get_p1_score <- function(score) as.numeric(str_extract(score, "^\\d+"))
get_p2_score <- function(score) as.numeric(str_extract(score, "\\d+$"))

final_momentum <- rep(NA, nrow(ms_split))

for (row_idx in 1:nrow(ms_split)) {
  p1_streak <- 0
  p2_streak <- 0
  game_momentum <- 0
  
  for (i in 2:max_points) {
    prev_str <- ms_split[[row_idx, paste0("point_", i - 1)]]
    curr_str <- ms_split[[row_idx, paste0("point_", i)]]
    if (is.na(prev_str) || is.na(curr_str)) next
    
    prev_p1 <- get_p1_score(prev_str)
    curr_p1 <- get_p1_score(curr_str)
    prev_p2 <- get_p2_score(prev_str)
    curr_p2 <- get_p2_score(curr_str)
    if (is.na(prev_p1) || is.na(curr_p1) || is.na(prev_p2) || is.na(curr_p2)) next
    
    if (curr_p1 > prev_p1) {
      p1_streak <- p1_streak + 1
      p2_streak <- 0
      game_momentum <- game_momentum + 1 + sqrt(p1_streak)
    } else if (curr_p2 > prev_p2) {
      # Original code used a plain else statement and therefore treated any
      # unrecognized score as a Team 2 point. Check Team 2's score directly.
      p2_streak <- p2_streak + 1
      p1_streak <- 0
      game_momentum <- game_momentum - 1 - sqrt(p2_streak)
    }
  }
  final_momentum[row_idx] <- game_momentum
}

ms_split$momentum <- final_momentum

ms_split <- ms_split %>%
  separate(game_2_score, into = c("g2_one", "g2_two"), sep = "-",
           remove = FALSE, convert = TRUE, fill = "right") %>%
  mutate(
    has_valid_g2 = !is.na(g2_one) & !is.na(g2_two) & g2_one != g2_two,
    p1_won_next_game = ifelse(has_valid_g2, as.integer(g2_one > g2_two), NA),
    p2_won_next_game = ifelse(has_valid_g2, as.integer(g2_two > g2_one), NA)
  )

analysis_data <- ms_split %>%
  filter(!is.na(momentum), !is.na(p1_won_next_game),
         !is.na(team_one_pre_match_elo), !is.na(team_two_pre_match_elo),
         !is.na(tournament_type), !is.na(round)) %>%
  transmute(
    momentum,
    won_next = p1_won_next_game,
    elo_difference_100 =
      (team_one_pre_match_elo - team_two_pre_match_elo) / 100,
    tournament_type,
    round
  )

cat("Number of matches:", nrow(analysis_data), "\n")

# Uncontrolled model
model_g1_logit <- glm(won_next ~ momentum, data = analysis_data,
                      family = binomial)
print(summary(model_g1_logit))

# Controlled model: adjust for skill, tournament type, and round.
model_g1_logit_controlled <- glm(
  won_next ~ momentum + elo_difference_100 +
    factor(tournament_type) + factor(round),
  data = analysis_data, family = binomial
)
cat("\n=== CONTROLLED MODEL ===\n")
print(summary(model_g1_logit_controlled))

regression_plot <- ggplot(analysis_data, aes(x = momentum, y = won_next)) +
  geom_jitter(height = 0.03, alpha = 0.15, color = "gray40") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              color = "#2ca25f", linewidth = 1.2, fill = "gray80", se = TRUE) +
  coord_cartesian(ylim = c(-0.05, 1.05)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  labs(title = "Game 2 Win Probability Based on Game 1 Momentum",
       x = "Game 1 Square-Root Momentum (Team 1 - Team 2)",
       y = "Probability Team 1 Wins Game 2") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title = element_text(face = "bold"))

print(regression_plot)

ggsave(paste0("figures/", tolower(discipline_to_use),
              "_game1_sqrt_momentum_vs_game2_win.png"),
       plot = regression_plot,
       width = 9, height = 6, dpi = 150)
