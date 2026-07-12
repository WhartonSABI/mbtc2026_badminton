# Compute pre-match standard and weighted Elo ratings for singles datasets
#
# Four columns are added to each dataset:
#   team_one_pre_match_elo
#   team_two_pre_match_elo
#   team_one_pre_match_weighted_elo
#   team_two_pre_match_weighted_elo
#
# Standard and weighted Elo are calculated separately for men's
# singles and women's singles. All players begin at 1500.

# rm(list=ls())

baseline_elo <- 1500
k_factor <- 72
gamma <- 0.5

round_order <- c(
  "Qualification round of 32" = 1,
  "Qualification round of 16" = 2,
  "Qualification quarter final" = 3,
  "Round of 64" = 4,
  "Round of 32" = 5,
  "Round of 16" = 6,
  "Quarter final" = 7,
  "Semi final" = 8,
  "Final" = 9,
  "Round 1" = 4,
  "Round 2" = 5,
  "Round 3" = 6
)

convert_to_logical <- function(x) {
  x_clean <- tolower(trimws(as.character(x)))
  result <- rep(NA, length(x_clean))
  result[x_clean %in% c("true", "t", "1", "yes", "y")] <- TRUE
  result[x_clean %in% c("false", "f", "0", "no", "n")] <- FALSE
  return(result)
}

get_rating <- function(ratings, player) {
  if (player %in% names(ratings)) {
    return(as.numeric(ratings[player]))
  }
  return(baseline_elo)
}

add_change <- function(changes, player, amount) {
  if (!(player %in% names(changes))) {
    changes[player] <- 0
  }
  changes[player] <- changes[player] + amount
  return(changes)
}

compute_elo_columns <- function(input_file, output_file) {

  matches <- read.csv(
    input_file,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  required_columns <- c(
    "date", "round", "winner", "nb_sets", "retired",
    "team_one_players", "team_two_players",
    "team_one_total_points", "team_two_total_points"
  )

  missing_columns <- setdiff(required_columns, names(matches))

  if (length(missing_columns) > 0) {
    stop(
      paste(
        "Missing required columns:",
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  # Convert "False"/"True" text to logical FALSE/TRUE.
  matches$.retired_logical <- convert_to_logical(matches$retired)

  if (any(is.na(matches$.retired_logical))) {
    unexpected_values <- unique(
      matches$retired[is.na(matches$.retired_logical)]
    )

    stop(
      paste(
        "Unexpected values found in retired:",
        paste(unexpected_values, collapse = ", ")
      )
    )
  }

  matches$.original_row <- seq_len(nrow(matches))
  matches$.match_date <- as.Date(matches$date, format = "%d-%m-%Y")

  if (any(is.na(matches$.match_date))) {
    stop(
      paste(
        "At least one date in",
        input_file,
        "could not be read using DD-MM-YYYY."
      )
    )
  }

  matches$.round_order <- unname(round_order[matches$round])
  matches$.round_order[is.na(matches$.round_order)] <- 99

  matches <- matches[
    order(
      matches$.match_date,
      matches$.round_order,
      matches$.original_row
    ),
  ]

  matches$team_one_pre_match_elo <- NA_real_
  matches$team_two_pre_match_elo <- NA_real_
  matches$team_one_pre_match_weighted_elo <- NA_real_
  matches$team_two_pre_match_weighted_elo <- NA_real_

  standard_ratings <- setNames(numeric(0), character(0))
  weighted_ratings <- setNames(numeric(0), character(0))

  matches$.date_round_group <- paste(
    format(matches$.match_date, "%Y-%m-%d"),
    matches$.round_order,
    sep = "_"
  )

  groups <- unique(matches$.date_round_group)

  for (group in groups) {

    rows <- which(matches$.date_round_group == group)

    standard_changes <- setNames(numeric(0), character(0))
    weighted_changes <- setNames(numeric(0), character(0))

    for (i in rows) {

      player_one <- matches$team_one_players[i]
      player_two <- matches$team_two_players[i]

      if (
        is.na(player_one) || is.na(player_two) ||
        player_one == "" || player_two == ""
      ) {
        stop(
          paste(
            "Missing player name in original row",
            matches$.original_row[i]
          )
        )
      }

      standard_one <- get_rating(standard_ratings, player_one)
      standard_two <- get_rating(standard_ratings, player_two)
      weighted_one <- get_rating(weighted_ratings, player_one)
      weighted_two <- get_rating(weighted_ratings, player_two)

      matches$team_one_pre_match_elo[i] <- standard_one
      matches$team_two_pre_match_elo[i] <- standard_two
      matches$team_one_pre_match_weighted_elo[i] <- weighted_one
      matches$team_two_pre_match_weighted_elo[i] <- weighted_two

      completed_match <- (
        !matches$.retired_logical[i] &&
        matches$winner[i] %in% c(1, 2) &&
        matches$nb_sets[i] %in% c(2, 3) &&
        !is.na(matches$team_one_total_points[i]) &&
        !is.na(matches$team_two_total_points[i]) &&
        (
          matches$team_one_total_points[i] +
          matches$team_two_total_points[i]
        ) > 0
      )

      if (!completed_match) {
        next
      }

      actual_one <- ifelse(matches$winner[i] == 1, 1, 0)

      # Standard Elo update
      expected_one_standard <- 1 / (
        1 + 10^((standard_two - standard_one) / 400)
      )

      standard_change_one <- k_factor * (
        actual_one - expected_one_standard
      )

      standard_changes <- add_change(
        standard_changes, player_one, standard_change_one
      )

      standard_changes <- add_change(
        standard_changes, player_two, -standard_change_one
      )

      # Weighted Elo update
      expected_one_weighted <- 1 / (
        1 + 10^((weighted_two - weighted_one) / 400)
      )

      if (matches$winner[i] == 1) {
        winner_points <- matches$team_one_total_points[i]
        loser_points <- matches$team_two_total_points[i]
      } else {
        winner_points <- matches$team_two_total_points[i]
        loser_points <- matches$team_one_total_points[i]
      }

      winner_games <- 2
      loser_games <- matches$nb_sets[i] - 2

      game_margin <- (
        winner_games - loser_games
      ) / (
        winner_games + loser_games
      )

      point_margin <- (
        winner_points - loser_points
      ) / (
        winner_points + loser_points
      )

      dominance_score <- (
        0.5 * game_margin +
        0.5 * point_margin
      )

      # Keep the score between 0 and 1.
      dominance_score <- max(0, min(1, dominance_score))

      margin_multiplier <- 1 + gamma * dominance_score

      weighted_change_one <- (
        k_factor *
        margin_multiplier *
        (actual_one - expected_one_weighted)
      )

      weighted_changes <- add_change(
        weighted_changes, player_one, weighted_change_one
      )

      weighted_changes <- add_change(
        weighted_changes, player_two, -weighted_change_one
      )
    }

    # Apply all updates only after all matches in this date-round group.
    for (player in names(standard_changes)) {
      standard_ratings[player] <- (
        get_rating(standard_ratings, player) +
        standard_changes[player]
      )
    }

    for (player in names(weighted_changes)) {
      weighted_ratings[player] <- (
        get_rating(weighted_ratings, player) +
        weighted_changes[player]
      )
    }
  }

  matches <- matches[order(matches$.original_row), ]

  matches$.retired_logical <- NULL
  matches$.original_row <- NULL
  matches$.match_date <- NULL
  matches$.round_order <- NULL
  matches$.date_round_group <- NULL

  write.csv(
    matches,
    output_file,
    row.names = FALSE,
    na = ""
  )

  message(
    "Saved ",
    nrow(matches),
    " rows to ",
    output_file
  )

  return(invisible(matches))
}

compute_elo_columns(
  input_file = "data/ms.csv",
  output_file = "data/processed/ms_with_elo.csv"
)

compute_elo_columns(
  input_file = "data/ws.csv",
  output_file = "data/processed/ws_with_elo.csv"
)


################################
# read in the processed datasets and check the new columns
################################

ms_elo <- read.csv("data/processed/ms_with_elo.csv", stringsAsFactors = FALSE)
ws_elo <- read.csv("data/processed/ws_with_elo.csv", stringsAsFactors = FALSE)

summary(ms_elo$team_one_pre_match_elo)
summary(ms_elo$team_two_pre_match_elo)

summary(ms_elo$team_one_pre_match_weighted_elo)
summary(ms_elo$team_two_pre_match_weighted_elo)


summary(ws_elo$team_one_pre_match_elo)
summary(ws_elo$team_two_pre_match_elo)

summary(ws_elo$team_one_pre_match_weighted_elo)
summary(ws_elo$team_two_pre_match_weighted_elo)

# check distn of differences
ms_elo <- ms_elo |>
  mutate(
    standard_elo_difference =
      team_one_pre_match_elo -
      team_two_pre_match_elo,
    
    weighted_elo_difference =
      team_one_pre_match_weighted_elo -
      team_two_pre_match_weighted_elo
  )

summary(ms_elo$standard_elo_difference)
summary(ms_elo$weighted_elo_difference)

hist(ms_elo$standard_elo_difference)
hist(ms_elo$weighted_elo_difference)


################################
## check to see whether elo or weighted elo is more predictive of match outcomes
################################

# Use 2018 to build the Elo ratings, then evaluate matches from 2019 onward.
evaluation_start_date <- as.Date("2019-01-01")

# ----------------------------
# Men's singles
# ----------------------------

ms_test <- ms_elo |>
  mutate(
    match_date = as.Date(date, format = "%d-%m-%Y"),
    
    # Actual result:
    # 1 if Team One won, 0 if Team Two won
    team_one_won = ifelse(winner == 1, 1, 0),
    
    # Predicted probability that Team One wins
    standard_probability = 1 / (
      1 + 10^(
        (
          team_two_pre_match_elo -
            team_one_pre_match_elo
        ) / 400
      )
    ),
    
    weighted_probability = 1 / (
      1 + 10^(
        (
          team_two_pre_match_weighted_elo -
            team_one_pre_match_weighted_elo
        ) / 400
      )
    )
  ) |>
  filter(
    retired == "False",
    winner %in% c(1, 2),
    match_date >= evaluation_start_date
  )


# RMSE of the predicted probabilities
# Lower RMSE means better predictions.

ms_standard_rmse <- sqrt(
  mean(
    (
      ms_test$team_one_won -
        ms_test$standard_probability
    )^2
  )
)

ms_weighted_rmse <- sqrt(
  mean(
    (
      ms_test$team_one_won -
        ms_test$weighted_probability
    )^2
  )
)


# Predict the match winner.
# Predictions of exactly 0.50 are counted as ties and excluded.

ms_test$standard_prediction <- ifelse(
  ms_test$standard_probability > 0.5,
  1,
  ifelse(
    ms_test$standard_probability < 0.5,
    0,
    NA
  )
)

ms_test$weighted_prediction <- ifelse(
  ms_test$weighted_probability > 0.5,
  1,
  ifelse(
    ms_test$weighted_probability < 0.5,
    0,
    NA
  )
)


# Accuracy
# Higher accuracy means better predictions.

ms_standard_accuracy <- mean(
  ms_test$standard_prediction ==
    ms_test$team_one_won,
  na.rm = TRUE
)

ms_weighted_accuracy <- mean(
  ms_test$weighted_prediction ==
    ms_test$team_one_won,
  na.rm = TRUE
)


# ----------------------------
# Women's singles
# ----------------------------

ws_test <- ws_elo |>
  mutate(
    match_date = as.Date(date, format = "%d-%m-%Y"),
    
    team_one_won = ifelse(winner == 1, 1, 0),
    
    standard_probability = 1 / (
      1 + 10^(
        (
          team_two_pre_match_elo -
            team_one_pre_match_elo
        ) / 400
      )
    ),
    
    weighted_probability = 1 / (
      1 + 10^(
        (
          team_two_pre_match_weighted_elo -
            team_one_pre_match_weighted_elo
        ) / 400
      )
    )
  ) |>
  filter(
    retired == "False",
    winner %in% c(1, 2),
    match_date >= evaluation_start_date
  )


ws_standard_rmse <- sqrt(
  mean(
    (
      ws_test$team_one_won -
        ws_test$standard_probability
    )^2
  )
)

ws_weighted_rmse <- sqrt(
  mean(
    (
      ws_test$team_one_won -
        ws_test$weighted_probability
    )^2
  )
)


ws_test$standard_prediction <- ifelse(
  ws_test$standard_probability > 0.5,
  1,
  ifelse(
    ws_test$standard_probability < 0.5,
    0,
    NA
  )
)

ws_test$weighted_prediction <- ifelse(
  ws_test$weighted_probability > 0.5,
  1,
  ifelse(
    ws_test$weighted_probability < 0.5,
    0,
    NA
  )
)


ws_standard_accuracy <- mean(
  ws_test$standard_prediction ==
    ws_test$team_one_won,
  na.rm = TRUE
)

ws_weighted_accuracy <- mean(
  ws_test$weighted_prediction ==
    ws_test$team_one_won,
  na.rm = TRUE
)


# ----------------------------
# Put results in one table
# ----------------------------

elo_comparison <- data.frame(
  discipline = c(
    "Men's singles",
    "Men's singles",
    "Women's singles",
    "Women's singles"
  ),
  
  elo_method = c(
    "Standard Elo",
    "Weighted Elo",
    "Standard Elo",
    "Weighted Elo"
  ),
  
  rmse = c(
    ms_standard_rmse,
    ms_weighted_rmse,
    ws_standard_rmse,
    ws_weighted_rmse
  ),
  
  accuracy = c(
    ms_standard_accuracy,
    ms_weighted_accuracy,
    ws_standard_accuracy,
    ws_weighted_accuracy
  )
)

elo_comparison

write.csv(elo_comparison, "data/processed/singles_elo_comparison.csv")
