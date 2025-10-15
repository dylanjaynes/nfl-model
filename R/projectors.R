# injection of random noise from the standard deviation of relevant stats
# main driver of model

# ---- PBP projector (your YPP-style) ----
project_matchup <- function(team1_abbr, team2_abbr, season, week, pbp, team1_metrics, team2_metrics) {

  # noises
  ypc_noise_1 <- rnorm(1, 0, team2_metrics$defense_sd$sd_ypc_diff_allowed)
  ypa_noise_1 <- rnorm(1, 0, team2_metrics$defense_sd$sd_ypa_diff_allowed)
  rush_att_noise_1 <- rnorm(1, 0, team1_metrics$attempt_summary$sd_rush_attempts_per_game)
  pass_att_noise_1 <- rnorm(1, 0, team1_metrics$attempt_summary$sd_pass_attempts_per_game)
  ypc_off_noise_1 <- rnorm(1, 0, team1_metrics$offense_sd$sd_ypc_diff)
  ypa_off_noise_1 <- rnorm(1, 0, team1_metrics$offense_sd$sd_ypa_diff)

  ypc_noise_2 <- rnorm(1, 0, team1_metrics$defense_sd$sd_ypc_diff_allowed)
  ypa_noise_2 <- rnorm(1, 0, team1_metrics$defense_sd$sd_ypa_diff_allowed)
  rush_att_noise_2 <- rnorm(1, 0, team2_metrics$attempt_summary$sd_rush_attempts_per_game)
  pass_att_noise_2 <- rnorm(1, 0, team2_metrics$attempt_summary$sd_pass_attempts_per_game)
  ypc_off_noise_2 <- rnorm(1, 0, team2_metrics$offense_sd$sd_ypc_diff)
  ypa_off_noise_2 <- rnorm(1, 0, team2_metrics$offense_sd$sd_ypa_diff)

  # projections
  projected_rushing_ypc_team1 <- (mean(team1_metrics$offense_vs_defense$offensive_ypc, na.rm = TRUE) + ypc_off_noise_1) *
    (1 + mean(team2_metrics$defense_vs_offense$ypc_diff_allowed, na.rm = TRUE) + ypc_noise_1)

  projected_passing_ypa_team1 <- (mean(team1_metrics$offense_vs_defense$offensive_ypa, na.rm = TRUE) + ypa_off_noise_1) *
    (1 + mean(team2_metrics$defense_vs_offense$ypa_diff_allowed, na.rm = TRUE) + ypa_noise_1)

  projected_rushing_ypc_team2 <- (mean(team2_metrics$offense_vs_defense$offensive_ypc, na.rm = TRUE) + ypc_off_noise_2) *
    (1 + mean(team1_metrics$defense_vs_offense$ypc_diff_allowed, na.rm = TRUE) + ypc_noise_2)

  projected_passing_ypa_team2 <- (mean(team2_metrics$offense_vs_defense$offensive_ypa, na.rm = TRUE) + ypa_off_noise_2) *
    (1 + mean(team1_metrics$defense_vs_offense$ypa_diff_allowed, na.rm = TRUE) + ypa_noise_2)

  team1_ypp <- mean(c(team1_metrics$offense_ypp$yards_per_point_offense,
                      team2_metrics$defense_ypp$yards_per_point_defense), na.rm = TRUE)
  team2_ypp <- mean(c(team2_metrics$offense_ypp$yards_per_point_offense,
                      team1_metrics$defense_ypp$yards_per_point_defense), na.rm = TRUE)

  projected_rushing_yards_team1 <- projected_rushing_ypc_team1 * (team1_metrics$attempt_summary$avg_rush_attempts_per_game + rush_att_noise_1)
  projected_passing_yards_team1 <- projected_passing_ypa_team1 * (team1_metrics$attempt_summary$avg_pass_attempts_per_game + pass_att_noise_1)
  projected_total_yards_team1   <- projected_rushing_yards_team1 + projected_passing_yards_team1

  projected_rushing_yards_team2 <- projected_rushing_ypc_team2 * (team2_metrics$attempt_summary$avg_rush_attempts_per_game + rush_att_noise_2)
  projected_passing_yards_team2 <- projected_passing_ypa_team2 * (team2_metrics$attempt_summary$avg_pass_attempts_per_game + pass_att_noise_2)
  projected_total_yards_team2   <- projected_rushing_yards_team2 + projected_passing_yards_team2

  projected_score_team1 <- (projected_total_yards_team1 / team1_ypp) + 0.75
  projected_score_team2 <- (projected_total_yards_team2 / team2_ypp) - 0.75

  c(projected_score_team1 = projected_score_team1,
    projected_score_team2 = projected_score_team2)
}

# ---- Eckel projector (NA-safe) ----
project_matchup_eckel <- function(team1_abbr, team2_abbr, season, week, pbp, team1_metrics, team2_metrics) {

  drives_per_game <- round(rnorm(1, mean = 11, sd = 0.75))
  drives_per_game <- min(max(drives_per_game, 8), 14)

  # truncated draws
  sim_eckel_rate_off_team1 <- tn1(0, .85,
                                  team1_metrics$offense_average_game$average_eckel_rate,
                                  team1_metrics$eckel_off_sd$sd_eckel_rate_diff)
  sim_eckel_rate_off_team2 <- tn1(0, .85,
                                  team2_metrics$offense_average_game$average_eckel_rate,
                                  team2_metrics$eckel_off_sd$sd_eckel_rate_diff)
  sim_eckel_rate_def_team1 <- tn1(0, .85,
                                  team1_metrics$defense_average_game$average_eckel_rate,
                                  team1_metrics$eckel_def_sd$sd_eckel_allowed_diff)
  sim_eckel_rate_def_team2 <- tn1(0, .85,
                                  team2_metrics$defense_average_game$average_eckel_rate,
                                  team2_metrics$eckel_def_sd$sd_eckel_allowed_diff)

  sim_point_eckel_off_team1 <- tn1(2, 5,
                                   team1_metrics$offense_average_game$average_points_per_eckel,
                                   team1_metrics$eckel_off_sd$sd_points_per_eckel_diff)
  sim_point_eckel_off_team2 <- tn1(2, 5,
                                   team2_metrics$offense_average_game$average_points_per_eckel,
                                   team2_metrics$eckel_off_sd$sd_points_per_eckel_diff)
  sim_point_eckel_def_team1 <- tn1(2, 5,
                                   team1_metrics$defense_average_game$average_points_per_eckel,
                                   team1_metrics$eckel_def_sd$sd_points_per_eckel_allow_diff)
  sim_point_eckel_def_team2 <- tn1(2, 5,
                                   team2_metrics$defense_average_game$average_points_per_eckel,
                                   team2_metrics$eckel_def_sd$sd_points_per_eckel_allow_diff)

  rn <- function(s) { s <- sd_safe(s); if (s == 1e-3) 0 else rnorm(1, 0, s) }

  points_noise_off_1   <- rn(team1_metrics$eckel_off_sd$sd_points_per_eckel_diff)
  field_noise_off_1    <- rn(team1_metrics$eckel_off_sd$sd_field_position_diff)
  turnover_noise_off_1 <- rn(team1_metrics$eckel_off_sd$sd_turnover_rate_diff)

  points_noise_allow_1   <- rn(team1_metrics$eckel_def_sd$sd_points_per_eckel_allow_diff)
  field_noise_allow_1    <- rn(team1_metrics$eckel_def_sd$sd_field_position_allow_diff)
  turnover_noise_allow_1 <- rn(team1_metrics$eckel_def_sd$sd_turnover_rate_def_diff)

  points_noise_off_2   <- rn(team2_metrics$eckel_off_sd$sd_points_per_eckel_diff)
  field_noise_off_2    <- rn(team2_metrics$eckel_off_sd$sd_field_position_diff)
  turnover_noise_off_2 <- rn(team2_metrics$eckel_off_sd$sd_turnover_rate_diff)

  points_noise_allow_2   <- rn(team2_metrics$eckel_def_sd$sd_points_per_eckel_allow_diff)
  field_noise_allow_2    <- rn(team2_metrics$eckel_def_sd$sd_field_position_allow_diff)
  turnover_noise_allow_2 <- rn(team2_metrics$eckel_def_sd$sd_turnover_rate_def_diff)

  team1_eckel_rate <- (
    (1 + nz_mean(team2_metrics$eckel_vs_offense$eckel_rate_allowed_diff)) * sim_eckel_rate_off_team1 +
      (1 + nz_mean(team1_metrics$eckel_vs_defense$eckel_rate_diff))        * sim_eckel_rate_def_team2
  ) / 2

  team1_points_per_eckel <- (
    (1 + nz_mean(team1_metrics$eckel_vs_defense$points_per_eckel_diff))      * sim_point_eckel_off_team1 +
      (1 + nz_mean(team2_metrics$eckel_vs_offense$points_per_eckel_allowed_diff)) * sim_point_eckel_def_team2
  ) / 2

  team2_eckel_rate <- (
    (1 + nz_mean(team1_metrics$eckel_vs_offense$eckel_rate_allowed_diff)) * sim_eckel_rate_off_team2 +
      (1 + nz_mean(team2_metrics$eckel_vs_defense$eckel_rate_diff))       * sim_eckel_rate_def_team1
  ) / 2

  team2_points_per_eckel <- (
    (1 + nz_mean(team2_metrics$eckel_vs_defense$points_per_eckel_diff))        * sim_point_eckel_off_team2 +
      (1 + nz_mean(team1_metrics$eckel_vs_offense$points_per_eckel_allowed_diff)) * sim_point_eckel_def_team1
  ) / 2

  projected_score_team1 <- drives_per_game * team1_eckel_rate * team1_points_per_eckel
  projected_score_team2 <- drives_per_game * team2_eckel_rate * team2_points_per_eckel

  c(projected_score_team1 = projected_score_team1,
    projected_score_team2 = projected_score_team2)
}
