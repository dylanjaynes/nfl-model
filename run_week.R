run_week <- function(season_target, week, sims = 10000,
                     w_eckel = 0.3,
                     targets = NULL,
                     vegas_lines = NULL,   # optional tibble
                     outfile = NULL,
                     tau_spread = 12,
                     sigma_spread = 6.5,
                     df_spread = 6,
                     sigma_total = 3,
                     df_total = 6,
                     w_hybrid = 0.5,
                     injuries = NULL,
                     pbp_cache_dir = "cache/pbp") {

  pbp_season_to_use <- if (!is.na(week) && week < 4) season_target - 1 else season_target
  through_req <- if (pbp_season_to_use == season_target) week else Inf

  pbp_src <- get_pbp_cached(
    season      = pbp_season_to_use,
    through_week= through_req,
    cache_dir   = pbp_cache_dir,
    refresh     = "auto"
  )

  # Load schedule
  sched <- nflreadr::load_schedules(season_target) %>%
    dplyr::filter(week == !!week, game_type == "REG") %>%
    dplyr::arrange(gameday, gametime, home_team)

  # Join in vegas_lines if provided
  if (!is.null(vegas_lines)) {
    sched <- sched %>%
      dplyr::select(game_id, season, week, gameday, gametime,
                    home_team, away_team, spread_line, total_line) %>%
      dplyr::left_join(
        vegas_lines %>% dplyr::select(home_team, away_team, week, spread_line, total_line),
        by = c("home_team", "away_team", "week"),
        relationship = "many-to-many"
      ) %>%
      dplyr::mutate(
        spread_line = dplyr::coalesce(spread_line.y, spread_line.x),
        total_line  = dplyr::coalesce(total_line.y,  total_line.x)
      ) %>%
      dplyr::select(-ends_with(".x"), -ends_with(".y"))
  } else {
    sched <- sched %>%
      dplyr::select(game_id, season, week, gameday, gametime,
                    home_team, away_team, spread_line, total_line)
  }

  # Run all games
  results <- purrr::map_dfr(seq_len(nrow(sched)), function(i) {
    row <- sched[i, ]
    run_one_game(
      season_target = season_target,
      week = week,
      home_team = row$home_team,
      away_team = row$away_team,
      sims = sims,
      pbp_src = pbp_src,
      pbp_season_to_use = pbp_season_to_use,
      w_eckel = w_eckel,
      targets = targets,
      vegas_spread_line = row$spread_line,
      vegas_total_line  = row$total_line,
      sched_row = row,
      tau_spread = tau_spread,
      sigma_spread = sigma_spread,
      df_spread = df_spread,
      sigma_total = sigma_total,
      df_total = df_total,
      w_hybrid = w_hybrid,
      injuries = injuries
    )
  })

  # Add cover prob for sorting
  results <- results %>%
    dplyr::mutate(
      cover_prob = dplyr::case_when(
        ATS_pick == home_team ~ home_cover_prob,
        ATS_pick == away_team ~ away_cover_prob,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::arrange(dplyr::desc(cover_prob), gameday, gametime, home_team)

  # Variance check
  spread_pre  <- mean(results$pre_sd_spread, na.rm = TRUE)
  spread_post <- mean(results$spread_sd, na.rm = TRUE)
  total_pre   <- mean(results$pre_sd_total, na.rm = TRUE)
  total_post  <- mean(results$total_sd, na.rm = TRUE)

  cat(sprintf(
    "\n=== Week %d variance check ===\nSpread SD (pre/post): %.2f → %.2f\nTotal SD  (pre/post): %.2f → %.2f\n",
    week, spread_pre, spread_post, total_pre, total_post
  ))


  # Keep only relevant fields for export
  export_results <- results %>%
    dplyr::select(
      game_id, week, home_team, away_team,
      home_mean, away_mean,
      ATS_pick, ATS_prob,
      vegas_spread_line,
      Total_pick, Total_prob,
      vegas_total_line
    ) %>%
    dplyr::mutate(
      home_mean = round(home_mean, 0),
      away_mean = round(away_mean, 0),
      ATS_prob = round(ATS_prob * 100, 1),     # convert to %
      Total_prob = round(Total_prob * 100, 1)  # convert to %
    )


  # Save if requested
  if (!is.null(outfile)) {
    writexl::write_xlsx(export_results, outfile)
    message("Saved week results to: ", outfile)
  }

  results
}
