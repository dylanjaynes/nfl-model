# ---------- helpers ----------
.make_sim_mat <- function(home_vec, away_vec) {
  stopifnot(length(home_vec) == length(away_vec))
  m <- rbind(home = home_vec, away = away_vec)
  return(m)
}

# Default Injury Impact Table - Not studied, guessed

injury_defaults <- list(
  QB  = -0.25,  # QB out ~ 25% hit to passing/offense
  WR1 = -0.08,  # WR1 out ~ 8% hit to Eckel/offense
  RB1 = -0.05,  # RB1 out ~ 5% hit to rushing
  OL  = -0.12,  # OL cluster ~ 12% hit (rushing > passing)
  CB  = -0.10,  # CB out ~ 10% worse pass defense
  DL  = -0.08   # DL/pass rusher out ~ 8% worse pressure/run defense
)


# Injury Adjustment Helper - update to include better metrics; currently missing epa/pass epa/rush
apply_injuries <- function(team_metrics, team, injuries, defaults = injury_defaults) {
  if (!is.null(injuries) && team %in% names(injuries)) {
    for (pos in names(injuries[[team]])) {
      # Look up percentage if not explicitly provided
      adj <- injuries[[team]][[pos]]
      if (is.character(adj) && adj %in% names(defaults)) {
        adj <- defaults[[adj]]
      } else if (is.null(adj)) {
        next
      }

      # Quarterback – highest impact
      if (pos == "QB") {
        team_metrics$off_epa_pass         <- team_metrics$off_epa_pass * (1 + adj)
        team_metrics$off_eckel_rate       <- team_metrics$off_eckel_rate * (1 + adj)
        team_metrics$off_points_per_eckel <- team_metrics$off_points_per_eckel * (1 + adj/2)
      }

      # WR1 – mostly Eckel rate
      if (pos == "WR1") {
        team_metrics$off_eckel_rate <- team_metrics$off_eckel_rate * (1 + adj)
      }

      # RB1 – rushing efficiency
      if (pos == "RB1") {
        team_metrics$off_epa_rush <- team_metrics$off_epa_rush * (1 + adj)
      }

      # OL – pass + rush efficiency
      if (pos == "OL") {
        team_metrics$off_epa_pass <- team_metrics$off_epa_pass * (1 + adj/2)
        team_metrics$off_epa_rush <- team_metrics$off_epa_rush * (1 + adj)
      }

      # CB – pass defense
      if (pos == "CB") {
        team_metrics$def_epa_pass <- team_metrics$def_epa_pass * (1 - adj)
      }

      # DL – run & pass defense
      if (pos == "DL") {
        team_metrics$def_epa_pass <- team_metrics$def_epa_pass * (1 - adj/2)
        team_metrics$def_epa_rush <- team_metrics$def_epa_rush * (1 - adj)
      }
    }
  }
  return(team_metrics)
}



# returns a list(home=vec, away=vec)
.sim_eckel_once <- function(ht, at, season_use, week_use, pbp_src, sims, t1, t2) {
  sims_list <- replicate(sims, {
    s <- project_matchup_eckel(ht, at, season_use, week_use, pbp_src, t1, t2)
    c(home = s[["projected_score_team1"]], away = s[["projected_score_team2"]])
  })
  # replicate with simplify=“matrix” returns a matrix with columns = sims unless a NULL slips in
  if (is.null(dim(sims_list))) {
    # fall back (rare path if replicate collapsed oddly)
    home <- vapply(seq_len(sims), function(i) NA_real_, 1.0)
    away <- vapply(seq_len(sims), function(i) NA_real_, 1.0)
  } else {
    # sims_list is 2 x sims
    rownames(sims_list) <- c("home", "away")
    home <- sims_list["home", ]
    away <- sims_list["away", ]
  }
  list(home = as.numeric(home), away = as.numeric(away))
}

# returns a list(home=vec, away=vec)
.sim_pbp_once <- function(ht, at, season_use, week_use, pbp_src, sims, t1, t2) {
  sims_list <- replicate(sims, {
    s <- project_matchup(ht, at, season_use, week_use, pbp_src, t1, t2)
    c(home = s[["projected_score_team1"]], away = s[["projected_score_team2"]])
  })
  if (is.null(dim(sims_list))) {
    home <- vapply(seq_len(sims), function(i) NA_real_, 1.0)
    away <- vapply(seq_len(sims), function(i) NA_real_, 1.0)
  } else {
    rownames(sims_list) <- c("home", "away")
    home <- sims_list["home", ]
    away <- sims_list["away", ]
  }
  list(home = as.numeric(home), away = as.numeric(away))
}

# optional variance calibration to target SDs
.calibrate_sd <- function(x, s_target) {
  if (!is.finite(s_target) || is.na(s_target)) return(x)
  s_model <- stats::sd(x, na.rm = TRUE)
  s_extra <- sqrt(pmax(0, s_target^2 - s_model^2))
  if (!is.finite(s_extra) || s_extra <= 0) return(x)
  x + stats::rnorm(length(x), 0, s_extra)
}

# safe percentiles
.pct_q <- function(x) stats::quantile(x, probs = c(.25, .5, .75), na.rm = TRUE, names = FALSE)

priors <- list(tau_spread = 12,
                                    sigma_spread = 6.5,
                                    df_spread = 6,
                                    sigma_total = 3,
                                    df_total = 6,
                                    w_hybrid = 0.4)

# ---------- main ----------
run_one_game <- function(season_target, week,
                         home_team, away_team,
                         sims = 10000,
                         w_eckel = 0.30,      # weight on Eckel; (1 - w_eckel) on PBP
                         targets = NULL,      # from learn_target_sds(); or NULL to skip
                         vegas_spread_line = NA_real_,
                         vegas_total_line  = NA_real_,
                         sched_row = NULL,     # <- NEW: pass a schedules row to carry id/date/time/lines
                         tau_spread = 12,
                         sigma_spread = 6.5,
                         df_spread = 6,
                         sigma_total = 3,
                         df_total = 6,
                         w_hybrid = 0.4,
                         injuries = NULL,
                         pbp_src = NULL,
                         pbp_season_to_use = NULL
                         ) {
  stopifnot(w_eckel >= 0, w_eckel <= 1)

  ## ---- schedule metadata & default lines (if provided) ----
  if (!is.null(sched_row)) {
    if (is.na(vegas_spread_line) && "spread_line" %in% names(sched_row))
      vegas_spread_line <- sched_row$spread_line
    if (is.na(vegas_total_line) && "total_line" %in% names(sched_row))
      vegas_total_line <- sched_row$total_line

    gameday  <- if ("gameday"  %in% names(sched_row))  as.character(sched_row$gameday)  else NA_character_
    gametime <- if ("gametime" %in% names(sched_row))  as.character(sched_row$gametime) else NA_character_
    game_id  <- if ("game_id"  %in% names(sched_row))  as.character(sched_row$game_id)
    else sprintf("%d_%02d_%s_%s", season_target, week, home_team, away_team)
  } else {
    gameday <- gametime <- NA_character_
    game_id <- sprintf("%d_%02d_%s_%s", season_target, week, home_team, away_team)
  }

  ## ---- choose PBP season (prior year if week < 4) ----
  # decide season if not passed
  if (is.null(pbp_season_to_use))
    pbp_season_to_use <- if (!is.na(week) && week < 4) season_target - 1 else season_target

  # load/calc pbp if caller didn’t pass it (fallback)
  if (is.null(pbp_src)) {
    pbp_src <- get_pbp_cached(pbp_season_to_use)
  }
  pbp_src <- pbp_src |>
    dplyr::mutate(week = as.numeric(week)) |>
    dplyr::filter(week != 18)
  metrics_week <- if (season_target != pbp_season_to_use) max(pbp_src$week, na.rm = TRUE) else week

  ## ---- team metrics once ----
  t1 <- calculate_team_metrics(home_team, pbp_season_to_use, metrics_week, pbp_src)
  t2 <- calculate_team_metrics(away_team, pbp_season_to_use, metrics_week, pbp_src)

  ## ____---- Injury component -----
  t1 <- apply_injuries(t1, home_team, injuries)
  t2 <- apply_injuries(t2, away_team, injuries)

  ## ---- simulate Eckel / PBP as needed ----
  have_eckel <- (w_eckel > 0)
  have_pbp   <- ((1 - w_eckel) > 0)

  if (have_eckel) {
    se <- .sim_eckel_once(home_team, away_team, pbp_season_to_use, metrics_week, pbp_src, sims, t1, t2)
  } else {
    se <- list(home = numeric(sims), away = numeric(sims))
  }

  if (have_pbp) {
    sp <- .sim_pbp_once(home_team, away_team, pbp_season_to_use, metrics_week, pbp_src, sims, t1, t2)
  } else {
    sp <- list(home = numeric(sims), away = numeric(sims))
  }

  ## ---- blend samples ----
  home_blend <- w_eckel * se$home + (1 - w_eckel) * sp$home
  away_blend <- w_eckel * se$away + (1 - w_eckel) * sp$away

  pre_spread_draws <- home_blend - away_blend
  pre_total_draws  <- home_blend + away_blend


  # Draw fat-tailed shocks (t-dist) for realism
  rt_ <- function(n, df, scale = 1) scale * as.numeric(stats::rt(n, df = df))


  g_shock  <- rt_(sims, df_total,  sigma_total)   # same sign to both
  s_shock  <- rt_(sims, df_spread, sigma_spread)  # opposite sign

  home_blend <- home_blend + g_shock +  0.5 * s_shock
  away_blend <- away_blend + g_shock + -0.5 * s_shock


  spread_draws <- home_blend - away_blend
  total_draws  <- home_blend + away_blend


  ## ---- optional variance calibration to historical SDs ----
  if (!is.null(targets)) {
    wk_row <- dplyr::filter(targets$by_week, week == !!week)
    s_target_spread <- if (nrow(wk_row) && is.finite(wk_row$spread_sd_target)) wk_row$spread_sd_target else targets$overall$spread_sd_overall
    s_target_total  <- if (nrow(wk_row) && is.finite(wk_row$total_sd_target))  wk_row$total_sd_target  else targets$overall$total_sd_overall

    # --- hybrid variance (blend structured + market-aware)
    s_model_spread <- sd(spread_draws, na.rm = TRUE)
    s_model_total  <- sd(total_draws,  na.rm = TRUE)


    s_hybrid_spread <- (1 - w_hybrid) * s_model_spread + w_hybrid * s_target_spread
    s_hybrid_total  <- (1 - w_hybrid) * s_model_total  + w_hybrid * s_target_total

    spread_draws <- .calibrate_sd(spread_draws, s_hybrid_spread)
    total_draws  <- .calibrate_sd(total_draws,  s_hybrid_total)
  }


  ## ---- recompute implied team scores from calibrated spread/total ----
  home_cal <- (total_draws + spread_draws) / 2
  away_cal <- (total_draws - spread_draws) / 2


  # ----- Market-anchored mean shrink (Bayesian) -----
  mu_model <- mean(spread_draws, na.rm = TRUE)
  var_model <- stats::var(spread_draws, na.rm = TRUE)

  mu_prior   <- vegas_spread_line

  if (is.finite(var_model) && var_model > 0 && is.finite(mu_prior)) {
    mu_post <- (mu_model/var_model + mu_prior/(tau_spread^2)) /
      (1/var_model + 1/(tau_spread^2))
    delta <- mu_post - mu_model

    # shift spread draws; keep total draws; adjust implied team scores
    spread_draws <- spread_draws + delta
    # if you already constructed home_cal/away_cal, adjust them:
    home_cal <- home_cal + 0.5 * delta
    away_cal <- away_cal - 0.5 * delta
  }


  ## ---- summary stats ----
  sm <- tibble::tibble(
    game_id = game_id,
    season_target = season_target,
    week = week,
    pbp_source_season = pbp_season_to_use,
    metrics_week = metrics_week,
    gameday = gameday,
    gametime = gametime,
    home_team = home_team,
    away_team = away_team,
    sims = sims,
   # spreads = c(spread_draws, na.rm = TRUE),
    home_mean = mean(home_cal, na.rm = TRUE),
    away_mean = mean(away_cal, na.rm = TRUE),
    spread_mean = mean(spread_draws, na.rm = TRUE),
    total_mean  = mean(total_draws,  na.rm = TRUE),
    home_sd = stats::sd(home_cal, na.rm = TRUE),
    away_sd = stats::sd(away_cal, na.rm = TRUE),
    spread_sd = stats::sd(spread_draws, na.rm = TRUE),
    total_sd  = stats::sd(total_draws,  na.rm = TRUE),
    pre_sd_spread = sd(pre_spread_draws, na.rm = TRUE),
    pre_sd_total = sd(pre_total_draws, na.rm = TRUE),
    na_rate_home   = mean(!is.finite(home_cal)),
    na_rate_away   = mean(!is.finite(away_cal)),
    na_rate_spread = mean(!is.finite(spread_draws)),
    na_rate_total  = mean(!is.finite(total_draws))
  )

  ## ---- percentiles ----
  hq <- .pct_q(home_cal); aq <- .pct_q(away_cal)
  sq <- .pct_q(spread_draws); tq <- .pct_q(total_draws)
  sm$home_p25 <- hq[1]; sm$home_p50 <- hq[2]; sm$home_p75 <- hq[3]
  sm$away_p25 <- aq[1]; sm$away_p50 <- aq[2]; sm$away_p75 <- aq[3]
  sm$spread_p25 <- sq[1]; sm$spread_p50 <- sq[2]; sm$spread_p75 <- sq[3]
  sm$total_p25 <- tq[1]; sm$total_p50 <- tq[2]; sm$total_p75 <- tq[3]

  ## ---- betting probabilities (IMPORTANT: spread_line is **home perspective**) ----
  sm$vegas_spread_line <- vegas_spread_line
  sm$vegas_total_line  <- vegas_total_line



  # ----- ATS block -----
  if (is.na(vegas_spread_line)) {
    sm$home_cover_prob <- NA_real_
    sm$away_cover_prob <- NA_real_
    sm$ATS_pick <- NA_character_
    sm$ATS_prob <- NA_real_
    sm$cover_prob <- NA_real_
    sm$ATS_edge <- NA_real_
  } else {
    home_cover_prob <- mean(spread_draws - vegas_spread_line > 0, na.rm = TRUE)
    away_cover_prob <- 1 - home_cover_prob

    # FORCE SCALARS
    hc <- as.numeric(home_cover_prob)[1]
    ac <- as.numeric(away_cover_prob)[1]

    sm$home_cover_prob <- hc
    sm$away_cover_prob <- ac
    sm$ATS_pick <- if (isTRUE(hc >= ac)) home_team else away_team
    sm$ATS_prob <- max(hc, ac)
    sm$cover_prob <- dplyr::case_when(
      sm$ATS_pick == home_team ~ hc,
      sm$ATS_pick == away_team ~ ac,
      TRUE ~ NA_real_
    )
    sm$ATS_edge <- sm$ATS_prob - 0.5
  }

  # ----- Totals block -----
  if (is.na(vegas_total_line)) {
    sm$over_prob <- NA_real_
    sm$under_prob <- NA_real_
    sm$Total_pick <- NA_character_
    sm$Total_prob <- NA_real_
    sm$Total_edge <- NA_real_
  } else {
    over_prob  <- mean(total_draws > vegas_total_line, na.rm = TRUE)
    under_prob <- 1 - over_prob

    # FORCE SCALARS
    po <- as.numeric(over_prob)[1]
    pu <- as.numeric(under_prob)[1]

    sm$over_prob  <- po
    sm$under_prob <- pu
    sm$Total_pick <- if (isTRUE(po >= pu)) "Over" else "Under"
    sm$Total_prob <- max(po, pu)
    sm$Total_edge <- sm$Total_prob - 0.5
  }


  sm
}
