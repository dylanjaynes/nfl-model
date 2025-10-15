#quickstart.R
set.seed(42)
source("R/utils.R"); source("R/run_week.R"); source("R/week_runner.R")
source("R/best_bet.R"); source("R/evaluate_week.R"); source("R/blend.R");
source("R/calibration.R"); source("R/metrics.R"); source("R/projectors.R");

targets <- learn_target_sds(2018:2024)
wk <- run_weekly_report(season = 2025, week = 5, targets = targets, sims = 2000, bankroll = 500)
print(head(wk$card, 10))
ev <- evaluate_week(wk$all_games, season_target = 2025, week_target = 5)
print(ev$summary)
