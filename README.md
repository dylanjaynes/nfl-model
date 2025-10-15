# nfl-model
Play-by-play and drive based Monte Carlo NFL simulation model. Using a team's data this model aims to project a score for each game and subsequently a probabilistic outcome against vegas spreads and totals. 

A blend of play-by-play (pbp) data and drive level data is ultimately simmed thousands of times, random noise is injected impacting the teams' projected rushing yards, passing yards, rushing attempts, passing attempts, points per drive, and scoring rate per drive. The noise is sourced using each metrics mean and standard deviation to simulate a realistic scenario where a team may perform over / under expectations but generally within a known bound. To avoid over-confidence the model Total and Spread projections are then shocked for realistic randomness, anchored back to the market with Bayesian shrinkage, and calibrated to historical standard deviation targets. 

The three main functions so far are run_one_game(), run_weekly_report(), and evaluate_week(). 

run_one_game() exists in all the other sim functions, but can be used standalone to see a single game - especially useful for hypothetical lines

run_weekly_report() generates 2 excel files, a "best bets" file and "all games" file for the selected week. 
  
  best bets are determined by having ran the model back 6 years looking for which set of parameters (sigma, tau, degrees of freedom, weight of both models) produced the best outcomes, and from those outcomes which are trustworthy by Brier score. 

evaluate_week() is a quick function to view the inputted week's results for spreads and totals picks delivering Mean Absolute Error (MAE), Root Mean Squared Error (RMSE), and Brier Score for each as well as simple hit rate on spreads and totals. 


# Quickstart
```R
targets <- learn_target_sds(2018:2024)

wk <- run_weekly_report(season = 2024, week = 5, targets = targets, sims = 10000, (optional) bankroll = 1000 # if you'd like the Kelly suggested bet stake )

wk
```

Once the week passes, or if running a previously done week then run:
```R
evaluate_week(df_model = wk$all_games, season = 2024, week = 5)
```
# required R packages
pkgs <- c(
  "dplyr","purrr","tibble","readr","writexl","janitor",
  "nflfastR","nflreadr","future","future.apply","truncnorm"
)
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
