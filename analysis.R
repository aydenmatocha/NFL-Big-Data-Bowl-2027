library(tidyverse)
source("separation_model.R")

final_results <- oof_predictions %>%
  mutate(sep_residual = actual_separation - .pred) %>%
  rename(pred_separation = .pred) %>%
  select(game_id, play_id, nfl_id, actual_separation, pred_separation, sep_residual)

leaderboard <- final_results %>%
  group_by(nfl_id) %>%
  summarize(
    n_plays = n(),
    avg_cse = mean(sep_residual),
    .groups = "drop"
  ) %>%
  arrange(-avg_cse)