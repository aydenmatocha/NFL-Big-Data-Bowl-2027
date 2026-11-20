library(tidyverse)
#source("separation_model.R")
#source("combine_processing.R")

final_results <- oof_predictions %>%
  mutate(sep_residual = actual_separation - .pred) %>%
  rename(pred_separation = .pred) %>%
  select(game_id, play_id, nfl_id, actual_separation, pred_separation, sep_residual)

routes <- summarized_wr %>% select(game_id, play_id, nfl_id, route_ran)
final_results <- left_join(final_results, routes, by = c("game_id", "play_id", "nfl_id"))

route_weights <- summarized_wr %>%
  group_by(route_ran) %>%
  summarize(route_mean = mean(actual_separation), .groups = "drop") %>%
  mutate(weight = weighted.mean(summarized_wr$actual_separation) / route_mean)

final_results <- final_results %>%
  left_join(route_weights, by = "route_ran") %>%
  mutate(weighted_soe = sep_residual * weight) %>%
  select(-route_ran, -route_mean, -weight)

leaderboard <- final_results %>%
  group_by(nfl_id) %>%
  summarize(
    n_plays = n(),
    avg_soe = mean(sep_residual),
    avg_w_soe = mean(weighted_soe),
    .groups = "drop"
  ) %>%
  arrange(-avg_soe)

# add weighted soe
leaderboard <- leaderboard %>%
  mutate(
    grand = weighted.mean(avg_w_soe, n_plays),
    k = median(n_plays),
    avg_w_soe = grand + (n_plays / (n_plays + k)) * (avg_w_soe - grand)
  ) %>%
  select(-grand, -k) %>%
  arrange(-avg_w_soe)

leaderboard <- inner_join(leaderboard, wr_combine_results, by = "nfl_id")
leaderboard_100 <- leaderboard %>% filter(n_plays >= 100)
leaderboard_150 <- leaderboard %>% filter(n_plays >= 150)
leaderboard_200 <- leaderboard %>% filter(n_plays >= 200) #%>% filter(nfl_id != 57144)
leaderboard_300 <- leaderboard %>% filter(n_plays >= 300)

ggplot(leaderboard_200, aes(x = avg_w_soe, y = a_break_deep, size = n_plays)) +
    geom_point()