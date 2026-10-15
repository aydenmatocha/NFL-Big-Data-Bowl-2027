library(tidyverse)

pbp <- read.csv("data/player_play.csv")
players <- read.csv("data/players.csv")
wr_ids <- players %>% filter(nfl_position == "WR") %>% pull(nfl_id)
combine_results <- read.csv("data/combine_results.csv")

pbp_wr <- pbp %>% filter(nfl_id %in% wr_ids)

# testing
pbp_cedric <- pbp_wr %>% filter(nfl_id == 55939) %>%
  select(game_id, play_id, nfl_id, separation_at_pass_forward, cushion, route_ran, team_coverage_man_zone)

combine_tracking <- read.csv("data/combine_tracking.csv")

# Summarize every play given a df
summarize_plays <- function(input_df) {
  play_summary <- input_df %>%
    group_by(game_id, play_id, nfl_id) %>% # should be 1 row
    group_modify(~ {
      tibble(
        actual_separation = unique(.x$separation_at_pass_forward),
        
        # variables for predicting
        cushion = .x$cushion,
        route_ran = .x$route_ran,
        team_coverage_man_zone = .x$team_coverage_man_zone
      )
    }) %>%
    ungroup()
  play_summary <- play_summary %>% filter(!is.na(actual_separation), !is.na(team_coverage_man_zone))
  return(play_summary)
}

summarized_wr <- summarize_plays(pbp_wr)
