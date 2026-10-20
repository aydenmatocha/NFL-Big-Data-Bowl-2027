library(tidyverse)

pbp <- read.csv("data/player_play.csv")
players <- read.csv("data/players.csv")
wr_ids <- players %>% filter(nfl_position == "WR") %>% pull(nfl_id)
combine_results <- read.csv("data/combine_results.csv")
game_tracking_2023 <- read.csv("data/game_tracking_2023.csv")
game_tracking_2024 <- read.csv("data/game_tracking_2024.csv")
game_tracking_2025 <- read.csv("data/game_tracking_2025.csv")
game_tracking <- bind_rows(game_tracking_2023, game_tracking_2024, game_tracking_2025)

pbp_wr <- pbp %>% filter(nfl_id %in% wr_ids, team_coverage_man_zone == "MAN_COVERAGE")
game_tracking_wr <- game_tracking %>% filter(nfl_id %in% wr_ids)

pbp_wr_tracking <- pbp_wr %>% left_join(game_tracking_wr, by = c("game_id", "play_id", "nfl_id"))

# testing
pbp_cedric <- pbp_wr_tracking %>% filter(nfl_id == 55939) %>%
  select(game_id, play_id, nfl_id, separation_at_pass_forward, cushion, route_ran, 
         team_coverage_type, offense_formation, receiver_alignment, in_motion_at_ball_snap, 
         x, y, event)

combine_tracking <- read.csv("data/combine_tracking.csv")

# distance traveled


# Summarize every play given a df
summarize_plays <- function(input_df) {
  play_summary <- input_df %>%
    group_by(game_id, play_id, nfl_id) %>% 
    group_modify(~ {
      play <- .x[1, ] # should be 1 row
      x_at_snap <- .x %>% filter(event == "ball_snap") %>% pull(x)
      y_at_snap <- .x %>% filter(event == "ball_snap") %>% pull(y)
      x_at_throw <- .x %>% filter(event == "pass_forward") %>% pull(x)
      y_at_throw <- .x %>% filter(event == "pass_forward") %>% pull(y)
      distance_traveled <- sqrt((x_at_throw-x_at_snap)^2+(y_at_throw-y_at_snap)^2)
      
      snap <- which(.x$event == "ball_snap")
      throw <- which(.x$event == "pass_forward")
      frames_bef_throw <- throw - snap
        
      tibble(
        actual_separation = play$separation_at_pass_forward,
        
        # variables for predicting
        cushion = play$cushion,
        route_ran = play$route_ran,
        team_coverage_type = play$team_coverage_type,
        offense_formation = play$offense_formation,
        receiver_alignment = play$receiver_alignment,
        in_motion_at_ball_snap = play$in_motion_at_ball_snap,
        distance_traveled = distance_traveled,
        frames_bef_throw = frames_bef_throw
      )
    }) %>%
    ungroup()
  play_summary <- play_summary %>% filter(!is.na(actual_separation))
  return(play_summary)
}

summarized_wr <- summarize_plays(pbp_wr_tracking)
