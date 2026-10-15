library(tidyverse)
library(ggplot2)
library(gganimate)
library(sportyR)
library(nflreadr)
teams_colors_logos <- nflreadr::load_teams()
source("data_processing.R")

# Animate a drill without a ball
animate_drill <- function(id, drill_input, att) {
  drill <- combine_tracking %>% filter(nfl_id == id, drill_name == drill_input, attempt == att)
  
  player <- drill %>% filter(entity_type == "PLAYER")
  player$row_num <- 1:nrow(player)
  
  nfl_field <- geom_football("nfl", x_trans = 60, y_trans = 26.6667)
  
  drill_anim <- nfl_field +
    geom_point(data = player, aes(x, y), size = 5) +
    transition_time(row_num)
  
  return(animate(drill_anim, fps = 10, nframes = max(player$row_num, na.rm = TRUE)))
}

# Animate a drill with a ball
animate_drill_ball <- function(id, drill_input, att) {
  drill <- combine_tracking %>% filter(nfl_id == id, drill_name == drill_input, attempt == att)
  
  player <- drill %>% filter(entity_type == "PLAYER")
  player$row_num <- 1:nrow(player)
  #ball <- drill %>% filter(entity_type == "BALL")
  #ball$row_num <- 1:nrow(ball)
  
  nfl_field <- geom_football("nfl", x_trans = 60, y_trans = 26.6667)
  
  drill_anim <- nfl_field +
    geom_point(data = player, aes(x, y), size = 5) +
    transition_time(row_num)
  
  return(animate(drill_anim, fps = 10, nframes = max(player$row_num, na.rm = TRUE)))
}