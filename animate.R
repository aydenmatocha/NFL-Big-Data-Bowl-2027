library(tidyverse)
library(ggplot2)
library(gganimate)
library(sportyR)
library(nflreadr)
#source("data_processing.R")

# Animate a drill without a ball
animate_drill <- function(id, drill_input, att) {
  drill <- combine_tracking %>% filter(nfl_id == id, drill_name == drill_input, attempt == att)
  
  player <- drill %>% filter(entity_type == "PLAYER")
  player$frame_id <- 1:nrow(player)
  
  nfl_field <- geom_football("nfl", x_trans = 60, y_trans = 26.6667)
  
  drill_anim <- nfl_field +
    geom_point(data = player, aes(x, y), size = 5) +
    geom_label(data = player, aes(x, y + 3, label = round(y, 2)), color = "black", size = 3) +
    transition_time(frame_id)
  
  return(animate(drill_anim, fps = 10, nframes = max(player$frame_id, na.rm = TRUE)))
}

# Animate a drill with a ball
animate_drill_ball <- function(id, drill_input, att) {
  drill <- combine_tracking %>% filter(nfl_id == id, drill_name == drill_input, attempt == att)
  
  player <- drill %>% filter(entity_type == "PLAYER")
  player$frame_id <- 1:nrow(player)
  ball <- drill %>% filter(entity_type == "BALL")
  ball$frame_id <- 1:nrow(ball)
  
  nfl_field <- geom_football("nfl", x_trans = 60, y_trans = 26.6667)
  
  drill_anim <- nfl_field +
    geom_point(data = player, aes(x, y), size = 5) +
    geom_point(data = ball, aes(x, y), size = 3, color = "#8B4513", shape = 19) +
    transition_time(frame_id)
  
  return(animate(drill_anim, fps = 10, nframes = max(player$frame_id, na.rm = TRUE)))
}