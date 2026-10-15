library(tidyverse)
source("data_processing.R")

# get player combine results
get_combine_results <- function(id) {
  combine_results <- combine_results %>% filter(nfl_id == id)
  
  return(combine_results)
}

# get list of drills ran by player
get_drills_list <- function(id) {
  drill_list <- combine_tracking %>% filter(nfl_id == id) %>% pull(drill_name)
  drill_list <- unique(drill_list)
  
  return(drill_list)
}

# get player details given their id
get_player_details <- function(id) {
  player <- players %>% filter(nfl_id == id)
  
  return(player)
}