library(tidyverse)
source("data_processing.R")

get_combine_results <- function(id) {
  combine_results <- combine_results %>% filter(nfl_id == id)
  
  return(combine_results)
}

get_player_details <- function(id) {
  player <- players %>% filter(nfl_id == id)
  
  return(player)
}