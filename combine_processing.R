library(tidyverse)

players <- read.csv("data/players.csv")
wr_ids <- players %>% filter(nfl_position == "WR") %>% pull(nfl_id)
combine_results <- read.csv("data/combine_results.csv")
combine_tracking <- read.csv("data/combine_tracking.csv")

forty <- c()
ten_yd <- c()
three_cone <- c()
short_shuttle <- c()

for (i in wr_ids) {
  results <- combine_results %>% filter(nfl_id == i)
  
  forty <- c(forty, results$forty)
  ten_yd <- c(ten_yd, results$ten_yd_split)
  three_cone <- c(three_cone, results$three_cone)
  short_shuttle <- c(short_shuttle, results$short_shuttle)
}

wr_combine_results <- data.frame(
  nfl_id = wr_ids,
  forty = forty,
  ten_yd_split = ten_yd,
  three_cone = three_cone,
  short_shuttle = short_shuttle
)

wr_combine_results$n_NA <- rowSums(is.na(wr_combine_results))
