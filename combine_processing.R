library(tidyverse)

players <- read.csv("data/players.csv")
wr_ids <- players %>% filter(nfl_position == "WR") %>% pull(nfl_id)
combine_results <- read.csv("data/combine_results.csv")
combine_tracking <- read.csv("data/combine_tracking.csv")

forty <- c()
ten_yd <- c()
three_cone <- c()
short_shuttle <- c()
gauntlet_avg_s <- c()

get_gauntlet_avg_s <- function(df) {
  
}

for (i in wr_ids) {
  results <- combine_results %>% filter(nfl_id == i)
  tracking <- combine_tracking %>% filter(nfl_id == i)
  
  forty <- c(forty, results$forty)
  ten_yd <- c(ten_yd, results$ten_yd_split)
  three_cone <- c(three_cone, results$three_cone)
  short_shuttle <- c(short_shuttle, results$short_shuttle)

  # tracking calculations
  # gauntlet avg s
  gauntlet_tracking <- tracking %>% filter(drill_name == "GAUNTLET_DRILL", y >= 5 & y <= 45)
  if (nrow(gauntlet_tracking) == 0) { # if didn't run, return NA
    gauntlet_avg_s <- c(gauntlet_avg_s, NA)
  }
  else {
    attempts <- gauntlet_tracking %>% pull(attempt)
    highest <- NA
    # Get highest avg speed from all attempts
    for (j in attempts) {
      gauntlet_att <- gauntlet_tracking %>% filter(attempt == j)
      avg_s <- mean(gauntlet_att$s)
      
      if (is.na(highest) | highest < avg_s) {
        highest <- avg_s
      }
    }
    gauntlet_avg_s <- c(gauntlet_avg_s, highest)
  }
}

wr_combine_results <- data.frame(
  nfl_id = wr_ids,
  forty = forty,
  ten_yd_split = ten_yd,
  three_cone = three_cone,
  short_shuttle = short_shuttle,
  gauntlet_avg_s = gauntlet_avg_s
)

wr_combine_results$n_NA <- rowSums(is.na(wr_combine_results))

# Testing
leaderboard_50 <- leaderboard %>% filter(n_plays >= 50)
join_test <- inner_join(leaderboard_50, wr_combine_results, by = "nfl_id")
