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
a_break_short <- c()

get_gauntlet_avg_s <- function(df) {
  
}

# short: "SLANT_ROUTE_LEFT"
get_a_break_short <- function(tracking, k = 3, min_turn = 45, min_speed = 2) {
  slant_tracking <- tracking %>% filter(drill_name == "SLANT_ROUTE_LEFT")
  if (nrow(slant_tracking) == 0) { # if didn't run, return NA
    return(NA)
  }
  
  # assign frame ids
  slant_tracking <- slant_tracking %>% group_by(attempt) %>%
    mutate(frame_id = row_number()) %>%
    ungroup()
  
  # determine cut frame
  angle_diff <- function(a, b) ((a - b + 180) %% 360) - 180
  attempts <- slant_tracking %>% pull(attempt)
  highest <- 0
  for (i in attempts) {
    slant_att <- slant_tracking %>% filter(attempt == i)
    slant_att <- slant_att %>%
      mutate(
        x_s = zoo::rollmean(x, 3, fill = "extend", align = "center"),
        y_s = zoo::rollmean(y, 3, fill = "extend", align = "center"),
        dx = x_s - lag(x_s),
        dy = y_s - lag(y_s),
        heading = (atan2(dx, dy) * 180 / pi) %% 360,   # 0 = +y, 90 = +x, matches NFL dir convention
        turn = abs(angle_diff(lead(heading, k), lag(heading, k))),
        speed_in = lag(s, k)
      )
    cuts <- slant_att %>%
      filter(turn > 45, speed_in > 2)
    if (nrow(cuts) == 0) next
    cut_frame <- cuts %>% slice_max(turn, n = 1, with_ties = FALSE) %>% pull(frame_id)
    
    rec_a <- slant_att[cut_frame+2, ] %>% pull(a)
    
    if (rec_a > highest) {
      highest <- rec_a
    }
  }
  return(highest)
}

# medium: dagger?
# deep: post

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
  
  a_break_short <- c(a_break_short, get_a_break_short(tracking))
}

wr_combine_results <- data.frame(
  nfl_id = wr_ids,
  forty = forty,
  ten_yd_split = ten_yd,
  three_cone = three_cone,
  short_shuttle = short_shuttle,
  gauntlet_avg_s = gauntlet_avg_s,
  a_break_short = a_break_short
)

wr_combine_results$n_NA <- rowSums(is.na(wr_combine_results))

# Testing
leaderboard_50 <- leaderboard %>% filter(n_plays >= 50)
join_test <- inner_join(leaderboard_50, wr_combine_results, by = "nfl_id")

join_test_200 <- join_test %>% filter(n_plays >= 200)
ggplot(join_test_200, aes(x = avg_cse, y = a_break_short)) +
  geom_point()
