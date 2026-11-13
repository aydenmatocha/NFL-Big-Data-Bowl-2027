library(tidyverse)

players <- read.csv("data/players.csv")
wr_ids <- players %>% filter(nfl_position == "WR") %>% pull(nfl_id)
combine_results <- read.csv("data/combine_results.csv")
combine_tracking <- read.csv("data/combine_tracking.csv")

ten_yd <- c()
a_break_short <- c()
a_break_med <- c()
a_break_deep <- c()

angle_diff <- function(a, b) ((a - b + 180) %% 360) - 180

get_break_frame <- function(df, k = 3, min_turn = 45, min_speed = 2) {
  df <- df %>%
    mutate(
      x_s = zoo::rollmean(x, 3, fill = "extend", align = "center"),
      y_s = zoo::rollmean(y, 3, fill = "extend", align = "center"),
      dx = x_s - lag(x_s),
      dy = y_s - lag(y_s),
      heading = (atan2(dx, dy) * 180 / pi) %% 360,   # 0 = +y, 90 = +x, matches NFL dir convention
      turn = abs(angle_diff(lead(heading, k), lag(heading, k))),
      speed_in = lag(s, k)
    )
  cuts <- df %>%
    filter(turn > 45, speed_in > 2)
  if (nrow(cuts) == 0) return(NA)
  cut_frame <- cuts %>% slice_max(turn, n = 1, with_ties = FALSE) %>% pull(frame_id)
  return(cut_frame)
}

# short: "SLANT_ROUTE_LEFT"
get_a_break_short <- function(tracking) {
  slant_tracking <- tracking %>% filter(drill_name == "SLANT_ROUTE_LEFT")
  if (nrow(slant_tracking) == 0) { # if didn't run, return NA
    return(NA)
  }
  
  # assign frame ids
  slant_tracking <- slant_tracking %>% group_by(attempt) %>%
    mutate(frame_id = row_number()) %>%
    ungroup()
  
  # determine cut frame
  attempts <- slant_tracking %>% pull(attempt)
  highest <- 0
  for (i in attempts) {
    slant_att <- slant_tracking %>% filter(attempt == i)
    cut_frame <- get_break_frame(slant_att)
    if (is.na(cut_frame)) next
    
    rec_a <- slant_att[cut_frame+2, ] %>% pull(a)
    
    if (rec_a > highest) {
      highest <- rec_a
    }
  }
  return(highest)
}

# medium: dagger?
get_a_break_med <- function(tracking) {
  dagger_tracking <- tracking %>% filter(drill_name == "DAGGER_ROUTE_LEFT")
  if (nrow(dagger_tracking) == 0) { # if didn't run, return NA
    return(NA)
  }
  
  # assign frame ids
  dagger_tracking <- dagger_tracking %>% group_by(attempt) %>%
    mutate(frame_id = row_number()) %>%
    ungroup()
  
  # determine cut frame
  attempts <- dagger_tracking %>% pull(attempt)
  highest <- 0
  for (i in attempts) {
    dagger_att <- dagger_tracking %>% filter(attempt == i)
    cut_frame <- get_break_frame(dagger_att)
    if (is.na(cut_frame)) next
    
    rec_a <- dagger_att[cut_frame-2, ] %>% pull(a)
    
    if (rec_a > highest) {
      highest <- rec_a
    }
  }
  return(highest)
}

# deep: post-corner
get_a_break_deep <- function(tracking, k = 3, min_turn = 45, min_speed = 2) {
  corner_tracking <- tracking %>% filter(drill_name == "POST_CORNER_ROUTE_RIGHT")
  if (nrow(corner_tracking) == 0) { # if didn't run, return NA
    return(NA)
  }
  
  # assign frame ids
  corner_tracking <- corner_tracking %>% group_by(attempt) %>%
    mutate(frame_id = row_number()) %>%
    ungroup()
  
  # determine cut frame
  attempts <- corner_tracking %>% pull(attempt)
  highest <- 0
  for (i in attempts) {
    corner_att <- corner_tracking %>% filter(attempt == i)
    cut_frame <- get_break_frame(corner_att)
    if (is.na(cut_frame)) next
    
    rec_a <- corner_att[cut_frame+2, ] %>% pull(a)
    
    if (rec_a > highest) {
      highest <- rec_a
    }
  }
  return(highest)
}

# Assign values
for (i in wr_ids) {
  results <- combine_results %>% filter(nfl_id == i)
  tracking <- combine_tracking %>% filter(nfl_id == i)
  
  ten_yd <- c(ten_yd, results$ten_yd_split)

  # tracking calculations
  a_break_short <- c(a_break_short, get_a_break_short(tracking))
  a_break_med <- c(a_break_med, get_a_break_med(tracking))
  a_break_deep <- c(a_break_deep, get_a_break_deep(tracking))
}

wr_combine_results <- data.frame(
  nfl_id = wr_ids,
  ten_yd_split = ten_yd,
  a_break_short = a_break_short,
  a_break_med = a_break_med,
  a_break_deep = a_break_deep
)

wr_combine_results$n_NA <- rowSums(is.na(wr_combine_results))

# Testing
leaderboard_50 <- leaderboard %>% filter(n_plays >= 50)
join_test <- inner_join(leaderboard_50, wr_combine_results, by = "nfl_id")

join_test_200 <- join_test %>% filter(n_plays >= 200) #%>% filter(nfl_id != 57144)
ggplot(join_test_200, aes(x = avg_soe, y = a_break_med)) +
  geom_point()
