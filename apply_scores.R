library(tidyverse)

get_short_score <- function(df) {
  min_score <- quantile(df$a_break_short, probs=0.1)
  max_score <- quantile(df$a_break_short, probs=0.9)
  range <- max_score - min_score
  
  df <- df %>%
    mutate(short_score = round(pmax(pmin((a_break_short-min_score) * (10/range), 10), 0), 2))
  
  return(df)
}

get_med_score <- function(df) {
  min_score <- quantile(df$a_break_med, probs=0.1)
  max_score <- quantile(df$a_break_med, probs=0.9)
  range <- max_score - min_score
  
  df <- df %>%
    mutate(med_score = round(pmax(pmin((max_score - a_break_med) * (10/range), 10), 0), 2))
  
  return(df)
}

get_deep_score <- function(df) {
  min_score <- quantile(df$a_break_deep, probs=0.1, na.rm = TRUE)
  max_score <- quantile(df$a_break_deep, probs=0.9, na.rm = TRUE)
  range <- max_score - min_score
  
  df <- df %>%
    mutate(deep_score = round(pmax(pmin((a_break_deep-min_score) * (10/range), 10), 0), 2))
  
  return(df)
}

get_scores <- function(df) {
  df <- get_short_score(df) %>% select(-a_break_short)
  df <- get_med_score(df) %>% select(-a_break_med)
  df <- get_deep_score(df) %>% select(-a_break_deep)
  df <- df %>% mutate(total = rowSums(pick(short_score, med_score, deep_score), na.rm = TRUE),
                      avg = rowMeans(pick(short_score, med_score, deep_score), na.rm = TRUE))

  return(df)
}

score_test <- get_scores(leaderboard_200)

ggplot(score_test, aes(x = avg_w_soe, y = avg, size = n_plays)) +
  geom_point()


