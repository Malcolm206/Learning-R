library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)

df_title_basics <- read_csv("imdb.title.basics.csv")
df_movie_budgets <- read_csv("tn.movie_budgets.csv")

select(df_title_basics, primary_title:genres)
basics <- as_tibble(df_title_basics)
budgets <- as_tibble(df_movie_budgets)

colnames(basics) [2] <- "movie"

budgets %>%
  mutate(start_year = substring(budgets$release_date, -4, -1))
select(budgets, start_year)

budgets$start_year <- substring(budgets$release_date, 
                                nchar(budgets$release_date) - 3,
                                nchar(budgets$release_date))
select(budgets, start_year)

budgets$start_year <- as.numeric(as.character(budgets$start_year))

m_data <- inner_join(basics, budgets, by = c("movie", "start_year"))
select_all(m_data)

duplicated(m_data$movie)
m_data[!duplicated(m_data$movie), ]
which(m_data$worldwide_gross != '$0')


gsub(',', '', m_data$worldwide_gross)
gsub('$', '', m_data$worldwide_gross)
as.numeric(worldwide_gross)

clean_dollars <- function(d) {
  a <- as.numeric(gsub("[$,]", "", d))
  a
}
clean_dollars(m_data$worldwide_gross)
select(m_data, genres)

rem_unnec_val <- function(df) {
  a <- df[!duplicated(df$movie), ]
  b <- a[which(a$worldwide_gross != '$0'), ]
  b
}
rem_unnec_val(m_data)

reduce <- function(x) {
  unname(x[!is.na(x)])
}

ind_str_parser <- function(df, parsed_column_str, list_strings) {
  df <- replace(df, c("NULL", "NA"), "none")
  no_genre_list = list()
  for (x in df[parsed_column_str]) {
    if (identical(x, 'none')) {
      no_genre_list <- c(no_genre_list, 1)
    } else {
      no_genre_list <- c(no_genre_list, 0)
    }
  }
  df$genres_not_parsed_id <- no_genre_list
  list_of_series <- list()
  # iterates through a list of genres
  for (string in list_strings) {
    # we create a list of 1 or 0 to check if the genre is a part of the obs.
    presence <- list()
    # for each genre, we check the genres in each observation
    for (x in df[parsed_column_str]) {
      # we check if the observation has the genre
      df['new'] <- ifelse(str_detect(x, pattern = string), 1, 0)
      new_column_name <- paste(parsed_column_str, string, 'id', sep = "_")
      colnames(df)[colnames(df) == 'new'] <- new_column_name
    list_of_series <- c(list_of_series, df[new_column_name])
    }
  }
  df <- df %>%
    rowwise() %>%
    mutate(genres_tuple = list(c_across(genres_Action_id : genres_Documentary_id)))
  return(df)
}



genres_list <- list('Action', 'Adventure', 'Comedy', 'Drama', 'Family', 'Thriller', 'Documentary')
genres_list
ind_str_parser(df = m_data, parsed_column_str = 'genres', list_strings =  genres_list)
m_data["genres"]

budgets_level <- function(df) {
  budget_category = list()
  for (x in df$total_costs) {
    if (x < 25000000) {
      budget_category <- c(budget_category, 'low')
    } else if (x < 100000000) {
      budget_category <- c(budget_category, 'mid')
    } else {
      budget_category <- c(budget_category, 'high')
    }
  }
  df$budget_category <- budget_category
  return(df)
}

cl_data <- function(df) {
  x <- rem_unnec_val(df)
  y <- x
  y$worldwide_gross <- clean_dollars(x$worldwide_gross)
  y$domestic_gross <- clean_dollars(x$domestic_gross)
  y$production_budget <- clean_dollars(x$production_budget)
  y$advertisement_budget = y$production_budget
  y$total_costs = y$advertisement_budget + y$production_budget
  y$profit = y$worldwide_gross - y$total_costs
  y$ROI = y$profit / y$total_costs * 100
  y <- budgets_level(y)
  genres_list <- list('Action', 'Adventure', 'Comedy', 'Drama', 'Family', 'Thriller', 'Documentary')
  y <- ind_str_parser(df = y, parsed_column_str = 'genres', list_strings =  genres_list)
  return(y)
}


clean_data <- cl_data(m_data)

low_budget_genres <- function(df) {
  r <- list()
  d <- df
  for (x in list(1, 2, 3, 4, 5, 6, 7)) {
    d['new'] <- sapply(d$genres_tuple, '[[', x)
    ftr <- median(d[which(d$total_costs < 25000000 & d$new == 1), ]$ROI)
    r <- c(r, ftr)
  }
  new_df <- data.frame(Median_ROI = unlist(r), Genres = unlist(genres_list))
  return(new_df)
}

mid_budget_genres <- function(df) {
  r <- list()
  d <- df
  for (x in list(1, 2, 3, 4, 5, 6, 7)) {
    d['new'] <- sapply(d$genres_tuple, '[[', x)
    ftr <- median(d[which(d$total_costs >= 25000000 & 
                            d$total_costs < 100000000 &
                            d$new == 1), ]$ROI)
    r <- c(r, ftr)
  }
  new_df <- data.frame(Median_ROI = unlist(r), Genres = unlist(genres_list))
  return(new_df)
}

high_budget_genres <- function(df) {
  r <- list()
  d <- df
  for (x in list(1, 2, 3, 4, 5, 6, 7)) {
    d['new'] <- sapply(d$genres_tuple, '[[', x)
    ftr <- median(d[which(d$total_costs > 100000000 & d$new == 1), ]$ROI)
    r <- c(r, ftr)
  }
  new_df <- data.frame(Median_ROI = unlist(r), Genres = unlist(genres_list))
  return(new_df)
}

low_budget_g <- low_budget_genres(clean_data)
mid_budget_g <- mid_budget_genres(clean_data)
high_budget_g <- high_budget_genres(clean_data)
sapply(clean_data$genres_tuple, '[[', 1)
median(clean_data[which(clean_data$total_costs < 25000000), ]$ROI)

low_budget_g %>%
  ggplot(aes(x = Genres, y = Median_ROI)) +
  geom_bar(stat = 'identity') +
  labs(title = "Median ROI for Low Budget Films by Genre", 
       x = 'Genres', 
       y = "Return on Investment")

mid_budget_g %>%
  ggplot(aes(x = Genres, y = Median_ROI)) +
  geom_bar(stat = 'identity') +
  labs(title = "Median ROI for Mid Budget Films by Genre", 
       x = 'Genres', 
       y = "Return on Investment")

high_budget_g %>%
  ggplot(aes(x = Genres, y = Median_ROI)) +
  geom_bar(stat = 'identity') +
  labs(title = "Median ROI for High Budget Films by Genre", 
       x = 'Genres', 
       y = "Return on Investment")

clean_data %>%
  ggplot(aes_string(x = 'total_costs', y = 'ROI')) +
  geom_point(stat = 'identity', alpha = 0.5) +
  labs(title = "Relationship Between Movie Budgets and ROI", 
       y = 'Return on Investment (ROI %)',
       x = 'Movie Budget ($)') +
  coord_cartesian(ylim = c(-200, 1500))

clean_data[which(clean_data$budget_category == 'low'), ] %>%
  ggplot(aes_string(x = 'total_costs', y = 'ROI')) +
  geom_point(stat = 'identity', alpha = 0.5) +
  labs(title = 'Low Budget Films Return on Investment', 
       x = 'Movie Budget ($)', 
       y = 'Return on Investment (%)') +
  coord_cartesian(ylim = c(-500, 3500))

clean_data[which(clean_data$budget_category == 'mid'), ] %>%
  ggplot(aes_string(x = 'total_costs', y = 'ROI')) +
  geom_point(stat = 'identity', alpha = 0.5) +
  labs(title = 'Mid Budget Films Return on Investment', 
       x = 'Movie Budget ($)', 
       y = 'Return on Investment (%)')

clean_data[which(clean_data$budget_category == 'high'), ] %>%
  ggplot(aes_string(x = 'total_costs', y = 'ROI')) +
  geom_point(stat = 'identity', alpha = 0.5) +
  labs(title = 'High Budget Films Return on Investment', 
       x = 'Movie Budget ($)', 
       y = 'Return on Investment (%)')

clean_data
  ggplot(aes(x = , y = ROI)) +
  geom_boxplot() +
  labs(title = 'Distribution of Return on Investment Percentage Grouped by Budget Tier',
       x = 'Budget Tier', 
       y = 'Return on Investment (%)')

