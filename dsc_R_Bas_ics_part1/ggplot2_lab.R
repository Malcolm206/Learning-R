library(dplyr)
library(readr)
library(ggplot2)

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
which(m_data$worldwide_gross == '$0')


gsub(',', '', m_data$worldwide_gross)
gsub('$', '', m_data$worldwide_gross)
as.numeric(worldwide_gross)

clean_dollars <- function(d) {
  a <- gsub(',', '', d)
  b <- gsub('$', '', a)
  b
}
clean_dollars(m_data$worldwide_gross)
