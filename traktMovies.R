# Trakt Movies?
library(tidyverse);library(lubridate)

# Create url
baseurl <- "https://api-v2launch.trakt.tv/users/"
call <- "/ratings/movies"

# Run setTrakt.R to get credentials
source("setTrakt.R")

url <- paste0(baseurl, traktUser, call)

# Set info for GET request. 
headers <- httr::add_headers(.headers = c("trakt-api-key" = traktApi,
                                          "Content-Type" = "application/json",
                                          "trakt-api-version" = 2))
response <- httr::GET(url, headers)
httr::stop_for_status(response)
response <- httr::content(response, as = "text")
response <- jsonlite::fromJSON(response, 
                               simplifyDataFrame = TRUE, 
                               flatten = TRUE)

movies <- response %>% 
    select(date = rated_at, rating, movie = movie.title) %>% 
    mutate(date = ymd(substr(date, 0, 10))) %>% 
    as_tibble()

ggplot(movies %>% filter(date > today() - years(1)),
       aes(x = date, y = rating, color = factor(rating), text = movie)) + 
    geom_point()
