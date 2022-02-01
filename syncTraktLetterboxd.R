## Sync trakt and letterboxd?
library(tidyverse);library(httr);library(jsonlite);library(lubridate);library(xml2)
source('traktShiny/setTrakt.R')

# 1. Get Trakt Movie History ----------------------------------------------

traktRawHis <- httr::GET(url = 'https://api.trakt.tv/users/tomcopple/watched/movies',
                    headers)
httr::stop_for_status(traktRawHis)
traktHis <- httr::content(traktRawHis, as = 'text') %>% 
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>% 
    select(title = movie.title, date = last_watched_at) %>% 
    mutate(date = as_date(date)) %>% 
    filter(date != lubridate::ymd('2011-08-24')) %>% 
    arrange(date) %>% 
    as_tibble()
traktHis %>% count(year(date)) %>% ggplot(aes(x = as.factor(`year(date)`), y = n)) + geom_col(fill = scales::hue_pal()(4)[[3]])

# Merge with Trakt ratings ------------------------------------------------

traktRawRat <- httr::GET(url = 'https://api.trakt.tv/users/tomcopple/ratings/movies',
                      headers)
traktRat <- httr::content(traktRawRat, as = 'text') %>% 
    jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>% 
    select(title = movie.title, rating) %>% 
    as_tibble()
traktRat %>% count(rating) %>% ggplot(aes(x = rating, y = n)) + geom_col(fill = scales::hue_pal()(4)[[1]])

trakt <- full_join(traktRat, traktHis) %>% 
    filter(str_detect(title, 'Charlie Brown', negate = TRUE))
trakt %>% filter(is.na(date))
trakt %>% filter(is.na(rating))


# Get Letterboxd History --------------------------------------------------

letFeed <- "https://letterboxd.com/tomcopple/rss/"
letNew <- httr::GET(letFeed) %>% 
    xml2::read_xml() %>% 
    xml2::xml_find_all(xpath = 'channel') %>% 
    xml2::xml_find_all(xpath = 'item') %>% 
    xml2::as_list() %>% 
    map(unlist) %>% 
    map_df(bind_rows) %>% 
    select(title = filmTitle, date = watchedDate, rating = memberRating) %>% 
    mutate(date = lubridate::ymd(date),
           rating = as.numeric(rating))

letHist <- rdrop2::drop_read_csv(file = 'R/trakt/letterboxd/letHist.csv',
                                 dtoken = dropbox) %>% 
    as_tibble() %>% 
    mutate(date = lubridate::ymd(date))

let <- bind_rows(letNew, letHist) %>% distinct(title, date, rating)
write_csv(let, here::here('tempData', 'letHist.csv'))
rdrop2::drop_upload(file = here::here('tempData', 'letHist.csv'),
                    path = 'R/trakt/letterboxd')


# Merge together ----------------------------------------------------------

## First look at what's in trakt but not letterboxd
anti_join(trakt, let %>% mutate(rating = as.integer(rating * 2)))

## Also see other way round (but don't do anything with this yet?)
anti_join(let %>% mutate(rating = as.integer(rating * 2)), trakt)

notLet <-  anti_join(trakt, let %>% mutate(rating = as.integer(rating * 2)),
                     by = 'title') %>% 
    arrange(desc(date))

notLet
