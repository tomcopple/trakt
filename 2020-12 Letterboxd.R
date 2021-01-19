### Download letterboxd diary entries

library(tidyverse);library(xml2);library(lubridate)

letFeed <- "https://letterboxd.com/tomcopple2/rss/"
let <- httr::GET(letFeed) %>% 
    xml2::read_xml() %>% 
    xml2::xml_find_all(x = ., xpath = 'channel') %>% 
    xml2::xml_find_all(x = ., xpath = 'item') %>% 
    xml2::as_list() %>% 
    map(unlist) %>% 
    map_df(bind_rows) %>% 
    select(title = filmTitle, date = watchedDate, rating = memberRating) %>% 
    mutate(date = lubridate::ymd(date),
           rating = as.numeric(rating))
glimpse(let)

dropbox <- readRDS('traktShiny/dropbox.rds')
letHist <- rdrop2::drop_read_csv(file = 'R/trakt/letterboxd/diary.csv',
                                 dtoken = dropbox) %>% 
    as_tibble() %>% 
    select(title = Name, date = Watched.Date, rating = Rating) %>% 
    mutate(date = lubridate::ymd(date))

glimpse(letHist)

let <- bind_rows(let, letHist) %>% 
    distinct(title, date, rating) %>% 
    mutate(year = year(date))

let %>% arrange(desc(date))

let %>% group_by(year) %>% count()

let %>% filter(year == 2020) %>% arrange(desc(rating))

let %>% arrange(date) %>% 
    group_by(date) %>% summarise(count = n(), .groups = 'drop') %>% 
    full_join(data.frame(date = seq.Date(from = min(let$date), to = today(),
                              by = 'days'))) %>% 
    mutate(count = replace_na(count, 0)) %>% 
    arrange(date) %>% 
    mutate(n = c(cumsum(count[1:364]), 
                 zoo::rollsum(x = count, k = 365, align = 'right'))) %>% 
    # filter(date >= "2017-01-01") %>% 
    ggplot(aes(x = date, y = n)) + geom_col()

