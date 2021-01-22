### Download letterboxd diary entries

library(tidyverse);library(xml2);library(lubridate)

letFeed <- "https://letterboxd.com/tomcopple2/rss/"
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

dropbox <- readRDS('traktShiny/dropbox.RDS')

diary <- rdrop2::drop_read_csv(file = 'R/trakt/letterboxd/diary.csv',
                                dtoken = dropbox) %>% 
    select(date = Watched.Date, title = Name, rating = Rating, tags = Tags) %>% 
    mutate(date = lubridate::ymd(date)) %>% 
    as_tibble()

# letHist <- rdrop2::drop_read_csv(file = 'R/trakt/letterboxd/letHist.csv',
#                                  dtoken = dropbox) %>% 
#     as_tibble() %>% 
#     mutate(date = lubridate::ymd(date))


let <- bind_rows(letNew, diary) %>% distinct(title, date, rating) %>% 
    na.omit()
# write_csv(let, here::here('tempData', 'letHist.csv'))
# rdrop2::drop_upload(file = here::here('tempData', 'letHist.csv'),
#                     path = 'R/trakt/letterboxd')

let %>% mutate(year = year(date)) %>% group_by(year) %>% count()

let %>% arrange(date) %>% 
    full_join(data.frame(date = seq.Date(from = min(let$date), to = today(),
                              by = 'days'))) %>% 
    arrange(date) %>% 
    mutate(count = ifelse(is.na(title), 0, 1)) %>% 
    mutate(n = c(cumsum(count[1:364]), 
                 zoo::rollsum(x = count, k = 365, align = 'right'))) %>% 
    # distinct(date,)
    ggplot(aes(x = date, y = n)) + geom_col(position = 'dodge')

let %>% filter(year(date) == 2020) %>% mutate(rating = rating * 2) %>% count(rating) %>% 
    ggplot(aes(x = rating, y = n)) + geom_col()

let %>% 
    ggplot(aes(x = date, y = rating, color = rating)) + geom_point()

diary %>%
    mutate(home = str_detect(tags, 'home'),
           out = str_detect(tags, 'out|mv')) %>% 
    filter(date >= "2012-01-01") %>% 
    full_join(data.frame(date = seq.Date(from = lubridate::ymd("2012-01-01"), 
                                         to = today(),
                                         by = 'days')))  %>% 
    arrange(date) %>% 
    mutate(home = replace_na(home, 0),
           out = replace_na(out, 0)) %>% 
    group_by(date) %>% 
    summarise(home = sum(home), out = sum(out), .groups = 'drop') %>%
    mutate(nHome = c(cumsum(home[1:29]), 
                 zoo::rollsum(x = home, k = 30, align = 'right')),
           nOut = c(cumsum(out[1:29]),
                    zoo::rollsum(x = out, k = 30, align = 'right'))) %>% 
    select(-home, -out) %>% 
    gather(-date, key = series, value = n) %>% 
    ggplot(aes(x = date, y = n, group = series, color = series)) +
    geom_point(alpha = 0.1, size = 0.5) + 
    geom_smooth(se   = FALSE, size = 1)

diary %>% 
    filter(str_detect(tags, 'home')) %>% 
    separate(tags, into = c('home', 'tag'), sep = ", ")

