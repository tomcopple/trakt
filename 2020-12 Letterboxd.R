### Download letterboxd diary entries

library(tidyverse);library(xml2);library(lubridate);library(plotly)

letFeed <- "https://letterboxd.com/tomcopple/rss/"
letNew <- httr::GET(letFeed) %>% 
    xml2::read_xml() %>% 
    xml2::xml_find_all(x = ., xpath = 'channel') %>% 
    xml2::xml_find_all(x = ., xpath = 'item') %>% 
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

let <- bind_rows(letNew, diary) %>% distinct(title, date, rating) %>% 
    na.omit()
# write_csv(let, here::here('tempData', 'letHist.csv'))
# rdrop2::drop_upload(file = here::here('tempData', 'letHist.csv'),
#                     path = 'R/trakt/letterboxd')

let %>% mutate(year = year(date)) %>% group_by(year) %>% count()

let %>% arrange(date) %>% 
    group_by(date) %>% summarise(count = n(), .groups = 'drop') %>% 
    full_join(data.frame(date = seq.Date(from = min(let$date), to = today(),
                              by = 'days'))) %>% 
    mutate(count = replace_na(count, 0)) %>% 
    arrange(date) %>% 
    mutate(n = c(cumsum(count[1:364]), 
                 zoo::rollsum(x = count, k = 365, align = 'right'))) %>% 
    filter(date >= "2011-12-31") %>% 
    ggplot(aes(x = date, y = n)) + geom_col(position = 'dodge')

let %>% mutate(rating = rating * 2) %>% count(rating) %>% 
    ggplot(aes(x = rating, y = n)) + geom_col()

let %>% 
    ggplot(aes(x = date, y = rating, color = rating)) + geom_point()

diary %>%
    mutate(home = str_detect(tags, 'home'),
           out = str_detect(tags, 'out|mv')) %>% 
    # filter(date >= "2012-01-01") %>% 
    full_join(data.frame(date = seq.Date(from = lubridate::ymd("2010-01-01"), 
                                         to = today(),
                                         by = 'days')))  %>% 
    arrange(date) %>% 
    mutate(home = replace_na(home, 0),
           out = replace_na(out, 0)) %>% 
    group_by(date) %>% 
    summarise(home = sum(home), out = sum(out), .groups = 'drop') %>%
    mutate(nHome = c(cumsum(home[1:364]), 
                 zoo::rollsum(x = home, k = 365, align = 'right')),
           nOut = c(cumsum(out[1:364]),
                    zoo::rollsum(x = out, k = 365, align = 'right'))) %>% 
    select(-home, -out) %>% 
    gather(-date, key = series, value = n) %>% 
    filter(date >= "2011-01-01") %>% 
    ggplot(aes(x = date, y = n, group = series, color = series)) +
    geom_point(alpha = 0.1, size = 0.5) + 
    geom_smooth(se   = FALSE, size = 1)

## out movies
diary %>% 
    filter(str_detect(tags, 'mv')) %>% 
    mutate(tags = str_remove_all(tags, 'mv')) %>% 
    separate(tags, into = c('1', '2', '3'), sep = ",") %>% 
    select(-title, -rating) %>% 
    gather(-date, key = series, value = tag) %>% 
    select(-series) %>% 
    mutate(tag = str_remove_all(tag, " ")) %>% 
    filter(tag != "") %>% 
    na.omit() %>% 
    filter(! tag %in% c('maidavale', 'hampstead', 'bakerstreet', 'screenonthegreen')) %>% 
    count(tag, sort = T)

diary %>% 
    filter(str_detect(tags, 'mv')) %>% 
    mutate(everyman = str_detect(tags, 'everyman'),
           odeon = str_detect(tags, 'odeon'),
           vue = str_detect(tags, 'vue'),
           curzon = str_detect(tags, 'curzon'),
           barbican = str_detect(tags, 'barbican'),
           bfi = str_detect(tags, 'bfi'),
           picturehouse = str_detect(tags, 'picturehouse')) %>% 
    mutate(others = ifelse(everyman|odeon|vue|curzon|barbican|bfi|picturehouse,
                  FALSE, TRUE)) %>% 
    full_join(data.frame(date = seq.Date(from = lubridate::ymd("2010-01-01"), 
                                         to = today(),
                                         by = 'days')))  %>% 
    arrange(date) %>% 
    select(-title, -rating, -tags) %>%
    mutate(everyman = replace_na(everyman, 0),
           odeon = replace_na(odeon, 0),
           vue = replace_na(vue, 0),
           curzon = replace_na(curzon, 0),
           barbican = replace_na(barbican, 0),
           bfi = replace_na(bfi, 0),
           picturehouse = replace_na(picturehouse, 0),
           others = replace_na(others, 0)) %>% 
    group_by(date) %>% 
    summarise(everyman = sum(everyman), odeon = sum(odeon),
              vue = sum(vue), curzon = sum(curzon),
              barbican = sum(barbican), bfi = sum(bfi),
              picturehouse = sum(picturehouse), others = sum(others),
              .groups = 'drop') %>%
    mutate(nEveryman = c(cumsum(everyman[1:364]), 
                     zoo::rollsum(x = everyman, k = 365, align = 'right')),
           nOdeon = c(cumsum(odeon[1:364]), 
                         zoo::rollsum(x = odeon, k = 365, align = 'right')),
           nVue = c(cumsum(vue[1:364]), 
                         zoo::rollsum(x = vue, k = 365, align = 'right')),
           nCurzon = c(cumsum(curzon[1:364]), 
                         zoo::rollsum(x = curzon, k = 365, align = 'right')),
           nBarbican = c(cumsum(barbican[1:364]), 
                         zoo::rollsum(x = barbican, k = 365, align = 'right')),
           nBFI = c(cumsum(bfi[1:364]), 
                         zoo::rollsum(x = bfi, k = 365, align = 'right')),
           nPicturehouse = c(cumsum(picturehouse[1:364]), 
                         zoo::rollsum(x = picturehouse, k = 365, align = 'right')),
           nOthers = c(cumsum(others[1:364]), 
                         zoo::rollsum(x = others, k = 365, align = 'right'))) %>% 
    select(date, nEveryman:nOthers) %>% 
    gather(-date, key = series, value = n) %>% 
    filter(date >= "2011-01-01") %>% 
    ggplot(aes(x = date, y = n, group = series, color = series)) +
    geom_point(alpha = 0.1, size = 0.5) + 
    geom_smooth(se   = FALSE, size = 1) +
    scale_y_continuous(limits = c(0, 25))



# Movies ------------------------------------------------------------------

tags <- diary %>% 
    filter(str_detect(tags, 'mv')) %>% 
    mutate(tags = str_remove_all(tags, 'mv')) %>% 
    mutate(tags = str_remove_all(tags, '^, '),
           tags = str_remove_all(tags, ', $'),
           tags = str_replace_all(tags, ', , ', ', ')) %>% 
    separate(tags, into = c('1', '2'), sep = ", ") %>% 
    select(-title, -rating) %>% 
    gather(-date, key = key, value = value) %>% 
    select(-key) %>% 
    na.omit()

top5 <- tags %>% 
    filter(!value %in% c('maidavale', 'hampstead', 'bakerstreet', 'screenonthegreen')) %>% 
    count(value, sort = T) %>% 
    slice(1:5) %>% 
    pull('value')
    
tags %>% 
    filter(!value %in% c('maidavale', 'hampstead', 'bakerstreet', 'screenonthegreen')) %>% 
    mutate(n = 1) %>% 
    mutate(tags = forcats::fct_other(value, keep = top5, other_level = 'Others')) %>% 
    select(-value) %>%
    group_by(tags) %>% 
    group_split() %>% 
    map(~full_join(
        x = .,
        y = data.frame(date = seq.Date(from = min(tags$date),
                                   to = today(), by = 'days'))
    )) %>% 
    map(arrange, date) %>% 
    map(mutate, n = replace_na(n, 0)) %>% 
    map(fill, tags, .direction = 'updown') %>% 
    map_df(mutate, cumsum = c(n[1:364],
                           zoo::rollsum(n, k = 365, align = 'right')))  %>% 
    ggplot(aes(x = date, y = cumsum, group = tags, color = tags)) +
    geom_point(alpha = 0.1, size = 0.5) + 
    geom_smooth(se   = FALSE, size = 1) +
    scale_y_continuous(limits = c(0, 25))


# Home --------------------------------------------------------------------

tags <- diary %>% 
    filter(str_detect(tags, 'home')) %>% 
    mutate(tags = str_remove_all(tags, 'home')) %>% 
    mutate(tags = str_remove_all(tags, '^, '),
           tags = str_remove_all(tags, ', $')) %>% 
    select(-title, -rating) %>% 
    gather(-date, key = key, value = value) %>% 
    select(-key) %>% 
    na.omit()

top5 <- tags %>% 
    count(value, sort = T) %>% 
    slice(1:5) %>% 
    pull('value')

tags %>% 
    mutate(n = 1) %>% 
    mutate(tags = forcats::fct_other(value, keep = top5, other_level = 'Others')) %>% 
    # select(-value) %>%
    group_by(tags) %>% 
    group_split() %>% 
    map(~full_join(
        x = .,
        y = data.frame(date = seq.Date(from = min(tags$date),
                                       to = today(), by = 'days'))
    )) %>% 
    map(arrange, date) %>% 
    map(mutate, n = replace_na(n, 0)) %>% 
    map(fill, tags, .direction = 'updown') %>% 
    map_df(mutate, cumsum = c(n[1:364],
                              zoo::rollsum(n, k = 365, align = 'right')))  %>% 
    filter(date >= "2011-01-01") %>% 
    ggplot(aes(x = date, y = cumsum, group = tags, color = tags)) +
    geom_point(alpha = 0.1, size = 0.5) + 
    geom_smooth(se   = FALSE, size = 1) +
    scale_y_continuous(limits = c(0, 25))
