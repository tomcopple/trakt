### Want to download Trakt history and sync to Plex
## NB Just syncing played status, nothing else. 

library(tidyverse);library(httr)


# 1. Import Trakt ---------------------------------------------------------

source('traktShiny/getTraktHistory.R')

trakt <- getTraktHistory(T)
trakt


# Get Plex History --------------------------------------------------------
token <- 'ABhPTJsJFC1CsCPKzzhb'

plex <- content(GET("http://192.168.1.99:32400/library/sections/2/search?type=4", 
                      add_headers("X-Plex-Token" = token))) %>% 
    magrittr::extract2('MediaContainer') %>% 
    magrittr::extract2('Metadata') %>% 
    map_df(function(x) {
        as_tibble(x) %>% 
            select(any_of(c('ratingKey', 'title', 'grandparentTitle', 'parentTitle',
                            'index', 'parentIndex', 'viewCount')))
    }) %>% 
    select(show = grandparentTitle, seasonName = parentTitle, title, 
           ratingKey, season = parentIndex, episode = index, viewCount) %>% 
    mutate(viewCount = ifelse(is.na(viewCount), FALSE, TRUE))


# Join together to get Played ---------------------------------------------

matched <- inner_join(trakt, plex, 
                      by = c('show', 'season', 'episode')) %>% 
    distinct(show, season, episode, seasonName, ratingKey, viewCount)
glimpse(matched)

unplayed <- matched %>% filter(!viewCount)
glimpse(unplayed)

# Scrobble to Plex --------------------------------------------------------

issues <- tibble(ratingKey = "")

markPlayed <- function(ratingKey) {
 req <- httr::GET('http://192.168.1.99:32400/:/scrobble',
                  query = list(
                      "X-Plex-Token" = token,
                      "identifier"   = "com.plexapp.plugins.library",
                      "key"          = ratingKey
                  ))   
 
 if(status_code(req) != 200) {
     print(str_c("Problem with ", ratingKey))
     .GlobalEnv$issues <- rbind(issues, tibble(
         ratingKey = ratingKey
     ))
 } else if (status_code(req) == 200) {
     print(str_c(ratingKey, " successful"))
 }
 
}

purrr::walk(.x = rev(unplayed$ratingKey), .f = markPlayed)
