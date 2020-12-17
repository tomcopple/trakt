### Want to download Trakt history and sync to Plex
## NB Just syncing played status, nothing else. 

library(tidyverse);library(httr)


# 1. Import Trakt ---------------------------------------------------------

source('traktShiny/getTraktHistory.R')

trakt <- getTraktHistory(T)
trakt


# Get Plex History --------------------------------------------------------
token <- 'ABhPTJsJFC1CsCPKzzhb'

plexTV <- content(GET("http://192.168.1.99:32400/library/sections/2/search?type=4", 
                      add_headers("X-Plex-Token" = token))) %>% 
    magrittr::extract2('MediaContainer') %>% 
    magrittr::extract2('Metadata') %>% 


    map_df(function(x) {
        as_tibble(x) %>% 
            select(any_of(c('ratingKey', 'title', 'grandparentTitle', 'parentTitle',
                            'index', 'parentIndex')))
    }) %>% 
    select(show = grandparentTitle, seasonName = parentTitle, title, 
           ratingKey, season = parentIndex, episode = index)


# Join together to get Played ---------------------------------------------

matched <- inner_join(trakt, plexTV)
glimpse(matched)


# Scrobble to Plex --------------------------------------------------------


