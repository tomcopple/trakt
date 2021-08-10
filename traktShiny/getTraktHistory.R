## Download all trakt scrobbles, and save local copy

getTraktHistory <- function(refresh = TRUE) {
    
    library(tidyverse);library(lubridate);library(stringr);library(httr);library(jsonlite);library(rdrop2)
    
    if (refresh) {
        
        # Create url
        baseurl <- "https://api-v2launch.trakt.tv/users/"
        call <- "/history/episodes?limit=100000"
        
        url <- paste0(baseurl, 'me', call)
        
        # Set info for GET request. 
        headers <- httr::add_headers(.headers = c("trakt-api-key" = trakt_id,
                                                  "Content-Type" = "application/json",
                                                  "trakt-api-version" = 2,
                                                  "Authorization" = paste('Bearer', accessCode)))
        response <- httr::GET(url, headers)
        httr::stop_for_status(response)
        response <- httr::content(response, as = "text")
        response <- jsonlite::fromJSON(response, 
                                       simplifyDataFrame = TRUE, 
                                       flatten = TRUE)
        
        history <- response %>% 
            select(
                id = id, 
                title = episode.title, 
                show = show.title, 
                season = episode.season, 
                episode = episode.number,
                date = watched_at,
                slug = show.ids.slug,
                tvdb = show.ids.tvdb
            ) %>% 
            mutate(date = ymd_hms(date)) %>% 
            as_tibble()
        
        ## Remove duplicate plays (i.e. anything marked played at the same date, same time)
        history <- history %>% group_by(date) %>% filter(n() == 1) %>% ungroup()
        
        ## And just remove everything from August 30 2015
        history <- history %>% filter(date(date) != '2015-08-30')
        
        write_csv(history, str_c(lubridate::today(), '-traktHistory.csv'))
        write_csv(history, 'traktHistory.csv')
        
        
        ## Dropbox authentication and save
        dropbox <- readRDS('traktShiny/dropbox.rds')
        rdrop2::drop_upload(file = str_c(lubridate::today(), '-traktHistory.csv'),
                            path = "R/trakt", dtoken = dropbox)
        rdrop2::drop_upload(file = 'traktHistory.csv', path = 'R/trakt', dtoken = dropbox)
        
        
    } else {
        
        dropbox <- readRDS('dropbox.rds')
        history <- rdrop2::drop_read_csv(file = 'R/trakt/traktHistory.csv', 
                                         dtoken = dropbox) %>% 
            as_tibble()
        
    }
    
    return(history)
    
}
