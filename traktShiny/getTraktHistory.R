## Download all trakt scrobbles, and save local copy

getTraktHistory <- function(refresh = TRUE, accessCode) {
    
    library(tidyverse);library(lubridate);library(stringr);library(httr2)

    if (refresh) {
        
        # Create url
        baseurl <- "https://api-v2launch.trakt.tv/users/"
        call <- "/history/episodes?limit=100000"
        
        req <- request(base_url = str_glue(
            "{baseurl}me{call}"
        ))
        
        trakt_id <- Sys.getenv('TRAKTSHINY_ID')
        
        req2 <- req %>% 
            req_auth_bearer_token(accessCode) %>% 
            req_headers(
                "trakt-api-key" = trakt_id,
                "Content-Type" = "application/json",
                "trakt-api-version" = 2
            )
        
        resp <- req_perform(req2)
        
        history <- resp_body_json(resp) %>% 
            map_df(function(x) {
                df <- tibble(
                    date = x$watched_at,
                    id = x$id,
                    title = x$episode$title,
                    show = x$show$title,
                    season = x$episode$season,
                    episode = x$episode$number,
                    slug = x$show$ids$slug,
                    tvdb = x$show$ids$tvdb
                )
                return(df)
            }) %>% 
            mutate(date = ymd_hms(date))
        
        
        ## Remove duplicate plays (i.e. anything marked played at the same date, same time)
        history <- distinct(history)
        
        ## And just remove everything from August 30 2015
        history <- history %>% filter(date(date) != '2015-08-30')
        
        write_csv(history, str_c(lubridate::today(), '-traktHistory.csv'))
        write_csv(history, 'traktHistory.csv')
        
        ## Dropbox authentication and save
        dropbox <- readRDS('dropbox.rds')
        
        reqUpload <- request('https://content.dropboxapi.com/2/files/upload/') %>% 
            req_auth_bearer_token(dropbox$access_token) %>% 
            req_headers('Content-Type' = 'application/octet-stream') %>% 
            req_headers(
                'Dropbox-API-Arg' = str_c('{',
                                          '"autorename":false,',
                                          '"mode":"overwrite",',
                                          '"path":"/R/trakt/traktHistory.csv",',
                                          '"strict_conflict":false', 
                                          '}')
            ) %>% 
            req_body_file(path = here::here('traktHistory.csv'))
        
        respUpload <- req_perform(reqUpload)
        
    } else {
        
        dropbox <- readRDS('dropbox.rds')
        
        reqDownload <-  request("https://content.dropboxapi.com/2/files/download") %>% 
            req_auth_bearer_token(dropbox$access_token) %>%
            req_method('POST') %>%
            req_headers(
                'Dropbox-API-Arg' = str_c('{',
                                          '"path":"/R/trakt/traktHistory.csv"',
                                          '}')
            )
        
        respDownload <- req_perform(reqDownload,
                                    path = here::here('traktHistory.csv'))
        
        history <- readr::read_csv('traktHistory.csv')
    }
    
    return(history)
    
}
