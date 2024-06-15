## Download all trakt scrobbles, and save local copy

getTraktHistory <- function(refresh = TRUE, accessCode) {
    
    dropboxClient <- oauth_client(
        id = Sys.getenv('DROPBOX_KEY'),
        secret = Sys.getenv('DROPBOX_SECRET'),
        token_url = "https://api.dropboxapi.com/oauth2/token",
        name = 'Rstudio_TC'
    )
    dropboxToken <- readRDS('dropbox.rds')
    
    
    library(tidyverse);library(lubridate);library(stringr);library(httr2)

    if (refresh) {
        
        # Create url
        baseurl <- "https://api.trakt.tv/users/"
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
        ## And all these other dates too which look wrong
        history <- history %>% 
            # filter(date(date) != '2015-08-30',
            #        date(date) != '2015-05-01',
            #        date(date) != '2015-06-18',
            #        date(date) != '2015-06-16',
            #        date(date) != '2011-08-29') %>% 
            filter(year(date) > 2010) %>% 
            mutate(season = ifelse(slug == 'doctor-who-2014', season + 13, season))
        
        write_csv(history, str_c(lubridate::today(), '-traktHistory.csv'))
        write_csv(history, 'traktHistory.csv')
        
        reqUpload <- request('https://content.dropboxapi.com/2/files/upload/') %>% 
            req_oauth_refresh(client = dropboxClient, 
                              refresh_token = dropboxToken$refresh_token) %>% 
            # req_auth_bearer_token(dropboxToken$refresh_token) %>% 
            req_headers('Content-Type' = 'application/octet-stream') %>% 
            req_headers(
                'Dropbox-API-Arg' = str_c('{',
                                          '"autorename":false,',
                                          '"mode":"overwrite",',
                                          '"path":"/R/trakt/traktHistory.csv",',
                                          '"strict_conflict":false', 
                                          '}')
            ) %>% 
            req_body_file(path = 'traktHistory.csv')
        
        respUpload <- req_perform(reqUpload)
        
    } else {
    
        reqDownload <-  request("https://content.dropboxapi.com/2/files/download") %>% 
            req_oauth_refresh(client = dropboxClient, 
                              refresh_token = dropboxToken$refresh_token) %>% 
            req_method('POST') %>%
            req_headers(
                'Dropbox-API-Arg' = str_c('{',
                                          '"path":"/R/trakt/traktHistory.csv"',
                                          '}')
            )
        
        respDownload <- req_perform(reqDownload,
                                    path = 'traktHistory.csv')
        
        history <- readr::read_csv('traktHistory.csv', show_col_types = FALSE)
    }
    
    return(history)
    
}
