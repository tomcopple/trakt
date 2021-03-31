## Download all trakt scrobbles, and save local copy

getTraktHistory <- function(refresh = TRUE) {
    
    library(tidyverse);library(lubridate);library(stringr);library(httr);library(jsonlite);library(rdrop2)
    
    if (refresh) {
        
        ## Authorisation - need to go through Oauth to access private users
        trakt_id <- Sys.getenv('TRAKTSHINY_ID')
        trakt_secret <- Sys.getenv('TRAKTSHINY_SECRET')
        traktUser <- Sys.getenv("TRAKT_USER")
        traktApi <- Sys.getenv('TRAKT_API')
        
        # Option 1: Enter a code --------------------------------------------------
        
        ## This works in the Project, but won't work in Shiny
        app <- oauth_app(appname = "traktShiny",
                         key = trakt_id,
                         secret = trakt_secret,
                         redirect_uri = "urn:ietf:wg:oauth:2.0:oob")
        
        endpoint <- oauth_endpoint(
            authorize = "https://trakt.tv/oauth/authorize",
            access = "https://api.trakt.tv/oauth/token"
        )
        
        
        token <- oauth2.0_token(endpoint = endpoint,
                                app = app,
                                use_oob = TRUE
        )
        
        accessCode <- token$credentials$access_token
        
        # Create url
        baseurl <- "https://api-v2launch.trakt.tv/users/"
        call <- "/history/episodes?limit=100000"
        
        # Run setTrakt.R to get credentials (NB From environmental vars)
        traktUser <- Sys.getenv("TRAKT_USER")
        traktApi <- Sys.getenv('TRAKT_API')
        
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
