getMyRatings <- function(refresh = TRUE) {
    
    
    dropboxClient <- oauth_client(
        id = Sys.getenv('DROPBOX_KEY'),
        secret = Sys.getenv('DROPBOX_SECRET'),
        token_url = "https://api.dropboxapi.com/oauth2/token",
        name = 'Rstudio_TC'
    )
    dropboxToken <- readRDS('dropbox.rds')
    
    traktClient <- oauth_client(
        id = Sys.getenv('TRAKTSHINY_ID'),
        secret = Sys.getenv('TRAKTSHINY_SECRET'),
        token_url = 'https://api.trakt.tv/oauth/token',
        name = 'traktShiny'
    )
    traktToken <- readRDS('trakt.rds')

    print(str_glue("Checking dropboxClient id ({dropboxClient$id}), token ({dropboxToken$refresh_token})"))
    
    require(tidyverse);require(lubridate);require(httr2)
    if (refresh) {
        
        print("Refreshing trakt ratings")
        # Create url
        baseurl <- "https://api.trakt.tv/users/"
        call <- "/ratings/episodes"
        
        req <- request(base_url = str_glue(
            "{baseurl}tomcopple{call}"
        ))
        
        trakt_id <- Sys.getenv('TRAKTSHINY_ID')
        # print(str_glue("Checking trakt id: {trakt_id}, access code: {accessCode}"))
        
        # Set info for GET request. 
        req2 <- req %>% 
            # req_oauth_refresh(client = traktClient, 
                              # refresh_token = traktToken$refresh_token) %>% 
            req_oauth_auth_code(
                client = traktClient,
                auth_url = 'https://trakt.tv/oauth/authorize',
                redirect_uri = 'http://localhost:43451/'
                ) %>%
            # req_auth_bearer_token(accessCode) %>%
            req_headers(
                "trakt-api-key" = trakt_id,
                "Content-Type" = "application/json",
                "trakt-api-version" = 2
            )
        
        resp <- req_perform(req2)
        
        print('Performed req')
        
        ratings <- resp_body_json(resp) %>% 
            map_df(function(x) {
                df <- tibble(
                    date = x$rated_at,
                    rating = x$rating,
                    title = x$episode$title,
                    show = x$show$title,
                    season = x$episode$season,
                    episode = x$episode$number,
                    slug = x$show$ids$slug,
                    tvdb = x$show$ids$tvdb
                )
                return(df)
            }) %>% 
            mutate(date = ymd(str_sub(date, 1, 10)))
        
        # Archer has some weird season/episode numbering, needs fixing. 
        ratings <- ratings %>% 
            mutate(
                episode = ifelse(show == "Archer" & season == 3,
                                 episode + 3, episode),
                episode = ifelse(show == "Archer" & season == 0,
                                 episode - 3, episode),
                season = ifelse(show == "Archer" & season == 0,
                                3, season)
            ) %>% 
            ## Want to include new Doctor Who as continuation of previous
            mutate(
                season = ifelse(
                    slug == 'doctor-who-2024' & season != 0,
                    season + 13,
                    season
                )
            ) %>% 
            ## Don't include specials
            filter(season != 0) %>% 
            ## And filter out shows with only one rating
            group_by(show) %>% filter(n() > 1)
        
        write_csv(ratings, 'traktRatings.csv')
        
        print("Uploading to Dropbox")
        reqUpload <- request('https://content.dropboxapi.com/2/files/upload/') %>% 
            req_oauth_auth_code(
                client = dropboxClient,
                auth_url = "https://www.dropbox.com/oauth2/authorize?token_access_type=offline",
                redirect_uri = 'http://localhost:43451/'
            ) %>%
            # req_oauth_refresh(client = dropboxClient, 
                              # refresh_token = dropboxToken$refresh_token) %>% 
            # req_auth_bearer_token(dropboxToken$refresh_token) %>% 
            req_headers('Content-Type' = 'application/octet-stream') %>% 
            req_headers(
                'Dropbox-API-Arg' = str_c('{',
                                          '"autorename":false,',
                                          '"mode":"overwrite",',
                                          '"path":"/R/trakt/traktRatings.csv",',
                                          '"strict_conflict":false', 
                                          '}')
            ) %>% 
            req_body_file(path = 'traktRatings.csv')
        
        respUpload <- req_perform(reqUpload)
    } else {
        
        print("Downloading trakt ratings from Dropbox")
        
        reqDownload <-  request("https://content.dropboxapi.com/2/files/download") %>% 
            req_oauth_refresh(client = dropboxClient, 
                              refresh_token = dropboxToken$refresh_token) %>% 
            req_method('POST') %>%
            req_headers(
                'Dropbox-API-Arg' = str_c('{',
                                          '"path":"/R/trakt/traktRatings.csv"',
                                          '}')
            )
        
        respDownload <- req_perform(reqDownload,
                                    path = 'traktRatings.csv')
        
        ratings <- readr::read_csv('traktRatings.csv', show_col_types = FALSE)
    }

    return(ratings)
    
    }
