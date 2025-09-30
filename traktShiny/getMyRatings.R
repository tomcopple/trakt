getMyRatings <- function(refresh = TRUE, accessCode) {
    
    
    dropboxClient <- oauth_client(
        id = Sys.getenv('DROPBOX_KEY'),
        secret = Sys.getenv('DROPBOX_SECRET'),
        token_url = "https://api.dropboxapi.com/oauth2/token",
        name = 'Rstudio_TC'
    )
    
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
        print(str_glue("Checking trakt id: {trakt_id}, access code: {accessCode}"))
        
        
        # Set info for GET request. 
        req2 <- req %>% 
            reqAuthTrakt() %>% 
            # req_auth_bearer_token(accessCode) %>%
            req_headers(
                "trakt-api-key" = trakt_id,
                "Content-Type" = "application/json",
                "trakt-api-version" = 2
            )
        
        resp <- req_perform(req2)
        
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
            ## Don't include specials
            filter(season != 0) %>% 
            ## And filter out shows with only one rating
            group_by(show) %>% filter(n() > 1)
        
        write_csv(ratings, 'traktRatings.csv')
        
        reqUpload <- request('https://content.dropboxapi.com/2/files/upload/') %>% 
            reqAuthDropbox() %>% 
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
            reqAuthDropbox() %>% 
            # req_oauth_refresh(client = dropboxClient, 
                              # refresh_token = dropboxToken$refresh_token) %>% 
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
