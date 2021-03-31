getMyRatings <- function() {
    require(tidyverse);require(lubridate);require(jsonlite);require(httr)
    
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
    call <- "/ratings/episodes"
    
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
    
    
    ratings <- response %>% 
        select(
            rating, 
            title = episode.title, 
            show = show.title, 
            season = episode.season, 
            episode = episode.number,
            slug = show.ids.slug,
            tvdb = show.ids.tvdb
        ) %>% 
        mutate(date = ymd(substr(response$rated_at, 0, 10)))  %>% 
        as_tibble()
    
    # Archer has some weird season/episode numbering, needs fixing. 
    ratings <- ratings %>% 
        mutate(
            spisode = ifelse(show == "Archer" & season == 3,
                             episode + 3, episode),
            episode = ifelse(show == "Archer" & season == 0,
                             episode - 3, episode),
            season = ifelse(show == "Archer" & season == 0,
                            3, season)
        ) %>% 
        filter(season != 0)
    
    min <- count(ratings, show) %>% filter(n > 2)
    
    ratings <- filter(ratings, show %in% min$show)

        return(ratings)

    }
