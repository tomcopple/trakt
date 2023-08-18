getMyRatings <- function(accessCode) {
    require(tidyverse);require(lubridate);require(jsonlite);require(httr)
    require(httr2)
    
    # Create url
    baseurl <- "https://api-v2launch.trakt.tv/users/"
    call <- "/ratings/episodes"
    
    req <- request(base_url = str_glue(
        "{baseurl}tomcopple{call}"
    ))
    
    trakt_id <- Sys.getenv('TRAKTSHINY_ID')
    
    
    # Set info for GET request. 
    req2 <- req %>% 
        req_auth_bearer_token(accessCode) %>%
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
    
    return(ratings)
    
    }
