getMyRatings <- function(accessCode) {
    require(tidyverse);require(lubridate);require(jsonlite);require(httr)
    
    # Create url
    baseurl <- "https://api-v2launch.trakt.tv/users/"
    call <- "/ratings/episodes"
    
    url <- paste0(baseurl, 'tomcopple', call)
    
    trakt_id <- Sys.getenv('TRAKTSHINY_ID')
    
    
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
            episode = ifelse(show == "Archer" & season == 3,
                             episode + 3, episode),
            episode = ifelse(show == "Archer" & season == 0,
                             episode - 3, episode),
            season = ifelse(show == "Archer" & season == 0,
                            3, season)
        ) %>% 
        filter(season != 0)
    
    min <- count(ratings, show) %>% filter(n > 1)
    
    ratings <- filter(ratings, show %in% min$show)

        return(ratings)

    }
