getMyRatings <- function() {
    require(tidyverse);require(lubridate);require(jsonlite);require(httr)
    
    # Create url
    baseurl <- "https://api-v2launch.trakt.tv/users/"
    call <- "/ratings/episodes"
    
    # Run setTrakt.R to get credentials
    source("setTrakt.R")

    url <- paste0(baseurl, traktUser, call)
    
    # Set info for GET request. 
    headers <- httr::add_headers(.headers = c("trakt-api-key" = traktApi,
                                              "Content-Type" = "application/json",
                                              "trakt-api-version" = 2))
    response <- httr::GET(url, headers)
    httr::stop_for_status(response)
    response <- httr::content(response, as = "text")
    response <- jsonlite::fromJSON(response, 
                                   simplifyDataFrame = TRUE, 
                                   flatten = TRUE)
    
    
    ratings <- response %>% 
        select(
            Rating = rating, 
            Title = episode.title, 
            Show = show.title, 
            Season = episode.season, 
            Episode = episode.number,
            slug = show.ids.slug,
            tvdb = show.ids.tvdb
        ) %>% 
        mutate(date = ymd(substr(response$rated_at, 0, 10)))  %>% 
        as_data_frame()
    
    # Archer has some weird season/episode numbering, needs fixing. 
    ratings <- ratings %>% 
        mutate(
            Episode = ifelse(Show == "Archer" & Season == 3,
                             Episode + 3, Episode),
            Episode = ifelse(Show == "Archer" & Season == 0,
                             Episode - 3, Episode),
            Season = ifelse(Show == "Archer" & Season == 0,
                            3, Season)
        ) %>% 
        filter(Season != 0)
    
    min <- count(ratings, Show) %>% filter(n > 2)
    ratings <- filter(ratings, Show %in% min$Show)

        return(ratings)

    }
