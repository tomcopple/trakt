# Run the API call for all ratings ==> "ratings"
getMyRatings <- function(env = .GlobalEnv) {
    
    library(tidyverse);library(lubridate);library(jsonlite);library(httr)
    
    # Create url (in case you want to change anything)
    # Assumes you're getting your own ratings. 
    baseurl <- "https://api-v2launch.trakt.tv/users/"
    call <- "/ratings/episodes"

    # Credentials set in Renviron
    traktUser <- Sys.getenv("TRAKT_USER")
    traktApi <- Sys.getenv('TRAKT_API')
    
    url <- paste0(baseurl, traktUser, call)
    
    # Set info for GET request. I don't think ratings requires a pages argument. 
    # NB This code taken from jemus42 - thanks!
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
            rating = rating, 
            episode = episode.title, 
            title = show.title, 
            seasonnum = episode.season, 
            episodenum = episode.number
        ) %>% 
        mutate(date = ymd(substr(response$rated_at, 0, 10))) %>% 
        as_data_frame()
    
    # Print to Global Environment (not sure if this is the best way to do this). 
    return(ratings)
}

