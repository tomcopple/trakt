## Get trakt movies
library(tidyverse);library(httr);library(jsonlite);library(lubridate);library(xml2)

req <- httr2::request(base_url = 'https://api.trakt.tv/users/tomcopple/history/movies?limit=100000') %>% 
    reqAuthTrakt() %>% 
    # req_auth_bearer_token(accessCode) %>%
    req_headers(
        "trakt-api-key" = trakt_id,
        "Content-Type" = "application/json",
        "trakt-api-version" = 2
    )

resp <- req_perform(req)


traktHis <- resp_body_json(resp) %>% 
    map_df(function(x) {
        df <- tibble(
            title = x$movie$title,
            date = x$watched_at,
            id = x$id
            )
        return(df)
    }) %>% 
    mutate(date = ymd_hms(date))
