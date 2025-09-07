## Get trakt movies
library(tidyverse);library(httr);library(jsonlite);library(lubridate);library(xml2)

traktClient <- oauth_client(
    id = Sys.getenv('TRAKTSHINY_ID'),
    secret = Sys.getenv('TRAKTSHINY_SECRET'),
    token_url = 'https://api.trakt.tv/oauth/token',
    name = 'traktShiny'
)

req <- httr2::request(base_url = 'https://api.trakt.tv/users/tomcopple/history/movies?limit=100000') %>% 
    req_oauth_auth_code(
        client = traktClient,
        auth_url = 'https://trakt.tv/oauth/authorize',
        redirect_uri = 'http://localhost:43451/'
    ) %>%
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
