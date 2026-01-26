## Remove from history
library(here);library(tidyverse);library(lubridate);library(httr2)

### Get a token - at the moment have to do this every time
traktClient <- oauth_client(
    id = Sys.getenv('TRAKTSHINY_ID'),
    secret = Sys.getenv('TRAKTSHINY_SECRET'),
    token_url = "https://api.trakt.tv/oauth/token",
    name = 'traktShiny'
)

reqAuthTrakt <- function(req) {
    req_oauth_auth_code(
        req, 
        client = traktClient,
        redirect_uri = "http://localhost:43451",
        auth_url = "https://trakt.tv/oauth/authorize"
    )
}

## Get TV episodes first, then movies (could probably merge this) ----

# Create url
baseurl <- "https://api.trakt.tv/users/"
call <- "/history/episodes?limit=100000"

req <- request(base_url = str_glue(
    "{baseurl}me{call}"
))

trakt_id <- Sys.getenv('TRAKTSHINY_ID')

req2 <- req %>% 
    reqAuthTrakt() %>% 
    # req_auth_bearer_token(accessCode) %>% 
    req_headers(
        "trakt-api-key" = trakt_id,
        "Content-Type" = "application/json",
        "trakt-api-version" = 2
    )

resp <- req_perform(req2)

history <- resp_body_json(resp) %>% 
    map_df(function(x) {
        df <- tibble(
            date = x$watched_at,
            id = x$id,
            title = x$episode$title,
            show = x$show$title,
            season = x$episode$season,
            episode = x$episode$number,
            slug = x$show$ids$slug,
            tvdb = x$show$ids$tvdb
        )
        return(df)
    }) %>% 
    mutate(date = ymd_hms(date))

## Filter fir what needs removing here:
ids <- history %>% 
    # group_by(show, title) %>% 
    # add_count() %>% 
    # mutate(day = as_date(date)) %>% 
    # filter(day == "2011-08-29") %>%  
    # filter(show == 'The Wire') %>% 
    # filter(n > 1) %>% ungroup() %>%
    # filter(date > lubridate::dmy_hms("27-11-24 18:00:00"),
           # date < lubridate::dmy_hms("27-11-24 18:30:00")) %>% 
    filter(as_date(date) == lubridate::today()) %>% 
    pull(id)


removeURL <- "https://api.trakt.tv/sync/history/remove/"
req <- request(base_url = removeURL)
reqFinal <- req %>% 
    # req_auth_bearer_token(accessCode) %>% 
    req_oauth_auth_code(
        client = traktClient,
        auth_url = 'https://trakt.tv/oauth/authorize',
        redirect_uri = 'http://localhost:43451/'
    ) %>%
    req_headers(
        "trakt-api-key" = trakt_id,
        "Content-Type" = "application/json",
        "trakt-api-version" = 2
    ) %>% 
    req_body_json(list(ids = as.list(ids)))

respRemove <- req_perform(reqFinal)

## And check for movies ----
req <- request(base_url = "https://api.trakt.tv/users/me/history/movies?limit=5000")

trakt_id <- Sys.getenv('TRAKTSHINY_ID')

req2 <- req %>% 
    reqAuthTrakt() %>% 
    # req_auth_bearer_token(accessCode) %>% 
    req_headers(
        "trakt-api-key" = trakt_id,
        "Content-Type" = "application/json",
        "trakt-api-version" = 2
    )

resp <- req_perform(req2)

history <- resp_body_json(resp) %>% 
    map_df(function(x) {
        df <- tibble(
            date = x$watched_at,
            id = x$id,
            title = x$movie$title
        )
        return(df)
    }) %>% 
    mutate(date = ymd_hms(date))
