## Remove from history

ids <- history %>% 
    # traktHis %>% 
    # group_by(show, title) %>% 
    # add_count() %>% 
    # mutate(day = as_date(date)) %>% 
    # filter(day == "2011-08-29") %>%  
    # filter(show == 'The Wire') %>% 
    # filter(n > 1) %>% ungroup() %>%
    filter(date > lubridate::dmy_hms("27-11-24 18:00:00"),
           date < lubridate::dmy_hms("27-11-24 18:30:00")) %>% 
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
