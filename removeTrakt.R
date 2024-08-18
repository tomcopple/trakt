## Remove from history

ids <- history %>% 
    group_by(show, title) %>% 
    add_count() %>% 
    mutate(day = as_date(date)) %>% 
    filter(day == "2011-08-29") %>%  
    filter(show == 'The Wire') %>% 
    # filter(n > 1) %>% ungroup() %>% 
    pull(id)


removeURL <- "https://api.trakt.tv/sync/history/remove/"
req <- request(base_url = removeURL)
reqFinal <- req %>% 
    req_auth_bearer_token(accessCode) %>% 
    req_headers(
        "trakt-api-key" = trakt_id,
        "Content-Type" = "application/json",
        "trakt-api-version" = 2
    ) %>% 
    req_body_json(list(ids = as.list(ids)))

respRemove <- req_perform(reqFinal)
