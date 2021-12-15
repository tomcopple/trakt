## Authenticate Trakt API through Oauth2.0

traktShinyAuth <- function() {
    # Environmental variables -------------------------------------------------
    trakt_id <- Sys.getenv('TRAKTSHINY_ID')
    trakt_secret <- Sys.getenv('TRAKTSHINY_SECRET')
    traktUser <- Sys.getenv("TRAKT_USER")
    traktApi <- Sys.getenv('TRAKT_API')
    
    ### Get a token, don't need to do this every time?
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
    
    if (token$credentials$expires_in < 700000){
        token$refresh()    
    }
    
    accessCode <- token$credentials$access_token
    
    return(accessCode)
}

