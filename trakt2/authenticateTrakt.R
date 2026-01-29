library(httr2)

# Trakt credentials
trakt_id <- Sys.getenv("TRAKT2_ID")
trakt_secret <- Sys.getenv('TRAKT2_SECRET')

## Use localhost redirect when running on laptop

trakt_client <- oauth_client(
    id = trakt_id,
    secret = trakt_secret,
    token_url = "https://api.trakt.tv/oauth/token",
    name = "Trakt2"
)

# Authenticate
token <- oauth_flow_auth_code(
    client = trakt_client,
    auth_url = "https://trakt.tv/oauth/authorize",
    redirect_uri = "http://localhost:1410/"
)

# Save token to include in deployment
saveRDS(token, "trakt_token.rds")
message("Token saved! Include this file when deploying to shinyapps.io")

