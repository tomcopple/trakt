## One-time authenticate dropbox script
library(httr2)

# Your Dropbox credentials
app_key <- Sys.getenv("DROPBOX_KEY")
app_secret <- Sys.getenv("DROPBOX_SECRET")

# Create OAuth client
dropbox_client <- oauth_client(
    id = app_key,
    secret = app_secret,
    token_url = "https://api.dropbox.com/oauth2/token",
    name = "RStudio_TC"
)

# Authenticate
token <- oauth_flow_auth_code(
    client = dropbox_client,
    auth_url = "https://www.dropbox.com/oauth2/authorize?token_access_type=offline",
    redirect_uri = "http://localhost:1410/"
)

# Save token for your app
saveRDS(token, "dropbox_token.rds")
message("Token saved! You can now use it in your Shiny app.")
