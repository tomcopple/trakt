library(httr2)

authTrakt <- function(state = 'local') {
    
    # Trakt credentials
    trakt_id <- Sys.getenv("TRAKT2_ID")
    trakt_secret <- Sys.getenv('TRAKT2_SECRET')

    if(state == 'local') {
        
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
        
    } else {
        
        # authenticate_trakt_oob.R - Run this LOCALLY but for OOB use
        library(jsonlite)
        
        # Step 1: Generate authorization URL
        auth_url <- paste0(
            "https://trakt.tv/oauth/authorize?",
            "response_type=code",
            "&client_id=", trakt_id,
            "&redirect_uri=urn:ietf:wg:oauth:2.0:oob"
        )
        
        cat("1. Visit this URL in your browser:\n")
        cat(auth_url, "\n\n")
        cat("2. Authorize the app\n")
        cat("3. Copy the authorization code displayed\n\n")
        
        # Step 2: Get the code from user
        auth_code <- "24e55e01"
        
        # Step 3: Exchange code for token
        token_response <- request("https://api.trakt.tv/oauth/token") |>
            req_method("POST") |>
            req_body_json(list(
                code = auth_code,
                client_id = trakt_id,
                client_secret = trakt_secret,
                redirect_uri = "urn:ietf:wg:oauth:2.0:oob",
                grant_type = "authorization_code"
            )) |>
            req_perform() |>
            resp_body_json()
        
        # Step 4: Create token object in httr2 format
        token <- structure(
            list(
                access_token = token_response$access_token,
                token_type = token_response$token_type,
                expires_in = token_response$expires_in,
                refresh_token = token_response$refresh_token,
                scope = token_response$scope,
                created_at = token_response$created_at,
                expires_at = as.POSIXct(token_response$created_at + token_response$expires_in, origin = "1970-01-01")
            ),
            class = "httr2_token"
        )
        
        # Verify we got a refresh token
        if (is.null(token$refresh_token)) {
            stop("No refresh token received!")
        }
        
        # Step 5: Save token
        saveRDS(token, "trakt_token.rds")
        
        cat("\n✓ Token saved successfully!\n")
        cat("✓ Access token expires at:", format(token$expires_at), "\n")
        cat("✓ Refresh token present\n")
        cat("\nYou can now deploy this token to shinyapps.io\n")
        
    }
}

