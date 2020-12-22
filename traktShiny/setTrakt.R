## Set trakt credentials from Environnmental vars

readRenviron(".Renviron")

traktUser <- Sys.getenv("TRAKT_USER")
traktApi <- Sys.getenv('TRAKT_API')
