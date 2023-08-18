# Get banner image for TV Show from thetvdb

getBanners <- function(refresh = FALSE, slugs = NA, tvdb = NA) {
    
    if (refresh) {
        
        library(tidyverse);library(httr2);library(httr)
        
        getImages <- function(slugs, tvdb) {
            
            # Authenticate first
            getToken <- httr::POST(url = "https://api.thetvdb.com/login", 
                                   body = list(apikey = "A0D79C9369911838"),
                                   encode = "json") %>% 
                httr::content(., as = "parsed")
            tvdbToken <- getToken$token
            
            APIurl = paste0("https://api.thetvdb.com/series/", 
                            tvdb, "/images/query?keyType=series")
            
            imageLinks <- data_frame(
                show = slugs, 
                imageLink = paste0(
                    "https://thetvdb.com/banners/",
                    httr::GET(url = APIurl, 
                              add_headers(Authorization = paste("Bearer", tvdbToken))) %>% 
                        httr::content(., as = "parsed") %>% 
                        extract2('data') %>% extract2(1) %>% extract2('fileName'))
            )
            
            return(imageLinks)
        }
        
        images <- map2_df(slugs, tvdb, getImages)
        
        write_csv(images, 'images.csv')
        
        ## Dropbox authentication and save
        dropbox <- readRDS('dropbox.rds')
        
        reqUpload <- request('https://content.dropboxapi.com/2/files/upload/') %>% 
            req_auth_bearer_token(dropbox$access_token) %>% 
            req_headers('Content-Type' = 'application/octet-stream') %>% 
            req_headers(
                'Dropbox-API-Arg' = str_c('{',
                                          '"autorename":false,',
                                          '"mode":"overwrite",',
                                          '"path":"/R/trakt/images.csv",',
                                          '"strict_conflict":false', 
                                          '}')
            ) %>% 
            req_body_file(path = 'images.csv')
        
        respUpload <- req_perform(reqUpload)
        
    } else {
        
        dropbox <- readRDS('dropbox.rds')
        
        reqDownload <-  request("https://content.dropboxapi.com/2/files/download") %>% 
            req_auth_bearer_token(dropbox$access_token) %>%
            req_method('POST') %>%
            req_headers(
                'Dropbox-API-Arg' = str_c('{',
                                          '"path":"/R/trakt/images.csv"',
                                          '}')
            )
        
        respDownload <- req_perform(reqDownload,
                                    path = 'images.csv')
        
        images <- readr::read_csv('images.csv')
        
    }
    
    return(images)
}

