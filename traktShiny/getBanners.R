# Get banner image for TV Show from thetvdb

getBanners <- function(refresh = FALSE, slugs = NA, tvdb = NA) {
    
    if (refresh) {
        
        library(tidyverse);library(httr);library(jsonlite);library(magrittr);library(rdrop2)
        
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
        rdrop2::drop_upload(file = 'images.csv',
                            path = "R/trakt", dtoken = dropbox)
    } else {
        
        dropbox <- readRDS('dropbox.rds')
        images <- rdrop2::drop_read_csv(file = 'R/trakt/images.csv',
                                        dtoken = dropbox) %>% 
            as_tibble()
    }
    
    return(images)
}

