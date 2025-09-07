### Testing Hardcover API

library(httr2);library(tidyverse);library(jsonlite)



baseurl <- "https://api.hardcover.app/v1/graphql"
token <- Sys.getenv('HARDCOVER_BEARER')

req <- request(baseurl) %>% 
    req_headers(
        # `content-type` = "application/json",
        Authorization = str_glue("Bearer {token}")
    ) %>% 
    req_body_json(list(
        "query" = "query {
        me {
            user_books(
                order_by: {last_read_date: desc_nulls_last}
                where: {status_id: {_eq: 3}, last_read_date: {_gte: \"2024-01-01\"}}
            ) {
                book {
                    contributions(limit: 1) {
                        author {
                            name
                        }
                    }
                    title
                }
                user_book_reads {
                    finished_at
                    started_at
                    user_book {
                        rating
                    }
                }
                user {
                    id
                }
            }
     }
        }"
    ))

resp <- req_perform(req)
resp %>% resp_body_json(simplifyVector = T) %>% 
    pluck('data', 'me', 'user_books', 1) %>% 
    unnest(c('book', 'user_book_reads')) %>% 
    unnest(c('contributions', 'user_book')) %>% 
    unnest('author') %>% 
    select(author = 1, title = 2, date_finished = 3, date_started = 4, rating = 5)
