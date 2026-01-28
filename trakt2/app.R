## Second attempt to make a trakt shiny app with authentication
## Testing dropbox first

library(shiny);library(tidyverse);library(httr2);library(jsonlite)
library(shinymaterial);library(plotly);library(zoo);library(scales)
# options(shiny.autoreload=TRUE)

readRenviron(".Renviron")

# ===== SETUP (runs once at app startup) =====
# Your Dropbox credentials
app_key <- Sys.getenv("DROPBOX_KEY")
app_secret <- Sys.getenv("DROPBOX_SECRET")

print(str_glue("Dropbox key: {app_key}"))
print(str_glue("Dropbox secret: {app_secret}"))

# Create OAuth client
dropbox_client <- oauth_client(
    id = app_key,
    secret = app_secret,
    token_url = "https://api.dropbox.com/oauth2/token",
    name = "RStudio_TC"
)

# Load saved token
if (!file.exists("dropbox_token.rds")) {
    stop("No token found. Run authenticateDropbox.R first.")
}
dropbox_token <- readRDS("dropbox_token.rds")

# ===== DROPBOX API FUNCTIONS =====
# Base request with authentication
dropbox_request <- function(endpoint, api_type = "api") {
    base_url <- switch(api_type,
                       "api" = "https://api.dropboxapi.com/2",
                       "content" = "https://content.dropboxapi.com/2",
                       stop("Invalid api_type")
    )
    
    request(base_url) |>
        req_url_path_append(endpoint) |>
        req_oauth_refresh(
            client = dropbox_client,
            refresh_token = dropbox_token$refresh_token
        )
}
# Download a file from Dropbox
download_dropbox_file <- function(dropbox_path) {
    resp <- dropbox_request("/files/download", api_type = "content") |>
        req_headers(
            `Dropbox-API-Arg` = toJSON(list(path = dropbox_path), auto_unbox = TRUE)
        ) |>
        req_perform()
    
    
    read_csv(rawToChar(resp_body_raw(resp)))
}
# Upload a file to Dropbox
upload_dropbox_file <- function(local_path, dropbox_path, mode = "add") {
    file_content <- readBin(local_path, "raw", file.info(local_path)$size)
    
    resp <- dropbox_request("/files/upload", api_type = "content") |>
        req_headers(
            `Dropbox-API-Arg` = toJSON(list(
                path = dropbox_path,
                mode = mode,
                autorename = FALSE,
                mute = FALSE
            ), auto_unbox = TRUE),
            `Content-Type` = "application/octet-stream"
        ) |>
        req_body_raw(file_content) |>
        req_perform()
    
    resp_body_json(resp)
}



## Download data first here ----
ratings <- download_dropbox_file("/R/trakt/traktRatings.csv")
history <- download_dropbox_file("/R/trakt/traktHistory.csv")
images <- download_dropbox_file("/R/trakt/images.csv")

showList <- history %>% 
    count(show) %>% 
    filter(n > 3) %>% 
    arrange(show) %>% pull('show')


# Trakt authentication ----
trakt_id <- Sys.getenv("TRAKT2_ID")
trakt_secret <- Sys.getenv('TRAKT2_SECRET')

print(str_glue("Trakt ID: {trakt_id}"))
print(str_glue("Trakt Secret: {trakt_secret}"))

trakt_client <- oauth_client(
    id = trakt_id,
    secret = trakt_secret,
    token_url = "https://api.trakt.tv/oauth/token",
    name = "Trakt2"
)


trakt_token <- readRDS('trakt_token.rds')

## Basic trakt request
trakt_request <- function(endpoint) {
    request("https://api.trakt.tv") |>
        req_url_path_append(endpoint) |>
        req_oauth_refresh(
            client = trakt_client,
            refresh_token = trakt_token$refresh_token
        ) |>
        req_headers(
            `Content-Type` = "application/json",
            `trakt-api-version` = "2",
            `trakt-api-key` = trakt_id
        )
}

getTraktRatings <- function() {
    
    print("Getting trakt ratings")
    
    resp <- trakt_request('/users/tomcopple/ratings/episodes') %>% 
        req_perform()
    
    ratings <- resp_body_json(resp) %>% 
        map_df(function(x) {
            df <- tibble(
                date = x$rated_at,
                rating = x$rating,
                title = x$episode$title,
                show = x$show$title,
                season = x$episode$season,
                episode = x$episode$number,
                slug = x$show$ids$slug,
                tvdb = x$show$ids$tvdb
            )
            return(df)
        }) %>% 
        mutate(date = ymd(str_sub(date, 1, 10)))
    
    ratings <- ratings %>% 
        mutate(
            episode = ifelse(show == "Archer" & season == 3,
                             episode + 3, episode),
            episode = ifelse(show == "Archer" & season == 0,
                             episode - 3, episode),
            season = ifelse(show == "Archer" & season == 0,
                            3, season)
        ) %>% 
        ## Don't include specials
        filter(season != 0) %>% 
        ## And filter out shows with only one rating
        group_by(show) %>% filter(n() > 1)
    
    temp_file <- tempfile(fileext = ".csv")
    write_csv(ratings, temp_file)
    print("Got ratings, uploading to Dropbox")
    upload_dropbox_file(temp_file, "/R/trakt/traktRatings.csv", "overwrite")
    unlink(temp_file)
    print("Done with ratings!")
    
    return(ratings)
}

getTraktHistory <- function() {
    
    print("Getting trakt history")
    resp <- trakt_request("/users/tomcopple/history/episodes") %>% 
        req_url_query(limit = 100000) %>% 
        req_perform()
    
    history <- resp_body_json(resp) %>% 
        map_df(function(x) {
            df <- tibble(
                date = x$watched_at,
                id = x$id,
                title = x$episode$title,
                show = x$show$title,
                season = x$episode$season,
                episode = x$episode$number,
                slug = x$show$ids$slug,
                tvdb = x$show$ids$tvdb
            )
            return(df)
        }) %>% 
        mutate(date = ymd_hms(date)) %>% 
        distinct() %>% 
        filter(date(date) != '2015-08-30',
               date(date) != '2015-05-01',
               date(date) != '2015-06-18',
               date(date) != '2015-06-16',
               date(date) != '2011-08-29')
    
    temp_file <- tempfile(fileext = ".csv")
    write_csv(history, temp_file)
    print("Got history, uploading to Dropbox")
    upload_dropbox_file(temp_file, "/R/trakt/traktHistory.csv", "overwrite")
    unlink(temp_file)
    
    print("Done with history!")
    
    return(history)
}

# UI ----
ui <- material_page(
    
    title = "Trakt Dashboard",
    nav_bar_color = "DEEP_ORANGE",
    
    material_tabs(
        tabs = c(
            "Main page"  = "main_page",
            "Show page"  = "show_page",
            "Maintenance"= "maintenance"
        )
    ),
    
    ## Main Page UI ----
    material_tab_content(
        tab_id = "main_page",
        tags$h2("Summary of recent plays"),
        
        material_row(
            material_column(
                width = 3,
                
                material_card(
                    title = "",
                    material_radio_button(
                        input_id = "chooseYear",
                        label    = "Select year:",
                        color    = "lightgreen",
                        choices  = c("All time", "Last 30 days",
                                     seq.int(from = year(max(history$date)),
                                             to   = year(min(history$date))))
                    )
                )
            ),
            material_column(
                width = 9,
                material_switch(
                    input_id  = "mainInput",
                    off_label = "Show ratings",
                    on_label  = "Show plays",
                    initial_value = TRUE
                ),
                conditionalPanel(
                    condition = "input.mainInput == false",
                    material_switch(
                        input_id = "showLines",
                        off_label = "Show lines",
                        initial_value = FALSE
                    )
                ),
                material_card(
                    title = "",
                    plotlyOutput("plotlyTime",height="100%")
                ),
                material_card(
                    title = "",
                    material_slider(
                        input_id = "sliderBar", 
                        label = "",
                        min_value = 1,
                        max_value = 10,
                        step_size = 1,
                        initial_value = 1
                    ),
                    plotlyOutput("plotlyBar",height = "100%")
                ),
                material_card(
                    title = "",
                    material_slider(
                        input_id = "sliderRat", 
                        label = "",
                        min_value = 1,
                        max_value = 10,
                        step_size = 1,
                        initial_value = 1
                    ),
                    plotlyOutput("plotlyRat",height = "100%")
                )
            )
        )
    ),
    
    ## Show Page UI ----
    material_tab_content(
        tab_id = "show_page",
        material_row(
            material_column(
                width = 3,
                material_card(
                    title = NULL,
                    material_radio_button(
                        input_id = "chooseShow",
                        label    = "Choose show:",
                        color    = "DEEP_ORANGE",
                        choices  = showList,
                        selected = ifelse(
                            first(history$show) %in% showList,
                            first(history$show),
                            first(showList)
                        )
                    )
                )
            ),
            material_column(
                width = 9,
                material_row(
                    uiOutput("bannerImage")
                ),
                material_row(
                    material_card(
                        title = "",
                        plotlyOutput("showTimeSeries", height = "100%")
                    )
                ),
                material_row(
                    material_card(
                        title = "",
                        plotlyOutput("showPlot", height = "100%")
                    )
                ),
                material_row(
                    material_card(
                        title = "",
                        plotlyOutput("ratingsPlot", height = "100%")
                    )
                )
            )
        )
    ),
    
    ## Maintenance Page UI ----
    material_tab_content(
        tab_id = "maintenance",
        material_card(
            title = NULL,
            material_button(
                input_id = "refresh",
                label    = "Refresh data",
                icon     = "refresh"
            )
        ),
        material_card(
            material_button(
                input_id = "updateBanners",
                label    = "Update show banners",
                icon     = "refresh"
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    ## Set reactive values so they can be updated?
    values <- reactiveValues()
    values$ratings <- ratings
    values$history <- history
    values$images <- images
    
    ## Maintenance: Refresh Trakt ----
    observeEvent(input$refresh, {
        ## Assume this just works for some reason
        if (input$refresh > 0) {
            material_spinner_show(session, 'showPlot')
            material_spinner_show(session, 'ratingsPlot')
            shiny::showNotification("Refreshing data, may take some time...", type = "default")
            values$ratings <- getTraktRatings()
            values$history <- getTraktHistory()
            shiny::showNotification("Done!", type = "message")
            material_spinner_hide(session, "showPlot")
            material_spinner_hide(session, "ratingsPlot")
        }
    })
    
    ## Create filtered dataframe for selected year
    observeEvent(input$chooseYear, {
        chooseYear <- input$chooseYear
        print(str_glue("Selecting {chooseYear}"))
        
        if (chooseYear == 'All time') {
            values$filtered <- values$history %>% mutate(date = as_date(date))
            values$minDate <- as_date(min(values$filtered$date))
            values$maxDate <- as_date(max(values$filtered$date))
            
        } else if (chooseYear == 'Last 30 days') {
            values$minDate <- today() - days(30)
            values$maxDate <- today()
            values$filtered <- values$history %>% mutate(date = as_date(date)) %>% 
                filter(date >= values$minDate)
        } else {
            values$filtered <- values$history %>% mutate(date = as_date(date)) %>% 
                filter(year(date) == chooseYear)
            values$minDate <- dmy(str_c("01-01-", chooseYear))
            values$maxDate <- dmy(str_c("31-12-", chooseYear))
        }
        
        ## Get top 10 shows for the time period
        values$top10 <- values$filtered %>% count(show, sort = T) %>% head(10) %>% pull('show')
        values$top10data <- values$filtered %>% 
            filter(show %in% values$top10) %>% 
            mutate(date = as_date(date)) %>% 
            select(date, show)
        print(values$top10)
        
    })
    
    ## plotlyTime: Plotly time series chart ----
    output$plotlyTime <- renderPlotly({
        
        if (input$mainInput) {
            
            print(input$mainInput)
            top10plays <- values$top10data %>% 
                ## Create a count of plays per day
                count(show, date) %>% 
                ## Then create a 30 day rolling avg?
                spread(key = show, value = n) %>% 
                full_join(., tibble(
                    date = seq.Date(from = values$minDate, to = values$maxDate, by = 1)
                ), by = 'date') %>% 
                gather(-date, key = show, value = n) %>% 
                mutate(n = replace_na(n, 0)) %>% 
                arrange(show, date) %>% 
                group_by(show) %>% 
                mutate(plays = c(cumsum(n[1:29]),
                                 zoo::rollsum(n, k = 30, align = 'right'))) %>% 
                ungroup() %>% 
                mutate(show = forcats::fct_relevel(show, values$top10)) %>% 
                ## Something weird happening with old episode of Daily show, delete 
                ## anything that says oveer 60 plays in 1 month
                mutate(plays = ifelse(plays >= 60, 60, plays)) 
            
            ## Plot plotly graph
            top10plays %>% 
                group_by(show) %>% 
                plot_ly(type = 'scatter', mode = 'markers', colors = 'Set3') %>% 
                add_trace(x = ~date, y = ~plays, color = ~show, mode = 'lines',
                          fill = 'tozeroy', 
                          text = ~str_c(show, '<br>Plays: ', plays), hoverinfo = 'text') %>% 
                add_lines(data = values$filtered %>% 
                              count(date) %>% 
                              full_join(., tibble(
                                  date = seq.Date(from = values$minDate, to = values$maxDate, by = 1)),
                                  by = 'date'
                              ) %>% 
                              mutate(n = replace_na(n, 0)) %>% 
                              arrange(date) %>% ungroup() %>% 
                              mutate(total = c(cumsum(n[1:29]),
                                               zoo::rollsum(n, k = 30, align = 'right'))),
                          name = 'Total',
                          text = ~str_c("Total: ", total), hoverinfo = 'text',
                          line = list(color = I('yellow')),
                          x = ~date, y = ~total, yaxis = 'y2') %>% 
                layout(xaxis = list(title = ""),
                       yaxis = list(title = 'Monthly plays',
                                    rangemode = 'tozero'),
                       yaxis2 = list(title = 'Total', overlaying = 'y', side = 'right',
                                     rangemode = 'tozero'),
                       title = str_c('Top 10 shows in ', input$chooseYear),
                       legend = list(orientation = 'h', xanchor = 'left', x = 0,
                                     yanchor = 'top', y = -0.05))    
        } else {
            chooseYear <- input$chooseYear
            
            values$ratingsChartValues <- inner_join(
                x = values$filtered %>% arrange(date) %>% select(date, show, season, episode, title),
                y = values$ratings %>% select(show, season, episode, rating),
                # x = filtered %>% arrange(date) %>% select(date, show, season, episode, title),
                # y = ratings %>% select(show, season, episode, rating),
                by = c('show', 'season', 'episode'), 
                relationship = 'many-to-many'
            ) %>% 
                group_by(show) %>% 
                filter(rating > 0, !is.na(rating)) %>% 
                filter(n() >= 3)
            
            ratingsChart <- if(input$showLines) {
                print('Adding lines to chart')
                
                values$ratingsChartPlot <- values$ratingsChartValues %>% 
                    ungroup() %>% 
                    mutate(
                        legendgroup = str_glue(
                            "{show} s{str_pad(season, width = 2, side = 'left', pad = '0')}"
                        )) %>% 
                    arrange(show, date) %>% 
                    group_by(legendgroup) %>% 
                    mutate(date2 = as_date(date)) %>% 
                    mutate(n = n_distinct(date2)) %>% 
                    mutate(loess = ifelse(n <= 3, mean(rating),
                                          predict(loess(rating ~ as.numeric(date), span = 9))
                    ))
                
                ## Think it needs to be in a for loop
                p1 <- plot_ly(x = ~date, color = ~show)
                
                for (i in unique(values$ratingsChartPlot$legendgroup)) {
                    data <- filter(values$ratingsChartPlot, legendgroup == i)
                    # showLegend <- unique(data$season) == 1
                    minSeason <- min(values$ratingsChartPlot %>% 
                                         filter(show == unique(data$show)) %>% 
                                         pull(season))
                    showLegend <- unique(data$season) == minSeason
                    p1 <- p1 %>% 
                        add_markers(
                            data = data,
                            y = ~rating, alpha = 0.4,
                            name = ~show,
                            legendgroup = ~show,
                            showlegend = FALSE,
                            text = ~str_glue(
                                "{show} s{str_pad(season, width = 2, side = 'left', pad = '0')}e{str_pad(episode, width = 2, side = 'left', pad = '0')} 
                                 {title}: {rating}"
                            ),
                            hoverinfo = 'text'
                        ) %>% 
                        add_lines(
                            data = data,
                            name = ~show,
                            y = ~loess,
                            showlegend = showLegend,
                            legendgroup = ~show, 
                            text = ~legendgroup, hoverinfo = 'text',
                            line = list(shape = 'spline', smoothing = 1.3)
                        )
                }
                
                ratingsChart <- p1 %>% layout(xaxis = list(title = ""),
                                              yaxis = list(title = 'Ratings'),
                                              title = str_glue("Episode ratings {chooseYear}"),
                                              legend = list(x = 100, y = 1))
                
            } else {
                print('No lines on chart')
                plot_ly(values$ratingsChartValues,
                        x = ~date, y = ~rating, color = ~show, type = 'scatter', 
                        mode = 'markers',
                        colors = 'Spectral',
                        text = ~str_glue(
                            "{show} 
                                  s{str_pad(season, width = 2, side = 'left', pad = '0')} 
                                  e{str_pad(episode, width = 2, side = 'left', pad = '0')} 
                                  {title}: {rating}"
                        ),
                        hoverinfo = 'text') %>% 
                    layout(xaxis = list(title = ""),
                           showlegend = FALSE,
                           yaxis = list(title = 'Ratings'),
                           title = str_glue("Episode ratings {chooseYear}")
                    )
            }
            ratingsChart
        }
    })
    
    ## Top 10 plays barchart ---- 
    output$plotlyBar <- renderPlotly({
        
        ## Just a horizontal bar showing totals
        minSlice <- input$sliderBar * 10 - 9
        maxSlice <- input$sliderBar * 10
        
        barData <- values$filtered %>% 
            count(show) %>% 
            # nest(data = c(id, title, season, episode, date, slug, tvdb)) %>% 
            arrange(desc(n)) %>% 
            slice(c(minSlice:maxSlice)) %>% 
            # unnest(data) %>% 
            mutate(show = forcats::fct_inorder(show)) %>% 
            mutate(show = forcats::fct_rev(show)) 
        
        plot_ly(barData, x = ~n, y = ~show, color = ~show, type = "bar",
                text = ~n, textposition = "outside",
                orientation = "h", colors = "Spectral",
                hovertext = ~str_c(show, n, sep = ": <br>"), hoverinfo = "text"
        ) %>% 
            layout(showlegend = FALSE, bargap = 0.5,
                   xaxis = list(title = "", zeroline = FALSE),
                   yaxis = list(title = ""),
                   margin = list(l = 120, r = 40))
    })
    
    ## Main page ratings plot ---------
    output$plotlyRat <- renderPlotly({
    
        minSlice <- input$sliderRat * 10 - 9
        maxSlice <- input$sliderRat * 10
        
        ## Filter data for shows with more than 3 ratings then show top 10
        topRat <- values$filtered %>% 
            inner_join(values$ratings, by = c('show', 'season', 'episode'), relationship = 'many-to-many') %>% 
            group_by(show, season) %>% 
            filter(rating > 0, !is.na(rating)) %>% 
            filter(n() >= 3) %>% 
            summarise(avRat = mean(rating), n = n()) %>% 
            ungroup() %>% 
            arrange(desc(avRat)) %>% 
            slice(c(minSlice:maxSlice)) %>% 
            unite(show, season, col = 'show', sep = ' s') %>% 
            arrange(avRat) %>% 
            mutate(show = forcats::fct_inorder(show)) %>% 
            group_by(show)
        
        plot_ly(data = topRat, x = ~avRat, y = ~show, 
                color = ~show, type = 'bar',
                text = ~round(avRat, 1), textposition = 'outside', 
                orientation = 'h', colors = 'Spectral', 
                hovertext = ~str_c(show, " (", n, "): <br>", round(avRat, 1)), 
                hoverinfo = 'text') %>% 
            layout(showlegend = FALSE, bargap = 0.5, 
                   xaxis = list(title = '', zeroline = FALSE),
                   yaxis = list(title = ''),
                   margin = list(l = 120, r = 40))
        
        # browser()
    })
    
    ## zhow page charts ----
    output$bannerImage <- renderUI({
        
        # Weird extra text from shinymaterial, probably a bug. 
        chooseShow <- gsub("_shinymaterialdropdownspace_", " ", input$chooseShow)
        
        # Send img html tag for banner image, based on selected show
        # Some images don't seem to work...
        print(str_c("Getting banner image for ", chooseShow))
        
        ## Replacing ratings slug with history, as not every show has ratings, e.g. John Oliver
        showSlug <- values$history %>% 
            filter(show == chooseShow) %>% 
            distinct(slug) %>% 
            pull(slug)
        print(showSlug)
        imgSrc <- values$images %>% filter(show == showSlug) %>% pull(imageLink)
        print(imgSrc)
        tags$img(src = imgSrc,
                 width = "100%")
        
    })

    ## Show time series ----
    
    output$showTimeSeries <- renderPlotly({
        
        print("showTimeSeries")
        chooseShow <- gsub("_shinymaterialdropdownspace_", " ", input$chooseShow)
        # chooseShow <- "Last Week Tonight with John Oliver"
        
        
        showHistory <- values$history %>% 
            filter(show == chooseShow, season != 0) %>% 
            mutate(date = as_date(date)) %>% 
            count(season, date) %>% 
            spread(key = season, value = n) %>% 
            full_join(., tibble(
                date = seq.Date(from = values$minDate, to = values$maxDate, by = 1)
            ), by = 'date') %>% 
            gather(-date, key = season, value = n) %>% 
            mutate(n = replace_na(n, 0)) %>% 
            arrange(season, date) %>% 
            group_by(season) %>% 
            mutate(plays = c(cumsum(n[1:29]),
                             zoo::rollsum(n, k = 30, align = 'right'))) %>% 
            mutate(plays = ifelse(plays >= 60, 60, plays))
        
        showHistory %>% 
            group_by(season) %>% 
            plot_ly(type = 'scatter', mode = 'markers', colors = 'Set3') %>% 
            add_trace(x = ~date, y = ~plays, color = ~season, mode = 'lines',
                      fill = 'tozeroy',
                      text = ~str_c("S", season, ": ", plays), hoverinfo = 'text') %>% 
            layout(xaxis = list(title = ""),
                   yaxis = list(title = 'Monthly plays',
                                rangemode = 'tozero'),
                   # yaxis2 = list(title = 'Total', overlaying = 'y', side = 'right',
                   #               rangemode = 'tozero'),
                   title = "Total plays over time",
                   legend = list(orientation = 'h', xanchor = 'left', x = 0,
                                 yanchor = 'top', y = -0.05))    
        
    })
    
    ## Show plot ---- 
    
    output$showPlot <- renderPlotly({
        
        print('showPlot')
        
        chooseShow <- gsub("_shinymaterialdropdownspace_", " ", input$chooseShow)
        
        showRatings <- values$ratings %>% 
            filter(show == chooseShow) %>% 
            mutate(season = as.factor(season), rating = as.integer(rating),
                   episode = as.integer(episode)) %>% 
            arrange(season, episode)
        
        # Plot graph
        pGraph <- plot_ly(type = "scatter", mode = "markers") %>% 
            layout(title = paste("Ratings for", unique(showRatings$show)),
                   xaxis = list(linecolor = "#898989",
                                type = "linear"))
        
        # Add traces individually by season
        for (i in unique(showRatings$season)) {
            pGraph <- pGraph %>% 
                add_trace(
                    data = filter(showRatings, season == i),
                    y = ~rating,
                    x = ~episode,
                    color = ~season, 
                    type = "scatter", 
                    mode = "markers",
                    text = ~paste0(
                        title, "<br>", "s", 
                        ifelse(as.numeric(season) < 10, paste0("0", season), season),
                        "e", 
                        ifelse(episode < 10, paste0("0", episode), episode)), 
                    hoverinfo = ~"text",
                    legendgroup = i,
                    showlegend = TRUE
                )
            
            # Only want to add smoother if more than one episode per season
            if (nrow(filter(showRatings, season == i)) > 1) {
                pGraph <- pGraph %>% 
                    add_lines(
                        data = filter(showRatings, season == i),
                        y = ~fitted(loess(rating~episode, span = 9)),
                        x = ~episode,
                        color = ~season,
                        line = list(smoothing = 0.8,
                                    shape = "spline"),
                        text = ~paste("Season", season), hoverinfo = ~"text",
                        showlegend = FALSE,
                        legendgroup = i
                        
                    )
            }
        }
        pGraph
    })
    
    ## Show ratings plot ----
    output$ratingsPlot <- renderPlotly({
        
        print('ratingsPlot')
        
        chooseShow <- gsub("_shinymaterialdropdownspace_", " ", input$chooseShow)
        
        avRatings <- values$ratings %>% 
            filter(show == chooseShow) %>% 
            group_by(season) %>% 
            summarise(avRating = round(mean(rating), 1)) %>% 
            arrange(season)
        
        plot_ly(data = avRatings, x = ~avRating, y = ~season,
                type = "bar", orientation = "h",
                color = ~as.factor(season),
                hoverinfo = "none"
        ) %>% 
            layout(showlegend = FALSE,
                   bargap = 0.5,
                   xaxis = list(title = "", range = list(c(0), c(10)),
                                fixedrange = FALSE),
                   annotations = list(
                       x = ~avRating, 
                       y = ~season, 
                       text = ~avRating, 
                       showarrow = FALSE, 
                       bgcolor = "white", 
                       xanchor = "left"
                   ),
                   yaxis = list(title = "Season",
                                autorange = "reversed",
                                dtick = 1))
    })
    
    ## Update Banners
    observeEvent(input$updateBanners, {
        if (input$updateBanners > 0) {
            print('Updating banners')
            material_spinner_show(session, 'updateBanners')
            shiny::showNotification('Updating TV banners', type = 'default')
            # values$images <- getBanners(TRUE, slugs = unique(values$history$slug),
                                        # tvdb = unique(values$history$tvdb)) 
            shiny::showNotification('Done!', type = 'message')
            material_spinner_hide(session, 'updateBanners')
        }
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
