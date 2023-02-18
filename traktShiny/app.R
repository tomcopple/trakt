library(shiny);library(plotly);library(shinymaterial)
library(tidyverse);library(lubridate);library(jsonlite)
library(httr);library(RColorBrewer);library(magrittr)
library(zoo);library(forcats);library(rdrop2)

# dropbox <- rdrop2::drop_auth()
# saveRDS(dropbox, 'dropbox.rds')
print('Getting Dropbox')
dropbox <- readRDS('dropbox.rds')
print('Got Dropbox')

# options(shiny.autoreload = TRUE)

readRenviron(".Renviron")
# options(shiny.trace = TRUE)
# Slightly different code for working locally; comment out before publishing to shiny
if(str_detect(getwd(), 'Shiny', negate = T)) setwd('traktShiny')

getwd()


print("Authenticating Trakt")
# Environmental variables -------------------------------------------------
trakt_id <- Sys.getenv('TRAKTSHINY_ID')
print(str_c('trakt_id: ', trakt_id))
trakt_secret <- Sys.getenv('TRAKTSHINY_SECRET')
print(str_c('trakt_secret: ', trakt_secret))
traktUser <- Sys.getenv("TRAKT_USER")
print(str_c('traktUser: ', traktUser))
traktApi <- Sys.getenv('TRAKT_API')
print(str_c('traktApi: ', traktApi))

### Get a token, don't need to do this every time?
app <- oauth_app(
    appname = "traktShiny",
    key = trakt_id,
    secret = trakt_secret,
    redirect_uri = "urn:ietf:wg:oauth:2.0:oob"
)

endpoint <- oauth_endpoint(authorize = "https://trakt.tv/oauth/authorize",
                           access = "https://api.trakt.tv/oauth/token")


token <- oauth2.0_token(endpoint = endpoint,
                        app = app,
                        use_oob = TRUE)

token$refresh()

accessCode <- token$credentials$access_token
print(str_c('Access code: ', accessCode))


source("getMyRatings.R")
source('getTraktHistory.R')
source('getBanners.R')

print('Getting ratings')
ratings <- getMyRatings(accessCode)
print('Got ratings, getting history')
history <- getTraktHistory(refresh = TRUE, accessCode)
print('Got history, getting banners')
images <- getBanners(refresh = T)
print('Got banners')

showList <- history %>% 
    distinct(show, title) %>%
    count(show) %>% 
    filter(n > 3) %>% 
    arrange(show) %>% 
    pull('show')


# UI ----------------------------------------------------------------------

ui <- material_page(
    
    # Application title
    title = "Trakt Dashboard",
    nav_bar_color = 'DEEP_ORANGE',
    
    material_tabs(
        tabs = c(
            "Main page" = "main_page",
            "Show page" = "show_page",
            "Maintenance" = "maintenance"
        )
    ),
    
    ### Main Page UI ----
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
                        label = "Select year:",
                        color = "green",
                        choices = c("All time", "Last 30 days",
                                    seq.int(from = year(max(history$date)), 
                                                        to = year(min(history$date))))
                    )
                )
            ),
            material_column(
                width = 9, 
                material_switch(input_id = 'mainInput', off_label = 'Show ratings', on_label = 'Show plays', initial_value = TRUE),
                material_card(title = "", plotlyOutput("plotlyTime", height = "100%")),
                material_card(title = "", 
                              material_slider(
                                  input_id = 'sliderBar', label = "", min_value = 1, max_value = 10, step_size = 1, initial_value = 1
                              ),
                              plotlyOutput("plotlyBar", height = "100%")),
                material_card(title = '', 
                              material_slider(
                                  input_id = 'sliderRat', label = "", min_value = 1, max_value = 10, step_size = 1, initial_value = 1
                              ),
                              plotlyOutput('plotlyRat',height = "100%"))
            )
        )
    ),
    
    ### Show Page UI ----
    material_tab_content(
        tab_id = "show_page",
        material_row(
            material_column(
                width = 3,
                material_card(
                    title = NULL,
                    material_dropdown(
                        input_id = "chooseShow",
                        label = "Choose show",
                        color = "DEEP_ORANGE",
                        choices = showList,
                        selected = first(history$show)
                    )
                ),
                material_card(
                    title = NULL,
                    material_button(
                        input_id = "refresh",
                        label = "Refresh data",
                        icon = "refresh"
                    )
                )
            ),
            material_column(
                width = 9,
                material_row(
                    uiOutput('bannerImage')
                ),
                material_row(
                    material_card(
                        title = "", plotlyOutput("showPlot", height = "100%")
                    )
                ),
                material_row(
                    material_card(title = "", plotlyOutput("ratingsPlot", height = "100%"))
                )
            )
        )
    ),
    
    ### Maintenance Page UI ----
    material_tab_content(
        tab_id = "maintenance",
        material_card(
            material_button('updateBanners', label = "Update show banners", icon = "refresh")
        )
    )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
    
    values <- reactiveValues()
    values$ratings <- ratings
    values$history <- history
    values$images <- images
    
    observeEvent(input$refresh, {
        if (input$refresh > 0) {
            material_spinner_show(session, 'showPlot')
            material_spinner_show(session, 'ratingsPlot')
            shiny::showNotification("Refreshing data, may take some time...", type = "default")
            values$ratings <- getMyRatings(accessCode)
            values$history <- getTraktHistory(refresh = TRUE, accessCode)
            shiny::showNotification("Done!", type = "message")
            material_spinner_hide(session, "showPlot")
            material_spinner_hide(session, "ratingsPlot")
        }
    })
    
    ### Create filtered data.frame by year ----
    observeEvent(input$chooseYear, {
        
        chooseYear <- input$chooseYear
        print(str_c("Selecting ", chooseYear))
        
        if (chooseYear == 'All time') {
            values$filtered <- values$history
            values$minDate <- as_date(min(values$filtered$date))
            values$maxDate <- as_date(max(values$filtered$date))
            
        } else if (chooseYear == 'Last 30 days') {
            values$minDate <- today() - days(30)
            values$maxDate <- today()
            values$filtered <- filter(values$history, date >= values$minDate)
        } else {
            values$filtered <- filter(values$history, year(date) == chooseYear)
            values$minDate <- dmy(str_c("01-01-", chooseYear))
            values$maxDate <- dmy(str_c("31-12-", chooseYear))
        }
        
        ## Get top ten shows for the time period
        values$top10 <- values$filtered %>% count(show, sort = T) %>% head(10) %>% pull('show')
        values$top10data <- values$filtered %>% 
            filter(show %in% values$top10) %>% 
            mutate(date = as_date(date)) %>% 
            select(date, show)
        print(values$top10)
    })
    
    ### plotlyTIme: Plotly time series area chart  ----
    output$plotlyTime <- renderPlotly({
        
        if (input$mainInput) {
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
                mutate(show = forcats::fct_relevel(show, levels = values$top10)) %>% 
                ## Something weird happening with old episode of Daily show, delete 
                ## anything that says oveer 60 plays in 1 month
                mutate(plays = ifelse(plays >= 60, 60, plays)) 
            
            ## Plot plotly graph
            top10plays %>% 
                group_by(show) %>% 
                plot_ly(x = ~date, y = ~plays, color = ~show, type = 'scatter', mode = 'lines',
                        fill = 'tozeroy', colors = 'Spectral', 
                        text = ~str_c(show, '<br>Plays: ', plays), hoverinfo = 'text') %>% 
                layout(xaxis = list(title = ""),
                       yaxis = list(title = 'Monthly plays'),
                       title = str_c('Top 10 shows in ', input$chooseYear),
                       legend = list(orientation = 'h', xanchor = 'left', x = 0,
                                     yanchor = 'top', y = -0.05))    
        } else {
            chooseYear <- input$chooseYear
            values$filtered %>% 
                # inner_join(values$ratings, by = c('show', 'season', 'episode')) %>% 
                inner_join(ratings, by = c('show', 'season', 'episode')) %>% 
                group_by(show) %>% 
                filter(rating > 0, !is.na(rating)) %>% 
                filter(n() >= 3) %>% 
                plot_ly(x = ~date.x, y = ~rating, color = ~show, type = 'scatter',
                        colors = 'Spectral', 
                        text = ~str_c(show, 
                                      ' s', str_pad(season, width = 2, side = 'left', pad = "0"),
                                      'e', str_pad(episode, width = 2, side = 'left', pad = "0"),
                                      " ", title.x, ": ", rating),
                        hoverinfo = 'text') %>% 
                layout(xaxis = list(title = ""),
                       showlegend = FALSE,
                       yaxis = list(title = 'Ratings'),
                       title = str_c("Episode Ratings for ", chooseYear),
                       legend = list(orientation = 'h', xanchor = 'left', 
                                     x = 0, yanchor = 'top', y = -0.05))
        }
        
        
                      
                      
    })
    
    ### Top 10 plays barchart ----
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
    
    
    #### Main page ratings plot ---------
    output$plotlyRat <- renderPlotly({
        
        minSlice <- input$sliderRat * 10 - 9
        maxSlice <- input$sliderRat * 10
        
        ## Filter data for shows with more than 3 ratings then show top 10
        topRat <- values$filtered %>% 
            inner_join(values$ratings, by = c('show', 'season', 'episode')) %>% 
            group_by(show) %>% 
            filter(rating > 0, !is.na(rating)) %>% 
            filter(n() >= 3) %>% 
            summarise(avRat = mean(rating), n = n()) %>% 
            ungroup() %>% 
            arrange(desc(avRat)) %>% 
            slice(c(minSlice:maxSlice)) %>% 
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
    

# Show page charts --------------------------------------------------------

    
    output$bannerImage <- renderUI({
        
        # Weird extra text from shinymaterial, probably a bug. 
        chooseShow <- gsub("_shinymaterialdropdownspace_", " ", input$chooseShow)
        
        # Send img html tag for banner image, based on selected show
        # Some images don't seem to work...
        print(str_c("Getting banner image for ", chooseShow))
        showSlug <- values$ratings %>% 
            filter(show == chooseShow) %>% 
            distinct(slug) %>% 
            pull(slug)
        print(showSlug)
        imgSrc <- values$images %>% filter(show == showSlug) %>% pull(imageLink)
        print(imgSrc)
        tags$img(src = imgSrc,
                 width = "100%")
        
    })
        
    ### Show ratings plot ---- 
    
        output$showPlot <- renderPlotly({
            
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
        
        output$ratingsPlot <- renderPlotly({
            
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
                values$images <- getBanners(TRUE, slugs = unique(values$history$slug),
                                            tvdb = unique(values$history$tvdb)) 
                shiny::showNotification('Done!', type = 'message')
                material_spinner_hide(session, 'updateBanners')
            }
        })
}

# Run the application 
shinyApp(ui = ui, server = server)

