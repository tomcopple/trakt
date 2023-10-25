suppressPackageStartupMessages({
    library(shiny)
    library(plotly)
    library(shinymaterial)
    library(tidyverse)
    library(lubridate)
    library(jsonlite)
    library(httr2)
    library(httr)
    library(RColorBrewer)
    library(zoo)
    library(forcats)
})

# Check if running in Shiny or locally, need to check WorkDir
if (!shiny::isRunning()) {
    if(str_detect(getwd(), 'Shiny', negate = T)) setwd('traktShiny')
    options(shiny.autoreload = TRUE)
} 
getwd()

# Dropbox Authenticatio ---------------------------------------------------

dropboxClient <- oauth_client(
    id = Sys.getenv('DROPBOX_KEY'),
    secret = Sys.getenv('DROPBOX_SECRET'),
    token_url = "https://api.dropboxapi.com/oauth2/token",
    name = 'Rstudio_TC'
)

# dropboxToken <- oauth_flow_auth_code(
#     dropboxClient, port = 43451,
#     auth_url = "https://www.dropbox.com/oauth2/authorize?token_access_type=offline"
# )
# saveRDS(dropboxToken, 'dropbox.RDS')
print('Getting dropbox token')
dropboxToken <- readRDS('dropbox.RDS')
exists(x = 'dropboxToken')

print("Getting trakt token")
# Environmental variables -------------------------------------------------
# trakt_id <- Sys.getenv('TRAKTSHINY_ID')
# print(str_c('trakt_id: ', trakt_id))
# trakt_secret <- Sys.getenv('TRAKTSHINY_SECRET')
# print(str_c('trakt_secret: ', trakt_secret))
# traktUser <- Sys.getenv("TRAKT_USER")
# print(str_c('traktUser: ', traktUser))
# traktApi <- Sys.getenv('TRAKT_API')
# print(str_c('traktApi: ', traktApi))

### Get a token, don't need to do this every time?
# traktClient <- oauth_client(
#     id = Sys.getenv('TRAKTSHINY_ID'),
#     secret = Sys.getenv('TRAKTSHINY_SECRET'),
#     token_url = "https://api.trakt.tv/oauth/token",
#     name = 'traktShiny'
# )
# 
# traktToken <- oauth_flow_auth_code(
#     traktClient, port = 43451,
#     auth_url = "https://trakt.tv/oauth/authorize"
# )

# saveRDS(traktToken, 'traktToken.RDS')
traktToken <- readRDS('traktToken.RDS')
exists('traktToken')

source("getMyRatings.R")
source('getTraktHistory.R')
source('getBanners.R')

accessCode <- traktToken$access_token

print('Getting ratings')
ratings <- getMyRatings(refresh = F, accessCode)
print('Got ratings, getting history')
history <- getTraktHistory(refresh = F, accessCode)
print('Got history, getting banners')
images <- getBanners(refresh = F)
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
                ## Trying to only show this button if the chart shows ratings
                conditionalPanel(
                    condition = 'input.mainInput == false',
                    material_switch(input_id = 'showLines', off_label = 'Show lines', initial_value = FALSE)
                ),
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
            values$ratings <- getMyRatings(refresh = TRUE, accessCode)
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
        
        print('plotlyTime')
        ## If mainInput then showing plays
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
                # plot_ly(values$ratingsChartValues,
                #         x = ~date, color = ~show, type = 'scatter',
                #         mode = 'markers', showlegend = FALSE) %>% 
                #     add_trace(y = ~rating, alpha = 0.4, legendgroup = ~str_glue("{show} s{season}"),
                #               text = ~str_glue(
                #                   "{show} 
                #                   s{str_pad(season, width = 2, side = 'left', pad = '0')} 
                #                   e{str_pad(episode, width = 2, side = 'left', pad = '0')} 
                #                   {title}: {rating}"
                #               ),
                #               hoverinfo = 'text') %>% 
                #     add_lines(y = ~rating, legendgroup = ~str_glue("{show} s{season}"),
                #               showlegend = TRUE,
                #               line = list(shape = 'spline', smoothing = 1.3)) %>% 
                #     layout(xaxis = list(title = ""),
                #            showlegend = TRUE,
                #            yaxis = list(title = 'Ratings'),
                #            title = str_glue("Episode ratings {chooseYear}"),
                #            legend = list(x = 100, y = 1)
                #     )
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
    
    ### Top 10 plays barchart ----
    output$plotlyBar <- renderPlotly({
        
        print('plotlyBar')
        
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
        
        print('plotlyRat')
        
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
                values$images <- getBanners(TRUE, slugs = unique(values$history$slug),
                                            tvdb = unique(values$history$tvdb)) 
                shiny::showNotification('Done!', type = 'message')
                material_spinner_hide(session, 'updateBanners')
            }
        })
}

# Run the application 
shinyApp(ui = ui, server = server)

