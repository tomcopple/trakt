# Function to filter ratings for specific show and draw a graph of ratings by season. 

CompareSeasons <- function(show, env = .GlobalEnv) {
    
    library(tidyverse);library(plotly)
    
    # Filter ratings for show
    if(!exists("ratings")) {return("Need to run 'getTraktRatings.R first'")}
    
    # Using "toupper" to allow for lower case search
    if(!(toupper(show) %in% toupper(ratings$title))) {
        return("Show not found, check spelling?")
        }
    
    rep1 <- filter(ratings, toupper(title) == toupper(show))
    
    # Archer has some weird season/episode numbering, needs fixing. 
    if(toupper(show) == "ARCHER") {
        rep1[rep1$seasonnum == 3,] <- mutate(filter(rep1, seasonnum == 3), 
                                         episodenum = episodenum + 3)
        rep1[rep1$seasonnum == 0,] <- mutate(filter(rep1, seasonnum == 0), 
                                         episodenum = episodenum - 3, 
                                         seasonnum = 3)
    }
    
    # Also print a table of average ratings per season and return to global environment. 
    table <- rep1 %>% 
        select(Season = seasonnum, Episode = episodenum, 
               Title = episode, Rating = rating) %>% 
        arrange(desc(Season), desc(Episode))
    print(table)
    env$table <- table
    
    # Draw graph of ratings by season
    graph <- ggplot(
        rep1 %>% 
            rename(Season = seasonnum, Episode = episodenum,
                   Title = episode, Show = title, Rating = rating) %>% 
            mutate(Season = factor(Season), Rating = factor(Rating),
                   Episode = as.integer(Episode)), 
        aes(
            x = Episode,
            y = Rating,
            color = Season,
            group = Season,
            text = Title
        )
    ) + 
        geom_smooth(se = FALSE,
                    span = 8) +
        # Putting in manual     limits to x axis to make sure it starts at 1.
        # Makes it more obvious if some ratings are missing. 
        scale_x_continuous(limits = c(1, max(rep1$episodenum)),
                           breaks = seq(1, max(rep1$episodenum))) +
        scale_color_brewer(palette = "Set2") +
        geom_point() +
        labs(x = "Episode", y = "Rating", color = "Season",
             title = paste("Ratings for", show)) +
        theme(text = element_text(size = 14, color = "#393939"),
              axis.text.y = element_text(margin = margin(l = 6)),
              axis.text.x = element_text(margin = margin(b = 6)))

    print(group_by(table, Season) %>% summarise(avrating = mean(Rating)))
    suppressWarnings(ggplotly(graph,
                              tooltip = c("text", "x", "y", "group")))
}
