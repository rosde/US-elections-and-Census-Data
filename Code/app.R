
#Libraries
library(shiny)
library(tidyverse)
library(ggthemes)
library(dplyr)
library(readr)
library(tidycensus)
library(shinythemes)
library(cowplot)
library(sf)
library(USAboundaries)
library(gganimate)
library(gifski)
library(stringr)

#Load Dataset
election_data_pre <- read_csv('../data/1976-2020-president.csv')
election2020 <- read.csv('../data/election2020_b.csv')
election2020_model <- read.csv('../data/election2020_model_a.csv')

load("bCleaning.rda") 

predYears <- c("2000", "2004", "2008", "2012", "2016", "2020")

three_party <- election_data_pre %>% #data set that has vote totals for Democrats, Republicans, and a sum of third party votes
  mutate(new_party = (ifelse(party_simplified == "DEMOCRAT", "Democrat", 
                             ifelse(party_simplified == "REPUBLICAN", "Republican", "ThirdParty"))),
         state = tolower(state)) %>% 
  group_by(year, state, new_party) %>% 
  summarize(party_total = sum(candidatevotes/totalvotes))

two_party <- election_data_pre %>% #data set with only vote totals for Democrats and Republicans to use for comparison in choropleth map
  mutate(new_party = (ifelse(party_simplified == "DEMOCRAT", "Democrat", 
                             ifelse(party_simplified == "REPUBLICAN", "Republican", "ThirdParty"))),
         state = tolower(state)) %>% 
  filter(new_party != "ThirdParty") %>% 
  group_by(year, state) %>% 
  summarize(compare_total = candidatevotes / sum(candidatevotes),
            new_party = new_party)


state_names <- three_party %>% pull(state) %>% unique()
colors <- c("#0000FF", "#FF0000", "#808080")

ui <- fluidPage(
  
  
  # Application title
  titlePanel("Exploring US Elections: Understanding Voting Trends and Demographic Patterns"),
  
  # Apply theme
  theme = shinytheme("readable"),
  
  # Main tabset panel
  tabsetPanel(
    
    # First tab with HTML content
    tabPanel("Overview", 
             includeHTML("blog_third_tab.html")
    ),
    
    # TBD tab
    tabPanel("Voting trends",
             sidebarLayout(
               sidebarPanel(
                 selectInput("stateSelect",
                             "Choose a State",
                             str_to_title(state_names))
               ),
               
               mainPanel(
                 plotOutput("us_map"),
                 plotOutput("state_scatter")
               )
               
             )
    ),
    
    # Third tab
    tabPanel("Predictions",
             # UI for the third tab
             sidebarLayout(
               sidebarPanel(
                 selectInput("yearSelect",
                             "Choose a Year",
                             predYears)
               ),
               mainPanel(
                 # Arrange plots side by side
                 fluidRow(
                   column(6, plotOutput("real_map")),
                   column(6, plotOutput("winner_map"))
                 ),
                 # Plot below the side-by-side plots
                 plotOutput("different_plot")
               )
             )
    )
    
  )  # End of tabsetPanel
  
)  # End of fluidPage




server <- function(input, output) {
  
  
  #Third tab 
  
  output$real_map <- renderPlot({
    
    plot_data <- predictedResults[[input$yearSelect]]
    
    electionResults <- plot_data %>% 
      st_drop_geometry() %>% 
      group_by(realWinner) %>% 
      summarize(votes = sum(as.numeric(electoralVotes))) %>% 
      arrange(realWinner)
    
    electionResults[1,2] <- electionResults[1,2] + 3
    
    a <- plot_data %>% 
      ggplot() +
      geom_sf(aes(fill = realWinner), color = 'black') +
      scale_fill_manual(values = c("blue", "red"), name = "Election Winner") +
      coord_sf(xlim = c(-125, -65), ylim = c(20, 50)) + 
      ggtitle("Actual Election Results") + 
      theme_classic() +
      theme(axis.line.x= element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            plot.title = element_text(face = "bold")) 
    
    b <- electionResults %>% 
      ggplot(aes(x = "", y = votes, fill = realWinner)) + 
      geom_bar(stat = "identity", width = 1) +
      geom_text(data = electionResults[1, ], aes(x = "", y = 438, label = votes), fontface = "bold", size = 6) +
      geom_text(data = electionResults[2, ], aes(x = "", y= 100, label = votes), fontface = "bold", size = 6) +
      coord_flip() +
      scale_fill_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
      theme_classic() +
      theme(axis.title = element_blank(), 
            axis.text = element_blank(),  
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "none") 
    
    plot_grid(a,b, 
              ncol = 1, rel_heights = c(1, 0.12))
  })
  
  output$winner_map <- renderPlot({
    plot_data <-  predictedResults[[input$yearSelect]]
    
    electionResults <- plot_data %>% 
      st_drop_geometry() %>% 
      group_by(predWinner) %>% 
      summarize(votes = sum(as.numeric(electoralVotes))) %>% 
      arrange(predWinner)
    
    electionResults[1,2] <- electionResults[1,2] + 3
    
    a <- plot_data %>% 
      ggplot() +
      geom_sf(aes(fill = predWinner), color = 'black') +
      scale_fill_manual(values = c("blue", "red"), name = "Election Winner") +
      coord_sf(xlim = c(-125, -65), ylim = c(20, 50)) + 
      ggtitle("Predicted Election Results Based on Demographic Model") + 
      theme_classic() +
      theme(axis.line.x= element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            plot.title = element_text(face = "bold"))
    
    b <- electionResults %>% 
      ggplot(aes(x = "", y = votes, fill = predWinner)) + 
      geom_bar(stat = "identity", width = 1) +
      geom_text(data = electionResults[1, ], aes(x = "", y = 438, label = votes), fontface = "bold", size = 6) +
      geom_text(data = electionResults[2, ], aes(x = "", y= 100, label = votes), fontface = "bold", size = 6) +
      coord_flip() +
      scale_fill_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
      theme_classic() +
      theme(axis.title = element_blank(), 
            axis.text = element_blank(),  
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "none") 
    
    plot_grid(a,b, 
              ncol = 1, rel_heights = c(1, 0.12))
  })
  
  output$different_plot <- renderPlot ({
    plot_data <-  predictedResults[[input$yearSelect]]
    
    plot_data %>% 
      mutate(demDif = demPred-Democrat) %>% 
      mutate(difParty = ifelse(demDif > 0, "Democrat", "Republican")) %>% 
      ggplot(aes(x=NAME, y=demDif, fill = difParty)) + 
      geom_hline(yintercept = 0, color = "black") + 
      scale_x_discrete(guide = guide_axis(angle = 50)) +
      geom_col() + 
      scale_fill_manual(values = c("blue", "red")) + 
      ylab("Difference in Predicted vs. Actual Results") + 
      xlab("State") +
      theme_classic() + 
      theme(legend.position = "none")
  })
  
  #second tab 
  
  #choropleth map
  output$us_map <- renderImage({
    
    animated_map <- tempfile(fileext = '.gif')
    
    map_data <- st_as_sf(maps::map("state", region = input$stateSelect, fill = TRUE, plot = TRUE)) %>% 
      rename(state = ID)
      
    map_colors <- two_party %>% 
      filter(new_party == "Democrat")

    map_colors <- left_join(map_data, map_colors, by = 'state') %>% 
      filter(new_party == "Democrat")
    
    map_with_initial_data <- ggplot(data = map_colors) +
      geom_sf(color = "black", aes(fill = compare_total)) +
      labs(fill = "Democratic Vote Share") +
      scale_fill_stepsn(colors = c("#FF0000", "#D52B2B", "#AA5555", "#808080", "#5555AA", "#2B2BD5", "#0000FF"),
                        breaks = seq(0.325, 0.675, by = 0.05),
                        limits = c(0.325, 0.675))
    
    animation_map <- map_with_initial_data +
      transition_time(year) +
      ggtitle('Year: {4*floor(frame_time/4)}') + 
      theme(plot.title = element_text(size = 40))
    
    anim_save("animated_map.gif", animate(animation_map, renderer = gifski_renderer(), end_pause = 30, duration = 11))
    
    list(src = "animated_map.gif",
         contentType = 'image/gif',
         width = 400,
         height = 400)
    
    
    
    
  }, deleteFile = TRUE)
  
  #Animated scatter plot
  output$state_scatter <- renderImage({
    
    animated_plot <- tempfile(fileext = '.gif')
    
    plot_data_state <- three_party %>%
      filter(state == tolower(input$stateSelect))
    
    static_line <- ggplot(data = plot_data_state, aes(x = year, y = party_total, color = new_party)) +
      geom_point() +
      geom_line() +
      scale_color_manual(values = colors) +
      labs(title = str_glue("Voting Trends in ", str_to_title(input$stateSelect)),
           x = " ",
           y = "Vote Percentage") +
      theme_clean()
    
    animated_plot <- static_line +
      transition_reveal(year)
    
    anim_save("animated_plot.gif", animate(animated_plot, renderer = gifski_renderer(), end_pause = 30, duration = 11))
    
    list(src = "animated_plot.gif",
         contentType = 'image/gif',
         width = 500,
         height = 500)
    
  }, deleteFile = TRUE)
  
}
# Run the application 
shinyApp(ui = ui, server = server)
