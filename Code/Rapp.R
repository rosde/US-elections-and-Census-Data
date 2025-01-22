
library(shiny)
library(tidyverse)
library(ggthemes)
library(dplyr)
library(readr)
library(tidycensus)
library(shinythemes)
library(USAboundaries)
library(gganimate)
library(scales)
library(sf)
library(gifski)


#Load Dataset
election_data_pre <- read_csv('../data/1976-2020-president.csv')
election_data <-  read.csv('../data/election_data.csv')
states_names <- read.csv('../data/state_names.csv')
colors <- read.csv('../data/colors.csv')
election_data2 <-  read.csv('../data/election_data2.csv')
data2020 <- read.csv('../data/data2020.csv')
# elec
election2020 <- read.csv('../data/election2020_b.csv')
election2020_model <- read.csv('../data/election2020_model_a.csv')

three_party <- election_data_pre %>% 
  mutate(new_party = (ifelse(party_simplified == "DEMOCRAT", "Democrat", 
                             ifelse(party_simplified == "REPUBLICAN", "Republican", "ThirdParty"))),
         state = tolower(state)) %>% 
  group_by(year, state, new_party) %>% 
  summarize(party_total = sum(candidatevotes/totalvotes))

two_party <- election_data_pre %>% 
  mutate(new_party = (ifelse(party_simplified == "DEMOCRAT", "Democrat", 
                             ifelse(party_simplified == "REPUBLICAN", "Republican", "ThirdParty"))),
         state = tolower(state)) %>% 
  filter(new_party != "ThirdParty") %>% 
  group_by(year, state) %>% 
  summarize(compare_total = candidatevotes / sum(candidatevotes),
            new_party = new_party)
  

state_names <- three_party %>% pull(state) %>% unique()


colors <- c("#0000FF", "#FF0000", "#808080")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput("stateSelect",
                  "Choose a State",
                  state_names)
    ),
  
  
  mainPanel(
    plotOutput("us_map"),
    plotOutput("state_scatter")
  )

))

server <- function(input, output) {
  
  output$us_map <- renderImage({
    
    animated_map <- tempfile(fileext = '.gif')
    
    map_data <- st_as_sf(maps::map("state", region = input$stateSelect, fill = TRUE, plot = TRUE)) %>% 
      rename(state = ID)
    
    map_colors <- two_party %>% 
      filter(new_party == "Democrat") %>% 
      mutate(state = tolower(state))
    
    map_colors <- left_join(map_data, map_colors, by = 'state') %>% 
      filter(new_party == "Democrat")
    
    map_with_initial_data <- ggplot(data = map_colors) +
      geom_sf(color = "black", aes(fill = compare_total)) +
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

   output$state_scatter <- renderImage({
     
     animated_plot <- tempfile(fileext = '.gif')

     plot_data_state <- three_party %>%
       filter(state == input$stateSelect)

     static_line <- ggplot(data = plot_data_state, aes(x = year, y = party_total, color = new_party)) +
       geom_point() +
       geom_line() +
       scale_color_manual(values = colors) +
       labs(title = str_glue("Voting Trends in ", input$stateSelect),
            x = " ",
            y = "Vote Percentage") +
       theme_clean()
     
     animated_plot <- static_line +
       transition_reveal(year)
     
     anim_save("animated_plot.gif", animate(animated_plot, renderer = gifski_renderer(), end_pause = 30, duration = 11))
     
     list(src = "animated_plot.gif",
          contentType = 'image/gif',
          width = 600,
          height = 400)
     
   }, deleteFile = TRUE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
