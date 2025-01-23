library(shiny)
library(tidyverse)
library(sf)
library(cowplot)

load("brianCleaning.rda") 

predYears <- c("2000", "2004", "2008", "2012", "2016", "2020")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput("yearSelect",
                  "Choose a Year",
                  predYears)
    ),
    
    
    mainPanel(
      plotOutput("real_map"),
      plotOutput("winner_map"),
      plotOutput("different_plot")
    )
    
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
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
              ncol = 1, rel_heights = c(1, 0.15), rel_widths = c(1, 0.5))
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
              ncol = 1, rel_heights = c(1, 0.15), rel_widths = c(1, 0.5))
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
}

# Run the application 
shinyApp(ui = ui, server = server)
