library(ggplot2)
library(tidyverse)
library(janitor)
library(jpeg)
library(shiny)
library(shinydashboard)
library(naniar)
library(shinythemes)
library(RColorBrewer)
library(paletteer)
library(ggthemes)
library(dplyr)
library(viridis)

conservatory <- read_csv("conservatory_data.csv") %>% clean_names()
conservation_status <- read_csv("conservation_data_NY.csv")

conservatory_cleaned <- conservatory %>% 
  mutate(region = native_region) %>% 
  select("accession_num", "region", "family") %>% 
  filter(region != "NA") %>% 
  filter(region != "Multiple Countries") %>% 
  filter(region != "Unknown")

conservatory_cleaned1 <- conservatory_cleaned %>% 
  group_by(region) %>% 
  summarise(n_samples = n()) %>% 
  mutate(n_samples = n_samples)

mapdata <- map_data("world")

mapdata <- left_join(mapdata, conservatory_cleaned1, by="region")

conservatory %>% 
  summarise(n_families <- n_distinct(family))

conservatory_families <- conservatory %>% 
  mutate("region" = native_region) %>% 
  filter(region != "NA") %>% 
  filter(region != "Multiple Countries") %>% 
  filter(region != "Unknown") %>% 
  filter(family != "Unknown")

ui <- fluidPage(    
  
  theme = shinytheme("cerulean"),
  
  titlePanel("UC Davis Conservatory Data"), # give the page a title
  
  # generate a row with a sidebar
  sidebarLayout(      
    
    # define the sidebar with one input
    sidebarPanel(
      selectInput("x", "Select Region", choices = unique(conservatory_families$region), hr())
    ),  
    
    box(width = 5,
        plotOutput("plot", width = "800px", height = "700px")
    ) 
  ) 
) 

server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp) #automatically stop the app when we close it
  
  output$plot <- renderPlot({
    
    conservatory_families %>% 
      filter(region == input$x) %>% 
      count(family) %>% 
      ggplot(aes(x = reorder(family, n), y = n, fill = family)) +
      geom_col() +
      labs(x = "Family",
           y = "Count") +
      scale_fill_viridis(option = "mako", discrete=TRUE) +
      guides(fill = FALSE) +
      theme(text = element_text(family = "mono")) + 
      coord_flip() +
      theme(axis.title.y = element_text(margin = margin(r = 10))) +
      theme(text = element_text(size = 12),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"))
    
  })
}

shinyApp(ui, server)

