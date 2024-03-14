
conservatory <- read_csv("conservatory_modified.csv") %>% clean_names()
conservation_status <- read_csv("filtered_data2.csv") %>% clean_names()
merged_data <- merge(conservatory_cleaned2, conservation_status, by = "scientific_name")


color_palette <- c("CR" = "red", "EN" = "orange", "VU" = "yellow", "NT" = "chartreuse", "LC" = "darkgreen", "DD" = "grey")

ui <- fluidPage(
  titlePanel("IUCN Categories for Selected Family"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("family", "Select Family", choices = unique(merged_data$family))
    ),
    mainPanel(
      plotOutput("plot", width = "600px", height = "500px")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    family_data <- merged_data %>%
      filter(family == input$family) %>%
      group_by(iucn_category) %>%
      summarise(count = n())
    
    # Create the bar plot with specified colors and legend
    ggplot(family_data, aes(x = iucn_category, y = count, fill = iucn_category)) +
      geom_bar(stat = "identity", show.legend = TRUE) +
      scale_fill_manual(values = color_palette, name = "IUCN Category") +  # Add legend title
      labs(title = paste("IUCN Categories for", input$family),
           x = "Category",
           y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the application
shinyApp(ui, server)
