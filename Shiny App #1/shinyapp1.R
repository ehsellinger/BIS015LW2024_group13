library("ggplot2")
library("tidyverse")
library("janitor")
library("jpeg")
library("shiny")
library("shinydashboard")
library("leaflet")
library("plotly")
library("shinythemes")


conservatory <- read_csv("conservatory_data.csv") %>% clean_names()


conservatory_cleaned <- conservatory %>% 
  rename(region = native_region) %>% 
  filter(region != "NA") %>% 
  filter(region != "Multiple Countries") %>% 
  filter(region != "Unknown")

mapdata <- map_data("world") ##ggplot2

data <- data.frame(country = c('Algeria', 'Angola', 'Argentina', 'Australia', "Belize", "Bolivia", "Brazil", "Cameroon", "Cayman Islands", "Chile", "China", "Colombia", "Comoros", "Costa Rica", "Cuba", "Dominica", "Ecuador", "Equatorial Guinea", "Ethiopia", "Fiji", "France", "Greece", "Guam", "Guatemala", "Guinea", "Honduras", "India", "Indonesia", "Israel", "Italy", "Jamaica", "Japan", "Kenya", "Lesotho", "Madagascar", "Malawi", "Malyasia", "Mali", "Mexico", "Micronesia", "Morocco", "Mozambique", "Namibia", "Nepal", "New Caledonia", "New Zealand", "Nicaragua", "Nigeria", "Oman", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Portugal", "Puerto Rico", "Rwanda", "Saudi Arabia", "Solomon Islands", "Somalia", "South Africa", "Spain", "Sri Lanka", "Suriname", "Taiwan", "Tanzania", "Thailand", "Trinidad", "Tunisia", "Turkey", "Uganda", "United Kingdom", "United States", "Uruguay", "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe"),
                   longitude = c(-3, 15, -67, 124, -89, -69, -55, 8, -81, -75, 120, -70, 44, -83, -75, -61, -78, 11, 35, 177, 2, 23, 144, -89, -9, -88, 92, 118, 35, 16, -73, 129, 37, 27, 43, 33, 114,-9, -100, 158, -11, 35, 16, 80, 166, 169, -85, 5, 52, -79, 160, -54, -76, 118, -8, -65, 29, 50, 157, 42, 23, -2, 81, -56, 120, 35, 100, -61, 7, 29, 30, 0, -100, -57, -67, 106, 43, 23, 26),
                   latitude = c(23.98125, -8.624707, -35.16679, -25.34033, 16.80898, -14.377246, -3.330664039, 4.290235, 19.37476, -50.65420, 55.86113, -2.55253911, -12.35654, 8.480323, 21.11104, 15.63311, -3.776855230, 1.7401856, 5.384082, -18.25488, 42.36211, 39.15830, 13.61465, 14.05010, 7.509961, 13.98735, 28.025732, -8.464258, 30.86020, 41.92812, 17.94043, 34.68657, -1.32255840, -29.84024, -22.69189, -13.959180, 1.4552733, 12.46465, 19.30938, 6.854638, 26.16269, -11.53213, -28.26455, 28.64961, -20.69873, -43.458984, 11.18447, 5.338574, 18.69649, 8.639209, -2.830176, -26.30879, -13.80283260, 10.035009, 41.92710, 18.14438, -2.766406, 18.73525, -8.713477, 4.3610840, -28.56269, 36.74546, 7.684473, 2.036475, 22.97490, -13.5820312, 13.431983, 10.53897, 34.41030, 36.69345, 2.40043926, 51.48447, 43.32744, -33.85859, 5.4475098, 10.921973, 12.83901, -12.956934, -17.91172))

data <- data %>% 
  rename(region = country)

combined <- left_join(data, conservatory_cleaned, by="region")

data2 <- combined %>% 
  select(region, family, genus, latitude, longitude)

data3 <- data2 %>% 
  select(region, family, longitude, latitude) %>% 
  group_by(family, region, longitude, latitude) %>% 
  summarize(n_distinct(family))

plants <- read_csv("conservation_data_NY.csv") %>% clean_names() %>% 
  filter(category=="Plant")

split_names <- strsplit(plants$scientific_name, "\\s+")
genus <- sapply(split_names, "[", 1)
species <- sapply(split_names, "[", 2)

status <- plants$global_conservation_rank

scientific_data <- data.frame(species=species, status=status)

conservatory_bind <- conservatory_cleaned %>% 
  select(family, genus, species)

pretty_data <- left_join(scientific_data, conservatory_bind, by="species")

pretty_data <- pretty_data %>% 
  filter(!family=="NA" | !genus=="NA")

pretty_data <- pretty_data %>% 
  mutate(endangered_status = case_when(
    status == "G1" ~ "Critically Endangered",
    status == "G2" ~ "Endangered",
    status == "G3" ~ "Vulnerable",
    status == "G4" ~ "Uncommon",
    status == "G5" ~ "Secure",
    status == "G1G2" ~ "Between Critically Endangered and Vulnerable",
    status == "G1?" ~ "Probably Critically Endangered",
    status == "G2?" ~ "Probably Endangered",
    status == "G4G5" ~ "Between Uncommon and Secure",
    status == "G4G5Q" ~ "Between Uncommon and Secure",
    status == "G4G5T4" ~ "Species is Between Uncommon and Secure, Subspecies is Uncommon",
    status == "G4T4" ~ "Uncommon",
    status == "G5?" ~ "Probably Secure",
    status == "G5?TNR" ~ "Probably Secure",
    status == "G5T1" ~ "Species is Secure, Subspecies is Critically Endangered",
    status == "G5T3" ~ "Species is Secure, Subspecies is Vulnerable",
    status == "G5T3T4" ~ "Species is Secure, Subspecies is Between Uncommon and Vulnerable",
    status == "G5T3T5" ~ "Species is Secure, Subspecies is Between Secure and Vulnerable",
    status == "G5T4" ~ "Species is Secure, Subspecies is Uncommon",
    status == "G5T4?" ~ "Species is Secure, Subspecies is Probably Uncommon",
    status == "G5T4T5" ~ "Species is Secure, Subspecies is Between Secure and Uncommon",
    status == "G5T5" ~ "Secure",
    status == "G5T5" ~ "Species is Secure, Subspecies is Probably Secure",
    status == "G5TNR" ~ "Secure",
    status == "G5TN..." ~ "Secure",
    TRUE ~ NA_character_  # Keep other rows as NA
  ))

correct_data <- pretty_data %>% 
  select(family, genus, species, endangered_status)

more_data <- left_join(data3, correct_data, join_by(family))

more <- distinct(more_data) %>% 
  filter(!genus=="NA",
         !species=="NA",
         !endangered_status=="NA",
         !family=="NA",
         !latitude=="NA",
         !longitude=="NA")


ui <- fluidPage(
  theme = shinytheme("journal"),  # Use the 'journal' theme from shinythemes package
  titlePanel("UC Davis Conservatory Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x", "Select Plant Family", choices = unique(more$family)),
      uiOutput("genus_selector"),
    ),
    mainPanel(
      img(src = "conservation_data_globalmap.png", height = 450, width = 800),
      titlePanel("Plant Family Locations Around the World"),
      leafletOutput("myMap"),
      h3("Status of Each Plant Species Within the Selected Family and Region"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  
  output$myMap <- renderLeaflet({
    leaflet(data = more) %>%
      addTiles() %>%
      setView(lng = mean(more$longitude), lat = mean(more$latitude), zoom=2) %>%
      addMarkers(~longitude, ~latitude, group = "myMarkers")
  })
  
  output$genus_selector <- renderUI({
    req(input$x)
    genus <- unique(more$genus[more$family == input$x])
    selectInput("genus", "Select Genus", choices = c("All", genus),
                theme(
                  text = element_text(family = "mono", size = 20)
                )
    )
  })
  
  observeEvent(input$x, {
    filtered_data <- more[more$family == input$x, ]
    leafletProxy("myMap") %>%
      clearMarkers() %>%
      addMarkers(data = filtered_data,
                 ~longitude,
                 ~latitude,
                 group = "myMarkers")
  })
  
  output$plot <- renderPlot({
    req(input$genus)
    
    if (input$genus == "All") {
      filtered_data <- more[more$family == input$x, ]
    } else {
      filtered_data <- more[more$family == input$x & more$genus == input$genus, ]
    }
    
    ggplot(filtered_data, aes(x = species, fill = endangered_status)) +
      geom_bar(position="dodge") +
      labs(x = "Species",
           y = "Count",
           fill = "Endangered Status") +
      scale_fill_manual(values = c("Endangered" = "lightblue2", "Vulnerable" = "lightblue3", "Uncommon" = "lightblue4", "Secure" = "lightcyan", "Between Critically Endangered and Vulnerable" = "lightcyan2", "Probably Critically Endangered" = "lightcyan3", "Probably Endangered"= "lightcyan4", "Between Uncommon and Secure" = "lightgray", "Species is Between Uncommon and Secure, Subspecies is Uncommon" = "lightsteelblue3", "Probably Secure" = "lightsteelblue4", "Species is Secure, Subspecies is Critically Endangered" = "mediumpurple1", "Species is Secure, Subspecies is Vulnerable" = "mediumslateblue", "Species is Secure, Subspecies is Between Uncommon and Vulnerable" = "mediumseagreen", "Species is Secure, Subspecies is Between Secure and Vulnerable" = "mediumaquamarine", "Species is Secure, Subspecies is Uncommon" = "mediumturquoise", "Species is Secure, Subspecies is Probably Uncommon" = "mintcream", "Species is Secure, Subspecies is Between Secure and Uncommon" = "mistyrose", "Species is Secure, Subspecies is Probably Secure"= "maroon")) +  # Set colors for each level
      facet_wrap(~ genus) +  # Facet by genus
      theme(
        text = element_text(family = "mono", size = 20),  # Adjust the size of the labels
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")
      )
    
  })
}


shinyApp(ui, server)