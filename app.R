#######################################
# Shiny App Week 2
# 
# Car Accident data - Midwest Focus
#
# Jakob Ziegler
#######################################

# Import necessary libraries
library(shiny)
library(shinythemes)
library(ggplot2)
library(ggiraph)
library(mapdata)
library(dplyr)
library(readr)
library(stringr)
library(maps)
library(shinydashboard)
library(shinyalert)

#Load accident data and prepare it
accident <- read_csv("accident.csv") 
population <- read_csv("census.csv")

# Filter data for Midwest states
midwest_states <- c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri", "Nebraska", 
                    "North Dakota", "Ohio", "South Dakota", "Wisconsin")

# Group data by state and county
accidents_midwest <- accident %>%
  filter(STATENAME %in% midwest_states) %>%
  group_by(STATENAME, COUNTYNAME) %>%
  summarize(total_fatalities = sum(FATALS), .groups = 'drop')

#grab only the populations of Midwestern states for normalization
population_midwest <- population %>%
  filter(NAME %in% midwest_states) %>%
  rename(POPULATION = POPESTIMATE2019) %>% # Ensure population column is named consistently
  rename(STATENAME = NAME) %>%             # Ensure STATENAME is correctly named consistently
  select(STATENAME, POPULATION)

#Merge onto the dataset
accidents_midwest <- accidents_midwest %>%
  left_join(population_midwest, by = c("STATENAME"))


####################################################
#The following code will declare the UI of the 
#application that will be displayed to the user for
#them to choose to do with as they please
####################################################


# UI
ui <- dashboardPage(
  dashboardHeader(
    title = "Midwest Car Accidents",
      titleWidth = 300 
  ),
  
  dashboardSidebar(disable = TRUE),  # Disable the sidebar
  dashboardBody(
    fluidRow(
    ),
      box(
        title = tagList(icon("sliders-h"), "Select View Options"), 
        width = 4, 
        solidHeader = TRUE,
        status = "primary",
        selectInput("selection_type", 
                    HTML("<i class='fa fa-map-marker-alt'></i> View Options:"), #HTML line adds map marker icon
                      choices = c("All Midwest States", "One State", "Two States"),
                      selected = "All Midwest States"
        ),
        
        uiOutput("state_select_ui"),
      ),
      
      box(
        title =  tagList(icon("map"), "Heat Map of Fatal Car Accidents in the Midwest"),
        width = 8,
          solidHeader = TRUE, 
          status = "info",
            girafeOutput("heatmap", height = "800px")
    )
 
  )
  
)



####################################################
#The following code is reactive, so that whatever 
#the user selects in the code, the graphics will 
#react accordingly to what they choose
####################################################

# Server 
server <- function(input, output, session) {
  
  #Makes a popup describing the app to the user
  observe({
    shinyalert(
      title = "Welcome to the Midwest Car Accident Dashboard",
      text = "Explore data on car accident fatalities across Midwest states, normalized by population. Use the options below to get started!",
        type = "info",
        closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          showConfirmButton = TRUE,
            confirmButtonText = "Get Started"
    )
  })
  
  # Dynamic UI for state selection based on primary selection
  output$state_select_ui <- renderUI({
    if (input$selection_type == "One State") {
      selectInput("state1", "Select State:", choices = midwest_states)
    } else if (input$selection_type == "Two States") {
      tagList(
        selectInput("state1", "Select First State:", choices = midwest_states),
        selectInput("state2", "Select Second State:", choices = midwest_states)
      )
    }
  })
  
  # Heat map of the selection from users
  output$heatmap <- renderGirafe({
    
    if (input$selection_type == "All Midwest States") {
      # Calculate fatalities per 100k for each state individually
      plot_data <- accidents_midwest %>%
        group_by(STATENAME) %>%
        summarize(
          total_fatalities = sum(total_fatalities),
          POPULATION = sum(POPULATION),
            fatalities_per_100k = (sum(total_fatalities) / sum(POPULATION)) * 100000,
            .groups = 'drop'
        )
      
      # Lowercase for merging
      plot_data$region <- tolower(plot_data$STATENAME)
      
      # Load and filter state boundaries
      states_map <- map_data("state") %>% filter(region %in% tolower(midwest_states))
      
      # Merge with state-level data
      map_data <- left_join(states_map, plot_data, by = "region")
      
      # Use fatalities per 100k for color fill
      fill_var <- "fatalities_per_100k"
      fill_label <- "Fatalities per 100k"
      
    } else {
      # Use county level data for selected states and show total fatalities
      selected_states <- if (input$selection_type == "One State") {
        input$state1
      } else {
        c(input$state1, input$state2)
      }
      
      # Prepare county-level plot_data
      plot_data <- accidents_midwest %>%
        filter(STATENAME %in% selected_states) %>%
        group_by(STATENAME, COUNTYNAME) %>%
          summarize(total_fatalities = sum(total_fatalities), .groups = 'drop')
      
      # Clean COUNTYNAME by removing numbers in parentheses
      plot_data$COUNTYNAME <- str_remove(plot_data$COUNTYNAME, " \\(.*\\)")
      
      # Add lowercase columns for merging
      plot_data$region <- tolower(plot_data$STATENAME)
      plot_data$subregion <- tolower(plot_data$COUNTYNAME)
      
      # Prepare county-level map data
      county_map <- map_data("county") %>%
        filter(region %in% tolower(selected_states))
      
      # Use left_join to keep the structure of county_map and retain the `group` variable
      map_data <- left_join(county_map, plot_data, by = c("region", "subregion"))
      
      # Set missing values for counties with no data to zero
      map_data$total_fatalities[is.na(map_data$total_fatalities)] <- 0
      
      # Set fill variable for individual state or county view
      fill_var <- "total_fatalities"
      fill_label <- "Total Fatalities"
    }
    
    # Define the tooltip text
    map_data$tooltip_text <- paste(
      "State:", map_data$region,
      if (input$selection_type != "All Midwest States") {
        paste("\nCounty:", map_data$subregion)
      },
      paste("\n"),
      paste(fill_label, ":", round(map_data[[fill_var]], 2))
    )
    
    # Create the ggplot
    gg <- ggplot(map_data, aes(x = long, y = lat, group = group, fill = .data[[fill_var]])) +
      geom_polygon_interactive(aes(tooltip = tooltip_text, data_id = group), color = "black") +
      scale_fill_gradient(low = "lightyellow", high = "red", na.value = "gray") +
        theme_minimal() +
        labs(fill = fill_label) +
          theme(axis.text = element_blank(), axis.title = element_blank(),
          panel.grid = element_blank())
    
    # Render girafe plot with hover effect
    girafe(
      ggobj = gg,
      options = list(
        opts_hover(css = "fill:green;stroke-width:2px;stroke:black;")
      )
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)