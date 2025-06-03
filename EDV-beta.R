library(shiny)
library(tidyverse)
library(readxl)

# User Interface
ui <- fluidPage(
  titlePanel("Economic Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload Excel (.xlsx) file",
                accept = c(".xlsx")),
      hr(),
      selectInput("variable", "Choose a variable:",
                  choices = NULL), # Choices will be updated in the server
      hr(),
      downloadButton("downloadPlot", "Download Plot")
    ),
    mainPanel(
      plotOutput("linePlot") 
    ) 
  )  
)

# Server Logic
server <- function(input, output, session) {
  # Reactive expression to read and process the uploaded data
  data <- reactive({
    req(input$datafile)
    
    # Read the original data
    raw_data <- read_excel(input$datafile$datapath)
    
    # First, ensure all columns except the first one are numeric
    numeric_data <- raw_data
    for(i in 2:ncol(raw_data)) {
      numeric_data[[i]] <- as.numeric(as.character(raw_data[[i]]))
    }
    
    # Create long format data
    long_data <- numeric_data %>%
      rename(Indicator = 1) %>%  # Rename first column to Indicator
      tidyr::pivot_longer(
        cols = -Indicator,
        names_to = "Year",
        values_to = "Value"
      ) %>%
      mutate(
        Year = as.numeric(gsub("[^0-9]", "", Year)),  # Extract only numbers from Year
        Value = as.numeric(sprintf("%.2f", Value))    # Round to 2 decimal places
      ) %>%
      filter(!is.na(Year), !is.na(Value)) %>%        # Remove any NA values
      arrange(Indicator, Year)
    
    long_data
  })
  
  # Update the selectInput choices based on the uploaded data
  observe({  
    req(data())
    # Get unique indicators
    choices <- unique(data()$Indicator)
    updateSelectInput(session, "variable",
                     label = "Choose a variable:",
                     choices = choices)
  })
  
  # Create the plot as a reactive expression
  plot_output <- reactive({
    req(data())
    req(input$variable)
    
    # Filter data for selected variable
    plot_data <- data() %>%
      filter(Indicator == input$variable)
    
    ggplot(plot_data, aes(x = Year, y = Value)) +
      geom_line(linewidth = 1.5, color = "blue") +
      geom_point(color = "blue", size = 3) +
      labs(title = paste(input$variable, "Over Time"),
           y = input$variable,
           x = "Year") +
      theme_minimal() +
      scale_x_continuous(breaks = unique(plot_data$Year)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
  })
  
  # Render the plot
  output$linePlot <- renderPlot({
    plot_output()
  })
  
  # Download handler for the plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0(make.names(input$variable), "_plot.png")
    },
    content = function(file) {
      ggsave(file, plot = plot_output(), device = "png", 
             width = 10, height = 6, units = "in", dpi = 300)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)