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
                  choices = NULL) # Choices will be updated in the server
    ),
    mainPanel(
      plotOutput("linePlot")
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Reactive expression to read the uploaded data
  data <- reactive({
    req(input$datafile)
    read_excel(input$datafile$datapath, sheet = "Transpose")
  })
  
  # Update the selectInput choices based on the uploaded data
  observe({
    req(data()) # Ensure data is loaded
    updateSelectInput(session, "variable",
                      label = "Choose a variable:",
                      choices = colnames(data()))
  })
  
  output$linePlot <- renderPlot({
    req(data()) # Ensure data is loaded
    req(input$variable) # Ensure a variable is selected
    
    ggplot(data(), aes(x = Year, y = .data[[input$variable]])) +
      geom_line(color = "blue", size = 1.5) +
      labs(title = paste(input$variable, "Over Time"),
           y = input$variable) +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)