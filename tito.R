library(shiny)

ui <- fluidPage(
  titlePanel("Dynamic UI Example"),
  sidebarLayout(
    sidebarPanel(
      textInput("inputText", "Enter something"),
      actionButton("submitButton", "Submit")
    ),
    mainPanel(
      uiOutput("dynamicContent")
    )
  )
)

server <- function(input, output) {
  # Initialize dynamic_content
  dynamic_content <- reactiveVal(NULL)
  
  observeEvent(input$submitButton, {
    # Check if the submit button is clicked
    if (input$submitButton > 0) {
      # Create dynamic content based on the input
      dynamic_content(paste("You entered:", input$inputText))
    }
  })
  
  # Render the dynamic content
  output$dynamicContent <- renderUI({
    if (!is.null(dynamic_content())) {
      p(dynamic_content())
    }else{
      p('hello hello')
    }
  })
}

shinyApp(ui, server)

