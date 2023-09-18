library(shiny)
library(reticulate)
library(dplyr)

# Sample data
# Assuming 'py$news20' is your data frame containing news articles

# Define a function to truncate the title
truncate_title <- function(title, max_length) {
  if (nchar(title) > max_length) {
    return(paste(substr(title, 1, max_length), "..."))
  } else {
    return(title)
  }
}

# Sample data (assuming 'py$news20' is your data frame)
dota <- py$news20
dota <- dota()
dota$publishedAt <- as.POSIXct(dota$publishedAt, format = "%Y-%m-%dT%H:%M")
dota$SentimentText <- ifelse(dota$sentiment > 0, "Positive", ifelse(dota$sentiment < 0, "Negative", "Neutral"))
max_title_length <- 30
dota$title2 <- sapply(dota$title, function(title) truncate_title(title, max_title_length))
dota <- dota %>% arrange(publishedAt)

ui <- fluidPage(
  fluidRow(
    column(
      width = 12, # Utilisez toute la largeur de la grille Bootstrap
      uiOutput("news_articles")
    )
  )
)

server <- function(input, output, session) {
  # Create a reactiveValues object to store the selected article index
  selected_article <- reactiveVal(NULL)
  
  output$news_articles <- renderUI({
    articles <- lapply(seq_len(nrow(dota)), function(i) {
      article <- dota[i, ]
      column(
        width = 3, # Divisez la largeur en 4 colonnes pour afficher 4 articles côte à côte
        box(
          title = h4(article$title),
          width = 12,
          img(src = article$urlToImage, width = "100%", height = "150px"),
          div(
            id = paste0("content_", i),
            HTML(paste0(substr(article$description, 1, 200), " ...")),
            style = "display: block;"
          ),
          actionButton(
            inputId = paste0("show_more_", i),
            label = "See More",
            icon = icon("plus"),
            style = "color: #f1f5ff;
            background-color: #4c934d;
            border-color: #e5dada;
            width: 100px;
            margin-left: 0;"
          )
        )
      )
    })
    
    fluidRow(do.call(tagList, articles))
  })
  
  # Function to handle the "See More" button click event
  observe({
    lapply(seq_len(nrow(dota)), function(i) {
      local_i <- i  # Store the value of i locally
      observeEvent(input[[paste0("show_more_", local_i)]], {
        selected_article(local_i)
        showModal(modalDialog(
          title = dota[local_i, "title"],
          img(src = dota[local_i, "urlToImage"], width = "100%", height = "150px"),
          HTML(dota[local_i, "content"]),
          footer = NULL,
          easyClose = TRUE
        ))
      })
    })
  })
}

shinyApp(ui, server)
