library(shiny)

# Sample data



dota <- py$news20
dota=py$news20()
dota$publishedAt=as.POSIXct(dota$publishedAt, format = "%Y-%m-%dT%H:%M")
dota$SentimentText <- ifelse(dota$sentiment > 0, "Positive", ifelse(dota$sentiment < 0, "Negative", "Neutral"))
max_title_length <- 30
dota$title2 <- sapply(dota$title, function(title) truncate_title(title, max_title_length))
dota =dota %>% arrange(publishedAt)



ui <- fluidPage(
  uiOutput("news_articles")
)




server <- function(input, output, session) {
  # Create a reactiveValues object to store the selected article index
  selected_article <- reactiveVal(NULL)
  
  output$news_articles <- renderUI({
    articles <- lapply(seq_len(nrow(dota)), function(i) {
      article <- dota[i, ]
      div(
        id = paste0("article_", i),
        fluidRow(
          column(
            width = 4,
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
        )
      )
    })
    
    do.call(tagList, articles)
  })
  
  # Function to handle the "See More" button click event
  observe({
    lapply(seq(1,nrow(dota)), function(i) {
      local_i <- i  # Store the value of i locally
      observeEvent(input[[paste0("show_more_", local_i)]], {
        selected_article(local_i)
        showModal(modalDialog(
          title = dota[local_i, "title"],
          img(src = dota[local_i, "urlToImage"], width = "100%", height = "150px"),
          HTML(dota[local_i, "content"]),
          footer = NULL ,easyClose = TRUE
        ))
      })
    })
  })
}

shinyApp(ui, server)
