library(shiny)
library(shinydashboard)

# Sample news data
news_data <- data.frame(
  Title = c("News Article 1", "News Article 2", "News Article 3"),
  Content = c(
    "This is the content of News Article 1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
    "This is the content of News Article 2. Ut ac justo vitae purus ullamcorper scelerisque nec eget metus.",
    "This is the content of News Article 3. Integer at sapien non sapien euismod euismod ut in odio."
  ),
  Image = c(
    "https://via.placeholder.com/150x150",
    "https://via.placeholder.com/150x150",
    "https://via.placeholder.com/150x150"
  ),
  stringsAsFactors = FALSE
)


colnames(dota)

news_data<-dota

ui <- fluidPage(
  dashboardHeader(title = "News Section"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      uiOutput("news_articles")
    )
  )
)

server <- function(input, output) {
  output$news_articles <- renderUI({
    articles <- lapply(1:nrow(news_data), function(i) {
      article <- news_data[i, ]
      column(4,
             box(
               title = article$title,
               width = 12,
               img(src = article$urlToImage, width = "100%", height = "150px"),
               div(
                 id = paste0("content_", i),
                 HTML(paste0(substr(article$description, 1, 100), " ...")),
                 style = "display: block;"
               ),
               actionButton(
                 inputId = paste0("show_more_", i),
                 label = "See More",
                 icon = icon("plus")
               ),
               actionButton(
                 inputId = paste0("show_less_", i),
                 label = "See Less",
                 icon = icon("minus"),
                 style = "display: none;"
               )
             )
      )
    })
    do.call(tagList, articles)
  })
  
  observe({
    for (i in 1:nrow(news_data)) {
      observeEvent(input[[paste0("show_more_", i)]], {
        shinyjs::toggle(paste0("content_", i))
        shinyjs::toggle(paste0("show_more_", i))
        shinyjs::toggle(paste0("show_less_", i))
      })
      
      observeEvent(input[[paste0("show_less_", i)]], {
        shinyjs::toggle(paste0("content_", i))
        shinyjs::toggle(paste0("show_more_", i))
        shinyjs::toggle(paste0("show_less_", i))
      })
    }
  })
}

shinyApp(ui, server)
