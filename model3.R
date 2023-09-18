library(shiny)

# Sample data
dota <- py$news20()

ui <- fluidPage(
  uiOutput("news_articles"),
  tags$script(
    HTML('
      $(document).on("click", "[id^=show_more_]", function() {
        var id = $(this).attr("id").split("_")[3];
        var content = $("#content_" + id).text();
        Shiny.setInputValue("selected_article_content", content);
      });
    ')
  )
)

server <- function(input, output, session) {
  output$news_articles <- renderUI({
    articles <- lapply(1:nrow(dota), function(i) {
      article <- dota[i, ]
      column(4,
             box(
               title = h4(article$title),
               width = 12,
               div(
                 id = paste0("content_", i),
                 HTML(paste0('<span id="content_', i, '">', article$description, '</span>')),
                 style = "display: block;"
               ),
               actionButton(
                 inputId = paste0("show_more_button_", i),
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
    do.call(tagList, articles)
  })
  
  observeEvent(input$selected_article_content, {
    showModal(modalDialog(
      title = "Full Article",
      HTML(input$selected_article_content),
      footer = tagList(
        actionButton("exit_modal", "Exit", icon = icon("times"))
      )
    ))
  })
  
  observeEvent(input$exit_modal, {
    removeModal()
  })
}

shinyApp(ui, server)
