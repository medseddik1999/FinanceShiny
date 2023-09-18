observeEvent(input$submitb2,{
  
  ddita = functi(input$NSS,input$source,news_source) 
  
  
  output$news_article2<-renderUI({
    articles <- lapply(seq_len(nrow(ddita)), function(i) {
      article <- ddita[i, ]
      div(
        id = paste0("article_", i),
        box(
          title = h4(article$title),
          width = 4,  
          img(src = article$urlToImage, width = "100%", height = "150px"),
          div(
            id = paste0("content_", i),
            HTML(paste0(substr(article$description, 1, 400), " ...")),
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
    
    do.call(tagList, articles)
    
    
    
  })
  
  
  selected_article2 <- reactiveVal(NULL) 
  
  observe({
    lapply(seq_len(nrow(ddita)), function(i) {
      local_i <- i  # Store the value of i locally
      observeEvent(input[[paste0("show_more_", local_i)]], {
        selected_article2(local_i)
        showModal(modalDialog(
          title = ddita[local_i, "title"],
          img(src = ddita[local_i, "urlToImage"], width = "100%", height = "150px"),
          HTML(ddita[local_i, "content"]),
          footer = NULL,
          easyClose = TRUE
        ))
      })
    })
  })
  
  
  
  output$gauge22 <-renderEcharts4r({
    v=sum(ddita$sentiment)/nrow(ddita)
    e_charts() |>
      e_gauge(round(v,2) , min= -1 , max= 1,
              "Setiment Score" , axisLine = list(
                lineStyle = list(
                  color=list(
                    c(0.3, "red"),
                    c(0.7, "#b9b9c0"),
                    c(1, "green")
                  )))) 
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$dynamicContent <- renderUI({
    if ( !is.na(input$NSS) | input$NSS!='') {
      mainPanel(fluidRow( column(offset = 5 , 1, div(
        actionButton("scrollButton", icon("arrow-down"), class = "btn-primary"),
        style = "text-align: center;
               margin-top: -60px;
               margin-left: 60px;"
      ))), 
      uiOutput("news_article2") 
      , width = 30 )
    } else {
      mainPanel(fluidRow( column(offset = 5 , 1, div(
        actionButton("scrollButton", icon("arrow-down"), class = "btn-primary"),
        style = "text-align: center;
               margin-top: -60px;
               margin-left: 60px;"
      ))), 
      uiOutput("news_articles") 
      , width = 30 )
      
    }
    
  })
  
  
  
}) 


