library(shiny)
library(dplyr) 
library(tidyquant)
library(quantmod) 
library(plotly) 
library(ggplot2)
library(stringr) 
library(timetk) 
library(shinyjs) 
library(reticulate) 
library(shinydashboard)  
library(echarts4r)

source('Functions.R')


PYTHON_DEPENDENCIES1 = c('newsapi-python' ,'pandas') 
PYTHON_DEPENDENCIES2 = c( 'numpy' ,'scikit-learn') 
 


virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
python_path = Sys.getenv('PYTHON_PATH')





# Create virtual env and install dependencies
reticulate::virtualenv_create(envname = virtualenv_dir) 
reticulate::virtualenv_install(virtualenv_dir, packages = PYTHON_DEPENDENCIES1, ignore_installed=TRUE)
reticulate::use_virtualenv(virtualenv_dir, required = T)





reticulate::use_virtualenv(virtualenv_dir, required = T)

source_python("trypy.py")



news_source=read.csv('local data/source.csv')





data<-read.csv('local data/tickers_symbols.csv') 
#all_symbols=all_symbols %>% select(-X) 
all_symbols<-data$symbol 
data$indices=str_remove(data$indices , "\\[|\\]")  
data$indices=str_remove_all(data$indices , "\\]|\\]")  
data$indices=str_remove_all(data$indices , "'") 

data$industries=str_remove(data$industries , "\\[|\\]")  
data$industries=str_remove_all(data$industries , "\\]|\\]")  
data$industries=str_remove_all(data$industries , "'") 


data1=data[,c('symbol','name' ,'industries' ,'indices')]


colnames(data1)<-c('Symbol','Name' ,'Industries' , 'Indices') 

#all_symbols=all_symbols %>% select(-X) 






# Define UI
ui <- 
  fluidPage(tags$head(
    
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")  
    
    
  ) ,  tags$head(
    tags$script(
      HTML('
        $(document).ready(function(){
          $("#scrollButton").click(function() {
            $("html, body").animate({
              scrollTop: $("html, body").scrollTop() + 700
            }, 1000);
          });
        });
      ')
    )
  ), 
    navbarPage(id="nav-1" , title = div(
      tags$i(class = "glyphicon glyphicon-stats", style = "margin-right: 10px;"),"Stock Market App"),
               
               ################################################
               # fluidRow() 
               
               tabPanel(title = 'Home' , 
                        
                        tags$div(
                          id = "gif-background",
                          tags$img(src = "gip.gif" , width='1440')
                        ),
                        
                        div(
                          id = "content",
                          h1("Welcome to My Shiny App"),
                          p("Welcome to the world of finance, where numbers come to life, opportunities abound, and the pursuit of financial success begins. Finance is a dynamic field that encompasses the management of money, investments, and the study of how individuals, businesses, and governments make financial decisions."),br(), 
                          p("In today's fast-paced and interconnected global economy, finance plays a vital role in shaping the world we live in. It drives economic growth, fuels innovation, and enables individuals and organizations to achieve their goals. From personal finance management to corporate finance strategies, the principles of finance guide our financial decisions and shape our financial futures.")
                        ),
                        
                                  
                                 
                        
                     div( id = "scroll-trigger1" ,
                         fluidRow(id='P1', class='part1' , style="font-size: 16px; color: #141515; background: #6aadcc;font-family: Helvetica Neue; 
                                                                               margin-right: -37px;  margin-left: -38px;
                                                                                " ,  
                                  h3('Portfolio Optimization' , id='Portfolio_Optimization' , 
                                     ),br(),
                            column(6, h4('Effortless Stock Selection'), br(),
                            
                            p("The user interface of this innovative stock selection window is designed to be exceptionally user-friendly. When you open the window, you'll find an intuitive dashboard that allows you to select stocks from an extensive list of options with ease. Whether you're interested in technology sector leaders, reliable blue-chip stocks, or exploring emerging market opportunities,
                              the interface streamlines the process of curating your personalized investment portfolio.") , br(), 
                               h4('Optimized Combinations and Comprehensive Statistics'), br(),  
                            p('What sets this feature window apart is its utilization of 
                            advanced optimization algorithms. As you select stocks,
                            these algorithms work tirelessly to recommend the most optimized combinations 
                            of key indicators, tailored specifically to your investment objectives and risk tolerance.
                            Additionally, the window provides you with comprehensive statistics on risk and return, including historical performance data, volatility assessments, and growth projections. Armed with this valuable information, you can confidently make well-informed investment decisions and navigate the complex world of finance
                              with precision.')
                            
                                  ) 
                           
                           
                           
                           
                           
                           , column(4, br() , tags$image( src = "Portofilio.png",  
                                                                          type = "img/svg",  
                                                                          width = "800",  
                                                                          height = "450" 
                                                                        ))
                           
                           )),
                        fluidRow(id='P2', class='part2' , style="font-size: 16px; color: #141515; background: #b1a9a1 ;font-family: Helvetica Neue; 
                                                                               margin-right: -37px;  margin-left: -38px;
                                                                                " ,  
                                 h3('News Sentiment' , id='Trend' , 
                                    style="font-size: 28px;text-align: center;" ),br(),
                                 fluidRow(column(4, tags$image(src = "Newspage.png",  
                                                               type = "img/svg",  
                                                               width = "800",  
                                                               height = "450" )) 
                                 
                                  , column(5, offset = 2 , p("A dynamic tool that redefines the way you consume news. Our website's Feature Window offers an unparalleled experience by allowing you to curate your news feed with precision. You can effortlessly handpick your preferred news subjects and sources, ensuring that the content you receive aligns perfectly with your interests. What truly distinguishes our Feature Window is its seamless integration of state-of-the-art AI sentiment analysis. As you navigate through the articles, our AI system diligently assesses the sentiment of each piece, delivering insightful sentiment scores. This unique feature enables you to gauge the emotional tone of the news, providing a nuanced perspective on current events. Make well-informed decisions about which stories to explore in-depth and gain a deeper understanding of the news landscape. Embrace the future of news consumption, harnessing the power of AI-driven sentiment analysis, 
                                  and elevate your news experience with our Feature Window." , 
                                                             style='margin-top: 60px;'))) 
                                 
                                 
                                 
                                 
                                 ) , 
                     
                     fluidRow(id='P3', class='part3' , style="font-size: 16px; color:#141515; background: #6f7a83 ;font-family: Helvetica Neue; 
                                                                               margin-right: -37px;  margin-left: -38px;
                                                                                " ,  
                              h3('Stock report' , id='stock2' ) , 
                              
                              p('Ongoing devlopment' , style='font-size: 40px ; text-align:center;')) 
                     
                        
                                 
                        ) 
                        , 
               tabPanel(title = "Portfolio Analytics",
                        
                 #reurns      
               sidebarLayout(
                 sidebarPanel(
                   selectInput("input_list", "Select Stock Symbole:", choices =all_symbols, multiple = TRUE),
                   dateRangeInput("date_range", "Select Date Range:", start = as.Date('2020-01-01'), end = NULL),
                   actionButton("submit_button", "Submit"),br(),br(),fluidPage(
                   textOutput('text_output1'),br(), tableOutput("shappo") , br() ,tableOutput("shappo2")) , 
                   style=' min-height: 20px;
    padding: 19px;
    margin-top: 30px;
    background-color: #e9e5e5;
    border: 1px solid #e3e3e3;
    border-radius: 4px;
    -webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,.05);
    box-shadow: 18px 18px 9px rgb(0 0 0 / 48%); 
   border-radius: 50px;')
                 , mainPanel(
                   fluidRow(
                     column(width = 12, plotlyOutput("stockprices" , height = "230px") , 
                            style='margin-top:40px')
                   ),br(), 
                   fluidRow(
                     column(width = 6, plotlyOutput("correlation_plot")), 
                     column(width = 6,plotlyOutput("risk_return_plot")) 
                 
                        ) , br() , fluidRow(column(width = 6 , plotlyOutput('reurns2')) , 
                                            column(width = 6 , plotlyOutput('S_return_plot'))) ) , 
               
             
    ##########
  )), tabPanel("News Overview", sidebarLayout(
    sidebarPanel( fluidRow(column(3, offset = 1,textInput('subject' ,'Subject' , value = '' , width = '320px')),
                           column(3 ,selectInput('source' , 'Select a source:',width = '280px' , 
                                                 choices = c('All',news_source$name)))) , fluidRow(
                           column(4, offset = 1,
                           actionButton('look' , ' research ' ))),  
                   br() , 
                   fluidRow(style='height: 280px;',column(7 ,offset = 1 ,
                    plotlyOutput('news_plot') , style='') , column(3, echarts4rOutput('gauge2') , 
                                                                   style='margin-top: -67px;
    margin-left: 50px;')) 
                  , width = 14 , 
  style='margin-top: 60px;
    background-color: #e9e5e5;
    border-radius: 180px; 
    box-shadow: 14px 0px 10px 8px rgb(0 0 0 / 48%);' ), mainPanel(fluidRow(column(offset = 5 , 1, div(
                    actionButton("scrollButton", icon("arrow-down"), class = "btn-primary"),
                    style = "text-align: center;
               margin-top: -60px;
               margin-left: 70px; 
               color: black;
               background-color:#e9e5e5;
               "
                  ))), 
                  uiOutput("news_article2") 
                  , width = 30 ) 
                   )  
  ) , 
  ################
  tabPanel("Stock Report" , )
  
  
  
  
  
  ))
              
                          
               
                












server <- function(input, output , session) {
  
  reticulate::virtualenv_install(virtualenv_dir, packages = PYTHON_DEPENDENCIES2, ignore_installed=TRUE)
  reticulate::use_virtualenv(virtualenv_dir, required = T)
  
  source_python("trypy.py") 
  

  
  #reticulate::virtualenv_install(virtualenv_dir, packages = PYTHON_DEPENDENCIES2, ignore_installed=TRUE)
  
  
  
  
  
   
  
  
  
  observe({
    # When the user scrolls to a certain point, trigger the appearance
    shinyjs::runjs(
      'if ($(window).scrollTop() > 200) {
         $("#scroll-trigger1").addClass("appear");
       }'
    )
  })

  
  
  
 

  
 #### Portfolio Page #####  
  observeEvent(input$submit_button, {
    input_list <- input$input_list
    min_return <- input$min_return
    max_risk <- input$max_risk
    
    
    
    # Implement your processing logic here
    dota <- create_data(input_list,from = input$date_range[1] , to = input$date_range[2])  
    dota[input_list] <- lapply(dota[input_list], as.numeric) 
    dvdd<-create_data_dvd(input_list,from = input$date_range[1] , to = input$date_range[2])
    # Convert the result to a data frame
    
    
    # Display the resulting data frame
    rownames(dota)<-dota$date1
    dvd=Return.calculate(dota[,2:length(dota)])  
    
    dvd<-dvd[2:nrow(dvd),] 
    
    returns2<-returns_STK(input_list ,dota , dvd)
    
    cov_matrix <- cov(returns2) 
    
    portfolio <- data.frame(
      Stock = input_list,
      Return=unlist(unname(as.list(colMeans(returns2)))) ,
      Risk = unlist(unname(as.list(apply(returns2, 2, sd)))) 
    ) 
    
   
    
    
    output$S_return_plot <- renderPlotly({
      # Create the risk-return scatter plot with surface
      v <- ggplot(portfolio, aes(x = Risk, y = Return)) +
        geom_text(aes(label = Stock), hjust = 0, vjust = 1, size = 3, fontface = "bold") +
        labs(x = "Risk", y = "Return", title = "Stocks Risk-Return") + 
        scale_y_continuous(labels = scales::percent) +  
        scale_x_continuous(labels = scales::percent) + 
        theme_minimal() 
      
      ggplotly(v) %>% layout(plot_bgcolor = "#d9d9e7",
                             paper_bgcolor = "#d9d9e7")
      
    })
    
    
    
    output$reurns2 <- renderPlotly({
      
      plot <-plot_ly() %>%
        layout(
          title = "Stock Returns",
          xaxis = list(title = "Date"),
          yaxis = list(title = "Price") , 
          showlegend = TRUE
        )  
      
      for (stock in input_list) {
        plot <- plot %>% add_trace(
          x = as.Date(rownames(returns2)),
          y = returns2[[stock]],
          name = stock,
          type = "scatter",
          mode = "lines"
        )} 
      plot %>% layout(plot_bgcolor = "#d9d9e7",
                  paper_bgcolor = "#d9d9e7")
    }) 
    
    
    
    
    
    
    
    output$stockprices <- renderPlotly({
      
      plot <- plot_ly() %>%
        layout(
          title = "Stock Price Fluctuation",
          xaxis = list(title = "Date"),
          yaxis = list(title = "Price")
        )  
      
      for (stock in input_list) {
        plot <- plot %>% add_trace(
          x = as.Date(dota$date1),
          y = dota[[stock]],
          name = stock,
          type = "scatter",
          mode = "lines"
        )
      } 
      
      plot <- plot %>% layout(title = "Stock Price Chart",
                              xaxis = list(
                                title = "Date",
                                rangeselector = list(
                                  buttons = list(
                                    list(count = 1, label = "1m", step = "month", stepmode = "backward"),
                                    list(count = 6, label = "6m", step = "month", stepmode = "backward"),
                                    list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
                                    list(count = 1, label = "1y", step = "year", stepmode = "backward"),
                                    list(step = "all")
                                  )
                                ),
                                rangeslider = list(visible = TRUE)
                              ),
                              yaxis = list(title = "Closing Price")
      )
      
      plot %>% layout(plot_bgcolor = "#d9d9e7",
                 paper_bgcolor = "#d9d9e7")
    })
    
    
     
    
    portfolio_data=generate_portfolios(dvd)  
    
    max_sharpe_index <- which.max(portfolio_data$SharpeRatio)
    min_variance_index <- which.min(portfolio_data$Risk)  
    
    colnames(portfolio_data[,1:length(input_list)])<-input_list
    print('ok')
    # Create the correlation plot
    output$correlation_plot <- renderPlotly({
      correlation_data <- as.matrix(cor(dota[, input_list]))
      
      plot_ly(
        x = colnames(correlation_data),
        y = colnames(correlation_data),
        z = correlation_data,
        type = "heatmap",
        colorscale = "RdBu",
        colorbar = list(title = "Correlation"),
        hoverinfo = "text",
        text = ~paste("Correlation: ", correlation_data),
        xaxis = list(side = "top") # Place x-axis labels on top
      ) %>%
        layout(
          title = "Correlation Matrix",
          xaxis = list(side = "bottom"), # Place x-axis labels on top
          yaxis = list(autorange = "reversed") # Reverse y-axis
           # Enable highlighting
        ) %>% layout(plot_bgcolor = "#d9d9e7",
                     paper_bgcolor = "#d9d9e7")
    })
    
    
    
    
    output$risk_return_plot <- renderPlotly({
      # Create the risk-return scatter plot with surface
      p<-ggplot(portfolio_data, aes(x = Risk, y = Return , color=SharpeRatio)) +
        geom_point() +
        scale_y_continuous(labels = scales::percent) +
        scale_x_continuous(labels = scales::percent) + 
        geom_point(aes(x = Risk[max_sharpe_index], y = Return[max_sharpe_index]),
                   color = "blue", size = 2) + 
        geom_point(aes(x = Risk[min_variance_index], y = Return[min_variance_index]),
                   color = "green", size = 2) 
        labs(x = "Risk", y = "Return" ) + 
        
         theme(
          legend.background = element_rect(fill = "#d9d9e7")
        )
        
        
        
        
        ggplotly(p) %>% layout(plot_bgcolor = "#d9d9e7",
                                                   paper_bgcolor = "#d9d9e7")
    })
    
  
    
     
    
    
    output$text_output1 <- renderText({
      "The Best Shap Ratio Portofilio Weights :"}) 
    
    output$shappo<-renderTable({ 
      c= portfolio_data[max_sharpe_index,1:length(input_list)]   
      colnames(c)<-input_list 
      c=t(c) 
      c<-as.data.frame(c) 
      colnames(c)<-'Weights'  
      c[,'Percent (%)']<-round(c$Weights*100 , 2)
      c
    } , width = "100%" , rownames = TRUE) 
    
    output$shappo2<-renderTable({ 
      portfolio_data[max_sharpe_index,c("Risk","Return","SharpeRatio")]   
      
    } , width = "120%")
    
    
    #store_portfolio 
  }) 
 ### Render news -----
    
  
 
 
  ddita <- reactiveVal(NULL)
  
  
  observeEvent(input$look, {
    
    
    
    source1 <- input$source
    subject<-input$subject 
    
    # Update ddita
    ddita_data <- functi(subject, source1, news_source)
    ddita(ddita_data)
    
    
   
  
  
  
    output$gauge2 <- renderEcharts4r({
      ddita=ddita()
      v <- sum(ddita$sentiment) / nrow(ddita)
      e_charts() |>
        e_gauge(round(v, 2), min = -1, max = 1,
                "Sentiment Score", axisLine = list(
                  lineStyle = list(
                    color = list(
                      c(0.3, "red"),
                      c(0.7, "#b9b9c0"),
                      c(1, "green")
                    ))))
    })
    
    output$news_plot <- renderPlotly({ 
      ddita = ddita() 
      line_colors <- ifelse(ddita$sentiment >= 0, "green", "red")
      plot_ly(data = ddita, x = ~publishedAt, y = ~sentiment, type = 'scatter', mode = 'lines',
              name = 'Sentiment',
              text = ~paste("Article: ", title2, "<br>Sentiment: ", SentimentText),
              marker = list(color = line_colors, width = 2)) %>%
        layout(title = 'News Sentiment Analysis',
               xaxis = list(title = 'Date'),
               yaxis = list(title = 'Sentiment'),
               showlegend = FALSE,
               hovermode = "closest") %>% config(displayModeBar = FALSE) %>%
        layout(width = 800, height = 250, plot_bgcolor = "#e9e5e5",
               paper_bgcolor = "#e9e5e5") 
    })

  
  
    
  selected_article2 <- reactiveVal(NULL)
  
  output$news_article2 <- renderUI({
    ddita1<-ddita()
    articles <- lapply(seq_len(nrow(ddita())), function(i) {
      article <- ddita1[i, ]
      div( 
        id = paste0("article_", i),
        box(
          title = h4(article$title),
          width = 4,
          img(src = article$urlToImage, width = "100%", height = "150px"),
          div(
            id = paste0("content_", i),
            HTML(paste0(substr(article$description, 1, 400), " ...")), br(), br(),
            HTML(paste0('Published at:  ' , article$publishedAt)) , 
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
  
  
  observe({ 
    dita1<-ddita()
    lapply(seq_len(nrow(ddita())), function(i) {
      local_i <- i  # Store the value of i locally
      observeEvent(input[[paste0("show_more_", local_i)]], {
        selected_article2(local_i)
        showModal(modalDialog(
          title = dita1[local_i, "title"],
          img(src = dita1[local_i, "urlToImage"], width = "100%", height = "150px"),
          HTML(dita1[local_i, "description"]), br() , br() , 
          HTML(paste0("The Source Url: <a href='",
                      dita1[local_i, "url"], "' target='_blank'>", dita1[local_i, "url"], "</a>")) , 
          footer = NULL,
          easyClose = TRUE
        ))
      })
    })
  })
  

  })

  
  


  
  
    
  
  }  


  
# Run the Shiny application
shinyApp(ui = ui, server = server)