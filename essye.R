library(reticulate) 
library(ggplot2) 
library(dplyr)
library(plotly)

library(echarts4r)







dota=py$news20()
dota$publishedAt=as.POSIXct(dota$publishedAt, format = "%Y-%m-%dT%H:%M")
dota$SentimentText <- ifelse(dota$sentiment > 0, "Positive", ifelse(dota$sentiment < 0, "Negative", "Neutral"))
max_title_length <- 30
dota$title2 <- sapply(dota$title, function(title) truncate_title(title, max_title_length))
dota =dota %>% arrange(publishedAt)


content=dota$content[1]

content
full_text <- paste(dota$content, collapse = ' ') 

full_text

cat(full_text)


dota$content[1]


dota2= dota %>% group_by(publishedAt) %>% summarise(sentiment=mean(sentiment)) 

list=1:nrow(dota) 

dota$ids=1:nrow(dota)


pop=ggplot(gn_news , aes(x = publishedAt, y = sentiment)) + geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  scale_x_datetime(
    
    labels = scales::date_format("%Y-%m-%d %H:%M")
  ) + xlab('TIME') +ylab('NEWS Sentiment') # Yellow area below the line  # Yellow area below the line

ggplotly(pop)


dota$publishedAt=as.POSIXct(dota$publishedAt, format = "%Y-%m-%dT%H:%M")

dota2= dota %>% group_by(publishedAt) %>% summarise(sentiment=mean(sentiment)) 


pop=ggplot(gn_news , aes(x = publishedAt, y = sentiment)) + geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  scale_x_datetime(
    
    labels = scales::date_format("%Y-%m-%d %H:%M")
  ) + xlab('TIME') +ylab('NEWS Sentiment') # Yellow area below the line  # Yellow area below the line

ggplotly(pop)






library(plotly)

# Sample data (replace with your actual data)
dates <- as.Date(c('2023-09-01', '2023-09-02', '2023-09-03', '2023-09-04', '2023-09-05'))
sentiment_scores <- c(0.2, -0.4, 0.8, -0.1, 0.0)
article_names <- c("Article 1", "Article 2", "Article 3", "Article 4", "Article 5")


truncate_title <- function(title, max_length) {
  if (nchar(title) > max_length) {
    return(paste(substr(title, 1, max_length), "..."))
  } else {
    return(title)
  }
}

max_title_length <- 30
dota$title2 <- sapply(dota$title, function(title) truncate_title(title, max_title_length))



names(dota)
# Create a data frame with sentiment scores, article names, and sentiment in words
chart_data <- data.frame(Date = dates, Sentiment = sentiment_scores, Article = article_names)

dota$SentimentText <- ifelse(dota$sentiment > 0, "Positive", ifelse(sentiment_scores < 0, "Negative", "Neutral"))

# Define line colors based on sentiment scores
line_colors <- ifelse(dota$sentiment >= 0, "green", "red")

# Create an interactive plot with line color based on sentiment and tooltips
plot_ly(data = dota, x = ~publishedAt, y = ~sentiment, type = 'scatter', mode = 'lines',
        name = 'Sentiment',
        text = ~paste("Article: ", title2 , "<br>Sentiment: ", SentimentText),
        marker = list(color = line_colors, width = 2)) %>%
  layout(title = 'News Sentiment Analysis',
         xaxis = list(title = 'Date'),
         yaxis = list(title = 'Sentiment'),
         showlegend = FALSE,
         hovermode = "closest")








gauge_chart <- plot_ly(
  type = "indicator",
  mode = "gauge+number",
  title = "News Sentiment Score",
  value = 0.3,
  domain = list(x = c(0, 1), y = c(0, 1)),
  gauge = list(
    axis = list(range = c(-1, 1)),
    bar = list(color = "blue"),
    steps = list(
      list(range = c(-1, -0.5), color = "red"),
      list(range = c(-0.5, 0.5), color = "yellow"),
      list(range = c(0.5, 1), color = "green")
    )
  )
)


library(echart4r)

# Sample data
library(echart4r)

# Sample data
data <- data.frame(
  publishedAt = seq(Sys.Date(), by = "days", length.out = 7),
  sentiment = c(0.2, -0.4, 0.8, -0.6, 0.0, 0.3, -0.2)
)

# Define a function to customize the marker symbol and color based on the value
customizeMarker <- function(value) {
  if (value > 0) {
    return(list(symbol = "pin", symbolSize = 10, itemStyle = list(color = "green")))
  } else if (value < 0) {
    return(list(symbol = "pin", symbolSize = 10, itemStyle = list(color = "red")))
  } else {
    return(list(symbol = "pin", symbolSize = 10, itemStyle = list(color = "blue")))
  }
}

# Create a separate data frame for the markers
markers_data <- data %>%
  mutate(
    symbol = sapply(sentiment, function(value) {
      return(customizeMarker(value)$symbol)
    }),
    symbolSize = 10,
    itemStyle = sapply(sentiment, function(value) {
      return(customizeMarker(value)$itemStyle)
    })
  )

data %>%
  e_charts(publishedAt) %>%
  e_line(sentiment) %>%
  e_mark_line(
    silent = TRUE,
    data = data.frame(sentiment = 0),  # Marker line at y = 0
    lineStyle = list(color = "blue")
  ) %>%
  e_mark_point(
    data = markers_data,
    symbol = markers_data$symbol,
    symbolSize = markers_data$symbolSize,
    itemStyle = markers_data$itemStyle
  ) %>%
  e_title("Line and area charts")







