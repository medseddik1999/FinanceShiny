library(PortfolioAnalytics)
library(reticulate)
library(lubridate) 





filter_data <- function(n , data){
  dori = data %>% filter()
}



truncate_title <- function(title, max_length) {
  if (nchar(title) > max_length) {
    return(paste(substr(title, 1, max_length), "..."))
  } else {
    return(title)
  }
}


create_data<- function(listo, from , to){
  dota=c()
  for (i in listo){
    data1<- tq_get(i, from = from, to = to ,get = "stock.prices") 
    col=data1$close
    dota<-cbind(dota,col)
    
  } 
  date1=as.character(data1$date) 
  colnames(dota)<-listo
  dota=cbind(date1,dota)
  dota=as.data.frame(dota)
  return(dota)
} 





create_data_dvd <- function(listo, from, to) {
  dota <- c()
  for (i in listo) {
    tryCatch({
      data1 <- getDividends(i, from = from, to = to)
      col <- data1[, 1]
      
      if (length(col) == 0) {
        col <- rep(0, length(dota))
      } else {
        col <- col
      }
      
      dota <- cbind(dota, col)
    }, error = function(err) {
      print(paste("An error occurred for symbol", i, ":", err))
    })
  }
  
  return(dota)
}


# Check the data types of the columns

comp_returns <- function(n ,d , listo) {
  returns <- diff(log(n))   
  returns
}

returns_STK<-function(listo ,prices_data , dvd_data){ 
  if (nrow(dvd_data) == 0){ 
    rownames(prices_data)<-prices_data$date1
    prices_data[,2:length(prices_data)]<- lapply(prices_data[,2:length(prices_data)], as.numeric)
    returns_dd_price=Return.calculate(prices_data[,2:length(prices_data)]) 
    return(prices_data)
  } else{
    prices_data[,2:length(prices_data)]<- lapply(prices_data[,2:length(prices_data)], as.numeric)
    dvd_data=as.data.frame(dvd_data)
    dvd_data[is.na(dvd_data)] <- 0   
    prices_data = prices_data %>% filter(date1 %in% rownames(dvd_data) ) 
    
    rownames(prices_data)<-prices_data$date1
    returns_price <- Return.calculate(prices_data[,2:length(prices_data)]) 
    prices_data$datos=prices_data$date1 
    dvd_data$datos=rownames(dvd_data) 
    
    returns_dd_price <- left_join(dvd_data, prices_data, by = "datos")  
    returns_price$datos<-rownames(returns_price) 
    returns_price =returns_price %>% filter(datos %in% dvd_data$datos) 
    returns_dvdd <- left_join(dvd_data, returns_price, by = "datos")  
    for (st in listo) {  
      if ( paste0(st,'.div') %in% colnames(returns_dd_price)){
        print(paste0(st,'.div')) 
        returns_dd_price[,st]=returns_dd_price[,paste0(st,'.div')]/returns_dd_price[,st] 
        returns_dd_price[,st]=returns_dd_price[,st]+returns_dvdd[,st]} else    
        { 
          b = returns_price[,st]  
          returns_dd_price[,st]=b 
        } 
    }
    
    returns_dd_price=returns_dd_price %>% select(listo) 
    rownames(returns_dd_price)<-dvd_data$datos
    returns_dd_price=na.omit(returns_dd_price)
    return(returns_dd_price)}
}

optimize_port <- function(df ,cov_matrix , min_return, max_risk) {
  
  
  # Create the portfolio object
  portfolio <- portfolio.spec(assets = colnames(cov_matrix))
  
  # Set the portfolio's assets and constraints
  portfolio <- add.constraint(portfolio, type = "weight_sum", min_sum = 1, max_sum = 1)
  portfolio <- add.constraint(portfolio, type = "box", min = 0, max = 1)
  
  # Add the risk objective
  portfolio <- add.objective(portfolio, type = "risk", name = "StdDev")
  
  # Add minimum return constraint
  portfolio <- add.constraint(portfolio, type = "return", return_target = min_return)
  
  # Add maximum risk constraint
  portfolio <- add.constraint(portfolio, type = "risk", risk_target = max_risk, strict = TRUE)
  portfolio <- add.constraint(portfolio, type="diversification", div_target=0.2)
  # Optimize the portfolio
  opt_portfolio <- optimize.portfolio(df, portfolio, cov_mat = cov_matrix)
  pot <- opt_portfolio$weights
  
  pot <- as.data.frame(pot)
  colnames(pot) <- "weights"
  pot
}



generate_portfolios <- function(prices_data) {
  num_port <- 5000
  
  # Creating a matrix to store the weights
  all_wts <- matrix(nrow = num_port, ncol = ncol(prices_data))
  
  # Creating an empty vector to store Portfolio returns
  port_returns <- vector('numeric', length = num_port)
  
  # Creating an empty vector to store Portfolio Standard deviation
  port_risk <- vector('numeric', length = num_port)
  
  # Creating an empty vector to store Portfolio Sharpe Ratio
  sharpe_ratio <- vector('numeric', length = num_port)
  
  for (i in seq_along(port_returns)) {
    wts <- runif(ncol(prices_data))
    wts <- wts / sum(wts)
    
    # Storing weight in the matrix
    all_wts[i, ] <- wts
    
    # Portfolio returns
    port_ret <- sum(wts * colMeans(prices_data))
    port_ret <- ((port_ret + 1)^252) - 1
    
    # Storing Portfolio Returns values
    port_returns[i] <- port_ret
    
    # Creating and storing portfolio risk
    port_sd <- sqrt(t(wts) %*% (cov(prices_data) %*% wts))
    port_risk[i] <- port_sd
    
    # Creating and storing Portfolio Sharpe Ratios
    # Assuming 0% Risk-free rate
    sr <- port_ret / port_sd
    sharpe_ratio[i] <- sr
  } 
  
  portfolio_values <- tibble(Return = port_returns, Risk = port_risk, SharpeRatio = sharpe_ratio)
  #all_wts <- as.data.frame(all_wts) 
  
  #colnames(all_wts) <- colnames(returns) 
  
  
  
  return( cbind(all_wts,portfolio_values))
} 


functi<-function(text=NULL ,y='All' ,data){
  if (y=='All' && text != ''){
    ddita=py$news201(text) 
    ddita$publishedAt=as.POSIXct(ddita$publishedAt, format = "%Y-%m-%dT%H:%M") 
    ddita$SentimentText <- ifelse(ddita$sentiment > 0, "Positive", ifelse(ddita$sentiment < 0, "Negative", "Neutral"))
    max_title_length <- 30
    ddita$title2 <- sapply(ddita$title, function(title) truncate_title(title, max_title_length))
    ddita =ddita %>% arrange(publishedAt)  
    return(ddita)
  } 
  if (y !='All' && text != ''){
    
    selected_id <- data$id[data$name == y] 
    ddita=py$news201(text ,selected_id ) 
    ddita$publishedAt=as.POSIXct(ddita$publishedAt, format = "%Y-%m-%dT%H:%M") 
    ddita$SentimentText <- ifelse(ddita$sentiment > 0, "Positive", ifelse(ddita$sentiment < 0, "Negative", "Neutral"))
    max_title_length <- 30
    ddita$title2 <- sapply(ddita$title, function(title) truncate_title(title, max_title_length))
    ddita =ddita %>% arrange(publishedAt) 
    return(ddita)
    
  }
  if (y=='All' && (text == '' | is.na(text))){
    ddita=py$news201('Stock Market News') 
    ddita$publishedAt=as.POSIXct(ddita$publishedAt, format = "%Y-%m-%dT%H:%M") 
    ddita$SentimentText <- ifelse(ddita$sentiment > 0, "Positive", ifelse(ddita$sentiment < 0, "Negative", "Neutral"))
    max_title_length <- 30
    ddita$title2 <- sapply(ddita$title, function(title) truncate_title(title, max_title_length))
    ddita =ddita %>% arrange(publishedAt)  
    return(ddita)
  } 
  if (y !='All' && (text == '' | is.na(text))){
    
    selected_id <- data$id[data$name == y] 
    ddita=py$news201('Stock Market News' ,selected_id ) 
    ddita$publishedAt=as.POSIXct(ddita$publishedAt, format = "%Y-%m-%dT%H:%M") 
    ddita$SentimentText <- ifelse(ddita$sentiment > 0, "Positive", ifelse(ddita$sentiment < 0, "Negative", "Neutral"))
    max_title_length <- 30
    ddita$title2 <- sapply(ddita$title, function(title) truncate_title(title, max_title_length))
    ddita =ddita %>% arrange(publishedAt) 
    return(ddita)
    
  }
  
}







