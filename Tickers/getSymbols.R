library(quantmod) 
library(tidyquant)
library(reticulate)
library(alphavantager)
library(dplyr)




# Set your Alpha Vantage API key
#api_key <- av_api_key('')

# Set the stock symbol for which you want to retrieve dividend data
#symbol <- "AAPL"  # Replace with your desired stock symbol

# Make the API call to retrieve dividend data
#dividend_data <- av_get(symbol = symbol, av_fun = "DIVIDEND", apikey = api_key)

# Print the dividend data
#print(dividend_data)




library(quantmod)

# Set the stock symbol for which you want to retrieve dividend data
#symbol <- "AAPL"  # Replace with your desired stock symbol

# Fetch dividend data from Yahoo Finance
#dividend_data <- getDividends("AAPL" , from = "2000-01-01", to = "2020-12-31")  

dvd=as.data.frame(dividend_data)

syso=c('KO','AAPL')
dota=c()
for (i in syso){
  data1<- getDividends(i, from ="2000-01-01", to = "2020-12-31") 
  col=data1[,1]
  dota<-cbind(dota,col)
}
syso

data=read.csv('tickers_symbols.csv') 

syso<-sample(data$symbol ,4)
syso


data1 <- getDividends("KO" ,from ="2020-01-01", to = "2020-12-31")

dd<-create_data(syso ,from ="2020-01-01", to = "2020-12-31" )
dvd<-create_data_dvd(syso ,from ="2020-01-01", to = "2020-12-31" )
dd[,2:length(dd)]<- lapply(dd[,2:length(dd)], as.numeric)

dvd[is.na(dvd)] <- 0 
#dvd<- apply(dvd,2, cumsum) 
dvd=as.data.frame(dvd) 
rownames(dd)<-dd$date1  
returns_price <- Return.calculate(dd[,2:length(dd)])   
#returns$datos=rownames(returns)
dvd$datos=rownames(dvd) 
dd$datos=dd$date1
returns_dd_price <- left_join(dvd, dd, by = "datos")   

returns1<-returns1[2:dim(returns1)[1],] 
returns1[is.na(returns1)] <- 0  
returns1[,colnames(dvd)]<- apply(returns1[,colnames(dvd)],2, cumsum)  

returns_price$datos<-rownames(returns_price) 
returns_price= returns_price %>% filter(datos %in% dvd$datos) 


returns1 <- left_join( dvd, dd, by = "datos") 
returns <- left_join( dvd, returns, by = "datos") 

dvd_r=c()
for (st in syso) { 
  print(paste0(st,'.div')) 
  returns1[,st]=returns1[,paste0(st,'.div')]/returns1[,st] 
  returns1[,st]=returns1[,st]+returns_price[,st]  
  
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

#WFC        VRSK          MRK        ALGN


df=returns_STK(syso ,prices_data=dd , dvd_data=dvd)   

sum(df$WFC)/8

df
portfolio <- data.frame(
  Stock = syso,
  Return=unlist(unname(as.list(colMeans(df)))) ,
  Risk = unlist(unname(as.list(apply(df, 2, sd)))) 
) 

#colnames(dota)<-c('KO','AAPL' ,'PG') 

#syso

#data1<- getDividends('KO', from ="2020-01-01", to = "2020-12-31") 
#ata1<-data.frame(data1) 
#data1
#############
syso

col=data1[,1]

col

# Print the dividend data
#dividend_data


#sum(dividend_data$AAPL.div)


wts <- runif(n = length(syso) , min = 0 , max=1) 
wts 

AAPL <- tq_get("KO") 
AAPL


AAPL$close
all_symbols
syso<-sample(data$symbol ,4)
syso



 





portfolio <- data.frame(
  Stock = syso,
  Return=unlist(unname(as.list(colMeans(dd[,syso])))) ,
  Risk = unlist(unname(as.list(apply(dd[,syso], 2, sd)))) 
) 








# Load the required libraries
library(PortfolioAnalytics)
library(ROI)

# Create the portfolio dataframe
# Convert returns and risks to matrices
returns <- as.matrix(portfolio$Return)
risks <- as.matrix(portfolio$Risk)

# Define the covariance matrix
cov_matrix <- cov(dd[,syso]) 





dim(dd)

returns<-as.data.frame(apply(dd[,syso],2, comp_returns))   
datos<-dd[2:dim(dd)[1],1] 
rownames(returns)<-datos
cov_matrix <-cov(returns)

sd(returns$BLMN)

library(PortfolioAnalytics) 




# Create the portfolio object
portfolio <- portfolio.spec(assets = colnames(cov_matrix)) 

# Set the portfolio's assets and constraints
portfolio <- add.constraint(portfolio, type = "weight_sum", min_sum = 1, max_sum = 1)
portfolio <- add.constraint(portfolio, type = "box", min = 0, max = 1)

# Add the risk objective
portfolio <- add.objective(portfolio, type = "risk", name = "StdDev")

# Optimize the portfolio
opt_portfolio <- optimize.portfolio(returns, portfolio, cov_mat = cov_matrix)

# Print the optimized portfolio weights
print(opt_portfolio)

summary(opt_portfolio)

returns<-as.data.frame(apply(dd[,syso],2, comp_returns))  
datos<-dd[2:dim(dd)[1],1] 
rownames(returns)<-datos

as.data.frame(opt_portfolio$weights) 




#ptimize_port(returns , min_return = 0.40 , max_risk = 5) 



library(quantmod) 

dividend_returns <- Return.calculate(dividend_df) 





doti[,2:5]<- lapply(doti[,2:5], as.numeric)
rownames(doti)<-doti$date1
returns <- Return.calculate(doti[,2:5])  


