

library(shinyjs)
library(dplyr)

data=read.csv('tickers_symbols.csv') 

syso<-sample(data$symbol ,3)



#data1 <- getDividends("KO" ,from ="2020-01-01", to = "2020-12-31")

dd<-create_data(syso ,from ="2020-01-01", to = "2022-12-31" )
dvd<-create_data_dvd(syso ,from ="2020-01-01", to = "2020-12-31" )
dd[,2:length(dd)]<- lapply(dd[,2:length(dd)], as.numeric)



Sys.Date()




num_portfolios <- 100  
rownames(dd)<-dd$date1
dvd=Return.calculate(dd[,2:length(dd)])  

dvd<-dvd[2:nrow(dvd),] 


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


to=generate_portfolios(dvd) 

colnames(t)[1:3]<-syso

min_var <-t[which.min(t$Risk),] 
max_sr <- t[which.max(t$SharpeRatio),] 



p<-min_var %>%
  gather(AAPL:XOM, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent)   

ggplotly(p) 




#num_port <- 5000

# Creating a matrix to store the weights

all_wts <- matrix(nrow = num_port,
                  ncol = length(syso))

# Creating an empty vector to store
# Portfolio returns

port_returns <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Standard deviation

port_risk <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Sharpe Ratio

sharpe_ratio <- vector('numeric', length = num_port)


for (i in seq_along(port_returns)) {
  
  wts <- runif(length(syso))
  wts <- wts/sum(wts)
  
  # Storing weight in the matrix
  all_wts[i,] <- wts
  
  # Portfolio returns
  
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^252) - 1
  
  # Storing Portfolio Returns values
  port_returns[i] <- port_ret
  
  
  # Creating and storing portfolio risk
  port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
  port_risk[i] <- port_sd
  
  # Creating and storing Portfolio Sharpe Ratios
  # Assuming 0% Risk free rate
  
  sr <- port_ret/port_sd
  sharpe_ratio[i] <- sr
  
} 

portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)




max_sharpe_index <- which.max(portfolio_data$SharpeRatio)
min_variance_index <- which.min(portfolio_data$Risk) 

portfolio_data<-to

# Plot all portfolios
p<-ggplot(portfolio_data, aes(x = Risk, y = Return , color=SharpeRatio)) +
  geom_point() + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  geom_point(aes(x = Risk[max_sharpe_index], y = Return[max_sharpe_index]),
             color = "red", size = 2) + 
  geom_point(aes(x = Risk[min_variance_index], y = Return[min_variance_index]),
             color = "green", size = 2)+ 
  labs(x = "Risk", y = "Return") +theme_bw()
  
   
  
ggplotly(p)



library(ggplot2)

# Create a sample data frame
portfolio_data <- data.frame(
  Risk = c(0.1, 0.2, 0.3, 0.4, 0.5),
  Return = c(0.05, 0.1, 0.15, 0.2, 0.25),
  SharpeRatio = c(0.8, 0.9, 1.0, 1.1, 1.2)
)
max_sharpe_index <- 3  # Index of the maximum Sharpe ratio
min_variance_index <- 5  # Index of the minimum variance

# Create the plot
p <- ggplot(portfolio_data, aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  geom_point(aes(x = Risk[max_sharpe_index], y = Return[max_sharpe_index]),
             color = "blue", size = 2) +
  geom_point(aes(x = Risk[min_variance_index], y = Return[min_variance_index]),
             color = "green", size = 2) +
  labs(x = "Risk", y = "Return") +
  theme_bw()

# Add legend for blue and green points
p <- p + guides(
  color = guide_legend(
    override.aes = list(
      shape = c(16, 16),  # Use solid circle (16) as the point shape for blue and green
      color = c("blue", "green"),  # Use blue and green colors
      size = c(2, 2)  # Use size 2 for blue and green points
    ),
    title = "Legend Title"  # Specify the legend title
  )
)
p



data1<- tq_get('S&P 500',  ,get = "stock.prices") 







