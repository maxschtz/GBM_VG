## please adjust
setwd("~/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/Parameters")

## input file paths - please adjust
input_file_path <- "/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/Data/DAX40_Data.csv"

## load stock data
DAX40_data <- read.csv(input_file_path)

## filter for first 6 months - including one prior day to calculate returns for june 1st
DAX40_data <- subset(DAX40_data, Date >= "2023-05-31" & Date <= "2024-05-31")

#### log returns ####
## df
log_returns <- data.frame(Date = DAX40_data$Date[-1]) 

for (i in 2:ncol(DAX40_data)) {
  stock_prices <- DAX40_data[[i]]  
  log_return <- diff(log(stock_prices))  
  log_returns[[colnames(DAX40_data)[i]]] <- log_return  
}


## save
write.csv(log_returns, file = "DAX40_log_returns.csv", row.names = TRUE)

####  drift ####
drift_values <- sapply(log_returns[, -1], mean, na.rm = TRUE)  #excluding the Date column
#df
drift_daily <- data.frame(
  Stock = colnames(log_returns)[-1],
  drift = drift_values
)

write.csv(drift_daily, file = "DAX40_drift.csv", row.names = FALSE)


#### volatility ####
volatility_values <- sapply(log_returns[, -1], sd, na.rm = TRUE) 
#df
volatility_daily <- data.frame(
  Stock = colnames(log_returns)[-1],
  Volatility = volatility_values
)

write.csv(volatility_daily, file = "DAX40_volatility.csv", row.names = FALSE)