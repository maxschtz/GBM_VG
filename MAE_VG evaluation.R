## setwd - please change
setwd("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/Evaluation")
## packags
library(tidyverse)

#### load simulations and data - please change file path ####
relevant_stocks <- read.csv("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/VG simulations final/VG selcetion/ks_nonnormal_stocks.csv")
relevant_stocks <- relevant_stocks[,1]
load("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/VG simulations final/VG_simulations.RData")
week_1_data <- read.csv("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/Parameters/week_1_data.csv")
week_1_data <- week_1_data %>% arrange(week_1_data[, 1])
week_1_data <- week_1_data[week_1_data$X %in% relevant_stocks, ]
week_2_data <- read.csv("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/Parameters/week_2_data.csv")
week_2_data <- week_2_data[week_2_data$X %in% relevant_stocks, ]
week_2_data <- week_2_data %>% arrange(week_2_data[, 1])
month_1_data <- read.csv("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/Parameters/month_1_data.csv")
month_1_data <- month_1_data %>% arrange(month_1_data[, 1])
month_1_data<- month_1_data[month_1_data$X %in% relevant_stocks, ]
month_6_data <- read.csv("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/Parameters/month_6_data.csv")
month_6_data <- month_6_data %>% arrange(month_6_data[, 1])
month_6_data<- month_6_data[month_6_data$X %in% relevant_stocks, ]

#### one week MAE ####
week_1_MAEs <- data.frame(stock = character(), MAE = numeric(), stringsAsFactors = FALSE)
for (i in 1:nrow(week_1_data)) {
  stock_name <- week_1_data[i, 1]
  actual_price <- week_1_data[i, 2]
  
  stock_simulation <- vg_simulations[[i]]
  simulated_prices_day_5 <- stock_simulation %>%
    filter(trading_days == 5) %>%
    pull(price)
  #MAE
  absolute_errors_day_5 <- abs(actual_price - simulated_prices_day_5)
  mae <- mean(absolute_errors_day_5)
  
  week_1_MAEs <- rbind(week_1_MAEs, data.frame(stock = stock_name, MAE = mae))
}
write.csv(week_1_MAEs, file = "VG_week_1_MAEs.csv", row.names = FALSE)

#### two weeks MAE ####
week_2_MAEs <- data.frame(stock = character(), MAE = numeric(), stringsAsFactors = FALSE)
# loop through each stock
for (i in 1:nrow(week_2_data)) {
  stock_name <- week_2_data[i, 1]
  actual_price <- week_2_data[i, 2]
  
  stock_simulation <- vg_simulations[[i]]
  simulated_prices_day_10 <- stock_simulation %>%
    filter(trading_days == 10) %>%
    pull(price)
  #MAE
  absolute_errors_day_10 <- abs(actual_price - simulated_prices_day_10)
  mae <- mean(absolute_errors_day_10)
  
  week_2_MAEs <- rbind(week_2_MAEs, data.frame(stock = stock_name, MAE = mae))
}
write.csv(week_2_MAEs, file = "VG_week_2_MAEs.csv", row.names = FALSE)

#### 1 month MAE ####
month_1_MAEs <- data.frame(stock = character(), MAE = numeric(), stringsAsFactors = FALSE)
# loop through each stock
for (i in 1:nrow(month_1_data)) {
  stock_name <- month_1_data[i, 1]
  actual_price <- month_1_data[i, 2]
  
  stock_simulation <- vg_simulations[[i]]
  simulated_prices_month_1 <- stock_simulation %>%
    filter(trading_days == 20) %>%
    pull(price)
  #MAE
  absolute_errors_month_1 <- abs(actual_price - simulated_prices_month_1)
  mae <- mean(absolute_errors_month_1)
  
  month_1_MAEs <- rbind(month_1_MAEs, data.frame(stock = stock_name, MAE = mae))
}
write.csv(month_1_MAEs, file = "VG_month_1_MAEs.csv", row.names = FALSE)

#### six months MAE ####
month_6_MAEs <- data.frame(stock = character(), MAE = numeric(), stringsAsFactors = FALSE)
# loop through each stock
for (i in 1:nrow(month_6_data)) {
  stock_name <- month_6_data[i, 1]
  actual_price <- month_6_data[i, 2]
  
  stock_simulation <- vg_simulations[[i]]
  simulated_prices_month_6 <- stock_simulation %>%
    filter(trading_days == 130) %>%
    pull(price)
  #MAE
  absolute_errors_month_6 <- abs(actual_price - simulated_prices_month_6)
  mae <- mean(absolute_errors_month_6)
  
  month_6_MAEs <- rbind(month_6_MAEs, data.frame(stock = stock_name, MAE = mae))
}
write.csv(month_6_MAEs, file = "VG_month_6_MAEs.csv", row.names = FALSE)
