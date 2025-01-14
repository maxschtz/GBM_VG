## please adjust
setwd("~/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/Parameters")

## input file paths - please adjust
input_file_path <- "/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/Data/DAX40_Data.csv"

#### load stock data ####
DAX40_data <- read.csv(input_file_path)

#### S0 ####
  S0_row <- DAX40_data[DAX40_data[, 1] == as.Date("2024-05-31"), ]
  ## stock names
  stock_names <- colnames(DAX40_data)[2:41]
  ## stcok values
  S0_values <- as.numeric(S0_row[, 2:41])
  ## df S0
  S0_data <- data.frame(S0 = S0_values, row.names = stock_names)
  ## save
  write.csv(S0_data, file = "S0_data.csv", row.names = TRUE)
  
#### Stock price of the first week (5 trading days) ####
  week_1_row <- DAX40_data[DAX40_data[, 1] == as.Date("2024-06-07"), ]
  ## stock values
  week_1_values <- as.numeric(week_1_row[, 2:41])
  ## df
  week_1_data <- data.frame(week_1 = week_1_values, row.names = stock_names)
  ## save
  write.csv(week_1_data, file = "week_1_data.csv", row.names = TRUE)

  #### Stock price of the second week (10 trading days) ####
  week_2_row <- DAX40_data[DAX40_data[, 1] == as.Date("2024-06-14"), ]
  ## stock values
  week_2_values <- as.numeric(week_2_row[, 2:41])
  ## df
  week_2_data <- data.frame(week_2 = week_2_values, row.names = stock_names)
  ## save
  write.csv(week_2_data, file = "week_2_data.csv", row.names = TRUE)
  
#### Stock price for the first month (20 trading days) ####
  month_1_row <- DAX40_data[DAX40_data[, 1] == as.Date("2024-06-28"), ]
  ## stock values
  month_1_values <- as.numeric(month_1_row[, 2:41])
  ## df
  month_1_data <- data.frame(month_1 = month_1_values, row.names = stock_names)
  ## save
  write.csv(month_1_data, file = "month_1_data.csv", row.names = TRUE)
  
#### Stock price for 6 months (130 trading days) ####
  month_6_row <- DAX40_data[DAX40_data[, 1] == as.Date("2024-11-29"), ]
  ## stock values
  month_6_values <- as.numeric(month_6_row[, 2:41])
  ## df
  month_6_data <- data.frame(month_6 = month_6_values, row.names = stock_names)
  ## save
  write.csv(month_6_data, file = "month_6_data.csv", row.names = TRUE)
  