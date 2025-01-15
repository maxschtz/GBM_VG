# wd please adjust
setwd("~/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/VG simulations final/VG selcetion")
# packages
library(ggplot2)
# documents
DAX_40_log_retruns <- read.csv("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/Parameters/DAX40_log_returns.csv")

# clean data
DAX_40_log_retruns <- DAX_40_log_retruns[, -c(1, 2)]

## mean and sd
log_return_stats <- data.frame(
  Stock = colnames(DAX_40_log_retruns),
  Mean = sapply(DAX_40_log_retruns, mean, na.rm = TRUE),
  SD = sapply(DAX_40_log_retruns, sd, na.rm = TRUE)
)


#### plot histograms ####
for (stock in colnames(DAX_40_log_retruns)) {
  stock_data <- DAX_40_log_retruns[[stock]]
  # hist and plottinh
  hist_plot <- ggplot(data.frame(Returns = stock_data), aes(x = Returns)) +
    geom_histogram(binwidth = 0.005, fill = "darkblue", color = "blue", alpha = 0.9) +
    theme_minimal() +
    labs(
      title = paste("Histogram of log returns for", stock),
      x = "Log Returns",
      y = "Frequency"
    )
  
  # Save the plot
  ggsave(
    filename = paste0("Histogram_", stock, ".png"),
    plot = hist_plot,
    width = 8,
    height = 6
  )
}



#### ks test ####
# storage
ks_test_results <- list()


for (i in 1:nrow(log_return_stats)) {
  stock <- log_return_stats$Stock[i]
  mean_stock <- log_return_stats$Mean[i]
  sd_stock <- log_return_stats$SD[i]
  # Empirical distribution
  stock_data <- DAX_40_log_retruns[[stock]]
  # applying ks
  ks_result <- ks.test(stock_data, "pnorm", mean = mean_stock, sd = sd_stock)
  ks_test_results[[stock]] <- list(
    Statistic = ks_result$statistic,
    P_Value = ks_result$p.value
  )
}


# df
ks_results <- data.frame(
  Stock = names(ks_test_results),
  KS_Statistic = sapply(ks_test_results, function(x) x$Statistic),
  P_Value = sapply(ks_test_results, function(x) x$P_Value)
)

## filter for non normal distributed
non_normal_stocks <- subset(ks_results, P_Value < 0.05)

## save
write.csv(non_normal_stocks, "ks_nonnormal_stocks.csv", row.names = FALSE)
