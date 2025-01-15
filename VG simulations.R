set.seed(2024)
library(VarianceGamma)
library(tidyverse)
# wd please change
setwd("~/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/VG simulations final")

#### get data - please change paths ####
relevant_stocks <- read.csv("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/VG simulations final/VG selcetion/ks_nonnormal_stocks.csv")
relevant_stocks <- relevant_stocks[,1]
VG_paramters <- read.csv("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/Parameters/DAX40_VG_parameters.csv")
VG_paramters <- VG_paramters[order(VG_paramters$Stock), ]
VG_paramters <- VG_paramters[VG_paramters$Stock %in% relevant_stocks, ]
Sigma <- VG_paramters[, c(1, 3)]
Theta <- VG_paramters[, c(1, 4)]
Nu <- VG_paramters[, c(1, 5)]
m <- read.csv("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/Parameters/DAX40_drift.csv")
m <- m[order(m$Stock), ]
m <- m[m$Stock %in% relevant_stocks, ]
ws <- read.csv("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/Parameters/VG_DAX40_ws.csv")
ws <- ws[order(ws$Stock), ]
ws <- ws[ws$Stock %in% relevant_stocks, ]
S0s <- read.csv("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/Parameters/S0_data.csv")
S0s <- S0s[order(S0s$X), ]
S0s <- S0s[S0s$X %in% relevant_stocks, ]

#### simulations of VG ####
nsim <- 10000
t <- 130
dt <- 1

#list to store the simulations
vg_simulations <- list()

# loop
for (i in 1:nrow(VG_paramters)) {
  stock_name <- VG_paramters$Stock[i]
  sigma <- VG_paramters$Sigma[i]
  theta <- VG_paramters$Theta[i]
  nu <- VG_paramters$Nu[i]
  m_value <- m$drift[match(stock_name, m$Stock)]
  ws_value <- ws$ws[match(stock_name, ws$Stock)]
  S0 <- S0s$S0[match(stock_name, S0s$X)]
  
  # simulations using rvg
  vg_simulation <- matrix(ncol = nsim,nrow=t+1)
  for (sim in 1:nsim) {
    # m_value + ws_value as "drift term"
    returns <-rvg(t, param = c(m_value + ws_value, sigma, theta, nu))
    # compounding the returns
    vg_simulation[, sim] <- S0 * exp(cumsum(c(0, returns)))
  }
  
  #df
  vg_df <- as.data.frame(vg_simulation) %>%
    mutate(ix = 1:nrow(vg_simulation)) %>%
    filter(ix != 1) %>%
    mutate(ix = ix - 1) %>%
    rename(trading_days = ix) %>%
    pivot_longer(-trading_days, names_to = 'sim', values_to = 'price') %>%
    mutate(stock = stock_name)
  
  # Store dataframe
  vg_simulations[[i]] <- vg_df
}

# save
save(vg_simulations, file = "VG_simulations.RData")

#plot
for (i in 1:length(vg_simulations)) {
  stock_data <- vg_simulations[[i]]
  stock_name <- stock_data$stock[1]
  plot <- ggplot(stock_data, aes(x = trading_days, y = price, group = sim, color = sim)) +
    geom_line(alpha = 0.3) +
    theme_minimal() +
    theme(legend.position = 'none') +
    labs(
      title = paste("10000 VG simulations for:", stock_name),
      x = "Trading Days",
      y = "Simulated Price"
    )
  print(plot)
  ggsave(filename = paste0("VG_simulations_", stock_name, ".png"), plot = plot, width = 8, height = 6)
}

