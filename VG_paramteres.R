## packages
  library(VarianceGamma)
## please adjust
setwd("~/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/Parameters")
log_returns <- read.csv("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/Parameters/DAX40_log_returns.csv")
log_returns <- log_returns[, -c(1, 2)]
log_returns <- log_returns 
  
# df
vg_params <- data.frame(
  Stock = colnames(log_returns),
  vgC = rep(NA, ncol(log_returns)),
  Sigma = rep(NA, ncol(log_returns)),
  Theta = rep(NA, ncol(log_returns)),
  Nu = rep(NA, ncol(log_returns)),
  Likelihood = rep(NA, ncol(log_returns)),
  Convergence = rep(NA, ncol(log_returns))
)

##loop
for (i in 1:ncol(log_returns)) {
  stock_name <- colnames(log_returns)[i]
  stock_returns <- as.numeric(log_returns[[i]])  
  #fit the VG model
  vg_fit <- try(
    vgFit(stock_returns, method = "Nelder-Mead", plots = FALSE, controlNM = list(maxit = 5000)), 
    silent = TRUE
  )
  
  #store
  if (!inherits(vg_fit, "try-error")) {
    vg_params$vgC[i] <- vg_fit[["param"]][["vgC"]]
    vg_params$Sigma[i] <- vg_fit[["param"]][["sigma"]]
    vg_params$Theta[i] <- vg_fit[["param"]][["theta"]]
    vg_params$Nu[i] <- vg_fit[["param"]][["nu"]]
    vg_params$Likelihood[i] <- vg_fit$maxLik  
    vg_params$Convergence[i] <- vg_fit$conv  
  } else {
    vg_params$Convergence[i] <- -1  
  }
}

## save
write.csv(vg_params, file = "DAX40_VG_parameters.csv", row.names = FALSE)
