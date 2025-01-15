# set wd - adjust
setwd("~/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/Parameters")

# load data - adjust
vg_params <- read.csv("/Users/max/Documents/Maasticht/Master Thesis/R_Masterthesis_VG/Parameters/DAX40_VG_parameters.csv")

## df
ws_results <- data.frame(
  Stock = vg_params$Stock,
  ws = NA
)

for (i in 1:nrow(vg_params)) {
  sigma <- vg_params$Sigma[i]
  theta <- vg_params$Theta[i]
  nu <- vg_params$Nu[i]

  ws_results$ws[i] <- (1 / nu) * log(1 - theta * nu - (sigma^2 * nu) / 2)
}

write.csv(ws_results, file = "VG_DAX40_ws.csv", row.names = FALSE)