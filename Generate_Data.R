# jonashaslbeck@gmail.com; July 2019

# This file generates the 2-week "ideal" data from the true bistable
# system which we use throughout Section 3

source("fun_DataGen.R")

# ----------------------------------------------------------------------
# --------- Generate Data  ---------------------------------------------
# ----------------------------------------------------------------------

no_minutes <- 14 * 24 * 60 # 2 tweeks, 24h, 60 min

set.seed(3)
data <- genData(time = no_minutes, 
                 stress = 1, 
                 timestep = 0.01, 
                 noiseSD = 4.5,
                 noise = TRUE)

# Thinned to 1 min measurements
data_thinned <- data[seq(1, no_minutes*100, length = no_minutes*10), ]
saveRDS(data_thinned, file = "files/data.RDS")




