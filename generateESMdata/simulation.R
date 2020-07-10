#!/usr/bin/env Rscript
iter <- commandArgs(trailingOnly=TRUE)
print(iter)
iter <- as.numeric(iter)

# ----------------------------------------------------------------------
# --------- Generate Data: ESM Datasets with matched length  -----------
# ----------------------------------------------------------------------

source("fun_DataGen.R")

set.seed(iter)

# Two stable fix points
no_minutes <- 14 * 24 * 60 #* 900 # 12 = 2 * 6, to get six month of data

data <- genData(time = no_minutes, 
                stress = 1, 
                timestep = 0.01, 
                noiseSD = 4.5,
                noise = TRUE, 
                pbar = FALSE)

nrow(data)

# Aggregate 1: Mean
n <- nrow(data)
int90 <- n / 9000
ind_int90 <- rep(1:int90, each=9000)
224 * 9000 == 14 * 24 * 60 * 100 # Two weeks of data

data_agg <- matrix(NA, int90, 5) # Storage
for(j in 1:4) data_agg[, j+1] <- by(data[, j+1], ind_int90, mean)

saveRDS(data_agg, file=paste0("data_agg_mean_", iter,".RDS"))

# Aggregate 2: Snapshot  
data_agg2 <- data[round(seq(1, nrow(data), length = int90)), ]
saveRDS(data_agg2, file=paste0("data_agg_snapshot_", iter,".RDS"))


















