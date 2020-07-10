
# ----- Load data -----

simResDir <- "generateESMdata/output/"
v_files <- list.files(simResDir)

# ----- Process Data into format for statistical models -----

l_data <- list() # storage

for(i in 1:900) {
  
  data_i <- readRDS(paste0(simResDir, v_files[i]))
  n <- nrow(data_i)
  

  x1 <- data_i[, 2]
  x2 <- data_i[, 3]
  x3 <- data_i[, 4]
  x4 <- data_i[, 5]
  D_i <- as.data.frame(cbind(x1, x2, x3, x4))
  
  l_data[[i]] <- D_i
  
} # end: for

data_esm_snap <- do.call(rbind, l_data)
time <- seq(0,((nrow(data_esm_snap)*90)-1), by=90)
data_esm_snap <- cbind(time,data_esm_snap)

saveRDS(data_esm_snap,"files/data_ESM.RDS")
