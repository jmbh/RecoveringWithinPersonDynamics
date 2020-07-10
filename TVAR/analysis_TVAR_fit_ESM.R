# ----------------------------------------------------------------------
# --------- Load and Prepare Data  -------------------------------------
# ----------------------------------------------------------------------

# ----- Load library ----
library(tsDyn)

# ----- Load Data ----
data <- readRDS("files/data_ESM.RDS")


# ----------------------------------------------------------------------
# --------- Fit Threshold VAR Model  -----------------------------------
# ----------------------------------------------------------------------

# mTh =1 specifies that x_1 should be used as the thresholding variable
# Note that running the TVAR function on this data took 9.66 hrs to run
tvar.est <- TVAR(data[,2:5],lag=1,mTh=1) 
saveRDS(tvar.est, file="tvar_est_ESM.RDS")

# first <- seq(0,nrow(data), by = 224) + 1
# first <- first[-length(first)]
# data <-  data[-first,]
# 
# tvar.est <- TVAR(data[,2:5],lag=1,mTh=1, ngrid = 20, around = 2.796) 
# 
# 
# saveRDS(tvar.est, file="TVAR/tvar_est_ESM_check.RDS")