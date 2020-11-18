# ----------------------------------------------------------------------
# --------- Load and Prepare Data  -------------------------------------
# ----------------------------------------------------------------------

# ----- Load data -----

data <- readRDS(file = "files/data.RDS")

# ----- Load library ----
library(tsDyn)

# ----------------------------------------------------------------------
# --------- Fit Threshold VAR Model  -----------------------------------
# ----------------------------------------------------------------------

# mTh =1 specifies that x_1 should be used as the thresholding variable
# Note that running the TVAR function on this data took 9.66 hrs to run
tvar.est <- TVAR(data[,2:5],lag=1,mTh=1) 
saveRDS(tvar.est, file="TVAR/tvar_est.RDS")