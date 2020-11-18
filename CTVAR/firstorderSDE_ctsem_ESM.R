# ryanoisin@gmail.com; November 2020

# The code in this file estimates the CT-VAR(1) on the ESM data
# As found in Appendix B

# ----------------------------------------------------------------------
# --------- Source helper functions ------------------------------------
# ----------------------------------------------------------------------

library(ctsem)

# ----------------------------------------------------------------------
# --------- Load and Prepare Data  -------------------------------------
# ----------------------------------------------------------------------

# Load data and "parellize" - instead of concatenating the seperate
# simulated time series, assign each a unique participant ID


# Get individual files
 simResDir <- "generateESMdata/output_snapshot/"
 v_files <- list.files(simResDir)

# ----- Process Data into format for statistical models -----

l_data <- list() # storage

for(i in 1:900) {

  data_i <- readRDS(paste0(simResDir, v_files[i]))
  n <- nrow(data_i)
  time <- seq(0,((nrow(data_i)*9)-1), by=9)
  time[-nrow(data_i)]

  x1 <- data_i[, 2]
  x2 <- data_i[, 3]
  x3 <- data_i[, 4]
  x4 <- data_i[, 5]
  D_i <- as.data.frame(cbind(i,time,x1, x2, x3, x4))

  l_data[[i]] <- D_i

} # end: for

data_esm_snap <- do.call(rbind, l_data)

saveRDS(data_esm_snap,"generateESMdata/files/data_ESM_pp.RDS")
data <- readRDS("generateESMdata/files/data_ESM_pp.RDS")
colnames(data)[1] <- "id"

# ----------------------------------------------------------------------
# ------------------ Set-up Model  -------------------------------------
# ----------------------------------------------------------------------

d <- 4

DRIFT = matrix(NA,d,d)
DIFFUSION = matrix(0, d,d)
for(j in 1:ncol(DRIFT)){
  for(i in 1:nrow(DRIFT)){
    DRIFT[i,j] = paste0('drift_',i,j)
    if(j==i) DIFFUSION[i,j] <- paste0('diffusion',i) 
    if(j < i) DIFFUSION[i,j] <- paste0('diffusion',i,j)
  }
}


#m odel specification
m <- ctModel( #free defaults for T0MEANS,T0VAR
  type='stanct', 
  LAMBDA=diag(d), 
  DIFFUSION=DIFFUSION,
  manifestNames=paste0('x',1:d),
  DRIFT=DRIFT,
  MANIFESTVAR=diag(0,d),
  MANIFESTMEANS=matrix(0,d),
  CINT=matrix(paste0('cint',1:d),d))

ctModelLatex(m,textsize = 'tiny')

m$pars$indvarying <- FALSE # no individual variation because same subject

ncores <- 3

# ----------------------------------------------------------------------
# ------------------ Estimate Model  -----------------------------------
# ----------------------------------------------------------------------

start <- Sys.time()

f <- ctStanFit(datalong = data,ctstanmodel = m,
               optimize=TRUE, nopriors=TRUE,cores=ncores,
               nlcontrol=list(maxtimestep = 1), #maybe needs to be even smaller for subsampling type approaches... slows down though
               optimcontrol=list(finishsamples=100
                                 # ,stochastic=TRUE,plotsgd=TRUE #slower but sometimes more robust stochastic optimizer
               ), 
               verbose=1)

end <- Sys.time()

end - start

saveRDS(f,"CTVAR/linearSDE_ESM.RDS")
