# ryanoisin@gmail.com; November 2020

# The code in this file estimates the CT-VAR(1) on the ideal data
# As found in Appendix B

# ----------------------------------------------------------------------
# --------- Source helper functions ------------------------------------
# ----------------------------------------------------------------------

library(ctsem)

# ----------------------------------------------------------------------
# --------- Load and Prepare Data  -------------------------------------
# ----------------------------------------------------------------------
data <- readRDS("files/data.RDS")


#  Function to "parallelize" model estimation

data_parallel <- function(data, n, t){
  
  samplestarts <- floor(nrow(data)/n) * c(0:(n-1))+1
  if(t > nrow(data)/n){ tsamp <- nrow(data)/n } else { tsamp <- t}
  mat <- sapply(samplestarts,function(x){ seq(x,(x+t-1))})
  
  time <- data[mat[,1],1]
  
  datal <- list()
  for(i in 1:n){
    datal[[i]] <- cbind(i,time,data[mat[,i],2:5])
  }
  
  df <- do.call("rbind", datal)
  df
}

# Split data into 6 "participants"
data_par <- data_parallel(data, n = 6, t = 33600)
data_par[,2] <- round(data_par[,2],2)

colnames(data_par) = c("id", "time", "x1", "x2","x3", "x4")

nobs <- nrow(data)

# ----------------------------------------------------------------------
# ------------------ Set-up Model  -------------------------------------
# ----------------------------------------------------------------------

# Make model Matrices
d <- 4
DRIFT = "auto"
DIFFUSION = "auto"

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

ncores <- 2

# ----------------------------------------------------------------------
# ------------------ Estimate Model  -----------------------------------
# ----------------------------------------------------------------------

start <- Sys.time()

f <- ctStanFit(datalong = data_par,ctstanmodel = m,
          optimize=TRUE, nopriors=TRUE,cores=ncores,
          nlcontrol=list(maxtimestep = 1), #maybe needs to be even smaller for subsampling type approaches... slows down though
          optimcontrol=list(finishsamples=100
                            # ,stochastic=TRUE,plotsgd=TRUE #slower but sometimes more robust stochastic optimizer
          ), 
          verbose=1)
end <- Sys.time()
  
print(end - start)
  
saveRDS(f, file = "CTVAR/linearSDE_ideal.RDS")
