# # Fit data generating model to ESM time series
# 
# 
# # 1. Get data in appropriate format
# 
# 
# simResDir <- "generateESMdata/output_snapshot/"
# v_files <- list.files(simResDir)
# 
# # ----- Process Data into format for statistical models -----
# 
# l_data <- list() # storage
# 
# for(i in 1:900) {
# 
#   data_i <- readRDS(paste0(simResDir, v_files[i]))
#   n <- nrow(data_i)
#   time <- seq(0,((nrow(data_i)*9)-1), by=9)
#   time[-nrow(data_i)]
# 
#   x1 <- data_i[, 2]
#   x2 <- data_i[, 3]
#   x3 <- data_i[, 4]
#   x4 <- data_i[, 5]
#   D_i <- as.data.frame(cbind(i,time,x1, x2, x3, x4))
# 
#   l_data[[i]] <- D_i
# 
# } # end: for
# 
# data_esm_snap <- do.call(rbind, l_data)
# 
# saveRDS(data_esm_snap,"generateESMdata/files/data_ESM_pp.RDS")

# Load data
library(ctsem)
data <- readRDS("generateESMdata/files/data_ESM_pp.RDS")





# --------------Set up model ------------------------

d <- 4

#setup some model elements 
DRIFT = matrix(NA,d,d) #function jacobian
PARS = matrix(NA,d*d,2) #additional parameter container
DIFFUSION=matrix(0,d,d)
parsrow <- 0
fullnonlinear <- FALSE #set true for intercept parameters for off diags of C matrix
for(j in 1:ncol(DRIFT)){
  for(i in 1:nrow(DRIFT)){
    parsrow <- parsrow + 1
    DRIFT[i,j] <- paste0('PARS[',parsrow,',',1,'] + PARS[',parsrow,',',2,'] * state[',i,']')
    PARS[parsrow,2] <- paste0('dr_mult_',i,'_',j)
    PARS[parsrow,1] <- ifelse(j==ifelse(fullnonlinear,j,i), paste0('dr_int_',i,'_',j), 0) #intercepts only for diagonal
    if(j==i) DIFFUSION[i,j] <- paste0('diffusion',i) 
  }
}



#model spec
m <- ctModel( #free defaults for T0MEANS,T0VAR
  type='stanct', 
  LAMBDA=diag(d), 
  DIFFUSION=DIFFUSION,
  manifestNames=paste0('x',1:d),
  DRIFT=DRIFT,
  PARS=PARS,
  MANIFESTVAR=diag(0,d),
  MANIFESTMEANS=matrix(0,d),
  CINT=matrix(paste0('cint',1:d),d))

ctModelLatex(m,textsize = 'tiny')

m$pars$indvarying<-FALSE #no individual variation because same subject

#make small dataset
ncores <- 4

colnames(data)[1] <- "id"

# subdata <- subset(data, id < 901)
subdata <- data

start <- Sys.time()

# f <- ctStanFit(datalong = subdata,ctstanmodel = m,
#                optimize=TRUE, nopriors=TRUE,cores=ncores,
#                nlcontrol=list(maxtimestep = 1), #maybe needs to be even smaller for subsampling type approaches... slows down though
#                optimcontrol=list(finishsamples=100
#                                  # ,stochastic=TRUE,plotsgd=TRUE #slower but sometimes more robust stochastic optimizer
#                ), 
#                verbose=1)


f <- ctStanFit(datalong = subdata,ctstanmodel = m,
               optimize=TRUE, nopriors=TRUE,cores=ncores,
               nlcontrol=list(maxtimestep = 1), #maybe needs to be even smaller for subsampling type approaches... slows down though
               optimcontrol=list(finishsamples=100
                                 # ,stochastic=TRUE,plotsgd=TRUE #slower but sometimes more robust stochastic optimizer
               ),
               init = c(0.12656, 0.12939, 0.47872, 0.48035, -0.28152, -1.06319, -1.0935, 
                        -1.20591, 1.14867, 0.83981, 1.14987, 0.92804, -0.73813, 2.74002, 
                        -0.74683, -2.9386, 2.93678, -0.77746, 2.21799, -2.92187, 2.56865, 
                        -0.72721, -0.23864, -0.27361, 0.69118, -0.57284, -0.6023, -0.2309, 
                        0.01142, -0.53282, -0.66078, -0.3946, -0.23438, -0.51021, 0.04786, 
                        -0.59823, 0.44327, -0.3615, -0.56758, 0.63006, -0.15757, -0.34643),
               verbose=1)

end <- Sys.time()

end - start

saveRDS(f,"truemodel_ESM.RDS")

summary(f)

# reduce maxtimestep
            