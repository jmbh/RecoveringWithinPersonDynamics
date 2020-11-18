library(ctsem)

# FIT CT-VAR to ideal data
data <- readRDS("files/data.RDS")
d=4 

# set up model matrices
DRIFT = matrix(NA,d,d)
DIFFUSION = matrix(0, d,d)
for(j in 1:ncol(DRIFT)){
  for(i in 1:nrow(DRIFT)){
    DRIFT[i,j] = paste0('drift_',i,j)
    if(j==i) DIFFUSION[i,j] <- paste0('diffusion',i) 
    if(j < i) DIFFUSION[i,j] <- paste0('diffusion',i,j)
  }
}


#model spec
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

m$pars$indvarying<-FALSE #no individual variation because same subject

#make small dataset
ncores <- 6
nsampspercore <- 33600
sampleinterval <- 1 #50 seemed to
samplestarts <- floor(nrow(data)/ncores) * c(0:(ncores-1))+1
samplerows <- lapply(samplestarts,function(x) seq(x,length.out = nsampspercore, by = sampleinterval))
datasml<-do.call(rbind,lapply(1:ncores,function(x) cbind(x,data[samplerows[[x]],])))
colnames(datasml)[1] <- 'id'

#fit
f <- ctStanFit(datalong = datasml,ctstanmodel = m,
               optimize=TRUE, nopriors=TRUE,cores=ncores,
               nlcontrol=list(maxtimestep = 1), #maybe needs to be even smaller for subsampling type approaches... slows down though
               optimcontrol=list(finishsamples=100
                                 # ,stochastic=TRUE,plotsgd=TRUE #slower but sometimes more robust stochastic optimizer
               ), 
               verbose=1)
summary(f)
