library(ctsem)

#bistable ctsem model
# setwd("C:/Users/Driver/Dropbox/MPIB/code/bistable multivariate")
# data <- readRDS("C:/Users/Driver/Dropbox/MPIB/code/bistable multivariate/data.RDS")
data <- readRDS("files/data.RDS")
d=4 



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
nsampspercore <- 1000
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


f<-ctStanGenerateFromFit(f,nsamples = 100)
ctStanPostPredict(f,wait=F)

k=ctStanKalman(fit = f,nsamples = 100,collapsefunc = mean)
errtest <- apply(k$err[1,,],2,arima, order=c(1,0,0)) #check for auto correlated error
ctKalman(f,subjects=1,subsetindices=1:2,kalmanvec=c('y','yprior'),plot=T,
  plotcontrol=list(ylim=c(0,3),xlim=c(0,20)))
