# jonashaslbeck@gmail.com; ryanoisin@gmail.com; July 2020

# This file contains functions to sample from the true bistable system. 

# This file is sourced by:
#    Generate_Data.R.
#    simulation.R


# ----------------------------------------------------------------------
# --------- Define Differential Equation  ------------------------------
# ----------------------------------------------------------------------

# --------- (1) Define base equation ---------


dXdt <- function(i,  # indicating the variable at hand
                 r, # p-vector of rate of changes
                 epsilon,  # noise term
                 C, # dependence matrix
                 mu, # adding fixed amount [seems to be a fix]
                 X, # p-vector of variable states
                 timestep = 1) {
  
  # epsilon <- rnorm(1, mean = 0, sd = 0.15)
  
  dX <- r[i] * X[i] + X[i] * sum(C[i, ] * X) + mu + epsilon
  
  dX * timestep
  
}


# --------- (2) Parameterize ---------

p <- 4
C <- matrix(c(-.2, .04, -.2, -.2,
              .04, -.2, -.2, -.2,
              -.2, -.2, -.2, .04,
              -.2, -.2, .04, -.2), p, p, byrow = T)


# ----------------------------------------------------------------------
# --------- Single Function for Data Generation ------------------------
# ----------------------------------------------------------------------

genData <- function(time, 
                    stress, 
                    timestep, 
                    noiseSD = 1, 
                    noise = TRUE, 
                    pbar=TRUE) {
  
  # Define Parameters
  C <- matrix(c(-.2, .04, -.2, -.2, 
                .04, -.2, -.2, -.2,
                -.2, -.2, -.2, .04, 
                -.2, -.2, .04, -.2), p, p, byrow = T)
  
  r <- c(1, 1, stress, stress)
  
  mu <- 1.6
  
  p <- 4
  
  # Storage
  nIter <- time / timestep
  X <- matrix(NA, nIter, 4)
  X[1, ] <- c(1.3, 1.3, 4.8, 4.8)
  
  
  # Set progress bar
  if(pbar) pb <- txtProgressBar(min = 2, max=nIter, initial=0, char="-", style = 3)
  
  
  # Solve with Euler's method
  for(j in 2:nIter) {
    for(i in 1:p) {
      X[j, i] <- X[j-1, i] + dXdt(i = i,
                                  r = r,
                                  epsilon = ifelse(noise, rnorm(1, mean = 0, sd = noiseSD), 0),
                                  C = C,
                                  mu = mu,
                                  X = X[j-1, ],
                                  timestep = timestep)
      
    }
    
    if(pbar)  setTxtProgressBar(pb, j)
  }
  
  # Return
  v_time <- seq(0, time, length = nIter)
  out <- cbind(v_time, X)
  colnames(out) <- c("time", "x1", "x2", "x3", "x4")
  return(out)
  
  
} # eoF







