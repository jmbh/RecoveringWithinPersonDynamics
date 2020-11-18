# jonashaslbeck@gmail.com; ryanoisin@gmail.com; July 2020

# This file contains functions to summarize and visualize the results of the
# statistical time series models reported in Section 3; also includes some
# plotting functions used in Section 4

# Sourced by:
#  analysis_TimeSeriesModels_ideal.R
#  analysis_TimeSeriesModels_ESM.R
#  analysis_additional_appendices.R


# ----------------------------------------------------------------------
# --------- Plot the two-week time series (Jonas) ----------------------
# ----------------------------------------------------------------------

# ---------- Plotting first two weeks of time series [thinned] (Jonas) -

plotTimeSeriesFirst2Weeks <- function(data_thinned, 
                                      xaxis = NULL,
                                      label="", 
                                      mar=c(5.1,4.1,4.1,1),
                                      ylim = c(0,8), 
                                      cex.lab = 1,
                                      line.lab = 2,
                                      cols = NULL,
                                      yat = NULL) {
  
  library(RColorBrewer)
  library(scales)
  
  if(is.null(xaxis)){
    xaxis = 1:nrow(data_thinned)
  }
  
  # Plot setup
  par(mar=mar)
  plot.new()
  plot.window(xlim=c(min(xaxis), max(xaxis)), ylim=ylim)
  if(is.null(cols)){
  cols <- brewer.pal(n = 4, "Set1")
  }
  # data
  for(i in 1:4) lines(y = data_thinned[, i+1], x= xaxis, col = alpha(cols[i], alpha=1), lwd=1)
  
  # axis
  aux_seq <- round( (seq(1, max(xaxis), length=15)) / max(xaxis) * 14 )
  axis(1, at = c(0, 5, 10, 14) / 14 * max(xaxis), labels = c(0, 5, 10, 14))
  if(is.null(yat)){ axis(2, c(0, 2, 4, 6, 8), las=2)} else { axis(2, yat, las=2)}
  
  # Title
  title(xlab = "Days", ylab = "Emotion Strength", line=line.lab, cex.lab = cex.lab)
  if(label!="") mtext(text = label, adj=0, line=1, cex=1.5)
  
}


# --------- Plotting first two weeks + regime (Jonas, Oisin) -----------

plotRegimes2Weeks <- function(data, 
                              regimes, 
                              cols_regimes = c("grey", "white"), 
                              alpha=.3, 
                              layout = TRUE, 
                              legend = FALSE, 
                              plot_regimes = TRUE, 
                              ylim = c(-2, 10), 
                              y_axis_label = c(-2, 0, 2, 4, 6, 8, 10),
                              lwd = 0.5,
                              cex.lab = 1,
                              line.lab = 2, 
                              cols = NULL){
  library(scales)
  library(RColorBrewer)
  
  
  ylim <- range(y_axis_label)
  n_regimes <- length(unique(regimes))
  if(is.null(cols)){
  cols <- brewer.pal(n = 4, "Set1")
  }
  
  # Input Checks
  if(nrow(data)!=length(regimes)){
    regimes <- regimes[1:nrow(data)]
    warning(paste0("data and regimes of different length, taking 
                   the first", nrow(data), "regime entries"))
  }
  
  
  # Setup layout
  if(layout) {  # possbility to switch off so function can be used to plot into more complex (global) layouts
    lmat <- matrix(c(1:2), nrow=2)
    layout(mat = lmat, heights = c(.1, 1))  
  }
  
  if(legend) {
    
    lmat <- cbind(3:2, c(4,1))
    lo <- layout(mat = lmat, heights = c(.1, 1), widths = c(1, .2))
    # layout.show(lo)
    
    if(plot_regimes == FALSE) {
      lmat <- rbind(2:1)
      lo <- layout(mat = lmat, widths = c(1, .2))
    }
    
    # legend:
    par(mar=c(0, 0, 0, 0))
    plot.new()
    plot.window(xlim=c(0, 1), ylim=c(0,1))
    legend(-.1, .5, c("Cheerful", "Content", "Anxious", "Sad"), 
           lwd = rep(1.5, 4), col = cols, bty='n', cex=1.3, yjust = 0)
    
    # rect(xleft = 0, ybottom = 0, xright = 1, ytop = 1, col="red")
    
    
  } # end if: legend
  
  
  
  # Color assignment for >2 components
  col.regimes <- rep(NA, nrow(data))
  for(i in 1:n_regimes) col.regimes[regimes == i] <- cols_regimes[i]
  
  # Find the change points, write to df
  changepoints <- c(FALSE, (regimes[-1] != regimes[-nrow(data)]))
  which_cp <- which(changepoints)
  rectangles <- data.frame(c(0,which_cp), # start point
                           c(which_cp,nrow(data)), # end point
                           c(col.regimes[1],col.regimes[which_cp]))
  
  # Plot the time series
  par(mar=c(3.5, 4, 0.5, 0))
  
  plot.new()
  plot.window(xlim=c(1, nrow(data)), ylim=ylim)
  
  if(plot_regimes) {
    
    # First plot background segmentation (regimes)
    for(i in 1:nrow(rectangles)){
      rect(xleft = rectangles[i,1], ybottom = ylim[1], 
           xright = rectangles[i,2], ytop = ylim[2],
           col = alpha(rectangles[i,3], alpha=alpha),
           # border = alpha(rectangles[i,3], alpha=alpha/3),
           border = TRUE,
           lwd=0.75)
    }
    
  } # if: plot regimes?
  
  # Then time series on
  for(i in 1:4) lines(data[, i+1], col = alpha(cols[i], alpha=1), lwd=lwd)
  # axis
  aux_seq <- round( (seq(1,  nrow(data), length=15)) / nrow(data) * 14 )
  axis(1, at = c(0, 5, 10, 14) / 14 * nrow(data), labels = c(0, 5, 10, 14))
  axis(2, y_axis_label, las=2)
  title(xlab = "Days", ylab = "Emotion Strength", line=line.lab, cex.lab = cex.lab)
  
  
  # Plot Legend of States
  if(plot_regimes) {
    shift_x <- .04
    par(mar=c(0, 4.1, 0, 2.1))
    plot.new()
    plot.window(xlim=c(0, 1), ylim=c(0, 1))
    
    rect(xleft = .1+shift_x, ybottom = .25 , xright = .3+shift_x, ytop = .75, col = alpha(cols_regimes[1], alpha), border = TRUE)
    text( 0.3+0.075+shift_x, .5, "State 1", col = "black")
    
    rect(xleft = .50+shift_x, ybottom = .25 , xright = .7+shift_x, ytop = .75, col = alpha(cols_regimes[2], alpha), border = TRUE)
    text(.7+0.075+shift_x, .5, "State 2", col = "black")
  } # if: plot regimes?
  
} # eoF



# --------- Scatter to Heatplot function (Jonas) -------------------------------------

Scatter2Heatplot <- function(x, y, 
                             n_res = 100,
                             xlim = c(0,10),
                             ylim = c(0,10),
                             breaksx = c(0,10),
                             breaksy = c(0,10),
                             xat = NULL,
                             yat = NULL){
  
  
  # Aggregate into grid
  breaksx1 <- seq(breaksx[1], breaksx[2], length=n_res+1)
  breaksy1 <- seq(breaksy[1], breaksy[2], length=n_res+1)
  m_D <- matrix(NA, n_res, n_res)
  
  for(i in 1:n_res) { # x
    for(j in 1:n_res) { # y
      
      ind_x <- x >= breaksx1[i] & x <= breaksx1[i+1] 
      ind_y <- y >= breaksy1[j] & y <= breaksy1[j+1] 
      ind <- ind_x & ind_y
      m_D[i, j] <- sum(ind)
      
    }
  }
  
  # Normalize
  m_Dt <- log(m_D)
  m_Dtn <- m_Dt / max(m_Dt) + 0 # Add constant
  m_Dtn2 <- m_Dtn / max(m_Dtn)
  
  
  # Define color scale
  gray_cols <- gray.colors(1000 + 1, 0, 1)
  gray_cols <- gray_cols[length(gray_cols):1] # high value = black
  
  
  # Map to color scale
  m_Dn_col <- matrix(NA, n_res, n_res)
  for(i in 1:n_res) { # x
    for(j in 1:n_res) { # y
      m_Dn_col[i, j] <- gray_cols[round(m_Dtn2[i, j], 3)*1000 + 1]
    }
  }
  
  
  
  # Setup plotting area
  plot.new()
  plot.window(xlim=xlim, ylim=ylim)
  if(is.null(xat)) axis(1) else axis(1, at = xat)
  if(is.null(yat)) axis(2, las=2) else axis(2, las=2, at = yat)
  
  
  # Plot grid
  for(i in 1:n_res) { # x
    for(j in 1:n_res) { # y
      rect(xleft = breaksx1[i], ybottom = breaksy1[j], xright = breaksx1[i+1], ytop = breaksy1[j+1], lty=0, col = m_Dn_col[i, j])
    }
  }
  
} # end of function




# ------ Function for network plots (Oisin) --------

plotGraphs <- function(mat, 
                       edge.labels = NULL, 
                       title = "", 
                       directed = FALSE, asize = 3.63,
                       line=-.7, 
                       maximum=.9, cex=1, 
                       mar = c(8, 10, 8, 8), rd=2, se=1,
                       fade = TRUE,
                       edge.labels.cex = 2) {
  
  if(is.null(edge.labels)) edge.labels <-  round(mat, rd)
  
  library(qgraph)
  
  emotions <- c("Cheerful", "Content", "Anxious", "Sad")
  layout <- rbind(c(0,1), 
                  c(1,1), 
                  c(0,0), 
                  c(1,0))
  
  m_lty <- matrix(1, 4, 4)
  m_lty[mat<0] <- 2 
  
  par(mar=par()$mar) # set to default
  
  qgraph(mat, 
         layout = layout,
         directed = directed, 
         edge.color = "darkgrey",
         edge.labels = edge.labels,
         edge.label.cex = edge.labels.cex,
         edge.label.color = "darkgrey",
         labels = c("Cheerful", "Content", "Anxious", "Sad"), 
         lty = m_lty, 
         vsize = 20, 
         esize = 12,
         asize= asize,
         mar = mar, 
         maximum=maximum,
         fade = fade)
  
  mtext(title, side=3, cex=cex, line=line)
  
}


EstimateVAR <- function(data, roundpoints = 4, summaries = FALSE, changescore = FALSE) {
  
  p <- ncol(data)
  Phi <- matrix(NA, p, p)
  alpha <- rep(NA, p)
  residuals <- matrix(NA, nrow(data)-1, p)
  summary_list<- list()
  for(i in 1:p) {
    
    # Estimates the change-score version of the var model instead
    if(changescore == TRUE){
      y = data[-1,i] - data[-nrow(data), i]
    } else {y <- data[-1, i]}
    
    
    X <- data[-nrow(data), ]
    
    coefs <- lm(y ~ X)
    Phi[i, ] <- coefs$coefficients[-1]
    alpha[i] <- coefs$coefficients[1]
    residuals[,i] <- coefs$residuals
    if(summaries) summary_list[[i]] <- summary(coefs)
  }
  Psi <- cov(residuals)
  mu <- as.vector(solve(diag(p)-Phi)%*%alpha)
  coefs <- list(round(alpha, roundpoints),round(mu,roundpoints), round(Phi, roundpoints), round(Psi,roundpoints))
  names(coefs) <- c("intercepts", "means","phi","psi")
  if(summaries){ return(summary_list) } else { return(coefs) }
  
}


# --------- Function to Extract Threshold VAR parameter estimates -----------

getParamsTVAR <- function(tvar.est, data = data, digits = 2){
  thresh <- tvar.est$model.specific$Thresh
  
  # Get regimes for entire time series
  states <- rep(0, nrow(data))
  states[data[, 2] < thresh] <- 1
  states[data[, 2] > thresh] <- 2
  
  
  int_1 <- tvar.est$coefficients$Bdown[,1]
  phi_1 <- tvar.est$coefficients$Bdown[,-1]
  
  int_2  <- tvar.est$coefficients$Bup[,1]
  phi_2 <- tvar.est$coefficients$Bup[,-1]
  
  vnames <- paste0("x", 1:4)
  names(int_1) <- names(int_2) <- vnames
  dimnames(phi_1) <- dimnames(phi_2) <- list(vnames,vnames)
  
  mu_1 <- as.vector(solve(diag(4)-phi_1)%*%int_1)
  mu_2 <- as.vector(solve(diag(4)-phi_2)%*%int_2)
  
  resids <- cbind(tvar.est$residuals,states[-1])
  psi_1 <- cov(resids[resids[,5]==1,1:4])
  psi_2 <- cov(resids[resids[,5]==2,1:4])
  
  
  # Collect in lists
  
  r1 <- list(round(int_1, digits = digits), round(mu_1,digits = digits), 
             round(phi_1, digits = digits), round(psi_1,digits = digits))
  names(r1) <- c("intercepts", "means","phi","psi")
  
  r2 <- list(round(int_2, digits = digits), round(mu_2, digits = digits), 
             round(phi_2, digits = digits), round(psi_2,digits = digits))
  names(r2) <- c("intercepts", "means","phi","psi")
  
  # switch order to match with ordering of regimes in MSVAR
  out <- list(r2,r1,thresh, states)
  names(out) <- c("r1", "r2", "thresh", "states")
  return(out)
}


#-------- Generate Data from TVAR -------


genData_TVAR <- function(tvar.coef,
                         n,
                         initial = c(1.3, 1.3, 4.8, 4.8)) {
  
  library(MASS)
  
  p <- 4
  
  # Storage
  data <- matrix(NA, nrow=n, ncol=p)
  data[1, ] <- initial
  state <- rep(NA, n)
  state[1] <- 0
  
  # Loop time points
  for(i in 2:n) {
    
    # Select parameter matrix
    if(data[i-1, 1] > tvar.coef$thresh) { # state 1 = healthy regime
      int <- tvar.coef$r1$intercepts
      phi <- tvar.coef$r1$phi # lagged effects
      psi <- tvar.coef$r1$psi # residual covariance
      state[i] <- 0
    } else {
      int <- tvar.coef$r2$intercepts
      phi <- tvar.coef$r2$phi # lagged effects
      psi <- tvar.coef$r2$psi # residual covariance
      state[i] <- 1
    }
    
    # Noise draw
    noise <- mvrnorm(n=1, mu=rep(0, p), Sigma = psi)
    
    # Loop variables
    for(v in 1:p) {
      
      data[i, v] <- int[v] + sum(data[i-1, ] * phi[v, ]) + noise[v]
      
    } # end for: i
  } # end for: v
  
  return(data)
  
} # eoF

# --------- Function to plot time series correlation functions -----------


plot_PACF <- function(ACFobj, PACFobj,
                      xaxis = c(0,50000),
                      xaxis2 = c(0,10),
                      mar=c(5.1,4.1,4.1,1),
                      ylim = c(-1,1), 
                      cex.lab = 1,
                      line.lab = 2,
                      cols = c("blue", "red"),
                      atext = "(a) Correlation Functions",
                      btext = "(b) Partial Correlation Functions",
                      layout = TRUE
){
  
  if(layout){
    lmat <- rbind(1:2)
    lo <- layout(mat = lmat, widths = c(1, 1))
  }
  
  
  
  
  # Get required data
  x1acfs <- ACFobj$acf[1:max(xaxis),c(1,3),1]
  
  
  par(mar=mar)
  plot.new()
  plot.window(xlim=c(min(xaxis), max(xaxis)), ylim=ylim)
  
  
  
  for(i in 1:2){
    kk <- 1:max(xaxis)
    lines(y = x1acfs[,i], x=1:max(xaxis), col = cols[i], lwd=1.5)
  }
  axis(1)
  axis(2)
  abline(h = 0, lty = 2)
  
  title(xlab = "Lag", ylab = "Correlation", line=line.lab, cex.lab = cex.lab)
  mtext(text = atext, adj=0, line=1, cex=1.5)
  
  legend("topright", c("Auto-Correlation", "Cross-Correlation \n(Between Valence)"), 
         lwd = rep(1.5, 2), col = cols, bty='n', cex=1, yjust = 0)
  
  # ------ Start Partial Correlation -----------
  
  
  pcfs <- PACFobj$acf[1:max(xaxis2),c(1,3),1]
  
  
  plot.new()
  plot.window(xlim=c(min(xaxis2), max(xaxis2)), ylim=ylim)
  for(i in 1:2){
    kk <- 1:max(xaxis2)
    lines(y = pcfs[,i], x=1:max(xaxis2), col = cols[i], lwd=1.5)
  }
  
  axis(1)
  axis(2)
  abline(h = 0, lty = 2)
  
  title(xlab = "Lag", ylab = "Partial Correlation", line=line.lab, cex.lab = cex.lab)
  mtext(text = btext, adj=0, line=1, cex=1.5)
  
  legend("topright", c("Partial AC", "Partial CC \n(Between Valence)"), 
         lwd = rep(1.5, 2), col = cols, bty='n', cex=1, yjust = 0)
}

