# o.ryan@uu.nl; December 2019

# This file contains functions to aid in plotting DT and CT networks and parameters

# ----------------------------------------------------------------------
# ---------- Plot matrix in greyscale network form  --------------------
# ----------------------------------------------------------------------

netplot <- function(mat, greyscale = FALSE, maximum = .5, asize = 6, edge.labels = TRUE,
                    edge.label.cex = 2, fade = FALSE, shape = "circle",
                    labels = TRUE,
                    vsize = 20,
                    esize = 12){
  
  layout <- rbind(c(0,1), 
                  c(1,1), 
                  c(1,0),
                  c(0,0))
  
  if(isTRUE(labels)){ labels = c("X1", "X2", "X3", "X4") }
  
  m_lty <- matrix(1, 4, 4)
  m_lty[mat<0] <- 2 
  
  m_col <- matrix("blue",4,4)
  m_col[mat > 0 ] <- "firebrick2"
  if(greyscale){
    qgraph::qgraph(t(mat), 
                   layout = layout,
                   directed = T, 
                   edge.color = "darkgrey",
                   edge.labels = edge.labels,
                   edge.label.cex = edge.label.cex,
                   edge.label.color = "darkgrey",
                   # curved = FALSE, 
                   lty = t(m_lty), 
                   vsize = vsize, 
                   esize = esize,
                   asize= asize,
                   # color = cols,
                   mar = c(8, 10, 8, 8), maximum=maximum,
                   fade = fade,
                   shape = shape,
                   maximum = maximum,
                   labels = labels)
  } else{
    qgraph::qgraph(t(mat),
                   edge.color = t(m_col),
                   layout = layout,
                   directed = T, 
                   edge.labels = edge.labels,
                   edge.label.cex = edge.label.cex,
                   # curved = FALSE, 
                   lty = t(m_lty), 
                   vsize = 20, 
                   esize = 12,
                   asize= asize,
                   # color = cols,
                   mar = c(8, 10, 8, 8), maximum=maximum,
                   fade = fade,
                   shape = shape,
                   maximum = maximum,
                   labels = labels)
  }
}



# ----------------------------------------------------------------------
# ---------- Calculate Phi(Dt) for a range of Dt  ----------------------
# ----------------------------------------------------------------------

get_phidt_ar <- function(drift,dts){
  p <- ncol(drift)
  out <- vapply(
    lapply(
      dts,
      FUN = function(inter)
        as.matrix(expm::expm(drift * inter))
    ), 
    identity, matrix(0, p, p))
  return(out)}


# ----------------------------------------------------------------------
# ---------- Plot Phi as function of Dt  -------------------------------
# ----------------------------------------------------------------------

phidt_plot_subset <- function(input, dts, lwd = 4, cols = cols, smat,
                              xl = 4, yl = 1.06, liney = 4.5, linex = 2,
                              cex.axis = 1.25,
                              cex.main  = 1.75,
                              cex.lab = 1.5,
                              titletext = "",
                              font.main = 1,
                              vline = c(0.5,1),
                              cex.leg = 1.5,
                              legtf = FALSE,
                              maintf = FALSE,
                              xlines = TRUE,
                              ylim = NULL){
  # needs phidt_array as input type
  
  if(is.null(ylim)){  ylim = c(min(input),max(input)) }
 
  plot.new()
  plot.window(xlim = range(dts), ylim = ylim)
  axis(1, cex.axis = cex.axis)
  axis(2, cex.axis = cex.axis)
  title(ylab = "Parameter Value", line=liney, cex.lab = cex.lab)
  title(xlab = expression(paste("Time Interval (", Delta, "t)")), line=linex, cex.lab = cex.lab)
  title(main = titletext, cex.main = cex.main, font.main = font.main)
  
  if(xlines == TRUE){
  abline(v = vline[1], lty = 2); abline(v = vline[2], lty = 2) 
  }
  for(i in 1:nrow(smat)){
    lines(y=input[smat[i,1],smat[i,2],],x=dts,lty=smat[i,1],col=cols[smat[i,1],smat[i,2]],lwd=lwd)
  }
  abline(h =0)
  
  if(isTRUE(legtf)){
    # Make a legend
    legtext <- as.vector(unlist(apply(smat, 1, function(row){
      bquote(Phi[.(paste0(row[1], row[2]))])
    })))
    
    
    legend(x = xl, y = yl,
           as.expression(legtext),
           lty = smat[, 1], col = cols[smat] , lwd = lwd,
           cex = cex.leg)
  }
}

# ----------------------------------------------------------------------
# ---------- Plot Intervention Trajectories ----------------------------
# ----------------------------------------------------------------------

trajplot <- function(drift, 
                     x = 2, 
                     m = c(1,3), 
                     s.time = 0.5, 
                     delta.t = 1, 
                     m.int = 0, 
                     x.int = 1,
                     lwd = 3,
                     lwdp = 2,
                     ymin = -0.75,
                     xmin = -0.05,
                     cex.axis = 1.25,
                     font.main = 1, 
                     cex.main  = 1.75,
                     cex.lab = 1.5,
                     xaxis = seq(0,1,length = 5),
                     yaxis = seq(-0.5,1,length = 4),
                     subleg = TRUE,
                     cex.leg = 1.25,
                     parts = c(1,2)
){
  
  # ------ Preliminary set up -----------
  
  n <- nrow(drift)
  
  # Assign mediators
  M <- m
  S <- diag(nrow(drift))
  S[M,M] <- 0 
  
  # Set vector of starting values
  start <- rep(0,n)
  start[x] <- x.int
  
  # Get CT matrix under indirect effects intervention (CT)
  drift_tilde <- S%*%drift%*%S
  
  # Set up the time vector
  dts2 <- seq(0,delta.t,.001)
  
  # Find the index of the time vector corresponding to DE inervention
  intmom <- which(dts2 == s.time)
  
  # Set up storage vectors
  y_ct <- matrix(0,length(dts2),n+1)
  y_dt2 <-  matrix(0,length(dts2),n+1)
  y_dt1 <- matrix(0, length(dts2)+1, n+1)
  
  
  # ------ Generate the trajectories  -----------
  
  # Get Trajectories for CT and total (DT2) effects
  
  for(i in 1:length(dts2)){
    # Total Effect
    y_ct[i,1:n] <- expm(drift_tilde*dts2[i])%*%start
    y_ct[i,n+1] <- y_dt2[i,n+1]  <- dts2[i]
    # CT Direct Effect
    y_dt2[i,1:n] <- expm(drift*dts2[i])%*%start
  }
  
  # Trajectory for DT1 DE (one intervention in the middle)
  
  for(i in 1:length(dts2)){
    if(i < intmom){ #normal up until intervention moment
      y_dt1[i,1:n] <- expm(drift*dts2[i])%*%start
      y_dt1[i,n+1] <- dts2[i]
    }
    if(i == intmom){
      y_dt1[i,1:n] <- expm(drift*dts2[i])%*%start
      y_dt1[i,n+1] <- dts2[i]
      # Intervene to set mediators to zero
      newstart <- y_dt1[i,1:n]
      newstart[M] <- 0
      newdt <- dts2[2:intmom]
      # Write extra observation (post-intervention)
      y_dt1[i+1,1:n] <- newstart
      y_dt1[i+1,n+1] <- dts2[i]
    }
    if(i > intmom){
      y_dt1[i+1,1:n] <- expm(drift*newdt[i-intmom])%*%newstart
      y_dt1[i+1,n+1] <- dts2[i]
    }
  }
  
  
  
  # -------------------------------------------
  # ------ Plot trajectories ------------------
  # -------------------------------------------
  
  # Tune allows us to see overlapping lines more clearly
  tune <- .01
  
  
  # Set up layout
  if(all(parts == c(1,2))){
    lmat <- matrix(c(1,2,3),1,3, byrow = TRUE)
    lo <- layout(mat = lmat, 
                 heights = c(1,1,1),
                 widths = c(1,1,.25))
    par(mar = c(5.1,4.1,4.1,.5))
    main1 <- "(a) TE Intervention"
    main2 <- "(b) DE Intervention"
  } else if(parts == 1){
    layout(mat = 1, 
           heights = 1,
           widths = 1)
    par(mar = c(5.1,4.1,4.1,.5))
    main1 <- ""
  } else if(parts == 2){
    lmat <- matrix(c(1,2),1,2, byrow = TRUE)
    lo <- layout(mat = lmat, 
                 heights = c(1,1),
                 widths = c(1,.25))
    par(mar = c(5.1,4.1,4.1,.5))
    main2 <- ""
  }
  
  
  
  # TE
  if(all(parts == c(1,2)) | parts == 1){
    plot.new()
    plot.window(xlim=c(xmin, delta.t), ylim=c(ymin,1))
    abline(h = 0)
    for(c in 1:4) lines(y = y_dt2[,c], x = y_dt2[,5], type = "l", col = cols[c,c], lwd = lwd)
    axis(1, at = xaxis,  cex.axis = cex.axis)
    axis(2, at = yaxis, cex.axis = cex.axis)
    points(y = y_dt2[1,x], x = y_dt1[1, n+1], pch =
             5, cex = 2, col = cols[x, x], lwd = lwdp)
    
    title(xlab = "Time (t)", ylab = "Variable Values", main = main1,
          font.main = font.main, cex.main = cex.main, cex.lab = cex.lab)
  }
  
  # DE
  if(all(parts == c(1,2)) | parts == 2){
    plot.new()
    plot.window(xlim=c(xmin, delta.t), ylim=c(ymin,1))
    abline(h = 0)
    
    # move mediators around a little for better visualization
    y_ctj <- y_ct ; y_ctj[,M[1]] <- y_ct[,M[1]] + tune ; y_ctj[,M[2]] <- y_ct[,M[2]] - tune 
    
    for(c in 1:4) lines(y = y_ctj[,c], x = y_ct[,n+1], type = "l", col = cols[c,c], lwd = lwd)
    axis(1, at = xaxis,  cex.axis = cex.axis)
    axis(2, at = yaxis, cex.axis = cex.axis)
    points(y = y_dt2[1,x], x = y_dt1[1, n+1], pch =
             5, cex = 2, col = cols[x, x], lwd = lwdp)
    # start intervention points
    points(y = y_ctj[1,m], x = rep(y_dt1[1, n+1],length(m)), pch =
             18, cex = 3, col = cols[1,m], lwd = lwdp)
    # end intervention points
    points(y = y_ctj[nrow(y_ctj),m], x = rep(y_dt1[nrow(y_ctj), n+1],length(m)), pch =
             18, cex = 3, col = cols[1,m], lwd = lwdp)
    
    
    title(xlab = "Time (t)", ylab = "Variable Values", main = main2,
          font.main = font.main, cex.main = cex.main, cex.lab = cex.lab)
    
    par(mar=c(0, 0, 0, 0))
    plot.new()
    plot.window(xlim=c(0, 1), ylim = c(0, 1))
    if(!isTRUE(subleg)) {
      legend("left", paste0("Y",1:n, "(t)"), 
             pch = 19, col=cols[1,], cex = cex.leg, yjust = 0, xjust = 0)
    }
    if(isTRUE(subleg)){
      legend("left",
             c(expression(Y[1](t)),
               expression(Y[2](t)),
               expression(Y[3](t)),
               expression(Y[4](t))), 
             pch = 19, col=cols[1,], cex = cex.leg, yjust = 0, xjust = 0)
    }
  }
  
}



