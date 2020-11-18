# jonashaslbeck@gmail.com; ryanoisin@gmail.com; November 2020

# The code in this file reproduces all results in Appendices B and D
# except the fitting of the Continuous-Time VAR model
# which is located in the separate folder /CTVAR



# ----------------------------------------------------------------------
# --------- Source helper functions ------------------------------------
# ----------------------------------------------------------------------

source("fun_StatsModels.R")
library(RColorBrewer)
library(stats)
library(ctsem)

# ----------------------------------------------------------------------
# --------- Load and Prepare Data  -------------------------------------
# ----------------------------------------------------------------------

# ----- Load ideal data -----

ideal_data <- readRDS(file = "files/data.RDS")

# ----- Load CT-VAR(1) estimates for ideal data -------

ctideal <- readRDS("CTVAR/linearSDE_ideal.RDS")

# ----- Load CT-VAR(1) estimates for ESM data -------

ctesm <- readRDS("CTVAR/linearSDE_ESM.RDS")


# ----------------------------------------------------------------------
# --------- Appendix B: Continuous-Time VAR(1) (Ideal)  ----------------
# ----------------------------------------------------------------------

# Get summary statistics from estimated model
ideal_sum <- summary(ctideal)

# get drift matrix
point_ideal <- ideal_sum$parmatrices
drift_ideal <- matrix(
  point_ideal[
    rownames(point_ideal) == "DRIFT", "Mean"],
  4,4)

# get diffusion matrix (CT residual covariance matrix)
diffusion_ideal <- matrix(0,4,4)
diffpars <- point_ideal[rownames(point_ideal) == "DIFFUSION",]
for(i in 1:nrow(diffpars)){
  diffusion_ideal[diffpars[i,1], diffpars[i,2]] <- diffpars[i,3]
}
diffusion_ideal <- as.matrix(Matrix::forceSymmetric(diffusion_ideal, "L"))

# ---------- Make Figure --------------------

sc <- .8
pdf("figures/FigureX_CTVAR.pdf", width = 10*sc, height = 5*sc)
par(mfrow=c(1,2))

plotGraphs(mat = t(drift_ideal), edge.labels = t(round(drift_ideal,2)),
           title = "            Drift Network   (a)", 
           line=1.5, maximum=max(abs(drift_ideal)), 
           mar = c(8, 10, 12, 8), directed=TRUE, asize=6,
           fade = FALSE, cex = 1.5)


plotGraphs(diffusion_ideal, edge.labels = round(diffusion_ideal,2), 
           title = "         Diffusion Network  (b)", 
           line=1.5, rd=3, maximum=max(abs(diffusion_ideal)), 
           mar = c(8, 10, 12, 8), directed=FALSE,
           fade = FALSE, cex = 1.5)

dev.off()

# ----------------------------------------------------------------------
# ---------- Appendix B: Continuous-Time VAR(1) (ESM)  -----------------
# ----------------------------------------------------------------------

# Get summary statistics from estimated model
esm_sum <- summary(ctesm)

# get drift matrix
point_esm <- esm_sum$parmatrices
drift_esm <- matrix(
  point_esm[
    rownames(point_esm) == "DRIFT", "Mean"],
  4,4)

# get diffusion matrix
diffusion_esm <- matrix(0,4,4)
diffpars <- point_esm[rownames(point_esm) == "DIFFUSION",]
for(i in 1:nrow(diffpars)){
  diffusion_esm[diffpars[i,1], diffpars[i,2]] <- diffpars[i,3]
}
diffusion_esm <- as.matrix(Matrix::forceSymmetric(diffusion_esm, "L"))


# ---------- Make Figure --------------------

sc <- .8
pdf("figures/FigureX_CTVAResm.pdf", width = 10*sc, height = 5*sc)
par(mfrow=c(1,2))

plotGraphs(mat = t(drift_esm), edge.labels = t(round(drift_esm,2)),
           title = "            Drift Network   (a)", 
           line=1.5, maximum=max(abs(drift_esm)), 
           mar = c(8, 10, 12, 8), directed=TRUE, asize=6,
           fade = FALSE, cex = 1.5)


plotGraphs(diffusion_esm, edge.labels = round(diffusion_esm,2), 
           title = "         Diffusion Network  (b)", 
           line=1.5, rd=3, maximum=max(abs(diffusion_esm)), 
           mar = c(8, 10, 12, 8), directed=FALSE,
           fade = FALSE, cex = 1.5)

dev.off()


# ----------------------------------------------------------------------
# --- Appendix D: Model Diagnostics and Assumption Checking ------------
# ----------------------------------------------------------------------


# ---------- Part 1: Initial Exploration ----------------

# Extract (Partial) Auto- and Cross- correlations
# Note: takes a while because of the large lag.max
ACFobj <- acf(ideal_data[,c(2,3,4)], lag.max = 100000, plot = FALSE)
PACFobj <- pacf(ideal_data[,c(2,3,4)], lag.max = 1000, plot = FALSE)

# ------- Make Figure ---------

pdf("figures_revision/FigureX_ACF_PACF.pdf", width = 10, height = 4.5)
  plot_PACF(ACFobj, PACFobj, xaxis=c(0,100000))
dev.off()

# -------- Part 2: Transformed Data --------------------

# Remove time-varying mean

data_centered <- t(apply(ideal_data[,-1],1,function(row){
  # center around healthy fixed point
  if(row[1] > 2.8 & row[2] > 2.8 & row[3] < 2.8 & row [4] < 2.8){
    row <- row - c(4.89, 4.89, 1.36, 1.36)
  } else { 
    row <- row - c(1.36, 1.36, 4.89, 4.89)
    }
    
}))


# Get correlation functions of centered data

cACFobj <- acf(data_centered[,c(1,3,4)], lag.max = 100000, plot = FALSE)
cPACFobj <- pacf(data_centered[,c(1,3,4)], lag.max = 1000, plot = FALSE)

# ------- Make Figure ---------

pdf("figures_revision/FigureX_centered.pdf", width = 10, height = 10)
lmat <- matrix(c(1,1,2,3), 2,2,byrow =TRUE)
layout(lmat)
plotTimeSeriesFirst2Weeks(data_thinned = center_thinned, 
                          label=("(a) Transformed Time Series"), 
                          mar = c(3.5,3.5,4.1,1), 
                          cex.lab = 1.5,
                          line.lab = 2.25,
                          ylim = c(-4,4),
                          yat = c(-4,-2,0,2,4))

plot_PACF(cACFobj, cPACFobj, xaxis = c(0,100000), 
          atext = "(b) Correlation Functions",
          btext="(c) Partial Correlation Functions",
          layout = FALSE)
dev.off()

# -------- Part 3: Estimate VAR  --------------------

# Estimate model
 VAR_detrend <- EstimateVAR(scale(data_centered))
 sums <- EstimateVAR(scale(data_centered), summaries = TRUE)
  
# ------- Make Figure -----

sc <- .6
pdf("figures_revision/FigureX_Lag1_VAR_detrend.pdf", width = 10*sc, height = 10*sc)

data_0 <- apply(data_centered,2,function(col){
  col[-length(col)]
}) # t
data_1 <- data_centered[-1,] # t+1
# setup plot
par(mfrow=c(2,2))

# A and B: Heat plot Marginal Relations

par(mar=c(4,4,2,2))

Scatter2Heatplot(data_0[, 1], data_1[, 2], 
                 n_res = 100,
                 xlim = c(-4, 4), 
                 ylim = c(-4, 4), 
                 breaksx = c(-4, 4),
                 breaksy = c(-4, 4))
title(ylab=expression("Content"[t]), xlab=expression("Cheerful"[t-1]), line=2.2)
abline(lm(data_1[, 2]~data_0[, 1]), col="red")
cor_23 <- cor(data_1[, 2], data_0[, 1])
text(6.5, 2, bquote(bold(rho == .(round(cor_23,2)))), col="red")
rect(xleft = 7.25, ybottom = 7.25, xright = 9, ytop = 9, border = FALSE, col = "white")
text(7.7, 7.7, "(a)", cex=1.2)



Scatter2Heatplot(data_0[, 2], data_1[, 3], 
                 n_res = 100,
                 xlim = c(-4, 4), 
                 ylim = c(-4, 4), 
                 breaksx = c(-4, 4),
                 breaksy = c(-4, 4))
title(ylab=expression("Anxious"[t]), xlab=expression("Content"[t-1]), line=2.2)
abline(lm(data_1[, 3]~data_0[, 2]), col="red")
cor_34 <- cor(data_1[, 3], data_0[, 2])
text(6.5, 2, bquote(bold(rho == .(round(cor_34,2)))), col="red")
text(7.7, 7.7, "(b)", cex=1.2)

# C and D: Make VAR networks

plotGraphs(mat = t(VAR_detrend$phi), edge.labels = t(round(VAR_detrend$phi,2)),
           title = "            Lag(1) Network      (c)", 
           line=0, maximum=max(abs(VAR_detrend$phi)), 
           mar = c(8, 10, 12, 8), directed=TRUE, asize=6,
           fade = FALSE, cex = 1.2)


  psi <- VAR_detrend$psi
  icormat_res <- -cov2cor(solve(psi))
  diag(icormat_res) <- 0


plotGraphs(icormat_res, edge.labels = round(icormat_res,2), 
           title = "         Residual Network     (d)", 
           line=0, rd=3, maximum=max(abs(icormat_res)), 
           mar = c(8, 10, 12, 8), directed=FALSE,
           fade = FALSE, cex = 1.2)

dev.off()

# Optional: Correlation function of residuals 
residuals  <- do.call("cbind", lapply(sums, function(list) list$residuals))

rACFobj <- acf(residuals[,c(1,3,4)], lag.max = 100000, plot = FALSE)
rPACFobj <- pacf(residuals[,c(1,3,4)], lag.max = 1000, plot = FALSE)

plot_PACF(rACFobj, rPACFobj)
