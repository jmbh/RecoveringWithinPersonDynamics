# jonashaslbeck@gmail.com; ryanoisin@gmail.com; July 2020

# The code in this file reproduces all results in Section 4 on statistical 
# except a) the time-series plot in Figure 7 (see `plot_TimeSeriesData.R' instead)
#        b) the fitting of the TVAR model, which is located in the separate folder /TVAR

# ----------------------------------------------------------------------
# --------- Source helper functions ------------------------------------
# ----------------------------------------------------------------------

source("fun_StatsModels.R")
library(RColorBrewer)
library(scales)

# ----------------------------------------------------------------------
# --------- Load and Prepare Data  -------------------------------------
# ----------------------------------------------------------------------

# ----- Load data -----

data <- readRDS(file ="files/data_ESM.RDS")

# Load original data
data_original <- readRDS(file = "files/data.RDS")

# ----- Thinned version of time series -----

# Here we downsample "thin" the time series in order to plot it in time-series plots
# Plotting all 201600 time points would lead to a overful figure and lead to huge file sizes

# thin_scale <- 50 # every 50 time point
short_scale <- (60*24*7*2)/90

thinner <- 1:short_scale
data_thinned <- data[thinner, ]

# For some figures we would like to use the original data (thinned) for visualizations
n <- nrow(data_original)
int90 <- n / 900

data_agg2 <- data_original[round(seq(1, nrow(data), length = int90)), ]

# head(data_agg2)

n <- nrow(data)


# ----------------------------------------------------------------------
# --------- Descriptives / Histograms [new March 2020] -----------------
# ----------------------------------------------------------------------

sc <- 1.1
pdf("figures/FigureX_DescriptiveAndDataViz_ESM.pdf", width = 8*sc, height = 6*sc)

n <- nrow(data)

par(mar=par()$mar)

# Set up layout
lmat <- rbind(1:4, c(5,5,6,6))
lo <- layout(lmat, heights = c(.6, 1.05))

# Plot histograms
names <- c("(a) Cheerful", "(b) Content", "(c) Anxious", "(d) Sad")
for(i in 1:4) {
  hist(data[, i+1], xlim=c(0, 8), ylim = c(0, .6), xaxt="n", las =2,
       freq = FALSE, breaks=40, main="", xlab="", ylab="")
  title(xlab="Emotion Intensity", cex.lab = 1.3, line=2.5)
  title(ylab="Density", cex.lab = 1.3, line=2.7)
  axis(1, c(0, 2, 4, 6, 8))
  mtext(text = names[i], side=3, line=.95, cex = 1)  
}

# Plot Scatter plots
par(mar=c(5.1, 4.1, 2, 2.1))
n_res <- 500 # should be 100
cex.lab <- 1.5

Scatter2Heatplot(data[, 2], data[, 3], 
                 n_res = n_res,
                 xlim = c(0, 8), 
                 ylim = c(0, 8), 
                 breaksx = c(0, 8),
                 breaksy = c(0, 8))
title(ylab="Content", line=2.7, cex.lab=cex.lab)
title(xlab="Cheerful", line=2.5, cex.lab=cex.lab)
# abline(lm(data[, 3]~data[, 2]), col="red", lwd=1.3)
cor_23 <- cor(data[, 3], data[, 2])
# text(6.5, 2, bquote(bold(rho == .(round(cor_23,2)))), col="red", cex=1.3)
rect(xleft = 7.25, ybottom = 7.25, xright = 9, ytop = 9, border = FALSE, col = "white")
text(7.7, 7.7, "(e)", cex=1.6)


Scatter2Heatplot(data[, 3], data[, 4], 
                 n_res = n_res,
                 xlim = c(0, 8), 
                 ylim = c(0, 8), 
                 breaksx = c(0, 8),
                 breaksy = c(0, 8))
title(ylab="Anxious", line=2.7, cex.lab=cex.lab)
title(xlab="Cheerful", line=2.5, cex.lab=cex.lab)
# abline(lm(data[, 4]~data[, 3]), col="red", lwd=1.3)
cor_34 <- cor(data[, 4], data[, 3])
# text(6.7, 3, bquote(bold(rho == .(round(cor_34,2)))), col="red", cex=1.3)
text(7.7, 7.7, "(f)", cex=1.6)

dev.off()


# ----------------------------------------------------------------------
# ---------------- Hidden Markov Model ---------------------------------
# ----------------------------------------------------------------------

library(depmixS4)

# ---------- k=2 HMM ---------

# Specify Model Object
k <- 2
mod_2k <- depmix(response = list(x1~1, x2~1, x3~1, x4~1), 
                 data = as.data.frame(data), 
                 family = list(gaussian(), gaussian(), gaussian(), gaussian()),
                 nstates = k, 
                 trstart = runif(k^2))

# Fit
set.seed(1)
fit_mod <- fit(mod_2k)
summary(fit_mod) # get parameters and transition matrix
fit_mod@transition

# Predict states
est.states <- posterior(fit_mod)

# Number of switches
switches <- est.states$state[-1] - est.states$state[-length(est.states$state)]
table(switches)

# Thin states
states <- est.states$state[thinner]

# --------- Plotting --------

cols_regimes <- c("grey", "white")

pdf("figures/FigureX_HMM_ESM_snap.pdf", width = 9, height = 4)

# Plot Segmented Time Series
plotRegimes2Weeks(data = data_thinned, 
                  regimes = states-1, 
                  alpha = .7,
                  cols_regimes = cols_regimes, 
                  legend = TRUE, 
                  y_axis_label = c(0, 2, 4, 6, 8),
                  cex.lab = 1.5,
                  line.lab = 2.25)

dev.off()



# --------- Generate 2 weeks of data from HMM (for Appendix) --------

n <- nrow(data)

summary(fit_mod) # get parameters and transition matrix

hmm.coef <- summary(fit_mod)
hmm.coef <- list(r1 =
              list(means = hmm.coef[1,c(1,3,5,7)], 
                   sds = hmm.coef[1,c(2,4,6,8)]),
            r2 = 
              list(means = hmm.coef[2,c(1,3,5,7)], 
                   sds = hmm.coef[2,c(2,4,6,8)])) 

# estimated probabilties of staying in the same state
p11 <- .918
p22 <- .919

# On minute level:
data_gen <- matrix(NA, nrow = n, ncol = 4)
S <- rep(NA, n)
S[1] <- 1

set.seed(3)

# Inital value
if(S[1] == 1) {
  for(j in 1:4){
  data_gen[1, j] <- rnorm(n=1, mean = hmm.coef$r1$means[j], sd = hmm.coef$r1$sds[j])
  }
} else {
  for(j in 1:4){
  data_gen[1, j] <- rnorm(n=1, mean = hmm.coef$r2$means[j], sd = hmm.coef$r2$sds[j])
  }
}


for(i in 2:n) {
  
  # Draw state of i
  if(S[i-1] == 1) {
    S[i] <- sample(1:2, size=1, prob = c(p11, 1-p11)) 
  } else {
    S[i] <- sample(1:2, size=1, prob = c(1-p22, p22))     
  }
  
  # Draw from Mixture
  if(S[i] == 1) {
    for(j in 1:4){
    data_gen[i, j] <- rnorm(n=1, mean = hmm.coef$r1$means[j], sd = hmm.coef$r1$sds[j])
    }
  } else {
    for(j in 1:4){
    data_gen[i, j] <- rnorm(n=1, mean = hmm.coef$r2$means[j], sd = hmm.coef$r2$sds[j])
    }
  }
  
} # end for: time points


# --------- Plot HMM generated Data --------

# Thin Time series for plotting
data_gen_thinned <- data_gen[thinner, ]
data_gen_thinned <- cbind(rep(NA, nrow(data_gen_thinned)), data_gen_thinned)
S_thinned <- S[thinner]


 pdf("figures/Figure_APP_DataGen_HMM_ESM_snap.pdf", 
            width = 10, height = 4.5)
plotRegimes2Weeks(data = data_gen_thinned, regimes = S_thinned,
                  cols_regimes = c("grey", "white"),
                  legend = TRUE, plot_regimes = FALSE,
                  cex.lab = 1.5,
                  line.lab = 2.25)
 dev.off()


# ----------------------------------------------------------------------
# --------- Lag-0 / Contemporaneous ------------------------------------
# ----------------------------------------------------------------------

# ---------- Compute correlation & partial correlation matrix ----------
cormat <- cor(data[, 2:5])
icormat <- -cov2cor(solve(cormat))
diag(icormat) <- 0


# ---------- Plotting ----------

sc <- .6
pdf("figures/FigureX_Lag0_ESM_snap.pdf", width = 10*sc, height = 10*sc)

# setup plot
par(mfrow=c(2,2))


# A) Heat plot Marginal Relations

par(mar=c(4,4,2,2))

Scatter2Heatplot(data[, 2], data[, 3], 
                 n_res = 100,
                 xlim = c(0, 8), 
                 ylim = c(0, 8), 
                 breaksx = c(0, 8),
                 breaksy = c(0, 8))
title(ylab="Content", xlab="Cheerful", line=2.2)
abline(lm(data[, 3]~data[, 2]), col="red")
cor_23 <- cor(data[, 3], data[, 2])
text(6.5, 2, bquote(bold(rho == .(round(cor_23,2)))), col="red")
rect(xleft = 7.25, ybottom = 7.25, xright = 9, ytop = 9, border = FALSE, col = "white")
text(7.7, 7.7, "(a)", cex=1.3)


Scatter2Heatplot(data[, 3], data[, 4], 
                 n_res = 100,
                 xlim = c(0, 8), 
                 ylim = c(0, 8), 
                 breaksx = c(0, 8),
                 breaksy = c(0, 8))
title(ylab="Anxious", xlab="Cheerful", line=2.2)
abline(lm(data[, 4]~data[, 3]), col="red")
cor_34 <- cor(data[, 4], data[, 3])
text(6, 5, bquote(bold(rho == .(round(cor_34,2)))), col="red")
# text(6, 5, paste(expression(rho,"Cor = ", round(cor_34, 2)), col="red")
text(7.7, 7.7, "(b)", cex=1.3)


# B) Network Plots: Correlation & Partial Correlations

# Correlation matrix
plotGraphs(cormat, title = "           Correlation Network   (c)", line=0, fade = FALSE)

# Partial correlation matrix
plotGraphs(icormat, title = "     Partial Correlation Network (d)", line=0, fade = FALSE)

dev.off()




# ----------------------------------------------------------------------
# ----------------------- Lag-1 / VAR ----------------------------------
# ----------------------------------------------------------------------


# --------- Estimate VAR model ---------
data <- as.matrix(data)
VARout <- EstimateVAR(data[, -1])


# --------- Lag Data ---------

data_0 <- apply(data[,-1],2,function(col){
  col[-length(col)]
}) # t
data_1 <- data[-1,-1] # t+1
colnames(data_1) <- paste0(colnames(data_1), " + 1")
data_delay <- cbind(data_0,data_1)


# --------- Plotting ---------

sc <- .6
pdf("figures/FigureX_Lag1_VAR_ESM_snap.pdf", width = 10*sc, height = 10*sc)

# setup plot
par(mfrow=c(2,2))

# A) Heat plot Marginal Relations

par(mar=c(4,4,2,2))

Scatter2Heatplot(data_0[, 1], data_1[, 2], 
                 n_res = 100,
                 xlim = c(0, 8), 
                 ylim = c(0, 8), 
                 breaksx = c(0, 8),
                 breaksy = c(0, 8))
title(ylab=expression("Content"[t]), xlab=expression("Cheerful"[t-1]), line=2.2)
abline(lm(data_1[, 2]~data_0[, 1]), col="red")
cor_23 <- cor(data_1[, 2], data_0[, 1])
text(6.5, 2, bquote(bold(rho == .(round(cor_23,2)))), col="red")
rect(xleft = 7.25, ybottom = 7.25, xright = 9, ytop = 9, border = FALSE, col = "white")
text(7.7, 7.7, "(a)", cex=1.2)


Scatter2Heatplot(data_0[, 2], data_1[, 3], 
                 n_res = 100,
                 xlim = c(0, 8), 
                 ylim = c(0, 8), 
                 breaksx = c(0, 8),
                 breaksy = c(0, 8))
title(ylab=expression("Anxious"[t]), xlab=expression("Content"[t-1]), line=2.2)
abline(lm(data_1[, 3]~data_0[, 2]), col="red")
cor_34 <- cor(data_1[, 3], data_0[, 2])
text(6, 5, bquote(bold(rho == .(round(cor_34,2)))), col="red")
text(7.7, 7.7, "(b)", cex=1.2)

# B) Network Plots: Correlation & Partial Correlations

# VAR parameters
phi <- VARout$phi

plotGraphs(mat = t(phi), edge.labels = t(round(VARout$phi,2)),
           title = "            Lag(1) Network      (c)", 
           line=0, maximum=max(abs(phi)), 
           mar = c(8, 10, 12, 8), directed=TRUE, asize=8, fade = FALSE,
           cex = 1.2)

# Residual Network
psi <- VARout$psi
icormat_res <- -cov2cor(solve(psi))
diag(icormat_res) <- 0


plotGraphs(icormat_res, edge.labels = round(icormat_res,2), 
           title = "         Residual Network     (d)", 
           line=0, rd=3, maximum=max(abs(icormat_res)), 
           mar = c(8, 10, 12, 8), directed=F,
           fade = FALSE,
           cex = 1.2)

dev.off()



# ----------------------------------------------------------------------
# ---------------- Threshold VAR ---------------------------------------
# ----------------------------------------------------------------------

# Model fitting is specified in the file TVAR/analysis_TVAR_fit.R

# ----- Load Model Estimates ----
tvar.est <- readRDS("TVAR/tvar_est_ESM.RDS")
tvar.est$coefficients

# ---- Get Parameter Estimates + Regimes -----
tvar.coef <- getParamsTVAR(tvar.est, data = data, digits = 4)
states <- tvar.coef$states[thinner]
thresh <- tvar.coef$thresh

# Make states for plot based on the original data
states_orig <- rep(0,nrow(data_agg2))
states_orig[data_agg2[,2] < thresh] <- 1
states_orig[data_agg2[,2] > thresh] <- 2

#-------- Plot regimes + networks -------

sc <- .8
pdf("figures/FigureX_TVAR_ESM_snap.pdf", width = 9*sc, height = 8*sc)

edge.labels.cex = 2.5

# set up layout
lmat <- rbind(c(2, 2),
              c(1, 1),
              3:4)
lo <- layout(mat = lmat, heights = c(.1, .9, 1))

# Plot Segmented Time Series
plotRegimes2Weeks(data = data_agg2, 
                  regimes = states_orig, 
                  alpha = .7, 
                  legend = FALSE, 
                  y_axis_label = c(0, 2, 4, 6, 8),
                  layout = FALSE,
                  cex.lab = 1.5,
                  line.lab = 2.25)

text(0, .5, "(a)", cex=2)

# Plot Healthy regime network
phi1 <- tvar.coef$r1$phi
plotGraphs(mat = t(phi1), edge.labels = t(round(tvar.coef$r1$phi,2)),
           title = "(b) Healthy Regime", 
           line = -2, 
           maximum=max(abs(tvar.coef$r1$phi)), 
           mar = c(8, 8, 8, 8), directed=TRUE, asize=8,
           fade = FALSE,
           cex = 1.2,
           edge.labels.cex = edge.labels.cex)

# Plot Unhealthy regime network
phi2 <- tvar.coef$r2$phi

plotGraphs(mat = t(phi2), edge.labels = t(round(tvar.coef$r2$phi,2)),
           title = "(c) Unhealthy Regime", 
           line = -2, 
           maximum=max(abs(tvar.coef$r2$phi)), 
           mar = c(8, 8, 8, 8), directed=TRUE, asize=8,
           fade = FALSE, cex = 1.2,
           edge.labels.cex = edge.labels.cex)

dev.off()


# --------- Plot HMM generated Data --------

# Thin Time series for plotting
short_scale <- (60*24*7*2)/90

thinner <- 1:short_scale
data_gen_thinned <- dataTVAR[thinner, ]
data_gen_thinned <- cbind(rep(NA, nrow(data_gen_thinned)), data_gen_thinned)

pdf("figures/Figure_APP_DataGen_TVAR_ESM_snap.pdf", width = 10, height = 4.5)
plotRegimes2Weeks(data = data_gen_thinned, regimes = rep(NA, nrow(data_gen_thinned)), 
                  cols_regimes = c("grey", "white"), 
                  legend = TRUE, plot_regimes = FALSE,
                  cex.lab = 1.5,
                  line.lab = 2.25)
dev.off()




