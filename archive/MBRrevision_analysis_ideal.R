# ----------------------------------------------------------------------
# --------- Source helper functions ------------------------------------
# ----------------------------------------------------------------------

source("fun_StatsModels.R")
library(RColorBrewer)
library(scales)
library(stats)


## better ACF plot
plot.acf <- function(ACFobj, ci = FALSE, type = "l", maxlag = NULL,
                     title = "Auto and Cross-Correlation Functions (Ideal)") {
  # extract array slice of interest
  x1acfs <- ACFobj$acf[,,1]
  # drop first row (lag = 0)
  # x1acfs <- x1acfs[-1,]
  
  labels <- list(c("x1"),
                 c("x1 x2"),
                 c("x1 x3")
  )
  par(mfrow = c(1,3), mar = c(5.1,4.1,4.1,2.1))
  for(i in 1:3){
    rr <- x1acfs[,i]
    if(is.null(maxlag)){ kk <- length(rr) } else { kk <- maxlag }
    
    plot(seq(kk), rr[1:kk], type = type, lwd = 2, yaxs = "i", xaxs = "i", 
         ylim = c(floor(min(rr)), 1), xlim = c(0, kk + 1), xlab = "Lag", 
         ylab = "Correlation", las = 1,
         main = labels[[i]])
    if(i == 2){
      mtext(title, side = 3, line = 2.2)
    }
    # optional: plot confidence intervals
    if(ci) {
      nn <- ACFobj$n.used
      abline(h = -1/nn + c(-2, 2)/sqrt(nn), lty = "dashed", col = "blue")
    }
    abline(h = 0)
  }
}

# ----------------------------------------------------------------------
# --------- Load and Prepare Data  -------------------------------------
# ----------------------------------------------------------------------

# ----- Load ideal data -----

data <- readRDS(file = "files/data.RDS")

# ----- Load ESM data -----

# data <- readRDS(file ="files/data_ESM.RDS")

# ----------------------------------------------------------------------
# --------- Model Diagnostics/Building: Ideal  -------------------------
# ----------------------------------------------------------------------

# (Partial) Auto- and Cross- correlations

ACFobj <- acf(data[,c(2,3,4)], lag.max = 100000, plot = FALSE)
PACFobj <- pacf(data[,c(2,3,4)], lag.max = 1000, plot = FALSE)


pdf("figures_revision/ACF_ideal.pdf", width = 9, height = 4)
plot.acf(ACFobj)
dev.off()

pdf("figures_revision/PACF_ideal.pdf", width = 9, height = 4)
plot.acf(PACFobj,
         title = "Partial Correlation Functions (Ideal)", maxlag = 20)
dev.off()

# ----------- De-trending? Seasonal Effects? ---------------

fltr_hr <- c(1/2, rep(1, times = (10*60)-2), 1/2)/(10*60)
fltr_day <- c(1/2, rep(1, times = (24*10*60)-2), 1/2)/ (24*10*60)

trend_hr <- filter(data[,"x1"], filter = fltr_hr)
trend_day <-  filter(data[,"x1"], filter = fltr_day)


plot.ts(trend_hr, ylab = "trend", main = "Trend (hourly)")
plot.ts(trend_day, ylab = "trend", main = "Trend (daily)")


# Allow automated decomposition
# i_day <- matrix(as.ts(data[,2]),14*24,60*10, byrow = TRUE)
# 
# 
# i_day <- as.ts(cbind(1:(14*24),i_day))
# decompose(i_day)


# de-trend
sel <- which(!is.na(trend_day))
id_detrend <- data[sel,]

for(i in c(2,3,4,5)){
  id_detrend[,i] <- id_detrend[,i] - filter(data[,i], filter = fltr_day)[sel]
}

plot(id_detrend[,2], main = "detrended")
(trend_day)


# actual detrending function

reg_centered <- t(apply(data[,-1],1,function(row){
  # center around healthy fixed point
  if(row[1] > 2.8 & row[2] > 2.8 & row[3] < 2.8 & row [4] < 2.8){
    row <- row - c(4.89, 4.89, 1.36, 1.36)
  } else { 
    row <- row - c(1.36, 1.36, 4.89, 4.89)
    }
    
}))

 plot(reg_centered[,2], main = "regime centered")

 
 VAR_detrend <- EstimateVAR(scale(reg_centered))
 
# now repeat analyses

sc <- .6
pdf("figures_revision/FigureX_Lag1_VAR_detrend.pdf", width = 10*sc, height = 10*sc)



data_0 <- apply(reg_centered,2,function(col){
  col[-length(col)]
}) # t
data_1 <- reg_centered[-1,] # t+1
# setup plot
par(mfrow=c(2,2))

# A) Heat plot Marginal Relations

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


# (Partial) Auto- and Cross- correlations

# ACFobj <- acf(data[,c(2,3,4)], lag.max = 100000, plot = FALSE)
# PACFobj <- pacf(data[,c(2,3,4)], lag.max = 1000, plot = FALSE)
# 
# 
# pdf("figures_revision/ACF_ideal.pdf", width = 9, height = 4)
# plot.acf(ACFobj)
# dev.off()
# 
# pdf("figures_revision/PACF_ideal.pdf", width = 9, height = 4)
# plot.acf(PACFobj,
#          title = "Partial Correlation Functions (Ideal)", maxlag = 20)
# dev.off()

# ----------------------------------------------------------------------
# --------------------- Continuous-Time Model  -------------------------
# ----------------------------------------------------------------------

library(ctsem)

head(data)

data_ctsem <- as.data.frame(data)
data_ctsem$id = rep(1,nrow(data))

datawide <- ctLongToWide(
  datalong = data_ctsem,
  id = "id",
  time = "time",
  manifestNames = c("x1", "x2", "x3","x4")
)

