# jonashaslbeck@gmail.com; ryanoisin@gmail.com; July 2020

# Plots Figures 2 and 7 displaying the 2-week "ideal" and ESM data used in Sections 3 and 4 respectively

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

data <- readRDS(file = "files/data.RDS")


# ----- Thinned version of time series -----

# Here we downsample "thin" the time series in order to plot it in time-series plots
# Plotting all 201600 time points would lead to a overful figure and lead to huge file sizes

thin_scale <- 50 # every 50 time point
thinner <- seq(1, nrow(data), length = round(nrow(data)) / thin_scale)
data_thinned <- data[thinner, ]


# ----------------------------------------------------------------------
# --------- Figure 2: Time Series Plot (Jonas) -------------------------
# ----------------------------------------------------------------------

library(RColorBrewer)
library(scales)

# ----- Select Colors ------

cols <- brewer.pal(n = 4, "Set1")
# cols <- c("#0db534", "#15bfe6", 
#           "#ff547c", "#b86807")


# ----- Plotting ------

pdf("figures/Figure2_TheTimeSeries.pdf", width = 10, height = 4.5)

# --- Set up Plot ---

lmat <- rbind(1:2)
lo <- layout(mat = lmat, widths = c(1, .4))

# --- Plot: full 2 weeks ---

plotTimeSeriesFirst2Weeks(data_thinned = data_thinned, 
                          label=("(a)"), 
                          mar = c(3.5,3.5,4.1,1), 
                          cex.lab = 1.5,
                          line.lab = 2.25)

# --- Plot: switch outtake ---

# Define switch location 
switch_start <- 128900
switch_end <- 130000
thin_scale <- 50
data_switch <- data[switch_start:switch_end, ]
rect(xleft = switch_start/thin_scale - 20,
     ybottom = 0, 
     xright = switch_end/thin_scale + 20,
     ytop = 8,
     lty = 2)


par(mar=c(3.5,0,4.1,2.1))
plot.new()
plot.window(xlim=c(1, nrow(data_switch)), ylim=c(0, 8))
for(i in 1:4) lines(data_switch[, i+1], col = cols[i], lwd=0.5)
# rect(xleft = 1, ybottom = 0, xright = nrow(data_switch), ytop = 8, lty=2)
box(lty=2)

legend("topright", c("Cheerful", "Content", "Anxious", "Sad"), 
       lwd = rep(1.5, 4), col = cols, bty='n', cex=0.8)

mtext(text = "(b)", adj=0, line=1, cex=1.5)

dev.off()

# ----------------------------------------------------------------------
# --------- Figure 3: Time Series Plot ---------------------------------
# ----------------------------------------------------------------------

# Recreate Figure 2 for the ESM data

thin90 = seq(1,nrow(data_thinned),18)

pdf("figures/Figure3_ESMTimeSeries.pdf", width = 10, height = 4.5)

# --- Set up Plot ---

lmat <- rbind(1:2)
lo <- layout(mat = lmat, widths = c(1, .4))

# --- Plot: full 2 weeks ---

plotTimeSeriesFirst2Weeks(data_thinned = data_thinned[thin90,],
                          xaxis = thin90,
                          label=("(a)"), 
                          mar = c(3.5,3.5,4.1,1), 
                          cex.lab = 1.5,
                          line.lab = 2.25)

# --- Plot: switch outtake ---

# Define switch location 
switch_start <- 128900 - 900
switch_end <- 130000 + 900
thin_scale <- 50
data_switch <- data[switch_start:switch_end, ]
data_switch90 <- data_switch[seq(1,nrow(data_switch),900),]
rect(xleft = switch_start/thin_scale,
     ybottom = 0, 
     xright = switch_end/thin_scale,
     ytop = 8,
     lty = 2)


par(mar=c(3.5,0,4.1,2.1))
plot.new()
plot.window(xlim=c(1, nrow(data_switch90)), ylim=c(0, 8))
for(i in 1:4) lines(data_switch90[, i+1], col = cols[i], lwd=0.5)
# rect(xleft = 1, ybottom = 0, xright = nrow(data_switch), ytop = 8, lty=2)
box(lty=2)

legend("topright", c("Cheerful", "Content", "Anxious", "Sad"), 
       lwd = rep(1.5, 4), col = cols, bty='n', cex=0.8)

mtext(text = "(b)", adj=0, line=1, cex=1.5)

dev.off()