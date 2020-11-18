# Analysis file for CT-VAR(1) model
source("archive/fun_plots_ctvar.R")

library(RColorBrewer)
cols <- brewer.pal(4, "Dark2")
cols <- matrix(cols,4,4,byrow=T)

# --------------------------------------------------------
# ------------------- Ideal data -------------------------
# --------------------------------------------------------

ctideal <- readRDS("linearSDE_esm.RDS")
ideal_sum <- summary(ctideal)

point_ideal <- ideal_sum$parmatrices
drift_ideal <- matrix(
  point_ideal[
    rownames(point_ideal) == "DRIFT", "Mean"],
  4,4)


print(round(drift_ideal,3))

round(expm::expm(drift_ideal*.1),2)

# diffusion ?

# integral?
dts <- seq(0,90,.1)
phidt_ideal <- get_phidt_ar(drift_ideal,dts)

# Cross-lagged effects
phidt_plot_subset(abs(phidt_ideal), dts, smat = t(combn(seq(1:4), 2)), cols = cols, ylim = c(0,.4),
                  xlines = F, legtf = FALSE,
                  xl = 6, yl= .3, lwd = 2)
# auto-regressive effects
phidt_plot_subset(phidt_ideal, dts, smat = cbind(1:4, 1:4), cols = cols, ylim = c(-1,1),
                  xlines = F, legtf = TRUE,
                  xl = 6, yl= .3)

plot(dts, phidt_ideal[2,1,], type = "l", col = "black")
lines(dts, -1*phidt_ideal[2,3,], col = "red")
# --------------------------------------------------------
# ------------------- ESM data -------------------------
# --------------------------------------------------------

ctesm <- readRDS("linearSDE_ESM.RDS")
esm_sum <- summary(ctesm)

point_esm <- esm_sum$parmatrices

drift_esm <- matrix(
  point_esm[
    rownames(point_esm) == "DRIFT", "Mean"],
  4,4)

print(round(drift_esm,3))

round(expm::expm(drift_esm*.04),2)



dts <- seq(0,9,.01)

phidt_esm <- get_phidt_ar(drift_esm,dts)

# Cross-lagged effects
phidt_plot_subset(phidt_esm, dts, smat = t(combn(seq(1:4), 2)), cols = cols, ylim = c(-.05,.15),
                  xlines = F, legtf = TRUE,
                  xl = 6, yl= .3)
abline(v = .5)
# auto-regressive effects
phidt_plot_subset(phidt_esm, dts, smat = cbind(1:4, 1:4), cols = cols, ylim = c(-1,1),
                  xlines = F, legtf = TRUE,
                  xl = 6, yl= .3)
abline(v = .5)


phidt_esm[,,dts == 2.5]

netplot(drift_ideal, greyscale = TRUE)

