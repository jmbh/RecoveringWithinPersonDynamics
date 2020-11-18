# Function to plot correlation and partial correlation functions (DIAGNOSTICS)

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
  lines(y = x1acfs[,i], x=1:max(xaxis), col = alpha(cols[i], alpha=1), lwd=1.5)
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
  lines(y = pcfs[,i], x=1:max(xaxis2), col = alpha(cols[i], alpha=1), lwd=1.5)
}

axis(1)
axis(2)
abline(h = 0, lty = 2)

title(xlab = "Lag", ylab = "Partial Correlation", line=line.lab, cex.lab = cex.lab)
mtext(text = btext, adj=0, line=1, cex=1.5)

legend("topright", c("Partial AC", "Partial CC \n(Between Valence)"), 
       lwd = rep(1.5, 2), col = cols, bty='n', cex=1, yjust = 0)
}
