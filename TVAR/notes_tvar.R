obj <- readRDS("TVAR/tvar_est_ESM.RDS")

dataTVAR <- as.data.frame(obj$model)
dataTVAR$regime <-regime(obj)

dtTVAR <- as.matrix(dataTVAR)

VAR_reg2_TRUE <- lm(dtTVAR[,1:4]~dtTVAR[,3:5]-1)

tvar.est <- TVAR(data[,2:5],lag=1,mTh=1, around = 2.8, ngrid = 100) 

dataTVAR$regime

pos_low <- dataTVAR[dataTVAR$regime==1,][,c(6,2)]
pos_high <- dataTVAR[dataTVAR$regime==2,][,c(11,2)]

neg_low <- dataTVAR[dataTVAR$regime==1,][,c(7,3)]
neg_high <- dataTVAR[dataTVAR$regime==2,][,c(12,3)]


plot.new()
plot.window(xlim=c(0, 8), ylim=c(0,8))
points(pos_high[seq(1,nrow(pos_high), by = 10),], col = "green")
abline(lm(pos_high[,2]~pos_high[,1]), col = "darkgreen")
cor12_high <- cor(pos_high[,2], pos_high[,1], use = "complete.obs")
text(6.5, 6, bquote(bold(rho == .(round(cor12_high,3)))), col="darkgreen")

points(pos_low[seq(1,nrow(pos_low), by = 10),], col = "red")
abline(lm(pos_low[,2]~pos_low[,1]), col = "darkred")
cor12_low <- cor(pos_low[,2], pos_low[,1], use = "complete.obs")
text(6.5, 2, bquote(bold(rho == .(round(cor12_low,3)))), col="darkred")
rect(xleft = 7.25, ybottom = 7.25, xright = 9, ytop = 9, border = FALSE, col = "white")
title(ylab=expression("Content"[t]), xlab=expression("Cheerful"[t-1]), line=2.2)
axis(1)
axis(2, las =2)

plot.new()
plot.window(xlim=c(0, 8), ylim=c(0,8))
points(neg_high[seq(1,nrow(neg_high), by = 10),], col = "green")
abline(lm(neg_high[,2]~neg_high[,1]), col = "darkgreen")
cor23_high <- cor(neg_high[,2], neg_high[,1], use = "complete.obs")
text(7, 2, bquote(bold(rho == .(round(cor23_high,3)))), col="darkgreen")

points(neg_low[seq(1,nrow(neg_low), by = 10),], col = "red")
abline(lm(neg_low[,2]~neg_low[,1]), col = "darkred")
cor23_low <- cor(neg_low[,2], neg_low[,1], use = "complete.obs")
text(6.5, 4, bquote(bold(rho == .(round(cor23_low,3)))), col="darkred")
rect(xleft = 7.25, ybottom = 7.25, xright = 9, ytop = 9, border = FALSE, col = "white")
title(ylab=expression("Anxious"[t]), xlab=expression("Content"[t-1]), line=2.2)
axis(1)
axis(2, las =2)
