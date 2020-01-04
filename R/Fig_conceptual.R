library(wesanderson)
mypal <- wes_palette("Chevalier1")

ar1
ar1 <- build_time_series()
mydata <- (ar1 - min(ar1))/(max(ar1) - min(ar1))

cex_titles <- .8

# Require an output from any of the 3 functions sampling_regular, sampling_random, sampling_iterative
pol_break <- function(cpt, col=1, y1=-.2, y2=1.2) {
  if(col==1) mycol=mypal[1]
  if(col==2) mycol=mypal[2]
  x1   <- cpt$changepoint
  x2   <- cpt$matrix$index[which(cpt$matrix$index==cpt$changepoint)-1]
  pol_x <- c(x1, x2, x2, x1)
  pol_y <- c(y1, y1, y2, y2)
  polygon(pol_x, pol_y, col= adjustcolor(mycol, alpha.f = .3), density = NULL, border = NA)
}

pdf(paste0(getwd(),"/Output/Fig_conceptual.pdf"), fonts = "Helvetica")

layout(matrix(c(1,2,0,
         3,4,5,
         0,0,6,
         0,0,7), ncol=3, byrow = T))
par(mar=c(1,2,6,2))

# Plot 1 -- all time series ####
plot(c(1,100),c(0,1), pch=NA, axes=F, xlab="",ylab="")
axis(1, lwd=2, at = c(-10,120))
axis(2, lwd=2, at = c(-1,2))
for (i in 1:50) {
  ar1 <- build_time_series(shift_time = sample(30:45, size = 1))
  ar1 <- (ar1 - min(ar1))/(max(ar1) - min(ar1))
  lines(ar1, lwd=.8, col=adjustcolor("grey", alpha.f = .4))
}
mydata <- ar1
lines(mydata)
mtext("a. 50 simulations", side = 3, line = 1, adj = 0, font=1, cex=cex_titles)

# Plot 2 -- example time series ####
plot(mydata, col="black", type="l", lwd=1.5, axes=F, xlab="",ylab="")
axis(1, lwd=2, at = c(-10,120))
axis(2, lwd=2, at = c(-1,2))
cp <- sample_regular(mydata, n=length(mydata), messages = F)
pol_break(cpt = cp)
lines(mydata, col="black", lwd=1.5)
text(cp$changepoint, .8, labels = "'real' \nchange point",pos = 4, col=mypal[1])
mtext("b. Example time-series", side = 3, line = 1, adj = 0, font=1, cex=cex_titles)

# Plot 3 -- random sampling ####
plot(mydata, col=adjustcolor("grey", alpha.f = .4), type="l", lwd=1.5, axes=F, xlab="",ylab="")
axis(1, lwd=2, at = c(-10,120))
axis(2, lwd=2, at = c(-1,2))
cp_random <- sample_random(mydata, n=15, messages = F)
points(cp_random$matrix$index, cp_random$matrix$x, pch=4, lwd=2)
pol_break(cpt = cp, col=1)
pol_break(cpt = cp_random, col=2)
lines(mydata, col="black", lwd=1.5)
text(100,0.95,labels = paste0("n = ",cp_random$final_n), pos = 2)
mtext("c. Random sampling", side = 3, line = 1, adj = 0, font=1, cex=cex_titles)

# Plot 4 -- regular sampling ####
plot(mydata, col=adjustcolor("grey", alpha.f = .4), type="l", lwd=1.5, axes=F, xlab="",ylab="")
axis(1, lwd=2, at = c(-10,120))
axis(2, lwd=2, at = c(-1,2))
cp_regular <- sample_regular(mydata, n=15, messages = F)
points(cp_regular$matrix$index, cp_regular$matrix$x, pch=4, lwd=2)
pol_break(cpt = cp, col=1)
pol_break(cpt = cp_regular, col=2)
lines(mydata, col="black", lwd=1.5)
text(100,0.95,labels = paste0("n = ",cp_regular$final_n), pos = 2)
mtext("d. Regular sampling", side = 3, line = 1, adj = 0, font=1, cex=cex_titles)

# Plot 5 -- iterative sampling (1/3) ####
plot(mydata, col=adjustcolor("grey", alpha.f = .4), type="l", lwd=1.5, axes=F, xlab="",ylab="")
cp_iterative <- sample_regular(mydata, n=5, messages = F)
pol_break(cpt = cp, col=1)
pol_break(cpt = cp_iterative, col=2)
lines(mydata, col="black", lwd=1.5)
axis(1, lwd=2, at = c(-10,120))
axis(2, lwd=2, at = c(-1,2))
points(cp_iterative$matrix$index, cp_iterative$matrix$x, pch=4, lwd=2)
text(100,0.95,labels = paste0("n = ",cp_iterative$final_n), pos = 2)
mtext("e. Iterative sampling (initial)", side = 3, line = 1, adj = 0, font=1, cex=cex_titles)
xleft_temp <- cp_iterative$matrix$index[which(cp_iterative$matrix$index==cp_iterative$changepoint)-1]-4 
xright_temp <- cp_iterative$changepoint+4 
par(xpd=T)
rect(xleft = xleft_temp, xright = xright_temp, ybottom = -0.1, ytop = 1.1, lty = 2)
text(xright_temp, 1, labels = "f", pos = 4)
par(xpd=F)

# Plot 6 -- iterative sampling (2/3) ####
plot(mydata, col=adjustcolor("grey", alpha.f = .4), type="l", lwd=1.5, axes=F, xlab="",ylab="", xlim=c(xleft_temp,xright_temp))
cp_iterative1 <- sample_regular(mydata, n=5, messages = F)
cp_iterative2 <- sample_iterative(mydata, n=6, messages = F)
cp_iterative3 <- sample_iterative(mydata, n=7, messages = F)
cp_iterative4 <- sample_iterative(mydata, n=8, messages = F)
axis(1, lwd=2, at = c(-10,120))
axis(2, lwd=2, at = c(-1,2))
mypal2 <- wes_palette("GrandBudapest2")
points(cp_iterative1$matrix$index, cp_iterative$matrix$x, col=grey(0.5), pch=4, lwd=2)
points(cp_iterative2$matrix$index[!cp_iterative2$matrix$index%in%cp_iterative1$matrix$index], cp_iterative2$matrix$x[!cp_iterative2$matrix$index%in%cp_iterative1$matrix$index], 
       col=mypal2[1], pch=4, lwd=2)
points(cp_iterative3$matrix$index[!cp_iterative3$matrix$index%in%cp_iterative2$matrix$index], cp_iterative3$matrix$x[!cp_iterative3$matrix$index%in%cp_iterative2$matrix$index], 
       col=mypal2[2], pch=4, lwd=2)
points(cp_iterative4$matrix$index[!cp_iterative4$matrix$index%in%cp_iterative3$matrix$index], cp_iterative4$matrix$x[!cp_iterative4$matrix$index%in%cp_iterative3$matrix$index], 
       col=mypal2[3], pch=4, lwd=2)
legend("topright", legend = c("t=0", "t=1", "t=2", "t=3"),
       col = c(grey(0.5), mypal2), pch=4, lwd=2, bty="n", lty=NA)
mtext("f. Zoom from e\n(slowly adding samples)", side = 3, line = 1, adj = 0, font=1, cex=cex_titles)

# Plot 7 -- iterative sampling (3/3) ####
plot(mydata, col=adjustcolor("grey", alpha.f = .4), type="l", lwd=1.5, axes=F, xlab="",ylab="")
cp_iterative <- sample_iterative(mydata, n=15, messages = F)
pol_break(cpt = cp, col=1)
pol_break(cpt = cp_iterative, col=2)
lines(mydata, col="black", lwd=1.5)
axis(1, lwd=2, at = c(-10,120))
axis(2, lwd=2, at = c(-1,2))
points(cp_iterative$matrix$index, cp_iterative$matrix$x, pch=4, lwd=2)
text(100,0.95,labels = paste0("n = ",cp_iterative$final_n), pos = 2)
mtext("g. Iterative sampling (final)", side = 3, line = 1, adj = 0, font=1, cex=cex_titles)

dev.off()
