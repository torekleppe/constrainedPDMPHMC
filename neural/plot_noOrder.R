library(latex2exp)
if(FALSE){
load("Computations_noOrder")
load("Computations")



s <- pdmphmc::getSample(fit)
s.o <- pdmphmc::getSample(fit.s.2)
}

pdf("neural_noOrder.pdf",height = 7,width=14)
par(mfrow=c(2,2))
plot(s[,"wts1_vec[1]"],pch=19,cex=0.2,col=adjustcolor(1,alpha.f = 0.2),ylim=c(-1,1),
     xlab="sample #",
     main=TeX("Input layer bias $\\delta_1$ (black) and $\\delta_2$ (red), unconstrained"),
     ylab="",
     cex.lab=1.4,
     cex.main=1.4,
     cex.axis=1.4)
points(s[,"wts1_vec[2]"],pch=19,cex=0.2,col=adjustcolor(2,alpha.f = 0.2))
for(i in 0:8){lines(c(i*1000,i*1000),c(-10,10),col="grey")}

plot(s.o[,"wts1_vec[1]"],pch=19,cex=0.2,col=adjustcolor(1,alpha.f = 0.2),ylim=c(-1,1),
     xlab="sample #",
     main=TeX("Input layer bias $\\delta_1$ (black) and $\\delta_2$ (red), with constraints"),
     ylab="",
     cex.lab=1.4,
     cex.main=1.4,
     cex.axis=1.4)
points(s.o[,"wts1_vec[2]"],pch=19,cex=0.2,col=adjustcolor(2,alpha.f = 0.2))
for(i in 0:8){lines(c(i*1000,i*1000),c(-10,10),col="grey")}

plot(s[,"wts2[1]"],pch=19,cex=0.2,col=adjustcolor(1,alpha.f = 0.2),ylim=c(-1.5,1.5),
     xlab="sample #",
     main=TeX("Output layer weights $w_1$ (black) and $w_2$ (red), unconstrained"),
     ylab="",
     cex.lab=1.4,
     cex.main=1.4,
     cex.axis=1.4)
points(s[,"wts2[2]"],pch=19,cex=0.2,col=adjustcolor(2,alpha.f = 0.2))
for(i in 0:8){lines(c(i*1000,i*1000),c(-10,10),col="grey")}

plot(s.o[,"wts2[1]"],pch=19,cex=0.2,col=adjustcolor(1,alpha.f = 0.2),ylim=c(-1.5,1.5),
     xlab="sample #",
     main=TeX("Output layer weights $w_1$ (black) and $w_2$ (red), with constraints"),
     ylab="",
     cex.lab=1.4,
     cex.main=1.4,
     cex.axis=1.4)
points(s.o[,"wts2[2]"],pch=19,cex=0.2,col=adjustcolor(2,alpha.f = 0.2))
for(i in 0:8){lines(c(i*1000,i*1000),c(-10,10),col="grey")}
dev.off()
