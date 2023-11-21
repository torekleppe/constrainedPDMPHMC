library(pdmphmc)
library("latex2exp")

mdl <- pdmphmc::build("main.cpp",process.type = "HMCProcessConstr",step.type = "RKBS32")

fit.0 <- run(mdl,data=list(ctype=0L))
fit.1 <- run(mdl,data=list(ctype=1L))
fit.2 <- run(mdl,data=list(ctype=2L))
fit.3 <- run(mdl,data=list(ctype=3L))

pdf("Ill.pdf",width = 14,height = 7)
par(mfrow=c(1,4))
main<- "Linear Constraint"
plot(getSample(fit.0),xlim=c(-3,3),ylim=c(-3,3),pch=20,cex=0.1,xlab=TeX("$q_1$"),ylab=TeX("$q_2$"),main=main,lwd=2,
     cex.lab=1.5,cex.axis=1.3,cex.main=1.5)
curve(0.5*(x+1),from=-4,to=4,add=TRUE,col="red")

main <- "L1 Constraint"
plot(getSample(fit.1),xlim=c(-3,3),ylim=c(-3,3),pch=20,cex=0.1,xlab=TeX("$q_1$"),ylab=TeX("$q_2$"),main=main,lwd=2,
     cex.lab=1.5,cex.axis=1.3,cex.main=1.5)
curve(-abs(x-0.5)+0.5*x+19/10,from=-1.5,to=2.5,add=TRUE,col="red")
curve(abs(x-0.5)+0.5*x-21/10,from=-1.5,to=2.5,add=TRUE,col="red")

main <- "L2 Constraint"
plot(getSample(fit.2),xlim=c(-3,3),ylim=c(-3,3),pch=20,cex=0.1,xlab=TeX("$q_1$"),ylab=TeX("$q_2$"),main=main,lwd=2,
     cex.lab=1.5,cex.axis=1.3,cex.main=1.5)
curve(0.5*x-0.1+0.5*sqrt(-4*x^2+4*x+15),from=-1.5,to=2.5,add=TRUE,col="red")
curve(0.5*x-0.1-0.5*sqrt(-4*x^2+4*x+15),from=-1.5,to=2.5,add=TRUE,col="red")

main <- "Non-linear Constraint"
plot(getSample(fit.3),xlim=c(-3,3),ylim=c(-3,3),pch=20,cex=0.1,xlab=TeX("$q_1$"),ylab=TeX("$q_2$"),main=main,lwd=2,
     cex.lab=1.5,cex.axis=1.3,cex.main=1.5)
curve(1/(50*x),from=-4,to=-1.0e-7,add=TRUE,col="red")
curve(1/(50*x),from=1.0e-7,to=4,add=TRUE,col="red")
curve(-7/(25*x),from=-4,to=-1.0e-7,add=TRUE,col="red")
curve(-7/(25*x),from=1.0e-7,to=4,add=TRUE,col="red")
dev.off()



