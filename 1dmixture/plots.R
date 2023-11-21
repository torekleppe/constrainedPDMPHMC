rm(list=ls())
library(latex2exp)
load("Computations")

s.0 <- pdmphmc::getSample(fit.0)
s.1 <- pdmphmc::getSample(fit.1)

pdf("mixtureScatter.pdf",width=14,height = 7)
par(mfrow=c(1,2))
plot(s.0[,"x[1]"],s.0[,"x[2]"],
     xlab=TeX("$\\mu_1$"),
     ylab=TeX("$\\log(\\mu_2-\\mu_1)$"),
     main="unconstrained transformed parameters",
     pch=19,cex=0.1,cex.lab=1.4,cex.axis=1.4)
plot(s.1[,"x[1]"],s.1[,"x[2]"],
     xlab=TeX("$\\mu_1$"),
     ylab=TeX("$\\mu_2$"),
     main="constrained parameters",
     pch=19,cex=0.1,cex.lab=1.4,cex.axis=1.4)
abline(0,1,col="red",lwd=2)
dev.off()



