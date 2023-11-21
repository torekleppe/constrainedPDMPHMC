
rm(list=ls())
load("Computations_15_zeromean")
library(pdmphmc)

Sig <- cov(dta)
wts.mle <- solve(Sig,rep(1.0,n.asset))
wts.mle <- wts.mle/sum(wts.mle)

s.c <- pdmphmc::getSample(fit.c)
s.uc <- pdmphmc::getSample(fit.uc)
wts.names <- paste0("wts[",(1:n.asset),"]")
n.sample <- dim(s.c)[1]
tgrid <- seq(from=0,to=0.5,length.out=n.sample)


pdf("wts15_short.pdf",width = 14,height = 7)
plot(-10,-10,xlim=c(0.5,n.asset+0.5),ylim=c(-0.2,0.6),
     xlab="asset #",ylab="portfolio weight",
     xaxt="n",cex.lab=1.4,cex.axis=1.4)
axis(1, at = 1:n.asset, las=1,cex=1.4)

for(i in 1:n.asset) lines(tgrid+(i-1)+0.5,s.c[,wts.names[i]],col="black",lwd=0.1)
for(i in 1:n.asset) lines(tgrid+(i-1)+1.0,s.uc[,wts.names[i]],col="darkgrey",lwd=0.1)
for(i in 1:(n.asset+1)) lines(c(i-1+0.5,i-1+0.5),c(-10,10),col="red")
#for(i in 1:n.asset) lines(c(-0.5+i,0.5+i),rep(wts.mle[i],2),col="green",lty=3,lwd=3)

dev.off()

print(sqrt(diag(cov(s.c[,wts.names])))/sqrt(diag(cov(s.uc[,wts.names]))))

m.uc <- getMonitor(fit.uc)[[1]]
m.c <- getMonitor(fit.c)[[1]]
mn <- (m.uc[,"mean"])
tt <- c()
for(j in 1:(n.asset-1)){
  for(i in (j+1):n.asset) tt <- c(tt,paste0("P[",i,",",j,"]"))
}
e.names <- names(sort(mn[tt],decreasing = TRUE))

pdf("offdiag15_short.pdf",height = 7,width=14)
par(mfrow=c(1,1))
nn <- length(e.names)
plot(-10,-10,xlim=c(0,nn-1),ylim=c(-0.5,0.5),
     xlab="off-diagonal element",ylab="90% interval",
     cex.lab=1.4,cex.axis=1.4)
polygon(c(1:nn,rev(1:nn)),c(m.uc[e.names,"Q5"],rev(m.uc[e.names,"Q95"])),col="grey",border = NA)
polygon(c(1:nn,rev(1:nn)),c(m.c[e.names,"Q5"],rev(m.c[e.names,"Q95"])),col="black")
legend(x="topright",legend=c("unconstrained","MTP2"),col=c("grey","black"),lwd=c(20,20),cex=1.5)
lines(c(-100,nn+100),c(0,0),col="red")

dev.off()
