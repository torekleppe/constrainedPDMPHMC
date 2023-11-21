#rm(list=ls())
#load("Computations")

nms <- paste0("beta[",1:7,"]")

l1 <- getSample(l.l1[[1]])
l1 <- l1[,nms]
l2 <- getSample(l.l2[[3]])
l2 <- l2[,nms]


pdf("L1L2_fixed_se.pdf",width=14,height=7)
par(mfrow=c(1,2))
plot(density(l1[,1]),col=1,xlim=c(-0.1,0.6),ylim=c(0,12),lwd=2,
     main="L1 constraint, s=0.2",
     xlab="parameter value",
     ylab="Posterior density (kernel estimates)")

lines(c(0,0),c(-1000,1000),col="gray",lty=3,lwd=2)
for(i in 2:7) lines(density(l1[,i]),col=i,lwd=2)



plot(density(l2[,1]),col=1,xlim=c(-0.1,0.6),ylim=c(0,12),lwd=2,
     main="L2 constraint, s=0.4",
     xlab="parameter value",
     ylab="Posterior density (kernel estimates)")

lines(c(0,0),c(-1000,1000),col="gray",lty=3,lwd=2)
for(i in 2:7) lines(density(l2[,i]),col=i,lwd=2)
dev.off()


