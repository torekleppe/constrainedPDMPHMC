library(latex2exp)
rr <- read.table("results.txt")

tols<-unique(rr$tol)
pdf("bias_assess.pdf",width=11,height = 5.5)
par(mfrow=c(1,2))
plot(rr$tol*(1+runif(length(rr$tol),min=-0.1,max=0.1)),rr$mean,log="x",pch=19,cex=0.5,col="grey",xaxt="n",
     xlab="tolerence",ylab="deviation from true value",main=TeX("$E(q_1)$"),
     cex.main=1.6,cex.axis=1.4,cex.lab=1.4)
axis(1,at=sort(tols),las=1,cex.axis=1.4)
lines(c(1e-14,1000),c(0,0),col="red")
for(tol in tols){
  tt <- rr$mean[rr$tol==tol]
  lines(c(0.8*tol,1.2*tol),rep(mean(tt),2),col="black",lwd=2)
}
plot(rr$tol*(1+runif(length(rr$tol),min=-0.1,max=0.1)),rr$sd,log="x",pch=19,cex=0.5,col="grey",xaxt="n",
     xlab="tolerence",ylab="deviation from true value",main=TeX("$SD(q_1)$"),
     cex.main=1.6,cex.axis=1.4,cex.lab=1.4)
axis(1,at=sort(tols),las=1,cex.axis=1.4)
lines(c(1e-14,1000),c(0,0),col="red")
for(tol in tols){
  tt <- rr$sd[rr$tol==tol]
  
  lines(c(0.8*tol,1.2*tol),rep(mean(tt),2),col="black",lwd=2)
}
dev.off()
