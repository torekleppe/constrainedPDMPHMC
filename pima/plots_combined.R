if(1==0){
rm(list=ls())
load("Computations")
s <- seq(from=0.2,to=1.5,length.out=14)
p.means.1 <- matrix(0.0,7,length(s))
p.sds.1 <- matrix(0.0,7,length(s))
p.means.2 <- p.means.1
p.sds.2 <- p.sds.1
n.eff.1 <- matrix(0.0,8,length(s))
n.eff.2 <- n.eff.1

for(i in 1:length(s)){
  m1 <- pdmphmc::getMonitor(l.l1[[i]])[[1]]
  p.means.1[,i] <- m1[2:8,"mean"]
  p.sds.1[,i] <- m1[2:8,"sd"]
  t <- sum(get_CPU_time(l.l1[[i]])[,"sampling"])
  n.eff.1[,i] <- round(m1[1:8,"n_eff"]/t)
  m2 <- pdmphmc::getMonitor(l.l2[[i]])[[1]]
  p.means.2[,i] <- m2[2:8,"mean"]
  p.sds.2[,i] <- m2[2:8,"sd"]
  t <- sum(get_CPU_time(l.l2[[i]])[,"sampling"])
  n.eff.2[,i] <- round(m2[1:8,"n_eff"]/t)
}

m.ref <- pdmphmc::getMonitor(fit.uc)[[1]]
ref <- m.ref[2:8,"mean"]
t <- sum(get_CPU_time(fit.uc)[,"sampling"])

n.eff.ref <- round(m.ref[1:8,"n_eff"]/t)


nms <- paste0("beta[",1:7,"]")

l1 <- getSample(l.l1[[1]])
l1 <- l1[,nms]
l2 <- getSample(l.l2[[1]])
l2 <- l2[,nms]
}
pdf("L1L2constr_comb.pdf",width=14,height=14)
par(mfrow=c(2,2))
#
plot(s,rep(ref[1],length(s)),type="l",ylim=c(-0.5,1.5),col="grey",lwd=3,lty=3,
     main="L1 constraint",
     xlab="s",
     ylab="parameter value",
     cex.lab=1.5,cex.main=1.6,cex.axis=1.4)
for(j in 2:7){
  lines(s,rep(ref[j],length(s)),col="grey",lwd=3,lty=3)
}

for(j in 1:7){
  lines(s,p.means.1[j,],col=j,lwd=2)
}
for(j in 1:7){
  xx <- c(s,rev(s))
  yy <- c(p.means.1[j,]-1*p.sds.1[j,],rev(p.means.1[j,]+1*p.sds.1[j,]))
  polygon(x=xx,y=yy,col=adjustcolor(j,alpha.f = 0.2),border=NA)
}

#
plot(s,rep(ref[1],length(s)),type="l",ylim=c(-0.5,1.5),col="grey",lwd=3,lty=3,
     main="L2 constraint",
     xlab="s",
     ylab="parameter value",
     cex.lab=1.5,cex.main=1.6,cex.axis=1.4)
for(j in 2:7){
  lines(s,rep(ref[j],length(s)),col="grey",lwd=3,lty=3)
}

for(j in 1:7){
  lines(s,p.means.2[j,],col=j,lwd=2)
}
for(j in 1:7){
  xx <- c(s,rev(s))
  yy <- c(p.means.2[j,]-1*p.sds.2[j,],rev(p.means.2[j,]+1*p.sds.2[j,]))
  polygon(x=xx,y=yy,col=adjustcolor(j,alpha.f = 0.2),border=NA)
}


#
plot(density(l1[,1]),col=1,xlim=c(-0.1,0.6),ylim=c(0,20),lwd=2,
     main="L1 constraint, s=0.2",
     xlab="parameter value",
     ylab="Posterior density (kernel estimates)",
     cex.lab=1.5,cex.main=1.6,cex.axis=1.4)

lines(c(0,0),c(-1000,1000),col="gray",lty=1,lwd=2)
for(i in 2:7) lines(density(l1[,i]),col=i,lwd=2)


#


plot(density(l2[,1]),col=1,xlim=c(-0.1,0.6),ylim=c(0,20),lwd=2,
     main="L2 constraint, s=0.2",
     xlab="parameter value",
     ylab="Posterior density (kernel estimates)",
     cex.lab=1.5,cex.main=1.6,cex.axis=1.4)

lines(c(0,0),c(-1000,1000),col="gray",lty=1,lwd=2)
for(i in 2:7) lines(density(l2[,i]),col=i,lwd=2)


dev.off()




