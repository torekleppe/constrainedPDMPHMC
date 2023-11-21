rm(list=ls())
load("Computations")

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


pdf("L1L2constr.pdf",width=14,height=7)
par(mfrow=c(1,2))
plot(s,rep(ref[1],length(s)),type="l",ylim=c(-0.5,1.5),col="grey",lwd=3,lty=3,
     main="L1 constraint",
     xlab="s",
     ylab="parameter value")
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



plot(s,rep(ref[1],length(s)),type="l",ylim=c(-0.5,1.5),col="grey",lwd=3,lty=3,
     main="L2 constraint",
     xlab="s",
     ylab="parameter value")
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

dev.off()




