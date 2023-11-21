rm(list=ls())
load("Computations")

p.means.1 <- matrix(0.0,7,length(s))
p.sds.1 <- matrix(0.0,7,length(s))
p.means.2 <- p.means.1
p.sds.2 <- p.sds.1
n.eff.1 <- matrix(0.0,8,length(s))
n.eff.2 <- n.eff.1
n.eff.1.r <- n.eff.1
n.eff.2.r <- n.eff.1
R.hat.1 <- n.eff.1
R.hat.2 <- n.eff.2

for(i in 1:length(s)){
  m1 <- pdmphmc::getMonitor(l.l1[[i]])[[1]]
  p.means.1[,i] <- m1[2:8,"mean"]
  p.sds.1[,i] <- m1[2:8,"sd"]
  t <- sum(get_CPU_time(l.l1[[i]])[,"sampling"])
  n.eff.1[,i] <- round(m1[1:8,"n_eff"]/t)
  n.eff.1.r[,i] <- round(m1[1:8,"n_eff"])
  R.hat.1[,i] <- m1[1:8,"Rhat"]
  m2 <- pdmphmc::getMonitor(l.l2[[i]])[[1]]
  p.means.2[,i] <- m2[2:8,"mean"]
  p.sds.2[,i] <- m2[2:8,"sd"]
  t <- sum(get_CPU_time(l.l2[[i]])[,"sampling"])
  n.eff.2[,i] <- round(m2[1:8,"n_eff"]/t)
  n.eff.2.r[,i] <- round(m2[1:8,"n_eff"])
  R.hat.2[,i] <- m2[1:8,"Rhat"]
}

m.ref <- pdmphmc::getMonitor(fit.uc)[[1]]
ref <- m.ref[2:8,"mean"]
t <- sum(get_CPU_time(fit.uc)[,"sampling"])

n.eff.ref <- round(m.ref[1:8,"n_eff"]/t)
n.eff.ref.r <- round(m.ref[1:8,"n_eff"])
R.hat.ref <- (m.ref[1:8,"Rhat"])

inds <- c(1,4,9,14)
tab <- c(n.eff.1.r[1,inds],n.eff.2.r[1,inds],n.eff.ref.r[1])
tab <- rbind(tab,c(n.eff.1[1,inds],n.eff.2[1,inds],n.eff.ref[1]))
tab <- rbind(tab,c(R.hat.1[1,inds],R.hat.2[1,inds],Rhat.ref[1]))
tab <- rbind(tab,
             c(apply(n.eff.1.r[2:8,inds],2,min),
               apply(n.eff.2.r[2:8,inds],2,min),
               min(n.eff.ref.r[2:8])))
tab <- rbind(tab,
             c(apply(n.eff.1.r[2:8,inds],2,median),
               apply(n.eff.2.r[2:8,inds],2,median),
               median(n.eff.ref.r[2:8])))

tab <- rbind(tab,
             c(apply(n.eff.1.r[2:8,inds],2,max),
               apply(n.eff.2.r[2:8,inds],2,max),
               max(n.eff.ref.r[2:8])))

tab <- rbind(tab,
             c(apply(n.eff.1[2:8,inds],2,min),
               apply(n.eff.2[2:8,inds],2,min),
               min(n.eff.ref[2:8])))

tab <- rbind(tab,
             c(apply(R.hat.1[2:8,inds],2,min),
               apply(R.hat.2[2:8,inds],2,min),
               min(R.hat.ref[2:8])))
