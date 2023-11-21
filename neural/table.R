load("Computations")
require(pdmphmc)


bias <- pdmphmc::getSample(fit.8)[,2:9]
d <- dim(bias)[1]

plot(1:d,bias[,1],type="l",ylim=c(-4,4))
for(i in 2:8){
  lines(1:d,bias[,i],col=i)
}


tab <- matrix(0.0,9,8)



m <- getMonitor(fit.s.1)[[1]]
np <- dim(m)[1]
tt <- sum(get_CPU_time(fit.s.1)[,"sampling"])
kk <- 1
tab[1,kk] <- round(m["sigma","mean"],4)
tab[2,kk] <- round(m["sigma","n_eff"])
tab[3,kk] <- round(m["sigma","n_eff"]/tt,1)
tab[4,kk] <- m["sigma","Rhat"]
tab[5,kk] <- round(min(m[2:(np-1),"n_eff"]))
tab[6,kk] <- round(median(m[2:(np-1),"n_eff"]))
tab[7,kk] <- round(max(m[2:(np-1),"n_eff"]))
tab[8,kk] <- round(min(m[2:(np-1),"n_eff"])/tt,1)
tab[9,kk] <- max(m[2:(np-1),"Rhat"])


m <- getMonitor(fit.s.2)[[1]]
np <- dim(m)[1]
tt <- sum(get_CPU_time(fit.s.2)[,"sampling"])
kk <- 2
tab[1,kk] <- round(m["sigma","mean"],4)
tab[2,kk] <- round(m["sigma","n_eff"])
tab[3,kk] <- round(m["sigma","n_eff"]/tt,1)
tab[4,kk] <- m["sigma","Rhat"]
tab[5,kk] <- round(min(m[2:(np-1),"n_eff"]))
tab[6,kk] <- round(median(m[2:(np-1),"n_eff"]))
tab[7,kk] <- round(max(m[2:(np-1),"n_eff"]))
tab[8,kk] <- round(min(m[2:(np-1),"n_eff"])/tt,1)
tab[9,kk] <- max(m[2:(np-1),"Rhat"])

m <- getMonitor(fit.s.4)[[1]]
np <- dim(m)[1]
tt <- sum(get_CPU_time(fit.s.4)[,"sampling"])
kk <- 3
tab[1,kk] <- round(m["sigma","mean"],4)
tab[2,kk] <- round(m["sigma","n_eff"])
tab[3,kk] <- round(m["sigma","n_eff"]/tt,1)
tab[4,kk] <- m["sigma","Rhat"]
tab[5,kk] <- round(min(m[2:(np-1),"n_eff"]))
tab[6,kk] <- round(median(m[2:(np-1),"n_eff"]))
tab[7,kk] <- round(max(m[2:(np-1),"n_eff"]))
tab[8,kk] <- round(min(m[2:(np-1),"n_eff"])/tt,1)
tab[9,kk] <- max(m[2:(np-1),"Rhat"])


m <- getMonitor(fit.s.8)[[1]]
np <- dim(m)[1]
tt <- sum(get_CPU_time(fit.s.8)[,"sampling"])
kk <- 4
tab[1,kk] <- round(m["sigma","mean"],4)
tab[2,kk] <- round(m["sigma","n_eff"])
tab[3,kk] <- round(m["sigma","n_eff"]/tt,1)
tab[4,kk] <- m["sigma","Rhat"]
tab[5,kk] <- round(min(m[2:(np-1),"n_eff"]))
tab[6,kk] <- round(median(m[2:(np-1),"n_eff"]))
tab[7,kk] <- round(max(m[2:(np-1),"n_eff"]))
tab[8,kk] <- round(min(m[2:(np-1),"n_eff"])/tt,1)
tab[9,kk] <- max(m[2:(np-1),"Rhat"])





m <- getMonitor(fit.1)[[1]]
np <- dim(m)[1]
tt <- sum(get_CPU_time(fit.1)[,"sampling"])
kk <- 5
tab[1,kk] <- round(m["sigma","mean"],4)
tab[2,kk] <- round(m["sigma","n_eff"])
tab[3,kk] <- round(m["sigma","n_eff"]/tt,1)
tab[4,kk] <- m["sigma","Rhat"]
tab[5,kk] <- round(min(m[2:(np-1),"n_eff"]))
tab[6,kk] <- round(median(m[2:(np-1),"n_eff"]))
tab[7,kk] <- round(max(m[2:(np-1),"n_eff"]))
tab[8,kk] <- round(min(m[2:(np-1),"n_eff"])/tt,1)
tab[9,kk] <- max(m[2:(np-1),"Rhat"])


m <- getMonitor(fit.2)[[1]]
np <- dim(m)[1]
tt <- sum(get_CPU_time(fit.2)[,"sampling"])
kk <- 6
tab[1,kk] <- round(m["sigma","mean"],4)
tab[2,kk] <- round(m["sigma","n_eff"])
tab[3,kk] <- round(m["sigma","n_eff"]/tt,1)
tab[4,kk] <- m["sigma","Rhat"]
tab[5,kk] <- round(min(m[2:(np-1),"n_eff"]))
tab[6,kk] <- round(median(m[2:(np-1),"n_eff"]))
tab[7,kk] <- round(max(m[2:(np-1),"n_eff"]))
tab[8,kk] <- round(min(m[2:(np-1),"n_eff"])/tt,1)
tab[9,kk] <- max(m[2:(np-1),"Rhat"])


m <- getMonitor(fit.4)[[1]]
np <- dim(m)[1]
tt <- sum(get_CPU_time(fit.4)[,"sampling"])
kk <- 7
tab[1,kk] <- round(m["sigma","mean"],4)
tab[2,kk] <- round(m["sigma","n_eff"])
tab[3,kk] <- round(m["sigma","n_eff"]/tt,1)
tab[4,kk] <- m["sigma","Rhat"]
tab[5,kk] <- round(min(m[2:(np-1),"n_eff"]))
tab[6,kk] <- round(median(m[2:(np-1),"n_eff"]))
tab[7,kk] <- round(max(m[2:(np-1),"n_eff"]))
tab[8,kk] <- round(min(m[2:(np-1),"n_eff"])/tt,1)
tab[9,kk] <- max(m[2:(np-1),"Rhat"])


m <- getMonitor(fit.8)[[1]]
np <- dim(m)[1]
tt <- sum(get_CPU_time(fit.8)[,"sampling"])
kk <- 8
tab[1,kk] <- round(m["sigma","mean"],4)
tab[2,kk] <- round(m["sigma","n_eff"])
tab[3,kk] <- round(m["sigma","n_eff"]/tt,1)
tab[4,kk] <- m["sigma","Rhat"]
tab[5,kk] <- round(min(m[2:(np-1),"n_eff"]))
tab[6,kk] <- round(median(m[2:(np-1),"n_eff"]))
tab[7,kk] <- round(max(m[2:(np-1),"n_eff"]))
tab[8,kk] <- round(min(m[2:(np-1),"n_eff"])/tt,1)
tab[9,kk] <- max(m[2:(np-1),"Rhat"])


