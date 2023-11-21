rm(list=ls())

load("Computations_15_zeromean")

m.c <- getMonitor(fit.c)[[1]]
m.uc <- getMonitor(fit.uc)[[1]]


tab <- matrix(0.0,4,10)

mon <- m.c
nms <- row.names(mon)
#mu.i <- grep("mu[",nms,fixed = TRUE)
wts.i <- grep("wts[",nms,fixed=TRUE)

tod <- c()
for(j in 1:(n.asset-1)){
  for(i in (j+1):n.asset) tod <- c(tod,paste0("P[",i,",",j,"]"))
}
tdd <- paste0("P[",(1:n.asset),",",(1:n.asset),"]")

# tab[1,1] <- min(mon[mu.i,"n_eff"])
# tab[1,2] <- median(mon[mu.i,"n_eff"])
# tab[1,3] <- max(mon[mu.i,"n_eff"])
# tab[1,4] <- round(min(mon[mu.i,"n_eff"])/sum(get_CPU_time(fit.c)[,"sampling"]),2)
# tab[1,5] <- round(max(mon[mu.i,"Rhat"]),4)

tab[2,1] <- min(mon[tdd,"n_eff"])
tab[2,2] <- median(mon[tdd,"n_eff"])
tab[2,3] <- max(mon[tdd,"n_eff"])
tab[2,4] <- round(min(mon[tdd,"n_eff"])/sum(get_CPU_time(fit.c)[,"sampling"]),2)
tab[2,5] <- round(max(mon[tdd,"Rhat"]),4)

tab[3,1] <- min(mon[tod,"n_eff"])
tab[3,2] <- median(mon[tod,"n_eff"])
tab[3,3] <- max(mon[tod,"n_eff"])
tab[3,4] <- round(min(mon[tod,"n_eff"])/sum(get_CPU_time(fit.c)[,"sampling"]),2)
tab[3,5] <- round(max(mon[tod,"Rhat"]),4)

tab[4,1] <- min(mon[wts.i,"n_eff"])
tab[4,2] <- median(mon[wts.i,"n_eff"])
tab[4,3] <- max(mon[wts.i,"n_eff"])
tab[4,4] <- round(min(mon[wts.i,"n_eff"])/sum(get_CPU_time(fit.c)[,"sampling"]),2)
tab[4,5] <- round(max(mon[wts.i,"Rhat"]),4)

mon <- m.uc
offset <- 5L
# tab[1,1+offset] <- min(mon[mu.i,"n_eff"])
# tab[1,2+offset] <- median(mon[mu.i,"n_eff"])
# tab[1,3+offset] <- max(mon[mu.i,"n_eff"])
# tab[1,4+offset] <- round(min(mon[mu.i,"n_eff"])/sum(get_CPU_time(fit.uc)[,"sampling"]),2)
# tab[1,5+offset] <- round(max(mon[mu.i,"Rhat"]),4)

tab[2,1+offset] <- min(mon[tdd,"n_eff"])
tab[2,2+offset] <- median(mon[tdd,"n_eff"])
tab[2,3+offset] <- max(mon[tdd,"n_eff"])
tab[2,4+offset] <- round(min(mon[tdd,"n_eff"])/sum(get_CPU_time(fit.uc)[,"sampling"]),2)
tab[2,5+offset] <- round(max(mon[tdd,"Rhat"]),4)

tab[3,1+offset] <- min(mon[tod,"n_eff"])
tab[3,2+offset] <- median(mon[tod,"n_eff"])
tab[3,3+offset] <- max(mon[tod,"n_eff"])
tab[3,4+offset] <- round(min(mon[tod,"n_eff"])/sum(get_CPU_time(fit.uc)[,"sampling"]),2)
tab[3,5+offset] <- round(max(mon[tod,"Rhat"]),4)

tab[4,1+offset] <- min(mon[wts.i,"n_eff"])
tab[4,2+offset] <- median(mon[wts.i,"n_eff"])
tab[4,3+offset] <- max(mon[wts.i,"n_eff"])
tab[4,4+offset] <- round(min(mon[wts.i,"n_eff"])/sum(get_CPU_time(fit.uc)[,"sampling"]),2)
tab[4,5+offset] <- round(max(mon[wts.i,"Rhat"]),4)


max(m.c[tod,"sd"]/m.uc[tod,"sd"])
max(m.c[wts.i,"sd"]/m.uc[wts.i,"sd"])
