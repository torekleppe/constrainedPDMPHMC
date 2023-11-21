rm(list=ls())
load("Computations_yield")

m.c <- pdmphmc::getMonitor(fit.c)[[1]]
m.uc <- pdmphmc::getMonitor(fit.uc)[[1]]

tab <- matrix(0.0,3,10)
nms <- row.names(m.c)

i.A <- grep("A[",nms,fixed=TRUE)
i.alp <- grep("alpha[",nms,fixed=TRUE)
i.prec <- grep("P[",nms,fixed=TRUE)



mon <- m.c
tab[1,1] <- min(mon[i.A,"n_eff"])
tab[1,2] <- median(mon[i.A,"n_eff"])
tab[1,3] <- max(mon[i.A,"n_eff"])
tab[1,4] <- round(min(mon[i.A,"n_eff"])/sum(pdmphmc::get_CPU_time(fit.c)[,"sampling"]),1)
tab[1,5] <- round(max(mon[i.A,"Rhat"]),4)

tab[2,1] <- min(mon[i.alp,"n_eff"])
tab[2,2] <- median(mon[i.alp,"n_eff"])
tab[2,3] <- max(mon[i.alp,"n_eff"])
tab[2,4] <- round(min(mon[i.alp,"n_eff"])/sum(pdmphmc::get_CPU_time(fit.c)[,"sampling"]),1)
tab[2,5] <- round(max(mon[i.alp,"Rhat"]),4)

tab[3,1] <- min(mon[i.prec,"n_eff"])
tab[3,2] <- median(mon[i.prec,"n_eff"])
tab[3,3] <- max(mon[i.prec,"n_eff"])
tab[3,4] <- round(min(mon[i.prec,"n_eff"])/sum(pdmphmc::get_CPU_time(fit.c)[,"sampling"]),1)
tab[3,5] <- round(max(mon[i.prec,"Rhat"]),4)

mon <- m.uc
offset <- 5L
tab[1,1+offset] <- min(mon[i.A,"n_eff"])
tab[1,2+offset] <- median(mon[i.A,"n_eff"])
tab[1,3+offset] <- max(mon[i.A,"n_eff"])
tab[1,4+offset] <- round(min(mon[i.A,"n_eff"])/sum(pdmphmc::get_CPU_time(fit.uc)[,"sampling"]),1)
tab[1,5+offset] <- round(max(mon[i.A,"Rhat"]),4)

tab[2,1+offset] <- min(mon[i.alp,"n_eff"])
tab[2,2+offset] <- median(mon[i.alp,"n_eff"])
tab[2,3+offset] <- max(mon[i.alp,"n_eff"])
tab[2,4+offset] <- round(min(mon[i.alp,"n_eff"])/sum(pdmphmc::get_CPU_time(fit.uc)[,"sampling"]),1)
tab[2,5+offset] <- round(max(mon[i.alp,"Rhat"]),4)

tab[3,1+offset] <- min(mon[i.prec,"n_eff"])
tab[3,2+offset] <- median(mon[i.prec,"n_eff"])
tab[3,3+offset] <- max(mon[i.prec,"n_eff"])
tab[3,4+offset] <- round(min(mon[i.prec,"n_eff"])/sum(pdmphmc::get_CPU_time(fit.uc)[,"sampling"]),1)
tab[3,5+offset] <- round(max(mon[i.prec,"Rhat"]),4)


m1<-matrix(m.c[i.A,"mean"],6,6,byrow=TRUE)
mc <- m1
s1<-matrix(m.c[i.A,"sd"],6,6,byrow=TRUE)
sc <- s1
a.c <- round(rbind(m1[1,],s1[1,],m1[2,],s1[2,],m1[3,],s1[3,],m1[4,],s1[4,],m1[5,],s1[5,],m1[6,],s1[6,]),3)

m1<-matrix(m.uc[i.A,"mean"],6,6,byrow=TRUE)
muc <- m1
s1<-matrix(m.uc[i.A,"sd"],6,6,byrow=TRUE)
suc <- s1
a.uc <- round(rbind(m1[1,],s1[1,],m1[2,],s1[2,],m1[3,],s1[3,],m1[4,],s1[4,],m1[5,],s1[5,],m1[6,],s1[6,]),3)


