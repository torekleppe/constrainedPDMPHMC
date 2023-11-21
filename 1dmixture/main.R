rm(list=ls())

set.seed(2345)

y <- c(rnorm(100,-0.5,1),rnorm(100,0.5,1))

mdl <- pdmphmc::build("main.cpp",process.type = "HMCProcessConstr",step.type = "RKBS32")


fit.0 <- pdmphmc::run(mdl,data=list(y=y,ctype=0L),chains=10L)
fit.1 <- pdmphmc::run(mdl,data=list(y=y,ctype=1L),chains=10L)

save.image("Computations")

#load("Computations")

m.0 <- getMonitor(fit.0)[[1]]
m.1 <- getMonitor(fit.1)[[1]]
i.0 <- getIntMonitor(fit.0)[[1]]
i.1 <- getIntMonitor(fit.1)[[1]]



tab <- matrix(0.0,3,8)

tab[,1] <- m.0[4:6,"n_eff"]
tab[,2] <- round(m.0[4:6,"n_eff"]/sum(get_CPU_time(fit.0)[,"sampling"]))
tab[,3] <- m.0[4:6,"se_mean"]
tab[,4] <- i.0[1:3,"se_mean"]


tab[,5] <- m.1[4:6,"n_eff"]
tab[,6] <- round(m.1[4:6,"n_eff"]/sum(get_CPU_time(fit.1)[,"sampling"]))
tab[,7] <- m.1[4:6,"se_mean"]
tab[,8] <- i.1[1:3,"se_mean"]
