rm(list=ls())

set.seed(345345)
y <- numeric(100)
y[1] <- rnorm(1,0,1)
for(i in 2:100) y[i] <- 0.99*y[i-1] + 0.1*rnorm(1)

mdl <- pdmphmc::build("main.cpp",process.type = "HMCProcessConstr",step.type = "RKBS32")
fit.0 <- pdmphmc::run(mdl,data=list(y=y,ctype=0L),chains=10L)
fit.1 <- pdmphmc::run(mdl,data=list(y=y,ctype=1L),chains=10L)

save.image("Computations")

#load("Computations")
m.0 <- getMonitor(fit.0)[[1]]
m.1 <- getMonitor(fit.1)[[1]]
i.0 <- getIntMonitor(fit.0)[[1]]
i.1 <- getIntMonitor(fit.1)[[1]]



tab <- matrix(0.0,2,8)

tab[1:2,1] <- m.0[3:4,"n_eff"]
tab[1:2,2] <- round(m.0[3:4,"n_eff"]/sum(get_CPU_time(fit.0)[,"sampling"]))
tab[1:2,3] <- m.0[3:4,"se_mean"]
tab[1:2,4] <- i.0[1:2,"se_mean"]


tab[1:2,5] <- m.1[3:4,"n_eff"]
tab[1:2,6] <- round(m.1[3:4,"n_eff"]/sum(get_CPU_time(fit.1)[,"sampling"]))
tab[1:2,7] <- m.1[3:4,"se_mean"]
tab[1:2,8] <- i.1[1:2,"se_mean"]

