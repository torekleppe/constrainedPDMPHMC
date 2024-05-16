library(pdmphmc)
library("latex2exp")

#mdl <- pdmphmc::build("main.cpp",process.type = "HMCProcessConstr",step.type = "RKBS32")

true.mean <- -0.14448909068631568433
true.sd <- 0.97108219528578667124

n.rep <- 100
res <- matrix(0.0,nrow = n.rep,ncol=3)

ctrl <- pdmphmc::pdmphmc_control()

tols <- c(1.0e-3,1.0e-4,1.0e-5,1.0e-6)
res <- matrix(0.0,nrow = n.rep*length(tols),ncol=3)

i <- 1

for(tol in tols){
ctrl$absTol <- tol
ctrl$relTol <- tol



for(rep in 1:n.rep){
  fit.0 <- run(mdl,data=list(ctype=0L),control = ctrl,seed=rep)
  m <- pdmphmc::getMonitor(fit.0,print=FALSE)[[1]]
  res[i,1] <- m[1,"mean"] - true.mean
  res[i,2] <- m[1,"sd"] - true.sd
  res[i,3] <- ctrl$absTol
  i <- i+1
}
}

colnames(res) <- c("mean","sd","tol")
write.table(res,"results.txt")
