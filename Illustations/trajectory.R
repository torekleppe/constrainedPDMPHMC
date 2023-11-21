library(pdmphmc)


mdl <- pdmphmc::build("main.cpp",process.type = "HMCProcessConstr",
                      step.type = "RKBS32",TM.type="identityTM",
                      include = "-D _NO_IPS_ -D _STORE_EVENT_STATES_")


ctrl <- pdmphmc_control()
ctrl$lambda <- c(3.0,1.0,0)
fit <- pdmphmc::run(mdl,data=list(ctype=0L),
                    Tmax=25.0,chains = 1,
                    control = ctrl,samples=500000L,
                    seed=32)
traj <- getSample(fit,include.warmup = TRUE)
diag <- fit@diagnostics[[1]]
pdf("traj.pdf",height = 7,width = 14)
plot(traj[,1],traj[,2],type="l",xlab=TeX("$q_1$"),ylab=TeX("$q_2$"),lwd=2,
     cex.lab=1.5,cex.axis=1.3,cex.main=1.5)
curve(0.5*(x+1),from=-4,to=4,add=TRUE,col="red")
points(0,0,pch=7,col="blue")
for(i in 1:dim(diag)[1]){
  if(diag[i,"eventType"]==0){
    points(diag[i,"state_0"],diag[i,"state_1"],pch=1)
  } else if(diag[i,"eventType"]==2){
    points(diag[i,"state_0"],diag[i,"state_1"],col="red",pch=0)
  }
}
dev.off()

