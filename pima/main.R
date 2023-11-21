rm(list=ls())
library(MASS)
library(pdmphmc)
data(Pima.te)
data(Pima.tr)
dta <- rbind(Pima.te,Pima.tr)
for(i in 1:7){
  dta[,i] <- (dta[,i]-mean(dta[,i]))/sd(dta[,i])
}

glm.fit <- glm(type~npreg+glu+bp+skin+bmi+ped+age,family = binomial(),data=dta)

nc <- glm.fit$coefficients[2:8]
nc.L1 <- sum(abs(nc))
nc.L2 <- sqrt(sum(nc^2))

x <- as.matrix(dta[,1:7])
y <- as.integer(dta[,8]=="Yes")



mdl <- pdmphmc::build("model.cpp",step.type = "RKBS32")

s <- seq(from=0.2,to=1.5,length.out=14)
#s <- c(0.3,0.7,1.0,1.5)
l.l1 <- list()
l.l2 <- list()
for(ss in s){
  print(paste0("L1, s=",ss))
  fit.l1 <- pdmphmc::run(mdl,data=list(x=x,y=y,L1thresh=ss*nc.L1,L2thresh=-1.0),cores=4L,chains=8L)
  l.l1 <- c(l.l1,fit.l1)
  print(paste0("L2, s=",ss))
 fit.l2 <- pdmphmc::run(mdl,data=list(x=x,y=y,L1thresh=-1.0,L2thresh=ss*nc.L2),cores=4L,chains=8L)
 l.l2 <- c(l.l2,fit.l2)
}

fit.uc <- pdmphmc::run(mdl,data=list(x=x,y=y,L1thresh=-1.0,L2thresh=-1.0),cores=4L,chains=8L)

save.image("Computations")

