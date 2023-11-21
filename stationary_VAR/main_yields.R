l <- as.data.frame(readxl::read_xlsx("Moench_Surprises_YieldData.xlsx"))


dta <- as.matrix(l[,c("3","6","12","24","60","120")])


# preconditioning to improve performace
TT <- nrow(dta)
reg.mat <- cbind(rep(1.0,TT-1),dta[1:(TT-1),])

AA <- c()
for(i in 1:ncol(dta)){
  bet <- lm.fit(reg.mat,dta[2:TT,i])$coefficients
  AA <- rbind(AA,bet)
}

resi <- matrix(0.0,ncol(dta),TT-1)
for(i in 1:(TT-1)){
  resi[,i] <- dta[i+1,] - AA%*%reg.mat[i,]
}

S <- diag(ncol(dta))
for(i in 1:(TT-1)){
  S <- S + resi[,i]%*%t(resi[,i])
}
S <- solve(S)





model <- pdmphmc::build("fullVar.cpp",step.type = "RKBS32")

fit.c <- pdmphmc::run(model,data=list(ys=dta,S=S,constrained=1L),cores=4L,chains=8L)
fit.uc <- pdmphmc::run(model,data=list(ys=dta,S=S,constrained=0L),cores=4L,chains=8L)

save.image("Computations_yield")



