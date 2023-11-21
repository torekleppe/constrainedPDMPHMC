rm(list=ls())


l <- read.table("dat_100.txt")
l <- l[,2:dim(l)[2]] # drop dates
n.asset <- 15L
n.time <- 250L
set.seed(1)
i <- sample.int(dim(l)[2],n.asset,replace=FALSE)

dta <- l[1:n.time,i]
dta <- scale(dta,center=TRUE,scale=FALSE)


# find initial condition for the precision matrix
P.raw <- solve(cov(dta))
P.init <- P.raw + 0.5*diag(n.asset) # simply flipping positive off-diagonal elements makes the matrix indefinite
for(j in 1:(n.asset-1)){
  for(i in (j+1):n.asset){
    if(P.raw[i,j]>0.0){
      P.init[i,j] <- -abs(P.raw[i,j])
      P.init[j,i] <- P.init[i,j]
    }
  }
}

priMean <- rep(1.0,n.asset) #diag(solve(cov(dta)))

mdl <- pdmphmc::build("model_asset_zeromean.cpp",step.type = "RKBS32")


fit.c <- pdmphmc::run(mdl,data=list(y=as.matrix(dta),
                                    constrained=1L,
                                    pinit=pdmphmc::SPDmatrix.asvector(P.init),
                                    priMean=priMean),
                      cores=4L,chains=8L)

fit.uc <- pdmphmc::run(mdl,data=list(y=as.matrix(dta),
                                     constrained=0L,
                                     pinit=pdmphmc::SPDmatrix.asvector(P.init),
                                     priMean=priMean),
                       cores=4L,chains=8L)


save.image("Computations_15_zeromean")
