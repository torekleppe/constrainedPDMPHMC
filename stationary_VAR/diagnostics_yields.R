load("Computations_yield")


s.uc <- pdmphmc::getSample(fit.uc)
s.c <- pdmphmc::getSample(fit.c)
nms <- colnames(s.uc)
jj <- grep("A[",nms,fixed=TRUE)

nsample <- nrow(s.uc)
rho.c <- numeric(nsample)
rho.uc <- numeric(nsample)
ndta <- ncol(dta)
for(i in 1:nsample){
  m <- matrix(s.c[i,jj],ndta,ndta,byrow=TRUE)
  rho.c[i] <- max(abs(eigen(m)$values))
  m <- matrix(s.uc[i,jj],ndta,ndta,byrow=TRUE)
  rho.uc[i] <- max(abs(eigen(m)$values))
}

hist(rho.uc)
hist(rho.c,add=TRUE)



