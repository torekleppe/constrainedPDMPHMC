load("Computations_15_zeromean")

s <- pdmphmc::getSample(fit.c)
s.u <- pdmphmc::getSample(fit.uc)


jj <- grep("prec_core[",colnames(s),fixed=TRUE)
max.ele <- numeric(nrow(s))
max.ele.u <- numeric(nrow(s))

for(i in 1:nrow(s)){
  P <- pdmphmc::SPDmatrix.asmatrix(s[i,jj])
  diag(P) <- rep(-10000,ncol(dta))
  max.ele[i] <- max(P)
  P.u <- pdmphmc::SPDmatrix.asmatrix(s.u[i,jj])
  diag(P.u) <- rep(-100000,ncol(dta))
  max.ele.u[i] <- max(P.u)
}


