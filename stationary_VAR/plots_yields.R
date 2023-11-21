if(FALSE){
  load("Computations_yield")
  library(pdmphmc)

  s <- pdmphmc::getSample(fit.c)
  su <- pdmphmc::getSample(fit.uc)

  jj.A <- grep("A[",colnames(s),fixed = TRUE)
  jj.alp <- grep("alpha[",colnames(s),fixed = TRUE)
  nn <- ncol(dta)

  jj.P <- grep("P[1,1]",colnames(s),fixed = TRUE)
  P <- matrix(0,nn,nn)
  P.u <- matrix(0,nn,nn)
  ones <- rep(1.0,nn)
  ones.outer <- ones%*%t(ones)

  init <- dta[nrow(dta),]
  np <- 24
  np.irf <- 24

  sam <- array(0.0,dim=c(nn,nrow(s),np))
  sam.u <- sam

  irf <- array(0.0,dim=c(nrow(s),np.irf,ncol(dta)))
  irf.u <- irf

  rho <- numeric(nrow(s))
  rho.u <- rho

  for(i in 1:nrow(s)){
    al <- s[i,jj.alp]
    A <- matrix(s[i,jj.A],nrow=nn,ncol=nn,byrow=TRUE)
    al.u <- su[i,jj.alp]
    A.u <- matrix(su[i,jj.A],nrow=nn,ncol=nn,byrow=TRUE)

    rho[i] <- max(abs(eigen(A)$values))
    rho.u[i] <- max(abs(eigen(A.u)$values))

    k <- 0
    for(jj in 1:nn){
      for(ii in jj:nn){
        P[ii,jj] <- s[i,jj.P+k]
        P[jj,ii] <- P[ii,jj]
        P.u[ii,jj] <- su[i,jj.P+k]
        P.u[jj,ii] <- P.u[ii,jj]
        k <- k+1
      }
    }

    pred <- init
    pred.u <- init
    for(t in 1:np){
      pred <- al + A%*%pred
      pred.u <- al.u + A.u%*%pred.u
      sam[,i,t] <- pred
      sam.u[,i,t] <- pred.u
    }

    Sig <- solve(P)
    Sig.u <- solve(P.u)
    W <- t(chol(Sig))
    dd <- diag(1/(diag(W)))
    W <- W%*%dd
    W.u <- t(chol(Sig.u))
    dd <- diag(1/diag(W.u))
    W.u <- W.u%*%dd

    phi <- diag(ncol(dta))
    phi.u <- diag(ncol(dta))
    for(jj in 1:np.irf){
      phi <- phi%*%A
      irf[i,jj,] <- (phi%*%W)[,1]
      phi.u <- phi.u%*%A.u
      irf.u[i,jj,] <- (phi.u%*%W.u)[,1]
    }


  }
}

if(FALSE){
  pdf("implulse_response.pdf",width = 14,height = 9)
  par(mfrow=c(2,3))
  for(jj in 1:6){
    qs <- apply(irf[,,jj],2,quantile,probs=c(0.25,0.5,0.75))
    plot(-10,-10,xlim=c(0,np.irf),ylim=c(0.3,1),
         main=TeX(paste0("3m $\\rightarrow$ ",colnames(dta)[jj],"m")),
         xlab="months after shock",
         ylab="impulse response function",
         cex.lab=1.4,
         cex.main=1.6,
         cex.axis=1.4)
    polygon(c(1:np.irf,rev(1:np.irf)),c(qs[1,],rev(qs[3,])),
            col=adjustcolor(1,alpha.f = 0.2),border=NA)
    lines(1:np.irf,qs[2,],type="l",lwd=3)

    qs <- apply(irf.u[,,jj],2,quantile,probs=c(0.25,0.5,0.75))
    polygon(c(1:np.irf,rev(1:np.irf)),c(qs[1,],rev(qs[3,])),
            col=adjustcolor(2,alpha.f = 0.2),border=NA)
    lines(1:np.irf,qs[2,],type="l",col="red",lwd=3,lty=2)
    if(jj==1){
      legend(x="bottomleft",
             legend=c("stationarity-constrained, posterior median",
                      "stationarity-constrained, post. 50% interval",
                      "unconstrained, posterior median",
                      "unconstrained, post. 50% interval"),
             lty=c(1,1,2,1),
             col=c(1,adjustcolor(1,alpha.f = 0.2),2,adjustcolor(2,alpha.f = 0.2)),
             lwd=c(3,10,3,10),
             cex=1.4)

    }
  }

  dev.off()


}

if(TRUE){

  pdf("VAR_rho_histogram.pdf",height = 7,width = 14)
  par(mfrow=c(1,2))
  hist(rho,probability=TRUE,breaks=15,xlim=c(0.94,1.05),ylim=c(0,50),border=NA,
       main="spectral radius of B: histogram",
       xlab=TeX("$\\rho(B)$"),
       cex.axis=1.4,
       cex.lab=1.4,
       cex.main=1.6)
  hist(rho.u,probability=TRUE,add=TRUE,breaks=25,col="black",density=20)

  legend("topleft",legend=c("stationarity-constrained","unconstrained"),
         col=c("NA","black"),fill=c("grey","black"),
         density=c(NA,20),border=c("NA","black"),
         horiz=FALSE,
         cex=1.4)

  plot(1:length(rho.u),rho.u,col=adjustcolor(2,alpha.f = 0.2),pch=19,cex=0.3,
       ylim=c(0.95,1.05),
       xlab="sample #",
       ylab=TeX("$\\rho(B)$"),
       main="spectral radius of B: samples",
       cex.axis=1.2,
       cex.lab=1.1,
       cex.main=1.6
  )
  points(1:length(rho),rho,col=adjustcolor(1,alpha.f = 0.2),pch=19,cex=0.3)
  for(jj in 0:8){ lines(c(jj*1000,jj*1000),c(-1,10),col="grey")}

  legend("topleft",legend=c("stationarity-constrained","unconstrained"),
         col=c(adjustcolor(1,alpha.f = 0.2),adjustcolor(2,alpha.f = 0.2)),
         pch=c(19,19),
         horiz=FALSE,
         cex=1.4)

  dev.off()
}

if(FALSE){
pdf("prediction_yield.pdf",height = 11.7,width=8.3)
par(mfrow=c(nn,2),
    mai=c(0.55,0.55,0.15,0.2),
    oma = c(0.2, 0.2, 0.2, 0.2))

ylims <- matrix(c(0,3,
                  0,3,
                  0,3,
                  1,4,
                  2,5,
                  3.0,6.0),nrow = 6,byrow = TRUE)
n.before <- 8

cnms <- colnames(dta)

for(j in 1:nn){

  xl <- NA
  if(j==nn) xl <- "periods after last observation"

  mn <- "stationarity-constrained pred. mean"
  mnu <- "unconstrained predictive mean"

  if(j>1){
    mn <- ""
    mnu <- ""
  }
  yl <-

  tt <- apply(sam[j,,],2,quantile,probs=c(0.25,0.5,0.75))

  plot(-100,-100,ylim=ylims[j,],xlim=c(-(n.before-1),np),
       xlab=xl,main=mn,ylab=paste0(cnms[j]," months"),
       cex.lab=1.4,
       cex.main=1.4,
       cex.axis=1.4)
  polygon(c(1:np,rev(1:np)),c(tt[1,],rev(tt[3,])),
          col=adjustcolor(1,alpha.f = 0.2),border=NA)
  lines(1:np,tt[2,],type="l",lwd=2)
  lines((-(n.before-1)):0,dta[((nrow(dta)-n.before+1):nrow(dta)),j])

  tt <- apply(sam.u[j,,],2,quantile,probs=c(0.25,0.5,0.75))

  plot(-100,-100,ylim=ylims[j,],xlim=c(-(n.before-1),np),
       xlab=xl,main=mnu,ylab="",
       cex.lab=1.4,
       cex.main=1.4,
       cex.axis=1.4)
  polygon(c(1:np,rev(1:np)),c(tt[1,],rev(tt[3,])),
          col=adjustcolor(1,alpha.f = 0.2),border=NA)
  lines(1:np,tt[2,],type="l",lwd=2)
  lines((-(n.before-1)):0,dta[((nrow(dta)-n.before+1):nrow(dta)),j])
}


dev.off()
}
