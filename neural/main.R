rm(list=ls())

set.seed(123)
train.x.sim <- cbind(rep(1.0,200),matrix(rnorm(200*2),nrow=200))
eta1 <- train.x.sim%*%c(0.5,1.0,0.0)
eta2 <- train.x.sim%*%c(-0.5,-0.1,1.0)
pred <- (-1.0+2.0*exp(eta1)/(1+exp(eta1))) + (-1.0+2.0*exp(eta2)/(1+exp(eta2)))
train.y.sim <- as.vector(pred + 0.1*rnorm(200))



dta <- read.table("prostate.data")
xp <- scale(dta,TRUE,TRUE)
y <- xp[,"lpsa"]
x <- cbind(rep(1.0,97),xp[,1:8])



mdl <- pdmphmc::build("model.cpp",step.type="RKBS32")


fit.s.0 <- pdmphmc::run(mdl,data=list(y=train.y.sim,x=train.x.sim,D=0L),cores=4L,chains = 8L)
fit.s.1 <- pdmphmc::run(mdl,data=list(y=train.y.sim,x=train.x.sim,D=1L),cores=4L,chains = 8L)
fit.s.2 <- pdmphmc::run(mdl,data=list(y=train.y.sim,x=train.x.sim,D=2L),cores=4L,chains = 8L)
fit.s.4 <- pdmphmc::run(mdl,data=list(y=train.y.sim,x=train.x.sim,D=4L),cores=4L,chains = 8L)
fit.s.8 <- pdmphmc::run(mdl,data=list(y=train.y.sim,x=train.x.sim,D=8L),cores=4L,chains = 8L)

save.image("Computations")

fit.0 <- pdmphmc::run(mdl,data=list(y=y,x=x,D=0L),cores=4L,chains = 8L)
fit.1 <- pdmphmc::run(mdl,data=list(y=y,x=x,D=1L),cores=4L,chains = 8L)
fit.2 <- pdmphmc::run(mdl,data=list(y=y,x=x,D=2L),cores=4L,chains = 8L)
fit.4 <- pdmphmc::run(mdl,data=list(y=y,x=x,D=4L),cores=4L,chains = 8L)
fit.8 <- pdmphmc::run(mdl,data=list(y=y,x=x,D=8L),cores=4L,chains = 8L)

save.image("Computations")
