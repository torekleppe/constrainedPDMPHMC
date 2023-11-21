rm(list=ls())

set.seed(123)
train.x.sim <- cbind(rep(1.0,200),matrix(rnorm(200*2),nrow=200))
eta1 <- train.x.sim%*%c(0.5,1.0,0.0)
eta2 <- train.x.sim%*%c(-0.5,-0.1,1.0)
pred <- (-1.0+2.0*exp(eta1)/(1+exp(eta1))) + (-1.0+2.0*exp(eta2)/(1+exp(eta2)))
train.y.sim <- as.vector(pred + 0.1*rnorm(200))



mdl <- pdmphmc::build("model_noOrder.cpp",step.type = "RKBS32")
fit <- pdmphmc::run(mdl,data=list(y=train.y.sim,x=train.x.sim,D=2L),cores=4L,chains = 8L)

save.image("Computations_noOrder")
