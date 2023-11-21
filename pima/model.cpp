using namespace amt;



struct model{


  DATA_MATRIX(x);
  DATA_IVECTOR(y);
  DATA_DOUBLE(L1thresh);
  DATA_DOUBLE(L2thresh);


  void preProcess(){

  }

  template < class varType, class tensorType, bool storeNames>
  void operator()(amt::amtModel<varType,tensorType,storeNames> &model__){
    PARAMETER_SCALAR(intercept,-1.0);
    PARAMETER_VECTOR(beta,7);



    // prior
    model__+=normal_ld(intercept,0.0,10.0);
    model__+=normal_ld(beta,0.0,10.0);

    if(L1thresh>0.0) model__.sparseLinL1Constraint(beta,L1thresh);
    if(L2thresh>0.0) model__.sparseLinL2Constraint(beta,L2thresh);

    // likelihood
    VectorXv eta;
    matVecProd(x,beta,eta);
    for(size_t i=0;i<eta.size();i++) eta.coeffRef(i) += intercept;
    model__+=bernoulli_logit_lm(y,eta);

    model__.generated(asDouble(beta),"beta_g");
    model__.generated(asDouble(intercept),"inte_g");
    double L1 = asDouble(beta).array().abs().sum();
    model__.generated(L1,"L1");
    double L2 = asDouble(beta).norm();
    model__.generated(L2,"L2");



  }// operator()
};




