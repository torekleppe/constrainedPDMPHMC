using namespace amt;



struct model{


  DATA_VECTOR(y);
  DATA_MATRIX(x)
    DATA_INT(D); // number of hidden neurons


  int N;
  int P;

  Eigen::VectorXd wts1Init;
  linearPreconditioner toBeta;


  void preProcess(){
    N = x.rows();
    P = x.cols();
    if(D>=1){
      wts1Init.resize(D*P);
      wts1Init.setZero();
      if(D>1) for(size_t i=0;i<D;i++) wts1Init(i) = -1.0 + 2.0*static_cast<double>(i)/static_cast<double>(D-1);
    }
    if(D<=0) toBeta.setRegressionPar(y,x);

    std::cout << "N : " << N << " P : " << P << " D : " << D << std::endl;

  }

  template < class varType, class tensorType, bool storeNames>
  void operator()(amt::amtModel<varType,tensorType,storeNames> &model__){
    PARAMETER_SCALAR(logSigma);
    model__+= expGamma_ld(logSigma,1.0,1.0);
    varType sigma = exp(logSigma);
    model__.generated(asDouble(sigma),"sigma");


    if(D<=0){
      // linear model
      PARAMETER_VECTOR(betaStd,P);
      VectorXv beta = toBeta(betaStd);
      model__.generated(beta,"beta");

      model__+= normal_ld(beta,0.0,100.0);

      VectorXv eta;
      matVecProd(x,beta,eta);
      model__ += normal_ld(y,eta,sigma);
    } else {


      PARAMETER_VECTOR(wts1_vec,P*D); //,wts1Init);
      MatrixXv wts1(P,D);
      size_t k=0;
      for(size_t i=0;i<P;i++){
        for(size_t j=0;j<D;j++){
          wts1.coeffRef(i,j) = wts1_vec.coeff(k);
          k++;
        }
      }
      PARAMETER_VECTOR(wts2,D);
      PARAMETER_SCALAR(bias2);

      model__+= normal_ld(wts1_vec,0.0,1.0);
      model__+= normal_ld(wts2,0.0,1.0);
      model__+= normal_ld(bias2,0.0,1.0);

      //for(size_t j=0;j<D;j++) model__.sparseLinConstraint(wts2.coeff(j));
      //for(size_t j=1;j<D;j++) model__.sparseLinConstraint(wts1_vec.coeff(j)-wts1_vec.coeff(j-1));

      VectorXv pred(N);
      VectorXv eta;
      for(size_t i=0;i<N;i++) pred.coeffRef(i) = bias2;

      for(size_t j=0;j<D;j++){
        matVecProd(x,wts1.col(j),eta);
        for(size_t i=0;i<N;i++) pred.coeffRef(i) += wts2.coeff(j)*(-1.0+2.0*inv_logit(eta.coeff(i)));
      }
      model__ += normal_ld(y,pred,sigma);
    }

/*
    // single neuron model
    PARAMETER_VECTOR(wts1,P);
    PARAMETER_SCALAR(bias2);
    PARAMETER_SCALAR(wt2,1.0);
    model__+= normal_ld(wts1,0.0,10.0);
    model__+= normal_ld(bias2,0.0,10.0);
    model__+= normal_ld(wt2,0.0,10.0);

    //model__.generated(wts1,"wts1");
    model__.sparseLinConstraint(wt2);


    VectorXv eta1;
    matVecProd(x,wts1,eta1);
    for(size_t i=0;i<eta1.size();i++) eta1.coeffRef(i) = bias2 +  wt2*(-1.0+2.0*inv_logit(eta1.coeff(i)));
    model__ += normal_ld(y,eta1,sigma);
*/

    /*
     // simple two-lag neural network model


     PARAMETER_MATRIX(wt1,2,D);
     PARAMETER_VECTOR(bias1,D,biasInit);
     PARAMETER_VECTOR(wt2,D);
     PARAMETER_SCALAR(bias2);
     VectorXv hidden(D);
     varType pred;

     for(int j=1;j<D;j++) model__.sparseLinConstraint(bias1.coeff(j)-bias1.coeff(j-1));

     for(int j=0;j<D;j++) for(int k=0;k<2;k++) model__+= normal_ld(wt1.coeff(k,j),0.0,1.0);
     model__+= normal_ld(bias1,0.0,1.0);
     model__+= normal_ld(wt2,0.0,1.0);
     model__+= normal_ld(bias2,0.0,1.0);



     for(int t=2;t<T;t++){
     pred = bias2;
     for(int j=0;j<D;j++){
     hidden.coeffRef(j) = inv_logit(y.coeff(t-2)*wt1.coeff(0,j)+y.coeff(t-1)*wt1.coeff(1,j)+bias1.coeff(j));
     pred += hidden.coeff(j)*wt2.coeff(j);
     }
     model__+=normal_ld(y.coeff(t),pred,sigma);
     }


     model__.generated(asDouble(hidden),"hidden");
     */


  }// operator()
};




