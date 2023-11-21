

using namespace amt;



template <class varType,class pType>
varType oneDmixture(const Eigen::VectorXd& y,
                    const pType p,
                    const varType mu1,
                    const varType mu2,
                    const varType sigma1,
                    const varType sigma2){

  varType ret = 0.0;
  varType lp1,lp2;
  pType logp = log(p);
  pType logq = log(1.0-p);
  for(size_t i=0;i<y.size();i++){
    lp1 = logp + normal_lpdf(y.coeff(i),mu1,sigma1);
    lp2 = logq + normal_lpdf(y.coeff(i),mu2,sigma2);
    if(asDouble(lp1)>asDouble(lp2)){
      ret += lp1 + log(1.0+exp(lp2-lp1));
    } else {
      ret += lp2 + log(1.0+exp(lp1-lp2));
    }
  }
  return(ret);
}


struct model{
  DATA_VECTOR(y);
  DATA_INT(ctype);


  void preProcess(){}

  template < class varType, class tensorType, bool storeNames>
  void operator()(amt::amtModel<varType,tensorType,storeNames> &model__){



    PARAMETER_VECTOR(x,2,1.0);
    PARAMETER_SCALAR(lambda);

    varType sigma = exp(lambda);
    model__+= expGamma_ld(lambda,1.0,1.0);

    varType mu1,mu2;

    if(ctype==0){
      mu1 = x(0);
      mu2 = mu1 + exp(x(1));
      model__+=x(1);
    } else {
      mu1 = x(0);
      mu2 = x(1);
      model__.sparseLinConstraint(x(1)-x(0));
    }

    model__+=oneDmixture(y,0.5,mu1,mu2,sigma,sigma);

    model__.generated(mu1,"mu1");
    model__.generated(mu2,"mu2");
    model__.generated(sigma,"sigma");
  }// operator()
};




