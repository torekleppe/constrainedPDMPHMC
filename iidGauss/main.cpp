

using namespace amt;




struct model{

  DATA_INT(ctype);
  Eigen::VectorXd y;

  void preProcess(){
    y.resize(4);
    y(0) = -1.0;
    y(1) = -0.3;
    y(2) = 0.3;
    y(3) = 1.2;
  }

  template < class varType, class tensorType, bool storeNames>
  void operator()(amt::amtModel<varType,tensorType,storeNames> &model__){



    PARAMETER_VECTOR(x,2,1.0);



    varType mu;
    varType sigma = exp(x(1));
    model__+= expGamma_ld(x(1),1.0,1.0);

    if(ctype==0){
      mu = exp(x(0));
      model__+=x(0);
    } else {
      mu = x(0);
      model__.sparseLinConstraint(x(0));
    }

    model__+=normal_ld(y,mu,sigma);

    model__.generated(mu,"mu");
    model__.generated(sigma,"sigma");
  }// operator()
};




