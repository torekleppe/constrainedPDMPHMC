

using namespace amt;




struct model{

  DATA_VECTOR(y);
  DATA_INT(ctype);


  void preProcess(){}

  template < class varType, class tensorType, bool storeNames>
  void operator()(amt::amtModel<varType,tensorType,storeNames> &model__){



    PARAMETER_VECTOR(x,2,1.0);



    varType phi;
    varType sigma = exp(x(1));
    model__+= expGamma_ld(x(1),1.0,1.0);

    if(ctype==0){
      phi = inv_logit(x(0));
      model__+=invLogitUniform_ld(x(0));
    } else {
      phi = x(0);
      model__.sparseLinConstraint(x(0));
      model__.sparseLinConstraint(1.0-x(0));
    }

    for(size_t i=1;i<y.size();i++) model__+=normal_ld(y.coeff(i),phi*y.coeff(i-1),sigma);

    model__.generated(phi,"phi");
    model__.generated(sigma,"sigma");
  }// operator()
};




