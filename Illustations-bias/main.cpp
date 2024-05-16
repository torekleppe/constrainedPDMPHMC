

using namespace amt;



class cstr : public constraintFunctor {

  template <class vType>
  inline vType eval(const Eigen::Matrix<vType,Eigen::Dynamic,1> arg) const {
    Eigen::Matrix<vType,Eigen::Dynamic,Eigen::Dynamic> m(2,2);
    m(0,0) = 0.8;
    m(1,0) = arg(0);
    m(0,1) = arg(1);
    m(1,1) = 0.9;
    return(1.0-FMVAD::spectralRadius(m));
  }

public:
  inline double operator()(const Eigen::VectorXd& arg) const {
    return(eval<double>(arg));
  }
  inline double operator()(const Eigen::VectorXd& arg,
                         Eigen::VectorXd& grad) const {
    FMVAD::FMVADvar ret = eval<FMVAD::FMVADvar>(FMVAD::independent(arg));
    grad = gradient(ret);
    return(ret.val());
  }
  inline std::string name() const {return "cstr";}
};




struct model{

  DATA_INT(ctype);
  cstr fun;


  void preProcess(){

  }

  template < class varType, class tensorType, bool storeNames>
  void operator()(amt::amtModel<varType,tensorType,storeNames> &model__){


    double rho=0.75;
    PARAMETER_VECTOR(x,2,0.0);
    PARAMETER_VECTOR(y,2,0.0);

    model__+= normal_ld(x(0),0.0,1.0);
    model__+= normal_ld(x(1),rho*x(0),sqrt(1.0-rho*rho));

    model__+= normal_ld(y,0.0,1.0);


    VectorXv yy(2);
    yy(0) = x(0)-0.5;
    yy(1) = x(1)-0.5*x(0)+0.1;

    if(ctype==0){
      model__.sparseLinConstraint(x(0)-2.0*x(1)+1.0);
    } else if(ctype==1){
      model__.sparseLinL1Constraint(yy,2.0);
    } else if(ctype==2){
      model__.sparseLinL2Constraint(yy,2.0);
    } else {
      model__.sparseLinFunConstraint(x,fun);
    }

  }// operator()
};




