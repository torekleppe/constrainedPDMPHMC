


using namespace amt;

class constraintFun : public constraintFunctor{
  size_t n_;
  inline size_t VlinInd(const size_t i, const size_t j) const {
    return(n_*(j+1) - ((j+1)*j)/2 + i - j - 1);
  }
public:
  void setup(const int d){
    n_ = static_cast<size_t>(d);

  }
  double operator()(const Eigen::VectorXd& arg) const {
    Eigen::VectorXd Lam_ = arg.head(n_).array().exp().matrix();
    double ret = -1.0e100;
    double t;
    for(size_t j=0;j<n_-1;j++){
      for(size_t i=j+1;i<n_;i++){
        t = Lam_.coeff(j)*arg.coeff(VlinInd(i,j));
        for(size_t k=0;k<j;k++){
          t+=arg.coeff(VlinInd(i,k))*arg.coeff(VlinInd(j,k))*Lam_.coeff(k);
        }
        ret = std::max(t,ret);
      }
    }
    //std::cout << "max offdiag: " << ret << std::endl;
    return -ret;
  }

  double operator()(const Eigen::VectorXd& arg,
                  Eigen::VectorXd& grad) const {
    Eigen::VectorXd Lam_ = arg.head(n_).array().exp().matrix();
    double ret = -1.0e100;
    double t;
    size_t ii,jj;
    for(size_t j=0;j<n_-1;j++){
      for(size_t i=j+1;i<n_;i++){
        t = Lam_.coeff(j)*arg.coeff(VlinInd(i,j));
        for(size_t k=0;k<j;k++){
          t+=arg.coeff(VlinInd(i,k))*arg.coeff(VlinInd(j,k))*Lam_.coeff(k);
        }
        if(t>ret){
          ret = t;
          ii = i;
          jj = j;
        }
      }
    }

    //std::cout << "i: " << ii << " j: " << jj << std::endl;
    //std::cout << "max offdiag: " << ret << std::endl;
    if(grad.size()!=arg.size()) grad.resize(arg.size());
    grad.setZero();
    grad.coeffRef(VlinInd(ii,jj)) = Lam_.coeff(jj);
    grad.coeffRef(jj) = Lam_.coeff(jj)*arg.coeff(VlinInd(ii,jj));

    for(size_t k=0;k<jj;k++){
      grad.coeffRef(VlinInd(ii,k)) += arg.coeff(VlinInd(jj,k))*Lam_.coeff(k);
      grad.coeffRef(VlinInd(jj,k)) += arg.coeff(VlinInd(ii,k))*Lam_.coeff(k);
      grad.coeffRef(k) += arg.coeff(VlinInd(ii,k))*arg.coeff(VlinInd(jj,k))*Lam_.coeff(k);
    }
    grad = -grad;
    return -ret;
  }
  std::string name() const {return "SPDmatrix positivity";}
};



struct model{


  DATA_MATRIX(y);
  DATA_INT(constrained);
  DATA_VECTOR(pinit);
  DATA_VECTOR(priMean);

  constraintFun cf;

  Eigen::MatrixXd yt;
  Eigen::VectorXd means;
  Eigen::VectorXd pdiag;
  Eigen::VectorXd ones;
  Eigen::VectorXd mu;
  int d;


  void preProcess(){
    yt = y.transpose();
    d=yt.rows();
    pdiag = (1.0/static_cast<double>(d+10))*priMean;
    cf.setup(d);
    ones.setConstant(d,1.0);
    mu.setConstant(d,0.0);
  }

  template < class varType, class tensorType, bool storeNames>
  void operator()(amt::amtModel<varType,tensorType,storeNames> &model__){


    PARAMETER_VECTOR(prec_core,(d*(d+1))/2,pinit);

    SPDmatrix P(d,prec_core);


    model__ += wishartDiagScale_ld(P,pdiag,static_cast<double>(d+10));

    model__ += iid_multi_normal_prec_ld(yt,mu,P);
    model__.generated(P,"P");

    if(constrained>0) model__.sparseLinFunConstraint(prec_core,cf);


    // portfolio weights
    Eigen::VectorXd wts = P.vecProd_double(ones);
    wts = (1.0/wts.dot(ones))*wts;
    model__.generated(wts,"wts");


  }// operator()
};

