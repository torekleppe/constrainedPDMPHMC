using namespace amt;

class cstr : public constraintFunctor {

  template <class vType>
  inline vType eval(const Eigen::Matrix<vType,Eigen::Dynamic,1> arg) const {
    int dim = std::sqrt(arg.size());

    Eigen::Matrix<vType,Eigen::Dynamic,Eigen::Dynamic> m(dim,dim);
    for(size_t i=0;i<dim;i++) m.row(i) = arg.segment(i*dim,dim);
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

  cstr fTor;
  DATA_MATRIX(ys);
  DATA_INT(constrained);
  DATA_MATRIX(S);
  size_t T;
  size_t dim;
  //Eigen::MatrixXd yvec;
  Eigen::MatrixXd yconst;
  Eigen::VectorXd zeroInit;
  Eigen::VectorXd w;

  std::vector<linearPreconditioner> toARow;
  wishartPreconditioner toP;
  void preProcess(){
    std::cout << "#rows: " << ys.rows() << std::endl;
    std::cout << "#cols: " << ys.cols() << std::endl;
    T = ys.rows();
    dim = ys.cols();

    yconst.resize(T-1,dim+1);
    yconst.col(0).setOnes();
    yconst.rightCols(dim) = ys.topRows(T-1);

    zeroInit.resize(dim*(dim+1));
    Eigen::VectorXd f(dim+1);


    toARow.resize(dim);
    for(int i=0;i<dim;i++){
      toARow[i].setRegressionPar(ys.col(i).tail(T-1),yconst);
      //toARow[i].printSummary();

      f.setConstant(dim+1,0.01);
      f.coeffRef(i+1) = 0.9;
      zeroInit.segment(i*(dim+1),dim+1) = toARow[i].toStdPar(f);

    }

    w.setConstant(dim,1.0);
    toP.setPars(static_cast<double>(dim+10+T-1),S);
  }

  template < class varType, class tensorType, bool storeNames>
  void operator()(amt::amtModel<varType,tensorType,storeNames> &model__){
    PARAMETER_VECTOR(beta_vec_std,(dim+1)*dim,zeroInit);
    PARAMETER_VECTOR(Pstd,toP.vecDim());
    SPDmatrix<varType> P = toP(model__,Pstd);

    MatrixXv Ap(dim,dim+1);
    VectorXv beta_std,beta;
    VectorXv A_vec(dim*dim);
    Eigen::MatrixXd A(dim,dim);
    Eigen::VectorXd alpha(dim);

    for(int i=0;i<dim;i++){
      // preconditioning
      beta_std = beta_vec_std.segment(i*(dim+1),dim+1);
      beta = toARow[i](beta_std);

      // store elements representing matrix A in a single vector (to be passed to constraint)
      A_vec.segment(i*dim,dim) = beta.tail(dim);

      // store for generated quantities
      alpha.coeffRef(i) = asDouble(beta.coeff(0));
      A.row(i) = asDouble(beta).tail(dim);

      // store for likelihood calculations
      Ap.row(i) = beta;

      // prior
      model__ += normal_ld(beta,0.0,4.0);
    }

    // time loop
    VectorXv mu;
    Eigen::VectorXd yy;
    for(int t=0; t<T-1; t++){

      matVecProd(Ap,yconst.row(t).transpose(),mu);
      yy = ys.row(t+1).transpose();
      model__ += multi_normal_prec_ld(yy,mu,P);
    }

    model__.generated(model__.getTargetDouble(),"loglike");


    // priors
    model__+=wishartDiagScale_ld(P,w,static_cast<double>(dim+10));


    // constraint
    if(constrained>0) model__.sparseLinFunConstraint(A_vec,fTor);

    model__.generated(beta_vec_std,"beta_std");
    model__.generated(alpha,"alpha");
    model__.generated(A,"A");
    model__.generated(P,"P");



  }// operator()
};




