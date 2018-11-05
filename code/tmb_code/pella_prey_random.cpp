// Space time 
#include <TMB.hpp>

// Posfun - penalize when below 0
template<class Type>
Type posfun(Type x, Type eps, Type &pen){
  pen += CppAD::CondExpLt(x,eps,Type(0.01)*pow(x-eps,2),Type(0));
  return CppAD::CondExpGe(x,eps,x,eps/(Type(2)-x/eps));
}

template<class Type>
Type objective_function<Type>::operator() ()
{
  // Data
  DATA_INTEGER(Nstocks);
  DATA_INTEGER(Nobs);
  DATA_SCALAR(p);
  DATA_FACTOR(StockID);
  DATA_VECTOR(B_t);
  DATA_VECTOR(P_t);
  DATA_VECTOR(Prey_t);

  // Parameters
  PARAMETER_VECTOR(ln_B0);
  PARAMETER_VECTOR(ln_r);
  PARAMETER_VECTOR(BetaT);
  PARAMETER_VECTOR(ln_sigmaP);
  PARAMETER(mu_T);
  PARAMETER(ln_sd_T);

  int i;
  Type nll=0;
  Type sd_T = exp(ln_sd_T);
  vector<Type> B0(Nstocks);
  vector<Type> r(Nstocks);
  vector<Type> sigmaP(Nstocks);
  for(int i=0; i<Nstocks; i++){
    B0(i) = exp(ln_B0(i));
    r(i) = exp(ln_r(i));
    sigmaP(i) = exp(ln_sigmaP(i));
  }
  
  // Likelihood contribution from observations (additive process error in production)
  vector<Type> P_t_exp(Nobs);
  for(int i=0; i<Nobs; i++){
    // exp for expected productivity
    P_t_exp(i) = r(StockID(i))/p*B_t(i)*(1-pow(B_t(i)/B0(StockID(i)),p)) * exp(Prey_t(i)*BetaT(StockID(i)));
    nll -= dnorm( P_t(i), P_t_exp(i), sigmaP(StockID(i)), true);
  }

  // Probability of random effects
  Type pen=0;
  Type eps=1e-3;
  vector<Type> B0_check(Nstocks);
  for(int i=0; i<Nstocks; i++){
    nll -= dnorm( BetaT(i), mu_T, sd_T, true);
    // penalize for B0 > 5
    B0_check(i) = 5 - B0(i);
    B0_check(i) = posfun(B0_check(i), eps, pen);
    nll += pen;
  }
  
  // Report productivity predictions/observations and B observations
  // matrix<Type> resids(Nobs, 3);
  // for(int i=0; i<Nobs; i++){
  //   resids(i,1) = B_t(i);
  //   resids(i,2) = P_t(i);
  //   resids(i,3) = P_t_exp(i);
  // }
  // REPORT( resids );
  
  ADREPORT( r );
  ADREPORT( B0 );
  ADREPORT( sigmaP );
  ADREPORT( sd_T );
  
  return nll;
}
