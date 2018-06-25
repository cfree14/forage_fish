// Space time 
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  // Data
  DATA_INTEGER(Nstocks);
  DATA_INTEGER(Nobs);
  DATA_FACTOR(StockID);
  DATA_VECTOR(B_t);
  DATA_VECTOR(P_t);

  // Parameters
  PARAMETER_VECTOR(ln_B0);
  PARAMETER_VECTOR(ln_r);
  PARAMETER_VECTOR(ln_sigmaP);

  int i;
  Type nll=0;
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
    P_t_exp(i) = r(StockID(i))*B_t(i)*(1-B_t(i)/B0(StockID(i)));
    nll -= dnorm( P_t(i), P_t_exp(i), sigmaP(StockID(i)), true);
  }
  
  ADREPORT( r );
  ADREPORT( B0 );
  ADREPORT( sigmaP );

  return nll;
}
