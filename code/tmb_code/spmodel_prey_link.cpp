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
    P_t_exp(i) = r(StockID(i))*B_t(i)*(1-B_t(i)/B0(StockID(i))) * exp(Prey_t(i)*BetaT(StockID(i)));
    nll -= dnorm( P_t(i), P_t_exp(i), sigmaP(StockID(i)), true);
  }

  // Probability of random effects
  for(int i=0; i<Nstocks; i++){
    nll -= dnorm( BetaT(i), mu_T, sd_T, true);
  }
  
  ADREPORT( r );
  ADREPORT( B0 );
  ADREPORT( sigmaP );
  ADREPORT( sd_T );
  
  return nll;
}
