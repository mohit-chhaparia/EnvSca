//'
//' Given a several time series within a group
//' Compute
//' (1) the group level spectral envelope
//' (2) the group level optimal scaling
//'
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List group_env_c(const arma::mat& yt_group, int L) {


  return Rcpp::List::create(Rcpp::Named("freq") = freq,
                            Rcpp::Named("envelope") = specenv,
                            Rcpp::Named("scale") = beta);
}

