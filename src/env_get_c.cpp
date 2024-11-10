#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List env_get_c(const arma::mat& yt, Rcpp::List fyy) {

  return Rcpp::List::create(Rcpp::Named("freq") = freq,
                            Rcpp::Named("envelope") = specenv,
                            Rcpp::Named("scale") = beta);
}
