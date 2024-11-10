//' Given a m times p-1 categorical time series, compute:
//' (1) the Fourier frequencies
//' (2) the spectral envelope
//' (3) the optimal scaling
//'
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List env.get_c(const arma::mat& yt, Rcpp::List fyy) {

  return Rcpp::List::create(Rcpp::Named("freq") = freq,
                            Rcpp::Named("envelope") = specenv,
                            Rcpp::Named("scale") = beta);
}
