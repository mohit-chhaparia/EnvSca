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
  int dimen = yt.n_cols;
  arma::mat v = arma::cov(yt);

  // Extract real part of spectral density matrix and frequency from fyy
  arma::cube fyy_re = Rcpp::as<arma::cube>(fyy["fxx"]);  // Assuming fyy$fxx is a cube
  arma::vec freq = Rcpp::as<arma::vec>(fyy["freq"]);
  int num = Rcpp::as<int>(fyy["n.used"]); // Number of observations used

  return Rcpp::List::create(Rcpp::Named("freq") = freq,
                            Rcpp::Named("envelope") = specenv,
                            Rcpp::Named("scale") = beta);
}
