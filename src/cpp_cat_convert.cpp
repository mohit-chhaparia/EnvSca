#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat cat_convert(const arma::vec& xt) {
  arma::vec stage = arma::sort(arma::unique(xt));
  int nobs = xt.n_elem;
  int n_stage = stage.n_elem - 1;
  arma::mat yt(nobs, n_stage, arma::fill::zeros);

  for (int j = 0; j < n_stage; j++) {
    yt.col(j) = arma::conv_to<arma::vec>::from(xt == stage(j));
  }

  return yt;
}
