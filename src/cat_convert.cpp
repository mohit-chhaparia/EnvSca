#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat cat_convert_c(const std::vector<std::string>& xt) {
  // Get unique values from xt
  std::vector<std::string> stage = xt;

  std::sort(stage.begin(), stage.end());
  Rcpp::Rcout << std::endl;
  stage.erase(std::unique(stage.begin(), stage.end()), stage.end());

  int nobs = xt.size();
  int nstage = stage.size();
  arma::mat yt(nobs, nstage - 1, arma::fill::zeros);

  for (int j = 0; j < nstage - 1; ++j) {
    for (int i = 0; i < nobs; ++i) {
      if (xt[i] == stage[j]) {
        yt(i, j) = 1;
      }
    }
  }
  return yt;
}
