#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat cat_convert_c(const arma::vec& xt) {

  // Initialize a container for valid (finite) elements
  arma::vec valid_xt;

  if (!arma::find_nonfinite(xt).is_empty()) {
    std::vector<double> valid_values;
    // Loop over xt to collect finite values
    for (int i = 0; i < xt.n_elem; i++) {
      if (arma::is_finite(xt(i))) {  // Check if the value is finite
        valid_values.push_back(xt(i));
      }
    }
    // Convert valid_values back to an Armadillo vector
    valid_xt = arma::vec(valid_values);
  } else {
    // If all elements are finite, use xt directly
    valid_xt = xt;
  }

  // Find unique stages (excluding NaN) and sort them
  arma::vec stage = arma::sort(arma::unique(valid_xt));

  int nobs = xt.n_elem;
  int n_stage = stage.n_elem - 1;
  arma::mat yt(nobs, n_stage, arma::fill::zeros);

  for (int j = 0; j < n_stage; j++) {
    yt.col(j) = arma::conv_to<arma::vec>::from(xt == stage(j));
  }

  return yt;
}
