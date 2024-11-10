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
Rcpp::List group_env_c(const arma::cube& yt_group, int L) {

  int nsub = yt_group.n_slices;
  arma::vec specenv;
  double beta = 0.0;

  for (int k = 0; k < nsub; k++) {

    Rcpp::List output = env.get(yt_group.slice(k), L);
    arma::vec tmp_env = output["envelope"];

    if (k == 0) {
      specenv = arma::vec(tmp_env.n_elem, arma::fill::zeros);
    }
  }

  arma::vec freq = output["freq"];

  return Rcpp::List::create(Rcpp::Named("freq") = freq,
                            Rcpp::Named("envelope") = specenv,
                            Rcpp::Named("scale") = beta);
}

