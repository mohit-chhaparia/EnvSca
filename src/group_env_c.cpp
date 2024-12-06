#include <RcppArmadillo.h>
#include <Rcpp.h>
// [[Rcpp::depends(RcppArmadillo)]]
// @noRd
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List group_env_c(const arma::cube& yt_group, const arma::vec& L) {

  Function env_get("env_get");

  int nsub = yt_group.n_slices;
  arma::vec specenv;
  Rcpp::List output;
  arma::mat beta;

  // Containers to store envelope and scaling values at each iteration
  Rcpp::List envelope_ind(nsub);
  Rcpp::List scale_ind(nsub);

  for (int k = 0; k < nsub; k++) {

    output = env_get(yt_group.slice(k), L);
    arma::vec tmp_env = output["envelope"];
    arma::mat scale = output["scale"];

    envelope_ind[k] = tmp_env;
    scale_ind[k] = scale;

    if (k == 0) {
      specenv = arma::vec(tmp_env.n_elem, arma::fill::zeros);
      beta = arma::mat(scale.n_rows, scale.n_cols, arma::fill::zeros);
    }

    specenv += tmp_env / static_cast<double>(nsub);
    beta += scale / static_cast<double>(nsub);
  }

  arma::vec freq = output["freq"];

  return Rcpp::List::create(Rcpp::Named("freq") = freq,
                            Rcpp::Named("envelope") = specenv,
                            Rcpp::Named("scale") = beta,
                            Rcpp::Named("envelope_ind") = envelope_ind,
                            Rcpp::Named("scale_ind") = scale_ind);
}

