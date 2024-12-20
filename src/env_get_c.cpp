#include <RcppArmadillo.h>
#include <Rcpp.h>
// [[Rcpp::depends(RcppArmadillo)]]
// @noRd
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List env_get_c(const arma::mat& yt, Rcpp::List fyy) {

  int dimen = yt.n_cols; // Number of columns (dimensions)
  arma::mat v = arma::cov(yt); // Covariance matrix

  // Extract real part of spectral density matrix and frequency from fyy
  arma::cube fyy_re = Rcpp::as<arma::cube>(fyy["fxx"]);  // Assuming fyy$fxx is a cube
  arma::vec freq = Rcpp::as<arma::vec>(fyy["freq"]); // Frequencies
  int num = Rcpp::as<int>(fyy["n.used"]); // Number of observations used

  arma::mat Q = arma::eye(dimen, dimen); // Identity matrix of size dimen
  int nfreq = freq.n_elem; // Number of elements in freq

  arma::vec specenv(nfreq, arma::fill::zeros); // Initialize spectral envelope with zeros
  arma::mat beta(nfreq, dimen, arma::fill::zeros); // Initialize beta matrix with zeros

  for (int k = 0; k < nfreq; k++) {
    arma::mat mat_to_eigen = 2.0 * Q * fyy_re.slice(k) * Q / static_cast<double>(num);
    mat_to_eigen = 0.5 * (mat_to_eigen + mat_to_eigen.t());

    arma::vec eigval;
    arma::mat eigvec;
    arma::eig_sym(eigval, eigvec, mat_to_eigen); // Eigenvalue and eigenvector calculation

    specenv(k) = eigval(dimen - 1); // Largest eigenvalue sorted in ascending order
    arma::vec b = Q * eigvec.col(dimen - 1); // Corresponding eigenvector
    beta.row(k) = arma::trans(b / std::sqrt(arma::accu(arma::square(b))));
  }

  return Rcpp::List::create(Rcpp::Named("freq") = freq,
                            Rcpp::Named("envelope") = specenv,
                            Rcpp::Named("scale") = beta);
}
