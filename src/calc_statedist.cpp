// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>
using namespace arma;

// [[Rcpp::export]]
arma::mat calc_statedist(const arma::vec& delta, const arma::cube& Gamma) {
  int n_states = delta.n_elem; // Number of states
  int n = Gamma.n_slices; // Number of time points

  arma::mat result(n, n_states); // Matrix to store results (n x n_states)

  // Set the first row to the initial distribution delta
  result.row(0) = delta.t();

  // Recursively compute the unconditional state distributions
  for (int t = 1; t < n; t++) {
    result.row(t) = result.row(t - 1) * Gamma.slice(t - 1);
  }

  return result;
}
