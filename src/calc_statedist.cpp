// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>
using namespace arma;
using namespace Rcpp;

//' State distribution
//' @description
//' Computation of the unconditional state distribution (written in C++)
//' @param delta initial distribution of the Markov chain - vector of length \code{n_states}
//' @param Gamma transition matrices of the Markov chain - array of dimension \code{c(n_states, n_states, nObs)}
//'
//' @return all unconditional state distributions of the Markov chain
//' @export
//' @examples
//' delta <- c(0.5, 0.5)
//' Gamma <- array(c(0.9, 0.1, 0.1, 0.9), dim = c(2, 2, 10))
//' calc_statedist(delta, Gamma)
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
