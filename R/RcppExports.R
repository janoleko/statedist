# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' State distribution
#' @description
#' Computation of the unconditional state distribution (written in C++)
#' @param delta initial distribution of the Markov chain - vector of length \code{n_states}
#' @param Gamma transition matrices of the Markov chain - array of dimension \code{c(n_states, n_states, nObs)}
#'
#' @return all unconditional state distributions of the Markov chain
#' @export
#' @examples
#' delta <- c(0.5, 0.5)
#' Gamma <- array(c(0.9, 0.1, 0.1, 0.9), dim = c(2, 2, 10))
#' calc_statedist(delta, Gamma)
calc_statedist <- function(delta, Gamma) {
    .Call(`_statedist_calc_statedist`, delta, Gamma)
}

