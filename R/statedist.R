#' State distribution
#' @description
#' Computes the covariate-dependent state distribution for all covariates based on fitted HMM model.
#'
#' @param mod model object as returned by `moveHMM`, `momentuHMM` or `hmmTMB`
#' @param method method to use for computing the state distribution.
#'
#' Options are "gamlss", "ar", "gan" and "bootstrap". See `Details'.
#'
#' @details
#' Additional details...
#'
#'
#' @return statedist object containing the results and a plotting function
#' @export
#'
#' @examples
#' # no examples yet
statedist <- function(mod, method = "gamlss"){
  ## process provided model to have standardised objects
  processed_hmm <- process_hmm(mod)

  ## apply the desired method
  state_dist <- apply_method(processed_hmm, method)

  ## set class and return
  class(state_dist) <- "statedist"
  return(state_dist)
}
