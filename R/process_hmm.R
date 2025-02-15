#' Processes HMM model object
#'
#' Extracts covariates and sets up a prediction function for the transition probability matrix
#'
#' @param mod model object as returned by `moveHMM`, `momentuHMM` or `hmmTMB`
#'
#' @return list containing original covariates `covs` and a function `predict_tpm`
#' to predict the transition probability matrix for new covariate values
#' @export
#'
#' @importFrom moveHMM predictTPM
#' @importFrom momentuHMM getTrProbs
#' @importFrom stats as.formula
#'
#' @examples
#' # no examples
process_hmm <- function(mod){
  ## initialise self object
  self <- list()

  ## determine which type of model is provided
  if(inherits(mod, "moveHMM")){
    ## store raw covariates
    self$covs <- mod$rawCovs

    ## create tpm prediction function that only needs covariates
    self$predict_tpm <- function(newcovs = self$covs){
      # moveHMM has simple predictTPM function
      predictTPM(m = mod,
                 newData = newcovs,
                 beta = mod$mle$beta)
    }
  } else if(inherits(mod, "momentuHMM")){
    ## store raw covariates
    self$covs <- mod$rawCovs

    ## create tpm prediction function that only needs covariates
    self$predict_tpm <- function(newcovs = self$covs){
      # momentuHMM has getTrProbs function
      getTrProbs(
        data = newcovs,
        nbStates = length(mod$stateNames),
        beta = mod$mle$beta,
        formula = mod$conditions$formula
      )
    }
  } else if(inherits(mod, "HMM")){ # hmmTMB
    # building desing matrices is slower in hmmTMB - and happens inside HMM$predict()
    cat("hmmTMB model provided - simulation methods will be slower\n")

    ## find covariate names
    cov_names <- unique(rapply(mod$hid()$formulas(), all.vars))
    cov_names <- cov_names[which(cov_names!="pi")]

    ## store raw covariates
    self$covs <- mod$obs()$data()[,cov_names]

    ## create tpm prediction function that only needs covariates
    self$predict_tpm <- function(newcovs = self$covs){
      # hmmTMB has predict method that can predict tpm
      mod$predict(what = "tpm",
                  newdata = newcovs)
      # fairly slow, because runs make_matrices for each parameter
    }
  }

  # class(self) <- "processed_hmm"
  return(self)
}
