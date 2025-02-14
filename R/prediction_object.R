#' Processes HMM model object for transition probability matrix prediction
#'
#' @param mod model object as returned by `moveHMM`, `momentuHMM` or `hmmTMB`
#'
#' @return list containing original covariates `covs` and a function `predict_tpm`
#' to predict the transition probability matrix
#' @export
#'
#' @importFrom moveHMM predictTPM
#' @importFrom momentuHMM getTrProbs
#'
#' @examples
#' # no examples
processHMM <- function(mod){
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

    cat("hmmTMB model provided - simulation methods will be slower\n")

    ## extract covariates
    # extract original data
    data <- mod$.__enclos_env__$private$obs_$data()
    # now find covariate names
    forms <- c(mod$hid()$formula())
    forms <- forms[forms != "."]
    forms <- sapply(forms, as.formula)
    cov_names <- unique(unlist(lapply(forms, all.vars)))
    ## store raw covariates
    self$covs <- data[, cov_names]

    ## create tpm prediction function that only needs covariates
    self$predict_tpm <- function(newcovs = self$covs){
      # hmmTMB has predict method that can predict tpm
      mod$predict(what = "tpm",
                  newdata = newcovs)
      # fairly slow, because runs make_matrices for each parameter
    }
  }

  class(self) <- "processedHMM"
  return(self)
}
