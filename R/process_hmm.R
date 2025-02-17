#' Processes HMM model object
#'
#' Extracts covariates and sets up a prediction function for the transition probability matrix
#'
#' @param mod model object as returned by `moveHMM`, `momentuHMM` or `hmmTMB`
#'
#' @return list containing:
#' \item{covs}{original covariates}
#' \item{predict_tpm}{function to predict the transition probability matrix for new covariate values}
#' \item{n_states}{number of states}
#' \item{colors}{color palette of length \code{n_states} for plotting}
#' @export
#'
#' @importFrom moveHMM predictTPM
#' @importFrom momentuHMM getTrProbs
#' @importFrom stats as.formula
#'
#' @examples
#' # no examples yet
process_hmm <- function(mod){
  ## initialise self object
  self <- list()

  ## determine which type of model is provided
  if(inherits(mod, "moveHMM")){
    ## store raw covariates
    self$covs <- cbind(mod$data$ID, mod$rawCovs)

    ## store covariate names, mainly for determining whether ID is included
    self$cov_names <- all.vars(mod$conditions$formula)

    ## create tpm prediction function that only needs covariates
    self$predict_tpm <- function(newcovs = self$covs, beta = NULL){
      # when beta is not provided, use the MLE
      if(is.null(beta)) beta <- mod$mle$beta
      # moveHMM has simple predictTPM function
      predictTPM(m = mod,
                 newData = newcovs,
                 beta = beta)
    }

    ## store number of states
    self$n_states <- ncol(mod$mle$stepPar)

    ## store colors
    self$colors <- getPalette(self$n_states)

  } else if(inherits(mod, "momentuHMM")){
    ## store raw covariates
    self$covs <- cbind(mod$data$ID, mod$rawCovs)

    ## store covariate names, mainly for determining whether ID is included
    self$cov_names <- all.vars(mod$conditions$formula)

    ## create tpm prediction function that only needs covariates
    self$predict_tpm <- function(newcovs = self$covs, beta){
      # when beta is not provided, use the MLE
      if(is.null(beta)) beta <- mod$mle$beta
      # momentuHMM has getTrProbs function
      getTrProbs(
        data = newcovs,
        nbStates = length(mod$stateNames),
        beta = beta,
        formula = mod$conditions$formula
      )
    }

    ## store number of states
    self$n_states <- length(mod$stateNames)

    ## store colors
    self$colors <- getPalette(self$n_states)

  } else if(inherits(mod, "HMM")){ # hmmTMB
    # building desing matrices is slower in hmmTMB - and happens inside HMM$predict()
    cat("hmmTMB model provided - simulation methods will be slower\n")

    ## find covariate names
    self$cov_names <- unique(rapply(mod$hid()$formulas(), all.vars))
    self$cov_names  <- self$cov_names [which(self$cov_names != "pi")]

    ## store raw covariates
    self$covs <- mod$obs()$data()[,c("ID", self$cov_names)]

    ## create tpm prediction function that only needs covariates
    self$predict_tpm <- function(newcovs = self$covs){
      # hmmTMB has predict method that can predict tpm
      mod$predict(what = "tpm",
                  newdata = newcovs)
      # fairly slow, because runs make_matrices for each parameter
    }

    ## store number of states
    self$n_states <- mod$hid()$nstates()

    ## store colors
    if(self$n_states < 7){
      self$colors <- c("#00798c", "#d1495b", "#edae49", "#66a182",
                       "#2e4057", "#8d96a3")[1:self$n_states]
    } else if(self$n_states >= 7){
      hues <- seq(15, 375, length = self$n_states + 1)
      self$colors <- grDevices::hcl(h = hues, l = 65, c = 100)[1:self$n_states]
    }
  }

  return(self)
}


#' Discrete colour palette for states
#'
#' @param n_states Number of states
#'
#' @return Vector of colours, of length n_states
#'
#' @importFrom grDevices hcl
getPalette <- function(n_states) {
  if(n_states < 8) {
    # color-blind friendly palette
    pal <- c("#E69F00", "#56B4E9", "#009E73",
             "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    col <- pal[1:n_states]
  } else {
    # to make sure that all colours are distinct (emulate ggplot default palette)
    hues <- seq(15, 375, length = n_states + 1)
    col <- hcl(h = hues, l = 65, c = 100)[1:n_states]
  }
  return(col)
}
