apply_method <- function(processed_hmm, method){
  covs <- processed_hmm$covs
  uIDs <- unique(covs$ID)
  pooling <- !("ID" %in% processed_hmm$cov_names)

  ## applying the methods
  res <- list()
  if(method == "GAMLSS"){
    for(id in uIDs){
      covs_id <- covs[covs$ID == id,]
      # self[[id]] <- statedist_gamlss(covs_id,
      #                               processed_hmm$predict_tpm)
    }
  } else if(method == "ar"){
    for(id in uIDs){
      covs_id <- covs[covs$ID == id,]
      # res[[id]] <- statedist_ar(covs_id,
      #                           processed_hmm$predict_tpm)
    }
  } else if(method == "gan"){
    for(id in uIDs){
      covs_id <- covs[covs$ID == id,]
      # res[[id]] <- statedist_gan(covs_id,
      #                            processed_hmm$predict_tpm)
    }
  } else if(method == "bootstrap"){
    for(id in uIDs){
      covs_id <- covs[covs$ID == id,]
      # res[[id]] <- statedist_bootstrap(covs_id,
      #                                  processed_hmm$predict_tpm)
    }
  }

  self$result <- res

  ## plotting
  self$plot <- function(type = "ggplot",
                        col = processed_hmm$colors,
                        ID = uIDs){

    ## Check if color argument matches
    if(length(col) != processed_hmm$n_states){
      if(length(col) >= processed_hmm$n_states){
        message("More colors provided than states, only using the first `n_states`")
        col <- col[1:processed_hmm$n_states]
      } else{
        stop("Number of colors provided must be at least the number of states")
      }
    }

    ## create the plot: either ggplot or base
    if(type == "ggplot"){

    } else if(type == "base"){

    } else{
      stop("type must be either 'ggplot' or 'base'")
    }
  }
}

# plot.statedist <- function(statedist,
#                            type = "ggplot",
#                            col = NULL,
#                            ID = "all"){
#   if(class(statedist) != "statedist"){
#     stop("Input must be a statedist object")
#   }
#
#   if(ID == "all" & is.null(col)){
#     statedist$plot(type = type)
#   } else if(ID == "all" & !is.null(col)){
#     statedist$plot(type = type, col = col)
#   } else if(ID != "all" & is.null(col)){
#     statedist$plot(type = type, ID = ID)
#   } else if(ID != "all" & !is.null(col)){
#     statedist$plot(type = type, col = col, ID = ID)
#   }
# }
