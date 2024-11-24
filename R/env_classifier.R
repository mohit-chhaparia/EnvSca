#'
#' This is the main function: It assigns classes to a time series
#'
#' @param yt DESCRIPTION
#' @param group DESCRIPTION
#' @param L DESCRIPTION
#' @param yt_new DESCRIPTION
#' @param kappa Tuning parameter controls the relative importance of the spectral envelope and optimal scalings in classifying time-series.
#' @return classes
#' @examples
#' # To be added later
#' @export
env_classifier <- function(yt, group, L, yt_new, kappa, plot = TRUE){
  nnew = dim(yt_new)[3]
  if(is.na(nnew)){
    nnew <- 1
  } else{
    nnew <- nnew
  }
  nclass <- length(unique(group))
  classes <- c()
  env <- list()
  scal <- list()

  # calculate group level statistics based on training time series
  for (j in 1:nclass){
    output <- group_env(yt[ , , group == j] , L)
    env[[j]] <- output$envelope
    scal[[j]] <- output$scale
    if(plot){
      plot_individual_envelope(output$envelope_ind, output$envelope)
    }
  }
  if(plot){
    plot_group_envelope_scaling(env, scal)
  }
  # for each of testing time series, assign a group to it
  for (k in 1:nnew){
    if(nnew == 1){
      new_env <- env_get(yt_new , L)$envelope
      new_scal <- env_get(yt_new , L)$scale
    }else{
      new_env <- env_get(yt_new[ , , k] , L)$envelope
      new_scal <- env_get(yt_new[ , , k] , L)$scale
    }
    g1 <- new_env
    g2 <- new_scal
    g <- c()
    for (j in 1:nclass){
      temp1 <- env[[j]]
      temp2 <- scal[[j]]
      g[j] <- kappa * sum((new_env - temp1) ^ 2) / sum(new_env ^ 2) +
        (1 - kappa) * sum((new_scal - temp2) ^ 2) / sum(new_scal ^ 2)
    }
    classes[k] <- which(g == min(g))
  }
  return(classes)
}
