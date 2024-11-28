#'
#' Classify Time Series based on Spectral Envelope and Optimal Scaling
#'
#' This function assigns a class to each time series in a set of test time series \code{yt_new} by comparing
#' them with training times series groups \code{yt}. Classification is based on group-level spectral
#' envelopes and optimal scaling with a tunable parameter \code{kappa} to adjust their relative importance.
#'
#' @param yt A 3D array representing the training time-series.
#' @param group A vector of integers indicating the group/class for each training time series. The length of
#' \code{group} must be equal to the number of slices in \code{yt}.
#' @param L A integer or vector of integers giving the widths of modified Daniell smoothers to be used
#' to smooth the periodogram of each time series in the group. The value of the elements in \code{L} range
#' from 2 to less than half of the number of rows of \code{yt}. It is feasible to have \code{L} less than
#' the cube root of the number of rows in \code{yt}.
#' @param yt_new A matrix representing the test time series. The dimensions are the same as \code{yt}.
#' @param kappa A numeric value between 0 and 1 that controls the relative importance of the spectral envelope
#' and optimal scaling in the classification decision. Higher values give more weight to the spectral envelope.
#' @param plot Logical; If \code{TRUE}, generates plots for individual spectral envelopes, and the
#' group-level envelopes and scalings during training. Default is \code{TRUE}.
#' @return A vector of predicted class labels for each time series in \code{yt_new}.
#' @examples
#' set.seed(12092024)
#' # Simulate training time series for two groups
#' yt <- array(rnorm(1500), dim = c(50, 3, 10))
#' group <- c(rep(1, 5), rep(2, 5))
#'
#' # Simulate test time series
#' yt_new <- matrix(rnorm(150), nrow = 50)
#'
#' # Classify the test time series
#' classes <- env_classifier(yt, group, L = 3, yt_new, kappa = 0.5, plot = TRUE)
#' print(classes)
#' @export
env_classifier <- function(yt, group, L, yt_new, kappa, plot = TRUE){

  # Check if yt is NULL
  if(is.null(yt)) stop("yt cannot be NULL.")
  # Check the dimensions of yt
  if(!is.array(yt) | length(dim(yt)) != 3)
    stop("yt should be a 3D array where each slice represents a time-series.")
  # Check if all elements of yt are numeric
  if(!all(is.numeric(yt))) stop("All elements of yt must be numeric.")
  # Check if yt has valid dimensions
  if(!all(dim(yt) >= c(2, 1, 1))) stop("Minimum dimension of yt should be (2, 1, 1).")
  # Check if yt is unusually large
  if(dim(yt)[2] > 1e3 | dim(yt)[3] > 1e4)
    stop("yt is too large. Reduce the number of columns or slices for computational feasibility.")


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
    output <- group_env(yt[ , , group == j] , L, plot = FALSE)
    env[[j]] <- output$envelope
    scal[[j]] <- output$scale
    if(plot){
      plot_individual_envelope(output$envelope_ind, output$envelope)
    }
  }
  if(plot){
    plot_group_envelope_scaling(env, scal, called_from = 'env_classifier')
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
