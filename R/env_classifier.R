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
#' @param yt_new A 3D array representing the test time series. The dimensions for rows and columns are the same as \code{yt}.
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
#' yt_new <- array(rnorm(150), dim = c(50, 3, 1))
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

  # Check if group is null
  if(is.null(group)) stop("group cannot be null")
  # Check if group is numeric
  if(!all(is.numeric(group))) stop("All elements of group should be numeric")
  # Check dimensions of group
  if(length(group) != dim(yt)[3])
    stop("Number of elements in group should be equal to the number of slices in yt.")

  # Check if L is NULL
  if(is.null(L)) stop("L cannot be NULL.")
  # Check if L is a vector
  if(!is.vector(L)) stop("L should be a single integer or a vector of integers.")
  # Check if L is numeric
  if(!all(is.numeric(L)))
    stop("All elements of L should be integers between 2 and half of number of rows in yt.")
  # Check if all elements of L are integers
  if(!all(L == round(L)))
    stop("All elements of L should be integers between 2 and half of number of rows in yt.")
  # Check if all elements of L are within the required range
  if(!all(L >= 2) | !all(L <= (dim(yt)[1] / 2)))
    stop("All elements of L should be integers between 2 and half of number of rows in yt.")
  # Warning to suggest optimal values for L
  if(!all(L <= (dim(yt)[1] ^ (1 / 3))))
    warning("It is feasible if all elements of L are integers between 2 and cube root of number of rows in yt.")

  # Check if yt_new is NULL
  if(is.null(yt_new)) stop("yt_new cannot be NULL.")
  # Check the dimensions of yt_new
  if(!is.array(yt_new) | length(dim(yt_new)) != 3)
    stop("yt_new should be a  array of 3 dimensions.")
  # Check if all elements of yt_new are numeric
  if(!all(is.numeric(yt_new))) stop("All elements of yt_new must be numeric.")
  # Check if yt_new has valid dimensions
  if(any(dim(yt_new[ , , 1]) != dim(yt[ , , 1]))) stop("Dimension of each slice of yt_new should match the dimension of each slice of yt.")

  # Check if kappa is NULL
  if(is.null(kappa)) stop("kappa cannot be NULL.")
  # Check if kappa is a vector
  if(!is.vector(kappa)) stop("kappa should be a single element between 0 and 1.")
  # Check if kappa is numeric
  if(!is.numeric(kappa)) stop("kappa should be a single numeric element between 0 and 1.")
  # Check if kappa is between 0 and 1
  if(kappa < 0 | kappa > 1) stop("kappa should be a single element between 0 and 1.")

  # Check if plot is logical
  if(!is.logical(plot)) stop("plot should be a single logical value.")
  # Check if plot contains a single value
  if(length(plot) != 1 | !is.vector(plot)) stop("plot should contain a single logical value.")


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
      if(is.na(dim(yt_new)[3])){
        new_env <- env_get(yt_new , L)$envelope
        new_scal <- env_get(yt_new , L)$scale
      } else{
        new_env <- env_get(yt_new[ , , 1] , L)$envelope
        new_scal <- env_get(yt_new[ , , 1] , L)$scale
      }

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
