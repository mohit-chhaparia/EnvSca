#'
#' Cross-Validation for Grid Search of \code{Kappa} in Time Series Classification
#'
#' This function performs cross-validation to evaluate the performance of the method across a range of \code{kappa}
#' values. This helps in selecting the optimal value of \code{kappa}, which controls the relative importance of
#' spectral envelope and optimal scaling in time-series classification.
#'
#' @param yt A 3D array representing the time-series data. Each slice corresponds to one time-series.
#' @param group A vector of integers indicating the class/group for each time-series in \code{yt}. The
#' length of \code{group} must match the number of slices in \code{yt}.
#' @param L A integer or vector of integers giving the widths of modified Daniell smoothers to be
#' used to smooth the periodogram. The value of the elements in \code{L} range from 2 to less than half of the
#' number of rows of \code{yt}. It is feasible to have \code{L} less than the cube root of the number of
#' rows in \code{yt}.
#' @param kappa A numeric vector of candidate values between 0 and 1 for the \code{kappa} parameter that controls
#' the relative importance of the spectral envelope and optimal scaling in the classification decision. Higher
#' values give more weight to the spectral envelope. Each value is tested to assess its classification accuracy.
#' @return A numeric vector where each element corresponds to the cross-validation classification accuracy for a
#' given \code{kappa} value.
#'
#' @examples
#' set.seed(12092024)
#'
#' # Simulate time series data for two classes
#' yt <- array(rnorm(1500), dim = c(50, 3, 10))
#' group <- c(rep(1, 5), rep(2, 5))
#'
#' # Range of kappa values
#' kappa_values <- seq(0, 1, length.out = 10)
#'
#' # Perform cross-validation
#' cv_results <- env_classifier_crossv(yt, group, L = 3, kappa = kappa_values)
#' print(cv_results)
#'
#' # Plot results
#' plot(kappa_values, cv_results / 10, type = "b", xlab = "Kappa", ylab = "Accuracy",
#'      main = "Cross-Validation Results")
#'
#' @export
env_classifier_crossv <- function(yt, group, L, kappa){

  # Check if yt is NULL
  if(is.null(yt)) stop("yt cannot be NULL.")
  # Check the dimensions of yt
  if(!is.array(yt) | length(dim(yt)) != 3)
    stop("yt should be a 3D array where each slice represents a time-series.")
  # Check if all elements of yt are numeric
  if(!all(is.numeric(yt))) stop("All elements of yt must be numeric.")
  # Check if yt has valid dimensions
  if(!all(dim(yt) >= c(2, 1, 2))) stop("Minimum dimension of yt should be (2, 1, 2).")
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

  nclass <- length(unique(group))
  ntun <- length(kappa)
  classes <- rep(0 , length(kappa))
  for (jj in 1:ntun){
    for (k in 1:length(group)){
      yt_temp <- yt[ , , -k]
      group_temp <- group[-k]
      yt_test <- yt[ , , k]
      group_test <- group[k]
      env <- list()
      scal <- list()
      for (j in 1:nclass){
        output1 <-  group_env(yt_temp[ , , group_temp == j] , L)
        env[[j]] <- output1$envelope
        scal[[j]] <- output1$scale
      }
      output2 <- env_get(yt_test , L)
      new_env <- output2$envelope
      new_scal <- output2$scale
      g <- c()
      for (j in 1:nclass){
        temp1 <- env[[j]]
        temp2 <- scal[[j]]
        g[j] <- kappa[jj] * sum((new_env - temp1) ^ 2) / sum(new_env ^ 2) +
          (1 - kappa[jj]) * sum((new_scal - temp2) ^ 2) / sum(new_scal ^ 2)
      }
      ig <- which(g == min(g))
      classes[jj] <- classes[jj] + (group_test == ig)
    }
  }
  return(classes)
}
