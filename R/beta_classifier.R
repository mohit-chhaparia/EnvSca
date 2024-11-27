#'
#' Classification Using only Optimal Scaling
#'
#' This function classifies test time-series based solely on the optimal scaling matrices derived from the
#' training time-series groups.
#'
#' @param yt A 3D array representing the training time-series. Each slice corresponds to one time-series.
#' @param group A vector of integers indicating the class/group for each time-series in \code{yt}. The
#' length of \code{group} must match the number of slices in \code{yt}.
#' @param L A integer or vector of integers giving the widths of modified Daniell smoothers to be
#' used to smooth the periodogram. The value of the elements in \code{L} range from 2 to less than half of the
#' number of rows of \code{yt}. It is feasible to have \code{L} less than the cube root of the number of
#' rows in \code{yt}.
#' @param yt_new A matrix representing the test time series. Dimensions are the same as \code{yt}.
#' @return A numeric vector where each element corresponds to the predicted class label for a test time-series in
#' \code{yt_new}.
#' @examples
#' set.seed(12092024)
#'
#' # Simulate training time-series for two classes
#' yt <- array(rnorm(1500), dim = c(50, 3, 10))
#' group <- c(rep(1, 5), rep(2, 5))
#'
#' # Simulate test time-series
#' yt_new <- matrix(rnorm(150), nrow = 50)
#'
#' # Classify the test time-series
#' classes <- beta_classifier(yt, group, L = 3, yt_new)
#' print(classes)
#'
#' @export
beta_classifier <- function(yt, group, L, yt_new){
  nnew <- dim(yt_new)[3]
  if(is.na(nnew)){
    nnew <- 1
  }else{
    nnew <- nnew
  }
  nclass <- length(unique(group))
  classes <- c()
  dist <- list()
  for (j in 1:nclass){
    dist[[j]] <- group_env(yt[ , , group == j] , L)$scale
  }
  for (k in 1:nnew){
    if(nnew == 1){
      new_dist <- env_get(yt_new , L)$scale
    }else{
      new_dist <- env_get(yt_new[ , , k] , L)$scale
    }
    g <- c()
    for (j in 1:nclass){
      temp <- dist[[j]]
      g[j] <- sum((new_dist - temp) ^ 2)
    }
    classes[k] <- which(g == min(g))
  }
  return(classes)
}
