#'
#' Get frequencies, spectral envelope, and optimal scaling.
#'
#' Given a m times p-1 categorical time series, compute:
#' (1) the Fourier frequencies
#' (2) the spectral envelope
#' (3) the optimal scaling
#'
#' @param yt A matrix containing univariate or multivariate time-series. Rows represent time points.
#' @param L A integer or vector of integers giving the widths of modified Daniell smoothers to be
#' used to smooth the periodogram. The value of the elements in \code{L} range from 1 to cube root of the number
#' of rows of \code{yt}.
#' @return A list containing the following components:
#' \describe{
#'   \item{\code{freq}}{A vector of frequencies.}
#'   \item{\code{envelope}}{A vector of spectral envelope values corresponding to the frequencies.}
#'   \item{\code{scale}}{A matrix of optimal scaling vectors corresponding to the frequencies.}
#' }
#' @examples
#' set.seed(12092024)
#' data <- matrix(rnorm(500), ncol = 5)
#' result <- env_get(data, L = 3)
#'
#' # Accessing results
#' freq <- result$freq # Frequencies
#' envelope <- result$envelope # Spectral Envelope
#' scale <- result$scale # Optimal Scaling
#'
#' @export
#' @importFrom astsa mvspec
env_get <- function(yt,L){

  # Check if yt is NULL
  if(is.null(yt)) stop("yt cannot be NULL")
  # Check if yt is a matrix
  if(!is.matrix(yt)) stop("yt must be a matrix")
  # Check if yt has atleast two rows and one column
  if(nrow(yt) < 2 | ncol(yt) < 1) stop("yt must have atleast 2 rows and 1 column.")
  # Check if all elements in yt are numeric
  if(!all(is.numeric(yt))) stop("All elements of yt must be numeric.")
  # Check if yt is very large
  if(ncol(yt) > 1e3) stop("yt has too many columns. Please reduce the dimensionality for computational feasibility.")

  # Check if L is NULL
  if(is.null(L)) stop("L cannot be NULL.")
  # Check if L is numeric
  if(!all(is.numeric(L))) stop("All elements of L should be positive integers.")
  # Check if L falls in the right range
  if(any(L <= 1) | any(L >= (nrow(yt) / 2)) | any(L != round(L)))
    stop("L should be a integer between 1 and half of the number of rows in yt.")
  if(any(L > (nrow(yt) ^ (1 / 3))))
    warning("It is feasible to have L less than the cube root of the number of rows in yt")


  fyy <- mvspec(yt, spans = c(L , L), plot = FALSE, kernel = "fejer")
  fyy$fxx <- Re(fyy$fxx)
  return(.Call(`_EnvSca_env_get_c`, yt, fyy))
}
