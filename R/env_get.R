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
  fyy <- mvspec(yt, spans = c(L , L), plot = FALSE, kernel = "fejer")
  fyy$fxx <- Re(fyy$fxx)
  return(.Call(`_EnvSca_env_get_c`, yt, fyy))
}
