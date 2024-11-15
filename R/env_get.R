#'
#' Given a m times p-1 categorical time series, compute:
#' (1) the Fourier frequencies
#' (2) the spectral envelope
#' (3) the optimal scaling
#'
#' @param yt univariate or multivariate time-series
#' @param L Integer giving the widths of modified Daniell smoothers to be used to smooth the periodogram
#' @return DESCRIPTION
#' @examples
#' # To be added later
#' @export
#' @importFrom astsa mvspec
env_get <- function(yt,L){
  fyy <- mvspec(yt, spans = c(L , L), plot = FALSE, kernel = "fejer")
  fyy$fxx <- Re(fyy$fxx)
  return(.Call(`_EnvSca_env_get_c`, yt, fyy))
}
