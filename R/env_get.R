#'
#' Given a m times p-1 categorical time series, compute:
#' (1) the Fourier frequencies
#' (2) the spectral envelope
#' (3) the optimal scaling
#'
#' @param yt DESCRIPTION
#' @param L DESCRIPTION
#' @return DESCRIPTION
#' @examples
#' To be added later
#' @export
#' @importFrom astsa mvspec
env_get <- function(yt,L){
  fyy <- mvspec(yt, spans = c(L , L), plot = FALSE, kernel = "fejer")
  return(.Call(`_EnvSca_env_get_c`, yt, fyy))
}
