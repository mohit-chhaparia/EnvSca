#'
#' Given a m times p-1 categorical time series, compute:
#' (1) the Fourier frequencies
#' (2) the spectral envelope
#' (3) the optimal scaling
#'
#' @param yt
#' @param L
#' @return
#' @examples
#' To be added later
#' @export
env.get <- function(yt,L){
  fyy <- mvspec(yt, spans = c(L , L), plot = FALSE, kernel = "fejer")
  return(.Call(`_EnvSca_env.get_c`, yt, fyy))
}
