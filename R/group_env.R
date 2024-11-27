#'
#' Get group-level spectral envelope and optimal scaling
#'
#' Given a several time series within a group
#' Compute
#' (1) the group level spectral envelope
#' (2) the group level optimal scaling
#' (3) plot the individual spectral envelopes
#' (4) plot the group-level spectral envelopes
#' (5) plot the group-level optimal scalings
#'
#' @param yt_group A 3D array where each slice represents a time-series in the group. Rows correspond
#' to time points.
#' @param L A integer or vector of integers giving the widths of modified Daniell smoothers to be used
#' to smooth the periodogram of each time series in the group. The value of the elements in \code{L} range
#' from 2 to less than half of the number of rows of \code{yt_group}. It is feasible to have \code{L} less
#' than the cube root of the number of rows in \code{yt_group}.
#' @param plot Logical; If \code{TRUE}, generates plots for individual spectral envelopes, and the
#' group-level scalings. Default is \code{FALSE}.
#' @return A list containing the following components:
#' \describe{
#'   \item{\code{freq}}{A vector of frequencies common across the group.}
#'   \item{\code{envelope}}{The group-level spectral envelope calculated as the average of individual envelopes.}
#'   \item{\code{scale}}{The group-level optimal scaling matrix, calculated as the average of individual
#'   scaling matrices.}
#'   \item{\code{envelope_ind}}{A list of spectral envelopes for each time series in the group.}
#'   \item{\code{scale_ind}}{A list of scaling matrices for each time series in the group.}
#' }
#' @examples
#' set.seed(12092024)
#' data <- array(rnorm(5000), dim = c(100, 5, 10))
#' result <- group_env(data, L = 3, plot = TRUE)
#'
#' # Accessing results
#' freq <- result$freq # Frequencies
#' envelope <- result$envelope # Group-Level Spectral Envelope
#' scale <- result$scale # Group-Level Optimal Scaling
#' individual_env <- result$envelope_ind # Individual Spectral Envelopes
#' individual_scale <- result$scale_ind # Individual Scaling Matrices
#' @export
group_env <- function(yt_group, L, plot = FALSE){
  output <- .Call(`_EnvSca_group_env_c`, yt_group, L)
  if(plot){
    plot_individual_envelope(output$envelope_ind, output$envelope)
    plot_group_envelope_scaling(output$envelope, output$scale, called_from = 'group_env')
  }
  return(output)
}
