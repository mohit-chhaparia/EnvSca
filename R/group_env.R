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

  # Check if yt_group is NULL
  if(is.null(yt_group)) stop("yt_group cannot be NULL.")
  # Check the dimensions of yt_group
  if(!is.array(yt_group) | length(dim(yt_group)) != 3)
    stop("yt_group should be a 3D array where each slice represents a time-series.")
  # Check if all elements of yt_group are numeric
  if(!all(is.numeric(yt_group))) stop("All elements of yt_group must be numeric.")
  # Check if yt_group has valid dimensions
  if(!all(dim(yt_group) >= c(2, 1, 1))) stop("Minimum dimension of yt_group should be (2, 1, 1).")
  # Check if yt_group is unusually large
  if(dim(yt_group)[2] > 1e3 | dim(yt_group)[3] > 1e4)
    stop("yt_group is too large. Reduce the number of columns or slices for computational feasibility.")

  # Check if L is NULL
  if(is.null(L)) stop("L cannot be NULL.")
  # Check if L is a vector
  if(!is.vector(L)) stop("L should be a single integer or a vector of integers.")
  # Check if L is numeric
  if(!all(is.numeric(L)))
    stop("All elements of L should be integers between 2 and half of number of rows in yt_group.")
  # Check if all elements of L are integers
  if(!all(L == round(L)))
    stop("All elements of L should be integers between 2 and half of number of rows in yt_group.")
  # Check if all elements of L are within the required range
  if(!all(L >= 2) | !all(L <= (dim(yt_group)[1] / 2)))
    stop("All elements of L should be integers between 2 and half of number of rows in yt_group.")
  # Warning to suggest optimal values for L
  if(!all(L <= (dim(yt_group)[1] ^ (1 / 3))))
    warning("It is feasible if all elements of L are integers between 2 and cube root of number of rows in yt_group.")

  # Check if plot is logical
  if(!is.logical(plot)) stop("plot should be a single logical value.")
  # Check if plot contains a single value
  if(length(plot) != 1 | !is.vector(plot)) stop("plot should contain a single logical value.")


  output <- .Call(`_EnvSca_group_env_c`, yt_group, L)
  if(plot){
    plot_individual_envelope(output$envelope_ind, output$envelope, 'Individual & Group Envelope for Given Class')
    plot_group_scaling(output$scale)
  }
  return(output)
}
