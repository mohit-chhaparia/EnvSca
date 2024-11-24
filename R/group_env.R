#'
#' Given a several time series within a group
#' Compute
#' (1) the group level spectral envelope
#' (2) the group level optimal scaling
#'
#' @param yt_group DESCRIPTION
#' @param L DESCRIPTION
#' @return DESCRIPTION
#' @examples
#' # To be added later
#' @export
group_env <- function(yt_group, L, plot = FALSE){
  output <- .Call(`_EnvSca_group_env_c`, yt_group, L)
  if(plot){
    plot_individual_envelope(output$envelope_ind, output$envelope)
    plot_group_envelope_scaling(env, scal, called_from = 'group_env')
  }
  return(output)
}
