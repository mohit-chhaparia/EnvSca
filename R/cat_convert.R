#'
#' Convert a categorical time-series into multivariate indicator process
#'
#' @param xt
#' @return
#' @examples
#' To be added later
#' @export
cat_convert <- function(xt){
  .Call(`_EnvSca_cat_convert_c`, xt)
}
