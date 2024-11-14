#'
#' Convert a categorical time-series into multivariate indicator process
#'
#' @param xt DESCRIPTION
#' @return DESCRIPTION
#' @examples
#' # To be added later
#' @export
cat_convert <- function(xt){
  if(length(xt) <= 0) stop("Length of xt is zero!!")

  if(any(is.na(xt)) | any(is.numeric(xt))) xt <- as.character(ifelse(is.na(xt), 'NA', xt))
  return(.Call(`_EnvSca_cat_convert_c`, xt))
}
