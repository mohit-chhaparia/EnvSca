#'
#' Convert a categorical time-series into multivariate indicator process
#'
#' @param xt A vector containing the categorical time-series. The elements of this vector can be numeric, character, or NAs. If xt contains NAs then they will be treated as category 'NA'.
#' @return Multivariate indicator matrix containing zeros and ones. Number of rows of yt is equal to the number of elements in xt. Number of columns of yt is to the number of unique elements in yt (including NAs) minus 1.
#' @examples
#' xt <- c(1, 2, 3, 2, 1, 3, 'a', 'b', NA, 'a', 'a', NA)
#' output <- cat_convert(xt)
#' print(output)
#'
#' @export
cat_convert <- function(xt){
  if(length(xt) <= 0) stop("Length of xt is zero!!")

  if(any(is.na(xt)) | any(is.numeric(xt))) xt <- as.character(ifelse(is.na(xt), 'NA', xt))

  return(.Call(`_EnvSca_cat_convert_c`, xt))
}
