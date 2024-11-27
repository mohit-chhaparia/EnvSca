#'
#' Convert a categorical time-series into multivariate indicator process
#'
#' This function transforms a categorical time series into a binary indicator matrix. Each category in the time
#' series is represented as a column in the matrix, except for one reference category which is omitted.
#'
#' @param xt A vector containing the categorical time-series. The elements of this vector can be numeric,
#' character, and can contain missing values (\code{NA}). If \code{xt} contains \code{NA} then they will
#' be treated as a distinct category labeled 'NA'.
#' @return A binary indicator matrix containing:
#' \itemize{
#'   \item \code{nrow}: Equal to the length of \code{xt}
#'   \item \code{ncol}: Equal to the number of unique categories in \code{xt} (including 'NA') minus one.
#'   The excluded category, used as the reference category, is the largest value in lexicographic order.
#' }
#'
#' @examples
#' xt <- c(1, 2, 3, 2, 1, 3, 'a', 'b', NA, 'a', 'a', NA)
#' output <- cat_convert(xt)
#' print(output)
#' # Note: The reference category ('b' in this case) is excluded from the columns and is
#' # represented by a row containing only zeros.
#'
#' @export
cat_convert <- function(xt){
  if(length(xt) <= 0) stop("Length of xt is zero!!")

  if(any(is.na(xt)) | any(is.numeric(xt))) xt <- as.character(ifelse(is.na(xt), 'NA', xt))

  return(.Call(`_EnvSca_cat_convert_c`, xt))
}
