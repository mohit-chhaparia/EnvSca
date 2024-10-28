#'
#' Classification using ONLY spectral envelope
#'
#' @param yt
#' @param group
#' @param L
#' @param yt_new
#' @return classes
#' @examples
#' To be added later
#' @export
gamma_classifier <- function(yt, group, L, yt_new){
  nnew <- dim(yt_new)[3]
  if(is.na(nnew)){
    nnew <- 1
  }else{
    nnew <- nnew
  }
  nclass <- length(unique(group))
  classes <- c()
  dist <- list()
  for (j in 1:nclass){
    dist[[j]] <- group_env(yt[ , , group == j] , L)$envelope
  }
  for (k in 1:nnew){
    if(nnew == 1){
      new_dist <- env.get(yt_new , L)$envelope
    }else{
      new_dist <- env.get(yt_new[ , , k] , L)$envelope
    }
    g <- c()
    for (j in 1:nclass){
      temp <- dist[[j]]
      g[j] <- sum((new_dist - temp) ^ 2)
    }
    classes[k] <- which(g == min(g))
  }
  return(classes)
}
