#'
#' Classification using ONLY optimal scaling
#'
#' @param yt DESCRIPTION
#' @param group DESCRIPTION
#' @param L DESCRIPTION
#' @param yt_new DESCRIPTION
#' @return classes
#' @examples
#' To be added later
#' @export
beta_classifier <- function(yt, group, L, yt_new){
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
    dist[[j]] <- group_env(yt[ , , group == j] , L)$scale
  }
  for (k in 1:nnew){
    if(nnew == 1){
      new_dist <- env_get(yt_new , L)$scale
    }else{
      new_dist <- env_get(yt_new[ , , k] , L)$scale
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
