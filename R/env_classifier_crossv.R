#'
#' Cross-validation to do grid search of kappa
#'
#' @param yt
#' @param group
#' @param L
#' @param kappa
#' @return classes
#' @examples
#' To be added later
#' @export
env_classifier_crossv <- function(yt, group, L, kappa){
  nclass <- length(unique(group))
  ntun <- length(kappa)
  classes <- rep(0 , length(kappa))
  for (jj in 1:ntun){
    for (k in 1:length(group)){
      yt_temp <- yt[ , , -k]
      group_temp <- group[-k]
      yt_test <- yt[ , , k]
      group_test <- group[k]
      env <- list()
      scal <- list()
      for (j in 1:nclass){
        output1 <-  group_env(yt_temp[ , , group_temp == j] , L)
        env[[j]] <- output1$envelope
        scal[[j]] <- output1$scale
      }
      output2 <- env_get(yt_test , L)
      new_env <- output2$envelope
      new_scal <- output2$scale
      g <- c()
      for (j in 1:nclass){
        temp1 <- env[[j]]
        temp2 <- scal[[j]]
        g[j] <- kappa[jj] * sum((new_env - temp1) ^ 2) / sum(new_env ^ 2) +
          (1 - kappa[jj]) * sum((new_scal - temp2) ^ 2) / sum(new_scal ^ 2)
      }
      ig <- which(g == min(g))
      classes[jj] <- classes[jj] + (group_test == ig)
    }
  }
  return(classes)
}
