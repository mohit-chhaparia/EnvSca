group_env <- function(yt_group, L){
  return(.Call(`_EnvSca_group_env_c`, yt_group, L))
}
