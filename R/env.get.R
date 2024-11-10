env.get <- function(yt,L){
  fyy <- mvspec(yt, spans = c(L , L), plot = FALSE, kernel = "fejer")
  return(.Call(`_EnvSca_env.get_c`, yt, fyy))
}
