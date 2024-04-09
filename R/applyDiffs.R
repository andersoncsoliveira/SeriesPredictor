#' Aplica as diferenciações necessárias à série temporal
#'


applyDiffs <- function(serie_temporal, d, D) {
  if (d > 0) {
    serie_temporal <- diff(serie_temporal, lag = frequency(serie_temporal), differences = D)
  }
  if (D > 0) {
    serie_temporal <- diff(serie_temporal, differences = d)
  }
  return(serie_temporal)
}
