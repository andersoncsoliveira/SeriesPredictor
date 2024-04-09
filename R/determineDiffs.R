#' determineDiffs
#'
#' @serie_temporal x Descrição do parâmetro x
#' @return Retorna o numero de diferença para retirar a tendencia e a sazonalidade




determineDiffs <- function(serie_temporal) {
  d <- ndiffs(serie_temporal)
  D <- ifelse(frequency(serie_temporal) > 1, nsdiffs(serie_temporal), 0)
  list(d = d, D = D)
}
