#' Formatar valor-p para apresentação
#'
#' Converte um valor-p numérico em uma string formatada para exibição,
#' utilizando quatro casas decimais. Valores muito pequenos são exibidos
#' como \code{"<0.0001"}. Valores \code{NA} são mantidos como \code{NA_character_}.
#'
#' @param p Valor-p numérico.
#'
#' @return Uma string formatada com o valor-p.
#'
#' @examples
#' fmt_p(0.03456)
#' fmt_p(0.00003)
#' fmt_p(NA)
#'
#' @export
fmt_p <- function(p) {
  if (is.na(p)) NA_character_
  else if (p < 0.0001) "<0.0001"
  else sprintf("%.4f", p)
}