#' Aplicar transformação de Box-Cox
#'
#' Aplica a transformação de Box-Cox em um vetor numérico \code{y} para um valor
#' específico de \code{lambda}. Quando \code{lambda} é aproximadamente zero, a
#' transformação é \code{log(y)}; caso contrário, utiliza
#' \eqn{(y^\lambda - 1)/\lambda}.
#'
#' @param y Vetor numérico (em geral, espera-se \code{y > 0} quando \code{lambda}
#'   for próximo de 0, pois \code{log(y)} é usado).
#' @param lambda Parâmetro da transformação de Box-Cox.
#'
#' @return Um vetor numérico com \code{y} transformado.
#'
#' @examples
#' y <- 1:10
#' boxcox_transform(y, lambda = 0.5)
#' boxcox_transform(y, lambda = 0)   # usa log
#'
#' @export
boxcox_transform <- function(y, lambda) {
  if (abs(lambda) > 1e-12) (y^lambda - 1) / lambda else log(y)
}