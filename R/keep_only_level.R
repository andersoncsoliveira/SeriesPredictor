#' Manter apenas um nível de intervalo em um objeto de previsão
#'
#' Reduz um objeto de previsão (tipicamente retornado por funções como
#' \code{forecast::forecast()}) para conter somente as colunas de intervalo
#' inferior e superior correspondentes a um único nível (ex.: 80 ou 95).
#'
#' A função procura a coluna cujo nome contenha o nível desejado (por exemplo,
#' \code{95\\%} ou \code{95}). Se não encontrar exatamente uma coluna, usa como
#' fallback a última coluna de \code{lower} e \code{upper} (em geral, o maior
#' nível disponível).
#'
#' @param fc Objeto de previsão contendo, quando disponível, os componentes
#'   \code{$lower} e \code{$upper} (geralmente matrizes com colunas para níveis
#'   de confiança).
#' @param level Nível do intervalo a manter (padrão: \code{95}). Pode ser
#'   numérico (ex.: \code{95}) ou coercível para numérico.
#'
#' @return O próprio objeto \code{fc}, porém com:
#' \itemize{
#'   \item \code{fc$lower} e \code{fc$upper} reduzidos a uma única coluna
#'     (matriz com \code{drop = FALSE});
#'   \item nomes de coluna definidos como \code{<level>\\%} (ex.: \code{95\\%});
#'   \item \code{fc$level} ajustado para \code{as.numeric(level)}.
#' }
#' Se \code{fc$lower} ou \code{fc$upper} forem \code{NULL}, o objeto é retornado
#' sem modificações.
#'
#' @examples
#' \dontrun{
#'   library(forecast)
#'   fit <- auto.arima(AirPassengers)
#'   fc  <- forecast(fit, h = 12, level = c(80, 95))
#'
#'   fc95 <- keep_only_level(fc, 95)
#'   fc80 <- keep_only_level(fc, 80)
#' }
#'
#' @export
keep_only_level <- function(fc, level = 95) {
  if (is.null(fc$lower) || is.null(fc$upper)) return(fc)

  lower <- as.matrix(fc$lower)
  upper <- as.matrix(fc$upper)

  # tenta achar colunas "95%" / "95"
  lev_raw <- colnames(lower) %||% character(0)
  lev_num <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", lev_raw)))
  j <- which(lev_num == as.numeric(level))

  if (length(j) != 1) {
    # fallback: pega a última coluna (geralmente a maior %)
    j <- ncol(lower)
  }

  fc$lower <- lower[, j, drop = FALSE]
  fc$upper <- upper[, j, drop = FALSE]
  colnames(fc$lower) <- paste0(level, "%")
  colnames(fc$upper) <- paste0(level, "%")
  fc$level <- as.numeric(level)
  fc
}
