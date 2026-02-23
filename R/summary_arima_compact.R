#' Resumo compacto de coeficientes de um modelo ARIMA
#'
#' Gera uma versão compacta do resumo de coeficientes produzido por
#' \code{summary_arima_custom()}, com a opção de remover coeficientes cuja
#' estimativa seja exatamente zero.
#'
#' @param model Objeto de modelo ARIMA (por exemplo, retornado por
#'   \code{forecast::Arima()} ou \code{stats::arima()}), aceito por
#'   \code{summary_arima_custom()}.
#' @param drop_zero Se \code{TRUE}, remove linhas onde a coluna \code{estimativa}
#'   é zero (padrão: \code{TRUE}).
#'
#' @return Um \code{data.frame} no mesmo formato de \code{summary_arima_custom()},
#'   porém com as linhas filtradas quando \code{drop_zero = TRUE}. Se todas as
#'   linhas forem removidas, retorna um \code{data.frame} vazio com as colunas
#'   originais.
#'
#' @seealso \code{\link{summary_arima_custom}}
#'
#' @examples
#' \dontrun{
#'   fit <- forecast::Arima(AirPassengers, order = c(1,1,1))
#'   summary_arima_compact(fit)
#'   summary_arima_compact(fit, drop_zero = FALSE)
#' }
#'
#' @export
summary_arima_compact <- function(model, drop_zero = TRUE) {
  tab <- summary_arima_custom(model)
  
  keep <- rep(TRUE, nrow(tab))
  
  if (isTRUE(drop_zero) && "estimativa" %in% colnames(tab)) {
    est <- as.numeric(tab[, "estimativa"])
    keep <- is.finite(est) & (est != 0)
  }
  
  tab2 <- tab[keep, , drop = FALSE]
  if (nrow(tab2) == 0) tab2 <- tab[0, , drop = FALSE]
  tab2
}