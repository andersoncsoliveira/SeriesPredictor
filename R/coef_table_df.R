#' Gerar tabela de coeficientes de um ajuste ARIMA em formato \code{data.frame}
#'
#' Constrói uma tabela de coeficientes a partir de um modelo ARIMA, usando
#' \code{summary_arima_custom()} (padrão) ou \code{summary_arima_compact()}
#' (modo compacto). Em seguida, garante a presença da coluna \code{coef} como
#' primeira coluna e remove nomes de linha.
#'
#' @param fit Objeto de modelo ARIMA aceito por \code{summary_arima_custom()}.
#' @param compact Se \code{TRUE}, utiliza \code{summary_arima_compact()} para
#'   remover coeficientes com estimativa zero (padrão: \code{FALSE}).
#'
#' @return Um \code{data.frame} com a coluna \code{coef} na primeira posição e
#'   as demais colunas geradas pelo resumo selecionado.
#'
#' @seealso \code{\link{summary_arima_custom}}, \code{\link{summary_arima_compact}}
#'
#' @examples
#' \dontrun{
#'   fit <- forecast::Arima(AirPassengers, order = c(1,1,1))
#'   coef_table_df(fit)
#'   coef_table_df(fit, compact = TRUE)
#' }
#'
#' @export
coef_table_df <- function(fit, compact = FALSE) {
  tab <- if (compact) summary_arima_compact(fit) else summary_arima_custom(fit)
  df <- as.data.frame(tab)
  df$coef <- rownames(df)
  rownames(df) <- NULL
  df <- df[, c("coef", setdiff(names(df), "coef")), drop = FALSE]
  df
}