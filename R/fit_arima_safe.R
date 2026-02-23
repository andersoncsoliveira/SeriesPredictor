#' Ajustar ARIMA com fallback automático entre métodos de estimação
#'
#' Tenta ajustar um modelo ARIMA usando \code{forecast::Arima()} com diferentes
#' métodos de estimação (\code{"CSS-ML"}, \code{"ML"}, \code{"CSS"}), seguindo
#' uma ordem de preferência definida pelo usuário. Se o ajuste falhar para um
#' método, a função tenta o próximo até obter sucesso.
#'
#' O controle do otimizador é fixado em \code{optim.control = list(maxit = 2000)}.
#' Se todos os métodos falharem, a função interrompe com a mensagem do último
#' erro capturado.
#'
#' @param ... Argumentos repassados diretamente para \code{forecast::Arima()},
#'   exceto \code{method} (definido internamente). Por exemplo: \code{y},
#'   \code{order}, \code{seasonal}, \code{xreg}, etc.
#' @param prefer Ordem de preferência inicial para o método de estimação.
#'   Pode ser \code{"CSS-ML"} (padrão), \code{"ML"} ou \code{"CSS"}.
#'
#' @return Um objeto de classe \code{"Arima"} retornado por \code{forecast::Arima()}.
#'
#' @examples
#' \dontrun{
#'   fit <- fit_arima_safe(AirPassengers, order = c(1,1,1))
#'   fit2 <- fit_arima_safe(AirPassengers, order = c(1,1,1), prefer = "ML")
#' }
#'
#' @export
fit_arima_safe <- function(..., prefer = c("CSS-ML", "ML", "CSS")) {
  prefer <- match.arg(prefer)
  
  try_methods <- switch(
    prefer,
    "CSS-ML" = c("CSS-ML", "ML", "CSS"),
    "ML"     = c("ML", "CSS-ML", "CSS"),
    "CSS"    = c("CSS", "CSS-ML", "ML")
  )
  
  last_err <- NULL
  for (m in try_methods) {
    fit <- tryCatch(
      forecast::Arima(..., method = m, optim.control = list(maxit = 2000)),
      error = function(e) { last_err <<- e; NULL }
    )
    if (!is.null(fit)) return(fit)
  }
  
  stop(last_err$message)
}