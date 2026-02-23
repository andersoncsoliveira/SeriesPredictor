#' Plotar valores-p do teste de Box-Pierce por defasagem
#'
#' Calcula e plota os valores-p do teste de Box-Pierce (\code{stats::Box.test})
#' para uma sequência de defasagens (\emph{lags}), permitindo avaliar indícios de
#' autocorrelação remanescente em resíduos de um modelo.
#'
#' O número máximo de defasagens é definido como \code{floor(10 * log10(n))},
#' onde \code{n} é o tamanho do vetor de resíduos.
#'
#' @param residuals Vetor numérico de resíduos (por exemplo, resíduos de um modelo
#'   ARIMA).
#'
#' @return Invisivelmente, \code{NULL}. A função produz um gráfico como efeito
#'   principal.
#'
#' @examples
#' \dontrun{
#'   fit <- forecast::auto.arima(AirPassengers)
#'   box_pierce_plot(residuals(fit))
#' }
#'
#' @export
box_pierce_plot <- function(residuals) {
  n <- length(residuals)
  max_lag <- floor(10 * log10(n))
  p_values <- rep(NA_real_, max_lag + 1)

  for (lag in 1:max_lag) {
    test <- Box.test(residuals, lag = lag, type = "Box-Pierce")
    p_values[lag + 1] <- test$p.value
  }

  plot(0:max_lag, p_values,
       pch = 19,col=red,
       xlab = "Lag", ylab = "valor-p", main = "Teste de Box-Pierce",
       ylim = c(0, 1))
  abline(h = 0.05, lty = 2)
  text(max_lag / 2, 0.06, "Significance level 0.05", pos = 3)
  grid()
}
