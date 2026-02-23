#' Converter objeto de previsão em \code{data.frame}
#'
#' Converte um objeto de previsão (por exemplo, retornado por
#' \code{forecast::forecast()}) em um \code{data.frame} com a previsão pontual e,
#' quando disponíveis, limites inferior e superior para um nível de confiança.
#'
#' A função procura a coluna correspondente ao nível indicado em \code{level}
#' dentro de \code{fc$lower} e \code{fc$upper}. Se não houver correspondência
#' exata, escolhe o nível mais próximo (quando possível) e, em último caso, usa
#' a primeira coluna.
#'
#' @param fc Objeto de previsão contendo ao menos \code{$mean}. Se \code{$lower}
#'   e \code{$upper} estiverem presentes, intervalos também serão adicionados.
#' @param level Nível do intervalo (em \%), usado para selecionar a coluna em
#'   \code{fc$lower}/\code{fc$upper} (padrão: \code{95}).
#' @param add_time Se \code{TRUE}, tenta incluir uma coluna \code{Tempo} obtida
#'   por \code{time(fc$mean)} (padrão: \code{FALSE}).
#'
#' @return Um \code{data.frame} com as colunas:
#' \describe{
#'   \item{\code{Passo}}{índice do horizonte de previsão (1..h)}
#'   \item{\code{Previsao}}{previsão pontual}
#'   \item{\code{Tempo}}{(opcional) índice temporal de \code{fc$mean}}
#'   \item{\code{LI_<level>}}{(opcional) limite inferior do intervalo}
#'   \item{\code{LS_<level>}}{(opcional) limite superior do intervalo}
#' }
#'
#' @examples
#' \dontrun{
#'   fit <- forecast::auto.arima(AirPassengers)
#'   fc  <- forecast::forecast(fit, h = 12, level = c(80, 95))
#'
#'   forecast_to_df(fc, level = 95)
#'   forecast_to_df(fc, level = 80, add_time = TRUE)
#' }
#'
#' @export
forecast_to_df <- function(fc, level = 95, add_time = FALSE) {
  m <- as.numeric(fc$mean)
  
  df <- data.frame(
    Passo    = seq_along(m),
    Previsao = m
  )
  
  if (isTRUE(add_time)) {
    tempo <- tryCatch(time(fc$mean), error = function(e) NULL)
    if (!is.null(tempo)) df <- cbind(Tempo = as.numeric(tempo), df)
  }
  
  if (!is.null(fc$lower) && !is.null(fc$upper)) {
    lower <- as.matrix(fc$lower)
    upper <- as.matrix(fc$upper)
    
    if (ncol(lower) == 1) {
      df[[paste0("LI_", level)]] <- as.numeric(lower[, 1])
      df[[paste0("LS_", level)]] <- as.numeric(upper[, 1])
    } else {
      lev_raw <- colnames(lower)
      lev_num <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", lev_raw)))
      j <- which(lev_num == as.numeric(level))
      if (length(j) != 1) {
        j <- which.min(abs(lev_num - as.numeric(level)))
        if (!is.finite(lev_num[j])) j <- 1
      }
      df[[paste0("LI_", level)]] <- as.numeric(lower[, j])
      df[[paste0("LS_", level)]] <- as.numeric(upper[, j])
    }
  }
  
  df
}