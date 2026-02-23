#' Obter rótulo temporal de forma segura em um objeto \code{ts}
#'
#' Retorna o rótulo de tempo (via \code{time()}) associado a um índice
#' específico de uma série temporal. Caso \code{time(x_ts)} não esteja
#' disponível, ou o índice seja inválido, retorna \code{NA_character_}.
#'
#' @param x_ts Objeto de série temporal (tipicamente \code{ts} ou similar),
#'   do qual será extraído \code{time(x_ts)}.
#' @param t_index Índice (1-based) da posição desejada no vetor de tempos.
#'
#' @return Uma string com o rótulo de tempo correspondente, ou
#'   \code{NA_character_} em caso de falha/índice inválido.
#'
#' @examples
#' x <- ts(1:12, start = c(2020, 1), frequency = 12)
#' safe_time_label(x, 1)
#' safe_time_label(x, 12)
#' safe_time_label(x, 999)  # NA
#'
#' @export
safe_time_label <- function(x_ts, t_index) {
  tt <- tryCatch(time(x_ts), error = function(e) NULL)
  if (is.null(tt) || !length(tt)) return(NA_character_)
  if (!is.finite(t_index) || t_index < 1 || t_index > length(tt)) return(NA_character_)
  as.character(tt[t_index])
}