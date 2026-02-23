#' Interpretar lags a partir de texto
#'
#' Converte uma string contendo defasagens (lags) separadas por vírgula, ponto e
#' vírgula ou espaços em um vetor de inteiros positivos, removendo duplicatas e
#' ordenando o resultado.
#'
#' Entradas \code{NULL} ou vazias (após \code{trimws()}) retornam
#' \code{integer(0)}.
#'
#' @param txt Texto com lags, por exemplo \code{"1, 2, 12"} ou \code{"1 2 12"}.
#'
#' @return Vetor de inteiros positivos, único e ordenado. Quando não há valores
#'   válidos, retorna \code{integer(0)}.
#'
#' @examples
#' parse_lags("1, 2, 12")
#' parse_lags("  3;6; 6  9 ")
#' parse_lags(NULL)
#' parse_lags("abc, -1, 0")
#'
#' @export
parse_lags <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(txt))) return(integer(0))
  x <- unlist(strsplit(txt, "[,;\\s]+"))
  x <- suppressWarnings(as.integer(x))
  x <- x[is.finite(x) & x > 0]
  sort(unique(x))
}