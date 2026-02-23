#' Formatar números com casas decimais fixas
#'
#' Converte \code{x} para numérico (quando possível) e formata com um número
#' fixo de casas decimais. Entradas não finitas (\code{NA}, \code{Inf}, \code{-Inf})
#' são retornadas como \code{NA_character_}. Se \code{x} já for \code{character},
#' é retornado sem alteração.
#'
#' @param x Vetor a ser formatado. Pode ser numérico, ou coercível para numérico.
#'   Se \code{length(x) == 0}, retorna \code{""}.
#' @param digits Número de casas decimais (padrão: \code{4}).
#'
#' @return Um vetor \code{character} com os valores formatados. Quando
#'   \code{length(x) == 0}, retorna \code{""}. Quando \code{x} é \code{character},
#'   retorna \code{x} sem alteração.
#'
#' @examples
#' fmt_num(c(1.23456, 2, NA))
#' fmt_num("3.14159", digits = 2)
#' fmt_num(character(0))
#'
#' @export
fmt_num <- function(x, digits = 4) {
  if (length(x) == 0) return("")
  if (is.character(x)) return(x)
  
  out <- suppressWarnings(as.numeric(x))
  ifelse(
    is.na(out) | !is.finite(out),
    NA_character_,
    formatC(out, format = "f", digits = digits)
  )
}