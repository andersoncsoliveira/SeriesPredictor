#' Operador de coalescência para NULL
#'
#' Retorna \code{a} se não for \code{NULL}; caso contrário retorna \code{b}.
#'
#' @param a Objeto a ser testado.
#' @param b Valor alternativo se \code{a} for \code{NULL}.
#'
#' @return \code{a} se não for \code{NULL}; caso contrário \code{b}.
#'
#' @name null_coalesce
#' @rdname null_coalesce
#' @export
`%||%` <- function(a, b) if (!is.null(a)) a else b
