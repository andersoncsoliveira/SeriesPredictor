#' Converter tabela chave-valor em layout de duas colunas
#'
#' Reorganiza um \code{data.frame} no formato chave-valor (colunas \code{Campo} e
#' \code{Valor}) para um layout com duas colunas de pares, útil para exibição em
#' relatórios ou telas (por exemplo, “Campo 1 / Valor 1” e “Campo 2 / Valor 2”).
#'
#' A função divide as linhas em duas metades: a primeira metade compõe a coluna 1
#' e a segunda metade compõe a coluna 2. Quando o número de linhas é ímpar, a
#' segunda metade é preenchida com strings vazias para manter o mesmo número de
#' linhas nas duas colunas.
#'
#' @param df_kv \code{data.frame} contendo obrigatoriamente as colunas
#'   \code{Campo} e \code{Valor}.
#'
#' @return Um \code{data.frame} com quatro colunas:
#' \describe{
#'   \item{\code{Campo_1}}{campos da primeira metade}
#'   \item{\code{Valor_1}}{valores correspondentes}
#'   \item{\code{Campo_2}}{campos da segunda metade (ou vazio se faltante)}
#'   \item{\code{Valor_2}}{valores correspondentes (ou vazio se faltante)}
#' }
#' Se \code{df_kv} tiver zero linhas, retorna um \code{data.frame} vazio com as
#' colunas esperadas.
#'
#' @examples
#' df <- data.frame(
#'   Campo = c("A", "B", "C", "D", "E"),
#'   Valor = c("1", "2", "3", "4", "5"),
#'   stringsAsFactors = FALSE
#' )
#' kv_to_2col(df)
#'
#' @export
kv_to_2col <- function(df_kv) {
  stopifnot(all(c("Campo", "Valor") %in% names(df_kv)))
  n <- nrow(df_kv)
  if (n == 0) return(data.frame(
    `Campo 1` = character(0), `Valor 1` = character(0),
    `Campo 2` = character(0), `Valor 2` = character(0)
  ))
  
  k <- ceiling(n / 2)
  
  a <- df_kv[seq_len(k), c("Campo", "Valor"), drop = FALSE]
  b <- df_kv[seq_len(n - k) + k, c("Campo", "Valor"), drop = FALSE]
  
  if (nrow(b) < k) {
    b <- rbind(
      b,
      data.frame(Campo = rep("", k - nrow(b)), Valor = rep("", k - nrow(b)))
    )
  }
  
  data.frame(
    Campo_1 = a$Campo, Valor_1 = a$Valor,
    Campo_2 = b$Campo, Valor_2 = b$Valor,
    stringsAsFactors = FALSE
  )
}