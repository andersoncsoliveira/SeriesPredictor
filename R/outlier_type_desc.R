#' Descrição textual de tipos de outliers (AO, LS, TC, IO)
#'
#' Retorna uma descrição curta e padronizada para códigos de tipos de outliers
#' usados em diagnóstico de séries temporais (por exemplo, \code{"AO"},
#' \code{"LS"}, \code{"TC"} e \code{"IO"}).
#'
#' A função é implementada com um dicionário interno (fechado em um \code{local()}
#' para evitar poluir o ambiente do pacote) e devolve \code{"Tipo não reconhecido."}
#' para códigos não mapeados.
#'
#' @param type Vetor com códigos de tipo de outlier (ex.: \code{"AO"}, \code{"LS"}).
#'
#' @return Um vetor de caracteres com o mesmo comprimento de \code{type}, contendo
#'   as descrições correspondentes.
#'
#' @examples
#' outlier_type_desc("AO")
#' outlier_type_desc(c("LS", "TC", "XYZ"))
#'
#' @export
outlier_type_desc <- local({
  OUTLIER_TYPE_DESC <- c(
    AO = "AO (Additive Outlier): choque pontual; afeta uma observação.",
    LS = "LS (Level Shift): mudança de nível permanente a partir de um ponto.",
    TC = "TC (Temporary Change): mudança temporária com decaimento ao longo do tempo.",
    IO = "IO (Innovational Outlier): choque na inovação; efeito se propaga via dinâmica ARMA."
  )
  
  function(type) {
    type <- as.character(type)
    ifelse(type %in% names(OUTLIER_TYPE_DESC),
           unname(OUTLIER_TYPE_DESC[type]),
           "Tipo não reconhecido.")
  }
})