#' Teste de Fisher para detecção de periodicidade no periodograma
#'
#' Aplica uma versão do teste de Fisher (g-test) para avaliar se existe uma
#' componente periódica dominante em um periodograma. O teste compara o maior
#' pico espectral com a soma total do espectro, produzindo a estatística
#' \code{g} e um valor-p aproximado.
#'
#' A função assume que \code{P} contém duas colunas: período (1ª coluna) e
#' densidade/espectro (2ª coluna). O período candidato é obtido pelo ponto de
#' maior densidade.
#'
#' Regras adicionais implementadas:
#' \itemize{
#'   \item Se o período estimado for menor que 3, o valor-p é forçado para 1
#'   (isto é, não sinaliza periodicidade curta).
#'   \item Se o valor-p numérico resultar \code{NA} ou maior que 1, ele é truncado
#'   para 1.
#' }
#'
#' @param P Matriz ou \code{data.frame} do periodograma, com:
#'   \code{P[,1]} = período e \code{P[,2]} = densidade (espectro).
#' @param alpha Nível de significância para decisão (padrão: \code{0.05}).
#'
#' @return Uma lista com:
#' \describe{
#'   \item{\code{g}}{estatística \code{g} (razão entre o maior pico e a soma do espectro)}
#'   \item{\code{p_num}}{valor-p numérico do teste}
#'   \item{\code{valor-p}}{valor-p formatado (string), com \code{"<0.0001"} quando aplicável}
#'   \item{\code{periodo}}{período detectado (arredondado), ou \code{1} quando não significativo}
#'   \item{\code{has_period}}{\code{TRUE} se \code{p_num < alpha}, caso contrário \code{FALSE}}
#' }
#'
#' @examples
#' \dontrun{
#'   P <- periodograma(AirPassengers)
#'   res <- Fisher.test(P, alpha = 0.05)
#'   res$has_period
#'   res$periodo
#'   res$`valor-p`
#' }
#'
#' @export
Fisher.test <- function(P, alpha = 0.05) {
  spec <- P[, 2]
  p <- which.max(spec)
  
  g <- max(spec, na.rm = TRUE) / sum(spec, na.rm = TRUE)
  n <- nrow(P)
  periodo <- round(P[p, 1], 2)
  
  x <- floor(1 / g)
  i <- 1:x
  valorp_num <- sum(choose(n, i) * (-1)^(i - 1) * (1 - i * g)^(n - 1))
  
  if (is.na(valorp_num) || valorp_num > 1) valorp_num <- 1
  if (periodo < 3) valorp_num <- 1
  
  has_period <- is.finite(valorp_num) && (valorp_num < alpha)
  periodo_out <- if (has_period) periodo else 1
  
  valorp_formatted <- if (valorp_num < 0.0001) "<0.0001" else sprintf("%.4f", valorp_num)
  
  list(
    g = round(g, 5),
    p_num = valorp_num,
    `valor-p` = valorp_formatted,
    periodo = periodo_out,
    has_period = has_period
  )
}