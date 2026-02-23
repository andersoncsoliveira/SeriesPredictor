#' Resumo de coeficientes de um modelo ARIMA com estatísticas e valor-p
#'
#' Gera uma tabela resumo dos coeficientes de um modelo ARIMA (por exemplo,
#' objetos retornados por \code{forecast::Arima()} ou \code{stats::arima()}),
#' incluindo estimativa, erro padrão (quando disponível), estatística t e
#' valor-p bicaudal.
#'
#' O erro padrão é extraído da diagonal de \code{model$var.coef}. A estatística t
#' é calculada como \code{coef / se}. O valor-p é calculado usando a distribuição
#' t de Student com graus de liberdade aproximados por
#' \code{max(1, length(residuals) - k)}, onde \code{k} é o número de coeficientes
#' com erro padrão finito.
#'
#' @param model Objeto de modelo contendo ao menos \code{$coef}. Para cálculo de
#'   erro padrão, deve conter \code{$var.coef}. Resíduos são obtidos via
#'   \code{stats::residuals(model)} para estimativa de graus de liberdade.
#'
#' @return Um \code{data.frame} com as colunas:
#' \describe{
#'   \item{\code{coef}}{nome do coeficiente}
#'   \item{\code{estimativa}}{estimativa do coeficiente}
#'   \item{\code{erro padrão}}{erro padrão (quando disponível)}
#'   \item{\code{estatistica t}}{estatística t (quando possível)}
#'   \item{\code{valor-p}}{valor-p formatado (\code{"<0.0001"} quando aplicável)}
#' }
#'
#' @examples
#' \dontrun{
#'   fit <- forecast::Arima(AirPassengers, order = c(1,1,1))
#'   summary_arima_custom(fit)
#' }
#'
#' @export
summary_arima_custom <- function(model) {
  coefs <- model$coef
  cn <- names(coefs)
  
  se    <- rep(NA_real_, length(coefs)); names(se)    <- cn
  tstat <- rep(NA_real_, length(coefs)); names(tstat) <- cn
  pnum  <- rep(NA_real_, length(coefs)); names(pnum)  <- cn
  
  Var <- model$var.coef
  
  if (!is.null(Var)) {
    d <- tryCatch(diag(Var), error = function(e) NULL)
    
    if (!is.null(d)) {
      d <- as.numeric(d)
      # evita sqrt de negativo e se=0
      d[!is.finite(d) | d <= 0] <- NA_real_
      se_est <- sqrt(d)
      names(se_est) <- names(diag(Var))
      
      if (!is.null(names(se_est))) {
        common <- intersect(names(se_est), cn)
        se[common] <- se_est[common]
      } else if (length(se_est) == length(coefs)) {
        se[] <- se_est
      } else {
        k <- min(length(se_est), length(coefs))
        se[1:k] <- se_est[1:k]
      }
    }
    
    ok <- is.finite(se) & se > 0
    tstat[ok] <- coefs[ok] / se[ok]
    
    k_est <- sum(ok)
    rr <- tryCatch(stats::residuals(model), error = function(e) numeric(0))
    rr <- rr[is.finite(rr)]
    df <- max(1, length(rr) - k_est)
    
    pnum[ok] <- 2 * (1 - stats::pt(abs(tstat[ok]), df))
  }
  
  p_fmt <- ifelse(
    is.finite(pnum),
    ifelse(pnum < 1e-4, "<0.0001", sprintf("%.4f", pnum)),
    NA_character_
  )
  
  data.frame(
    coef            = cn,
    estimativa      = as.numeric(coefs),
    `erro padrão`   = as.numeric(se),
    `estatistica t` = as.numeric(tstat),
    `valor-p`       = p_fmt,
    check.names = FALSE
  )
}