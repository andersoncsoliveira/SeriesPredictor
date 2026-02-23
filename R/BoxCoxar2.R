#' Estimar \eqn{\lambda} de Box-Cox via máxima verossimilhança com modelo AR
#'
#' Calcula a log-verossimilhança do parâmetro de transformação de Box-Cox
#' (\eqn{\lambda}) assumindo que a série transformada segue um modelo
#' autorregressivo (AR) de ordem fixa. A ordem pode ser informada pelo usuário
#' ou estimada automaticamente a partir de \code{ar(log(y), ...)$order}.
#'
#' Para cada valor de \code{lambda}, a série é transformada por:
#' \deqn{y^{(\lambda)} = \frac{y^\lambda - 1}{\lambda}, \quad \lambda \neq 0}
#' e por \eqn{\log(y)} quando \eqn{\lambda = 0}.
#'
#' Retorna o \eqn{\lambda} de máxima verossimilhança (MLE) e um intervalo de
#' confiança aproximado de 95\\% baseado na razão de verossimilhança.
#'
#' @param y Vetor numérico de dados (deve ser estritamente positivo e finito).
#' @param order Ordem do modelo AR. Se omitida, é estimada por \code{stats::ar()}.
#' @param lambda Vetor de candidatos a \eqn{\lambda} (padrão: \code{seq(-2, 2, .01)}).
#' @param plotit Se \code{TRUE}, plota a curva de log-verossimilhança e marca o MLE,
#'   o intervalo de confiança e o limiar do teste (padrão: \code{TRUE}).
#' @param method Método para estimação do modelo AR em \code{stats::ar()}:
#'   \code{"burg"}, \code{"yw"}, \code{"yule-walker"}, \code{"ols"} ou \code{"mle"}.
#'   Note que \code{"mle"} é mapeado internamente para \code{"yw"} em \code{ar()}.
#' @param ... Argumentos adicionais passados para \code{stats::ar()}.
#'
#' @return Invisivelmente, uma lista com:
#' \describe{
#'   \item{\code{lambda}}{vetor de valores de \eqn{\lambda} avaliados}
#'   \item{\code{loglike}}{log-verossimilhança para cada \eqn{\lambda}}
#'   \item{\code{mle}}{valor de \eqn{\lambda} que maximiza a log-verossimilhança}
#'   \item{\code{ci}}{intervalo de confiança aproximado (95\\%) para \eqn{\lambda}}
#' }
#'
#' @examples
#' \dontrun{
#'   y <- as.numeric(AirPassengers)
#'   res <- BoxCoxar2(y, method = "yw")
#'   res$mle
#'   res$ci
#' }
#'
#' @export
BoxCoxar2 <- function (y, order, lambda=seq(-2,2,.01), plotit=TRUE,
                        method=c("burg","yw","yule-walker","ols","mle"), ...) {

  method <- match.arg(method)
  y <- as.vector(y)

  if (any(!is.finite(y))) stop("y tem NA/Inf.")
  if (any(y <= 0)) stop("Data values must be positive")

  nlngmy <- sum(log(y))

  if (missing(order)) {
    order <- ar(log(y), method = if (method=="mle") "yw" else method)$order
  }

  xl <- lambda
  loglik <- numeric(length(xl))

  for (i in seq_along(xl)) {
    lam <- xl[i]

    if (abs(lam) > 1e-12) {
      yt <- (y^lam - 1)/lam
      ar.result <- ar(yt, method = if (method=="mle") "yw" else method, order.max = order, ...)
      loglik[i] <- -length(y)/2 * log(ar.result$var.pred) + (lam - 1) * nlngmy
    } else {
      yt <- log(y)
      ar.result <- ar(yt, method = if (method=="mle") "yw" else method, order.max = order, ...)
      loglik[i] <- -length(y)/2 * log(ar.result$var.pred) - nlngmy
    }
  }

  mle <- xl[which.max(loglik)][1]
  limit <- max(loglik) - 0.5 * qchisq(0.95, 1)
  in.interval <- xl[loglik >= limit]
  ci <- c(in.interval[1], rev(in.interval)[1])

  if (plotit) {
    plot(xl, loglik, type="l", xlab=expression(lambda), ylab="Log Likelihood")
    abline(v=mle, lty=2)
    abline(v=ci, lty=3)
    abline(h=limit, lty=2)
  }

  invisible(list(lambda=xl, loglike=loglik, mle=mle, ci=ci))
}
