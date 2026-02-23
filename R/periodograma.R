#' Calcular periodograma (densidade espectral) de uma série temporal
#'
#' Calcula um periodograma a partir da Transformada Rápida de Fourier (FFT) ou
#' diretamente em frequências especificadas, retornando um \code{data.frame}
#' com período e densidade.
#'
#' Por padrão (\code{fr = "default"}), o periodograma é calculado para as
#' frequências harmônicas usuais \eqn{k/n}, com \eqn{k = 1, \dots, \lfloor n/2 \rfloor},
#' onde \eqn{n} é o tamanho da série. Alternativamente, \code{fr} pode ser:
#' \itemize{
#'   \item um único número (\code{length(fr) == 1}): a série é completada com zeros
#'   até comprimento \code{fr} (zero-padding) e o periodograma é avaliado na grade
#'   \eqn{k/fr};
#'   \item um vetor numérico de frequências: o periodograma é avaliado diretamente
#'   nas frequências fornecidas.
#' }
#'
#' O resultado final é expresso em termos de \strong{período} (\code{1/f}) e
#' ordenado de forma crescente por período.
#'
#' @param z Vetor numérico contendo a série temporal.
#' @param fr Grade de frequência. Use \code{"default"} para a grade padrão; um
#'   único número para definir o tamanho da FFT com zero-padding; ou um vetor
#'   numérico de frequências onde o periodograma será avaliado.
#'
#' @return Um \code{data.frame} com duas colunas:
#' \describe{
#'   \item{\code{periodo}}{período associado a cada frequência (\code{1/f})}
#'   \item{\code{densidade}}{valor do periodograma (densidade espectral)}
#' }
#' O \code{data.frame} é retornado ordenado por \code{periodo}.
#'
#' @examples
#' set.seed(1)
#' z <- sin(2*pi*(1:200)/12) + rnorm(200, sd = 0.3)
#'
#' # Grade padrão
#' p1 <- periodograma(z)
#'
#' # FFT com zero-padding até 512 pontos
#' p2 <- periodograma(z, fr = 512)
#'
#' # Avaliar em frequências específicas
#' p3 <- periodograma(z, fr = c(1/12, 1/6, 1/4))
#'
#' @export
periodograma <- function(z, fr = "default") {
  n <- length(z)
  
  if (identical(fr, "default")) {
    ans <- cbind((1:floor(n / 2)) / n, (Mod(fft(z))^2 / n)[2:(n %/% 2 + 1)])
  } else {
    stopifnot(is.numeric(fr))
    
    if (length(fr) == 1) {
      madj <- fr - n
      x <- c(z, rep(0, madj))
      ans <- cbind((1:floor(fr / 2)) / fr, (Mod(fft(x))^2 / n)[2:(fr %/% 2 + 1)])
    } else {
      ans <- cbind(fr, Re(sapply(fr, function(f0) {
        y <- sum(z * exp((0 + 1i) * 2 * pi * f0 * seq(0, n - 1)))
        y * Conj(y)
      }) / length(z)))
    }
  }
  
  periodo <- 1 / ans[, 1]
  ans[, 1] <- periodo
  colnames(ans) <- c("periodo", "densidade")
  ans <- as.data.frame(ans)
  ans <- ans[order(ans$periodo), ]
  ans
}