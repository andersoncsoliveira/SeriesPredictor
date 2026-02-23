#' Salvar um gráfico em arquivo (PNG ou PDF) a partir de uma função de plot
#'
#' Abre um dispositivo gráfico (\code{png()} ou \code{pdf()}), executa uma função
#' responsável por desenhar o gráfico e fecha o dispositivo ao final.
#'
#' Essa abordagem é útil quando o código do gráfico está encapsulado em uma
#' função (por exemplo, para reutilização em Shiny, relatórios e exportações).
#'
#' @param file Caminho do arquivo de saída (incluindo extensão, por exemplo
#'   \code{"grafico.png"} ou \code{"grafico.pdf"}).
#' @param device Dispositivo de saída: \code{"png"} ou \code{"pdf"}.
#' @param plot_fun Função sem argumentos que desenha o gráfico (deve chamar
#'   comandos de plotagem, como \code{plot()}, \code{lines()}, \code{ggplot2::print()},
#'   etc.).
#' @param width Largura em polegadas para \code{pdf()} (padrão: \code{11}).
#' @param height Altura em polegadas para \code{pdf()} (padrão: \code{6}).
#' @param px_w Largura em pixels para \code{png()} (padrão: \code{1200}).
#' @param px_h Altura em pixels para \code{png()} (padrão: \code{700}).
#' @param dpi Resolução em DPI para \code{png()} via \code{res} (padrão: \code{150}).
#'
#' @return Invisivelmente, \code{NULL}. O efeito principal é a criação do arquivo
#'   no caminho indicado em \code{file}.
#'
#' @examples
#' \dontrun{
#'   save_plot("exemplo.png", "png", plot_fun = function() {
#'     plot(1:10, (1:10)^2, type = "b")
#'   })
#'
#'   save_plot("exemplo.pdf", "pdf", plot_fun = function() {
#'     hist(rnorm(100))
#'   })
#' }
#'
#' @export
save_plot <- function(file, device = c("png", "pdf"), plot_fun,
                      width = 11, height = 6,
                      px_w = 1200, px_h = 700, dpi = 150) {
  device <- match.arg(device)
  if (device == "png") png(file, width = px_w, height = px_h, res = dpi)
  if (device == "pdf") pdf(file, width = width, height = height)
  on.exit(dev.off(), add = TRUE)
  plot_fun()
}