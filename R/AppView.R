#' Executa o Aplicativo Shiny para Análise de Séries Temporais
#'
#' Esta funcões inicia o aplicativo Series Predictor Shiny incluído no pacote.
#'
#' @export
AppView <- function() {
  appDir <- system.file("AppView", package = "SeriesPredictor")
  if (appDir == "") {
    stop("Não foi possível encontrar o aplicativo Shiny no pacote (inst/AppView).", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}


