#' Executa o Aplicativo Shiny para Análise de Séries Temporais
#'
#' Esta função inicia um aplicativo Shiny contido no pacote.
#' @export

AppView<- function(){

  appDir<- system.file("AppView",package = "SeriesPredictor")
  if (appDir =="../inst/AppView") {
    stop("Não foi possível encontrar o aplicativo Shiny no pacote")
  }
  shiny::runApp()

}

