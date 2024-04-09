#' Executa o Aplicativo Shiny para Análise de Séries Temporais
#'
#' Esta função inicia um aplicativo Shiny contido no pacote.
#' @export

AppView<- function(){
  appDir<- system.file("shiny",package = "SeriesPredictor")
  if (appDir ==" ") {
    stop("Não foi possível encontrar o aplicativo Shiny no pacote")
  }
  shiny::runApp(appDir)

}

