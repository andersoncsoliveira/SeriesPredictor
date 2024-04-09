library(shiny)
library(shinydashboard)
library(forecast)
library(dplyr)
library(readxl)
library(highcharter)
library(shinyWidgets)
library(shinythemes)
library(randtests)
library(ptest)

ui <- dashboardPage(
  dashboardHeader(title = "Análise de Séries Temporais"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Importar e Visualizar Dados", tabName = "importAndView", icon = icon("upload")),
      menuItem("Transformar em Série Temporal", tabName = "timeSeries", icon = icon("line-chart")),
      menuItem("Testes Estatísticos", tabName = "TestSerie", icon = icon("pen-to-square"),
               menuSubItem("Tendência", tabName = "trendTab"),
               menuSubItem("Sazonalidade", tabName = "seasonTab")),
      menuItem("Ajuste de Modelos ARIMA/SARIMA",tabName ="modelo",icon = icon("gears"),
               menuSubItem("Ajuste Automático", tabName = "AutoSerie", icon = icon("play")),
               menuSubItem("Identificação e Ajuste de Modelos", tabName = "ModSerie", icon = icon("play")))
    )
  ),
  dashboardBody(
    theme = shinytheme('cosmo'),
    tabItems(
      # Aba para importação e visualização de dados
      tabItem(
        tabName = "importAndView",
        fluidPage(
          titlePanel("Importar e Visualizar Dados"),
          fileInput("file", "Selecione a base de dados"),
          tableOutput("viewData")
        )
      ),

      # Aba para transformação em série temporal e análise
      tabItem(
        tabName = "timeSeries",
        fluidPage(
          titlePanel("Transformação em Série Temporal e Análise"),
          selectInput("col_valores", "Selecione a série temporal", ""),
          numericInput("start_period", "Início da Série:", value = 1),
          numericInput("frequency", "Periodicidade (mês/ano):", value = 1),
          actionButton("btnTransform", "Transformar em Série Temporal"),
          plotOutput("timeSeriesPlot")
        )
      ),

      # Aba para Teste de Tendência
      tabItem(
        tabName = "trendTab",
        fluidPage(
          titlePanel("Teste de Tendência"),
          plotOutput("trendPlot"),
          verbatimTextOutput("trendResult")
        )
      ),

      # Aba para Teste de Sazonalidade
      tabItem(
        tabName = "seasonTab",
        fluidPage(
          titlePanel("Teste de Sazonalidade"),
          plotOutput("trendPlot2"),
          verbatimTextOutput("trendResult2")
        )
      ),

      # Aba para Auto Arima
      tabItem(
        tabName = "AutoSerie",
        fluidPage(
          titlePanel("Ajuste Automático "),
          tabBox(
            tabPanel("Ajuste", actionButton("btnAutoArima", "Executar Auto Arima"),verbatimTextOutput("autoArimaOutput")),
            tabPanel("Avaliação de Modelos (resíduos)", plotOutput("residualPlotAutoArima"), verbatimTextOutput("pValueOutputAutoArima")),
            tabPanel("Predição",numericInput("np1", "Número de Predicoes", value = 10), plotOutput("previsaoGraficoAutoArima"), dataTableOutput("tabelaPrevisaoAutoArima"))
          )
        )
      ),

      # Aba para Ajuste de ARIMA/SARIMA
      tabItem(
        tabName ="ModSerie",
        fluidPage(
          titlePanel("Identificação e Ajuste de Modelos"),
          tabBox(
            tabPanel("Ajuste",
                     fluidRow(
                       column(10,
                              textOutput("numDiffs"), # Para mostrar o número de diferenças sazonais
                              plotOutput("acfPlot"), # Para a função de autocorrelação
                              plotOutput("pacfPlot")
                       ),
                       column(10,
                              numericInput("order_AR", "Ordem AR:", value = 1),
                              numericInput("order_I", "Ordem I:", value = 1),
                              numericInput("order_MA", "Ordem MA:", value = 1),
                              numericInput("seasonal_AR", "Ordem AR Sazonal:", value = 0),
                              numericInput("seasonal_I", "Ordem I Sazonal:", value = 0),
                              numericInput("seasonal_MA", "Ordem MA Sazonal:", value = 0),
                              numericInput("seasonal_period", "Período Sazonal:", value = 12),
                       ),
                       column(10,
                              actionButton("btnConstruirModelo", "Construir Modelo"),
                              verbatimTextOutput("ArimaOutput")
                       )
                     )
            ),
            tabPanel("Avaliação de Modelos (resíduos)", plotOutput("residualPlot"), verbatimTextOutput("pValueOutput")),
            tabPanel("Predição", numericInput("np2", "Número de Predicoes", value = 10),plotOutput("previsaoGrafico"), dataTableOutput("tabelaPrevisao"))
          )
        )
      ) #TERMINA

    )
  )

)

server <- function(input, output, session) {
  ##Carregar dados######
  dados <- reactive({
    req(input$file)
    readxl::read_xlsx(input$file$datapath)
  })

  observe({
    updateSelectInput(session, "col_valores", choices = names(dados()))
  })

  ##Renderizar tabela com dados importados######
  output$viewData <- renderTable({
    req(dados())  # Garantir que os dados estejam disponíveis
    dados()  # Exibe os dados importados
  })

  ##Transformar em série temporal######
  dados_ts <- eventReactive(input$btnTransform, {
    req(input$col_valores)
    valores <- dados()[[input$col_valores]]
    ts(valores, start = input$start_period, frequency = input$frequency)
  })

  ##Renderizar gráfico da série temporal######
  output$timeSeriesPlot <- renderPlot({
    req(dados_ts())
    plot(dados_ts(), main = "Série Temporal", xlab = "Tempo", ylab = "Valor", type = "o")
  })

  # Renderizar gráfico para Teste de Tendência
  output$trendPlot <- renderPlot({
    dados_reactive <- dados_ts()
    if (!is.null(dados_reactive) && length(dados_reactive) >= 2) {
      dado_d <- decompose(dados_reactive)
      plot(dado_d$trend)  # Gráfico de decomposição
    } else {
      plot(1, type = "n", xlab = "", ylab = "", main = "Série temporal não possui período ou tem menos de 2 observações")
    }
  })


  # Calcular e renderizar resultado do Teste de Tendência
  output$trendResult <- renderText({
    dados_reactive <- dados_ts()
    if (!is.null(dados_reactive)) {
      p_valor <- cox.stuart.test(dados_reactive)$p.value
      result_text <- sprintf("Resultado do Teste de Tendência: O valor é %.1e", p_valor)
      if (p_valor < 0.05) {
        result_text <- paste(result_text,"\n", "Existe tendência nos dados (valor-p < 0.05)")
      } else {
        result_text <- paste(result_text,"\n", "Não há evidência de tendência nos dados (valor-p >= 0.05)")
      }
      result_text
    }
  })

  #0----------Sazonalidade--------------------------#
  output$trendPlot2 <- renderPlot({
    dados_reactive <- dados_ts()
    if (!is.null(dados_reactive) && length(dados_reactive) >= 2) {
      dado_d <- decompose(dados_reactive)
      plot(dado_d$seasonal)  # Gráfico de decomposição
    } else {
      plot(1, type = "n", xlab = "", ylab = "", main = "Série temporal não possui período ou tem menos de 2 observações")
    }
  })

  # Calcular e renderizar resultado do Teste de Sazonalidade
  output$trendResult2 <- renderText({
    dados_reactive2 <- dados_ts()
    if (!is.null(dados_reactive2)) {
      p_valor2 <- ptestg(dados_reactive2,method = c("Fisher"))$pvalue
      result_text2 <- sprintf("Resultado do Teste de Sazonalidade: O valor-p é %.1e", p_valor2)
      if (p_valor2 < 0.05) {
        result_text2 <- paste(result_text2,"\n", "Existe sazonalidade nos dados (valor-p < 0.05)")
      } else {
        result_text2 <- paste(result_text2,"\n", "Não há evidência de sazonalidade nos dados (valor-p >= 0.05)")
      }
      result_text2
    }
  })

  ################Ajuste do AUTO-ARIMA

  autoModel <- reactiveVal()

  observeEvent(input$btnAutoArima, {
    req(dados_ts())
    model <- forecast::auto.arima(dados_ts(), seasonal = TRUE)
    autoModel(model) # Armazena o modelo na variável reativa
    output$autoArimaOutput <- renderPrint({
      print(model)
    })
  })

  output$residualPlotAutoArima <- renderPlot({
    req(autoModel())
    # Captura o gráfico de resíduos
    checkresiduals(autoModel())

  })

  output$pValueOutputAutoArima <- renderPrint({
    req(autoModel())
    res <- residuals(autoModel())
    box_test <- Box.test(res, lag = log(length(res)), type = "Ljung-Box")
    p_value <- box_test$p.value
    paste("Valor-p do teste de Ljung-Box: ", format(p_value, scientific = TRUE))
  })


  output$previsaoGraficoAutoArima <- renderPlot({
    req(autoModel())
    forecast_length <- input$np1 # Número de projeções definido pelo usuário
    forecasted_values <- forecast::forecast(autoModel(), h = forecast_length)
    plot(forecasted_values)
  })

  # Para exibir a tabela de previsões
  output$tabelaPrevisaoAutoArima <- renderDataTable({
    req(autoModel())
    forecast_length <- input$np1
    forecasted_values <- forecast::forecast(autoModel(), h = forecast_length)

    data.frame(
      Tempo = seq(length(forecasted_values$mean) - forecast_length + 1, length(forecasted_values$mean)),
      Previsão = forecasted_values$mean,
      Limite_Inferior = forecasted_values$lower[,1],
      Limite_Superior = forecasted_values$upper[,2]
    )
  })

  ####Ajuste do modelo por identificação do modelo

  # Reativa para calcular diferenças
  diffs <- reactive({
    req(dados_ts())
    determineDiffs(dados_ts())
  })

  # Exibe o número de diferenças sazonais e não sazonais
  output$numDiffs <- renderText({
    difs <- diffs()
    paste("Diferenças  (d):", difs$d, "\n
          Diferenças sazonais (D):", difs$D)
  })

  # Série temporal diferenciada
  serieDiferenciada <- reactive({
    req(dados_ts())
    difs <- diffs()
    applyDiffs(dados_ts(), difs$d, difs$D)
  })

  # Gráfico ACF
  output$acfPlot <- renderPlot({
    req(serieDiferenciada())
    ggAcf(serieDiferenciada())
  })

  # Gráfico PACF
  output$pacfPlot <- renderPlot({
    req(serieDiferenciada())
    ggAcf(serieDiferenciada(),type="partial")
  })

  # Construir modelo ARIMA com parâmetros especificados

  model <- reactiveVal()
  observeEvent(input$btnConstruirModelo, {
    req(dados_ts())

    modelo <- Arima(dados_ts(), order = c(input$order_AR, input$order_I, input$order_MA),
                    seasonal = list(order = c(input$seasonal_AR, input$seasonal_I, input$seasonal_MA), period = input$seasonal_period))
    output$ArimaOutput <- renderPrint({
      summary(modelo)
    })
    model(modelo)
  })

  #Residuos
  output$residualPlot<- renderPlot({
    req(model())
    # Captura o gráfico de resíduos
    checkresiduals(model())

  })

  output$pValueOutput <- renderPrint({
    req(model())
    res <- residuals(model())
    box_test <- Box.test(res, lag = log(length(res)), type = "Ljung-Box")
    p_value <- box_test$p.value
    paste("Valor-p do teste de Ljung-Box: ", format(p_value, scientific = TRUE))
  })

  output$previsaoGrafico <- renderPlot({
    req(model())
    forecast_length <- input$np2 # Número de projeções definido pelo usuário
    forecasted_values <- forecast::forecast(model(), h = forecast_length)
    plot(forecasted_values)
  })

  # Para exibir a tabela de previsões
  output$tabelaPrevisao <- renderDataTable({
    req(model())
    forecast_length <- input$np2
    forecasted_values <- forecast::forecast(model(), h = forecast_length)

    data.frame(
      Tempo = seq(length(forecasted_values$mean) - forecast_length + 1, length(forecasted_values$mean)),
      Previsão = forecasted_values$mean,
      Limite_Inferior = forecasted_values$lower[,1],
      Limite_Superior = forecasted_values$upper[,2]
    )
  })


}

shinyApp(ui, server)
