library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(DT)
library(forecast)
library(randtests)
library(openxlsx)
library(tsoutliers)
library(TSA)

# UI ------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Séries Temporais"),

  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      selected = "home",
      menuItem("Início", tabName = "home", icon = icon("home")),
      menuItem("Importar", tabName = "import", icon = icon("upload")),
      menuItem("Série Temporal", tabName = "ts", icon = icon("line-chart")),
      menuItem("Testes Estatísticos", tabName = "tests", icon = icon("flask")),
      menuItem("Suavização Exponencial", tabName = "smooth", icon = icon("wave-square")),
      menuItem(
        "ARIMA / SARIMA", icon = icon("gears"),
        menuSubItem("Auto ARIMA", tabName = "autoarima", icon = icon("robot")),
        menuSubItem("FAC & FACP", tabName = "acf_pacf", icon = icon("chart-line")),
        menuSubItem("Ajuste Manual", tabName = "arima_manual", icon = icon("sliders")),
        menuSubItem("ARIMA/SARIMA Incompleto", tabName = "inc_arima", icon = icon("filter")),
        menuSubItem("Outliers", tabName = "arima_outliers", icon = icon("bullseye"))
      )
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box-body .row { margin-left: 0 !important; margin-right: 0 !important; }
        .box-body [class*='col-'] { padding-left: 8px !important; padding-right: 8px !important; }
      "))
    ),

  tabItems(
    tabItem(
      tabName = "home",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "SeriesPredictor 📈",
            tags$p("SeriesPredictor é um pacote em R voltado à análise, modelagem e previsão de séries temporais.")
          )
        )
      ),

    # 1) IMPORTAR -------------------------------------------------------
    tabItem(
      tabName = "import",
        fluidRow(
          box(
            title = "Importar Excel (.xls/.xlsx)", status = "info", solidHeader = TRUE, width = 4,
            fileInput("file", "Selecione o arquivo", accept = c(".xlsx", ".xls")),
            uiOutput("ui_sheet"),
            actionButton("btn_load", "Carregar", icon = icon("play"))
          ),
          box(
            title = "Prévia", status = "info", solidHeader = TRUE, width = 8,
            DTOutput("tbl_data")
          )
        )
      ),

    # 2) SÉRIE TEMPORAL -------------------------------------------------
    tabItem(
      tabName = "ts",
        fluidRow(
          box(
            title = "Configurar série", status = "info", solidHeader = TRUE, width = 6,
            fluidRow(column(12, uiOutput("ui_ts_value_col"))),
            hr(),
            fluidRow(
              column(4, numericInput("ts_start_year", "Ano inicial", value = 2020, min = 0)),
              column(4, numericInput("ts_freq", "Frequência", value = 12, min = 1)),
              column(4, numericInput("ts_start_period", "Período inicial \n
                                     (Dias, mes, trimestre", value = 1, min = 1))
            ),
            br(),
            actionButton("btn_build_ts", "Construir ts", icon = icon("cogs"))
          ),
          box(
            title = NULL, status = "info", solidHeader = FALSE, width = 6,
            textInput("plot_title", "Título do gráfico", value = "Série Temporal"),
            fluidRow(
              column(6, textInput("plot_xlab", "Eixo X (rótulo)", value = "Tempo")),
              column(6, textInput("plot_ylab", "Eixo Y (rótulo)", value = "Valor"))
            )
          )
        ),

        fluidRow(
          box(
            title = "Visualização",status = "info", solidHeader = TRUE, width = 12,
            plotOutput("plot_ts", height = 340),
            verbatimTextOutput("ts_info"),
            column(6, downloadButton("btn_download_plot", "PNG", icon = icon("download"), width = "140px")),
            column(6, downloadButton("btn_download_plot_pdf", "PDF", icon = icon("download"), width = "140px")
          )
        ),

        fluidRow(
          box(
            title = "Configurações de Exportação (PNG/PDF)", status = "info", solidHeader = TRUE, width = 12,
            fluidRow(
              column(4, numericInput("png_w", "Largura (px)", value = 1600, min = 400, step = 100)),
              column(4, numericInput("png_h", "Altura (px)", value = 900,  min = 300, step = 100)),
              column(4, numericInput("png_dpi", "DPI", value = 150, min = 72, max = 600, step = 10))
            )
          )
          )
        )
      ),

    # 3) TESTES ---------------------------------------------------------
    tabItem(
      tabName = "tests",
        tabBox(
          width = 12,
          tabPanel(
            "Variância (Box-Cox)",
            fluidRow(
              box(
                width = 12, status = "info", solidHeader = TRUE,
                tags$p("Box-Cox será aplicado aos testes quando o IC 95% NÃO contiver 1. Se contiver 1, não aplicamos (equivale a “sem transformação”)."),
                fluidRow(
                  column(4, numericInput("bc_lmin", "Lambda mínimo", value = -2, step = 0.05)),
                  column(4, numericInput("bc_lmax", "Lambda máximo", value = 2,  step = 0.05)),
                  column(4, numericInput("bc_step", "Passo", value = 0.01, step = 0.01))
                ),
                fluidRow(
                  column(6, numericInput("bc_order", "Ordem AR (order.max)", value = 2, min = 0, step = 1)),
                  column(6, selectInput("bc_method", "Método (ar)", choices = c("yw","burg","ols","yule-walker","mle"), selected = "yw"))
                ),
                br(),
                actionButton("btn_boxcox_calc", "Calcular Box-Cox (BoxCox.ar2)", icon = icon("sliders")),
                br(), br(),
                verbatimTextOutput("txt_boxcox_ar2"),
                plotOutput("plot_boxcox_ar2", height = 260)
              )
            )
          ),

          tabPanel(
            "Tendência",
            fluidRow(
              box(
                title = "Tendência (decompose + Cox-Stuart)", status = "info", solidHeader = TRUE, width = 12,
                tags$p("Se o Box-Cox estiver “aplicável”, estes testes usam a série transformada automaticamente."),
                plotOutput("plot_trend", height = 280),
                verbatimTextOutput("txt_trend_test")
              )
            )
          ),

          tabPanel(
            "Sazonalidade / Periodicidade (Periodograma + Fisher)",
            fluidRow(
              box(
                title = "Sazonalidade / Periodicidade", status = "info", solidHeader = TRUE, width = 12,
                plotOutput("plot_periodograma", height = 280),
                verbatimTextOutput("txt_fisher"),
                # Inputs do periodograma (largura/altura/dpi) + botões de download
              )
            ),



            # Exportação (padrão)
            fluidRow(
              box(
                title = "Configurações de Exportação (PNG/PDF)", width = 12, status = "info", solidHeader = TRUE,
              column(3, numericInput("per_w",   "Largura (px)", value = 1600, min = 400, step = 100)),
              column(3, numericInput("per_h",   "Altura (px)",  value = 900,  min = 300, step = 100)),
              column(3, numericInput("per_dpi", "DPI",          value = 150,  min = 72,  max = 600, step = 10)),
              column(
                3,
                tags$div(
                  style = "text-align:right; padding-top: 25px;",
                  downloadButton("btn_download_period_png", "Salvar periodograma (PNG)", icon = icon("download")),
                  downloadButton("btn_download_period_pdf", "Salvar periodograma (PDF)", icon = icon("download"))
                )
              )
              )
            )
          )
        )
      ),

      # 4) Suavização -------------------
      tabItem(
        tabName = "smooth",

        fluidRow(
          box(
            width = 12, title = "Configurar modelo", status = "info", solidHeader = TRUE,

            selectInput(
              "smooth_method", "Método:",
              choices = c(
                "ETS (automático)" = "ETS",
                "SES (simples)" = "SES",
                "Holt (tendência)" = "HOLT",
                "Holt-Winters (sazonal)" = "HW"
              ),
              selected = "ETS"
            ),

            conditionalPanel(
              condition = "input.smooth_method == 'ETS'",
              textInput("ets_model", "Modelo ETS (ex.: ZZZ, AAA, MAM...)", value = "ZZZ")
            ),

            conditionalPanel(
              condition = "input.smooth_method == 'HW'",
              selectInput(
                "smooth_seasonal", "Sazonalidade (HW):",
                choices = c("Aditiva" = "additive", "Multiplicativa" = "multiplicative"),
                selected = "additive"
              )
            ),

            conditionalPanel(
              condition = "input.smooth_method == 'HOLT' || input.smooth_method == 'HW'",
              checkboxInput("smooth_damped", "Tendência amortecida (damped)", value = FALSE)
            ),

            numericInput("smooth_h", "Horizonte de previsão (h):", value = 10, min = 1),

            hr(),
            br(),
            actionButton("btnFitSmooth", "Ajustar modelo", icon = icon("play"))
          )
        ),

        fluidRow(
          box(
            title = "Métricas do modelo", width = 12, status = "info", solidHeader = TRUE,
            downloadButton("download_smooth_resumo_xlsx", "Baixar resumo (xlsx)"),
            DTOutput("smoothReportTable")
          )
        ),

        fluidRow(
          box(
            title = "Parâmetros do modelo", width = 12, status = "info", solidHeader = TRUE,
            downloadButton("download_smooth_params_xlsx", "Baixar parâmetros (xlsx)"),
            DTOutput("smoothParamsTable")
          )
        ),

        # Exportação (padrão)
        fluidRow(
          box(
            title = "Configurações de Exportação (PNG/PDF)", width = 12, status = "info", solidHeader = TRUE,
            fluidRow(
              column(4, numericInput("smooth_px_w",  "Largura (px)", value = 1600, min = 400, step = 100)),
              column(4, numericInput("smooth_px_h",  "Altura (px)",  value = 900,  min = 300, step = 100)),
              column(4, numericInput("smooth_dpi",   "DPI",          value = 150,  min = 72, max = 600, step = 10))
            )
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Previsão (IC 95%)", status = "info", solidHeader = TRUE,
            plotOutput("smoothForecastPlot", height = 360),
            downloadButton("download_smooth_forecast_png", "Baixar PNG"),
            downloadButton("download_smooth_forecast_pdf", "Baixar PDF"),
            downloadButton("download_smooth_table_xlsx", "Baixar tabela"),
            DTOutput("smoothForecastTable")
          )
        ),

        fluidRow(
          box(
            width = 12, title = "Diagnóstico dos resíduos", status = "info", solidHeader = TRUE,
            plotOutput("smoothResidualPlot", height = 320),
            downloadButton("download_smooth_resid_png", "Baixar PNG"),
            downloadButton("download_smooth_resid_pdf", "Baixar PDF"),
            plotOutput("smoothBoxPiercePlot", height = 320),
            downloadButton("download_boxpierce_png", "Baixar PNG"),
            downloadButton("download_boxpierce_pdf", "Baixar PDF"),
            verbatimTextOutput("smoothLjungBox")
          )
        )
      ),

      # 5) AUTO-ARIMA ----------
      tabItem(
        tabName = "autoarima",
        fluidRow(
          box(
            width = 12, title = "Auto ARIMA / SARIMA", status = "info", solidHeader = TRUE,
            numericInput("arima_h", "Horizonte de previsão (h):", value = 10, min = 1),
            checkboxInput("arima_seasonal", "Considerar sazonalidade (seasonal=TRUE)", value = TRUE),
            checkboxInput("arima_stepwise", "Stepwise", value = TRUE),
            checkboxInput("arima_approx", "Approximation", value = FALSE),

            hr(),

            br(),
            actionButton("btnFitArima", "Ajustar auto.arima", icon = icon("play")),
            hr(),
            verbatimTextOutput("autoArimaOutput")
          )
        ),

        fluidRow(
          box(
            width = 12, title = "Métricas do modelo", status = "info", solidHeader = TRUE,
            downloadButton("download_autoarima_resumo_xlsx", "Baixar resumo (xlsx)"),
            DTOutput("autoArimaReportTable")
          )
        ),

        fluidRow(
          box(
            width = 12, title = "Parâmetros do modelo", status = "info", solidHeader = TRUE,
            downloadButton("download_autoarima_coef_xlsx", "Baixar Parâmetros (xlsx)"),
            DTOutput("autoArimaCoefTable")
          )
        ),

        # Exportação (padrão)
        fluidRow(
          box(
            title = "Configurações de Exportação (PNG/PDF)", width = 12, status = "info", solidHeader = TRUE,
            fluidRow(
              column(4, numericInput("arima_px_w", "Largura (px)", value = 1600, min = 400, step = 100)),
              column(4, numericInput("arima_px_h", "Altura (px)",  value = 900,  min = 300, step = 100)),
              column(4, numericInput("arima_dpi",  "DPI",          value = 150,  min = 72, max = 600, step = 10))
            )
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Previsão (IC 95%)", status = "info", solidHeader = TRUE,
            plotOutput("autoArimaForecastPlot", height = 360),
            downloadButton("download_autoarima_forecast_png", "Baixar PNG"),
            downloadButton("download_autoarima_forecast_pdf", "Baixar PDF"),
            downloadButton("download_autoarima_table_xlsx", "Baixar tabela"),
            DTOutput("autoArimaForecastTable")
          )
        ),

        fluidRow(
          box(
            width = 12, title = "Diagnóstico dos resíduos", status = "info", solidHeader = TRUE,
            plotOutput("autoArimaResidualPlot", height = 320),
            downloadButton("download_autoarima_resid_png", "Baixar PNG"),
            downloadButton("download_autoarima_resid_pdf", "Baixar PDF"),
            plotOutput("autoArimaBoxPiercePlot", height = 320),
            downloadButton("download_autoarima_boxpierce_png", "Baixar PNG"),
            downloadButton("download_autoarima_boxpierce_pdf", "Baixar PDF"),
            verbatimTextOutput("autoArimaLjungBox")
          )
        )
      ),

      # 6) ACF e PACF -----------
      tabItem(
        tabName = "acf_pacf",
        fluidRow(
          box(
            width = 12, title = "Diferenças de Tendência e Sazonalidade", status = "info", solidHeader = TRUE,
            verbatimTextOutput("diffSuggestedOnly"),
            actionButton("btnUseSuggestedDiffs", "Usar sugestões nos campos", icon = icon("magic"))
          )
        ),
        fluidRow(
          box(
            width = 4, title = "Definir d, D e S", status = "info", solidHeader = TRUE,
            numericInput("acf_d", "d (diferenças não sazonais):", value = 0, min = 0),
            numericInput("acf_D", "D (diferenças sazonais):", value = 0, min = 0),
            numericInput("acf_S", "S (período sazonal):", value = 12, min = 1),
            helpText("Ex.: mensal → S=12, trimestral → S=4. Se usar o detectado, clique em “Usar sugestões”."),
            hr(),
          ),
          box(
            width = 4, title = "FAC (após diferenças)", status = "info", solidHeader = TRUE,
            plotOutput("acfPlot_manual", height = 320),
            downloadButton("download_acf_png", "Baixar PNG"),
            downloadButton("download_acf_pdf", "Baixar PDF")
          ),
          box(
            width = 4, title = "FACP (após diferenças)", status = "info", solidHeader = TRUE,
            plotOutput("pacfPlot_manual", height = 320),
            downloadButton("download_pacf_png", "Baixar PNG"),
            downloadButton("download_pacf_pdf", "Baixar PDF")
          )
        ),
        # Exportação (padrão)
        fluidRow(
          box(
            title = "Configurações de Exportação (PNG/PDF)", width = 12, status = "info", solidHeader = TRUE,
            fluidRow(
              column(4, numericInput("acf_px_w", "Largura (px)", value = 1600, min = 400, step = 100)),
              column(4, numericInput("acf_px_h", "Altura (px)",  value = 900,  min = 300, step = 100)),
              column(4, numericInput("acf_dpi",  "DPI",          value = 150,  min = 72,  max = 600, step = 10))
            )
          )
        )
      ),

      # 7) Ajuste manual ----
      tabItem(
        tabName = "arima_manual",
        fluidRow(
          box(
            width = 12, title = "Ajuste Manual (ARIMA/SARIMA)", status = "info", solidHeader = TRUE,
            helpText("Defina as ordens a partir da leitura da FAC e FACP."),
            actionButton("btnCopyDDS", "Copiar d, D e S da aba FAC/FACP", icon = icon("copy")),
            hr(),
            fluidRow(
              column(2, numericInput("man_p", "p (AR):", value = 0, min = 0)),
              column(2, numericInput("man_d", "d (Dif. não saz.):", value = 0, min = 0)),
              column(2, numericInput("man_q", "q (MA):", value = 0, min = 0)),
              column(2, numericInput("man_P", "P (AR saz.):", value = 0, min = 0)),
              column(2, numericInput("man_D", "D (Dif. saz.):", value = 0, min = 0)),
              column(2, numericInput("man_Q", "Q (MA saz.):", value = 0, min = 0))
            ),
            fluidRow(
              column(3, numericInput("man_S", "S (Período sazonal):", value = 12, min = 1)),
              column(3, checkboxInput("man_const", "Incluir constante", value = TRUE)),
              column(3, numericInput("man_h", "Horizonte de previsão (h):", value = 10, min = 1)),
              column(3, actionButton("btnFitManualArima", "Ajustar modelo", icon = icon("play")))
            ),
            hr(),
            verbatimTextOutput("manualArimaOutput")
          )
        ),

        fluidRow(
          box(
            title = "", width = 12, status = "info", solidHeader = TRUE,
            downloadButton("download_manualarima_resumo_xlsx", "Baixar resumo (xlsx)"),
            DT::DTOutput("manualArimaReportTable")
          )
        ),
        fluidRow(
          box(
            title = "Coeficientes do modelo", width = 12, status = "info", solidHeader = TRUE,
            downloadButton("download_manualarima_coef_xlsx", "Baixar coeficientes (xlsx)"),
            downloadButton("download_manualarima_coef_csv",  "Baixar coeficientes (csv)"),
            DT::DTOutput("manualArimaCoefTable")
          )
        ),

        # Exportação (padrão)
        fluidRow(
          box(
            title = "Configurações de Exportação (PNG/PDF)", width = 12, status = "info", solidHeader = TRUE,
            fluidRow(
              column(4, numericInput("man_px_w", "Largura (px)", value = 1600, min = 600)),
              column(4, numericInput("man_px_h", "Altura (px)",  value = 900,  min = 400)),
              column(4, numericInput("man_dpi",  "DPI",          value = 150,  min = 72))
            )
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Previsão (IC 95%)", status = "info", solidHeader = TRUE,
            plotOutput("manualArimaForecastPlot", height = 360),
            downloadButton("download_manualarima_forecast_png", "Baixar PNG"),
            downloadButton("download_manualarima_forecast_pdf", "Baixar PDF"),
            downloadButton("download_manualarima_table_xlsx", "Baixar tabela"),
            DT::DTOutput("manualArimaForecastTable")
          )
        ),
        fluidRow(
          box(
            width = 12, title = "Diagnóstico dos resíduos", status = "info", solidHeader = TRUE,
            plotOutput("manualArimaResidualPlot", height = 320),
            downloadButton("download_manualarima_resid_png", "Baixar PNG"),
            downloadButton("download_manualarima_resid_pdf", "Baixar PDF"),
            plotOutput("manualArimaBoxPiercePlot", height = 320),
            downloadButton("download_manualarima_boxpierce_png", "Baixar PNG"),
            downloadButton("download_manualarima_boxpierce_pdf", "Baixar PDF"),
            verbatimTextOutput("manualArimaLjungBox")
          )
        )
      ),


      # 8) ARIMA/SARIMA INCOMPLETO---------
      tabItem(
        tabName = "inc_arima",

        # 1) Parâmetros do modelo
        fluidRow(
          box(
            width = 12,
            title  = "Parâmetros do Modelo (ARIMA/SARIMA Incompleto)",
            status = "info", solidHeader = TRUE,

            # Linha 1: ordens não sazonais
            fluidRow(
              column(2, numericInput("inc_p", "p", 0, min = 0)),
              column(2, numericInput("inc_d", "d", 0, min = 0)),
              column(2, numericInput("inc_q", "q", 0, min = 0)),
              column(
                6,
                tags$div(style = "padding-top: 25px;",
                         actionButton("btnCopyDDS_inc", "Copiar d/D/S da aba FAC/FACP", icon = icon("copy")))
              )
            ),

            # Linha 2: ordens sazonais + período
            fluidRow(
              column(2, numericInput("inc_P", "P", 0, min = 0)),
              column(2, numericInput("inc_D", "D", 0, min = 0)),
              column(2, numericInput("inc_Q", "Q", 0, min = 0)),
              column(3, numericInput("inc_S", "Período sazonal (S)", 12, min = 1)),
              column(3, checkboxInput("inc_const", "Constante", value = FALSE))
            ),

            tags$hr(),

            # Linha 3: exclusões
            fluidRow(
              column(3, textInput("inc_excl_ar",  "Excluir AR (ex.: 1,3)", "")),
              column(3, textInput("inc_excl_ma",  "Excluir MA (ex.: 1,2)", "")),
              column(3, textInput("inc_excl_sar", "Excluir SAR (ex.: 1)", "")),
              column(3, textInput("inc_excl_sma", "Excluir SMA (ex.: 1)", ""))
            ),

            tags$hr(),

            # Linha 4: horizonte + botão ajustar
            fluidRow(
              column(3, numericInput("inc_h", "Horizonte de previsão (h)", 10, min = 1)),
              column(
                9,
                tags$div(style = "padding-top: 25px;",
                         actionButton("btnFitIncompleteArima", "Ajustar modelo incompleto",
                                      icon = icon("play")))
              )
            ),

            tags$hr(),
            verbatimTextOutput("incompleteArimaOutput")
          )
        ),
        # 2) Resumo + Coeficientes
        fluidRow(
          box(
            width = 12, title = "Resumo do Modelo",
            status = "info", solidHeader = TRUE,
            downloadButton("download_incarima_resumo_xlsx", "Baixar resumo (xlsx)"),
            DT::DTOutput("incompleteArimaReportTable")
          )
        ),

        fluidRow(
          box(
            width = 12, title = "Coeficientes do Modelo",
            status = "info", solidHeader = TRUE,
            downloadButton("download_incarima_coef_xlsx", "Baixar coeficientes (xlsx)"),
            downloadButton("download_incarima_coef_csv",  "Baixar coeficientes (csv)"),
            DT::DTOutput("incompleteArimaCoefTable")
          )
        ),

        # Exportação (padrão)
        fluidRow(
          box(
            title = "Configurações de Exportação (PNG/PDF)", width = 12, status = "info", solidHeader = TRUE,
            fluidRow(
              column(4, numericInput("inc_px_w", "Largura (px)", value = 1600, min = 600)),
              column(4, numericInput("inc_px_h", "Altura (px)",  value = 900,  min = 400)),
              column(4, numericInput("inc_dpi",  "DPI",          value = 150,  min = 72))
            )
          )
        ),
        # 3) Previsão
        fluidRow(
          box(
            width = 12, title = "Previsão (IC 95%)",
            status = "info", solidHeader = TRUE,
            plotOutput("incompleteArimaForecastPlot", height = 360),
            tags$div(style = "margin-top:10px;",
                     downloadButton("download_incarima_forecast_png", "Baixar PNG"),
                     downloadButton("download_incarima_forecast_pdf", "Baixar PDF"),
                     downloadButton("download_incarima_table_xlsx", "Baixar tabela (xlsx)")
            ),
            DT::DTOutput("incompleteArimaForecastTable")
          )
        ),

        # 4) Diagnóstico
        fluidRow(
          box(
            width = 12, title = "Diagnóstico dos Resíduos",
            status = "info", solidHeader = TRUE,

            plotOutput("incompleteArimaResidualPlot", height = 320),
            tags$div(style = "margin-top:10px;",
                     downloadButton("download_incarima_resid_png", "Baixar PNG"),
                     downloadButton("download_incarima_resid_pdf", "Baixar PDF")
            ),

            tags$hr(),

            plotOutput("incompleteArimaBoxPiercePlot", height = 320),
            tags$div(style = "margin-top:10px;",
                     downloadButton("download_incarima_boxpierce_png", "Baixar Box–Pierce PNG"),
                     downloadButton("download_incarima_boxpierce_pdf", "Baixar Box–Pierce PDF")
            ),

            tags$hr(),
            verbatimTextOutput("incompleteArimaLjungBox")
          )
        )
      ),


    # 9) Modelo com Outliers ----
    tabItem(
      tabName = "arima_outliers",
      # BOX 1) MODELO BASE
      fluidRow(
        box(
          width = 12,
          title = "1) Modelo Base",
          status = "info",
          solidHeader = TRUE,

          helpText("Passo 1: Carregar o modelo base vindo do Ajuste Manual ou do Modelo Incompleto."),

          fluidRow(
            column(
              6,
              radioButtons(
                "out_base_source",
                "Modelo base (fonte):",
                choices  = c("Ajuste Manual" = "manual", "Modelo Incompleto" = "incomplete"),
                selected = "manual",
                inline   = TRUE
              )
            ),
            column(
              6,
              tags$div(
                style = "padding-top: 25px;",
                actionButton(
                  "btnLoadBase_out",
                  "1) Carregar modelo base",
                  icon  = icon("download")
                )
              )
            )
          ),

          hr(),

          h4("Resultados do Modelo Base (Sem Outliers)"),
          tabBox(
            id = "tab_out_base",
            width = 12,
            title = "Análise Inicial",
            tabPanel(
              "Coeficientes Base",
              DT::DTOutput("out_base_coef_table"),
              br(),
              downloadButton("download_out_base_coef_xlsx", "Baixar Coeficientes Base (XLSX)")
            ),
            tabPanel(
              "Métricas Base",
              DT::DTOutput("out_base_metrics_table"),
              br(),
              downloadButton("download_out_base_metrics_xlsx", "Baixar Métricas Base (XLSX)")
            )
          )
        )
      ),
      # BOX 2) DETECTAR OUTLIERS
      fluidRow(
        box(
          width = 12,
          title = "2) Detectar Outliers",
          status = "info",
          solidHeader = TRUE,

          helpText("Passo 2: Detectar outliers a partir dos resíduos do modelo base."),

          fluidRow(
            column(4, numericInput("out_cval", "cval (limiar |t|):", value = 3.5, min = 1.0, step = 0.1)),
            column(4, sliderInput("out_delta", "delta (TC):", min = 0.1, max = 0.95, value = 0.7, step = 0.05)),
            column(
              4,
              checkboxGroupInput(
                "out_types",
                "Tipos:",
                choices  = c("AO", "LS", "TC", "IO"),
                selected = c("AO", "LS", "TC", "IO"),
                inline   = TRUE
              )
            )
          ),

          fluidRow(
            column(
              12,
              tags$div(
                style = "margin-top:10px;",
                actionButton(
                  "btnDetect_out",
                  "2) Detectar outliers",
                  icon  = icon("search")
                )
              )
            )
          ),

          hr(),

          h4("Lista de Outliers Detectados"),
          DT::DTOutput("out_table"),
          br(),

          fluidRow(
            column(6, actionButton("btnResetOut_out", "Resetar lista", icon = icon("rotate-left"))),
            column(6, actionButton("btnRemoveSel_out", "Remover selecionados", icon = icon("trash")))
          )
        )
      ),

      #BOX 3) AJUSTE FINAL (XREG)
      fluidRow(
        box(
          width = 12,
          title = "3) Ajuste do Modelo Final (com xreg)",
          status = "info",
          solidHeader = TRUE,

          helpText("Passo 3: Ajustar o modelo final incorporando a matriz xreg construída a partir dos outliers."),

          fluidRow(
            column(3, numericInput("out_h", "Horizonte de previsão (h):", value = 12, min = 1)),
            column(
              9,
              tags$div(
                style = "padding-top: 25px;",
                actionButton(
                  "btnFitWithOut_out",
                  "3) Ajustar Modelo Final (xreg)",
                  icon  = icon("gears")
                )
              )
            )
          ),

          hr(),

          h4("Resultados do Modelo Final (Modelo + Outliers)"),
          tabBox(
            id = "tab_out_final",
            width = 12,
            tabPanel(
              "Métricas e Resumo",
              DT::DTOutput("out_final_report_table"),
              br(),
              downloadButton("download_out_final_resumo_xlsx", "Baixar Resumo (XLSX)")
            ),
            tabPanel(
              "Coeficientes Estimados",
              checkboxInput("out_compact_coef", "Compactar coeficientes (remover ~0)", value = FALSE),
              DT::DTOutput("out_final_coef_table"),
              br(),
              downloadButton("download_out_final_coef_xlsx", "Baixar Coeficientes (XLSX)")
            )
          )
        )
      ),

      # CONTROLES DE EXPORTAÇÃO (LARGURA/ALTURA/DPI)
      fluidRow(
        box(
          width = 12,
          title = "Configurações de Exportação (PNG/PDF)",
          status = "info",
          solidHeader = TRUE,

          fluidRow(
            column(4, numericInput("out_px_w", "Largura (px)", value = 1600, min = 600, step = 50)),
            column(4, numericInput("out_px_h", "Altura (px)",  value = 900,  min = 400, step = 50)),
            column(4, numericInput("out_dpi",  "DPI",          value = 150,  min = 72,  step = 10))
          ),
          helpText("Usado nos downloads de PNG. Para PDF, largura/altura serão convertidas a partir de px/dpi.")
        )
      ),

        # 2) PREVISÃO
        fluidRow(
          box(
            width = 12,
            title = "Previsão do Modelo com Outliers (IC 95%)",
            status = "info",
            solidHeader = TRUE,

            plotOutput("out_forecast_plot", height = 400),
            br(),

            fluidRow(
              column(4, downloadButton("download_out_forecast_png", "Baixar PNG")),
              column(4, downloadButton("download_out_forecast_pdf", "Baixar PDF")),
              column(4, downloadButton("download_out_table_xlsx", "Baixar Tabela (XLSX)"))
            ),

            hr(),
            h4("Tabela de Valores Previstos"),
            DT::DTOutput("out_forecast_table")
          )
        ),

        # 3) DIAGNÓSTICO
        fluidRow(
          box(
            width = 12,
            title = "Diagnóstico dos Resíduos (Modelo Final)",
            status = "info",
            solidHeader = TRUE,

            tabBox(
              id = "tab_out_diag",
              width = 12,
              tabPanel(
                "Análise Residual",
                plotOutput("out_resid_plot", height = 400),
                br(),
                downloadButton("download_out_resid_png", "Baixar PNG"),
                downloadButton("download_out_resid_pdf", "Baixar PDF")
              ),
              tabPanel(
                "Teste Box-Pierce / Ljung-Box",
                plotOutput("out_boxpierce_plot", height = 400),
                br(),
                downloadButton("download_out_boxpierce_png", "Baixar PNG"),
                downloadButton("download_out_boxpierce_pdf", "Baixar PDF")
              )
            )
          )
        )


      )

    )
  )
)

# SERVER --------------------------------------------------------------

server <- function(input, output, session) {

  # Importar dados --------

  rv <- reactiveValues(data = NULL, ts = NULL)

  output$ui_sheet <- renderUI({
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    validate(need(ext %in% c("xlsx", "xls"), "Envie apenas .xls ou .xlsx"))
    sheets <- readxl::excel_sheets(input$file$datapath)
    selectInput("sheet", "Aba (Excel)", choices = sheets, selected = sheets[1])
  })

  observeEvent(input$btn_load, {
    req(input$file, input$sheet)

    df <- tryCatch(
      readxl::read_excel(input$file$datapath, sheet = input$sheet) |> as.data.frame(),
      error = function(e) {
        showNotification(paste("Erro ao carregar:", e$message), type = "error", duration = 8)
        NULL
      }
    )

    rv$data <- df
    rv$ts <- NULL
  })

  output$tbl_data <- renderDT({
    req(rv$data)
    DT::datatable(rv$data, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$ui_ts_value_col <- renderUI({
    req(rv$data)
    cols <- names(rv$data)
    selectInput("ts_value_col", "Coluna de valores (numérica)", choices = cols, selected = cols[1])
  })

  observeEvent(input$ts_value_col, {
    req(input$ts_value_col)
    updateTextInput(session, "plot_ylab", value = input$ts_value_col)
  }, ignoreInit = TRUE)

  observeEvent(input$btn_build_ts, {
    req(rv$data, input$ts_value_col)

    y <- suppressWarnings(as.numeric(rv$data[[input$ts_value_col]]))
    if (all(is.na(y))) {
      showNotification("A coluna selecionada não parece numérica.", type = "error", duration = 8)
      return()
    }

    freq  <- as.integer(input$ts_freq)
    start <- c(as.integer(input$ts_start_year), as.integer(input$ts_start_period))

    rv$ts <- ts(y, frequency = freq, start = start)
  })

  y_ts <- reactive({ req(rv$ts); rv$ts })
  y_num <- reactive({ req(y_ts()); as.numeric(y_ts()) })

  # ---------- Plot principal ----------

  output$plot_ts <- renderPlot({
    req(y_ts())
    plot(y_ts(), main = input$plot_title, xlab = input$plot_xlab, ylab = input$plot_ylab)
  })

  output$ts_info <- renderPrint({
    req(y_ts())
    cat("Frequência:", frequency(y_ts()), "\n")
    cat("Início:", paste(start(y_ts()), collapse = "/"), "\n")
    cat("Fim:", paste(end(y_ts()), collapse = "/"), "\n\n")
    print(summary(y_ts()))
  })

  output$btn_download_plot <- downloadHandler(
    filename = function() paste0("serie_temporal_", format(Sys.Date(), "%Y-%m-%d"), ".png"),
    content = function(file) {
      req(y_ts())
      save_plot(
        file = file, device = "png",
        px_w = as.integer(input$png_w), px_h = as.integer(input$png_h), dpi = as.integer(input$png_dpi),
        plot_fun = function() plot(y_ts(), main = input$plot_title, xlab = input$plot_xlab, ylab = input$plot_ylab)
      )
    }
  )

  output$btn_download_plot_pdf <- downloadHandler(
    filename = function() paste0("serie_temporal_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"),
    content = function(file) {
      req(y_ts())
      save_plot(
        file = file, device = "pdf", width = 11, height = 6,
        plot_fun = function() plot(y_ts(), main = input$plot_title, xlab = input$plot_xlab, ylab = input$plot_ylab)
      )
    }
  )

  # ================= BOX-COX ===================================

  rv_tests <- reactiveValues(bc = NULL)

  # limpar Box-Cox ao mudar de série/dados
  observeEvent(input$btn_load,     { rv_tests$bc <- NULL }, ignoreInit = TRUE)
  observeEvent(input$btn_build_ts, { rv_tests$bc <- NULL }, ignoreInit = TRUE)
  observeEvent(input$ts_value_col, { rv_tests$bc <- NULL }, ignoreInit = TRUE)

  observeEvent(input$btn_boxcox_calc, {
    req(y_num())

    s <- na.omit(y_num())
    validate(need(length(s) >= 8, "Série muito curta para Box-Cox AR."))

    lmin <- input$bc_lmin
    lmax <- input$bc_lmax
    step <- input$bc_step
    validate(need(step > 0, "Passo deve ser > 0."))
    validate(need(lmax > lmin, "Lambda máximo deve ser maior que lambda mínimo."))

    grid <- seq(lmin, lmax, by = step)

    if (any(s <= 0)) {
      rv_tests$bc <- list(error = "BoxCox.ar2 requer valores positivos. Sua série possui valores ≤ 0.")
      return()
    }

    res <- tryCatch(
      BoxCoxar2(s, order = as.integer(input$bc_order), lambda = grid, plotit = FALSE, method = input$bc_method),
      error = function(e) list(error = e$message)
    )
    rv_tests$bc <- res
  }, ignoreInit = TRUE)

  boxcox_decision <- reactive({
    res <- rv_tests$bc
    if (is.null(res)) return(list(ok = FALSE, why = "Calcule o Box-Cox primeiro.", apply = FALSE))
    if (!is.null(res$error)) return(list(ok = FALSE, why = res$error, apply = FALSE))

    ci <- res$ci
    contains_1 <- (ci[1] <= 1 && 1 <= ci[2])

    list(
      ok = TRUE,
      apply = !contains_1,
      contains_1 = contains_1,
      mle = res$mle,
      ci = ci,
      res = res
    )
  })

  # série usada nos testes (transformada se Box-Cox aplicável)
  y_used <- reactive({
    req(y_ts(), y_num())
    y0 <- y_ts()
    s  <- as.numeric(y0)

    # se Box-Cox não calculado, usa original
    if (is.null(rv_tests$bc) || (is.list(rv_tests$bc) && !is.null(rv_tests$bc$error))) return(y0)

    dec <- boxcox_decision()
    if (!dec$ok) return(y0)

    if (isTRUE(dec$apply)) {
      if (any(s <= 0, na.rm = TRUE)) return(y0)
      st <- boxcox_transform(s, dec$mle)
      ts(st, start = start(y0), frequency = frequency(y0))
    } else {
      y0
    }
  })

  output$txt_boxcox_ar2 <- renderPrint({
    res <- rv_tests$bc
    if (is.null(res)) { cat("Calcule o Box-Cox para avaliar variância.\n"); return() }
    if (!is.null(res$error)) { cat("Erro:", res$error, "\n"); return() }

    dec <- boxcox_decision()
    ci <- res$ci

    cat("Box-Cox (BoxCox.ar2)\n")
    cat("lambda (MLE):", round(res$mle, 4), "\n")
    cat("IC (95%): [", round(ci[1], 4), ", ", round(ci[2], 4), "]\n", sep = "")
    cat("IC contém 1?:", if (dec$contains_1) "SIM" else "NÃO", "\n")
    if (dec$contains_1) cat("Decisão no app: NÃO aplicar Box-Cox nos testes (usa série original).\n")
    else cat("Decisão no app: Aplicar Box-Cox nos testes (usa série transformada).\n")
  })

  output$plot_boxcox_ar2 <- renderPlot({
    res <- rv_tests$bc
    if (is.null(res) || !is.null(res$error)) {
      plot.new(); title("Box-Cox: calcule para ver o gráfico"); return()
    }

    xl <- res$lambda
    ll <- res$loglike
    mle <- round(res$mle,2)
    ci <- res$ci
    limit <- max(ll) - 0.5 * qchisq(0.95, 1)

    plot(xl, ll, type = "l", xlab = expression(lambda), ylab = "Log Likelihood",
         main = "Box-Cox: lambda (MLE) e IC 95%")
    abline(v = mle, lty = 2)
    abline(v = ci, lty = 3)
    abline(h = limit, lty = 2)
    text(mle, max(ll), labels = paste0("mle=", round(mle, 2)), pos = 4)
    text(mean(ci), limit, labels = paste0("IC=[", round(ci[1],2), ", ", round(ci[2],2), "]"), pos = 3)
  })

  # ================== TENDÊNCIA =====================================

  dcmp_obj <- reactive({
    req(y_used())
    validate(need(frequency(y_used()) > 1, "A frequência da série deve ser > 1 para usar decompose()."))
    tryCatch(decompose(y_used()), error = function(e) NULL)
  })

  output$plot_trend <- renderPlot({
    req(dcmp_obj())
    tr <- dcmp_obj()$trend
    plot(tr, main = "Componente de Tendência (decompose)", xlab = "Tempo", ylab = "Trend")
  })

  trend_test_text <- reactive({
    req(y_used())
    s <- na.omit(as.numeric(y_used()))
    validate(need(length(s) >= 10, "Série muito curta para o teste de Cox-Stuart (mínimo ~10 pontos)."))

    p <- tryCatch(randtests::cox.stuart.test(s)$p.value, error = function(e) NA_real_)
    p_fmt <- fmt_p(p)

    msg <- sprintf("Teste de Tendência (Cox-Stuart): valor-p = %s", p_fmt)

    if (!is.na(p) && p < 0.05) paste0(msg, "\nConclusão: há evidência de tendência (p < 0.05).")
    else paste0(msg, "\nConclusão: não há evidência de tendência (p ≥ 0.05).")
  })

  output$txt_trend_test <- renderText(trend_test_text())

  has_trend <- reactive({
    req(y_used())
    s <- na.omit(as.numeric(y_used()))
    if (length(s) < 10) return(FALSE)
    p <- tryCatch(randtests::cox.stuart.test(s)$p.value, error = function(e) NA_real_)
    is.finite(p) && p < 0.05
  })

  # ================== SAZONALIDADE ===================================

  # ndiffs agora é automático se houver tendência (sem checkbox)
  y_for_period <- reactive({
    req(y_used())
    y <- y_used()
    s <- as.numeric(y)

    if (has_trend()) {
      d <- forecast::ndiffs(s)
      if (d > 0) diff(y, differences = d) else y
    } else {
      y
    }
  })

  P_obj <- reactive({
    req(y_for_period())
    z <- as.numeric(y_for_period())
    z <- z[is.finite(z)]
    validate(need(length(z) >= 8, "Série muito curta para calcular periodograma."))
    periodograma(z)
  })

  output$plot_periodograma <- renderPlot({
    req(P_obj())
    P <- P_obj()
    plot(P$periodo, P$densidade, type = "l",
         main = "Periodograma", xlab = "Período", ylab = "Densidade espectral")
  })

  fisher_obj <- reactive({
    req(P_obj())
    Fisher.test(P_obj(), alpha = 0.05)
  })

  output$txt_fisher <- renderPrint({
    req(fisher_obj())
    ft <- fisher_obj()

    cat("Teste de Fisher (periodograma)\n")
    cat("g =", ft$g, "\n")
    cat("valor-p =", ft$`valor-p`, "\n")

    if (ft$has_period) {
      cat("Conclusão: há evidência de sazonalidade/periodicidade (p < 0.05).\n")
      cat("Período estimado:", ft$periodo, "\n")
    } else {
      cat("Conclusão: não há evidência de sazonalidade/periodicidade (p ≥ 0.05).\n")
    }
  })

  # Downloads do periodograma: usam per_w, per_h, per_dpi
  output$btn_download_period_png <- downloadHandler(
    filename = function() paste0("periodograma_", format(Sys.Date(), "%Y-%m-%d"), ".png"),
    content = function(file) {
      req(P_obj())
      save_plot(
        file = file, device = "png",
        px_w = as.integer(input$per_w),
        px_h = as.integer(input$per_h),
        dpi  = as.integer(input$per_dpi),
        plot_fun = function() {
          P <- P_obj()
          plot(P$periodo, P$densidade, type = "l",
               main = "Periodograma", xlab = "Período", ylab = "Densidade espectral")
        }
      )
    }
  )

  output$btn_download_period_pdf <- downloadHandler(
    filename = function() paste0("periodograma_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"),
    content = function(file) {
      req(P_obj())
      save_plot(
        file = file, device = "pdf",
        width  = as.numeric(input$per_w) / as.numeric(input$per_dpi),
        height = as.numeric(input$per_h) / as.numeric(input$per_dpi),
        plot_fun = function() {
          P <- P_obj()
          plot(P$periodo, P$densidade, type = "l",
               main = "Periodograma", xlab = "Período", ylab = "Densidade espectral")
        }
      )
    }
  )

  # Suvização Exponencial ----------------------
  # --- storage (zera quando muda a planilha/série) ---
  smoothForecast <- reactiveVal(NULL)
  smoothLambda   <- reactiveVal(NULL)
  smoothBias     <- reactiveVal(FALSE)

  # zera ao importar nova planilha / reconstruir ts / trocar coluna
  observeEvent(input$btn_load,     { smoothForecast(NULL); smoothLambda(NULL); smoothBias(FALSE) }, ignoreInit = TRUE)
  observeEvent(input$btn_build_ts, { smoothForecast(NULL); smoothLambda(NULL); smoothBias(FALSE) }, ignoreInit = TRUE)
  observeEvent(input$ts_value_col, { smoothForecast(NULL); smoothLambda(NULL); smoothBias(FALSE) }, ignoreInit = TRUE)

  # --- helpers no padrão do app (usa Box-Cox já calculado) ---
  get_lambda_models <- function() {
    dec <- boxcox_decision()
    if (isTRUE(dec$ok) && isTRUE(dec$apply)) dec$mle else NULL
  }
  get_biasadj <- function() FALSE

  # --- Ajuste do modelo ---
  observeEvent(input$btnFitSmooth, {
    req(y_used())

    s <- y_used()
    validate(need(length(na.omit(as.numeric(s))) >= 6, "A série precisa ter pelo menos 6 observações para ajustar modelos."))

    lam  <- get_lambda_models()
    bias <- get_biasadj()

    smoothLambda(lam)
    smoothBias(bias)

    h   <- as.integer(input$smooth_h)
    lev <- 95

    fc <- switch(
      input$smooth_method,

      "SES" = forecast::ses(s, h = h, level = lev, lambda = lam, biasadj = bias),

      "HOLT" = forecast::holt(
        s, h = h, level = lev,
        damped = isTRUE(input$smooth_damped),
        lambda = lam, biasadj = bias
      ),

      "HW" = {
        validate(need(frequency(s) > 1, "Holt-Winters requer série sazonal (frequência > 1)."))
        forecast::hw(
          s, h = h, level = lev,
          seasonal = input$smooth_seasonal,
          damped   = isTRUE(input$smooth_damped),
          lambda   = lam, biasadj = bias
        )
      },

      "ETS" = {
        mdl <- forecast::ets(s, model = input$ets_model, lambda = lam, biasadj = bias)
        forecast::forecast(mdl, h = h, level = lev)
      }
    )

    smoothForecast(fc)
  })

  # --- Plot previsão ---
  output$smoothForecastPlot <- renderPlot({
    req(smoothForecast())
    plot(smoothForecast(), main = "Previsão - Suavização Exponencial")
  })

  # --- Resumo (KV) + 2 colunas no padrão ---
  smoothReportKV <- reactive({
    req(smoothForecast())
    fc <- smoothForecast()
    m  <- fc$model

    lam  <- smoothLambda()
    bias <- smoothBias()

    method_str <- tryCatch(m$method, error = function(e) NULL)
    if (is.null(method_str)) method_str <- class(m)[1]

    sigma2 <- tryCatch(m$sigma2, error = function(e) NA_real_)
    sigma  <- tryCatch(m$sigma,  error = function(e) NA_real_)
    if (!is.finite(sigma) && is.finite(sigma2)) sigma <- sqrt(sigma2)

    ll   <- tryCatch(as.numeric(m$loglik), error = function(e) NA_real_)
    aic  <- tryCatch(m$aic,  error = function(e) NA_real_)
    aicc <- tryCatch(m$aicc, error = function(e) NA_real_)
    bic  <- tryCatch(m$bic,  error = function(e) NA_real_)

    df <- data.frame(
      Campo = c("Lambda usado (modelos)", "Biasadj", "Modelo",
                "sigma", "logLik", "AIC", "AICc", "BIC"),
      Valor = c(
        ifelse(is.null(lam), "NULL", as.character(lam)),
        as.character(isTRUE(bias)),
        as.character(method_str),
        fmt_num(sigma), fmt_num(ll), fmt_num(aic), fmt_num(aicc), fmt_num(bic)
      ),
      stringsAsFactors = FALSE
    )

    acc <- tryCatch(as.data.frame(forecast::accuracy(fc)), error = function(e) NULL)
    if (!is.null(acc) && nrow(acc) >= 1) {
      acc1 <- acc[1, , drop = FALSE]
      for (nm in names(acc1)) {
        df <- rbind(
          df,
          data.frame(
            Campo = paste0("Acurácia (treino) - ", nm),
            Valor = fmt_num(as.numeric(acc1[[nm]]), digits = 6),
            stringsAsFactors = FALSE
          )
        )
      }
    }

    df
  })

  smoothReport2Col <- reactive({
    req(smoothReportKV())
    kv_to_2col(smoothReportKV())
  })

  output$smoothReportTable <- renderDT({
    req(smoothReport2Col())
    DT::datatable(
      smoothReport2Col(),
      rownames = FALSE,
      colnames = c("Campo", "Valor", "Campo", "Valor"),
      options = list(pageLength = 25, scrollX = TRUE, dom = "tip", ordering = FALSE)
    )
  })

  output$download_smooth_resumo_xlsx <- downloadHandler(
    filename = function() paste0("suavizacao_resumo_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(smoothReportKV())
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Resumo")
      openxlsx::writeData(wb, "Resumo", smoothReportKV())
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # --- Parâmetros ---
  smoothParamsDF <- reactive({
    req(smoothForecast())
    m <- smoothForecast()$model

    sp   <- tryCatch(m$par,       error = function(e) NULL); if (is.null(sp))   sp   <- numeric(0)
    init <- tryCatch(m$initstate, error = function(e) NULL); if (is.null(init)) init <- numeric(0)

    vals <- c(sp, init)
    if (length(vals) == 0) return(data.frame(Parametro = character(0), Valor = character(0), stringsAsFactors = FALSE))

    nm <- names(vals)
    if (is.null(nm) || any(!nzchar(nm))) {
      nm <- c(
        names(sp)   %||% paste0("par_",   seq_along(sp)),
        names(init) %||% paste0("state_", seq_along(init))
      )
    }
    names(vals) <- nm

    keep <- !duplicated(names(vals))
    vals <- vals[keep]

    data.frame(
      Parametro = names(vals),
      Valor     = vapply(vals, fmt_num, character(1), digits = 6),
      stringsAsFactors = FALSE
    )
  })

  output$smoothParamsTable <- renderDT({
    req(smoothParamsDF())
    DT::datatable(smoothParamsDF(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, dom = "tip"))
  })

  output$download_smooth_params_xlsx <- downloadHandler(
    filename = function() paste0("suavizacao_parametros_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(smoothParamsDF())
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Parametros")
      openxlsx::writeData(wb, "Parametros", smoothParamsDF())
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # --- Tabela previsão ---
  smooth_table_df <- reactive({
    req(smoothForecast())
    fc <- smoothForecast()

    out <- data.frame(
      Passo    = seq_along(fc$mean),
      Previsao = as.numeric(fc$mean),
      stringsAsFactors = FALSE
    )

    if (is.matrix(fc$lower) && is.matrix(fc$upper) && !is.null(colnames(fc$lower))) {
      idx95 <- match("95%", colnames(fc$lower))
      if (is.finite(idx95)) {
        out$LI_95 <- as.numeric(fc$lower[, idx95])
        out$LS_95 <- as.numeric(fc$upper[, idx95])
      }
    } else if (is.matrix(fc$lower) && is.matrix(fc$upper) && ncol(fc$lower) >= 1) {
      out$LI_95 <- as.numeric(fc$lower[, ncol(fc$lower)])
      out$LS_95 <- as.numeric(fc$upper[, ncol(fc$upper)])
    }

    out
  })

  output$smoothForecastTable <- renderDT({
    req(smooth_table_df())
    DT::datatable(smooth_table_df(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$download_smooth_table_xlsx <- downloadHandler(
    filename = function() paste0("suavizacao_tabela_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(smooth_table_df())
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Previsao")
      openxlsx::writeData(wb, "Previsao", smooth_table_df())
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # --- Downloads previsão (usa UMA vez per aba: smooth_px_w/smooth_px_h/smooth_dpi) ---
  output$download_smooth_forecast_png <- downloadHandler(
    filename = function() paste0("suavizacao_previsao_", Sys.Date(), ".png"),
    content  = function(file) {
      req(smoothForecast())
      save_plot(
        file, "png",
        px_w = as.integer(input$smooth_px_w),
        px_h = as.integer(input$smooth_px_h),
        dpi  = as.integer(input$smooth_dpi),
        plot_fun = function() plot(smoothForecast(), main = "Previsão - Suavização Exponencial")
      )
    }
  )

  output$download_smooth_forecast_pdf <- downloadHandler(
    filename = function() paste0("suavizacao_previsao_", Sys.Date(), ".pdf"),
    content  = function(file) {
      req(smoothForecast())
      save_plot(
        file, "pdf",
        width  = as.numeric(input$smooth_px_w) / as.numeric(input$smooth_dpi),
        height = as.numeric(input$smooth_px_h) / as.numeric(input$smooth_dpi),
        plot_fun = function() plot(smoothForecast(), main = "Previsão - Suavização Exponencial")
      )
    }
  )

  # --- Diagnósticos ---
  output$smoothResidualPlot <- renderPlot({
    req(smoothForecast())
    forecast::checkresiduals(smoothForecast()$model)
  })

  output$smoothBoxPiercePlot <- renderPlot({
    req(smoothForecast())
    res <- residuals(smoothForecast()$model)
    res <- res[is.finite(res)]
    validate(need(length(res) >= 10, "Resíduos insuficientes para Box–Pierce com estabilidade."))
    box_pierce_plot(res)
  })

  output$download_smooth_resid_png <- downloadHandler(
    filename = function() paste0("suavizacao_residuos_", Sys.Date(), ".png"),
    content  = function(file) {
      req(smoothForecast())
      save_plot(
        file, "png",
        px_w = as.integer(input$smooth_px_w),
        px_h = as.integer(input$smooth_px_h),
        dpi  = as.integer(input$smooth_dpi),
        plot_fun = function() forecast::checkresiduals(smoothForecast()$model)
      )
    }
  )

  output$download_smooth_resid_pdf <- downloadHandler(
    filename = function() paste0("suavizacao_residuos_", Sys.Date(), ".pdf"),
    content  = function(file) {
      req(smoothForecast())
      save_plot(
        file, "pdf",
        width  = as.numeric(input$smooth_px_w) / as.numeric(input$smooth_dpi),
        height = as.numeric(input$smooth_px_h) / as.numeric(input$smooth_dpi),
        plot_fun = function() forecast::checkresiduals(smoothForecast()$model)
      )
    }
  )

  output$download_boxpierce_png <- downloadHandler(
    filename = function() paste0("box_pierce_", Sys.Date(), ".png"),
    content  = function(file) {
      req(smoothForecast())
      save_plot(
        file, "png",
        px_w = as.integer(input$smooth_px_w),
        px_h = as.integer(input$smooth_px_h),
        dpi  = as.integer(input$smooth_dpi),
        plot_fun = function() {
          res <- residuals(smoothForecast()$model)
          res <- res[is.finite(res)]
          box_pierce_plot(res)
        }
      )
    }
  )

  output$download_boxpierce_pdf <- downloadHandler(
    filename = function() paste0("box_pierce_", Sys.Date(), ".pdf"),
    content  = function(file) {
      req(smoothForecast())
      save_plot(
        file, "pdf",
        width  = as.numeric(input$smooth_px_w) / as.numeric(input$smooth_dpi),
        height = as.numeric(input$smooth_px_h) / as.numeric(input$smooth_dpi),
        plot_fun = function() {
          res <- residuals(smoothForecast()$model)
          res <- res[is.finite(res)]
          box_pierce_plot(res)
        }
      )
    }
  )

  # AUTO-ARIMA ------------

  #Fisher S (frequency)  --- SEM serie_para_fisher() ---
  fisher_S <- reactive({
    req(y_ts(), fisher_obj())

    ft <- fisher_obj()  # <- resultado do Fisher no menu de sazonalidade

    # se detectou periodicidade, usa o período estimado
    S <- if (isTRUE(ft$has_period)) {
      suppressWarnings(as.integer(round(as.numeric(ft$periodo))))
    } else {
      1L
    }

    # saneamento mínimo
    if (!is.finite(S) || S < 2L) S <- 1L

    # >>> IMPORTANTE: para não "matar" o S=6 quando n é pequeno,
    # ao invés de forçar S=1, apenas desliga sazonalidade no auto.arima
    # (o S continua sendo reportado corretamente)
    S
  })

  dados_ts_fisherfreq <- reactive({
    req(y_ts(), fisher_S())

    y  <- y_ts()
    S  <- fisher_S()
    st <- tryCatch(start(y), error = function(e) NULL)

    if (!is.null(st)) ts(as.numeric(y), start = st, frequency = S)
    else ts(as.numeric(y), frequency = S)
  })

  # --- storage (zera quando muda a planilha/série) ---
  autoArimaModel  <- reactiveVal(NULL)
  autoArimaLambda <- reactiveVal(NULL)
  autoArimaBias   <- reactiveVal(FALSE)
  autoArimaS      <- reactiveVal(1L)

  # zera ao importar nova planilha / reconstruir ts / trocar coluna
  observeEvent(input$btn_load,     { autoArimaModel(NULL); autoArimaLambda(NULL); autoArimaBias(FALSE); autoArimaS(1L) }, ignoreInit = TRUE)
  observeEvent(input$btn_build_ts, { autoArimaModel(NULL); autoArimaLambda(NULL); autoArimaBias(FALSE); autoArimaS(1L) }, ignoreInit = TRUE)
  observeEvent(input$ts_value_col, { autoArimaModel(NULL); autoArimaLambda(NULL); autoArimaBias(FALSE); autoArimaS(1L) }, ignoreInit = TRUE)

  # --- helpers no padrão do app (usa Box-Cox já calculado) ---
  # >>> regra: só devolve lambda se Box-Cox foi calculado E for aplicável (IC 95% NÃO contém 1)
  get_lambda_models_arima <- function(x_num) {
    dec <- boxcox_decision()
    if (!isTRUE(dec$ok) || !isTRUE(dec$apply)) return(NULL)

    # Box-Cox só pode se todos os valores forem > 0
    if (any(x_num <= 0, na.rm = TRUE)) return(NULL)

    dec$mle
  }

  get_biasadj_arima <- function(lam) {
    # se você quiser permitir biasadj no futuro, ajuste aqui
    # por enquanto: só faz sentido quando lambda existe
    !is.null(lam) && isTRUE(get_biasadj())
  }

  # --- Ajuste do auto.arima ---
  observeEvent(input$btnFitArima, {
    req(dados_ts_fisherfreq())

    s <- dados_ts_fisherfreq()
    x <- as.numeric(s)
    n <- length(x)
    S <- frequency(s)

    validate(need(length(x) >= 6, "A série precisa ter pelo menos 6 observações."))
    validate(need(all(is.finite(x)), "Série contém NA/Inf."))

    lam  <- get_lambda_models_arima(x)
    bias <- get_biasadj_arima(lam)

    seasonal_ok <- isTRUE(input$arima_seasonal) && (S > 1) && (n >= 2*S)

    fit <- tryCatch(
      forecast::auto.arima(
        s,
        seasonal      = seasonal_ok,
        stepwise      = isTRUE(input$arima_stepwise),
        approximation = isTRUE(input$arima_approx),
        lambda        = lam
      ),
      error = function(e) {
        showNotification(paste0("Erro no auto.arima:\n", e$message),
                         type = "error", duration = 12)
        NULL
      }
    )
    req(fit)

    autoArimaModel(fit)
    autoArimaLambda(lam)
    autoArimaBias(bias)
    autoArimaS(S)
  }, ignoreInit = TRUE)

  # --- Forecast ---
  autoArimaForecast <- reactive({
    req(autoArimaModel())

    fit <- autoArimaModel()
    h   <- as.integer(input$arima_h)
    if (!is.finite(h) || h < 1) h <- 10L

    fc <- tryCatch(
      forecast::forecast(
        object  = fit,
        h       = h,
        level   = c(80, 95),
        biasadj = isTRUE(autoArimaBias())
      ),
      error = function(e) {
        showNotification(paste0("Erro ao prever (auto.arima):\n", e$message),
                         type = "error", duration = 12)
        NULL
      }
    )
    req(fc)
    fc
  })



  output$autoArimaForecastPlot <- renderPlot({
    req(autoArimaForecast())
    plot(autoArimaForecast(), main = paste0("Previsão - auto.arima (S=", autoArimaS(), ")"))
  })

  autoarima_table_df <- reactive({
    req(autoArimaForecast())
    forecast_to_df(autoArimaForecast(), level = 95)
  })

  output$autoArimaForecastTable <- DT::renderDT({
    req(autoarima_table_df())
    DT::datatable(autoarima_table_df(), rownames = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE))
  })

  # --- Downloads (usa arima_px_w/arima_px_h/arima_dpi do UI)
  output$download_autoarima_forecast_png <- downloadHandler(
    filename = function() paste0("autoarima_previsao_", Sys.Date(), ".png"),
    content  = function(file) {
      req(autoArimaForecast())
      save_plot(
        file, "png",
        px_w = as.integer(input$arima_px_w),
        px_h = as.integer(input$arima_px_h),
        dpi  = as.integer(input$arima_dpi),
        plot_fun = function() {
          plot(autoArimaForecast(), main = paste0("Previsão - auto.arima (S=", autoArimaS(), ")"))
        }
      )
    }
  )

  output$download_autoarima_forecast_pdf <- downloadHandler(
    filename = function() paste0("autoarima_previsao_", Sys.Date(), ".pdf"),
    content  = function(file) {
      req(autoArimaForecast())
      save_plot(
        file, "pdf",
        width  = as.numeric(input$arima_px_w) / as.numeric(input$arima_dpi),
        height = as.numeric(input$arima_px_h) / as.numeric(input$arima_dpi),
        plot_fun = function() {
          plot(autoArimaForecast(), main = paste0("Previsão - auto.arima (S=", autoArimaS(), ")"))
        }
      )
    }
  )

  output$download_autoarima_table_xlsx <- downloadHandler(
    filename = function() paste0("tabela_previsao_autoarima_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(autoarima_table_df())
      write_xlsx(file, autoarima_table_df(), sheet = "Previsao")
    }
  )

  # --- Resumo (KV -> 2 colunas)
  autoArimaReportKV <- reactive({
    req(autoArimaModel())
    fit <- autoArimaModel()

    lam  <- autoArimaLambda()
    bias <- autoArimaBias()
    S    <- autoArimaS()

    ll     <- tryCatch(as.numeric(fit$loglik), error = function(e) NA_real_)
    aic    <- tryCatch(fit$aic, error = function(e) NA_real_)
    bic    <- tryCatch(fit$bic, error = function(e) NA_real_)
    aicc   <- tryCatch(forecast::AICc(fit), error = function(e) NA_real_)
    sigma2 <- tryCatch(fit$sigma2, error = function(e) NA_real_)

    ord <- tryCatch(forecast::arimaorder(fit), error = function(e) NULL)
    ord_str <- if (!is.null(ord)) {
      paste0("(", ord[["p"]], ",", ord[["d"]], ",", ord[["q"]], ")",
             if (S > 1) paste0(" x (", ord[["P"]], ",", ord[["D"]], ",", ord[["Q"]], ")[", S, "]") else "")
    } else {
      NA_character_
    }

    df <- data.frame(
      Campo = c("Período sazonal (S)", "ARIMA ordem", "Lambda", "Biasadj",
                "logLik", "AIC", "AICc", "BIC", "sigma2"),
      Valor = c(
        as.character(S),
        ord_str,
        ifelse(is.null(lam), "NULL", as.character(lam)),
        as.character(isTRUE(bias)),
        fmt_num(ll), fmt_num(aic), fmt_num(aicc), fmt_num(bic), fmt_num(sigma2)
      ),
      stringsAsFactors = FALSE
    )

    acc <- tryCatch(as.data.frame(forecast::accuracy(fit)), error = function(e) NULL)
    if (!is.null(acc) && nrow(acc) >= 1) {
      acc1 <- acc[1, , drop = FALSE]
      for (nm in names(acc1)) {
        df <- rbind(
          df,
          data.frame(
            Campo = paste0("Acurácia (treino) - ", nm),
            Valor = fmt_num(acc1[[nm]], digits = 6),
            stringsAsFactors = FALSE
          )
        )
      }
    }
    df
  })

  autoArimaReport2Col <- reactive({
    req(autoArimaReportKV())
    kv_to_2col(autoArimaReportKV())
  })

  output$autoArimaReportTable <- DT::renderDT({
    req(autoArimaReport2Col())
    DT::datatable(
      autoArimaReport2Col(),
      rownames = FALSE,
      colnames = c("Campo", "Valor", "Campo", "Valor"),
      options = list(pageLength = 25, scrollX = TRUE, dom = "tip", ordering = FALSE)
    )
  })

  output$download_autoarima_resumo_xlsx <- downloadHandler(
    filename = function() paste0("autoarima_resumo_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(autoArimaReport2Col())
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Resumo")
      openxlsx::writeData(wb, "Resumo", autoArimaReport2Col())
      openxlsx::setColWidths(wb, "Resumo", cols = 1:4, widths = "auto")
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # --- Coeficientes (DT + download)
  output$autoArimaCoefTable <- DT::renderDT({
    fit <- autoArimaModel()
    req(fit)
    df <- coef_table_df(fit, compact = TRUE)
    DT::datatable(df, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, dom = "tip"))
  })

  output$download_autoarima_coef_xlsx <- downloadHandler(
    filename = function() paste0("autoarima_parametros_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      fit <- autoArimaModel()
      req(fit)
      df <- coef_table_df(fit, compact = TRUE)

      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Parametros")
      openxlsx::writeData(wb, "Parametros", df)
      openxlsx::setColWidths(wb, "Parametros", cols = 1:ncol(df), widths = "auto")
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # --- Diagnóstico
  output$autoArimaResidualPlot <- renderPlot({
    req(autoArimaModel())
    forecast::checkresiduals(autoArimaModel())
  })

  output$autoArimaBoxPiercePlot <- renderPlot({
    req(autoArimaModel())
    res <- residuals(autoArimaModel())
    res <- res[is.finite(res)]
    validate(need(length(res) >= 10, "Resíduos insuficientes para Box–Pierce com estabilidade."))
    box_pierce_plot(res)
  })

  output$download_autoarima_resid_png <- downloadHandler(
    filename = function() paste0("autoarima_residuos_", Sys.Date(), ".png"),
    content  = function(file) {
      req(autoArimaModel())
      save_plot(
        file, "png",
        px_w = as.integer(input$arima_px_w),
        px_h = as.integer(input$arima_px_h),
        dpi  = as.integer(input$arima_dpi),
        plot_fun = function() forecast::checkresiduals(autoArimaModel())
      )
    }
  )

  output$download_autoarima_resid_pdf <- downloadHandler(
    filename = function() paste0("autoarima_residuos_", Sys.Date(), ".pdf"),
    content  = function(file) {
      req(autoArimaModel())
      save_plot(
        file, "pdf",
        width  = as.numeric(input$arima_px_w) / as.numeric(input$arima_dpi),
        height = as.numeric(input$arima_px_h) / as.numeric(input$arima_dpi),
        plot_fun = function() forecast::checkresiduals(autoArimaModel())
      )
    }
  )

  output$download_autoarima_boxpierce_png <- downloadHandler(
    filename = function() paste0("autoarima_boxpierce_", Sys.Date(), ".png"),
    content  = function(file) {
      req(autoArimaModel())
      save_plot(
        file, "png",
        px_w = as.integer(input$arima_px_w),
        px_h = as.integer(input$arima_px_h),
        dpi  = as.integer(input$arima_dpi),
        plot_fun = function() {
          res <- residuals(autoArimaModel())
          res <- res[is.finite(res)]
          box_pierce_plot(res)
        }
      )
    }
  )

  output$download_autoarima_boxpierce_pdf <- downloadHandler(
    filename = function() paste0("autoarima_boxpierce_", Sys.Date(), ".pdf"),
    content  = function(file) {
      req(autoArimaModel())
      save_plot(
        file, "pdf",
        width  = as.numeric(input$arima_px_w) / as.numeric(input$arima_dpi),
        height = as.numeric(input$arima_px_h) / as.numeric(input$arima_dpi),
        plot_fun = function() {
          res <- residuals(autoArimaModel())
          res <- res[is.finite(res)]
          box_pierce_plot(res)
        }
      )
    }
  )

  #---------------- FAC & FACP ----------------

  acf_state <- reactiveValues(suggest = NULL)

  observeEvent(input$btn_load,     { acf_state$suggest <- NULL }, ignoreInit = TRUE)
  observeEvent(input$btn_build_ts, { acf_state$suggest <- NULL }, ignoreInit = TRUE)
  observeEvent(input$ts_value_col, { acf_state$suggest <- NULL }, ignoreInit = TRUE)

  # --- Sugestões automáticas (d, D, S) usando o MESMO Fisher do teste de sazonalidade ---
  suggestedDiffs <- reactive({
    req(y_ts())

    s0 <- y_ts()  # <<< série ORIGINAL (sem Box-Cox)
    x0 <- as.numeric(s0)
    x0 <- x0[is.finite(x0)]
    validate(need(length(x0) >= 6, "Série muito curta para sugerir diferenças."))

    # d sugerido (tendência)
    d_sug <- tryCatch(as.integer(forecast::ndiffs(s0)), error = function(e) 0L)
    if (!is.finite(d_sug) || d_sug < 0) d_sug <- 0L

    # Fisher do periodograma (na série após remover tendência via diff(d) e centralizar)
    x <- x0
    if (d_sug > 0 && length(x) > d_sug) x <- diff(x, differences = d_sug)
    x <- x - mean(x, na.rm = TRUE)
    x <- x[is.finite(x)]

    periodo_det <- NA_real_
    p_fisher    <- NA_real_
    S_det       <- 1L

    if (length(x) >= 8) {
      Pper <- periodograma(x, fr = "default")
      ft   <- Fisher.test(Pper, alpha = 0.05)

      periodo_det <- suppressWarnings(as.numeric(ft$periodo))
      p_fisher    <- suppressWarnings(as.numeric(ft$p_num))

      if (isTRUE(ft$has_period) && is.finite(periodo_det) && periodo_det >= 2) {
        S_cand <- as.integer(round(periodo_det))
        if (is.finite(S_cand) && S_cand >= 2) S_det <- S_cand
      }
    }

    if (!is.finite(S_det) || S_det < 2) S_det <- 1L

    # D sugerido (sazonalidade)
    D_sug <- 0L
    if (S_det >= 2) {
      sS <- ts(as.numeric(s0), start = tryCatch(start(s0), error = function(e) c(1, 1)), frequency = S_det)
      D_sug <- tryCatch(as.integer(forecast::nsdiffs(sS)), error = function(e) 0L)
      if (!is.finite(D_sug) || D_sug < 0) D_sug <- 0L
    }

    out <- list(d = d_sug, D = D_sug, S = S_det, periodo = periodo_det, p_fisher = p_fisher)
    acf_state$suggest <- out
    out
  })

  # --- Texto com as sugestões ---
  output$diffSuggestedOnly <- renderPrint({
    req(y_ts())
    z <- tryCatch(suggestedDiffs(), error = function(e) NULL)
    if (is.null(z)) {
      cat("Erro ao calcular sugestões (d, D, S).\n")
      return(invisible())
    }

    cat("Sugestões automáticas (FAC/FACP):\n")
    cat("d (Tendência) = ", z$d, "\n", sep = "")
    cat("D (Sazonalidade) = ", z$D, "\n", sep = "")
    cat("Período detectado (Fisher) = ", ifelse(is.finite(z$periodo), z$periodo, "NA"), "\n", sep = "")
    # por esta:
    p_f_txt <- ifelse(
      is.finite(z$p_fisher),
      ifelse(z$p_fisher < 1e-4, "<0.0001", sprintf("%.4f", z$p_fisher)),
      "NA"
    )
    cat("valor-p Fisher = ", p_f_txt, "\n", sep = "")
    cat("Período arredondado (S) = ", z$S, "\n\n", sep = "")

    if (is.finite(z$p_fisher) && (z$p_fisher < 0.05) && (z$S >= 2)) {
      cat("Conclusão: há evidência de sazonalidade (p < 0.05).\n")
    } else {
      cat("Conclusão: sem evidência de sazonalidade → S=1 e D=0.\n")
    }
  })

  # --- Aplicar sugestões nos inputs da aba FAC/FACP ---
  observeEvent(input$btnUseSuggestedDiffs, {
    req(suggestedDiffs())
    z <- suggestedDiffs()

    updateNumericInput(session, "acf_d", value = as.integer(z$d))
    updateNumericInput(session, "acf_D", value = as.integer(z$D))
    updateNumericInput(session, "acf_S", value = as.integer(z$S))
  }, ignoreInit = TRUE)

  # --- Série usada para FAC/FACP (com diferenças d e D) ---
  serie_for_acf <- reactive({
    req(y_ts())
    s0 <- y_ts()  # <<< série ORIGINAL (sem Box-Cox)

    d <- suppressWarnings(as.integer(input$acf_d))
    D <- suppressWarnings(as.integer(input$acf_D))
    S <- suppressWarnings(as.integer(input$acf_S))

    if (!is.finite(d) || d < 0) d <- 0L
    if (!is.finite(D) || D < 0) D <- 0L
    if (!is.finite(S) || S < 1) S <- 1L

    if (S <= 1 && D > 0) {
      D <- 0L
      updateNumericInput(session, "acf_D", value = 0)
    }

    sS <- ts(as.numeric(s0),
             start = tryCatch(start(s0), error = function(e) c(1, 1)),
             frequency = S)

    y <- sS
    if (d > 0) y <- diff(y, differences = d)
    if (D > 0 && S >= 2) y <- diff(y, lag = S, differences = D)

    y <- y[is.finite(y)]
    validate(need(length(y) >= 6, "Série ficou curta demais após as diferenças (mín. 6 pontos)."))
    y
  })

  # --- FAC ---
  output$acfPlot_manual <- renderPlot({
    req(serie_for_acf())
    y <- serie_for_acf()

    S <- suppressWarnings(as.integer(input$acf_S))
    if (!is.finite(S) || S < 1) S <- 1L

    lagmax <- max(24, 3 * max(2, S))
    acf(y, lag.max = lagmax,
        main = paste0("FAC | d=", input$acf_d, ", D=", input$acf_D, ", S=", S))

    if (S >= 2) abline(v = seq(S, lagmax, by = S), lty = 3, col = "red")
  })

  # --- FACP ---
  output$pacfPlot_manual <- renderPlot({
    req(serie_for_acf())
    y <- serie_for_acf()

    S <- suppressWarnings(as.integer(input$acf_S))
    if (!is.finite(S) || S < 1) S <- 1L

    lagmax <- max(24, 3 * max(2, S))
    pacf(y, lag.max = lagmax,
         main = paste0("FACP | d=", input$acf_d, ", D=", input$acf_D, ", S=", S))

    if (S >= 2) abline(v = seq(S, lagmax, by = S), lty = 3, col = "red")
  })

  # --- Downloads FAC (usa UMA vez: acf_px_w / acf_px_h / acf_dpi) ---
  output$download_acf_png <- downloadHandler(
    filename = function() paste0("FAC_d", input$acf_d, "_D", input$acf_D, "_S", input$acf_S, "_", Sys.Date(), ".png"),
    content  = function(file) {
      save_plot(
        file, "png",
        px_w = as.integer(input$acf_px_w),
        px_h = as.integer(input$acf_px_h),
        dpi  = as.integer(input$acf_dpi),
        plot_fun = function() {
          y <- serie_for_acf()
          S <- suppressWarnings(as.integer(input$acf_S)); if (!is.finite(S) || S < 1) S <- 1L
          lagmax <- max(24, 3 * max(2, S))
          acf(y, lag.max = lagmax,
              main = paste0("FAC | d=", input$acf_d, ", D=", input$acf_D, ", S=", S))
          if (S >= 2) abline(v = seq(S, lagmax, by = S), lty = 3, col = "red")
        }
      )
    }
  )

  output$download_acf_pdf <- downloadHandler(
    filename = function() paste0("FAC_d", input$acf_d, "_D", input$acf_D, "_S", input$acf_S, "_", Sys.Date(), ".pdf"),
    content  = function(file) {
      save_plot(
        file, "pdf",
        width  = as.numeric(input$acf_px_w) / as.numeric(input$acf_dpi),
        height = as.numeric(input$acf_px_h) / as.numeric(input$acf_dpi),
        plot_fun = function() {
          y <- serie_for_acf()
          S <- suppressWarnings(as.integer(input$acf_S)); if (!is.finite(S) || S < 1) S <- 1L
          lagmax <- max(24, 3 * max(2, S))
          acf(y, lag.max = lagmax,
              main = paste0("FAC | d=", input$acf_d, ", D=", input$acf_D, ", S=", S))
          if (S >= 2) abline(v = seq(S, lagmax, by = S), lty = 3, col = "red")
        }
      )
    }
  )

  # --- Downloads FACP (usa UMA vez: acf_px_w / acf_px_h / acf_dpi) ---
  output$download_pacf_png <- downloadHandler(
    filename = function() paste0("FACP_d", input$acf_d, "_D", input$acf_D, "_S", input$acf_S, "_", Sys.Date(), ".png"),
    content  = function(file) {
      save_plot(
        file, "png",
        px_w = as.integer(input$acf_px_w),
        px_h = as.integer(input$acf_px_h),
        dpi  = as.integer(input$acf_dpi),
        plot_fun = function() {
          y <- serie_for_acf()
          S <- suppressWarnings(as.integer(input$acf_S)); if (!is.finite(S) || S < 1) S <- 1L
          lagmax <- max(24, 3 * max(2, S))
          pacf(y, lag.max = lagmax,
               main = paste0("FACP | d=", input$acf_d, ", D=", input$acf_D, ", S=", S))
          if (S >= 2) abline(v = seq(S, lagmax, by = S), lty = 3, col = "red")
        }
      )
    }
  )

  output$download_pacf_pdf <- downloadHandler(
    filename = function() paste0("FACP_d", input$acf_d, "_D", input$acf_D, "_S", input$acf_S, "_", Sys.Date(), ".pdf"),
    content  = function(file) {
      save_plot(
        file, "pdf",
        width  = as.numeric(input$acf_px_w) / as.numeric(input$acf_dpi),
        height = as.numeric(input$acf_px_h) / as.numeric(input$acf_dpi),
        plot_fun = function() {
          y <- serie_for_acf()
          S <- suppressWarnings(as.integer(input$acf_S)); if (!is.finite(S) || S < 1) S <- 1L
          lagmax <- max(24, 3 * max(2, S))
          pacf(y, lag.max = lagmax,
               main = paste0("FACP | d=", input$acf_d, ", D=", input$acf_D, ", S=", S))
          if (S >= 2) abline(v = seq(S, lagmax, by = S), lty = 3, col = "red")
        }
      )
    }
  )

  ## ---------------- ARIMA/SARIMA MANUAL (PADRÃO DO APP) ----------------

  manualArimaModel  <- reactiveVal(NULL)
  manualArimaLambda <- reactiveVal(NULL)
  manualArimaBias   <- reactiveVal(FALSE)
  manualArimaS      <- reactiveVal(1L)
  manualArimaNote   <- reactiveVal("")

  observeEvent(input$btn_load,     { manualArimaModel(NULL); manualArimaLambda(NULL); manualArimaBias(FALSE); manualArimaS(1L); manualArimaNote("") }, ignoreInit = TRUE)
  observeEvent(input$btn_build_ts, { manualArimaModel(NULL); manualArimaLambda(NULL); manualArimaBias(FALSE); manualArimaS(1L); manualArimaNote("") }, ignoreInit = TRUE)
  observeEvent(input$ts_value_col, { manualArimaModel(NULL); manualArimaLambda(NULL); manualArimaBias(FALSE); manualArimaS(1L); manualArimaNote("") }, ignoreInit = TRUE)

  # --- helpers (iguais ao padrão) ---
  get_lambda_models_manual <- function(x_num) {
    dec <- boxcox_decision()
    if (!isTRUE(dec$ok) || !isTRUE(dec$apply)) return(NULL)
    if (any(x_num <= 0, na.rm = TRUE)) return(NULL)
    dec$mle
  }
  get_biasadj_manual <- function(lam) {
    !is.null(lam) && isTRUE(get_biasadj())
  }

  # --- botão: copiar d/D/S da FAC/FACP ---
  observeEvent(input$btnCopyDDS, {
    updateNumericInput(session, "man_d", value = as.integer(input$acf_d %||% 0))
    updateNumericInput(session, "man_D", value = as.integer(input$acf_D %||% 0))
    updateNumericInput(session, "man_S", value = as.integer(input$acf_S %||% 1))
    showNotification("Copiado da FAC/FACP: d, D e S.", type = "message", duration = 3)
  }, ignoreInit = TRUE)

  # --- Ajuste do ARIMA/SARIMA manual ---
  observeEvent(input$btnFitManualArima, {
    req(y_ts())

    # usa a série ORIGINAL (sem transformação)
    s0 <- y_ts()
    x  <- as.numeric(s0)

    validate(need(all(is.finite(x)), "Série contém NA/Inf. Limpe os dados antes."))
    validate(need(length(x) >= 6, "A série precisa ter pelo menos 6 observações."))

    # ordens
    p <- max(0L, as.integer(input$man_p))
    d <- max(0L, as.integer(input$man_d))
    q <- max(0L, as.integer(input$man_q))

    P <- max(0L, as.integer(input$man_P))
    D <- max(0L, as.integer(input$man_D))
    Q <- max(0L, as.integer(input$man_Q))

    S <- max(1L, as.integer(input$man_S))

    # travas de sazonalidade
    n <- length(x)
    if (!is.finite(S) || S < 1L) S <- 1L
    if (S > floor(n/2)) {
      showNotification("Período S grande para o tamanho da série. Ajustei S=1 e zerei P/D/Q.",
                       type = "warning", duration = 8)
      S <- 1L
      updateNumericInput(session, "man_S", value = 1)
      P <- D <- Q <- 0L
      updateNumericInput(session, "man_P", value = 0)
      updateNumericInput(session, "man_D", value = 0)
      updateNumericInput(session, "man_Q", value = 0)
    }
    if (S <= 1L) {
      if (P > 0 || D > 0 || Q > 0) showNotification("S=1 não permite termos sazonais. Zerei P/D/Q.", type = "warning", duration = 6)
      P <- D <- Q <- 0L
      updateNumericInput(session, "man_P", value = 0)
      updateNumericInput(session, "man_D", value = 0)
      updateNumericInput(session, "man_Q", value = 0)
    }

    # constante: se d+D>0, desliga para evitar instabilidade
    d_total  <- d + D
    use_const <- if (d_total > 0) FALSE else isTRUE(input$man_const)
    if (isTRUE(input$man_const) && d_total > 0) {
      showNotification("d+D>0 ⇒ constante foi DESATIVADA (evita drift/instabilidade).", type = "warning", duration = 6)
    }

    # lambda / bias (Box-Cox já calculado)
    lam  <- get_lambda_models_manual(x)
    bias <- get_biasadj_manual(lam)

    # monta ts com frequência S (mantém start original se existir)
    st <- tryCatch(start(s0), error = function(e) c(1, 1))
    s  <- ts(x, start = st, frequency = S)

    fit <- tryCatch(
      forecast::Arima(
        s,
        order = c(p, d, q),
        seasonal = list(order = c(P, D, Q), period = S),
        include.constant = use_const,
        lambda = lam,
        biasadj = bias,
        method = "CSS-ML"
      ),
      error = function(e) {
        showNotification(paste0("Erro ao ajustar ARIMA Manual:\n", e$message), type = "error", duration = 12)
        NULL
      }
    )
    req(fit)

    manualArimaModel(fit)
    manualArimaLambda(lam)
    manualArimaBias(bias)
    manualArimaS(S)

    manualArimaNote(
      paste0(
        "p,d,q=(", p, ",", d, ",", q, ")",
        if (S > 1) paste0(" | P,D,Q=(", P, ",", D, ",", Q, ")[", S, "]") else "",
        " | const=", use_const,
        " | lambda=", ifelse(is.null(lam), "NULL", as.character(lam)),
        " | biasadj=", bias
      )
    )

  }, ignoreInit = TRUE)

  # --- Resumo (KV -> 2 colunas) ---
  manualArimaReportKV <- reactive({
    req(manualArimaModel())
    fit <- manualArimaModel()

    S    <- manualArimaS()
    lam  <- manualArimaLambda()
    bias <- manualArimaBias()

    ll     <- tryCatch(as.numeric(fit$loglik), error = function(e) NA_real_)
    aic    <- tryCatch(fit$aic, error = function(e) NA_real_)
    bic    <- tryCatch(fit$bic, error = function(e) NA_real_)
    aicc   <- tryCatch(forecast::AICc(fit), error = function(e) NA_real_)
    sigma2 <- tryCatch(fit$sigma2, error = function(e) NA_real_)

    ord <- tryCatch(forecast::arimaorder(fit), error = function(e) NULL)
    ord_str <- if (!is.null(ord)) {
      paste0("(", ord[["p"]], ",", ord[["d"]], ",", ord[["q"]], ")",
             if (S > 1) paste0(" x (", ord[["P"]], ",", ord[["D"]], ",", ord[["Q"]], ")[", S, "]") else "")
    } else NA_character_

    df <- data.frame(
      Campo = c("Período sazonal (S)", "ARIMA ordem", "Lambda", "Biasadj",
                "logLik", "AIC", "AICc", "BIC", "sigma2", "Notas"),
      Valor = c(
        as.character(S),
        ord_str,
        ifelse(is.null(lam), "NULL", as.character(lam)),
        as.character(isTRUE(bias)),
        fmt_num(ll), fmt_num(aic), fmt_num(aicc), fmt_num(bic), fmt_num(sigma2),
        manualArimaNote()
      ),
      stringsAsFactors = FALSE
    )

    acc <- tryCatch(as.data.frame(forecast::accuracy(fit)), error = function(e) NULL)
    if (!is.null(acc) && nrow(acc) >= 1) {
      acc1 <- acc[1, , drop = FALSE]
      for (nm in names(acc1)) {
        df <- rbind(df, data.frame(
          Campo = paste0("Acurácia (treino) - ", nm),
          Valor = fmt_num(acc1[[nm]], digits = 6),
          stringsAsFactors = FALSE
        ))
      }
    }
    df
  })

  manualArimaReport2Col <- reactive({
    req(manualArimaReportKV())
    kv_to_2col(manualArimaReportKV())
  })

  output$manualArimaReportTable <- DT::renderDT({
    req(manualArimaReport2Col())
    DT::datatable(
      manualArimaReport2Col(),
      rownames = FALSE,
      colnames = c("Campo", "Valor", "Campo", "Valor"),
      options = list(pageLength = 25, scrollX = TRUE, dom = "tip", ordering = FALSE)
    )
  })

  output$download_manualarima_resumo_xlsx <- downloadHandler(
    filename = function() paste0("manual_arima_resumo_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(manualArimaReportKV())
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Resumo")
      openxlsx::writeData(wb, "Resumo", manualArimaReportKV())
      openxlsx::setColWidths(wb, "Resumo", cols = 1:2, widths = "auto")
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # --- Coeficientes ---
  manualArimaCoefDF <- reactive({
    req(manualArimaModel())
    coef_table_df(manualArimaModel(), compact = TRUE)
  })

  output$manualArimaCoefTable <- DT::renderDT({
    req(manualArimaCoefDF())
    DT::datatable(manualArimaCoefDF(), rownames = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE, dom = "tip"))
  })

  output$download_manualarima_coef_xlsx <- downloadHandler(
    filename = function() paste0("manual_arima_coeficientes_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(manualArimaCoefDF())
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Coeficientes")
      openxlsx::writeData(wb, "Coeficientes", manualArimaCoefDF())
      openxlsx::setColWidths(wb, "Coeficientes", cols = 1:ncol(manualArimaCoefDF()), widths = "auto")
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  output$download_manualarima_coef_csv <- downloadHandler(
    filename = function() paste0("manual_arima_coeficientes_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(manualArimaCoefDF())
      utils::write.csv(manualArimaCoefDF(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # --- Forecast (IC 95%) ---
  manualArimaForecast <- reactive({
    req(manualArimaModel())
    fit <- manualArimaModel()

    h <- as.integer(input$man_h)
    if (!is.finite(h) || h < 1) h <- 10L

    fc <- tryCatch(
      forecast::forecast(fit, h = h, level = 95, biasadj = isTRUE(manualArimaBias())),
      error = function(e) {
        showNotification(paste0("Erro ao prever (manual):\n", e$message), type = "error", duration = 12)
        NULL
      }
    )
    req(fc)
    fc
  })

  output$manualArimaForecastPlot <- renderPlot({
    req(manualArimaForecast())
    plot(manualArimaForecast(), main = paste0("Previsão - ARIMA Manual (S=", manualArimaS(), ")"))
  })

  manualArimaTable <- reactive({
    req(manualArimaForecast())
    forecast_to_df(manualArimaForecast(), level = 95)
  })

  output$manualArimaForecastTable <- DT::renderDT({
    req(manualArimaTable())
    DT::datatable(manualArimaTable(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$download_manualarima_table_xlsx <- downloadHandler(
    filename = function() paste0("tabela_previsao_manual_arima_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(manualArimaTable())
      write_xlsx(file, manualArimaTable(), sheet = "Previsao")
    }
  )

  # --- Downloads (usa UMA vez: man_px_w / man_px_h / man_dpi) ---
  output$download_manualarima_forecast_png <- downloadHandler(
    filename = function() paste0("manual_arima_previsao_", Sys.Date(), ".png"),
    content  = function(file) {
      req(manualArimaForecast())
      save_plot(
        file, "png",
        px_w = as.integer(input$man_px_w),
        px_h = as.integer(input$man_px_h),
        dpi  = as.integer(input$man_dpi),
        plot_fun = function() plot(manualArimaForecast(), main = paste0("Previsão - ARIMA Manual (S=", manualArimaS(), ")"))
      )
    }
  )

  output$download_manualarima_forecast_pdf <- downloadHandler(
    filename = function() paste0("manual_arima_previsao_", Sys.Date(), ".pdf"),
    content  = function(file) {
      req(manualArimaForecast())
      save_plot(
        file, "pdf",
        width  = as.numeric(input$man_px_w) / as.numeric(input$man_dpi),
        height = as.numeric(input$man_px_h) / as.numeric(input$man_dpi),
        plot_fun = function() plot(manualArimaForecast(), main = paste0("Previsão - ARIMA Manual (S=", manualArimaS(), ")"))
      )
    }
  )

  # --- Diagnóstico ---
  output$manualArimaResidualPlot <- renderPlot({
    req(manualArimaModel())
    forecast::checkresiduals(manualArimaModel())
  })

  output$manualArimaBoxPiercePlot <- renderPlot({
    req(manualArimaModel())
    res <- residuals(manualArimaModel())
    res <- res[is.finite(res)]
    validate(need(length(res) >= 10, "Resíduos insuficientes para Box–Pierce com estabilidade."))
    box_pierce_plot(res)
  })

  output$download_manualarima_resid_png <- downloadHandler(
    filename = function() paste0("manual_arima_residuos_", Sys.Date(), ".png"),
    content  = function(file) {
      req(manualArimaModel())
      save_plot(
        file, "png",
        px_w = as.integer(input$man_px_w),
        px_h = as.integer(input$man_px_h),
        dpi  = as.integer(input$man_dpi),
        plot_fun = function() forecast::checkresiduals(manualArimaModel())
      )
    }
  )

  output$download_manualarima_resid_pdf <- downloadHandler(
    filename = function() paste0("manual_arima_residuos_", Sys.Date(), ".pdf"),
    content  = function(file) {
      req(manualArimaModel())
      save_plot(
        file, "pdf",
        width  = as.numeric(input$man_px_w) / as.numeric(input$man_dpi),
        height = as.numeric(input$man_px_h) / as.numeric(input$man_dpi),
        plot_fun = function() forecast::checkresiduals(manualArimaModel())
      )
    }
  )

  output$download_manualarima_boxpierce_png <- downloadHandler(
    filename = function() paste0("manual_arima_box_pierce_", Sys.Date(), ".png"),
    content  = function(file) {
      req(manualArimaModel())
      save_plot(
        file, "png",
        px_w = as.integer(input$man_px_w),
        px_h = as.integer(input$man_px_h),
        dpi  = as.integer(input$man_dpi),
        plot_fun = function() {
          res <- residuals(manualArimaModel())
          res <- res[is.finite(res)]
          if (length(res) < 10) { plot.new(); text(0.5, 0.5, "Resíduos insuficientes"); return() }
          box_pierce_plot(res)
        }
      )
    }
  )

  output$download_manualarima_boxpierce_pdf <- downloadHandler(
    filename = function() paste0("manual_arima_box_pierce_", Sys.Date(), ".pdf"),
    content  = function(file) {
      req(manualArimaModel())
      save_plot(
        file, "pdf",
        width  = as.numeric(input$man_px_w) / as.numeric(input$man_dpi),
        height = as.numeric(input$man_px_h) / as.numeric(input$man_dpi),
        plot_fun = function() {
          res <- residuals(manualArimaModel())
          res <- res[is.finite(res)]
          if (length(res) < 10) { plot.new(); text(0.5, 0.5, "Resíduos insuficientes"); return() }
          box_pierce_plot(res)
        }
      )
    }
  )

  # INCOMPLETO ARIMA/SARIMA ----------------

  # storage (zera quando muda a planilha/série)
  incompleteArimaModel  <- reactiveVal(NULL)
  incompleteArimaLambda <- reactiveVal(NULL)
  incompleteArimaBias   <- reactiveVal(FALSE)
  incompleteArimaS      <- reactiveVal(1L)
  incompleteArimaNote   <- reactiveVal("")

  observeEvent(input$btn_load,     { incompleteArimaModel(NULL); incompleteArimaLambda(NULL); incompleteArimaBias(FALSE); incompleteArimaS(1L); incompleteArimaNote("") }, ignoreInit = TRUE)
  observeEvent(input$btn_build_ts, { incompleteArimaModel(NULL); incompleteArimaLambda(NULL); incompleteArimaBias(FALSE); incompleteArimaS(1L); incompleteArimaNote("") }, ignoreInit = TRUE)
  observeEvent(input$ts_value_col, { incompleteArimaModel(NULL); incompleteArimaLambda(NULL); incompleteArimaBias(FALSE); incompleteArimaS(1L); incompleteArimaNote("") }, ignoreInit = TRUE)

  # 1) Copiar d/D/S da FAC/FACP
  observeEvent(input$btnCopyDDS_inc, {
    updateNumericInput(session, "inc_d", value = as.integer(input$acf_d %||% 0))
    updateNumericInput(session, "inc_D", value = as.integer(input$acf_D %||% 0))
    updateNumericInput(session, "inc_S", value = as.integer(input$acf_S %||% 1))
    showNotification("Copiado da FAC/FACP: d, D e S.", type = "message", duration = 3)
  }, ignoreInit = TRUE)

  # helpers no padrão do app (Box-Cox já calculado)
  get_lambda_models_incarima <- function(x_num) {
    dec <- boxcox_decision()
    if (!isTRUE(dec$ok) || !isTRUE(dec$apply)) return(NULL)
    if (any(x_num <= 0, na.rm = TRUE)) return(NULL)
    dec$mle
  }
  get_biasadj_incarima <- function(lam) {
    !is.null(lam) && isTRUE(get_biasadj())
  }

  # 2) Ajustar ARIMA/SARIMA incompleto (com exclusões)
  observeEvent(input$btnFitIncompleteArima, {
    req(y_ts())

    s_raw <- as.numeric(y_ts())
    s_raw <- s_raw[is.finite(s_raw)]
    validate(need(length(s_raw) >= 6, "A série precisa ter pelo menos 6 observações."))

    # parâmetros
    p <- max(0L, as.integer(input$inc_p %||% 0))
    d <- max(0L, as.integer(input$inc_d %||% 0))
    q <- max(0L, as.integer(input$inc_q %||% 0))

    P <- max(0L, as.integer(input$inc_P %||% 0))
    D <- max(0L, as.integer(input$inc_D %||% 0))
    Q <- max(0L, as.integer(input$inc_Q %||% 0))

    S <- max(1L, as.integer(input$inc_S %||% 1))

    # travas de sazonalidade
    if (S <= 1L) {
      if (P > 0 || D > 0 || Q > 0) {
        showNotification("S=1 não permite termos sazonais. Zerei P/D/Q.", type = "warning", duration = 6)
      }
      P <- D <- Q <- 0L
      updateNumericInput(session, "inc_P", value = 0)
      updateNumericInput(session, "inc_D", value = 0)
      updateNumericInput(session, "inc_Q", value = 0)
    }

    n <- length(s_raw)
    if (S > floor(n/2)) {
      showNotification("Período S grande para o tamanho da série. Ajustei S=1 e zerei termos sazonais.",
                       type = "warning", duration = 8)
      S <- 1L
      updateNumericInput(session, "inc_S", value = 1)
      P <- D <- Q <- 0L
      updateNumericInput(session, "inc_P", value = 0)
      updateNumericInput(session, "inc_D", value = 0)
      updateNumericInput(session, "inc_Q", value = 0)
    }

    # constante: se tem diferença (d ou D), não usar constante
    use_const <- if ((d + D) > 0) FALSE else isTRUE(input$inc_const)

    # >>> lambda/bias (mesma regra do app)
    lam  <- get_lambda_models_incarima(s_raw)
    bias <- get_biasadj_incarima(lam)

    # ts (mantém start da série)
    st <- tryCatch(start(y_ts()), error = function(e) c(1, 1))
    s  <- ts(s_raw, start = st, frequency = S)

    # exclusões
    excl_ar  <- parse_lags(input$inc_excl_ar)
    excl_ma  <- parse_lags(input$inc_excl_ma)
    excl_sar <- parse_lags(input$inc_excl_sar)
    excl_sma <- parse_lags(input$inc_excl_sma)

    # filtra só o que existe
    excl_ar  <- excl_ar[excl_ar >= 1 & excl_ar <= p]
    excl_ma  <- excl_ma[excl_ma >= 1 & excl_ma <= q]
    excl_sar <- excl_sar[excl_sar >= 1 & excl_sar <= P]
    excl_sma <- excl_sma[excl_sma >= 1 & excl_sma <= Q]

    has_exclusion <- (length(excl_ar) + length(excl_ma) + length(excl_sar) + length(excl_sma)) > 0

    # monta fixed na ordem: ar.., ma.., sar.., sma.., (const por último)
    fixed <- NULL
    if (has_exclusion) {
      npar <- p + q + P + Q + if (use_const) 1L else 0L
      fixed <- rep(NA_real_, npar)
      if (p > 0 && length(excl_ar))  fixed[excl_ar] <- 0
      if (q > 0 && length(excl_ma))  fixed[p + excl_ma] <- 0
      if (P > 0 && length(excl_sar)) fixed[p + q + excl_sar] <- 0
      if (Q > 0 && length(excl_sma)) fixed[p + q + P + excl_sma] <- 0
    }

    fit <- tryCatch({
      if (!has_exclusion) {
        fit_arima_safe(
          s,
          order = c(p, d, q),
          seasonal = list(order = c(P, D, Q), period = S),
          include.constant = use_const,
          lambda = lam,
          biasadj = bias,
          prefer = "CSS-ML"
        )
      } else {
        forecast::Arima(
          s,
          order = c(p, d, q),
          seasonal = list(order = c(P, D, Q), period = S),
          include.constant = use_const,
          fixed = fixed,
          lambda = lam,
          biasadj = bias,
          method = "ML"
        )
      }
    }, error = function(e) {
      showNotification(paste0("Erro ao ajustar ARIMA incompleto:\n", e$message), type = "error", duration = 12)
      NULL
    })

    req(fit)

    incompleteArimaModel(fit)
    incompleteArimaLambda(lam)
    incompleteArimaBias(bias)
    incompleteArimaS(S)



    showNotification("Modelo incompleto ajustado com sucesso!", type = "message", duration = 5)
  }, ignoreInit = TRUE)



  # 3) Resumo do modelo (KV -> 2 colunas)
  incompleteArimaReportKV <- reactive({
    fit <- incompleteArimaModel(); req(fit)

    S    <- incompleteArimaS()
    lam  <- incompleteArimaLambda()
    bias <- incompleteArimaBias()

    ll     <- tryCatch(as.numeric(fit$loglik), error = function(e) NA_real_)
    aic    <- tryCatch(fit$aic, error = function(e) NA_real_)
    bic    <- tryCatch(fit$bic, error = function(e) NA_real_)
    aicc   <- tryCatch(forecast::AICc(fit), error = function(e) NA_real_)
    sigma2 <- tryCatch(fit$sigma2, error = function(e) NA_real_)

    ord <- tryCatch(forecast::arimaorder(fit), error = function(e) NULL)
    ord_str <- if (!is.null(ord)) {
      paste0("(", ord[["p"]], ",", ord[["d"]], ",", ord[["q"]], ")",
             if (S > 1) paste0(" x (", ord[["P"]], ",", ord[["D"]], ",", ord[["Q"]], ")[", S, "]") else "")
    } else NA_character_

    data.frame(
      Campo = c("Período sazonal (S)", "Ordem ARIMA", "Lambda usado (modelos)", "Biasadj",
                "AIC", "AICc", "BIC", "logLik", "sigma2", "Notas"),
      Valor = c(as.character(S), ord_str,
                ifelse(is.null(lam), "NULL", as.character(lam)),
                as.character(isTRUE(bias)),
                fmt_num(aic), fmt_num(aicc), fmt_num(bic), fmt_num(ll), fmt_num(sigma2),
                incompleteArimaNote()),
      stringsAsFactors = FALSE
    )
  })

  incompleteArimaReport2Col <- reactive({
    req(incompleteArimaReportKV())
    kv_to_2col(incompleteArimaReportKV())
  })

  output$incompleteArimaReportTable <- DT::renderDT({
    req(incompleteArimaReport2Col())
    DT::datatable(
      incompleteArimaReport2Col(),
      rownames = FALSE,
      colnames = c("Campo", "Valor", "Campo", "Valor"),
      options = list(pageLength = 25, scrollX = TRUE, dom = "tip", ordering = FALSE)
    )
  })

  # 4) Coeficientes
  incompleteArimaCoefDF <- reactive({
    fit <- incompleteArimaModel(); req(fit)
    coef_table_df(fit, compact = TRUE)
  })

  output$incompleteArimaCoefTable <- DT::renderDT({
    req(incompleteArimaCoefDF())
    DT::datatable(incompleteArimaCoefDF(), rownames = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE, dom = "tip"))
  })

  # 5) Previsão (somente 95)
  incompleteArimaForecast <- reactive({
    fit <- incompleteArimaModel(); req(fit)
    h <- as.integer(input$inc_h); if (!is.finite(h) || h < 1) h <- 10L
    forecast::forecast(fit, h = h, level = 95, biasadj = isTRUE(incompleteArimaBias()))
  })

  output$incompleteArimaForecastPlot <- renderPlot({
    req(incompleteArimaForecast())
    plot(incompleteArimaForecast(), main = paste0("Previsão - Incompleto (S=", incompleteArimaS(), ") (IC 95%)"))
    grid()
  })

  incompleteArimaForecastTableDF <- reactive({
    req(incompleteArimaForecast())
    forecast_to_df(incompleteArimaForecast(), level = 95)
  })

  output$incompleteArimaForecastTable <- DT::renderDT({
    req(incompleteArimaForecastTableDF())
    DT::datatable(incompleteArimaForecastTableDF(), rownames = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE))
  })

  # 6) Diagnóstico
  output$incompleteArimaResidualPlot <- renderPlot({
    fit <- incompleteArimaModel(); req(fit)
    forecast::checkresiduals(fit)
  })

  output$incompleteArimaBoxPiercePlot <- renderPlot({
    fit <- incompleteArimaModel(); req(fit)
    res <- residuals(fit)
    res <- res[is.finite(res)]
    validate(need(length(res) >= 10, "Resíduos insuficientes para Box–Pierce com estabilidade."))
    box_pierce_plot(res)
  })

  #Downloads (usando inc_px_w/inc_px_h/inc_dpi)
  output$download_incarima_resumo_xlsx <- downloadHandler(
    filename = function() paste0("inc_arima_resumo_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(incompleteArimaReportKV())
      write_xlsx(file, incompleteArimaReportKV(), sheet = "Resumo")
    }
  )

  output$download_incarima_coef_xlsx <- downloadHandler(
    filename = function() paste0("inc_arima_coef_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(incompleteArimaCoefDF())
      write_xlsx(file, incompleteArimaCoefDF(), sheet = "Coeficientes")
    }
  )

  output$download_incarima_coef_csv <- downloadHandler(
    filename = function() paste0("inc_arima_coef_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(incompleteArimaCoefDF())
      utils::write.csv(incompleteArimaCoefDF(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$download_incarima_table_xlsx <- downloadHandler(
    filename = function() paste0("inc_arima_tabela_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(incompleteArimaForecast())
      write_forecast_xlsx(file, incompleteArimaForecast(), sheet = "Previsao", level = 95)
    }
  )

  output$download_incarima_forecast_png <- downloadHandler(
    filename = function() paste0("inc_arima_previsao_", Sys.Date(), ".png"),
    content  = function(file) {
      req(incompleteArimaForecast())
      save_plot(
        file, "png",
        px_w = as.integer(input$inc_px_w), px_h = as.integer(input$inc_px_h), dpi = as.integer(input$inc_dpi),
        plot_fun = function() {
          plot(incompleteArimaForecast(), main = paste0("Previsão - Incompleto (S=", incompleteArimaS(), ") (IC 95%)"))
          grid()
        }
      )
    }
  )

  output$download_incarima_forecast_pdf <- downloadHandler(
    filename = function() paste0("inc_arima_previsao_", Sys.Date(), ".pdf"),
    content  = function(file) {
      req(incompleteArimaForecast())
      save_plot(
        file, "pdf",
        width  = as.numeric(input$inc_px_w) / as.numeric(input$inc_dpi),
        height = as.numeric(input$inc_px_h) / as.numeric(input$inc_dpi),
        plot_fun = function() {
          plot(incompleteArimaForecast(), main = paste0("Previsão - Incompleto (S=", incompleteArimaS(), ") (IC 95%)"))
          grid()
        }
      )
    }
  )

  output$download_incarima_resid_png <- downloadHandler(
    filename = function() paste0("inc_arima_residuos_", Sys.Date(), ".png"),
    content  = function(file) {
      req(incompleteArimaModel())
      save_plot(
        file, "png",
        px_w = as.integer(input$inc_px_w), px_h = as.integer(input$inc_px_h), dpi = as.integer(input$inc_dpi),
        plot_fun = function() forecast::checkresiduals(incompleteArimaModel())
      )
    }
  )

  output$download_incarima_resid_pdf <- downloadHandler(
    filename = function() paste0("inc_arima_residuos_", Sys.Date(), ".pdf"),
    content  = function(file) {
      req(incompleteArimaModel())
      save_plot(
        file, "pdf",
        width  = as.numeric(input$inc_px_w) / as.numeric(input$inc_dpi),
        height = as.numeric(input$inc_px_h) / as.numeric(input$inc_dpi),
        plot_fun = function() forecast::checkresiduals(incompleteArimaModel())
      )
    }
  )

  output$download_incarima_boxpierce_png <- downloadHandler(
    filename = function() paste0("inc_arima_boxpierce_", Sys.Date(), ".png"),
    content  = function(file) {
      req(incompleteArimaModel())
      save_plot(
        file, "png",
        px_w = as.integer(input$inc_px_w), px_h = as.integer(input$inc_px_h), dpi = as.integer(input$inc_dpi),
        plot_fun = function() {
          res <- residuals(incompleteArimaModel())
          res <- res[is.finite(res)]
          if (length(res) < 10) { plot.new(); text(0.5, 0.5, "Resíduos insuficientes"); return() }
          box_pierce_plot(res)
        }
      )
    }
  )

  output$download_incarima_boxpierce_pdf <- downloadHandler(
    filename = function() paste0("inc_arima_boxpierce_", Sys.Date(), ".pdf"),
    content  = function(file) {
      req(incompleteArimaModel())
      save_plot(
        file, "pdf",
        width  = as.numeric(input$inc_px_w) / as.numeric(input$inc_dpi),
        height = as.numeric(input$inc_px_h) / as.numeric(input$inc_dpi),
        plot_fun = function() {
          res <- residuals(incompleteArimaModel())
          res <- res[is.finite(res)]
          if (length(res) < 10) { plot.new(); text(0.5, 0.5, "Resíduos insuficientes"); return() }
          box_pierce_plot(res)
        }
      )
    }
  )

  # Outliers --------------

  #  storage
  outBaseModel   <- reactiveVal(NULL)   # modelo base carregado (manual ou incompleto)
  outFinalModel  <- reactiveVal(NULL)   # modelo final (com xreg)
  outMO          <- reactiveVal(NULL)   # matriz t/type
  outDetectRaw   <- reactiveVal(NULL)   # guarda detecção (out, mo_mat, pars)
  outFinalXreg   <- reactiveVal(NULL)   # xreg (treino)
  outNewXreg     <- reactiveVal(NULL)   # newxreg (forecast)

  # metadados
  outLambda <- reactiveVal(NULL)
  outBias   <- reactiveVal(FALSE)
  outS      <- reactiveVal(1L)
  outNote   <- reactiveVal("")

  #reset quando série muda
  observeEvent(input$btn_load,     { outBaseModel(NULL); outFinalModel(NULL); outMO(NULL); outDetectRaw(NULL); outFinalXreg(NULL); outNewXreg(NULL) }, ignoreInit = TRUE)
  observeEvent(input$btn_build_ts, { outBaseModel(NULL); outFinalModel(NULL); outMO(NULL); outDetectRaw(NULL); outFinalXreg(NULL); outNewXreg(NULL) }, ignoreInit = TRUE)
  observeEvent(input$ts_value_col, { outBaseModel(NULL); outFinalModel(NULL); outMO(NULL); outDetectRaw(NULL); outFinalXreg(NULL); outNewXreg(NULL) }, ignoreInit = TRUE)

  # helpers: pegar lambda/bias do padrão do app
  # (igual ao que você fez na suavização)
  get_lambda_models_out <- function(x_num) {
    dec <- boxcox_decision()
    if (!isTRUE(dec$ok) || !isTRUE(dec$apply)) return(NULL)
    if (any(x_num <= 0, na.rm = TRUE)) return(NULL)
    dec$mle
  }
  get_biasadj_out <- function(lam) {
    !is.null(lam) && isTRUE(get_biasadj()) # se no seu app biasadj é sempre FALSE, isso vira FALSE
  }

  # botão copiar d/D/S da FAC/FACP (apenas para conveniência de UI)
  observeEvent(input$btnCopyDDS_out, {
    updateNumericInput(session, "out_d", value = as.integer(input$acf_d %||% 0))
    updateNumericInput(session, "out_D", value = as.integer(input$acf_D %||% 0))
    updateNumericInput(session, "out_S", value = as.integer(input$acf_S %||% 1))
    showNotification("Copiado da FAC/FACP: d, D e S.", type = "message", duration = 3)
  })


  # PASSO 1) CARREGAR MODELO BASE (MANUAL ou INCOMPLETO)

  observeEvent(input$btnLoadBase_out, {
    req(y_ts())

    src <- input$out_base_source %||% "manual"

    base_fit <- NULL
    if (identical(src, "manual")) {
      base_fit <- manualArimaModel()
      if (is.null(base_fit)) {
        showNotification("Modelo Manual ainda não foi ajustado. Vá na aba Ajuste Manual e ajuste o modelo.", type="error", duration = 8)
        return()
      }
    } else {
      base_fit <- incompleteArimaModel()
      if (is.null(base_fit)) {
        showNotification("Modelo Incompleto ainda não foi ajustado. Vá na aba Modelo Incompleto e ajuste o modelo.", type="error", duration = 8)
        return()
      }
    }

    # valida: mesmo tamanho da série atual (evita pegar modelo de outra série)
    y_now <- as.numeric(y_ts()); y_now <- y_now[is.finite(y_now)]
    y_fit <- tryCatch(as.numeric(base_fit$x), error = function(e) NULL)
    if (is.null(y_fit)) {
      # fallback: tenta length residuals + fitted
      y_fit <- tryCatch(as.numeric(fitted(base_fit) + residuals(base_fit)), error = function(e) NULL)
    }
    if (!is.null(y_fit)) {
      y_fit <- y_fit[is.finite(y_fit)]
      if (length(y_fit) != length(y_now)) {
        showNotification("O modelo base parece ter sido ajustado em outra série (tamanho diferente). Ajuste o modelo base novamente para a série atual.", type="error", duration = 10)
        return()
      }
    }

    outBaseModel(base_fit)

    # metadata (S / lambda / bias) extraídos do próprio fit quando possível
    S  <- tryCatch(frequency(base_fit$x), error = function(e) tryCatch(base_fit$arma[5], error = function(e2) 1L))
    if (!is.finite(S) || S < 1) S <- 1L
    outS(as.integer(S))

    xnum <- as.numeric(y_ts()); xnum <- xnum[is.finite(xnum)]
    lam  <- get_lambda_models_out(xnum)
    bias <- get_biasadj_out(lam)

    outLambda(lam)
    outBias(bias)

    ord <- tryCatch(forecast::arimaorder(base_fit), error = function(e) NULL)
    ord_str <- if (!is.null(ord)) {
      paste0("(", ord[["p"]], ",", ord[["d"]], ",", ord[["q"]], ")",
             if (S > 1) paste0(" x (", ord[["P"]], ",", ord[["D"]], ",", ord[["Q"]], ")[", S, "]") else "")
    } else "NA"

    outNote(paste0("Base carregada: ", if (src=="manual") "Manual" else "Incompleto",
                   " | Ordem: ", ord_str,
                   " | lambda=", ifelse(is.null(lam), "NULL", as.character(lam)),
                   " | biasadj=", as.character(isTRUE(bias))))

    # limpa passo 2/3
    outFinalModel(NULL); outMO(NULL); outDetectRaw(NULL); outFinalXreg(NULL); outNewXreg(NULL)

    showNotification("Modelo base carregado. Agora clique em 'Detectar outliers'.", type="message", duration = 6)
  })


  # RESULTADOS BASE (Coef + Métricas)

  outBaseCoefDF <- reactive({
    req(outBaseModel())
    coef_table_df(outBaseModel(), compact = TRUE)
  })

  output$out_base_coef_table <- DT::renderDT({
    req(outBaseCoefDF())
    DT::datatable(outBaseCoefDF(), rownames = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE, dom = "tip"))
  })

  outBaseMetricsDF <- reactive({
    fit <- outBaseModel(); req(fit)
    S    <- outS()
    lam  <- outLambda()
    bias <- outBias()

    ll     <- tryCatch(as.numeric(fit$loglik), error = function(e) NA_real_)
    aic    <- tryCatch(fit$aic, error = function(e) NA_real_)
    bic    <- tryCatch(fit$bic, error = function(e) NA_real_)
    aicc   <- tryCatch(forecast::AICc(fit), error = function(e) NA_real_)
    sigma2 <- tryCatch(fit$sigma2, error = function(e) NA_real_)

    ord <- tryCatch(forecast::arimaorder(fit), error = function(e) NULL)
    ord_str <- if (!is.null(ord)) {
      paste0("(", ord[["p"]], ",", ord[["d"]], ",", ord[["q"]], ")",
             if (S > 1) paste0(" x (", ord[["P"]], ",", ord[["D"]], ",", ord[["Q"]], ")[", S, "]") else "")
    } else NA_character_

    data.frame(
      Metrica = c("S", "Ordem", "Lambda", "Biasadj", "logLik", "AIC", "AICc", "BIC", "sigma2"),
      Valor   = c(as.character(S),
                  ord_str,
                  ifelse(is.null(lam), "NULL", as.character(lam)),
                  as.character(isTRUE(bias)),
                  fmt_num(ll), fmt_num(aic), fmt_num(aicc), fmt_num(bic), fmt_num(sigma2)),
      stringsAsFactors = FALSE
    )
  })

  output$out_base_metrics_table <- DT::renderDT({
    req(outBaseMetricsDF())
    DT::datatable(outBaseMetricsDF(), rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$download_out_base_metrics_xlsx <- downloadHandler(
    filename = function() paste0("metricas_modelo_base_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(outBaseMetricsDF())
      write_xlsx(file, outBaseMetricsDF(), sheet = "Metricas_Base")
    }
  )

  output$download_out_base_coef_xlsx <- downloadHandler(
    filename = function() paste0("coeficientes_modelo_base_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(outBaseCoefDF())
      write_xlsx(file, outBaseCoefDF(), sheet = "Coef_Base")
    }
  )


  # PASSO 2) DETECTAR OUTLIERS (usa outBaseModel)

  observeEvent(input$btnDetect_out, {
    fit <- outBaseModel()
    req(fit)

    showNotification("Detectando outliers...", type = "message", duration = 3)

    r_all <- as.numeric(residuals(fit))
    idx   <- which(is.finite(r_all))
    r     <- r_all[idx]
    validate(need(length(r) >= 10, "Resíduos insuficientes para detecção (mín. ~10)."))

    pars <- tryCatch(tsoutliers::coefs2poly(fit), error = function(e) NULL)
    if (is.null(pars)) {
      showNotification("Falha ao obter pars via coefs2poly().", type = "error", duration = 8)
      return()
    }
    class(pars) <- "ArimaPars"

    out_found <- tryCatch(
      tsoutliers::locate.outliers(
        r, pars,
        cval  = input$out_cval %||% 3.5,
        types = input$out_types %||% c("AO","LS","TC","IO"),
        delta = input$out_delta %||% 0.7
      ),
      error = function(e) {
        showNotification(paste0("Erro na detecção de outliers:\n", e$message),
                         type = "error", duration = 12)
        NULL
      }
    )

    if (is.null(out_found) || nrow(out_found) == 0) {
      outMO(NULL)
      outDetectRaw(NULL)
      showNotification("Nenhum outlier encontrado.", type = "warning", duration = 5)
      return()
    }

    out_df <- as.data.frame(out_found)
    if (!all(c("type","ind") %in% names(out_df))) {
      outMO(NULL)
      outDetectRaw(NULL)
      showNotification("Detecção retornou formato inesperado (sem colunas type/ind).", type = "error", duration = 8)
      return()
    }

    # ind é posição dentro de r (vetor filtrado). idx mapeia para posição original.
    t_orig <- idx[as.integer(out_df$ind)]

    mo_mat <- cbind(
      t    = as.integer(t_orig),
      type = as.character(out_df$type)
    )

    outMO(mo_mat)
    outDetectRaw(list(out = out_df, mo_mat = mo_mat, pars = pars))

    showNotification(paste0(nrow(mo_mat), " outliers identificados."), type = "message", duration = 5)
  })

  # tabela dos outliers (UI)
  outTableViewDF <- reactive({
    mo <- outMO()
    if (is.null(mo) || nrow(mo) == 0) return(NULL)

    base <- outBaseModel()
    x_ts <- tryCatch(base$x, error = function(e) NULL)
    if (is.null(x_ts)) x_ts <- tryCatch(dados_ts(), error = function(e) NULL)

    data.frame(
      ID          = seq_len(nrow(mo)),
      t           = as.integer(mo[, "t"]),
      Tempo       = if (!is.null(x_ts))
        vapply(as.integer(mo[, "t"]), function(k) safe_time_label(x_ts, k), character(1))
      else NA_character_,
      type        = as.character(mo[, "type"]),
      Significado = outlier_type_desc(as.character(mo[, "type"])),
      stringsAsFactors = FALSE
    )
  })

  output$out_table <- DT::renderDT({
    df <- outTableViewDF()
    if (is.null(df) || nrow(df) == 0) {
      return(DT::datatable(data.frame(Mensagem = "Nenhum outlier na lista."), options = list(dom = "t")))
    }
    DT::datatable(df, selection = "multiple", rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  observeEvent(input$btnResetOut_out, {
    outMO(NULL)
    outDetectRaw(NULL)
    showNotification("Lista de outliers resetada.", type="message", duration = 4)
  })

  observeEvent(input$btnRemoveSel_out, {
    mo <- outMO()
    req(mo)

    sel <- input$out_table_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showNotification("Selecione linhas na tabela para remover.", type="message", duration = 4)
      return()
    }

    mo2 <- mo[-sel, , drop = FALSE]
    if (nrow(mo2) == 0) mo2 <- NULL
    outMO(mo2)
  })


  # PASSO 3) AJUSTAR MODELO FINAL (xreg) — usa a base como referência p/ pars/det

  # helper robusto (coloque 1x no topo do server)
  get_orders_from_fit <- function(fit) {
    a <- fit$arma
    # a = c(p, q, P, Q, S, d, D)
    list(
      p = as.integer(a[1]),
      q = as.integer(a[2]),
      P = as.integer(a[3]),
      Q = as.integer(a[4]),
      S = as.integer(a[5]),
      d = as.integer(a[6]),
      D = as.integer(a[7])
    )
  }

  observeEvent(input$btnFitWithOut_out, {

    req(y_ts())
    base_fit <- outBaseModel()
    req(base_fit)

    mo_mat <- outMO()
    if (is.null(mo_mat) || nrow(mo_mat) == 0) {
      showNotification("Não há outliers na lista. Primeiro detecte outliers.", type = "warning", duration = 6)
      return()
    }

    # série (sempre usa o valor do reativo)
    y <- y_ts()
    n <- length(y)
    h <- max(1L, as.integer(input$out_h %||% 12))

    # -------- pars (preferência: da detecção; senão recalcula) --------
    det  <- outDetectRaw()
    pars <- if (!is.null(det) && !is.null(det$pars)) det$pars else {
      p2 <- tryCatch(tsoutliers::coefs2poly(base_fit), error = function(e) NULL)
      if (!is.null(p2)) { class(p2) <- "ArimaPars"; p2 } else NULL
    }

    if (is.null(pars)) {
      showNotification("Não consegui obter pars (ArimaPars). Refaça o carregamento do modelo base.", type = "error", duration = 10)
      return()
    }

    # -------- xreg / newxreg --------
    xpair <- safe_do_call(
      build_xreg_pair(
        mo       = mo_mat,
        n        = n,
        h        = h,
        pars     = pars,
        max_cols = 80,
        tol_var  = 0,
        tol_qr   = 1e-10
      ),
      title = "Falha ao construir xreg/newxreg (effects+pars)",
      on_error = "notify"
    )
    if (is.null(xpair)) return()

    # validações fortes para evitar crash silencioso
    if (!is.list(xpair) || is.null(xpair$xreg)) {
      showNotification("build_xreg_pair() não retornou xreg.", type = "error", duration = 10)
      return()
    }

    xreg    <- xpair$xreg
    newxreg <- xpair$newxreg

    if (!is.matrix(xreg)) xreg <- as.matrix(xreg)
    if (nrow(xreg) != n) {
      showNotification("xreg com nº de linhas diferente do tamanho da série.", type = "error", duration = 10)
      return()
    }

    if (!is.null(newxreg)) {
      if (!is.matrix(newxreg)) newxreg <- as.matrix(newxreg)
      if (nrow(newxreg) != h || ncol(newxreg) != ncol(xreg)) {
        showNotification("newxreg incompatível com xreg (dimensões).", type = "error", duration = 10)
        return()
      }
    }

    # -------- ordens do base (ROBUSTO: usa $arma) --------
    ord <- tryCatch(get_orders_from_fit(base_fit), error = function(e) NULL)
    if (is.null(ord)) {
      showNotification("Não consegui ler as ordens via base_fit$arma.", type = "error", duration = 8)
      return()
    }

    # período sazonal: use o do base (mais seguro que outS())
    S <- ord$S
    if (!is.finite(S) || S < 1) S <- 1L

    # constante: se há diferenças (d ou D), não usar constante
    d_total  <- ord$d + ord$D
    use_const <- if (d_total > 0) FALSE else TRUE

    lam  <- outLambda()
    bias <- outBias()

    # -------- ajuste final --------
    fit_final <- safe_do_call(
      fit_arima_safe(
        y,
        order    = c(ord$p, ord$d, ord$q),
        seasonal = list(order = c(ord$P, ord$D, ord$Q), period = S),
        xreg     = xreg,
        include.constant = use_const,
        lambda   = lam,
        biasadj  = bias,
        prefer   = "ML"
      ),
      title = "Falha ao ajustar ARIMA com xreg",
      on_error = "notify"
    )
    if (is.null(fit_final)) return()

    outFinalModel(fit_final)
    outFinalXreg(xreg)
    outNewXreg(newxreg)
    outS(S)  # atualiza metadado

    showNotification("Modelo final (com outliers) ajustado com sucesso!", type = "message", duration = 6)
  })

  # RELATÓRIOS FINAL

  outFinalReportDF <- reactive({
    fit <- outFinalModel(); req(fit)

    S    <- outS()
    lam  <- outLambda()
    bias <- outBias()
    n_out <- if (is.null(outMO())) 0L else nrow(outMO())

    ll     <- tryCatch(as.numeric(fit$loglik), error = function(e) NA_real_)
    aic    <- tryCatch(fit$aic,  error = function(e) NA_real_)
    bic    <- tryCatch(fit$bic,  error = function(e) NA_real_)
    aicc   <- tryCatch(forecast::AICc(fit), error = function(e) NA_real_)
    sigma2 <- tryCatch(fit$sigma2, error = function(e) NA_real_)

    ord <- tryCatch(forecast::arimaorder(fit), error = function(e) NULL)
    ord_str <- if (!is.null(ord)) {
      paste0("(", ord[["p"]], ",", ord[["d"]], ",", ord[["q"]], ")",
             if (S > 1) paste0(" x (", ord[["P"]], ",", ord[["D"]], ",", ord[["Q"]], ")[", S, "]") else "")
    } else NA_character_

    data.frame(
      Metrica = c("S", "Ordem", "Lambda", "Biasadj", "Nº outliers", "logLik", "AIC", "AICc", "BIC", "sigma2", "Notas"),
      Valor   = c(as.character(S),
                  ord_str,
                  ifelse(is.null(lam), "NULL", as.character(lam)),
                  as.character(isTRUE(bias)),
                  as.character(n_out),
                  fmt_num(ll), fmt_num(aic), fmt_num(aicc), fmt_num(bic), fmt_num(sigma2),
                  outNote()),
      stringsAsFactors = FALSE
    )
  })

  output$out_final_report_table <- DT::renderDT({
    req(outFinalReportDF())
    DT::datatable(outFinalReportDF(), rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$download_out_final_resumo_xlsx <- downloadHandler(
    filename = function() paste0("outliers_resumo_final_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(outFinalReportDF())
      write_xlsx(file, outFinalReportDF(), sheet = "Resumo_Final")
    }
  )

  outFinalCoefDF <- reactive({
    req(outFinalModel())
    coef_table_df(outFinalModel(), compact = isTRUE(input$out_compact_coef))
  })

  output$out_final_coef_table <- DT::renderDT({
    req(outFinalCoefDF())
    DT::datatable(outFinalCoefDF(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, dom = "tip"))
  })

  output$download_out_final_coef_xlsx <- downloadHandler(
    filename = function() paste0("outliers_coef_final_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(outFinalCoefDF())
      write_xlsx(file, outFinalCoefDF(), sheet = "Coef_Final")
    }
  )


  # PREVISÃO — usa newxreg
  outForecast <- reactive({
    fit <- outFinalModel(); req(fit)
    h  <- max(1L, as.integer(input$out_h %||% 12))
    nx <- outNewXreg()

    fc <- tryCatch({
      if (!is.null(nx)) forecast::forecast(fit, h = h, xreg = nx, level = 95, biasadj = isTRUE(outBias()))
      else              forecast::forecast(fit, h = h,           level = 95, biasadj = isTRUE(outBias()))
    }, error = function(e) {
      showNotification(paste0("Erro ao prever (outliers):\n", e$message), type = "error", duration = 10)
      NULL
    })
    req(fc)
    keep_only_level(fc, 95)
  })

  output$out_forecast_plot <- renderPlot({
    req(outForecast())
    plot(outForecast(), main = paste0("Previsão - Outliers (S=", outS(), ") (IC 95%)"))
    grid()
  })

  outForecastTable <- reactive({
    req(outForecast())
    forecast_to_df(outForecast(), level = 95)
  })

  output$out_forecast_table <- DT::renderDT({
    req(outForecastTable())
    DT::datatable(outForecastTable(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$download_out_table_xlsx <- downloadHandler(
    filename = function() paste0("tabela_previsao_outliers_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(outForecast())
      write_forecast_xlsx(file, outForecast(), sheet = "Previsao", level = 95)
    }
  )


  # DIAGNÓSTICO

  output$out_resid_plot <- renderPlot({
    req(outFinalModel())
    forecast::checkresiduals(outFinalModel())
  })

  output$out_boxpierce_plot <- renderPlot({
    req(outFinalModel())
    res <- residuals(outFinalModel())
    res <- res[is.finite(res)]
    validate(need(length(res) >= 10, "Resíduos insuficientes para Box–Pierce."))
    box_pierce_plot(res)
  })


  # DOWNLOADS (usando out_px_w/out_px_h/out_dpi do UI)

  output$download_out_forecast_png <- downloadHandler(
    filename = function() paste0("outliers_previsao_", Sys.Date(), ".png"),
    content  = function(file) {
      req(outForecast())
      save_plot(
        file = file, device = "png",
        px_w = as.integer(input$out_px_w),
        px_h = as.integer(input$out_px_h),
        dpi  = as.integer(input$out_dpi),
        plot_fun = function() {
          plot(outForecast(), main = paste0("Previsão - Outliers (S=", outS(), ") (IC 95%)"))
          grid()
        }
      )
    }
  )

  output$download_out_forecast_pdf <- downloadHandler(
    filename = function() paste0("outliers_previsao_", Sys.Date(), ".pdf"),
    content  = function(file) {
      req(outForecast())
      save_plot(
        file = file, device = "pdf",
        width  = as.numeric(input$out_px_w) / as.numeric(input$out_dpi),
        height = as.numeric(input$out_px_h) / as.numeric(input$out_dpi),
        plot_fun = function() {
          plot(outForecast(), main = paste0("Previsão - Outliers (S=", outS(), ") (IC 95%)"))
          grid()
        }
      )
    }
  )

  output$download_out_resid_png <- downloadHandler(
    filename = function() paste0("outliers_residuos_", Sys.Date(), ".png"),
    content  = function(file) {
      req(outFinalModel())
      save_plot(
        file = file, device = "png",
        px_w = as.integer(input$out_px_w),
        px_h = as.integer(input$out_px_h),
        dpi  = as.integer(input$out_dpi),
        plot_fun = function() forecast::checkresiduals(outFinalModel())
      )
    }
  )

  output$download_out_resid_pdf <- downloadHandler(
    filename = function() paste0("outliers_residuos_", Sys.Date(), ".pdf"),
    content  = function(file) {
      req(outFinalModel())
      save_plot(
        file = file, device = "pdf",
        width  = as.numeric(input$out_px_w) / as.numeric(input$out_dpi),
        height = as.numeric(input$out_px_h) / as.numeric(input$out_dpi),
        plot_fun = function() forecast::checkresiduals(outFinalModel())
      )
    }
  )

  output$download_out_boxpierce_png <- downloadHandler(
    filename = function() paste0("outliers_boxpierce_", Sys.Date(), ".png"),
    content  = function(file) {
      req(outFinalModel())
      save_plot(
        file = file, device = "png",
        px_w = as.integer(input$out_px_w),
        px_h = as.integer(input$out_px_h),
        dpi  = as.integer(input$out_dpi),
        plot_fun = function() {
          res <- residuals(outFinalModel()); res <- res[is.finite(res)]
          if (length(res) < 10) { plot.new(); text(0.5,0.5,"Resíduos insuficientes"); return() }
          box_pierce_plot(res)
        }
      )
    }
  )

  output$download_out_boxpierce_pdf <- downloadHandler(
    filename = function() paste0("outliers_boxpierce_", Sys.Date(), ".pdf"),
    content  = function(file) {
      req(outFinalModel())
      save_plot(
        file = file, device = "pdf",
        width  = as.numeric(input$out_px_w) / as.numeric(input$out_dpi),
        height = as.numeric(input$out_px_h) / as.numeric(input$out_dpi),
        plot_fun = function() {
          res <- residuals(outFinalModel()); res <- res[is.finite(res)]
          if (length(res) < 10) { plot.new(); text(0.5,0.5,"Resíduos insuficientes"); return() }
          box_pierce_plot(res)
        }
      )
    }
  )
}

shinyApp(ui, server)
