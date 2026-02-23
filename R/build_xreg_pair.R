
#' Construir pares de regressoras externas (xreg/newxreg) a partir de outliers
#'
#' Gera matrizes de regressoras externas para ajuste e previsão em modelos ARIMA
#' com outliers, a partir de uma tabela de detecções contendo tempo (\code{t})
#' e tipo (\code{type}). Pode usar efeitos exatos via \pkg{tsoutliers} ou uma
#' construção manual aproximada.
#'
#' A função retorna um par \code{xreg}/\code{newxreg} consistente com os
#' argumentos \code{xreg} e \code{newxreg} de \code{forecast::Arima()} e
#' \code{forecast::forecast()}.
#'
#' @param mo Matriz ou \code{data.frame} com pelo menos as colunas
#'   \code{t} (posição do outlier na série) e \code{type} (tipo de outlier,
#'   por exemplo \code{"AO"}, \code{"LS"}, \code{"TC"}, \code{"IO"}).
#' @param n Tamanho da amostra de treino.
#' @param h Horizonte de previsão.
#' @param method Método de construção dos efeitos:
#'   \describe{
#'     \item{\code{"effects"}}{usa \code{tsoutliers::outliers.effects()} com
#'     parâmetros ARIMA fornecidos em \code{pars}}
#'     \item{\code{"manual"}}{gera efeitos aproximados manualmente}
#'   }
#' @param pars Objeto com parâmetros ARIMA (classe \code{ArimaPars}) necessário
#'   quando \code{method = "effects"}.
#'
#' @param delta Fator de decaimento para outliers do tipo \code{"TC"} no modo
#'   manual (padrão = 0.7).
#' @param keep_types Vetor de tipos de outliers a manter
#'   (padrão: \code{c("AO","LS","TC","IO")}).
#' @param io_policy Política para tratar \code{"IO"} no modo manual:
#'   \describe{
#'     \item{\code{"drop_then_AO"}}{remove IO se houver outros tipos; caso
#'     contrário, converte para AO}
#'     \item{\code{"drop"}}{remove IO}
#'     \item{\code{"AO"}}{converte IO para AO}
#'   }
#'
#' @param max_cols Número máximo de colunas permitido em \code{xreg}
#'   (padrão = 80).
#' @param tol_var Tolerância mínima de variância para manter colunas
#'   (padrão = 0).
#' @param tol_qr Tolerância numérica usada na decomposição QR para checagem de
#'   posto (padrão = \code{1e-10}).
#'
#' @details
#' O procedimento:
#' \enumerate{
#'   \item Filtra linhas válidas e tipos de outliers desejados.
#'   \item Constrói efeitos para toda a extensão \code{n + h}.
#'   \item Separa treino (\code{xreg}) e previsão (\code{newxreg}).
#'   \item Remove colunas sem variância.
#'   \item Limita o número de colunas a \code{max_cols}.
#'   \item Garante posto completo via decomposição QR.
#' }
#'
#' O resultado é seguro para uso em modelos ARIMA com regressoras externas.
#'
#' @return Uma lista com dois elementos:
#' \describe{
#'   \item{\code{xreg}}{matriz numérica de dimensão \code{n x k} para ajuste}
#'   \item{\code{newxreg}}{matriz numérica de dimensão \code{h x k} para previsão}
#' }
#'
#' @examples
#' \dontrun{
#'   mo <- data.frame(t = c(10, 25), type = c("AO", "LS"))
#'   res <- build_xreg_pair(mo, n = 100, h = 12, method = "manual")
#'
#'   xreg    <- res$xreg
#'   newxreg <- res$newxreg
#' }
#'
#' @export
build_xreg_pair <- function(mo, n, h,
                            method = c("effects", "manual"),
                            pars = NULL,
                            # manual:
                            delta = 0.7,
                            keep_types = c("AO","LS","TC","IO"),
                            io_policy = c("drop_then_AO","drop","AO"),
                            # filtros comuns:
                            max_cols = 80,
                            tol_var  = 0,
                            tol_qr   = 1e-10) {
  
  method    <- match.arg(method)
  io_policy <- match.arg(io_policy)
  
  if (is.null(mo)) stop("mo é NULL.")
  if (is.data.frame(mo)) mo <- as.matrix(mo)
  
  if (!all(c("t","type") %in% colnames(mo))) {
    if (ncol(mo) >= 2) colnames(mo)[1:2] <- c("t","type")
  }
  if (!all(c("t","type") %in% colnames(mo))) stop("mo inválido: preciso colunas t/type.")
  
  t_idx <- suppressWarnings(as.integer(mo[, "t"]))
  tp    <- as.character(mo[, "type"])
  
  ok <- is.finite(t_idx) & !is.na(tp) & nzchar(tp)
  t_idx <- t_idx[ok]
  tp    <- tp[ok]
  if (length(t_idx) == 0) stop("mo sem linhas válidas.")
  
  t_idx[t_idx < 1] <- 1L
  t_idx[t_idx > n] <- n
  
  # filtra tipos
  keep <- tp %in% keep_types
  t_idx <- t_idx[keep]
  tp    <- tp[keep]
  if (length(t_idx) == 0) stop("Nenhum outlier após filtrar tipos.")
  
  Ntot <- as.integer(n) + as.integer(h)
  
  if (method == "effects") {
    if (is.null(pars)) stop("method='effects' requer pars (ArimaPars).")
    if (!inherits(pars, "ArimaPars")) class(pars) <- "ArimaPars"
    
    mo_obj  <- tsoutliers::outliers(type = tp, ind = t_idx)  # <- singular: type
    eff_all <- tsoutliers::outliers.effects(mo_obj, n = Ntot, pars = pars)
    eff_all <- as.matrix(eff_all)
    
  } else {
    # method == "manual" (IO nao e bem representado -> politica)
    if (any(tp == "IO")) {
      if (io_policy == "drop") {
        sel <- tp != "IO"
        t_idx <- t_idx[sel]; tp <- tp[sel]
      } else if (io_policy == "AO") {
        tp[tp == "IO"] <- "AO"
      } else {
        # drop_then_AO
        sel <- tp != "IO"
        t_try <- t_idx[sel]; tp_try <- tp[sel]
        if (length(t_try) == 0) {
          tp[tp == "IO"] <- "AO"
        } else {
          t_idx <- t_try; tp <- tp_try
        }
      }
    }
    if (length(t_idx) == 0) stop("Todos os outliers eram IO e foram removidos (manual).")
    
    k <- length(t_idx)
    eff_all <- matrix(0, nrow = Ntot, ncol = k)
    
    for (j in seq_len(k)) {
      tt <- t_idx[j]
      type <- tp[j]
      
      if (type == "AO") {
        eff_all[tt, j] <- 1
      } else if (type == "LS") {
        eff_all[tt:Ntot, j] <- 1
      } else if (type == "TC") {
        eff_all[tt:Ntot, j] <- delta ^ (0:(Ntot - tt))
      } else {
        # fallback: trata como AO
        eff_all[tt, j] <- 1
      }
    }
    
    colnames(eff_all) <- paste0(tp, "_t", t_idx, "_", seq_len(k))
  }
  
  # separa treino / previsao
  xreg    <- eff_all[1:n, , drop = FALSE]
  newxreg <- eff_all[(n+1):(n+h), , drop = FALSE]
  
  storage.mode(xreg)    <- "double"
  storage.mode(newxreg) <- "double"
  
  # remove colunas sem variancia
  v <- apply(xreg, 2, var)
  keep_var <- is.finite(v) & (v > tol_var)
  xreg    <- xreg[, keep_var, drop = FALSE]
  newxreg <- newxreg[, keep_var, drop = FALSE]
  if (ncol(xreg) == 0) stop("xreg ficou vazio após remover colunas sem variância.")
  
  # limita colunas
  if (ncol(xreg) > max_cols) {
    xreg    <- xreg[, seq_len(max_cols), drop = FALSE]
    newxreg <- newxreg[, seq_len(max_cols), drop = FALSE]
  }
  
  # garante posto (QR)
  qrX <- qr(xreg, tol = tol_qr)
  rnk <- qrX$rank
  if (!is.finite(rnk) || rnk < 1) stop("xreg sem posto (rank < 1).")
  
  piv <- qrX$pivot
  keep_qr <- sort(piv[seq_len(rnk)])
  xreg    <- xreg[, keep_qr, drop = FALSE]
  newxreg <- newxreg[, keep_qr, drop = FALSE]
  
  if (is.null(colnames(xreg)) || any(colnames(xreg) == "")) {
    colnames(xreg) <- paste0("out", seq_len(ncol(xreg)))
    colnames(newxreg) <- colnames(xreg)
  }
  
  list(xreg = xreg, newxreg = newxreg)
}