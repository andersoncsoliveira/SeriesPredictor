#' Executar uma expressão com tratamento seguro de erro
#'
#' Avalia uma expressão e intercepta erros via \code{tryCatch()}, permitindo
#' escolher entre notificar o usuário (via Shiny), silenciar o erro ou parar
#' a execução.
#'
#' Quando \code{on_error = "notify"}, a função tenta exibir uma notificação com
#' \code{shiny::showNotification()} (se o pacote \pkg{shiny} estiver disponível).
#' Em qualquer modo de erro, o retorno em caso de falha é \code{NULL}, exceto
#' quando \code{on_error = "stop"}, em que o erro é propagado.
#'
#' @param expr Expressão a ser avaliada (por exemplo, uma chamada de função).
#'   Deve ser passada sem aspas.
#' @param on_error Estratégia em caso de erro:
#'   \describe{
#'     \item{\code{"notify"}}{exibe notificação no Shiny (se disponível) e retorna \code{NULL}}
#'     \item{\code{"stop"}}{propaga o erro (interrompe a execução)}
#'     \item{\code{"silent"}}{retorna \code{NULL} sem notificar}
#'   }
#' @param title Título/prefixo da mensagem de erro exibida (padrão: \code{"Erro"}).
#' @param session Sessão Shiny (não utilizada atualmente; mantida para compatibilidade
#'   futura com APIs que exigem \code{session}).
#' @param duration Duração (em segundos) da notificação Shiny quando
#'   \code{on_error = "notify"} (padrão: \code{10}).
#'
#' @return O resultado de \code{expr} se não houver erro; caso contrário,
#'   \code{NULL} (nos modos \code{"notify"} e \code{"silent"}).
#'   No modo \code{"stop"}, a função interrompe com erro.
#'
#' @examples
#' \dontrun{
#'   # Exemplo 1: notificar no Shiny e seguir o fluxo
#'   val <- safe_do_call({
#'     stop("Falhou!")
#'   }, on_error = "notify", title = "Ops")
#'   # val == NULL
#'
#'   # Exemplo 2: silenciar
#'   val2 <- safe_do_call(stop("Falhou!"), on_error = "silent")
#'
#'   # Exemplo 3: parar
#'   safe_do_call(stop("Falhou!"), on_error = "stop")
#' }
#'
#' @export
safe_do_call <- function(expr,
                         on_error = c("notify", "stop", "silent"),
                         title = "Erro",
                         session = NULL,
                         duration = 10) {
  on_error <- match.arg(on_error)
  
  tryCatch(
    expr,
    error = function(e) {
      msg <- paste0(title, ":\n", conditionMessage(e))
      
      if (on_error == "notify") {
        if (requireNamespace("shiny", quietly = TRUE)) {
          shiny::showNotification(msg, type = "error", duration = duration)
        }
        return(NULL)
      }
      
      if (on_error == "silent") return(NULL)
      
      stop(e)
    }
  )
}