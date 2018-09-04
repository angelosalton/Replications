
# Carrega vários arquivos CSV numa mesma pasta
carrega <- function(extensao = "csv", sep = ";"){
require(readr)  
arqs <- list.files(pattern = extensao)

for(i in arqs){
  assign(i, read_delim(i, delim = sep, escape_double = FALSE, col_names = FALSE, trim_ws = TRUE), envir = .GlobalEnv)
  }
}

#' Transforma uma série mensal pra trimestral.
#'
#' @param serie objeto do tipo `ts`
#' @param funcao pode ser `last`, `mean`, `cumsum`...
#'
#' @return
#' @export
#'
#' @examples
ts.trans <- function(serie, freq = 4, funcao){
  aggregate(serie, nfrequency = freq, FUN = funcao)
}

#' Calcula a variação percentual da série de tempo.
#'
#' @param x objeto do tipo `ts`
#'
#' @return
#' @export
#'
#' @examples
percentual <- function(x){
  y <- x/stats::lag(x,-1) - 1
  y <- y*100
  y
}

#' Gera um índice base 100
#'
#' @param x objeto do tipo `ts`
#' @param base um vetor `integer` que indica quantas observações na base
#'
#' @return
#' @export
#'
#' @examples
indice <- function(x, base=1){
  y <- x/mean(x[base], na.rm = T)*100
  y
}
