#' @title Função Dicionário-Enunciado
#'
#' @name func_labels
#'
#' @description Utilizada para padronizar a entrada do parâmetro DICIONARIO da função
#' FUN_isoladas, é muito útil nos casos em que trabalhamos com dois arquivos distintos,
#' um contendo os lábels das variáveis e outro com o enunciado.
#'
#' @param tabela_labels Tabela contendo o label da variável.
#' @param tabela_enunciado Tabela contendo o enunciado da variável.
#' @param col_codigo Variável presente em \code{tabela_labels} contendo o código do label.
#' @param col_label Variável presente em \code{tabela_labels} contendo os label's da variável.
#' @param col_var Variável presente em \code{tabela_enunciado} e \code{tabela_labels} contendo a variável (Observe que as colunas contendo as variáveis precisam ter o mesmo nome nas duas Tabelas).
#' @param col_enunciado Variável presente em \code{tabela_enunciado} contendo o enunciado da variável.
#'
#'
#' @return Entra-se com duas tabelas e retorna-se apenas uma contedo 4 colunas:
#' \itemize{
#'  \item '\code{opcao_cod}': Coluna com o código do label.
#'  \item \code{opcao_label}: Coluna com os label's da variável.
#'  \item \code{opcao_variavel}: Coluna com a variável.
#'  \item \code{pergunta_enunciado}: Coluna com o enunciado da variável.
#' }
#'
#' @author Felipe Ferreira \email{felipe179971@hotmail.com} e Rafael Peixoto \email{peixoto.rafael@outlook.com}.
#'
#' @examples
#' #Arquivos
#' dados<-PacoteInnovare::Example()[[1]]
#' Label<-PacoteInnovare::Example()[[2]]
#' Enunciado<-PacoteInnovare::Example()[[3]]
#' #Como tenho dois arquivos, um para o enunciado e outro para o dicionário,
#' head(Label)
#' head(Enunciado)
#' #usarei a função func_labels
#'
#' Dicionario=PacoteInnovare::func_labels(tabela_labels=Label,
#'                                        tabela_enunciado=Enunciado,
#'                                        col_codigo="codigo",
#'                                        col_label="label",
#'                                        col_var="variavel",
#'                                        col_enunciado="enunciado")
#' head(Dicionario)
#'
#' @import stringr dplyr
#'
#' @encoding UTF-8
#'
#' @export

func_labels<-function(tabela_labels, tabela_enunciado, col_codigo, col_label, col_var, col_enunciado){
  `%nin%` = Negate(`%in%`)
  # Verificar se a tabela de labels possuem as colunas
  for (col in c(all_of(col_codigo), col_label, col_var)) {

    if (col %nin% names(tabela_labels)) {
      warning(stringr::str_c("Coluna '", col, "' nao presente na tabela de labels"))
    }

  }

  # Verificar se a tabela de enunciado possuem as colunas
  for (col in c(col_enunciado, col_var)) {

    if (col %nin% names(tabela_enunciado)) {
      warning(stringr::str_c("Coluna '", col, "' nao presente na tabela de enunciado"))
    }

  }

  if (any(c(all_of(col_codigo), col_label, col_var) %nin% names(tabela_labels))) {
    stop(stringr::str_c("Parando a funcao. Colunas nao presentes na tabela de labels"))
  }
  if (any(c(col_enunciado, col_var) %nin% names(tabela_enunciado))) {
    stop(stringr::str_c("Parando a funcao. Colunas nao presentes na tabela de enunciado"))
  }

  # Renomear as colunas e juntas as tabelas
  labels <- tabela_labels %>%
    dplyr::rename(
      "opcao_cod" = col_codigo,
      "opcao_label" = col_label,
      "opcao_variavel" = col_var
    )

  enunciado <- tabela_enunciado %>%
    dplyr::rename(
      "pergunta_enunciado" = col_enunciado,
      "opcao_variavel" = col_var
    )

  out <- dplyr::left_join(
    x = labels %>% dplyr::select(all_of(c("opcao_cod", "opcao_label", "opcao_variavel"))),
    y = enunciado %>% dplyr::select(all_of(c("pergunta_enunciado", "opcao_variavel"))),
    by = "opcao_variavel"
  )

  return(out)

}


