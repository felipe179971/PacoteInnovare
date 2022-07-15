#' @title func_labels
#'
#' @name func_labels
#'
#' @description Calcula a diferença entre dois vetores do tipo \code{Sys.time}.
#' Normalmente utilizada para avaliar o tempo de execução das demais funções do
#' \code{PaceteInnovare}, é internalizada em diversas funções do pacote mas pode ser utilizada livremente.
#'
#' @param tabela_labels ...
#' @param tabela_enunciado ...
#' @param col_codigo ...
#' @param col_label ...
#' @param col_var ...
#' @param col_enunciado ...
#'
#'
#' @return ...
#'
#' @author Felipe Ferreira \email{felipe179971@hotmail.com} e Rafael Peixoto \email{peixoto.rafael@outlook.com}.
#'
#'
#' @import stringr dplyr
#'
#' @encoding UTF-8
#'
#' @export

func_labels<-function(tabela_labels, tabela_enunciado, col_codigo, col_label, col_var, col_enunciado){
  `%nin%` = Negate(`%in%`)
  # Verificar se a tabela de labels possuem as colunas
  for (col in c(col_codigo, col_label, col_var)) {

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

  if (any(c(col_codigo, col_label, col_var) %nin% names(tabela_labels))) {
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


