#' @title Função de Frequência
#'
#' @name FUN_isoladas
#'
#' @description Essa função calcula a frequência e a porcentagem das variáveis desejadas.
#'
#' @param TABELA Tabela já filtrada pelos splits
#' @param DICIONARIO Arquivo com o significado dos labels.
#' @param variaveis Quais variáveis serão calculadas.
#' @param adc_labels se TRUE (o padrão) retorna uma coluna conteno o label do split
#'
#' @return Se \code{adc_labels=TRUE}, essa função retorna um \code{tibble} contendo 6 colunas:
#' \itemize{
#'  \item '\code{variável}': valores possíveis da variável.
#'  \item \code{n}: número de observações da \code{variaveis}.
#'  \item \code{n_peso}: número de observações da \code{variaveis} (com peso aplicado).
#'  \item \code{pct}: porcentagem da \code{variaveis}.
#'  \item \code{pct_peso}: porcentagem da \code{variaveis} (com peso aplicado).
#'  \item \code{variável_label}: Retorna o label do split (só se \code{adc_labels=TRUE}).
#' }
#'
#' @author Felipe Ferreira \email{felipe179971@hotmail.com} e Rafael Peixoto \email{peixoto.rafael@outlook.com}.
#'
#' @examples
#' library(tidyverse)
#' #Criando o data.frame com os dados#########################
#' #Número de linhas
#' n=100
#' #Variável de sexo
#' set.seed(1)
#' v1=sample(1:2,n,replace = TRUE)
#' #variável de renda
#' set.seed(2)
#' v2=sample(1:4,n,replace = TRUE)
#' #Variável da UF
#' set.seed(3)
#' v3=sample(1:10,n,replace = TRUE)
#' #Peso (sexo)
#' v4<-v1
#' v4[which(v4==1)]<-(n/2)/length(v1[which(v1==1)])
#' v4[which(v4==2)]<-(n/2)/length(v1[which(v1==2)])
#'
#' dados<-data.frame(v1,v2,v3,v4)
#' #Criando o Dicionário dos dados#########################
#' Label=data.frame(
#'   id=c(rep(1,length(v1%>%unique())),
#'        rep(2,length(v2%>%unique())),
#'        rep(3,length(v3%>%unique()))),
#'   variavel=c(rep("v1",length(v1%>%unique())),
#'              rep("v2",length(v2%>%unique())),
#'              rep("v3",length(v3%>%unique()))),
#'   codigo=c(v1%>%unique(),v2%>%unique(),v3%>%unique()),
#'   label=c("Masculino","Feminino",
#'           "Baixa Renda","Classe Média","Classe Alta","Milionário",
#'           paste("Cidade",seq(1:10)))
#' )
#' #Tratando para entrar na função#######################
#' dados_peso <- dados %>%
#'   dplyr::mutate("peso" = v4)
#'
#' splits <- dados_peso %>% dplyr::mutate(v1=as.character(v1))%>%
#'   dplyr::distinct(v1) %>%
#'   dplyr::left_join(
#'     y = Label %>%
#'       dplyr::filter(variavel == "v1") %>%
#'       dplyr::select("v1" = codigo, "splits" = label) %>%
#'       dplyr::mutate(across(v1, as.character))
#'   )
#' Dados_splits <- dados_peso %>% dplyr::mutate(v1=as.character(v1))%>%
#'   dplyr::left_join(splits) %>%
#'   dplyr::group_nest(splits) %>%
#'   dplyr::bind_rows(
#'     dados_peso %>% dplyr::mutate(v1=as.character(v1))%>%
#'       dplyr::mutate(splits = "Geral") %>%
#'       dplyr::group_nest(splits)
#'   ) %>%
#'   dplyr::mutate(data = map(data, ~ dplyr::select(., peso, matches("v\\d+"))))
#' Label<-Label%>%dplyr::rename(opcao_variavel="variavel",opcao_cod="codigo",opcao_label="label")
#'
#' #Executando#########################################
#' #Apenas uma variável sem label do split (adc_labels = FALSE)
#' FUN_isoladas(TABELA = dados_peso, DICIONARIO = Label, variaveis = "v1",
#'             adc_labels = FALSE)
#' #Mais de uma variável e com label do split (adc_labels = TRUE)
#' FUN_isoladas(TABELA = dados_peso, DICIONARIO = Label, variaveis = c("v1","v2","v3"),
#'              adc_labels = TRUE)
#' #Pelos splits, com label
#' Dados_splits%>%
#'   dplyr::mutate(data = map(data, .f = ~ FUN_isoladas(., DICIONARIO = Label, variaveis = "v1",
#'    adc_labels = TRUE)))
#'
#'rm(a,dados,dados_peso,Dados_splits,Label,splits,n,v1,v2,v3,v4)%>%suppressWarnings()
#'
#' @import tidyverse Hmisc purrr dplyr stringr
#'
#' @encoding UTF-8
#' @export


FUN_isoladas <- function(TABELA, DICIONARIO, variaveis, adc_labels = T) {

  out <- vector("list", length = length(variaveis))
  base <- vector("list", length = length(variaveis))

  # Verificar se cada variável está no dicionário
  for (i in seq_along(out)) {

    if (variaveis[i] %nin% c(DICIONARIO %>% distinct(opcao_variavel) %>% pull())) {
      warning(str_c("Variavel ", variaveis[i], " nao presente no dicionario"))
    }

  }
  if (any(variaveis %nin% c(DICIONARIO %>% distinct(opcao_variavel) %>% pull()))) {
    stop(str_c("Parando a funcao. Variaveis nao presentes no dicionario"))
  }

  # calcular a tabela de frequência
  for (i in seq_along(out)) {

    var <- variaveis[i]
    labels <- DICIONARIO %>%
      filter(opcao_variavel == var) %>%
      select(opcao_cod, opcao_label) %>%
      rename(!!var := "opcao_cod") %>%
      rename(!!str_c(var, "_label") := "opcao_label") %>%
      mutate(across(var, as.character)) %>%
      arrange(.[[1]] %in% "-88") %>%
      arrange(.[[1]] %in% "-99")

    base[[i]] <- TABELA %>%
      select(all_of(var), peso) %>%
      mutate("n_base" = ifelse(test = rowSums(!is.na(across(all_of(var)))) > 0, 1, 0)) %>%
      mutate("n_base" = sum(`n_base`)) %>%
      mutate("pct_base" = (100 * `n_base`) / nrow(.)) %>%
      mutate("n_base_peso" = ifelse(test = rowSums(!is.na(across(all_of(var)))) > 0, peso, 0)) %>%
      mutate("n_base_peso" = sum(`n_base_peso`)) %>%
      mutate("pct_base_peso" = (100 * `n_base_peso`) / sum(peso)) %>%
      distinct(n_base, pct_base, n_base_peso, pct_base_peso)

    out[[i]] <- TABELA %>%
      select(all_of(var), peso) %>%
      mutate(across(all_of(var), ~ factor(., levels = labels[[1]]))) %>%
      group_by(across(all_of(var)), .drop = F) %>%
      summarise("n" = n(), "n_peso" = sum(peso), .groups = "drop") %>%
      filter(!is.na(.[1])) %>%
      mutate("pct" = (100 * n / sum(n))) %>%
      mutate("pct_peso" = (100 * n_peso / sum(n_peso))) %>%
      mutate(across(all_of(var), as.character)) %>%
      map_df(.f = ~ c(., ifelse(is.numeric(.), sum(., na.rm = TRUE), "Total"))) %>%
      rbind(list("Base", base[[i]]$n_base, base[[i]]$n_base_peso, base[[i]]$pct_base, base[[i]]$pct_base_peso)) %>%
      {
        if (adc_labels) {
          left_join(x = ., y = labels , by = var) %>%
            mutate(across(6, ~ replace(., .data[[var]] == "Total", "Total"))) %>%
            mutate(across(6, ~ replace(., .data[[var]] == "Base", "Base")))
        } else {
          tibble(.)
        }
      }

  }

  return(out)

}
