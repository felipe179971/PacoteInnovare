#' @title Função de Frequência
#'
#' @name FUN_isoladas
#'
#' @description Essa função calcula a frequência e a porcentagem das variáveis desejadas.
#'
#' @param TABELA Tabela já filtrada pelos splits.
#' @param DICIONARIO Arquivo com o significado dos labels.
#' @param variaveis Quais variáveis serão calculadas.
#' @param adc_labels se TRUE (o padrão) retorna uma coluna conteno o label do split.
#'
#' @return Se \code{adc_labels=TRUE}, essa função retorna um \code{tibble} contendo 6 colunas:
#' \itemize{
#'  \item '\code{variável}': valores possíveis da variável;
#'  \item \code{n}: número de observações da \code{variaveis};
#'  \item \code{n_peso}: número de observações da \code{variaveis} (com peso aplicado);
#'  \item \code{pct}: porcentagem da \code{variaveis};
#'  \item \code{pct_peso}: porcentagem da \code{variaveis} (com peso aplicado);
#'  \item \code{variável_label}: retorna o label do split (só se \code{adc_labels=TRUE}).
#' }
#'
#' @details
#' \itemize{
#'  \item \code{TABELA}: o parâmetro só aceita tabelas no formato \code{Nest Tibble} (splits  x data). Além disso, precisa ter uma coluna chamada “peso” contendo o peso a ser aplicado ao respectivo Nest. Caso nenhum peso precise ser aplicado, atribuir peso=1.
#'  \item \code{DICIONARIO}: o arquivo do dicionário precisa, OBRIGATORIAMENTE, ter no mínimo as seguintes colunas:
#'  \itemize{
#'  \item \code{opcao_cod}: coluna contendo o código.
#'  \item \code{opcao_label}: coluna contendo o significado do código;
#'  \item \code{opcao_variavel}: coluna contendo a variável;
#'  \item \code{pergunta_enunciado}: coluna contendo o enunciado da vaiável.
#'
#'  \itemize{
#'  \item{\code{Caso você tenha dois arquivos separador, sendo um para os labels e outro para o enunciado, ver func_labels().}}
#'  }
#'  }
#'
#'}
#'
#' @author Felipe Ferreira \email{felipe179971@hotmail.com} e Rafael Peixoto \email{peixoto.rafael@outlook.com}.
#'
#' @examples
#'
#' library(tidyverse)
#' #Arquivos
#' dados<-PacoteInnovare::Example()[[1]]
#' Label<-PacoteInnovare::Example()[[2]]
#' Enunciado<-PacoteInnovare::Example()[[3]]
#' ##Fazendo o tratamento para entrar na função
#' #Como tenho dois arquivos, um para o enunciado e outro para o dicionário,
#' #usarei a função func_labels
#' Dicionario=PacoteInnovare::func_labels(tabela_labels=Label,
#'                                        tabela_enunciado=Enunciado,
#'                                        col_codigo="codigo",
#'                                        col_label="label",
#'                                        col_var="variavel",
#'                                        col_enunciado="enunciado")
#' #Identificando nosso splits
#' split_sexo<-Dicionario%>%
#'   dplyr::filter(opcao_variavel=="v1")%>%
#'   dplyr::select("v1"=opcao_cod,"splits"=opcao_label)
#' split_time<-Dicionario%>%
#'   dplyr::filter(opcao_variavel=="v17")%>%
#'   dplyr::select("v17"=opcao_cod,"splits"=opcao_label)
#' split_tipo<-Dicionario%>%
#'   dplyr::filter(opcao_variavel=="v19")%>%
#'   dplyr::select("v19"=opcao_cod,"splits"=opcao_label)
#' #Criando a tabela no formato Nest Tibble dividido pelos splits
#' dados_split<-dplyr::bind_rows(
#'   #Split por sexo, cujo peso está na v4
#'   dados %>%
#'     # Atribuir o peso para a coluna "peso"
#'     dplyr::mutate("peso" = v4) %>%
#'     # Juntar a tabela com os labels dos splits
#'     dplyr::left_join(split_sexo) %>%
#'     # dividir a tabela pelos splits
#'     dplyr::group_nest(splits),
#'   #Adicionando o Split por time, cujo peso está na v18
#'   dados %>%
#'     # Atribuir o peso para a coluna "peso"
#'     dplyr::mutate("peso" = v18) %>%
#'     # Juntar a tabela com os labels dos splits
#'     dplyr::left_join(split_time) %>%
#'     # dividir a tabela pelos splits
#'     dplyr::group_nest(splits),
#'   #Adicionando o Split por tipo (não tem peso)
#'   dados %>%
#'     # Atribuir o peso para a coluna "peso"
#'     dplyr::mutate("peso" = 1) %>%
#'     # Juntar a tabela com os labels dos splits
#'     dplyr::left_join(split_tipo) %>%
#'     # dividir a tabela pelos splits
#'     dplyr::group_nest(splits),
#'   #Adicionando o Geral (não tem peso)
#'   dados%>%
#'     # Atribuir o peso para a coluna "peso"
#'     dplyr::mutate("peso" = 1,splits=as.character("Geral")) %>%
#'     # dividir a tabela pelos splits
#'     dplyr::group_nest(splits)
#' )
#' #Calculando a frequência de "v1" no split "Geral" com label
#' PacoteInnovare::FUN_isoladas(TABELA=dados_split[[2]][[which(dados_split$splits=="Geral")]],
#'                              DICIONARIO=Dicionario,
#'                              variaveis="v1",
#'                              adc_labels = TRUE)
#' #Calculando a frequência de "v1" e "v2" no split "Feminino" sem label
#' PacoteInnovare::FUN_isoladas(TABELA=dados_split[[2]][[which(dados_split$splits=="Feminino")]],
#'                              DICIONARIO=Dicionario,
#'                              variaveis=c("v1","v2"),
#'                              adc_labels = FALSE)
#'
#'
#' @import tidyverse purrr dplyr stringr tibble
#'
#' @encoding UTF-8
#' @export


FUN_isoladas <- function(TABELA, DICIONARIO, variaveis, adc_labels = TRUE) {
  Tempo_Inicio<-Sys.time()
  `%nin%` = Negate(`%in%`)
  out <- vector("list", length = length(variaveis))
  base <- vector("list", length = length(variaveis))
  Log_isoladas<-list()

  #Error
  erro=0

  #Run
  if(erro==0){
    # calcular a tabela de frequência
    for (i in seq_along(out)) {

      var <- variaveis[i]
      labels <- DICIONARIO %>%
        dplyr::filter(opcao_variavel == var) %>%
        dplyr::select(opcao_cod, opcao_label) %>%
        dplyr::rename(!!var := "opcao_cod") %>%
        dplyr::rename(!!str_c(var, "_label") := "opcao_label") %>%
        dplyr::mutate(across(var, as.character)) %>%
        dplyr::arrange(.[[1]] %in% "-88") %>%
        dplyr::arrange(.[[1]] %in% "-99")

      base[[i]] <- TABELA %>%
        dplyr::select(all_of(var), peso) %>%
        dplyr::mutate("n_base" = ifelse(test = rowSums(!is.na(across(all_of(var)))) > 0, 1, 0)) %>%
        dplyr::mutate("n_base" = sum(`n_base`)) %>%
        dplyr::mutate("pct_base" = (100 * `n_base`) / nrow(.)) %>%
        dplyr::mutate("n_base_peso" = ifelse(test = rowSums(!is.na(across(all_of(var)))) > 0, peso, 0)) %>%
        dplyr::mutate("n_base_peso" = sum(`n_base_peso`)) %>%
        dplyr::mutate("pct_base_peso" = (100 * `n_base_peso`) / sum(peso)) %>%
        dplyr::distinct(n_base, pct_base, n_base_peso, pct_base_peso)

      out[[i]] <- TABELA %>%
        dplyr::select(all_of(var), peso) %>%
        #dplyr::mutate(across(all_of(var), ~ factor(., levels = labels[[1]]))) %>%
        dplyr::group_by(across(all_of(var)), .drop = FALSE) %>%
        dplyr::summarise("n" = n(), "n_peso" = sum(peso), .groups = "drop") %>%
        dplyr::filter(!is.na(.[1])) %>%
        dplyr::mutate("pct" = (100 * n / sum(n))) %>%
        dplyr::mutate("pct_peso" = (100 * n_peso / sum(n_peso))) %>%
        dplyr::mutate(across(all_of(var), as.character)) %>%
        full_join(labels,by=var)%>%arrange(desc(.[[var]]))%>%
        purrr::map_df(.f = ~ c(., ifelse(is.numeric(.), sum(., na.rm = TRUE), "Total"))) %>%
        rbind(list("Base", base[[i]]$n_base, base[[i]]$n_base_peso, base[[i]]$pct_base, base[[i]]$pct_base_peso,"Base")) %>%
        dplyr::mutate(across(.cols=c("n","n_peso","pct","pct_peso"),~ifelse(is.na(.x),0,.x)))
      print(paste0(var," [Variavel Simples ",i,"/",length(variaveis),"]"))

      if(any(is.na(out[[i]]%>%dplyr::pull(6)))){

        if (var %nin% c(DICIONARIO %>% dplyr::distinct(opcao_variavel) %>% dplyr::pull()) ) {
          warning(str_c("Variavel ", var, " nao presente no dicionario"))
          if(length(Log_isoladas)==0){Log_isoladas[[1]]<-tibble::tibble(Variavel=var,`Problema`=str_c("Variavel ", var, " nao presente no dicionario"),Status="rodou")}else{Log_isoladas[[1]]<-dplyr::bind_rows(Log_isoladas[[1]],tibble::tibble(Variavel=var,`Problema`=str_c("Variavel ", var, " nao presente no dicionario"),Status="rodou"))  }
        }else{
          msg=str_c("Variavel ", var, " possui label nao presente no dicionario. Codigo(s) [",paste(as.character(out[[i]][which(is.na(out[[i]][,6])),1]%>%dplyr::pull()),collapse = ","),"]")
          warning(msg)
          if(length(Log_isoladas)==0){Log_isoladas[[1]]<-tibble::tibble(Variavel=var,`Problema`=msg,Status="rodou")}else{Log_isoladas[[1]]<-dplyr::bind_rows(Log_isoladas[[1]],tibble::tibble(Variavel=var,`Problema`=msg,Status="rodou"))  }
        }

      }

      if(adc_labels==FALSE){out[[i]]<-out[[i]]%>%dplyr::select(-ncol(.))}

    }

  }


  print(paste0("A funcao levou ",PacoteInnovare::Time_Difference(Sys.time(),Tempo_Inicio)," para calcular as frequencias (variaveis simples)"))
  return(list(Resultado_isoladas=out,Log_isoladas=Log_isoladas))
}
