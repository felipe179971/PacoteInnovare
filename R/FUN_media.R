#' @title Função de Frequência
#'
#' @name FUN_media
#'
#' @description Função apra calcular a média e/ou a média aparada.
#'
#'
#' @param TABELA Tabela já filtrada pelos splits.
#' @param variaveis Variáveis cuja média deverá ser calculada.
#' @param vars_na Código a serem transformados em NA .
#' @param aparada Se quer (TRUE) ou não (FALSE) a média aparada.
#' @param cut Para a média aparada: Porcentagem de corte (Ex.: cut=2.5 para remover os 2.5% menores e 97.5% maiores)
#'
#' @return Se \code{aparada=TRUE}, essa função retorna um \code{tibble} contendo 6 colunas:
#' \itemize{
#'  \item '\code{variavel}': ...
#'  \item \code{media}: ...
#'  \item \code{media_peso}: ...
#'  \item \code{n}: ...
#'  \item \code{n_peso}: ...
#'  \item \code{desvp}: ...
#'  \item \code{erro}: ...
#'  \item \code{erro_peso}: ...
#' }
#'
#' @details
#' \itemize{
#'  \item \code{TABELA}: o parâmetro só aceita tabelas no formato \code{Nest Tibble} (splits  x data). Além disso, precisa ter uma coluna chamada “peso” contendo o peso a ser aplicado ao respectivo Nest. Caso nenhum peso precise ser aplicado, atribuir peso=1.
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
#' ##Calculando a média de v15,v16 e v21 para o split Feminino
#' #(utilziando o padrão de vars_na, aparada e cut)
#' PacoteInnovare::FUN_media(TABELA=dados_split[[2]][[which(dados_split$splits=="Geral")]],
#'                           variaveis=c("v15","v16","v21"))
#' #Transformando apenas o código -88 em NA
#' PacoteInnovare::FUN_media(TABELA=dados_split[[2]][[which(dados_split$splits=="Geral")]],
#'                           variaveis=c("v15","v16","v21"),
#'                           vars_na=c(-88)
#'                           )
#' #Média aparada (2.5%) e nenhum código sendo transformando em NA
#' PacoteInnovare::FUN_media(TABELA=dados_split[[2]][[which(dados_split$splits=="Geral")]],
#'                           variaveis=c("v15","v16","v21"),
#'                           vars_na=NA,
#'                           aparada = TRUE
#' )
#' #Média aparada (3%) com -88,-99 sendo transformando em NA
#' PacoteInnovare::FUN_media(TABELA=dados_split[[2]][[which(dados_split$splits=="Geral")]],
#'                           variaveis=c("v15","v16","v21"),
#'                           aparada = TRUE,
#'                           cut=3
#' )
#'
#'#Observando alguns erros e warnings da função
#' dados_split[[2]][[which(dados_split$splits=="Geral")]]%>%dplyr::mutate(
#'   v15=NA,
#'   v16=ifelse(v16==6,"character",v16),
#'   v21=as.character(v21)
#' )
#' PacoteInnovare::FUN_media(
#'   TABELA=dados_split[[2]][[which(dados_split$splits=="Geral")]]%>%dplyr::mutate(
#'     v15=NA,
#'     v16=ifelse(v16==6,"character",v16),
#'     v21=as.character(v21)
#'     ),
#'   variaveis=c("v15","v16","v21"),
#' )
#' #Erro CUT
#' PacoteInnovare::FUN_media(TABELA=dados_split[[2]][[which(dados_split$splits=="Geral")]],
#'                           variaveis=c("v15","v16","v21"),
#'                           cut=50)
#'
#'
#'
#'
#' @import tidyverse purrr dplyr stringr tibble tidyr Hmisc
#'
#' @encoding UTF-8
#' @export

FUN_media <- function(TABELA, variaveis, vars_na = c(-88, -99), aparada = FALSE, cut = 2.5) {
  Tempo_Inicio<-Sys.time()
  `%nin%` = Negate(`%in%`)

  out <- vector("list", length = length(variaveis))
  Log_media<-list()

  for (i in seq_along(out)) {
    #Error
    erro=0
    #Se a variável não for numérica
    if(!is.numeric(TABELA[[variaveis[i]]])){
      antes<-sum(is.na(TABELA[[variaveis[i]]]))
      TABELA[[variaveis[i]]]<-as.numeric(TABELA[[variaveis[i]]])%>%suppressWarnings()
      depois<-sum(is.na(TABELA[[variaveis[i]]]))
      #Se tenho mais NA's que antes, algum valor virou NA e não numérico. DEU RUIM! (mas se ela for só NA, já tenho um warning pra ela)
      if(depois!=antes){
        msg=stringr::str_c("Variavel ", variaveis[i]," nao e numerica. Na tentativa de forcar a conversao ",depois-antes," NA(s) foram gerados.Por tanto, calculos nao foram executados")
        erro=erro+1
        warning(msg)
        if(length(Log_media)==0){Log_media[[1]]<-tibble::tibble(Variavel=variaveis[i],`Problema`=msg,Status="nao rodou")}else{Log_media[[1]]<-dplyr::bind_rows(Log_media[[1]],tibble::tibble(Variavel=variaveis[i],`Problema`=msg,Status="nao rodou"))  }
      }else{
        #Se ela for 100% NA, já tenho um warning pra ela
        if(sum(is.na(TABELA[[variaveis[i]]]))!=nrow(TABELA)){
          msg=stringr::str_c("Variavel ", variaveis[i]," nao e numerica. Conversao concluida sem a criacao de nenhum NA. Por tanto, calculos foram executados com a variavel convertida")
          warning(msg)
          if(length(Log_media)==0){Log_media[[1]]<-tibble::tibble(Variavel=variaveis[i],`Problema`=msg,Status="rodou")}else{Log_media[[1]]<-dplyr::bind_rows(Log_media[[1]],tibble::tibble(Variavel=variaveis[i],`Problema`=msg,Status="rodou"))  }
          }
        }
    }
    #Se tenho cut>=50, estou tentando excluir mais de 100% dos dados
    if(cut>=50){
      msg=stringr::str_c("Variavel ", variaveis[i],": voce tentou excluir 100% ou mais dos dados. cut deve ser < 50")
      erro=erro+1
      warning(msg)
      if(length(Log_media)==0){Log_media[[1]]<-tibble::tibble(Variavel=variaveis[i],`Problema`=msg,Status="nao rodou")}else{Log_media[[1]]<-dplyr::bind_rows(Log_media[[1]],tibble::tibble(Variavel=variaveis[i],`Problema`=msg,Status="nao rodou"))  }
    }


    #Run
    if(erro==0){
      out[[i]] <- TABELA %>%
        dplyr::select(peso, variaveis[i]) %>%
        dplyr::mutate(dplyr::across(2, ~ replace(., . %in% vars_na, NA))) %>%
        {
          if (aparada) {
            tibble::tibble(.) %>%
              dplyr::filter(dplyr::ntile(.[2], 100) <= (100 - cut)) %>%
              dplyr::filter(dplyr::ntile(.[2], 100) >= cut)
          } else {
            tibble::tibble(.) %>%
              dplyr::filter(!is.na(.[2]))
          }
        } %>%
        # summarise(across(2, ~ weighted.mean(., w = peso, na.rm = T))) %>%
        dplyr::summarise(
          "media" = mean(x = .[[2]], na.rm = TRUE),
          "media_peso" = stats::weighted.mean(x = .[[2]], w = .[[1]], na.rm = TRUE),
          "n" = n(),
          "n_peso" = sum(peso),
          "desvp" = stats::sd(x = .[[2]], na.rm = TRUE),
          "desvp_peso" = sqrt(Hmisc::wtd.var(x = .[[2]], weights = .[[1]], na.rm = TRUE))%>%suppressWarnings(),
          "erro" = desvp/sqrt(n),
          "erro_peso" = desvp_peso/sqrt(n)
        ) %>%
        dplyr::mutate("variavel" = variaveis[i], .before = 1) %>%
        {
          if (aparada) {
            tibble::tibble(.) %>%
              dplyr::mutate(dplyr::across("variavel", ~ stringr::str_replace(., "(v\\d+).*", "\\1ma")))
          } else {
            tibble::tibble(.) %>%
              dplyr::mutate(dplyr::across("variavel", ~ stringr::str_replace(., "(v\\d+).*", "\\1m")))
          }
        }%>%
        dplyr::mutate(dplyr::across(.cols=c("media","media_peso","n","n_peso","desvp","desvp_peso","erro","erro_peso"),~ifelse(is.na(.x),0,.x)))
    }
   #Se a variável só tem NA
    if(sum(is.na(TABELA[[variaveis[i]]]))==nrow(TABELA) ){
      msg=stringr::str_c("Variavel ", variaveis[i]," so tem NA")
      warning(msg)
      if(length(Log_media)==0){Log_media[[1]]<-tibble::tibble(Variavel=variaveis[i],`Problema`=msg,Status="rodou")}else{Log_media[[1]]<-dplyr::bind_rows(Log_media[[1]],tibble::tibble(Variavel=variaveis[i],`Problema`=msg,Status="rodou"))  }
    }
  #Se vars_na for diferente de NA, ver quantos foram convertidos em NA
    if(all(!is.na(vars_na))){
      if(erro==0){if(sum(is.na(TABELA[[variaveis[i]]]))!=nrow(TABELA)){ #Se for isso, já tenho warnings pra ele
        msg=stringr::str_c("Variavel ", variaveis[i],": ",sum(TABELA[[variaveis[i]]]%in%vars_na)," codigo(s) ",paste(vars_na,collapse = " ")," (vars_na) foram transformado(s) em NA")
        warning(msg)
        if(length(Log_media)==0){Log_media[[1]]<-tibble::tibble(Variavel=variaveis[i],`Problema`=msg,Status="rodou")}else{Log_media[[1]]<-dplyr::bind_rows(Log_media[[1]],tibble::tibble(Variavel=variaveis[i],`Problema`=msg,Status="rodou"))  }
      }}
    }
    print(paste0(variaveis[i]," [Variavel media ",i,"/",length(variaveis),"]"))

  }
  print(paste0("A funcao levou ",PacoteInnovare::Time_Difference(Sys.time(),Tempo_Inicio)," para calcular as frequencias (Medias)"))

  return(list(Resultado_media=dplyr::bind_rows(out),Log_media=Log_media))
}

