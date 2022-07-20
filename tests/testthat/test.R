#Número de linhas
n=100
#Variável de sexo
set.seed(1)
v1=sample(1:2,n,replace = T)
#variável de renda
set.seed(2)
v2=sample(1:4,n,replace = T)
#Variável da UF
set.seed(3)
v3=sample(1:10,n,replace = T)
#Peso (sexo)
v4<-v1
v4[which(v4==1)]<-(n/2)/length(v1[which(v1==1)])
v4[which(v4==2)]<-(n/2)/length(v1[which(v1==2)])

dados<-data.frame(v1,v2,v3,v4)
#Criando o Dicionário dos dados#########################
Label=data.frame(
  id=c(rep(1,length(v1%>%unique())),
       rep(2,length(v2%>%unique())),
       rep(3,length(v3%>%unique()))),
  variavel=c(rep("v1",length(v1%>%unique())),
             rep("v2",length(v2%>%unique())),
             rep("v3",length(v3%>%unique()))),
  codigo=c(v1%>%unique(),v2%>%unique(),v3%>%unique()),
  label=c("Masculino","Feminino",
          "Baixa Renda","Classe Média","Classe Alta","Milionário",
          paste("Cidade",seq(1:10)))
)
#Tratando para entrar na função#######################
dados_peso <- dados %>%
  dplyr::mutate("peso" = v4)

splits <- dados_peso %>% dplyr::mutate(v1=as.character(v1))%>%
  dplyr::distinct(v1) %>%
  dplyr::left_join(
    y = Label %>%
      dplyr::filter(variavel == "v1") %>%
      dplyr::select("v1" = codigo, "splits" = label) %>%
      dplyr::mutate(dplyr::across(v1, as.character))
  )
Dados_splits <- dados_peso %>% dplyr::mutate(v1=as.character(v1))%>%
  dplyr::left_join(splits) %>%
  dplyr::group_nest(splits) %>%
  dplyr::bind_rows(
    dados_peso %>% dplyr::mutate(v1=as.character(v1))%>%
      dplyr::mutate(splits = "Geral") %>%
      dplyr::group_nest(splits)
  ) %>%
  dplyr::mutate(data = map(data, ~ dplyr::select(., peso, matches("v\\d+"))))
Label<-Label%>%dplyr::rename(opcao_variavel="variavel",opcao_cod="codigo",opcao_label="label")

#Executando#########################################
#Testando se faz a frequencia
a<-PacoteInnovare::FUN_isoladas(TABELA = dados_peso, DICIONARIO = Label, variaveis = "v1", adc_labels = FALSE)
testthat::expect_equal(all(is.na(a[[2]][[1]])),TRUE)
a<-a[[1]]%>%data.frame()
testthat::expect_equal(nrow(a),4);testthat::expect_equal(ncol(a),5);testthat::expect_equal(colnames(a),c("v1","n","n_peso","pct","pct_peso"))
testthat::expect_equal(a[,1],c("2","1","Total","Base"));testthat::expect_equal(a[,2],c(51,49,100,100));testthat::expect_equal(a[,3],c(50,50,100,100));testthat::expect_equal(a[,4],c(51,49,100,100));testthat::expect_equal(a[,5],c(50,50,100,100))
#Testando o parâmetro adc_labels
a<-PacoteInnovare::FUN_isoladas(TABELA = dados_peso, DICIONARIO = Label, variaveis = "v1", adc_labels = TRUE)
testthat::expect_equal(all(is.na(a[[2]][[1]])),TRUE)
a<-a[[1]]%>%data.frame()
testthat::expect_equal(nrow(a),4);testthat::expect_equal(ncol(a),6);testthat::expect_equal(colnames(a),c("v1","n","n_peso","pct","pct_peso","v1_label"))
testthat::expect_equal(a[,1],c("2","1","Total","Base"));testthat::expect_equal(a[,2],c(51,49,100,100));testthat::expect_equal(a[,3],c(50,50,100,100));testthat::expect_equal(a[,4],c(51,49,100,100));testthat::expect_equal(a[,5],c(50,50,100,100));testthat::expect_equal(a[,6],c("Feminino","Masculino","Total","Base"))

rm(a,dados,dados_peso,Dados_splits,Label,splits,n,v1,v2,v3,v4)%>%suppressWarnings()

###################Time_Difference##############################################
testthat::expect_equal(PacoteInnovare::Time_Difference("2022-07-14 17:00:53 -03","2022-07-14 16:00:53 -03"),"1:0:0")
testthat::expect_equal(PacoteInnovare::Time_Difference("2022-07-14 17:00:53 -03","2022-07-14 16:00:00 -03"),"1:0:53")
testthat::expect_equal(PacoteInnovare::Time_Difference("2022-07-14 17:00:00 -03","2022-07-14 16:50:00 -03"),"0:10:0")
testthat::expect_equal(PacoteInnovare::Time_Difference("2022-07-14 17:00:00 -03","2022-07-15 16:52:10 -03"),"-23:23:13")

#######################func_labels##############################################
Label<-PacoteInnovare::Example()[[2]]
Enunciado<-PacoteInnovare::Example()[[3]]
Dicionario=PacoteInnovare::func_labels(tabela_labels=Label,
                                      tabela_enunciado=Enunciado,
                                      col_codigo="codigo",
                                      col_label="label",
                                      col_var="variavel",
                                      col_enunciado="enunciado")
testthat::expect_equal(colnames(Dicionario),c("opcao_cod","opcao_label","opcao_variavel","pergunta_enunciado") )
testthat::expect_equal(sum(is.na(Label)),0)
testthat::expect_equal(sum(is.na(Enunciado)),0)
testthat::expect_equal(sum(is.na(Dicionario)),0)

################################################################################
#######################Usando o exemplo#########################################
################################################################################
dados<-PacoteInnovare::Example()[[1]]
Label<-PacoteInnovare::Example()[[2]]
Enunciado<-PacoteInnovare::Example()[[3]]
##Fazendo o tratamento para entrar na função
#Como tenho dois arquivos, um para o enunciado e outro para o dicionário,
#usarei a função func_labels
Dicionario=PacoteInnovare::func_labels(tabela_labels=Label,
                                       tabela_enunciado=Enunciado,
                                       col_codigo="codigo",
                                       col_label="label",
                                       col_var="variavel",
                                       col_enunciado="enunciado")
#Identificando nosso splits
split_sexo<-Dicionario%>%
  dplyr::filter(opcao_variavel=="v1")%>%
  dplyr::select("v1"=opcao_cod,"splits"=opcao_label)
split_time<-Dicionario%>%
  dplyr::filter(opcao_variavel=="v17")%>%
  dplyr::select("v17"=opcao_cod,"splits"=opcao_label)
split_tipo<-Dicionario%>%
  dplyr::filter(opcao_variavel=="v19")%>%
  dplyr::select("v19"=opcao_cod,"splits"=opcao_label)
#Criando a tabela no formato Nest Tibble dividido pelos splits
dados_split<-dplyr::bind_rows(
  #Split por sexo, cujo peso está na v4
  dados %>%
    # Atribuir o peso para a coluna "peso"
    dplyr::mutate("peso" = v4) %>%
    # Juntar a tabela com os labels dos splits
    dplyr::left_join(split_sexo) %>%
    # dividir a tabela pelos splits
    dplyr::group_nest(splits),
  #Adicionando o Split por time, cujo peso está na v18
  dados %>%
    # Atribuir o peso para a coluna "peso"
    dplyr::mutate("peso" = v18) %>%
    # Juntar a tabela com os labels dos splits
    dplyr::left_join(split_time) %>%
    # dividir a tabela pelos splits
    dplyr::group_nest(splits),
  #Adicionando o Split por tipo (não tem peso)
  dados %>%
    # Atribuir o peso para a coluna "peso"
    dplyr::mutate("peso" = 1) %>%
    # Juntar a tabela com os labels dos splits
    dplyr::left_join(split_tipo) %>%
    # dividir a tabela pelos splits
    dplyr::group_nest(splits),
  #Adicionando o Geral (não tem peso)
  dados%>%
    # Atribuir o peso para a coluna "peso"
    dplyr::mutate("peso" = 1,splits=as.character("Geral")) %>%
    # dividir a tabela pelos splits
    dplyr::group_nest(splits)
)
#####################################MRG########################################
mrg_V13<-function(nome_split,dadoss_split,pesoo,Dicionarioo){
  dadoss<-dadoss_split[[2]][[which(dadoss_split$splits==nome_split)]]
  a<-PacoteInnovare::FUN_MRG(TABELA=dadoss,
                             DICIONARIO=Dicionarioo,
                             lista_variaveis=list("MRG13"=c("v13","v14")),
                             adc_labels = TRUE)
  testthat::expect_equal(all(is.na(a[[2]][[1]])),TRUE)
  a<-a[[1]]

  testthat::expect_equal(colnames(a[[1]]),c("v13mrg","n","n_peso","pct","pct_peso","v13mrg_label"))
  testthat::expect_equal(a[[1]][["v13mrg_label"]],c("MRG valor 2","MRG valor 1","Total","Base"))
  testthat::expect_equal(a[[1]][["v13mrg"]],c("2","1","Total","Base"))

  n_sem_peso2<-dadoss%>%tidyr::pivot_longer(cols=c(v13,v14))%>%dplyr::filter(value==2)%>%dplyr::count(value,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_sem_peso2<-ifelse(is.na(n_sem_peso2),0,n_sem_peso2)
  n_sem_peso1<-dadoss%>%tidyr::pivot_longer(cols=c(v13,v14))%>%dplyr::filter(value==1)%>%dplyr::count(value,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_sem_peso1<-ifelse(is.na(n_sem_peso1),0,n_sem_peso1)
  n_sem_peso12<-n_sem_peso1+n_sem_peso2;n_sem_peso12<-ifelse(is.na(n_sem_peso12),0,n_sem_peso12)
  n_com_peso2<-dadoss%>%tidyr::pivot_longer(cols=c(v13,v14))%>%dplyr::filter(value==2)%>%dplyr::count(value,wt=peso,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_com_peso2<-ifelse(is.na(n_com_peso2),0,n_com_peso2)
  n_com_peso1<-dadoss%>%tidyr::pivot_longer(cols=c(v13,v14))%>%dplyr::filter(value==1)%>%dplyr::count(value,wt=peso,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_com_peso1<-ifelse(is.na(n_com_peso1),0,n_com_peso1)
  n_com_peso12<-n_com_peso2+n_com_peso1;n_com_peso12<-ifelse(is.na(n_com_peso12),0,n_com_peso12)

  pct_sem_peso2=n_sem_peso2/n_sem_peso12*100;pct_sem_peso2<-ifelse(is.na(pct_sem_peso2),0,pct_sem_peso2)
  pct_com_peso2=n_com_peso2/n_com_peso12*100;pct_com_peso2<-ifelse(is.na(pct_com_peso2),0,pct_com_peso2)
  pct_sem_peso1=n_sem_peso1/n_sem_peso12*100;pct_sem_peso1<-ifelse(is.na(pct_sem_peso1),0,pct_sem_peso1)
  pct_com_peso1=n_com_peso1/n_com_peso12*100;pct_com_peso1<-ifelse(is.na(pct_com_peso1),0,pct_com_peso1)
  pct_sem_peso12=pct_sem_peso2+pct_sem_peso1;pct_sem_peso12<-ifelse(is.na(pct_sem_peso12),0,pct_sem_peso12)
  pct_com_peso12=pct_com_peso2+pct_com_peso1;pct_com_peso12<-ifelse(is.na(pct_com_peso12),0,pct_com_peso12)

  base_sem_peso<-dadoss%>%dplyr::mutate(base=ifelse(!is.na(dadoss$v13) | !is.na(dadoss$v14),1,NA))%>%dplyr::summarise(n=sum(base,na.rm=TRUE))%>%dplyr::select(n)%>%as.numeric();base_sem_peso<-ifelse(is.na(base_sem_peso),0,base_sem_peso)
  base_com_peso<-dadoss%>%dplyr::mutate(base=ifelse(!is.na(dadoss$v13) | !is.na(dadoss$v14),peso,NA))%>%dplyr::summarise(n=sum(base,na.rm=TRUE))%>%dplyr::select(n)%>%as.numeric();base_com_peso<-ifelse(is.na(base_com_peso),0,base_com_peso)
  pct_base_bem_peso=base_sem_peso/nrow(dadoss)*100;pct_base_bem_peso<-ifelse(is.na(pct_base_bem_peso),0,pct_base_bem_peso)
  pct_base_com_peso=base_com_peso/dadoss%>%dplyr::summarise(sum(peso,na.rm=TRUE))%>%as.numeric()*100;pct_base_com_peso<-ifelse(is.na(pct_base_com_peso),0,pct_base_com_peso)

  testthat::expect_equal(a[[1]][["n"]],c(n_sem_peso2,n_sem_peso1,n_sem_peso12,base_sem_peso))
  testthat::expect_equal(a[[1]][["n_peso"]],c(n_com_peso2,n_com_peso1,n_com_peso12,base_com_peso))
  testthat::expect_equal(a[[1]][["pct"]],c(pct_sem_peso2,pct_sem_peso1,pct_sem_peso12,pct_base_bem_peso))
  testthat::expect_equal(a[[1]][["pct_peso"]],c(pct_com_peso2,pct_com_peso1,pct_com_peso12,pct_base_com_peso))


  dadoss<-dadoss_split[[2]][[which(dadoss_split$splits==nome_split)]]
  a<-PacoteInnovare::FUN_MRG(TABELA=dadoss,
                             DICIONARIO=Dicionarioo,
                             lista_variaveis=list("MRG13"=c("v13","v14")),
                             adc_labels = FALSE)
  testthat::expect_equal(all(is.na(a[[2]][[1]])),TRUE)
  a<-a[[1]]

  testthat::expect_equal(colnames(a[[1]]),c("v13mrg","n","n_peso","pct","pct_peso"))
  testthat::expect_equal(a[[1]][["v13mrg"]],c("2","1","Total","Base"))
}
mrg_V13_com_codigo_a_mais<-function(nome_split,dadoss_split,pesoo,Dicionarioo){
  dadoss<-dadoss_split[[2]][[which(dadoss_split$splits==nome_split)]]
  a<-PacoteInnovare::FUN_MRG(TABELA=dadoss,
                             DICIONARIO=Dicionarioo,
                             lista_variaveis=list("MRG13"=c("v13","v14")),
                             adc_labels = TRUE)

  testthat::expect_equal(length(a[[2]]),1)
  a<-a[[1]]

  testthat::expect_equal(colnames(a[[1]]),c("v13mrg","n","n_peso","pct","pct_peso","v13mrg_label"))
  testthat::expect_equal(a[[1]][["v13mrg_label"]],c(NA,"MRG valor 2","MRG valor 1","Total","Base"))
  testthat::expect_equal(a[[1]][["v13mrg"]],c("3","2","1","Total","Base"))

  n_sem_peso3<-dadoss%>%tidyr::pivot_longer(cols=c(v13,v14))%>%dplyr::filter(value==3)%>%dplyr::count(value,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_sem_peso3<-ifelse(is.na(n_sem_peso3),0,n_sem_peso3)
  n_sem_peso2<-dadoss%>%tidyr::pivot_longer(cols=c(v13,v14))%>%dplyr::filter(value==2)%>%dplyr::count(value,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_sem_peso2<-ifelse(is.na(n_sem_peso2),0,n_sem_peso2)
  n_sem_peso1<-dadoss%>%tidyr::pivot_longer(cols=c(v13,v14))%>%dplyr::filter(value==1)%>%dplyr::count(value,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_sem_peso1<-ifelse(is.na(n_sem_peso1),0,n_sem_peso1)
  n_sem_peso123<-n_sem_peso1+n_sem_peso2+n_sem_peso3;n_sem_peso123<-ifelse(is.na(n_sem_peso123),0,n_sem_peso123)
  n_com_peso3<-dadoss%>%tidyr::pivot_longer(cols=c(v13,v14))%>%dplyr::filter(value==3)%>%dplyr::count(value,wt=peso,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_com_peso3<-ifelse(is.na(n_com_peso3),0,n_com_peso3)
  n_com_peso2<-dadoss%>%tidyr::pivot_longer(cols=c(v13,v14))%>%dplyr::filter(value==2)%>%dplyr::count(value,wt=peso,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_com_peso2<-ifelse(is.na(n_com_peso2),0,n_com_peso2)
  n_com_peso1<-dadoss%>%tidyr::pivot_longer(cols=c(v13,v14))%>%dplyr::filter(value==1)%>%dplyr::count(value,wt=peso,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_com_peso1<-ifelse(is.na(n_com_peso1),0,n_com_peso1)
  n_com_peso123<-n_com_peso3+n_com_peso2+n_com_peso1;n_com_peso123<-ifelse(is.na(n_com_peso123),0,n_com_peso123)

  pct_sem_peso3=n_sem_peso3/n_sem_peso123*100;pct_sem_peso3<-ifelse(is.na(pct_sem_peso3),0,pct_sem_peso3)
  pct_com_peso3=n_com_peso3/n_com_peso123*100;pct_com_peso3<-ifelse(is.na(pct_com_peso3),0,pct_com_peso3)
  pct_sem_peso2=n_sem_peso2/n_sem_peso123*100;pct_sem_peso2<-ifelse(is.na(pct_sem_peso2),0,pct_sem_peso2)
  pct_com_peso2=n_com_peso2/n_com_peso123*100;pct_com_peso2<-ifelse(is.na(pct_com_peso2),0,pct_com_peso2)
  pct_sem_peso1=n_sem_peso1/n_sem_peso123*100;pct_sem_peso1<-ifelse(is.na(pct_sem_peso1),0,pct_sem_peso1)
  pct_com_peso1=n_com_peso1/n_com_peso123*100;pct_com_peso1<-ifelse(is.na(pct_com_peso1),0,pct_com_peso1)
  pct_sem_peso123=pct_sem_peso3+pct_sem_peso2+pct_sem_peso1;pct_sem_peso123<-ifelse(is.na(pct_sem_peso123),0,pct_sem_peso123)
  pct_com_peso123=pct_com_peso3+pct_com_peso2+pct_com_peso1;pct_com_peso123<-ifelse(is.na(pct_com_peso123),0,pct_com_peso123)

  base_sem_peso<-dadoss%>%dplyr::mutate(base=ifelse(!is.na(dadoss$v13) | !is.na(dadoss$v14),1,NA))%>%dplyr::summarise(n=sum(base,na.rm=TRUE))%>%dplyr::select(n)%>%as.numeric();base_sem_peso<-ifelse(is.na(base_sem_peso),0,base_sem_peso)
  base_com_peso<-dadoss%>%dplyr::mutate(base=ifelse(!is.na(dadoss$v13) | !is.na(dadoss$v14),peso,NA))%>%dplyr::summarise(n=sum(base,na.rm=TRUE))%>%dplyr::select(n)%>%as.numeric();base_com_peso<-ifelse(is.na(base_com_peso),0,base_com_peso)
  pct_base_bem_peso=base_sem_peso/nrow(dadoss)*100;pct_base_bem_peso<-ifelse(is.na(pct_base_bem_peso),0,pct_base_bem_peso)
  pct_base_com_peso=base_com_peso/dadoss%>%dplyr::summarise(sum(peso,na.rm=TRUE))%>%as.numeric()*100;pct_base_com_peso<-ifelse(is.na(pct_base_com_peso),0,pct_base_com_peso)

  testthat::expect_equal(a[[1]][["n"]],c(n_sem_peso3,n_sem_peso2,n_sem_peso1,n_sem_peso123,base_sem_peso))
  testthat::expect_equal(a[[1]][["n_peso"]],c(n_com_peso3,n_com_peso2,n_com_peso1,n_com_peso123,base_com_peso))
  testthat::expect_equal(a[[1]][["pct"]],c(pct_sem_peso3,pct_sem_peso2,pct_sem_peso1,pct_sem_peso123,pct_base_bem_peso))
  testthat::expect_equal(a[[1]][["pct_peso"]],c(pct_com_peso3,pct_com_peso2,pct_com_peso1,pct_com_peso123,pct_base_com_peso))


  dadoss<-dadoss_split[[2]][[which(dadoss_split$splits==nome_split)]]
  a<-PacoteInnovare::FUN_MRG(TABELA=dadoss,
                             DICIONARIO=Dicionarioo,
                             lista_variaveis=list("MRG13"=c("v13","v14")),
                             adc_labels = FALSE)
  testthat::expect_equal(length(a[[2]]),1)
  a<-a[[1]]

  testthat::expect_equal(colnames(a[[1]]),c("v13mrg","n","n_peso","pct","pct_peso"))
  testthat::expect_equal(a[[1]][["v13mrg"]],c("3","2","1","Total","Base"))

}
fazer<-dados_split[[1]]
#Mundo perfeiro
for(i in sample(1:length(fazer),3,replace = FALSE) ){
  mrg_V13(nome_split=fazer[[i]],dadoss_split=dados_split,Dicionarioo=Dicionario)
}
#Alguns NA's
for(i in sample(1:length(fazer),3,replace = FALSE)){
  mrg_V13(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v13", ~ ifelse(. == 2, NA, .))))),Dicionarioo=Dicionario)
  mrg_V13(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v13", ~ ifelse(. == 2, NA, .)))))%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v14", ~ ifelse(. == 2, NA, .))))),Dicionarioo=Dicionario )
  mrg_V13(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v13", ~ ifelse(. %in%c(1,2), NA, .)))))%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v14", ~ ifelse(. %in%c(1,2), NA, .))))),Dicionarioo=Dicionario )
}
#Código que não tem no dicionário
for(i in sample(1:length(fazer),3,replace = FALSE)){
  mrg_V13_com_codigo_a_mais(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v13", ~ ifelse(. == 2, 3, .))))),Dicionarioo=Dicionario)
  mrg_V13_com_codigo_a_mais(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v13", ~ ifelse(. == 2, 3, .)))))%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v14", ~ ifelse(. == 2, 3, .))))),Dicionarioo=Dicionario )
  mrg_V13_com_codigo_a_mais(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v13", ~ ifelse(. %in%c(1,2), 3, .)))))%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v14", ~ ifelse(. %in%c(1,2), 3, .))))),Dicionarioo=Dicionario )
  mrg_V13_com_codigo_a_mais(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v13", ~ ifelse(. %in%c(1,2), NA, .)))))%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v14", ~ ifelse(. ==1, 3, NA ))))),Dicionarioo=Dicionario )
  mrg_V13_com_codigo_a_mais(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v13", ~ ifelse(. ==1, 3, NA)))))%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v14", ~ ifelse(. ==1, 3, NA ))))),Dicionarioo=Dicionario )
  #Com 2 códigos
  nome_split=fazer[[i]];dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v13", ~ ifelse(. == 2, 3, .)))))%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v14", ~ ifelse(. == 2, 4, .)))));Dicionarioo=Dicionario

  dadoss<-dadoss_split[[2]][[which(dadoss_split$splits==nome_split)]]
  a<-PacoteInnovare::FUN_MRG(TABELA=dadoss,
                             DICIONARIO=Dicionarioo,
                             lista_variaveis=list("MRG13"=c("v13","v14")),
                             adc_labels = TRUE)
  testthat::expect_equal(as.character(a[[2]][[1]])[1],"v13mrg")
  testthat::expect_equal(as.character(a[[2]][[1]])[2],"Variavel v13mrg possui label nao presente no dicionario. Codigo(s) [4,3]")
  testthat::expect_equal(as.character(a[[2]][[1]])[3],"rodou")
  a<-a[[1]]
  testthat::expect_equal(colnames(a[[1]]),c("v13mrg","n","n_peso","pct","pct_peso","v13mrg_label"))
  testthat::expect_equal(a[[1]][["v13mrg_label"]],c(NA,NA,"MRG valor 2","MRG valor 1","Total","Base"))
  testthat::expect_equal(a[[1]][["v13mrg"]],c("4","3","2","1","Total","Base"))
  rm(dadoss,a,nome_split,dadoss_split,Dicionarioo)%>%suppressWarnings()
}

#Sem estar no dicionário
for(i in sample(1:length(fazer),3,replace = FALSE)){
  nome_split=fazer[[i]];dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v13", ~ ifelse(. ==1, 3, NA)))));Dicionarioo=Dicionario%>%dplyr::mutate(opcao_variavel=ifelse(opcao_variavel=="v13mrg","v15mrg",opcao_variavel))

  dadoss<-dadoss_split[[2]][[which(dadoss_split$splits==nome_split)]]
  a<-PacoteInnovare::FUN_MRG(TABELA=dadoss,
                             DICIONARIO=Dicionarioo,
                             lista_variaveis=list("MRG13"=c("v13","v14")),
                             adc_labels = TRUE)
  testthat::expect_equal(colnames(a[[1]][[1]]),c("v13mrg","n","n_peso","pct","pct_peso","v13mrg_label"))
  testthat::expect_equal(a[[1]][[1]][["v13mrg_label"]],c(NA,NA,NA,"Total","Base"))
  testthat::expect_equal(a[[1]][[1]][["v13mrg"]],c("3","2","1","Total","Base"))

  testthat::expect_equal(as.character(a[[2]][[1]])[1],"v13mrg")
  testthat::expect_equal(as.character(a[[2]][[1]])[2],"Variavel v13mrg nao presente no dicionario")
  testthat::expect_equal(as.character(a[[2]][[1]])[3],"rodou")

  dadoss<-dadoss_split[[2]][[which(dadoss_split$splits==nome_split)]]%>%dplyr::mutate(v15=v13,v16=v14)
  a<-PacoteInnovare::FUN_MRG(TABELA=dadoss,
                             DICIONARIO=Dicionarioo,
                             lista_variaveis=list("MRG13"=c("v13","v14"),"MRG15"=c("v15","v16")),
                             adc_labels = TRUE)

  testthat::expect_equal(colnames(a[[1]][[1]]),c("v13mrg","n","n_peso","pct","pct_peso","v13mrg_label"))
  testthat::expect_equal(a[[1]][[1]][["v13mrg_label"]],c(NA,NA,NA,"Total","Base"))
  testthat::expect_equal(a[[1]][[1]][["v13mrg"]],c("3","2","1","Total","Base"))

  testthat::expect_equal(colnames(a[[1]][[2]]),c("v15mrg","n","n_peso","pct","pct_peso","v15mrg_label"))
  testthat::expect_equal(a[[1]][[2]][["v15mrg_label"]],c(NA,"MRG valor 2","MRG valor 1","Total","Base"))
  testthat::expect_equal(a[[1]][[2]][["v15mrg"]],c("3","2","1","Total","Base"))

  testthat::expect_equal(as.character(a[[2]][[1]])[1],"c(\"v13mrg\", \"v15mrg\")")
  testthat::expect_equal(as.character(a[[2]][[1]])[2],"c(\"Variavel v13mrg nao presente no dicionario\", \"Variavel v15mrg possui label nao presente no dicionario. Codigo(s) [3]\")")
  testthat::expect_equal(as.character(a[[2]][[1]])[3],"c(\"rodou\", \"rodou\")")

  rm(a,nome_split,dadoss_split,Dicionarioo,dadoss)%>%suppressWarnings()
}

















#######################Isolada######################################################
fazer<-dados_split[[1]]

#todas as variáveis
for(i in 1:sample(1:length(fazer),2,replace = FALSE) ){
  nome_split=fazer[i]
  dadoss_split=dados_split
  Dicionarioo=Dicionario
  #Mundo perfeito
  dadoss<-dadoss_split[[2]][[which(dadoss_split$splits==nome_split)]]
  a<-PacoteInnovare::FUN_isoladas(
                             TABELA=dadoss,
                             DICIONARIO=Dicionarioo,
                             variaveis=colnames(dadoss),
                             adc_labels = TRUE)

  testthat::expect_equal(as.character(a[[2]][[1]])[1],"c(\"v4\", \"v15\", \"v16\", \"v18\", \"v20\", \"peso\")")
  testthat::expect_equal(as.character(a[[2]][[1]])[2],"c(\"Variavel v4 nao presente no dicionario\", \"Variavel v15 nao presente no dicionario\", \"Variavel v16 nao presente no dicionario\", \"Variavel v18 nao presente no dicionario\", \"Variavel v20 nao presente no dicionario\", \"Variavel peso nao presente no dicionario\")")
  testthat::expect_equal(as.character(a[[2]][[1]])[3],"c(\"rodou\", \"rodou\", \"rodou\", \"rodou\", \"rodou\", \"rodou\")")

  a<-a[[1]]
  for(j in 1:length(a)){
    testthat::expect_equal(colnames(a[[j]]),c(colnames(dadoss)[j],"n","n_peso","pct","pct_peso",paste0(colnames(dadoss)[j],"_label")))
    if(any(a[[j]]%in%c("v4","v15","v16","v18","v20","v18","peso"))){
      testthat::expect_equal(a[[j]][[6]],c(rep(NA,nrow(a[[j]])-2),"Total","Base"))
    }
    testthat::expect_equal(a[[j]][which(a[[j]][1]=="Total"),][[2]],dadoss%>%dplyr::count(colnames(dadoss)[j],na.rm=T)%>%summarise(soma=sum(n))%>%as.numeric())
    testthat::expect_equal(a[[j]][which(a[[j]][1]=="Total"),][[3]],dadoss%>%dplyr::count(colnames(dadoss)[j],wt=peso,na.rm=T)%>%summarise(soma=sum(n))%>%as.numeric())
    testthat::expect_equal(a[[j]][which(a[[j]][1]=="Total"),][[4]],100)
    testthat::expect_equal(a[[j]][which(a[[j]][1]=="Total"),][[5]],100)
    for(w in 1:(nrow(a[[j]])-2)){
      testthat::expect_equal(as.numeric(a[[j]][[4]][[w]]),as.numeric(a[[j]][[2]][[w]])/dadoss%>%dplyr::count(colnames(dadoss)[j],na.rm=T)%>%summarise(soma=sum(n))%>%as.numeric()*100)
      testthat::expect_equal(as.numeric(a[[j]][[5]][[w]]),as.numeric(a[[j]][[3]][[w]])/dadoss%>%dplyr::count(colnames(dadoss)[j],wt=peso,na.rm=T)%>%summarise(soma=sum(n))%>%as.numeric()*100)
    }
  }
}


isolada_V13<-function(nome_split,dadoss_split,Dicionarioo){

  dadoss<-dadoss_split[[2]][[which(dadoss_split$splits==nome_split)]]

  a<-PacoteInnovare::FUN_isoladas(TABELA=dadoss,
                             DICIONARIO=Dicionarioo,
                             variaveis=c("v13"),
                             adc_labels = TRUE)
  testthat::expect_equal(all(is.na(a[[2]][[1]])),TRUE)
  a<-a[[1]]

  testthat::expect_equal(colnames(a[[1]]),c("v13","n","n_peso","pct","pct_peso","v13_label"))
  testthat::expect_equal(a[[1]][["v13_label"]],c("MRG valor 2","MRG valor 1","Total","Base"))
  testthat::expect_equal(a[[1]][["v13"]],c("2","1","Total","Base"))

  n_sem_peso2<-dadoss%>%tidyr::pivot_longer(cols=c(v13))%>%dplyr::filter(value==2)%>%dplyr::count(value,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_sem_peso2<-ifelse(is.na(n_sem_peso2),0,n_sem_peso2)
  n_sem_peso1<-dadoss%>%tidyr::pivot_longer(cols=c(v13))%>%dplyr::filter(value==1)%>%dplyr::count(value,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_sem_peso1<-ifelse(is.na(n_sem_peso1),0,n_sem_peso1)
  n_sem_peso12<-n_sem_peso1+n_sem_peso2;n_sem_peso12<-ifelse(is.na(n_sem_peso12),0,n_sem_peso12)
  n_com_peso2<-dadoss%>%tidyr::pivot_longer(cols=c(v13))%>%dplyr::filter(value==2)%>%dplyr::count(value,wt=peso,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_com_peso2<-ifelse(is.na(n_com_peso2),0,n_com_peso2)
  n_com_peso1<-dadoss%>%tidyr::pivot_longer(cols=c(v13))%>%dplyr::filter(value==1)%>%dplyr::count(value,wt=peso,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_com_peso1<-ifelse(is.na(n_com_peso1),0,n_com_peso1)
  n_com_peso12<-n_com_peso2+n_com_peso1;n_com_peso12<-ifelse(is.na(n_com_peso12),0,n_com_peso12)

  pct_sem_peso2=n_sem_peso2/n_sem_peso12*100;pct_sem_peso2<-ifelse(is.na(pct_sem_peso2),0,pct_sem_peso2)
  pct_com_peso2=n_com_peso2/n_com_peso12*100;pct_com_peso2<-ifelse(is.na(pct_com_peso2),0,pct_com_peso2)
  pct_sem_peso1=n_sem_peso1/n_sem_peso12*100;pct_sem_peso1<-ifelse(is.na(pct_sem_peso1),0,pct_sem_peso1)
  pct_com_peso1=n_com_peso1/n_com_peso12*100;pct_com_peso1<-ifelse(is.na(pct_com_peso1),0,pct_com_peso1)
  pct_sem_peso12=pct_sem_peso2+pct_sem_peso1;pct_sem_peso12<-ifelse(is.na(pct_sem_peso12),0,pct_sem_peso12)
  pct_com_peso12=pct_com_peso2+pct_com_peso1;pct_com_peso12<-ifelse(is.na(pct_com_peso12),0,pct_com_peso12)

  base_sem_peso<-dadoss%>%dplyr::mutate(base=ifelse(!is.na(dadoss$v13) ,1,NA))%>%dplyr::summarise(n=sum(base,na.rm=TRUE))%>%dplyr::select(n)%>%as.numeric();base_sem_peso<-ifelse(is.na(base_sem_peso),0,base_sem_peso)
  base_com_peso<-dadoss%>%dplyr::mutate(base=ifelse(!is.na(dadoss$v13) ,peso,NA))%>%dplyr::summarise(n=sum(base,na.rm=TRUE))%>%dplyr::select(n)%>%as.numeric();base_com_peso<-ifelse(is.na(base_com_peso),0,base_com_peso)
  pct_base_bem_peso=base_sem_peso/nrow(dadoss)*100;pct_base_bem_peso<-ifelse(is.na(pct_base_bem_peso),0,pct_base_bem_peso)
  pct_base_com_peso=base_com_peso/dadoss%>%dplyr::summarise(sum(peso,na.rm=TRUE))%>%as.numeric()*100;pct_base_com_peso<-ifelse(is.na(pct_base_com_peso),0,pct_base_com_peso)

  testthat::expect_equal(a[[1]][["n"]],c(n_sem_peso2,n_sem_peso1,n_sem_peso12,base_sem_peso))
  testthat::expect_equal(a[[1]][["n_peso"]],c(n_com_peso2,n_com_peso1,n_com_peso12,base_com_peso))
  testthat::expect_equal(a[[1]][["pct"]],c(pct_sem_peso2,pct_sem_peso1,pct_sem_peso12,pct_base_bem_peso))
  testthat::expect_equal(a[[1]][["pct_peso"]],c(pct_com_peso2,pct_com_peso1,pct_com_peso12,pct_base_com_peso))
}
#Alguns NA's
for(i in sample(1:length(fazer),5,replace = FALSE)){
  isolada_V13(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v13", ~ ifelse(. == 2, NA, .))))),Dicionarioo=Dicionario )
  isolada_V13(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v13", ~ ifelse(. %in%c(1,2), NA, .))))),Dicionarioo=Dicionario )
}

#testando códigos a mais
isolada_V13_com_codigo_a_mais<-function(nome_split,dadoss_split,Dicionarioo){
  dadoss<-dadoss_split[[2]][[which(dadoss_split$splits==nome_split)]]
  a<-PacoteInnovare::FUN_isoladas(TABELA=dadoss,
                             DICIONARIO=Dicionarioo,
                             variaveis=c("v13"),
                             adc_labels = TRUE)
  if(all(is.na(dadoss$v13))){testthat::expect_equal(all(is.na(a[[2]][[1]])),TRUE)}else{testthat::expect_equal(all(is.na(a[[2]][[1]])),FALSE)}

  a<-a[[1]]

  testthat::expect_equal(colnames(a[[1]]),c("v13","n","n_peso","pct","pct_peso","v13_label"))
  if(all(is.na(dadoss$v13))){testthat::expect_equal(a[[1]][["v13_label"]],c("MRG valor 2","MRG valor 1","Total","Base"))}else{testthat::expect_equal(a[[1]][["v13_label"]],c(NA,"MRG valor 2","MRG valor 1","Total","Base"))}
  if(all(is.na(dadoss$v13))){testthat::expect_equal(a[[1]][["v13"]],c("2","1","Total","Base"))}else{testthat::expect_equal(a[[1]][["v13"]],c("3","2","1","Total","Base"))}



  n_sem_peso3<-dadoss%>%tidyr::pivot_longer(cols=c(v13))%>%dplyr::filter(value==3)%>%dplyr::count(value,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_sem_peso3<-ifelse(is.na(n_sem_peso3),0,n_sem_peso3)
  n_sem_peso2<-dadoss%>%tidyr::pivot_longer(cols=c(v13))%>%dplyr::filter(value==2)%>%dplyr::count(value,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_sem_peso2<-ifelse(is.na(n_sem_peso2),0,n_sem_peso2)
  n_sem_peso1<-dadoss%>%tidyr::pivot_longer(cols=c(v13))%>%dplyr::filter(value==1)%>%dplyr::count(value,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_sem_peso1<-ifelse(is.na(n_sem_peso1),0,n_sem_peso1)
  n_sem_peso123<-n_sem_peso1+n_sem_peso2+n_sem_peso3;n_sem_peso123<-ifelse(is.na(n_sem_peso123),0,n_sem_peso123)
  n_com_peso3<-dadoss%>%tidyr::pivot_longer(cols=c(v13))%>%dplyr::filter(value==3)%>%dplyr::count(value,wt=peso,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_com_peso3<-ifelse(is.na(n_com_peso3),0,n_com_peso3)
  n_com_peso2<-dadoss%>%tidyr::pivot_longer(cols=c(v13))%>%dplyr::filter(value==2)%>%dplyr::count(value,wt=peso,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_com_peso2<-ifelse(is.na(n_com_peso2),0,n_com_peso2)
  n_com_peso1<-dadoss%>%tidyr::pivot_longer(cols=c(v13))%>%dplyr::filter(value==1)%>%dplyr::count(value,wt=peso,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_com_peso1<-ifelse(is.na(n_com_peso1),0,n_com_peso1)
  n_com_peso123<-n_com_peso3+n_com_peso2+n_com_peso1;n_com_peso123<-ifelse(is.na(n_com_peso123),0,n_com_peso123)

  pct_sem_peso3=n_sem_peso3/n_sem_peso123*100;pct_sem_peso3<-ifelse(is.na(pct_sem_peso3),0,pct_sem_peso3)
  pct_com_peso3=n_com_peso3/n_com_peso123*100;pct_com_peso3<-ifelse(is.na(pct_com_peso3),0,pct_com_peso3)
  pct_sem_peso2=n_sem_peso2/n_sem_peso123*100;pct_sem_peso2<-ifelse(is.na(pct_sem_peso2),0,pct_sem_peso2)
  pct_com_peso2=n_com_peso2/n_com_peso123*100;pct_com_peso2<-ifelse(is.na(pct_com_peso2),0,pct_com_peso2)
  pct_sem_peso1=n_sem_peso1/n_sem_peso123*100;pct_sem_peso1<-ifelse(is.na(pct_sem_peso1),0,pct_sem_peso1)
  pct_com_peso1=n_com_peso1/n_com_peso123*100;pct_com_peso1<-ifelse(is.na(pct_com_peso1),0,pct_com_peso1)
  pct_sem_peso123=pct_sem_peso3+pct_sem_peso2+pct_sem_peso1;pct_sem_peso123<-ifelse(is.na(pct_sem_peso123),0,pct_sem_peso123)
  pct_com_peso123=pct_com_peso3+pct_com_peso2+pct_com_peso1;pct_com_peso123<-ifelse(is.na(pct_com_peso123),0,pct_com_peso123)

  base_sem_peso<-dadoss%>%dplyr::mutate(base=ifelse(!is.na(dadoss$v13),1,NA))%>%dplyr::summarise(n=sum(base,na.rm=TRUE))%>%dplyr::select(n)%>%as.numeric();base_sem_peso<-ifelse(is.na(base_sem_peso),0,base_sem_peso)
  base_com_peso<-dadoss%>%dplyr::mutate(base=ifelse(!is.na(dadoss$v13),peso,NA))%>%dplyr::summarise(n=sum(base,na.rm=TRUE))%>%dplyr::select(n)%>%as.numeric();base_com_peso<-ifelse(is.na(base_com_peso),0,base_com_peso)
  pct_base_bem_peso=base_sem_peso/nrow(dadoss)*100;pct_base_bem_peso<-ifelse(is.na(pct_base_bem_peso),0,pct_base_bem_peso)
  pct_base_com_peso=base_com_peso/dadoss%>%dplyr::summarise(sum(peso,na.rm=TRUE))%>%as.numeric()*100;pct_base_com_peso<-ifelse(is.na(pct_base_com_peso),0,pct_base_com_peso)

  if(all(is.na(dadoss$v13))){testthat::expect_equal(a[[1]][["n"]],c(n_sem_peso2,n_sem_peso1,n_sem_peso123,base_sem_peso))}else{testthat::expect_equal(a[[1]][["n"]],c(n_sem_peso3,n_sem_peso2,n_sem_peso1,n_sem_peso123,base_sem_peso))}
  if(all(is.na(dadoss$v13))){testthat::expect_equal(a[[1]][["n_peso"]],c(n_com_peso2,n_com_peso1,n_com_peso123,base_com_peso))}else{testthat::expect_equal(a[[1]][["n_peso"]],c(n_com_peso3,n_com_peso2,n_com_peso1,n_com_peso123,base_com_peso))}
  if(all(is.na(dadoss$v13))){testthat::expect_equal(a[[1]][["pct"]],c(pct_sem_peso2,pct_sem_peso1,pct_sem_peso123,pct_base_bem_peso))}else{testthat::expect_equal(a[[1]][["pct"]],c(pct_sem_peso3,pct_sem_peso2,pct_sem_peso1,pct_sem_peso123,pct_base_bem_peso))}
  if(all(is.na(dadoss$v13))){testthat::expect_equal(a[[1]][["pct_peso"]],c(pct_com_peso2,pct_com_peso1,pct_com_peso123,pct_base_com_peso))}else{testthat::expect_equal(a[[1]][["pct_peso"]],c(pct_com_peso3,pct_com_peso2,pct_com_peso1,pct_com_peso123,pct_base_com_peso))}

  a<-PacoteInnovare::FUN_isoladas(TABELA=dadoss,
                                  DICIONARIO=Dicionarioo,
                                  variaveis=c("v13"),
                                  adc_labels = FALSE)
  if(all(is.na(dadoss$v13))){testthat::expect_equal(all(is.na(a[[2]][[1]])),TRUE)}else{testthat::expect_equal(all(is.na(a[[2]][[1]])),FALSE)}
  a<-a[[1]]
  testthat::expect_equal(colnames(a[[1]]),c("v13","n","n_peso","pct","pct_peso"))

}
fazer<-dados_split[[1]]
#Código que não tem no dicionário
for(i in sample(1:length(fazer),5,replace = FALSE)){
  isolada_V13_com_codigo_a_mais(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v13", ~ ifelse(. == 2, 3, .))))),Dicionarioo=Dicionario)
  isolada_V13_com_codigo_a_mais(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v13", ~ ifelse(. %in%c(1,2), 3, .))))),Dicionarioo=Dicionario )
  isolada_V13_com_codigo_a_mais(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v13", ~ ifelse(. %in%c(1,2), NA, .))))),Dicionarioo=Dicionario )
  #Com 2 códigos
  nome_split=fazer[[i]];dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v13", ~ ifelse(. == 2, 3, .)))))%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v13", ~ ifelse(. == 1, 4, .)))));Dicionarioo=Dicionario

  dadoss<-dadoss_split[[2]][[which(dadoss_split$splits==nome_split)]]
  a<-PacoteInnovare::FUN_isoladas(TABELA=dadoss,
                             DICIONARIO=Dicionarioo,
                             variaveis=c("v13","v14"),
                             adc_labels = TRUE)
  testthat::expect_equal(as.character(a[[2]][[1]])[1],"v13")
  testthat::expect_equal(as.character(a[[2]][[1]])[2],"Variavel v13 possui label nao presente no dicionario. Codigo(s) [4,3]")
  testthat::expect_equal(as.character(a[[2]][[1]])[3],"rodou")
  a<-a[[1]]
  testthat::expect_equal(colnames(a[[1]]),c("v13","n","n_peso","pct","pct_peso","v13_label"))
  testthat::expect_equal(a[[1]][["v13_label"]],c(NA,NA,"MRG valor 2","MRG valor 1","Total","Base"))
  testthat::expect_equal(a[[1]][["v13"]],c("4","3","2","1","Total","Base"))
  rm(dadoss,a,nome_split,dadoss_split,Dicionarioo)%>%suppressWarnings()
}

#Sem estar no dicionário
for(i in sample(1:length(fazer),5,replace = FALSE)){
  nome_split=fazer[[i]];dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v13", ~ ifelse(. ==1, 3, NA)))));Dicionarioo=Dicionario%>%dplyr::mutate(opcao_variavel=ifelse(opcao_variavel=="v13","v31",opcao_variavel))

  dadoss<-dadoss_split[[2]][[which(dadoss_split$splits==nome_split)]]
  a<-PacoteInnovare::FUN_isoladas(TABELA=dadoss,
                             DICIONARIO=Dicionarioo,
                             variaveis=c("v13","v14"),
                             adc_labels = TRUE)
  testthat::expect_equal(colnames(a[[1]][[1]]),c("v13","n","n_peso","pct","pct_peso","v13_label"))
  testthat::expect_equal(a[[1]][[1]][["v13_label"]],c(NA,"Total","Base"))
  testthat::expect_equal(a[[1]][[1]][["v13"]],c("3","Total","Base"))

  testthat::expect_equal(colnames(a[[1]][[2]]),c("v14","n","n_peso","pct","pct_peso","v14_label"))
  testthat::expect_equal(a[[1]][[2]][["v14_label"]],c("MRG valor 2","MRG valor 1","Total","Base"))
  testthat::expect_equal(a[[1]][[2]][["v14"]],c("2","1","Total","Base"))

  testthat::expect_equal(as.character(a[[2]][[1]])[1],"v13")
  testthat::expect_equal(as.character(a[[2]][[1]])[2],"Variavel v13 nao presente no dicionario")
  testthat::expect_equal(as.character(a[[2]][[1]])[3],"rodou")
  rm(a,nome_split,dadoss_split,Dicionarioo,dadoss)%>%suppressWarnings()
}

















































#####################################CITOU######################################
mrg_citou<-function(nome_split,dadoss_split,pesoo,Dicionarioo){
  dadoss<-dadoss_split[[2]][[which(dadoss_split$splits==nome_split)]]
  a<-PacoteInnovare::FUN_Citou(TABELA=dadoss,
                             DICIONARIO=Dicionarioo,
                             lista_variaveis=list("MRG5"=c("v5","v6")),
                             adc_labels = TRUE)
  levels=dadoss%>%dplyr::select(v5,v6)%>%pivot_longer(cols=c(v5,v6))%>%mutate(value=as.factor(value))%>%select(value)%>%summarise(levels(value))%>%pull

  if(length(levels)==2){
    if(length(levels)%in%c(1,2)){
      ifelse(all(c("1","2")%in%levels),testthat::expect_equal(all(is.na(a[[2]][[1]])),TRUE),testthat::expect_equal(all(is.na(a[[2]][[1]])),FALSE))
    }
  }
  if(length(levels)==3){
    if(all(levels%in%c("1","3","4"))){testthat::expect_equal(as.character(a[[2]][[1]]["Problema"]),"Variavel v5mrg: codigo a mais encontrado em TABELA. Codigo [3,4] foi tratado como nao/nao citou") }
    if(all(levels%in%c("1","2","3"))){testthat::expect_equal(as.character(a[[2]][[1]]["Problema"]),"Variavel v5mrg: codigo a mais encontrado em TABELA. Codigo [3] foi tratado como nao/nao citou") }
  }


  #print(a)
  a<-a[[1]]
  testthat::expect_equal(colnames(a[[1]]),c("v5mrg","n","n_peso","pct","pct_peso","v5mrg_label"))
  testthat::expect_equal(a[[1]][["v5mrg_label"]],c("Enunciado v5","Enunciado v6","Total","Base"))
  testthat::expect_equal(a[[1]][["v5mrg"]],c("v5","v6","Total","Base"))

  n_sem_peso2<-dadoss%>%tidyr::pivot_longer(cols=c(v5))%>%dplyr::filter(value==1)%>%dplyr::count(value,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_sem_peso2<-ifelse(is.na(n_sem_peso2),0,n_sem_peso2)
  n_sem_peso1<-dadoss%>%tidyr::pivot_longer(cols=c(v6))%>%dplyr::filter(value==1)%>%dplyr::count(value,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_sem_peso1<-ifelse(is.na(n_sem_peso1),0,n_sem_peso1)
  n_sem_peso12<-n_sem_peso1+n_sem_peso2;n_sem_peso12<-ifelse(is.na(n_sem_peso12),0,n_sem_peso12)
  n_com_peso2<-dadoss%>%tidyr::pivot_longer(cols=c(v5))%>%dplyr::filter(value==1)%>%dplyr::count(value,wt=peso,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_com_peso2<-ifelse(is.na(n_com_peso2),0,n_com_peso2)
  n_com_peso1<-dadoss%>%tidyr::pivot_longer(cols=c(v6))%>%dplyr::filter(value==1)%>%dplyr::count(value,wt=peso,na.rm=TRUE)%>%dplyr::select(n)%>%as.numeric();n_com_peso1<-ifelse(is.na(n_com_peso1),0,n_com_peso1)
  n_com_peso12<-n_com_peso2+n_com_peso1;n_com_peso12<-ifelse(is.na(n_com_peso12),0,n_com_peso12)

  pct_sem_peso2=n_sem_peso2/n_sem_peso12*100;pct_sem_peso2<-ifelse(is.na(pct_sem_peso2),0,pct_sem_peso2)
  pct_com_peso2=n_com_peso2/n_com_peso12*100;pct_com_peso2<-ifelse(is.na(pct_com_peso2),0,pct_com_peso2)
  pct_sem_peso1=n_sem_peso1/n_sem_peso12*100;pct_sem_peso1<-ifelse(is.na(pct_sem_peso1),0,pct_sem_peso1)
  pct_com_peso1=n_com_peso1/n_com_peso12*100;pct_com_peso1<-ifelse(is.na(pct_com_peso1),0,pct_com_peso1)
  pct_sem_peso12=pct_sem_peso2+pct_sem_peso1;pct_sem_peso12<-ifelse(is.na(pct_sem_peso12),0,pct_sem_peso12)
  pct_com_peso12=pct_com_peso2+pct_com_peso1;pct_com_peso12<-ifelse(is.na(pct_com_peso12),0,pct_com_peso12)

  base_sem_peso<-dadoss%>%dplyr::mutate(base=ifelse(!is.na(dadoss$v5) | !is.na(dadoss$v6),1,NA))%>%dplyr::summarise(n=sum(base,na.rm=TRUE))%>%dplyr::select(n)%>%as.numeric();base_sem_peso<-ifelse(is.na(base_sem_peso),0,base_sem_peso)
  base_com_peso<-dadoss%>%dplyr::mutate(base=ifelse(!is.na(dadoss$v5) | !is.na(dadoss$v6),peso,NA))%>%dplyr::summarise(n=sum(base,na.rm=TRUE))%>%dplyr::select(n)%>%as.numeric();base_com_peso<-ifelse(is.na(base_com_peso),0,base_com_peso)
  pct_base_bem_peso=base_sem_peso/nrow(dadoss)*100;pct_base_bem_peso<-ifelse(is.na(pct_base_bem_peso),0,pct_base_bem_peso)
  pct_base_com_peso=base_com_peso/dadoss%>%dplyr::summarise(sum(peso,na.rm=TRUE))%>%as.numeric()*100;pct_base_com_peso<-ifelse(is.na(pct_base_com_peso),0,pct_base_com_peso)

  testthat::expect_equal(a[[1]][["n"]],c(n_sem_peso2,n_sem_peso1,n_sem_peso12,base_sem_peso))
  testthat::expect_equal(a[[1]][["n_peso"]],c(n_com_peso2,n_com_peso1,n_com_peso12,base_com_peso))
  testthat::expect_equal(a[[1]][["pct"]],c(pct_sem_peso2,pct_sem_peso1,pct_sem_peso12,pct_base_bem_peso))
  testthat::expect_equal(a[[1]][["pct_peso"]],c(pct_com_peso2,pct_com_peso1,pct_com_peso12,pct_base_com_peso))


  dadoss<-dadoss_split[[2]][[which(dadoss_split$splits==nome_split)]]
  a<-PacoteInnovare::FUN_Citou(TABELA=dadoss,
                               DICIONARIO=Dicionarioo,
                               lista_variaveis=list("MRG5"=c("v5","v6")),
                               adc_labels = FALSE)

  levels=dadoss%>%dplyr::select(v5,v6)%>%pivot_longer(cols=c(v5,v6))%>%mutate(value=as.factor(value))%>%select(value)%>%summarise(levels(value))%>%pull
  if(length(levels)==2){
    if(length(levels)%in%c(1,2)){
      ifelse(all(c("1","2")%in%levels),testthat::expect_equal(all(is.na(a[[2]][[1]])),TRUE),testthat::expect_equal(all(is.na(a[[2]][[1]])),FALSE))
    }
  }
  if(length(levels)==3){
    if(all(levels%in%c("1","3","4"))){testthat::expect_equal(as.character(a[[2]][[1]]["Problema"]),"Variavel v5mrg: codigo a mais encontrado em TABELA. Codigo [3,4] foi tratado como nao/nao citou") }
    if(all(levels%in%c("1","2","3"))){testthat::expect_equal(as.character(a[[2]][[1]]["Problema"]),"Variavel v5mrg: codigo a mais encontrado em TABELA. Codigo [3] foi tratado como nao/nao citou") }
  }
  a<-a[[1]]

  testthat::expect_equal(colnames(a[[1]]),c("v5mrg","n","n_peso","pct","pct_peso"))
  testthat::expect_equal(a[[1]][["v5mrg"]],c("v5","v6","Total","Base"))

  testthat::expect_equal(a[[1]][["n"]],c(n_sem_peso2,n_sem_peso1,n_sem_peso12,base_sem_peso))
  testthat::expect_equal(a[[1]][["n_peso"]],c(n_com_peso2,n_com_peso1,n_com_peso12,base_com_peso))
  testthat::expect_equal(a[[1]][["pct"]],c(pct_sem_peso2,pct_sem_peso1,pct_sem_peso12,pct_base_bem_peso))
  testthat::expect_equal(a[[1]][["pct_peso"]],c(pct_com_peso2,pct_com_peso1,pct_com_peso12,pct_base_com_peso))

}
fazer<-dados_split[[1]]
#Mundo perfeiro
for(i in sample(1:length(fazer),3,replace = FALSE) ){
  mrg_citou(nome_split=fazer[[i]],dadoss_split=dados_split,Dicionarioo=Dicionario)
}
for(i in sample(1:length(fazer),3,replace = FALSE)){
  #Alguns NA's
  mrg_citou(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v5", ~ ifelse(. == 2, NA, .))))),Dicionarioo=Dicionario)
  mrg_citou(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v5", ~ ifelse(. == 1, NA, .))))),Dicionarioo=Dicionario)

  mrg_citou(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v5", ~ ifelse(. == 2, NA, .)))))%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v6", ~ ifelse(. == 2, NA, .))))),Dicionarioo=Dicionario )
  mrg_citou(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v5", ~ ifelse(. == 1, NA, .)))))%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v6", ~ ifelse(. == 1, NA, .))))),Dicionarioo=Dicionario )

  mrg_citou(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v5", ~ ifelse(. %in%c(1,2), NA, .))))),Dicionarioo=Dicionario )
  mrg_citou(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v5", ~ ifelse(. %in%c(1,2), NA, .)))))%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v6", ~ ifelse(. %in%c(1,2), NA, .))))),Dicionarioo=Dicionario )

  #Código que não tem no dicionário
  mrg_citou(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v5", ~ ifelse(. == 2, 3, .))))),Dicionarioo=Dicionario)
  mrg_citou(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v5", ~ ifelse(. == 1, 3, .))))),Dicionarioo=Dicionario)

  mrg_citou(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v5", ~ ifelse(. == 2, 3, .)))))%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v6", ~ ifelse(. == 2, 3, .))))),Dicionarioo=Dicionario )
  mrg_citou(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v5", ~ ifelse(. == 1, 3, .)))))%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v6", ~ ifelse(. == 1, 3, .))))),Dicionarioo=Dicionario )

  mrg_citou(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v5", ~ ifelse(. %in%c(1,2), 3, .))))),Dicionarioo=Dicionario )
  mrg_citou(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v5", ~ ifelse(. %in%c(1,2), 3, .)))))%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v6", ~ ifelse(. %in%c(1,2), 3, .))))),Dicionarioo=Dicionario )

  mrg_citou(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v5", ~ ifelse(. %in%c(1,2), 4, .))))),Dicionarioo=Dicionario )
  mrg_citou(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v5", ~ ifelse(. %in%c(1), 4, .)))))%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v6", ~ ifelse(. %in%c(1,2), 3, .))))),Dicionarioo=Dicionario )
  mrg_citou(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v5", ~ ifelse(. %in%c(1), 4, .)))))%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v6", ~ ifelse(. %in%c(1,2), NA, .))))),Dicionarioo=Dicionario )
  mrg_citou(nome_split=fazer[[i]],dadoss_split=dados_split%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v5", ~ ifelse(. %in%c(1), 4, .)))))%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v5", ~ ifelse(. %in%c(2), NA, .)))))%>%dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(., dplyr::across("v6", ~ ifelse(. %in%c(1,2), NA, .))))),Dicionarioo=Dicionario )
  #sem estar no dicionário (enunciado)
  mrg_citou(nome_split=fazer[[i]],dadoss_split=dados_split,Dicionarioo=Dicionario%>%dplyr::mutate(opcao_label=NA))
}

#Sem estar no dicionário (enunciado)
for(i in sample(1:length(fazer),3,replace = FALSE)){
  nome_split=fazer[[i]];dadoss_split=dados_split;Dicionarioo=Dicionario
  dadoss<-dadoss_split[[2]][[which(dadoss_split$splits==nome_split)]]
  b<-PacoteInnovare::FUN_Citou(TABELA=dadoss,
                               DICIONARIO=Dicionarioo,
                               lista_variaveis=list("MRG5"=c("v5","v6")),
                               adc_labels = TRUE)

  nome_split=fazer[[i]];dadoss_split=dados_split;Dicionarioo=Dicionario%>%dplyr::mutate(pergunta_enunciado=ifelse(opcao_variavel%in%c("v5"),NA,opcao_variavel))
  dadoss<-dadoss_split[[2]][[which(dadoss_split$splits==nome_split)]]
  a<-PacoteInnovare::FUN_Citou(TABELA=dadoss,
                               DICIONARIO=Dicionarioo,
                               lista_variaveis=list("MRG5"=c("v5","v6")),
                               adc_labels = TRUE)
  testthat::expect_equal(a[[2]][[1]][[1]],"v5mrg")
  testthat::expect_equal(a[[2]][[1]][[2]],"Variavel v5mrg possui label nao presente no dicionario. Enuncaiado(s) [v5] ausente(s).")
  testthat::expect_equal(a[[2]][[1]][[3]],"rodou")

  testthat::expect_equal(a[[1]][[1]][[1]],b[[1]][[1]][[1]])
  testthat::expect_equal(a[[1]][[1]][[2]],b[[1]][[1]][[2]])
  testthat::expect_equal(a[[1]][[1]][[3]],b[[1]][[1]][[3]])
  testthat::expect_equal(a[[1]][[1]][[4]],b[[1]][[1]][[4]])
  testthat::expect_equal(a[[1]][[1]][[5]],b[[1]][[1]][[5]])
  testthat::expect_equal(a[[1]][[1]][[6]],c(NA,"v6","Total","Base"))


  nome_split=fazer[[i]];dadoss_split=dados_split;Dicionarioo=Dicionario%>%dplyr::mutate(pergunta_enunciado=ifelse(opcao_variavel%in%c("v5","v6"),NA,opcao_variavel))
  dadoss<-dadoss_split[[2]][[which(dadoss_split$splits==nome_split)]]
  a<-PacoteInnovare::FUN_Citou(TABELA=dadoss,
                               DICIONARIO=Dicionarioo,
                               lista_variaveis=list("MRG5"=c("v5","v6")),
                               adc_labels = TRUE)
  testthat::expect_equal(a[[2]][[1]][[1]],"v5mrg")
  testthat::expect_equal(a[[2]][[1]][[2]],"Variavel v5mrg possui label nao presente no dicionario. Enuncaiado(s) [v5,v6] ausente(s).")
  testthat::expect_equal(a[[2]][[1]][[3]],"rodou")

  testthat::expect_equal(a[[1]][[1]][[1]],b[[1]][[1]][[1]])
  testthat::expect_equal(a[[1]][[1]][[2]],b[[1]][[1]][[2]])
  testthat::expect_equal(a[[1]][[1]][[3]],b[[1]][[1]][[3]])
  testthat::expect_equal(a[[1]][[1]][[4]],b[[1]][[1]][[4]])
  testthat::expect_equal(a[[1]][[1]][[5]],b[[1]][[1]][[5]])
  testthat::expect_equal(a[[1]][[1]][[6]],c(NA,NA,"Total","Base"))

  a<-PacoteInnovare::FUN_Citou(TABELA=dadoss,
                               DICIONARIO=Dicionarioo,
                               lista_variaveis=list("MRG5"=c("v5","v6")),
                               adc_labels = FALSE)
  testthat::expect_equal(a[[2]][[1]][[1]],"v5mrg")
  testthat::expect_equal(a[[2]][[1]][[2]],"Variavel v5mrg possui label nao presente no dicionario. Enuncaiado(s) [v5,v6] ausente(s).")
  testthat::expect_equal(a[[2]][[1]][[3]],"rodou")

  testthat::expect_equal(a[[1]][[1]][[1]],b[[1]][[1]][[1]])
  testthat::expect_equal(a[[1]][[1]][[2]],b[[1]][[1]][[2]])
  testthat::expect_equal(a[[1]][[1]][[3]],b[[1]][[1]][[3]])
  testthat::expect_equal(a[[1]][[1]][[4]],b[[1]][[1]][[4]])
  testthat::expect_equal(a[[1]][[1]][[5]],b[[1]][[1]][[5]])
}

rm(b,a,levels,n_com_peso1,n_com_peso12,fazer,i,n_com_peso2,n_com_peso1,n_com_peso12,n_sem_peso2,n_sem_peso1,n_sem_peso12,base_com_peso,base_sem_peso,nome_split,pct_base_bem_peso,pct_base_com_peso,pct_com_peso1,pct_com_peso12,pct_com_peso2,pct_com_peso2,pct_sem_peso1,pct_sem_peso12,pct_sem_peso2,dadoss,dadoss_split,Dicionarioo)
