#' @title Arquivos para Exemplificar o uso das Funções
#'
#' @name Example
#'
#' @description Essa função gera três data.frames's que podem ser usados para testar e usar as funções do \code{PacoteInnovare}.
#'
#' @return Essa função retorna uma \code{lista} contendo 3 \code{data.frame}:
#' \itemize{
#'  \item '\code{Dados}': dados fictícios.
#'  \item \code{Label}: dicionário dos dados fictícios.
#'  \item \code{Enunciado}: enunciado dos dados fictícios.
#' }
#'
#' @author Felipe Ferreira \email{felipe179971@hotmail.com} e Rafael Peixoto \email{peixoto.rafael@outlook.com}.
#'
#' @examples
#' #Executando
#' Dados<-Example()[[1]]
#' Label<-Example()[[2]]
#' Enunciado<-Example()[[3]]
#' #Verificando
#' head(Dados)
#' head(Label)
#' head(Enunciado)
#'
#'
#' @encoding UTF-8
#'
#' @import dplyr
#'
#' @export

Example<-function(){
  #Número de linhas
  n=100
  #Variável de sexo
  set.seed(1)
  v1=sample(1:2,n,replace = TRUE)
  #variável de renda
  set.seed(2)
  v2=sample(1:4,n,replace = TRUE)
  #Variável da UF
  set.seed(3)
  v3=sample(1:10,n,replace = TRUE)
  #Peso (sexo)
  v4<-v1
  v4[which(v4==1)]<-(n*.1)/length(v1[which(v1==1)]) #testando pesos extremos
  v4[which(v4==2)]<-(n*.9)/length(v1[which(v1==2)])
  #Primeiro MRG citou/não citou
  set.seed(4);v5=sample(1:2,n,replace = TRUE)
  set.seed(5);v6=sample(1:2,n,replace = TRUE)
  set.seed(6);v7=sample(1:2,n,replace = TRUE)
  set.seed(7);v8=sample(1:2,n,replace = TRUE)
  #MRG citou/não citou mas com sim e não
  v9=v5
  v10=v6
  v11=v7
  v12=v8
  #MRG com variaveis completas
  v13=v5
  v14=v6
  #Média completa (não entra no arquivo de label)
  set.seed(8);v15=rep(10,n) #verificar se a média dá 10 mesmo
  set.seed(9);v16=sample(1:20,n,replace = TRUE)
  #Time
  set.seed(10);v17=sample(1:3,n,replace = TRUE)
  v18<-v17
  v18[which(v18==1)]<-(n*.6)/length(v17[which(v17==1)])
  v18[which(v18==2)]<-(n*.1)/length(v17[which(v17==2)])
  v18[which(v18==3)]<-(n*.3)/length(v17[which(v17==3)])

  #Tipo (não terá peso)
  set.seed(11);v19=sample(1:5,n,replace = TRUE)
  #Com texto (sem label)
  set.seed(12);v20<-paste("Texto",sample(1:3,n,replace = TRUE))
  #Média com NS e NR
  set.seed(13);v21=sample(1:100,n,replace = TRUE);v21[which(v21%in%c(88))]<--88;v21[which(v21%in%c(69))]<--99
  Dados<-data.frame(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19,v20,v21)
  #Criando o Dicionário dos Dados#
  Label=data.frame(
    id=c(rep(1,length(v1%>%unique())),
         rep(2,length(v2%>%unique())),
         rep(3,length(v3%>%unique())),
         rep(5,length(v5%>%unique())),
         rep(6,length(v6%>%unique())),
         rep(7,length(v7%>%unique())),
         rep(8,length(v8%>%unique())),
         rep(9,length(v9%>%unique())),
         rep(10,length(v10%>%unique())),
         rep(11,length(v11%>%unique())),
         rep(12,length(v12%>%unique())),
         rep(13,length(v13%>%unique())),
         rep(14,length(v14%>%unique())),
         rep(17,length(v17%>%unique())),
         rep(19,length(v19%>%unique())),
         rep(21,2)
    ),
    variavel=c(rep("v1",length(v1%>%unique())),
               rep("v2",length(v2%>%unique())),
               rep("v3",length(v3%>%unique())),
               rep("v5",length(v5%>%unique())),
               rep("v6",length(v6%>%unique())),
               rep("v7",length(v7%>%unique())),
               rep("v8",length(v8%>%unique())),
               rep("v9",length(v9%>%unique())),
               rep("v10",length(v10%>%unique())),
               rep("v11",length(v11%>%unique())),
               rep("v12",length(v12%>%unique())),
               rep("v13",length(v13%>%unique())),
               rep("v14",length(v14%>%unique())),
               rep("v17",length(v17%>%unique())),
               rep("v19",length(v19%>%unique())),
               rep("v21",2)
    ),
    codigo=c(v1%>%unique(),v2%>%unique(),v3%>%unique(),rep(v5%>%unique(),4),rep(v5%>%unique(),4),v13%>%unique(),v14%>%unique(),1,2,3,v19%>%unique(),-88,-99),
    label=c("Masculino","Feminino",
            "Baixa Renda","Classe Media","Classe Alta","Milionario",
            paste("Cidade",seq(1:10)),
            rep(c("nao citou","citou"),4),
            rep(c("nao","sim"),4),
            rep(c("MRG valor 2","MRG valor 1"),2),
            c("Flamengo","Vasco","Outros"),
            c("A","B","C","D","E"),
            c("NS","NR")

    ))
  #Colocando o Label do MRG
  Label<-rbind(Label,Label[which(Label$variavel%in%c("v13")),]%>%dplyr::mutate(id=max(Label$id)+1,variavel="v13mrg"))
  #Criando o arquivo com o enunciado
  vars<-c(colnames(Dados)[-which(colnames(Dados)%in%c("v4","v15","v16"))],"v13mrg","v15m","v16m")
  Enunciado=data.frame(
    id=seq(1:length(vars)),
    variavel=vars
  )%>%dplyr::mutate(enunciado=paste("Enunciado",variavel))
  rm(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19,vars,n,v20,v21)%>%suppressWarnings()
  return(list(Dados,Label,Enunciado))
}
