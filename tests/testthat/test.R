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
      dplyr::mutate(across(v1, as.character))
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
a<-FUN_isoladas(TABELA = dados_peso, DICIONARIO = Label, variaveis = "v1", adc_labels = F)%>%data.frame()
expect_equal(nrow(a),4);expect_equal(ncol(a),5);expect_equal(colnames(a),c("v1","n","n_peso","pct","pct_peso"))
expect_equal(a[,1],c("1","2","Total","Base"));expect_equal(a[,2],c(49,51,100,100));expect_equal(a[,3],c(50,50,100,100));expect_equal(a[,4],c(49,51,100,100));expect_equal(a[,5],c(50,50,100,100))
#Testando o parâmetro adc_labels
a<-FUN_isoladas(TABELA = dados_peso, DICIONARIO = Label, variaveis = "v1", adc_labels = T)%>%data.frame()
expect_equal(nrow(a),4);expect_equal(ncol(a),6);expect_equal(colnames(a),c("v1","n","n_peso","pct","pct_peso","v1_label"))
expect_equal(a[,1],c("1","2","Total","Base"));expect_equal(a[,2],c(49,51,100,100));expect_equal(a[,3],c(50,50,100,100));expect_equal(a[,4],c(49,51,100,100));expect_equal(a[,5],c(50,50,100,100));expect_equal(a[,6],c("Masculino","Feminino","Total","Base"))

rm(a,dados,dados_peso,Dados_splits,Label,splits,n,v1,v2,v3,v4)%>%suppressWarnings()
