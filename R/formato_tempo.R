#' @title Time Difference
#'
#' @name Exemplos
#'
#' @description Calcula a diferença entre dois vetores do tipo \code{Sys.time}.
#' Normalmente utilizada para avaliar o tempo de execução das demais funções do
#' \code{PaceteInnovare}, é internalizada em diversas funções do pacote mas pode ser utilizada livremente.
#'
#' @param end Tempo final.
#' @param start Tempo inicial.
#'
#' @return Retorna o texto \code{X horas, Y minutos e Z segundos}:
#'
#' @author Felipe Ferreira \email{felipe179971@hotmail.com} e Rafael Peixoto \email{peixoto.rafael@outlook.com}.
#'
#' @examples
#' Time_Difference("2022-07-14 17:00:53 -03","2022-07-14 16:00:53 -03")
#' Time_Difference("2022-07-14 17:00:53 -03","2022-07-14 16:00:00 -03")
#' Time_Difference("2022-07-14 17:00:00 -03","2022-07-14 16:50:00 -03")
#' Time_Difference("2022-07-14 17:00:00 -03","2022-07-15 16:52:10 -03")
#' Time_Difference(Sys.time(),"2022-07-15 17:00:53 -03")
#'
#' @import stringr
#'
#' @encoding UTF-8
#' @export

Time_Difference<-function(end,start){
  dif=difftime(end, start, units = "secs")[[1]]
  horas=(dif/60)/60
  if(stringr::str_detect(horas,"[.]")){minutos=as.numeric(paste0("0.",stringr::str_remove(sub("*..", "", as.character(horas,1)),"[.]") ))*60}else{minutos=0}
  if(stringr::str_detect(minutos,"[.]")){segundos=as.numeric(paste0("0.",stringr::str_remove(sub("*..", "", as.character(minutos,1)),"[.]") ))*60}else{segundos=0};if(horas==0 & minutos==0){segundos=dif}
  return(paste(trunc(horas),"horas,",trunc(minutos),"minutos e",segundos,"segundos"))
}

