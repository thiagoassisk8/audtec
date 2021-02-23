library(tidyverse)
library(rvest)
library(bizdays)
library(lubridate)

filtro <- function(dado, min) {
  dado[dado$`Dias corridos` == min(dado[dado$`Dias corridos` >= min & dados$`Dias corridos` <= min + 7,][,2]),]

}
#### Carregar dados ####

html_url <- "https://audtecgestao.com.br/capa.asp?infoid=1336"
#link <- paste0("http://www2.bmf.com.br/pages/portal/bmfbovespa/lumis/lum-taxas-referenciais-bmf-ptBR.asp?Data=", data,"&Data1=", format(as.Date(data, format = "%d/%m/%y"), "%Y%m%d"),"&slcTaxa=PRE")


html <- read_html(html_url)

dados <- html %>%
  html_nodes('tr') %>%
  html_text() %>%
  str_replace(",", ".") %>%
  as.numeric() %>%
  matrix(ncol = 3, byrow = TRUE) %>%
  as.data.frame()

names(dados) <- c("Vigência", "Dispositivo legal", "Valor")

rm(list = ls(pattern = "html"))

dia <- bizseq(from = "Julho 1994", to = "Janeiro 2021", cal = "Brazil/ANBIMA") %>%
  last()

if (weekdays(dia) == "segunda-feira") {
  dia <- dia - 3
} else {
  dia <- dia - 1
}

dados <- cbind(dia, dados[,c(1:2)])

#### Formatar dados ####

dia30 <- filtro(dados, 30)
dia60 <- filtro(dados, 60)
dia90 <- filtro(dados, 90)
dia120 <- filtro(dados, 120)
dia180 <- filtro(dados, 180)
dia360 <- filtro(dados, 360)
dia1080 <- filtro(dados, 1080)

dados <- data.frame(atualizada = dia, Valor = dia30[,3], Valor = dia60[,3], Valor = dia90[,3],
                    Valor = dia120[,3], Valor = dia180[,3], Valor = dia360[,3], valor = dia1080[,3])

names(dados) <- c( "atualizada" 
                   ,"Swaps_DI_Pre_30"
                   ,"Swaps_DI_Pre_60"
                   ,"Swaps_DI_Pre_90"
                   ,"Swaps_DI_Pre_120"
                   ,"Swaps_DI_Pre_180"
                   ,"Swaps_DI_Pre_360"
                   ,"Swaps_DI_Pre_1080"
)

remove(list = ls(pattern = "dia"))
