

#--------------------------------------------------------------------------
#=======================Impacto en pobreza por Remesas
#=======================COVID
#---------------------------------------------------------------------------

#----------Librerias

library(ggplot2)
library(dplyr)
library(readr)
library(ggridges)
library(COVID19)


#.........Actualizando el dato del covid para el mapa en qgis

baseCovid<-covid19(ISO = NULL,start=Sys.Date(),end = Sys.Date())
View(baseCovid)
sum(baseCovid$confirmed)
sum(baseCovid$deaths)
sum(baseCovid$recovered)
sum(baseCovid$tests)
sum(baseCovid$pop)

baseUsa<-covid19("USA",level=3,start="2020-05-08",end = Sys.Date())
View(baseUsa)
sum(baseUsa$confirmed)


baseUsa2<-covid19("USA",level=2,start="2020-05-08",end = Sys.Date())
View(baseUsa2)
sum(baseUsa2$confirmed)

#--------Guardar y abrir el archivo  encsv

write.csv(baseUsa,file="F:/CONSULTORIAS/IdeaData/Temas Investigacion/Tema Impacto Pobreza/Bases/Covid_Usa.csv",fileEncoding="utf-8",row.names = F)

baseUsa<-read.csv("F:/CONSULTORIAS/IdeaData/Temas Investigacion/Tema Impacto Pobreza/Bases/Covid_Usa.csv",header = TRUE)


#---------Cuadros de analisis

colnames(baseUsa)

plot(baseUsa$confirmed)


#----------------Estados
Estados<-baseUsa2%>%
  dplyr::select(state, confirmed, recovered, deaths,pop)%>%
  dplyr::rename(Estado=state)%>%
  dplyr::group_by(Estado)%>%
  dplyr::summarise(Confirmados=sum(confirmed), Recuperados=sum(recovered),Muertes=sum(deaths),Poblacion=sum(pop))%>%
  dplyr::mutate(TasaMuerte=round(Muertes/Poblacion*100,digits=2))%>%
  dplyr::arrange(desc(Confirmados))

Estados<-na.omit(Estados)
sum(Estados$Poblacion)
sum(Estados$Confirmados)
sum(Estados$Muertes)

write.csv(Usa,file="F:/CONSULTORIAS/IdeaData/Temas Investigacion/Tema Impacto Pobreza/Cuadros/EstadosUsa.csv",fileEncoding="utf-8",row.names = F)


#-------------Condados
Condados<-baseUsa%>%
  dplyr::select(state,city, confirmed,recovered, deaths,pop)%>%
  dplyr::rename(Estado=state,Ciudad=city)%>%
  dplyr::group_by(Estado,Ciudad)%>%
  dplyr::summarise(Confirmados=sum(confirmed),Recuperados=sum(recovered),Muertes=sum(deaths),Poblacion=sum(pop))%>%
  dplyr::mutate(TasaMuerte=round(Muertes/Poblacion*100,digits=2))%>%
  dplyr::arrange(desc(Confirmados))

Condados<-na.omit(Condados)
sum(Condados$Poblacion)
sum(Condados$Confirmados)
sum(Condados$Muertes)


write.csv(Usa,file="F:/CONSULTORIAS/IdeaData/Temas Investigacion/Tema Impacto Pobreza/Cuadros/CondadosUsa.csv",fileEncoding="utf-8",row.names = F)


#----------------------modelo Gompertz
library(drc)
library(primer)
library(readxl)
library(stats)
library(kableExtra)


#data.frame de prueba
dataset1 <- read_excel("dataset1.xlsx")
#gráfica de los puntos
plot(dataset1$Time1, dataset1$Response1, xlab = "Tiempo", ylab = "N")



tablamod <- data.frame(
  Modelo = c("Exponencial", "Gompertz", "Logístico", "Log-logístico", "Weibull"),
  Parametro.fct = c("EXD.3()", "G.4()", "L.5()", "LL.5()", "W1.4()")
)
kable(tablamod, caption = "Tabla 1.  Códigos para modelos en fct") %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, width = "10em")

#Exponencial	EXD.3()
#Gompertz	G.4()
#Logístico	L.5()
#Log-logístico	LL.5()
#Weibull	W1.4()

#cálculo de parámetros de cada modelo (repetir c/u) - aqui con Gompertz
result.G <- drm(dataset1$Response1~dataset1$Time1, data = dataset1, fct = G.4())
#parámetros y prueba estadística
summary(result.G)

#gráfica
plot(result.G, xlab = "Tiempo", ylab = "N")

#Por municipio:
# https://drive.google.com/open?id=1AGKlyVPfM-TYphf6aw3pno40sh7AJCtT
#-https://drive.google.com/drive/folders/1ogDPV1gcID0kUihszESo8f1NETdV8gOk
