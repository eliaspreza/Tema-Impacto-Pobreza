
#--------------------------------------------------------------------------
#=======================Impacto en pobreza por Remesas
#=======================Modelo de regresión múltiple
#---------------------------------------------------------------------------

#----------Librerias

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tsbox)
library(plotly)
library(readr)
library(ggridges)
library(GGally)   #----Normalidad
library(psych)    #----Correlaciones
library(MASS)     #----librerias para regresion
library(lmtest)   #----librerias para regresion
library(stats)    #----librerias para regresion
library(Ecdat)    #----librerias para regresion
library(FrF2)     #----librerias para regresion
library(normtest) #----librerias para regresion
library(nortest)  #----librerias para regresion
library(car)      #----librerias para regresion
library(faraway)  #----librerias para regresion
#library(COVID19)
#library(expss)

#===========================================================
#-------Abriri las base
#==========================================================

base <-read.csv("F:/CONSULTORIAS/IdeaData/Temas Investigacion/Tema Impacto Pobreza/Bases/Base.csv",
                header = TRUE,sep=";",dec=".",quote=" \"")

colnames(base)
#-----Extrayendo un sutset para las pruebas

baseModelo<-base%>%
dplyr::select(PIB_Real_USA,Remesas_SV,TasaDesempleo_USA)%>%
dplyr::mutate(LnPIB_USA=log(PIB_Real_USA))%>%
dplyr::mutate(Corr=seq(1:41))


#============================
#-------Corriendo el modelo
#===========================

modelo <- lm(Remesas_SV ~ PIB_Real_USA,data = base) #---modelo 1
modelo <- lm(Remesas_SV ~ PIB_Real_USA+TasaDesempleo_USA,data = base)#---modelo 2
modelo <- lm(Remesas_SV ~ TasaDesempleo_USA,data = base) #---modelo 3
modelo <- lm(Remesas_SV ~ LnPIB_USA,data = baseModelo) #---modelo 4

modelo

coefficients(modelo)
summary(modelo)



#==========================================
#-----Grafica de las variables
#==========================================

##grafico de correlacio


#--Grafico 1

p3 <- ggplot(base, aes(x=base$Remesas_SV, y=base$PIB_Real_USA)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()+
  labs(title ="PIB USA y Remesas (Trim-2010 a Trim 2019)", x = "Remesas US$", y = "PIB US$")
  

plot(p3)

#--Version 2
t<- ggplot(base, aes(Remesas_SV,PIB_Real_USA)) +geom_jitter(colour = "red",size = 2.5,linetype = 3)+geom_smooth()

t+labs(title="PIB de EE.UU. versus Monto de Remesas en Millones de US $",subtitle="BCR y U.S. Government Information (serie en periodos Trimestrales 2010-2020)",
       x="Monto Remesas US$",y="PIB Usa US$",caption = "Elaboración propia con información del Govinfo.gov")+
  #geom_hline(yintercept = media,color="Sky Blue",size=1)+  
  #geom_label(aes(x = 115, y = 17.9, label = "18.9%,04-2020"))+
  #geom_label(aes(x = 10, y = 7.7, label = "Media=7.7 %"))+
  theme_ipsum()



#--Gráfico 2

plot(base$Remesas_SV, base$PIB_Real_USA,
     xlim=c(700,1500) , ylim=c(15000,20000), 
     pch=18, 
     cex=2, 
     col="#69b3a2",
     xlab="value of X", ylab="value of Y",
     main="A simple scatterplot"
)
#abline(h=mean(base$PIB_Real_USA),col="red")
abline(a=0,b=15000)
abline(modelo)

##--Gráfico 3

plot(baseModelo, pch=20 , cex=1.5 , col="#69b3a2")


#---PGráfico 4

multi.hist(x = baseModelo, dcol = c("blue", "red"), dlty = c("dotted", "solid"), main = "")

#---PGráfico 5
ggpairs(baseModelo, lower = list(continuous = "smooth"), diag = list(continuous = "bar"), axisLabels = "none")

#coplot(PIB_Real_USA~Remesas_SV|base)
pairs(baseModelo)
#---PGráfico 7

temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

coeff<-1

ggplot(base, aes(x=Remesas_SV)) +
  
  geom_line( aes(y=PIB_Real_USA), size=2, color=temperatureColor) + 
  geom_line( aes(y=Remesas_SV), size=2, color=priceColor) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Temperature (Celsius °)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., name="Price ($)")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
  ) +
  
  ggtitle("Temperature down, price up")

#============================================
#-----Pruebas del modelo
#============================================

## Correlaciones
round(cor(baseModelo),2)

##Datos del modelo en una sola pantalla

plot(base$Remesas_SV, base$PIB_Real_USA, xlab="Remesas",ylab="PIB_USA")
abline(a=0,b=15000)
abline(regres,lwd=1.5)

par(mfrow=c(2,2))
plot(modelo)

par(mfrow=c(1,1))




## Grafica de los residuos del modelo para comprobar independencia

#-----Calculo de los residuales

fitted(modelo) 
residuals(modelo)
rstandard(modelo)
predict(modelo)


scatterplot (x=baseModelo$Remesas_SV,y=predict(modelo))

plot(predict(modelo))
#---grafico  y pruebas

plot(residuals (modelo)) 
abline(h=0,col="red")
jb.norm.test(residuals(modelo))
shapiro.test(residuals(modelo))
lillie.test(residuals(modelo))# Kolmogorov-Smirnov test
ad.test(residuals(modelo)) # Anderson-Darling test
cvm.test(residuals(modelo)) # Cramer-von Mises test

x <- residuals(modelo)
h <- hist(x, breaks = 50, xlab = "Puntuación", main = "Histogram con curva normal")
xfit <- seq(min(x), max(x), length = 50)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit * diff(h$mids[1:2]) * length(x)
lines(xfit, yfit, col = "blue", lwd = 2)


## Grafica de los residuos estandarizados del modelo para comprobar normalidad

qqnorm(rstandard(modelo)) 
qqline(rstandard(modelo),col="red") 

qqnorm(residuals(modelo)) 
qqline(residuals(modelo),col="red") 

x2 <- base$Remesas_SV
h <- hist(x2, breaks = 50, xlab = "Puntuación", main = "Histogram con curva normal")
xfit <- seq(min(x2), max(x2), length = 50)
yfit <- dnorm(xfit, mean = mean(x2), sd = sd(x2))
yfit <- yfit * diff(h$mids[1:2]) * length(x2)
lines(xfit, yfit, col = "blue", lwd = 2)


## Pruebas de Normalidad

shapiro.test(rstandard(modelo)) # Shapiro-Wilks test
ks.test(rstandard(modelo),"pnorm")# Krus Kar Wallis
lillie.test(rstandard(modelo))# Kolmogorov-Smirnov test
ad.test(rstandard(modelo)) # Anderson-Darling test
cvm.test(rstandard(modelo)) # Cramer-von Mises test

##Grafica para validar homocedasticidad

plot(fitted(modelo), residuals(modelo), ylab="Residuales",xlab = "Valor ajustado")
abline(h=0,col="red")

###Multicolinealidad
vif(modelo)

#----grafica de predicción

scatter.smooth(predict(modelo),baseModelo$Remesas_SV)

scatterplot(baseModelo$Remesas_SV,predict(modelo),
            col="red",
            smooth=list(smoother=quantregLine),
            main="Gráfica del modelo de predicción",xlab="Remesas US$",ylab="Predicción US$")

plot(intervals(modelo))


plot(predict(modelo),type="l")
plot(baseModelo$Remesas_SV,type="l")


#=================================================
#-----Graficas del estudio
#===============================================

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tsbox)
library(plotly)
library(readr)
library(tseries)
library(ggfortify)

baseUmploy<-read.csv("F:/CONSULTORIAS/IdeaData/Temas Investigacion/Tema Impacto Pobreza/Bases/BaseEco.csv",header = TRUE,sep=";",dec=".",quote=" \"")

baseUmploy$Corr<-seq(1:124)






#------Gráfico 1

#----media
media<-mean(baseUmploy$TasaDesempleo)
media

t<- ggplot(baseUmploy, aes(Corr, TasaDesempleo)) +geom_jitter(colour = "red",size = 2.5,linetype = 3)+geom_smooth()

t+labs(title="Tasa de Desempleo Latino en EE.UU.",subtitle="U.S. Bureau of Labor Statistics (serie: ene´10-abr´20)",
       x="Serie",y="Tasa en %",caption = "Elaboración propia con información del BLS")+
  geom_hline(yintercept = media,color="Sky Blue",size=1)+  
  geom_label(aes(x = 115, y = 17.9, label = "18.9%,04-2020"))+
  geom_label(aes(x = 10, y = 7.7, label = "Media=7.7 %"))+
  theme_ipsum()


t<- ggplot(baseUmploy, aes(Corr, TasaDesempleo)) +geom_jitter(colour = "red",linetype = 1,size = 2.5)+geom_smooth()

t+labs(title="Tasa de Desempleo Latino en EE.UU.",subtitle="U.S. Bureau of Labor Statistics (ene´10-abr´20)",
       x="Serie",y="Tasa en %",caption = "Elaboración propia con información del BLS")+
  geom_hline(yintercept = media,color="Sky Blue",size=1)+  
  geom_label(aes(x = 123, y = 17.9, label = "18.9%,04-2020"))+
  geom_label(aes(x = 10, y = 7.7, label = "Media=7.7 %"))+
  theme_modern_rc()

t<- ggplot(baseUmploy, aes(Corr, TasaDesempleo)) +geom_line(colour = "red",linetype = 1,size = 2)
t+labs(title="Tasa de Desempleo Latino en EE.UU.",subtitle="U.S. Bureau of Labor Statistics (ene´10-abr´20)",
       x="Serie",y="Tasa en %",caption = "Elaboración propia con información del BLS")+
  geom_hline(yintercept = media,color="Sky Blue",size=1.5)+  
  geom_label(aes(x = 118, y = 17.9, label = "18.9%,04-2020"))+
  geom_label(aes(x = 10, y = 7.7, label = "Media=7.7 %"))+
  theme_ipsum()



#----Gráfico 2

co2ts<-ts(baseUmploy$TasaDesempleo, start = c(2010,1), frequency = 12)

plot(co2ts,type = "s", main = "Desempleo Hispano",submain="eee",xlab="Período",ylab="Tasa",col = "red",
     lwd = 3,)
abline(h=mean(baseUmploy$TasaDesempleo))



