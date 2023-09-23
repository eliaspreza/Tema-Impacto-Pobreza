
#--------------------------------------------------------------------------
#=======================Impacto en pobreza por Remesas
#=======================Índices de Pobreza FGT
#---------------------------------------------------------------------------

#----------Librerias

library(tidyverse)
library(plotly)
library(readr)
library(haven)
library(sjmisc)#-para frecuencias y factores


#===========================================================
#-------Abriri las base
#==========================================================

base<-read_spss("F:/CONSULTORIAS/IdeaData/Temas Investigacion/Tema Impacto Pobreza/Bases/HOGARES_EHPM 2018_2.sav")

colnames(base)
str(base)

baseMuni<-read.csv("F:/CONSULTORIAS/IdeaData/Temas Investigacion/Tema Impacto Pobreza/Bases/BaseMuniEHPM_50.csv",header=T,
                   sep=";",dec=".")

base$codigomunic<-as.numeric(base$codigomunic)

base<-full_join(base,baseMuni,by=c("codigomunic"="codigo"))

#--------------------Costos de las canastas de mercado
#-----------CBA_Urbana 53.40 (106.8), CBA_Rural 34.03 (68.03)
#-----------------------------------------------------

#---realizando un subset
base2<-base%>%
       dplyr::select(KEY_PERSONA,KEY_HOGAR,fac00,area,region,codigomunic,Departamento,Municipio,COD_MUN4,r004,
                     autorrepresentado,pobreza,miemh,ingfa,ingpe,totayuda)

colnames(base2)



#----Comprobando niveles de pobreza

#---forma 1
frq(base2$pobreza,weights=base2$fac00)

#---forma 2

PobrezaEHPM<-base2%>%
  dplyr::select(pobreza,fac00)%>%
  dplyr::group_by(pobreza)%>%
  dplyr::summarise(Hogares=sum(fac00),Porcentaje=(Hogares/sum(base2$fac00))*100)

PobrezaEHPM

#--------Replicando la pobreza con los valore de la canasta

pobreza<-base2%>%
          dplyr::select(area,ingfa,ingpe,miemh,fac00)%>%
          dplyr::mutate(z_u1=53.40,z_u2=106.8,z_r1=34.03,z_r2=68.03)%>%
          dplyr::mutate(Pobreza=if_else(area=="1"&ingpe< z_u1,1,if_else(area=="1"&ingpe>=z_u1&ingpe<z_u2,2,
                                if_else(area=="0"&ingpe<z_r1,1, if_else(area=="0"&ingpe>=z_r1&ingpe<z_r2,2,3) ) ) ) )%>%
          dplyr::group_by(Pobreza)%>%
          dplyr::summarise(Hogares=sum(fac00),Personas=sum(miemh*fac00),Porcentaje_H=(Hogares/sum(base2$fac00))*100)


pobreza

#-----------------------------------------porMuni

#---Hogares en los 50 municipios
HogaresMuni_50<-base2%>%
  dplyr::group_by(Departamento,Municipio)%>%
  dplyr::filter(autorrepresentado==1)%>%
  dplyr::summarise(Hogares=sum(fac00),Porcentaje=(Hogares/sum(base2$fac00))*100)


sum(HogaresMuni_50$Hogares)


#--Pobreza extrema
pobrezaMuni_1<-base2%>%
  dplyr::select(COD_MUN4,Departamento,Municipio,autorrepresentado,area,ingfa,ingpe,miemh,fac00)%>%
  dplyr::mutate(z_u1=53.40,z_u2=106.8,z_r1=34.03,z_r2=68.03)%>%
  dplyr::mutate(Pobreza=if_else(area=="1"&ingpe< z_u1,1,if_else(area=="1"&ingpe>=z_u1&ingpe<z_u2,2,
                                                                if_else(area=="0"&ingpe<z_r1,1, if_else(area=="0"&ingpe>=z_r1&ingpe<z_r2,2,3) ) ) ) )%>%
  dplyr::group_by(COD_MUN4,Departamento,Municipio,Pobreza)%>%
  dplyr::filter(Pobreza==1& autorrepresentado==1)%>%
  dplyr::summarise(Hogares=sum(fac00),Porcentaje=(Hogares/sum(base2$fac00))*100)

pobrezaMuni_1

#--Pobreza relativa
pobrezaMuni_2<-base2%>%
  dplyr::select(COD_MUN4,Departamento,Municipio,autorrepresentado,area,ingfa,ingpe,miemh,fac00)%>%
  dplyr::mutate(z_u1=53.40,z_u2=106.8,z_r1=34.03,z_r2=68.03)%>%
  dplyr::mutate(Pobreza=if_else(area=="1"&ingpe< z_u1,1,if_else(area=="1"&ingpe>=z_u1&ingpe<z_u2,2,
                                                                if_else(area=="0"&ingpe<z_r1,1, if_else(area=="0"&ingpe>=z_r1&ingpe<z_r2,2,3) ) ) ) )%>%
  dplyr::group_by(COD_MUN4,Departamento,Municipio,Pobreza)%>%
  dplyr::filter(Pobreza==2& autorrepresentado==1)%>%
  dplyr::summarise(Hogares=sum(fac00),Porcentaje=(Hogares/sum(base2$fac00))*100)

pobrezaMuni_2

#--No Pobreza 
pobrezaMuni_3<-base2%>%
  dplyr::select(COD_MUN4,Departamento,Municipio,autorrepresentado,area,ingfa,ingpe,miemh,fac00)%>%
  dplyr::mutate(z_u1=53.40,z_u2=106.8,z_r1=34.03,z_r2=68.03)%>%
  dplyr::mutate(Pobreza=if_else(area=="1"&ingpe< z_u1,1,if_else(area=="1"&ingpe>=z_u1&ingpe<z_u2,2,
                                                                if_else(area=="0"&ingpe<z_r1,1, if_else(area=="0"&ingpe>=z_r1&ingpe<z_r2,2,3) ) ) ) )%>%
  dplyr::group_by(COD_MUN4,Departamento,Municipio,Pobreza)%>%
  dplyr::filter(Pobreza==3& autorrepresentado==1)%>%
  dplyr::summarise(Hogares=sum(fac00),Porcentaje=(Hogares/sum(base2$fac00))*100)

pobrezaMuni_3

baseMuniPobreza<-full_join(pobrezaMuni_1,pobrezaMuni_2,by=c("Municipio"="Municipio"))
baseMuniPobreza<-full_join(baseMuniPobreza,pobrezaMuni_3,by=c("Municipio"="Municipio"))

write.csv(baseMuniPobreza,file="F:/CONSULTORIAS/IdeaData/Temas Investigacion/Tema Impacto Pobreza/Bases/MuniPobrezaEHPM_50.csv",fileEncoding="utf-8",row.names = F)

#===================================================================================                       
#--Restando los ingresos de remesas del ingreso total y calculando las perdidas


base2<-base2%>%
dplyr::mutate(ingNoRem=ingfa-totayuda,perdida_1=totayuda-(totayuda*0.36),perdida_2=totayuda-(totayuda*0.12),
              ingfa2=ingNoRem+perdida_1,ingfa3=ingNoRem+perdida_2, ingpe2=(ingfa2)/miemh,ingpe3=(ingfa3)/miemh)#%>%
  


#===================================Replicando la pobreza total con los escenarios

#--Escenario 1

pobreza2<-base2%>%
  dplyr::select(area,ingfa,ingpe,ingpe2,ingpe3,miemh,fac00)%>%
  dplyr::mutate(z_u1=53.40,z_u2=106.8,z_r1=34.03,z_r2=68.03)%>%
  dplyr::mutate(Pobreza2=if_else(area=="1"&ingpe2< z_u1,1,if_else(area=="1"&ingpe2>=z_u1&ingpe2<z_u2,2,
                              if_else(area=="0"&ingpe2<z_r1,1, if_else(area=="0"&ingpe2>=z_r1&ingpe2<z_r2,2,3) ) ) ) )%>%
  dplyr::group_by(Pobreza2)%>%
  dplyr::summarise(Hogares=sum(fac00),Personas=sum(miemh*fac00),Porcentaje_H=(Hogares/sum(base2$fac00))*100)


pobreza2



#--Escenario 2

pobreza3<-base2%>%
  dplyr::select(area,ingfa,ingpe,ingpe2,ingpe3,fac00)%>%
  dplyr::mutate(z_u1=53.40,z_u2=106.8,z_r1=34.03,z_r2=68.03)%>%
  dplyr::mutate(Pobreza3=if_else(area=="1"&ingpe3< z_u1,1,if_else(area=="1"&ingpe3>=z_u1&ingpe3<z_u2,2,
                              if_else(area=="0"&ingpe3<z_r1,1, if_else(area=="0"&ingpe3>=z_r1&ingpe3<z_r2,2,3) ) ) ) )%>%
  dplyr::group_by(Pobreza3)%>%
  dplyr::summarise(Hogares=sum(fac00),Porcentaje=(Hogares/sum(base2$fac00))*100)


pobreza3

#===================================Indices de pobreza FGT escenario actual

#---Brecha

brecha<-base2%>%
        dplyr::mutate(z_u2=106.8)%>%
        dplyr::mutate(BrechaPobreza=((z_u2-ingpe)/z_u2))%>%
        dplyr::group_by(Departamento,Municipio)%>%
        dplyr::filter(BrechaPobreza>0&autorrepresentado==1)%>%
        dplyr::summarise(brecha=sum((BrechaPobreza*fac00)/sum(fac00)),Hogares=sum(fac00),Porcentaje=(Hogares/sum(base2$fac00))*100)
       
brecha

sum(brecha$Hogares)
sum(base2$fac00)

#--severidad

severidad<-base2%>%
  dplyr::mutate(z_u2=106.8)%>%
  dplyr::mutate(Severidad=((z_u2-ingpe)/(z_u2)))%>%
  dplyr::group_by(Departamento,Municipio)%>%
  dplyr::filter(Severidad>0&autorrepresentado==1)%>%
  dplyr::summarise(severidad=sum((Severidad*fac00)/sum(fac00))^2,Hogares=sum(fac00),Porcentaje=(Hogares/sum(base2$fac00))*100)

severidad

sum(severidad$Hogares)
sum(base2$fac00)


MuniFGT<-full_join(baseMuniPobreza,brecha,by=c("Municipio"="Municipio"))
MuniFGT<-full_join(MuniFGT,severidad,by=c("Municipio"="Municipio"))

#---Uniendo las bases por municipio para estimar los FGT normales

write.csv(MuniFGT,file="F:/CONSULTORIAS/IdeaData/Temas Investigacion/Tema Impacto Pobreza/Bases/MuniFGT_50.csv",fileEncoding="utf-8",row.names = F)


#==========================================================================================
#---Escenario 1 de la pobreza para los municipios, uso del ingpe2
#=============================================================================================

#-----------------------------------------porMuni

#--Pobreza extrema
pobrezaMuni_11<-base2%>%
  dplyr::select(COD_MUN4,Departamento,Municipio,autorrepresentado,area,ingfa,ingpe2,miemh,fac00)%>%
  dplyr::mutate(z_u1=53.40,z_u2=106.8,z_r1=34.03,z_r2=68.03)%>%
  dplyr::mutate(Pobreza=if_else(area=="1"&ingpe2< z_u1,1,if_else(area=="1"&ingpe2>=z_u1&ingpe2<z_u2,2,
                                                                if_else(area=="0"&ingpe2<z_r1,1, if_else(area=="0"&ingpe2>=z_r1&ingpe2<z_r2,2,3) ) ) ) )%>%
  dplyr::group_by(COD_MUN4,Departamento,Municipio,Pobreza)%>%
  dplyr::filter(Pobreza==1& autorrepresentado==1)%>%
  dplyr::summarise(Hogares=sum(fac00),Porcentaje=(Hogares/sum(base2$fac00))*100)

pobrezaMuni_11

#--Pobreza relativa
pobrezaMuni_22<-base2%>%
  dplyr::select(COD_MUN4,Departamento,Municipio,autorrepresentado,area,ingfa,ingpe2,miemh,fac00)%>%
  dplyr::mutate(z_u1=53.40,z_u2=106.8,z_r1=34.03,z_r2=68.03)%>%
  dplyr::mutate(Pobreza=if_else(area=="1"&ingpe2< z_u1,1,if_else(area=="1"&ingpe2>=z_u1&ingpe2<z_u2,2,
                                                                if_else(area=="0"&ingpe2<z_r1,1, if_else(area=="0"&ingpe2>=z_r1&ingpe2<z_r2,2,3) ) ) ) )%>%
  dplyr::group_by(COD_MUN4,Departamento,Municipio,Pobreza)%>%
  dplyr::filter(Pobreza==2& autorrepresentado==1)%>%
  dplyr::summarise(Hogares=sum(fac00),Porcentaje=(Hogares/sum(base2$fac00))*100)

pobrezaMuni_22

#--No Pobreza 
pobrezaMuni_33<-base2%>%
  dplyr::select(COD_MUN4,Departamento,Municipio,autorrepresentado,area,ingfa,ingpe2,miemh,fac00)%>%
  dplyr::mutate(z_u1=53.40,z_u2=106.8,z_r1=34.03,z_r2=68.03)%>%
  dplyr::mutate(Pobreza=if_else(area=="1"&ingpe2< z_u1,1,if_else(area=="1"&ingpe2>=z_u1&ingpe2<z_u2,2,
                                                                if_else(area=="0"&ingpe2<z_r1,1, if_else(area=="0"&ingpe2>=z_r1&ingpe2<z_r2,2,3) ) ) ) )%>%
  dplyr::group_by(COD_MUN4,Departamento,Municipio,Pobreza)%>%
  dplyr::filter(Pobreza==3& autorrepresentado==1)%>%
  dplyr::summarise(Hogares=sum(fac00),Porcentaje=(Hogares/sum(base2$fac00))*100)

pobrezaMuni_33

baseMuniPobreza2<-full_join(pobrezaMuni_11,pobrezaMuni_22,by=c("Municipio"="Municipio"))
baseMuniPobreza2<-full_join(baseMuniPobreza2,pobrezaMuni_33,by=c("Municipio"="Municipio"))

write.csv(baseMuniPobreza2,file="F:/CONSULTORIAS/IdeaData/Temas Investigacion/Tema Impacto Pobreza/Bases/MuniPobrezaEHPM_50_2.csv",fileEncoding="utf-8",row.names = F)


#---calculando los FGT en el escenario 1


#---Brecha2

brecha2<-base2%>%
  dplyr::mutate(z_u2=106.8)%>%
  dplyr::mutate(BrechaPobreza=((z_u2-ingpe2)/z_u2))%>%
  dplyr::group_by(Departamento,Municipio)%>%
  dplyr::filter(BrechaPobreza>0&autorrepresentado==1)%>%
  dplyr::summarise(brecha=sum((BrechaPobreza*fac00)/sum(fac00)),Hogares=sum(fac00),Porcentaje=(Hogares/sum(base2$fac00))*100)

brecha2

sum(brecha2$Hogares)
sum(base2$fac00)

#--severidad

severidad2<-base2%>%
  dplyr::mutate(z_u2=106.8)%>%
  dplyr::mutate(Severidad=((z_u2-ingpe2)/(z_u2)))%>%
  dplyr::group_by(Departamento,Municipio)%>%
  dplyr::filter(Severidad>0&autorrepresentado==1)%>%
  dplyr::summarise(severidad=sum((Severidad*fac00)/sum(fac00))^2,Hogares=sum(fac00),Porcentaje=(Hogares/sum(base2$fac00))*100)

severidad2

sum(severidad2$Hogares)
sum(base2$fac00)


MuniFGT2<-full_join(baseMuniPobreza2,brecha2,by=c("Municipio"="Municipio"))
MuniFGT2<-full_join(MuniFGT2,severidad2,by=c("Municipio"="Municipio"))

#---Uniendo las bases por municipio para estimar los FGT normales

write.csv(MuniFGT2,file="F:/CONSULTORIAS/IdeaData/Temas Investigacion/Tema Impacto Pobreza/Bases/MuniFGT_50_2.csv",fileEncoding="utf-8",row.names = F)



