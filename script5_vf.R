getwd()
setwd("C:/Users/ecger/Desktop/Tesis_definitivo")
library(dplyr)
library(forecast)
library(openxlsx)
library(nowcasting)
library(Metrics)

#Cierre de mes, estadísticas monetarias------------------------
monetarias_bcu=openxlsx::read.xlsx("Variables.xlsx", sheet = 2, startRow=1, colNames=TRUE)

bcu=ts(monetarias_bcu[,2:18], start=c(2011, 01), end=c(2021, 09), frequency = 12)
bcu=window(bcu, end=c(2020,12))
dim(bcu)

#Delay bcu--------------------------------

delaybcu=vector("list", (length(monetarias_bcu)-1))
delaybcu=unlist(delaybcu)
delaybcu[1:(length(monetarias_bcu)-1)]=0


#Estandarización de las variables del nuevo bloque----------------------
media_bcu=vector("list", length(ncol(bcu)))
varianza_bcu=vector("list", length(ncol(bcu)))
desvio_bcu=vector("list", length(ncol(bcu)))
base_centrada_bcu=vector("list", length(ncol(bcu)))

for (i in 1:ncol(bcu)){
  media_bcu[[i]]=mean(bcu[,i], na.rm = TRUE)
  varianza_bcu[[i]]=var(bcu[,i], na.rm = TRUE)
  desvio_bcu[[i]]=sqrt(varianza_bcu[[i]])
  print(paste("Estandarizando", colnames(bcu)[i]))
  base_centrada_bcu[[i]]=(bcu[,i]-media_bcu[[i]])/desvio_bcu[[i]]
}
bcu_c=data.frame(matrix(unlist(base_centrada_bcu),nrow=nrow(bcu), ncol = ncol(bcu), byrow=F))
bcu_c_ts=ts(bcu_c, start = c(2011,01), frequency = 12)
colnames(bcu_c_ts)=colnames(bcu)

#Autoarima BCU---------------------------------------------

VarARIMA_bcu=vector("list", length(monetarias_bcu)-1)
#series_c=vector("list", length(base_df))

j=0
for (h in 1:(length(monetarias_bcu)-1)){
  j=j+1
  print(paste("Modelando serie",j))
  VarARIMA_bcu[[h]]=auto.arima(bcu_c_ts[,h],test = "adf")
}

VarARIMA_bcu



urfin=vector("list", (length(monetarias_bcu)-1))
surfin=vector("list", (length(monetarias_bcu)-1))

for (l in 1:(length(monetarias_bcu)-1)){
  
  urfin[[l]]=VarARIMA_bcu[[l]]$arma[6]
  surfin[[l]]=VarARIMA_bcu[[l]]$arma[7]
}

raiz_1_bcu=unlist(urfin)
raiz_12_bcu=unlist(surfin)

#Creo el vector de transformaciones de las variables financieras-------------

#ur=sur=0 trans=0
#ur=sur=1 trans=4
#ur=0 sur=1 trans=5
#ur=1 sur=0 trans=2

transbcu=vector("list", (length(monetarias_bcu)-1))

for (i in 1:(length(monetarias_bcu)-1)){
  
  if (raiz_1_bcu[i]==raiz_12_bcu[i]){
    if (raiz_1_bcu[i]==0){
      print("Transformación 0")
      transbcu[[i]]=0
    }
    else if(raiz_1_bcu[i]==1){
      print("Transformación 4")
      transbcu[[i]]=4
    }
  }
  
  if(raiz_1_bcu[i]!=raiz_12_bcu[i]){
    if(raiz_1_bcu[i]==1){
      print("Transformación 2")
      transbcu[[i]]=2
    }
    else if(raiz_1_bcu[i]==0){
      print("Transformación 5")
      transbcu[[i]]=5
    }
  }  
} 
transbcu
transformaciones_bcu=unlist(transbcu)
raiz_1_bcu
raiz_12_bcu
transformaciones_bcu
#Balanceo la porción del panel correspondiente a las variables financieras----

bcuM10 <- Bpanel(base = bcu_c_ts, trans = transformaciones_bcu, NA.replace = F, aggregate = F, na.prop = 1)


#uno los bloques tradicionales y google al de variables monetarias del bcu---------------------------
x=colnames(base)
base_3bloques=cbind(datosM1_EM, bcuM10)
colnames(base_3bloques)[1]="ivf_m"
colnames(base_3bloques)[2:length(x)]=x[2:length(x)]
colnames(base_3bloques)[(length(x)+1):ncol(base_3bloques)]=colnames(bcu)



#Genero la matriz de bloques-------------------------------------------

#El bloque de las variables tradicionales también es integrado por el IVF


bloque_tradicionales=vector("list", ncol(base_3bloques))
bloque_google=vector("list", ncol(base_3bloques))
bloque_bcu=vector("list", ncol(base_3bloques))

bloque_tradicionales[1:(length(nombrestrad)+1)]=1
bloque_tradicionales[(length(nombrestrad)+2):ncol(base_3bloques)]=0
bloque_tradicionales=unlist(bloque_tradicionales)

bloque_google[1:(length(nombrestrad)+1)]=0
bloque_google[(length(nombrestrad)+2):((length(nombrestrad)+2)+length(gt_nombres))]=1
bloque_google[(((length(nombrestrad)+2)+length(gt_nombres))+1):ncol(base_3bloques)]=0
bloque_google=unlist(bloque_google)

bloque_bcu[1:((length(nombrestrad)+2)+length(gt_nombres))]=0
bloque_bcu[(((length(nombrestrad)+2)+length(gt_nombres))+1):ncol(base_3bloques)]=1
bloque_bcu=unlist(bloque_bcu)


bloques_3=data.frame(bloque_tradicionales, bloque_google, bloque_bcu)
#Vector de retrasos de publicación en días-----------------------


delay_3bl=c(delay,delaybcu)

#vector de frecuencia 3 bloques-----------------------

frecuencia_3=c(4, rep(12, (ncol(base_3bloques))-1))

#REVISAR ELIMINACIÓN (por duplicación)------------------------------------------------
#install.packages("graphics")
library(graphics)

ivf_2021=openxlsx::read.xlsx("IVF.xlsx", sheet = 1, startRow=1, colNames=TRUE)
ivf_2021=ivf_2021[,2]


ivf_2021_ts=ts(ivf_2021, start=c(2004, 01), end=c(2021, 02), frequency = 4)
ivf_2021_ts=window(ivf_2021_ts, start=c(2011, 01), end=c(2021, 02), frequency = 4)


ivf_2021_ts_c=(ivf_2021_ts-media_IVF)/desvío_IVF[[1]]

ivf_oos=diff(diff(ivf_2021_ts_c,4))



#Agrego bloque de empleo-----------------------------------------
#'Como delay de referencia para este bloque se considerará el promedio
#'de días de retraso con el que fueron publicados los informes mensuales
#'entre setiembre de 2019 y diciembre de 2020 (47 días). A la actualidad dicho retraso
#'se ha visto sistemáticamente disminuido hasta el último disponible a noviembre
#'de 2021 (37 días).

ine=openxlsx::read.xlsx("Variables.xlsx", sheet = 3, startRow=1, colNames=TRUE)

ine_ts=ts(ine, start=c(2006, 01), end=c(2020, 12), frequency = 12)
ine_ts=window(ine_ts,start=c(2011, 01), end=c(2020,12))
dim(ine_ts)

#Delay ine--------------------------------

delayine=rep(47, ncol(ine_ts))

#Centramiento del nuevo bloque------------------------
media_ine=vector("list", length(ncol(ine_ts)))
varianza_ine=vector("list", length(ncol(ine_ts)))
desvio_ine=vector("list", length(ncol(ine_ts)))
base_centrada_ine=vector("list", length(ncol(ine_ts)))

for (i in 1:ncol(ine_ts)){
  media_ine[[i]]=mean(ine_ts[,i], na.rm = TRUE)
  varianza_ine[[i]]=var(ine_ts[,i], na.rm = TRUE)
  desvio_ine[[i]]=sqrt(varianza_ine[[i]])
  print(paste("Centrando", colnames(ine_ts)[i]))
  base_centrada_ine[[i]]=(ine_ts[,i]-media_ine[[i]])/desvio_ine[[i]]
}
ine_c=data.frame(matrix(unlist(base_centrada_ine),nrow=nrow(ine_ts), ncol = ncol(ine_ts), byrow=F))
ine_c_ts=ts(ine_c, start = c(2011,01), frequency = 12)
colnames(ine_c_ts)=colnames(ine_ts)

#Autoarima INE---------------------------------------------

VarARIMA_ine=vector("list", ncol(ine_ts))
#series_c=vector("list", length(base_df))

j=0
for (h in 1:(ncol(ine_ts))){
  j=j+1
  print(paste("Modelando serie",j))
  VarARIMA_ine[[h]]=auto.arima(ine_c_ts[,h],test = "adf")
}

VarARIMA_ine



urine=vector("list", ncol(ine_ts))
surine=vector("list", ncol(ine_ts))

for (l in 1:ncol(ine_ts)){
  
  urine[[l]]=VarARIMA_ine[[l]]$arma[6]
  surine[[l]]=VarARIMA_ine[[l]]$arma[7]
}

raiz_1_ine=unlist(urine)
raiz_12_ine=unlist(surine)

#Creo el vector de transformaciones de las variables del bloque ine-------------

#ur=sur=0 trans=0
#ur=sur=1 trans=4
#ur=0 sur=1 trans=5
#ur=1 sur=0 trans=2

transine=vector("list", ncol(ine_ts))

for (i in 1:ncol(ine_ts)){
  
  if (raiz_1_ine[i]==raiz_12_bcu[i]){
    if (raiz_1_ine[i]==0){
      transine[[i]]=0
    }
    else if(raiz_1_ine[i]==1){
      transine[[i]]=4
    }
  }
  
  if(raiz_1_ine[i]!=raiz_12_ine[i]){
    if(raiz_1_ine[i]==1){
      transine[[i]]=2
    }
    else if(raiz_1_ine[i]==0){
      transine[[i]]=5
    }
  }  
} 
transine
transformaciones_ine=unlist(transine)
raiz_1_ine
raiz_12_ine
transformaciones_ine
#Balanceo la porción del panel correspondiente a las variables del ine----

ine_b <- Bpanel(base = ine_c_ts, trans = transformaciones_ine, NA.replace = F, aggregate = F, na.prop = 1)

x=colnames(base)
base_4bloques=cbind(base_3bloques, ine_b)
colnames(base_4bloques)[1:ncol(base_3bloques)]=colnames(base_3bloques)
colnames(base_4bloques)[(ncol(base_3bloques)+1):ncol(base_4bloques)]=colnames(ine_c_ts)


#Nowcasting sin variables de Google------------------------------------


#'Elimino el bloque dedicado a los índices de búsqueda y chequeo el ajuste de
#'los modelos resultantes.

#Datos sin google-------------------------------------------------------

base_sin_g_3=base_4bloques[,-((length(nombrestrad)+2):((length(nombrestrad)+1)+length(nombres_gt)))]

#Matriz de Bloques (incluye bloques: tradicionales, bcu, ine. Excluye Google)------------------------------------------------------

bloque_tradicionales_3s=vector("list", ncol(base_sin_g_3))
bloque_bcu_3s=vector("list",  ncol(base_sin_g_3))
bloque_ine_3s=vector("list",  ncol(base_sin_g_3))

#'dentro del bloque de tradicionales aquí se incluye en PIB
bloque_tradicionales_3s[1:(length(nombrestrad)+1)]=1
bloque_tradicionales_3s[(length(nombrestrad)+2):(ncol(base_sin_g_3))]=0
bloque_tradicionales_3s=unlist(bloque_tradicionales_3s)

bloque_bcu_3s[1:(length(nombrestrad)+1)]=0
bloque_bcu_3s[(length(nombrestrad)+2):(ncol(base_sin_g_3)-length(ine))]=1
bloque_bcu_3s[(ncol(base_sin_g_3)-length(ine)+1):ncol(base_sin_g_3)]=0

bloque_bcu_3s=unlist(bloque_bcu_3s)


bloque_ine_3s[1:(ncol(base_sin_g_3)-length(ine))]=0
bloque_ine_3s[(ncol(base_sin_g_3)-length(ine)+1):ncol(base_sin_g_3)]=1


bloque_ine_3s=unlist(bloque_ine_3s)

bloques_sin_google_3=data.frame(bloque_tradicionales_3s, bloque_bcu_3s, bloque_ine_3s)



#frecuencia--------------------

frecuencia_sin=c(4, rep(12, ncol(base_sin_g_3)-1))

#Delays---------------------------------------------------------------

dias
delay_tradicionales=unlist(dias)
delaybcu

delay_M4M6=c(delay_tradicionales, delaybcu, delayine)



#Serie de modelos con 3 bloques excluyendo bloque Google-----------------


modelos_M4M6 <- mclapply(1:3, function(x) {nowcast(formula = ivf_m ~ ., data = base_sin_g_3, r = 1, p = x, 
                                                   method = "EM", blocks = bloques_sin_google_3, frequency = frecuencia_sin)})

#Guardo los nowcasts de los 3 primeros modelos.
resultados_M4M6=mclapply(1:3, function(x) {modelos_M4M6[[x]]$yfcst})
ivf_oos

#RMSE M4 a M6----------------------------

#Dentro de la muestra----------

EnMuestra_M4M6=mclapply(1:3, function(x) {window(resultados_M4M6[[x]][,2], start=c(2012,02), end=c(2020,4))})

resultados_M4M6_dentro=mclapply(1:3, function(x){rmse(window(ivf_oos, end=c(2020,04)), EnMuestra_M4M6[[x]])})

#Fuera de la muestra-----------------

FueraMuestra_M4M6=mclapply(1:3, function(x) {window(resultados_M4M6[[x]][,3], start=c(2021,01), end=c(2021,01))})

resultados_M4M6_fuera=mclapply(1:3, function(x){rmse(window(ivf_oos, start=c(2021,01), end=c(2021,01)), FueraMuestra_M4M6[[x]])})



#Grafico de los Nowcastings----------------------------------------

par(mfrow = c(1,1))

graficos_M4M6_ref=mclapply(1:3, function(x) {nowcast.plot(window(modelos_M4M6[[x]]))})

#Modelos 7 a 9 (grupo de modelos con 2 bloques: excluye INE y Google)---------
#Matriz de bloques excluyendo Google e ine (Modelos 7 a 9)-------------------------
#'Datos

base_sin_g_2=base_sin_g_3[,-((ncol(base_sin_g_3)-length(ine)+1):ncol(base_sin_g_3))]


bloque_tradicionales_2s_1=bloque_tradicionales_3s[-((ncol(base_sin_g_3)-length(ine)+1):ncol(base_sin_g_3))]
bloque_bcu_2s_1=bloque_bcu_3s[-((ncol(base_sin_g_3)-length(ine)+1):ncol(base_sin_g_3))]

bloques_sin_google_2_v1=data.frame(bloque_tradicionales_2s_1, bloque_bcu_2s_1)

dislay_2bloques_1=c(dias, delaybcu)
frecuenciaM7M9=c(4, rep(12,(ncol(base_sin_g_2)-1)))


modelos_M7M9 <- mclapply(1:3, function(x) {nowcast(formula = ivf_m ~ ., data = base_sin_g_2, r = 1, p = x, 
                                                   method = "EM", blocks = bloques_sin_google_2_v1, frequency = frecuenciaM7M9)})

#Guardo los nowcasts de los 3 primeros modelos.
resultados_M7M9=mclapply(1:3, function(x) {modelos_M7M9[[x]]$yfcst})
ivf_oos

#RMSE M7 a M9----------------------------

#Dentro de la muestra----------

EnMuestra_M7M9=mclapply(1:3, function(x) {window(resultados_M7M9[[x]][,2], start=c(2012,02), end=c(2020,4))})

resultados_M7M9_dentro=mclapply(1:3, function(x){rmse(window(ivf_oos, end=c(2020,04)), EnMuestra_M7M9[[x]])})

#Fuera de la muestra-----------------

FueraMuestra_M7M9=mclapply(1:3, function(x) {window(resultados_M7M9[[x]][,3], start=c(2021,01), end=c(2021,01))})

resultados_M7M9_fuera=mclapply(1:3, function(x){rmse(window(ivf_oos, start=c(2021,01), end=c(2021,01)), FueraMuestra_M7M9[[x]])})



#Grafico de los Nowcastings----------------------------------------

par(mfrow = c(1,1))

graficos_M7M9_ref=mclapply(1:3, function(x) {nowcast.plot(window(modelos_M7M9[[x]]))})



#Modelos 10 a 12 (grupo de modelos con 2 bloques: excluye bcu y Google)---------
#'incluye únicamente tradicionales e ine

#'Datos

base_M10M12=base_sin_g_3[,-((length(nombrestrad)+2):(ncol(base_sin_g_3)-length(ine)))]


bloque_tradicionales_M10M12_=bloque_tradicionales_3s[-((length(nombrestrad)+2):(ncol(base_sin_g_3)))]
bloque_tradicionales_M10M12=c(bloque_tradicionales_M10M12_,rep(0, length(ine)))
bloque_ine_M10M12_=rep(0, (length(nombrestrad)+1))

bloque_ine_M10M12=c(bloque_ine_M10M12_, rep(1, length(ine)))

bloques_M10M12=data.frame(bloque_tradicionales_M10M12, bloque_ine_M10M12)

dislay_M10M12=c(dias, delayine)

frecuenciaM10M12=c(4, rep(12,(ncol(base_M10M12)-1)))

#Estimo Modelos 10 al 12----------------------------------------------------


modelos_M10M12 <- mclapply(1:3, function(x) {nowcast(formula = ivf_m ~ ., data = base_M10M12, r = 1, p = x, 
                                                   method = "EM", blocks = bloques_M10M12, frequency = frecuenciaM10M12)})

#Guardo los nowcasts de los 3 primeros modelos.
resultados_M10M12=mclapply(1:3, function(x) {modelos_M10M12[[x]]$yfcst})

#RMSE M10 a M12----------------------------

#Dentro de la muestra----------

EnMuestra_M10M12=mclapply(1:3, function(x) {window(resultados_M10M12[[x]][,2], start=c(2012,02), end=c(2020,4))})

resultados_M10M12_dentro=mclapply(1:3, function(x){rmse(window(ivf_oos, end=c(2020,04)), EnMuestra_M10M12[[x]])})

#Fuera de la muestra-----------------

FueraMuestra_M10M12=mclapply(1:3, function(x) {window(resultados_M10M12[[x]][,3], start=c(2021,01), end=c(2021,01))})

resultados_M10M12_fuera=mclapply(1:3, function(x){rmse(window(ivf_oos, start=c(2021,01), end=c(2021,01)), FueraMuestra_M10M12[[x]])})



#Grafico de los Nowcastings----------------------------------------

par(mfrow = c(1,1))

graficos_M10M12_ref=mclapply(1:3, function(x) {nowcast.plot(window(modelos_M10M12[[x]]))})

#Modelos 13 al 15 (con los 4 bloques)-------------------------------------

base_4bloques

#genero los bloques

bloques_google_4_=rep(1, length(nombres_gt))
bloques_google_4__=rep(0, (length(nombrestrad)+1))
bloques_google_4_2=rep(0, (length(monetarias_bcu)-1+length(ine)))
bloques_google_4=c(bloques_google_4__,bloques_google_4_,bloques_google_4_2)


bloques_tr_4_=rep(0, length(nombres_gt))
bloques_tr_4__=rep(1, (length(nombrestrad)+1))
bloques_tr_4_2=rep(0, (length(monetarias_bcu)-1+length(ine)))
bloques_tr_4=c(bloques_tr_4__,bloques_tr_4_,bloques_tr_4_2)


bloques_ine_4_=rep(1, length(ine))
bloques_ine_4__=rep(0, (ncol(base_4bloques)-length(ine)))
bloques_ine_4=c(bloques_ine_4__,bloques_ine_4_)


bloques_bcu_4_=rep(0, length(nombres_gt))
bloques_bcu_4__=rep(0, (length(nombrestrad)+1))
bloques_bcu_4_2=rep(1, (length(monetarias_bcu)-1))
bloques_bcu_4_3=rep(0, (length(ine)))

bloques_bcu_4=c(bloques_bcu_4__, bloques_bcu_4_,bloques_bcu_4_2,bloques_bcu_4_3)

bloques4=data.frame(bloques_tr_4,bloques_google_4,bloques_bcu_4,bloques_ine_4)

frecuenciaM13M15=c(4, rep(12, (ncol(base_4bloques)-1)))

modelos_M13M15 <- mclapply(1:3, function(x) {nowcast(formula = ivf_m ~ ., data = base_4bloques, r = 1, p = x, 
                                                   method = "EM", blocks = bloques4, frequency = frecuenciaM13M15)})


resultados_M13M15=mclapply(1:3, function(x) {modelos_M13M15[[x]]$yfcst})

#Dentro de la muestra----------

EnMuestra_M13M15=mclapply(1:3, function(x) {window(resultados_M13M15[[x]][,2], start=c(2012,02), end=c(2020,4))})

resultados_M13M15_dentro=mclapply(1:3, function(x){rmse(window(ivf_oos, end=c(2020,04)), EnMuestra_M13M15[[x]])})

#Fuera de la muestra-----------------

FueraMuestra_M13M15=mclapply(1:3, function(x) {window(resultados_M13M15[[x]][,3], start=c(2021,01), end=c(2021,01))})

resultados_M13M15_fuera=mclapply(1:3, function(x){rmse(window(ivf_oos, start=c(2021,01), end=c(2021,01)), FueraMuestra_M13M15[[x]])})


View(modelos_M10M12[[3]]$xfcst)
modelos_M13M15[[3]]$yfcst
View(modelos_M13M15[[2]]$xfcst)
