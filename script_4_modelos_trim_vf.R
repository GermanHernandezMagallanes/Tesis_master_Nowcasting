setwd("C:/Users/ecger/Desktop/Tesis_definitivo")
getwd()


library(openxlsx)
library(nowcasting)
library(forecast)
library(Metrics)
library(data.table)



#Resultados-----------------------------------------------------------

#'insumos
datosM1_EM


#Estimación de modelos------------------------------------------------


#Estimo los 3 modelos con 2 bloques (tradicionales y google)----------------------------------------------

library(parallel)
library(data.table)



modelos_M1M3 <- mclapply(1:3, function(x) {nowcast(formula = ivf_m ~ ., data = datosM1_EM, r = 1, p = x, 
                                           method = "EM", blocks = bloques, frequency = frecuenciaM1_EM)})
                 
#Guardo los nowcasts de los 3 primeros modelos.
resultados_M1M3=mclapply(1:3, function(x) {modelos_M1M3[[x]]$yfcst})


#Para realizar el calculo de la RMSE llamo a IVF

ivf

ivf_2021=openxlsx::read.xlsx("IVF_corregido_2016.xlsx", sheet = 3, startRow=1, colNames=TRUE)
ivf_2021_ts=ts(ivf_2021, start=c(2011, 01), end=c(2021, 02), frequency = 4)
ivf_2021_ts_c=(ivf_2021_ts-media_IVF)/desvío_IVF[[1]]


ivf_oos=diff(diff(ivf_2021_ts_c[,2],4))


#RMSE M1 a M3----------------------------

#Dentro de la muestra----------



EnMuestra_M1M3=mclapply(1:3, function(x) {window(resultados_M1M3[[x]][,2], start=c(2012,02), end=c(2020,4))})

resultados_M1M3_dentro=mclapply(1:3, function(x){rmse(window(ivf_oos, end=c(2020,04)), EnMuestra_M1M3[[x]])})

#Fuera de la muestra-----------------

FueraMuestra_M1M3=mclapply(1:3, function(x) {window(resultados_M1M3[[x]][,3], start=c(2021,01), end=c(2021,01))})

resultados_M1M3_fuera=mclapply(1:3, function(x){rmse(window(ivf_oos, start=c(2021,01), end=c(2021,01)), FueraMuestra_M1M3[[x]])})



#Grafico de los Nowcastings----------------------------------------

par(mfrow = c(1,1))

graficos_M1M3_ref=mclapply(1:3, function(x) {nowcast.plot(window(modelos_M1M3[[x]]))})



#Modelo 1 Modelo 1: EM sin agregación. VAR 1 en los factores dinámicos----
delay_n=as.numeric(unlist(delay))
