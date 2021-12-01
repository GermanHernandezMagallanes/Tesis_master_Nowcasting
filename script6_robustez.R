
library(parallel)
library(data.table)
library(Metrics)

#Análisis de robustez de las variables de Google Trends (leave one out)
#'Para el siguiente análisis de robustez se procederá a estimar los modelos
#'1 al 6, en repetidas ocasiones, de de forma que en cada repetición se quite
#'una de las variables de Google Trends.

#Leave one out Variables de Google trends-------------------------------
#'Se procede a reestimar los modelos 1 al 12 dejando una variable de 
#'Google Trends por fuera de cada estimación.


gt_nombres_v2=colnames(datosM1_EM)[(length(nombrestrad)+2):ncol(datosM1_EM)]

datosM1_EM

#Grupo de Modelos 1------------------------------

basesM1=mclapply(1:length(gt_nombres), function(x) {
  posicion=which(colnames(datosM1_EM)==gt_nombres[1])
  posicion=posicion-1
  posicion=posicion+x
  datosM1_EM[,-posicion]
  })

bloquesM1=mclapply(1:length(gt_nombres), function(x) {
  posicion=which(colnames(datosM1_EM)==gt_nombres[1])
  posicion=posicion-1
  posicion=posicion+x
  bloques[-posicion,]
  })  

frecuenciaM1=mclapply(1:length(gt_nombres), function(x) {
  posicion=which(colnames(datosM1_EM)==gt_nombres[1])
  posicion=posicion-1
  posicion=posicion+x
  frecuenciaM1_EM[-posicion]
})

j=0
robustezM1=mclapply(1:length(gt_nombres), function(x){
  j=j+x
  print(j)
  nowcast(formula = ivf_m ~ ., data = basesM1[[x]], r = 1, p = 1, 
                                            method = "EM", blocks = bloquesM1[[x]], frequency = frecuenciaM1[[x]])})


#Grupo de modelos 2----------------------------------------------


j=0
robustezM2=mclapply(1:length(gt_nombres), function(x){
  j=j+x
  print(j)
  nowcast(formula = ivf_m ~ ., data = basesM1[[x]], r = 1, p = 2, 
          method = "EM", blocks = bloquesM1[[x]], frequency = frecuenciaM1[[x]])})

#Grupo de modelos 3----------------------------------------------


j=0
robustezM3=mclapply(1:length(gt_nombres), function(x){
  j=j+x
  print(j)
  nowcast(formula = ivf_m ~ ., data = basesM1[[x]], r = 1, p = 3, 
          method = "EM", blocks = bloquesM1[[x]], frequency = frecuenciaM1[[x]])})

#Grupo de modelos 13----------------------------------------------


basesM13=mclapply(1:length(gt_nombres), function(x) {
  posicion=which(colnames(base_4bloques)==gt_nombres[1])
  posicion=posicion-1
  posicion=posicion+x
  base_4bloques[,-posicion]
})

bloquesM13=mclapply(1:length(gt_nombres), function(x) {
  posicion=which(colnames(base_4bloques)==gt_nombres[1])
  posicion=posicion-1
  posicion=posicion+x
  bloques4[-posicion,]
})  

frecuenciaM13=mclapply(1:length(gt_nombres), function(x) {
  posicion=which(colnames(datosM1_EM)==gt_nombres[1])
  posicion=posicion-1
  posicion=posicion+x
  frecuenciaM13M15[-posicion]
})


j=0
robustezM13=mclapply(1:length(gt_nombres), function(x){
  j=j+x
  print(j)
  nowcast(formula = ivf_m ~ ., data = basesM13[[x]], r = 1, p = 1, 
          method = "EM", blocks = bloquesM13[[x]], frequency = frecuenciaM13[[x]])})


#Grupo de modelos 14----------------------------------------------
j=0
robustezM14=mclapply(1:length(gt_nombres), function(x){
  j=j+x
  print(j)
  nowcast(formula = ivf_m ~ ., data = basesM13[[x]], r = 1, p = 2, 
          method = "EM", blocks = bloquesM13[[x]], frequency = frecuenciaM13[[x]])})



#Grupo de modelos 15----------------------------------------------

j=0
robustezM15=mclapply(1:length(gt_nombres), function(x){
  j=j+x
  print(j)
  nowcast(formula = ivf_m ~ ., data = basesM13[[x]], r = 1, p = 3, 
          method = "EM", blocks = bloquesM13[[x]], frequency = frecuenciaM13[[x]])})


#Análisis de resultados----------------------------------------------------

#'Variación del error de medición en la muestra, según variable eliminada.
#'El objetivo será crear una matriz con los RMSE en la muestra.

#'Modelos de referencia (incluyen todas las variables)


M1M3_ref=mclapply(1:3, function(x) {modelos_M1M3[[x]]$yfcst})
M13M15_ref=mclapply(1:3, function(x) {modelos_M13M15[[x]]$yfcst})


M1_muestra_ref=mclapply(1, function(x){window(M1M3_ref[[x]][,2], start=c(2012,02), end=c(2020,4))})
M2_muestra_ref=mclapply(2, function(x) {window(M1M3_ref[[x]][,2], start=c(2012,02), end=c(2020,4))})
M3_muestra_ref=mclapply(3, function(x) {window(M1M3_ref[[x]][,2], start=c(2012,02), end=c(2020,4))})


M13_muestra_ref=mclapply(1, function(x){window(M13M15_ref[[x]][,2], start=c(2012,02), end=c(2020,4))})
M14_muestra_ref=mclapply(2, function(x) {window(M13M15_ref[[x]][,2], start=c(2012,02), end=c(2020,4))})
M15_muestra_ref=mclapply(3, function(x) {window(M13M15_ref[[x]][,2], start=c(2012,02), end=c(2020,4))})



rmse_ref_dentro_M1=mclapply(1, function(x){rmse(window(ivf_oos, start=c(2012,2), end=c(2020,4)), M1_muestra_ref[[x]])})
rmse_ref_dentro_M1=unlist(rmse_ref_dentro_M1)

rmse_ref_dentro_M2=mclapply(1, function(x){rmse(window(ivf_oos, start=c(2012,2), end=c(2020,4)), M2_muestra_ref[[x]])})
rmse_ref_dentro_M2=unlist(rmse_ref_dentro_M2)

rmse_ref_dentro_M3=mclapply(1, function(x){rmse(window(ivf_oos, start=c(2012,2), end=c(2020,4)), M3_muestra_ref[[x]])})
rmse_ref_dentro_M3=unlist(rmse_ref_dentro_M3)

rmse_ref_dentro_M13=mclapply(1, function(x){rmse(window(ivf_oos, start=c(2012,2), end=c(2020,4)), M13_muestra_ref[[x]])})
rmse_ref_dentro_M13=unlist(rmse_ref_dentro_M13)

rmse_ref_dentro_M14=mclapply(1, function(x){rmse(window(ivf_oos, start=c(2012,2), end=c(2020,4)), M14_muestra_ref[[x]])})
rmse_ref_dentro_M14=unlist(rmse_ref_dentro_M14)

rmse_ref_dentro_M15=mclapply(1, function(x){rmse(window(ivf_oos, start=c(2012,2), end=c(2020,4)), M15_muestra_ref[[x]])})
rmse_ref_dentro_M15=unlist(rmse_ref_dentro_M15)


#'Modelos "leave one out" sobre variables de Google

M1_rob=mclapply(1:length(nombres_gt), function(x) {robustezM1[[x]]$yfcst})
M2_rob=mclapply(1:length(nombres_gt), function(x) {robustezM2[[x]]$yfcst})
M3_rob=mclapply(1:length(nombres_gt), function(x) {robustezM3[[x]]$yfcst})

M13_rob=mclapply(1:length(nombres_gt), function(x) {robustezM13[[x]]$yfcst})
M14_rob=mclapply(1:length(nombres_gt), function(x) {robustezM14[[x]]$yfcst})
M15_rob=mclapply(1:length(nombres_gt), function(x) {robustezM15[[x]]$yfcst})




M1_muestra_rob=mclapply(1:length(nombres_gt), function(x){window(M1_rob[[x]][,2], start=c(2012,02), end=c(2020,4))})
M2_muestra_rob=mclapply(1:length(nombres_gt), function(x) {window(M2_rob[[x]][,2], start=c(2012,02), end=c(2020,4))})
M3_muestra_rob=mclapply(1:length(nombres_gt), function(x) {window(M3_rob[[x]][,2], start=c(2012,02), end=c(2020,4))})


M13_muestra_rob=mclapply(1:length(nombres_gt), function(x){window(M13_rob[[x]][,2], start=c(2012,02), end=c(2020,4))})
M14_muestra_rob=mclapply(1:length(nombres_gt), function(x) {window(M14_rob[[x]][,2], start=c(2012,02), end=c(2020,4))})
M15_muestra_rob=mclapply(1:length(nombres_gt), function(x) {window(M15_rob[[x]][,2], start=c(2012,02), end=c(2020,4))})



rmse_rob_dentro_M1=mclapply(1:length(nombres_gt), function(x){rmse(window(ivf_oos, start=c(2012,2), end=c(2020,4)), M1_muestra_rob[[x]])})
rmse_rob_dentro_M1=unlist(rmse_rob_dentro_M1)

rmse_rob_dentro_M2=mclapply(1:length(nombres_gt), function(x){rmse(window(ivf_oos, start=c(2012,2), end=c(2020,4)), M2_muestra_rob[[x]])})
rmse_rob_dentro_M2=unlist(rmse_rob_dentro_M2)

rmse_rob_dentro_M3=mclapply(1:length(nombres_gt), function(x){rmse(window(ivf_oos, start=c(2012,2), end=c(2020,4)), M3_muestra_rob[[x]])})
rmse_rob_dentro_M3=unlist(rmse_rob_dentro_M3)

rmse_rob_dentro_M13=mclapply(1:length(nombres_gt), function(x){rmse(window(ivf_oos, start=c(2012,2), end=c(2020,4)), M13_muestra_rob[[x]])})
rmse_rob_dentro_M13=unlist(rmse_rob_dentro_M13)

rmse_rob_dentro_M14=mclapply(1:length(nombres_gt), function(x){rmse(window(ivf_oos, start=c(2012,2), end=c(2020,4)), M14_muestra_rob[[x]])})
rmse_rob_dentro_M14=unlist(rmse_rob_dentro_M14)

rmse_rob_dentro_M15=mclapply(1:length(nombres_gt), function(x){rmse(window(ivf_oos, start=c(2012,2), end=c(2020,4)), M15_muestra_rob[[x]])})
rmse_rob_dentro_M15=unlist(rmse_rob_dentro_M15)


rmse_muestra <- matrix(0, nrow = 6, ncol = (length(nombres_gt)+1))
colnames(rmse_muestra)[2:(length(nombres_gt)+1)] <- nombres_gt
colnames(rmse_muestra)[1]="Ninguna Exc."
View(rmse_muestra)
row.names(rmse_muestra)=c("M1", "M2", "M3", "M13", "M14", "M15")

rmse_muestra[1,2:ncol(rmse_muestra)]=rmse_rob_dentro_M1
rmse_muestra[2,2:ncol(rmse_muestra)]=rmse_rob_dentro_M2
rmse_muestra[3,2:ncol(rmse_muestra)]=rmse_rob_dentro_M3
rmse_muestra[4,2:ncol(rmse_muestra)]=rmse_rob_dentro_M13
rmse_muestra[5,2:ncol(rmse_muestra)]=rmse_rob_dentro_M14
rmse_muestra[6,2:ncol(rmse_muestra)]=rmse_rob_dentro_M15

rmse_muestra[1,1]=rmse_ref_dentro_M1
rmse_muestra[2,1]=rmse_ref_dentro_M2
rmse_muestra[3,1]=rmse_ref_dentro_M3
rmse_muestra[4,1]=rmse_ref_dentro_M13
rmse_muestra[5,1]=rmse_ref_dentro_M14
rmse_muestra[6,1]=rmse_ref_dentro_M15
  

View(rmse_muestra)

#Error absoluto de predicción a 1 trimestre---------------------------

#'A continuación se replica el anterior ejercicio de construcción de una
#'matriz con los errores absolutos.

error_abs <- matrix(0, nrow = 6, ncol = (length(nombres_gt)+1))
colnames(error_abs)[2:(length(nombres_gt)+1)] <- nombres_gt
colnames(error_abs)[1]="Ninguna Exc."
View(error_abs)
row.names(error_abs)=c("M1", "M2", "M3", "M13", "M14", "M15")

#Modelos de referencia


M1M3_ref=mclapply(1:3, function(x) {modelos_M1M3[[x]]$yfcst})
M13M15_ref=mclapply(1:3, function(x) {modelos_M13M15[[x]]$yfcst})


M1_fuera_ref=mclapply(1, function(x){window(M1M3_ref[[x]][,3], start=c(2021,01), end=c(2021,01))})
M2_fuera_ref=mclapply(2, function(x) {window(M1M3_ref[[x]][,3], start=c(2021,01), end=c(2021,01))})
M3_fuera_ref=mclapply(3, function(x) {window(M1M3_ref[[x]][,3], start=c(2021,01), end=c(2021,01))})


M13_fuera_ref=mclapply(1, function(x){window(M13M15_ref[[x]][,3], start=c(2021,01), end=c(2021,01))})
M14_fuera_ref=mclapply(2, function(x) {window(M13M15_ref[[x]][,3], start=c(2021,01), end=c(2021,01))})
M15_fuera_ref=mclapply(3, function(x) {window(M13M15_ref[[x]][,3], start=c(2021,01), end=c(2021,01))})



rmse_ref_fuera_M1=mclapply(1, function(x){rmse(window(ivf_oos, start=c(2021,1), end=c(2021,1)), M1_fuera_ref[[x]])})
rmse_ref_fuera_M1=unlist(rmse_ref_fuera_M1)

rmse_ref_fuera_M2=mclapply(1, function(x){rmse(window(ivf_oos, start=c(2021,1), end=c(2021,1)), M2_fuera_ref[[x]])})
rmse_ref_fuera_M2=unlist(rmse_ref_fuera_M2)

rmse_ref_fuera_M3=mclapply(1, function(x){rmse(window(ivf_oos, start=c(2021,1), end=c(2021,1)), M3_fuera_ref[[x]])})
rmse_ref_fuera_M3=unlist(rmse_ref_fuera_M3)

rmse_ref_fuera_M13=mclapply(1, function(x){rmse(window(ivf_oos, start=c(2021,1), end=c(2021,1)), M13_fuera_ref[[x]])})
rmse_ref_fuera_M13=unlist(rmse_ref_fuera_M13)

rmse_ref_fuera_M14=mclapply(1, function(x){rmse(window(ivf_oos, start=c(2021,1), end=c(2021,1)), M14_fuera_ref[[x]])})
rmse_ref_fuera_M14=unlist(rmse_ref_fuera_M14)

rmse_ref_fuera_M15=mclapply(1, function(x){rmse(window(ivf_oos, start=c(2021,1), end=c(2021,1)), M15_fuera_ref[[x]])})
rmse_ref_fuera_M15=unlist(rmse_ref_fuera_M15)



#Modelos Leave one out


M1_rob
M2_rob
M3_rob
M13_rob
M14_rob
M15_rob

M1_fuera_rob=mclapply(1:length(nombres_gt), function(x){window(M1_rob[[x]][,3], start=c(2021,01), end=c(2021,01))})
M2_fuera_rob=mclapply(1:length(nombres_gt), function(x) {window(M2_rob[[x]][,3], start=c(2021,01), end=c(2021,01))})
M3_fuera_rob=mclapply(1:length(nombres_gt), function(x) {window(M3_rob[[x]][,3], start=c(2021,01), end=c(2021,01))})


M13_fuera_rob=mclapply(1:length(nombres_gt), function(x){window(M13_rob[[x]][,3], start=c(2021,01), end=c(2021,01))})
M14_fuera_rob=mclapply(1:length(nombres_gt), function(x) {window(M14_rob[[x]][,3], start=c(2021,01), end=c(2021,01))})
M15_fuera_rob=mclapply(1:length(nombres_gt), function(x) {window(M15_rob[[x]][,3], start=c(2021,01), end=c(2021,01))})



rmse_rob_fuera_M1=mclapply(1:length(nombres_gt), function(x){rmse(window(ivf_oos, start=c(2021,1), end=c(2021,1)), M1_fuera_rob[[x]])})
rmse_rob_fuera_M1=unlist(rmse_rob_fuera_M1)

rmse_rob_fuera_M2=mclapply(1:length(nombres_gt), function(x){rmse(window(ivf_oos, start=c(2021,1), end=c(2021,1)), M2_fuera_rob[[x]])})
rmse_rob_fuera_M2=unlist(rmse_rob_fuera_M2)

rmse_rob_fuera_M3=mclapply(1:length(nombres_gt), function(x){rmse(window(ivf_oos, start=c(2021,1), end=c(2021,1)), M3_fuera_rob[[x]])})
rmse_rob_fuera_M3=unlist(rmse_rob_fuera_M3)

rmse_rob_fuera_M13=mclapply(1:length(nombres_gt), function(x){rmse(window(ivf_oos, start=c(2021,1), end=c(2021,1)), M13_fuera_rob[[x]])})
rmse_rob_fuera_M13=unlist(rmse_rob_fuera_M13)

rmse_rob_fuera_M14=mclapply(1:length(nombres_gt), function(x){rmse(window(ivf_oos, start=c(2021,1), end=c(2021,1)), M14_fuera_rob[[x]])})
rmse_rob_fuera_M14=unlist(rmse_rob_fuera_M14)

rmse_rob_fuera_M15=mclapply(1:length(nombres_gt), function(x){rmse(window(ivf_oos, start=c(2021,1), end=c(2021,1)), M15_fuera_rob[[x]])})
rmse_rob_fuera_M15=unlist(rmse_rob_fuera_M15)

#Cargo la matriz error_abs---------------------------------


error_abs[1,2:ncol(error_abs)]=rmse_rob_fuera_M1
error_abs[2,2:ncol(error_abs)]=rmse_rob_fuera_M2
error_abs[3,2:ncol(error_abs)]=rmse_rob_fuera_M3
error_abs[4,2:ncol(error_abs)]=rmse_rob_fuera_M13
error_abs[5,2:ncol(error_abs)]=rmse_rob_fuera_M14
error_abs[6,2:ncol(error_abs)]=rmse_rob_fuera_M15

error_abs[1,1]=rmse_ref_fuera_M1
error_abs[2,1]=rmse_ref_fuera_M2
error_abs[3,1]=rmse_ref_fuera_M3
error_abs[4,1]=rmse_ref_fuera_M13
error_abs[5,1]=rmse_ref_fuera_M14
error_abs[6,1]=rmse_ref_fuera_M15


View(error_abs)

