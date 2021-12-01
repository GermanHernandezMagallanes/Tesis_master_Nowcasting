#Expectation Maximization


#Expectation Maximization

#Modelo 1: Solo tradicionales 2011-2020

setwd("C:/Users/ecger/Desktop/Tesis_definitivo")
getwd()

library(openxlsx)
library(nowcasting)
library(forecast)

#Depuración de la base de datos----------------------------------------


#'MOdifico el formato de datos para hacerlo compatible con el método, 
#'el cual requiere la presencia de NA en los meses donde no se observa el PIB 
#'trimestral


#llamo a ivf_q (diff 1 de la diff 4 del IVF estandarizado)--------------------




ivf_m=qtr2month(ivf_q, reference_month = 3, interpolation = FALSE)
ivf_m

na_ts=c(NA, NA)
df=c(na_ts,ivf_m)


ivf_m=ts(df, start = c(2012,4), end=c(2020,12), frequency =12)

#Llamo al resto de la base-----------------------

#cargo la base (con el pib ya con diferencia anual y trimestral realizadas)

#ivf_m

base_df

tradicionalests
basegt

base=ts(base_df, start=c(2011, 01), end = c(2020, 12), frequency = 12)
#base=window(base, start=c(2012, 01), frequency = 12)

#CAMBIO----------------------------------------------------
base=cbind(ivf_m, base)
#-------------------------------------

colnames(base)[2:ncol(base)]=colnames(base_df)

#Cargo las transformaciones. Agrego la transformación del ivf (0, ya fue transformado)------------------


length(trans)

trans=vector("list", length(ivf)+length(base_df)+1)
trans[1]=0
trans[2:ncol(base)]=transformaciones
trans=unlist(trans)


#Genero la matriz de bloques-------------------------------------------

#El bloque de las variables tradicionales también es integrado por el IVF
bloque_tradicionales=vector("list", ncol(base))
bloque_google=vector("list", ncol(base))

bloque_tradicionales[1:(length(nombrestrad)+1)]=1
bloque_tradicionales[(length(nombrestrad)+2):ncol(base)]=0
bloque_tradicionales=unlist(bloque_tradicionales)

bloque_google[1:(length(nombrestrad)+1)]=0
bloque_google[(length(nombrestrad)+2):ncol(base)]=1
bloque_google=unlist(bloque_google)



bloques=data.frame(bloque_tradicionales, bloque_google)
#Vector de retrasos de publicación en días-----------------------

delay=vector("list", ncol(base))

#En el caso de UBI se toman 0 días de delay porque estoy utilizando el promedio
#mensual a mes cerrado y su publicación es diaria.

dias=list(85, 30, 37, 34, 30, 37, 42, 16, 5, 16, 0)
delay_g=vector("list", length(gt_nombres))
delay_g[1:length(gt_nombres)]=0
delay_g=unlist(delay_g)

for(i in 1:ncol(base)){
  if (i<ncol(base)-length(gt_nombres)+1){
delay[[i]]=dias[[i]]
  }
  else if(i>ncol(base)-length(gt_nombres)){
    delay[[i]]=0
  }
}
delay=unlist(delay)


#Balanceo el panel---------------------------------------------------------------


BloquesM1=bloques

TransM1_EM=trans

BaseM1EM_ts=ts(base, start=c(2011, 01), end=c(2020,12), frequency = 12)

datosM1_EM <- Bpanel(base = BaseM1EM_ts, trans = TransM1_EM, NA.replace = F, na.prop = 1)

delay


frecuenciaM1_EM = c(4, rep(12, (ncol(datosM1_EM)-1)))

