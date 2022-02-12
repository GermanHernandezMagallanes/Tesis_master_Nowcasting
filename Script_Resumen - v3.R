# Script RESUMEN

#Defino directorio y cargo paquetes-------------------

setwd("C:/Users/afern/Dropbox/Docencia/Tesis German Nowcasting/Series y corridas/Corridas")
getwd()

install.packages(c("forecast","data.table","sweep","nowcasting","ggplot2","dplyr","hrbrthemes","scales"))
install.packages(c("openxlsx","gtrendsR","Rcpp"))
install.packages(c("Metrics","parallel"))
install.packages(c("kableExtra","knitr"))
install.packages("tidyverse")
 
library(data.table)
library(dplyr)
library(forecast)
library(ggplot2)
library(gtrendsR)
library(hrbrthemes)
library(Metrics)
library(nowcasting)
library(openxlsx)
library(parallel)
library(Rcpp)
library(scales)
library(sweep)
library(kableExtra)
library(knitr)
library(tidyverse)

# @@@@@@  SCRIPT 1


gt=list(
  "ofertas","descuentos","mides","trabajo","vacantes","BPS",	"banco de prevision social","crisis","depresion","Argentina","Brasil","dólar",	"dolar","inflacion","miami box","mercado libre","automotoras","autos","automovil","auto","UTE","OSE","Antel","Ancap","el correo","correo uruguayo","El país","el pais","el observador","la mañana","la diaria","montevideo portal")


#El siguiente loop automatiza la descarga de los términos seleccionados.
#Cuenta con un límite de 200 búsquedas diarias, con lo que de correrse más de 3 veces en un día
#habrán errores. Los mismos se solucionan esperando al siguiente día o recurriendo a un VPN.


out <- vector("list", length(gt))
for (i in seq_along(gt)) {
  print(paste("descargando", gt[[i]]))
  out[[i]]=gtrends(gt[[i]] , hl = "es", geo = "UY", time = "2004-01-01 2021-01-31")
  out[[i]]=out[[i]]$interest_over_time$hits
  as.vector(out[[i]])
}

outdf=data.frame(matrix(unlist(out), nrow=length(out[[i]]), ncol = length(gt), byrow=F))
colnames(outdf)=gt
View(outdf)

# ts - comando que transforma a series de tiempo

google_mts=ts(outdf,start = c(2004,01), end=c(2020,12), frequency = 12)
outdfc=data.frame(outdf)
View(google_mts)

# preparando para standarizar variables

media_g=vector("list", length(gt))
varianza_g=vector("list", length(gt))
desvio_g=vector("list", length(gt))
gtc=vector("list", length(gt))
typeof(outdf[[1]])   # muestra qué tipo de variable es


outdfts=ts(outdf, start=c(2004, 01), end=c(2020, 12), frequency=12)
outdfts=window(outdfts, start=c(2011, 01), end=c(2020, 12), frequency=12)   # crea ventana con la muestra a trabajar
outdf=data.frame(outdfts)

for (i in seq_along(gt)){
  outdf[[i]]=as.numeric(outdf[[i]])
  media_g[[i]]=mean(outdf[[i]], na.rm = FALSE)
  varianza_g[[i]]=var(outdf[[i]], na.rm = FALSE)
  desvio_g[[i]]=sqrt(varianza_g[[i]])
  print(paste("Centrando", gt[[i]]))
  gtc[[i]]=(outdf[[i]]-media_g[[i]])/desvio_g[[i]]   # vector con las series standarizadas
}

View(gtc)

outdfc=data.frame(matrix(unlist(gtc), nrow=length(outdf[[i]]), ncol = length(gtc), byrow=F))
colnames(outdfc)=gt
View(outdfc)

#Creo las series de tiempo.

outdfcts=ts(outdfc, start=c(2011, 01), end=c(2020, 12), frequency = 12)

#Debido a la alta proporción de ceros que exhibe la mayoría de las series entre 2004
# y 2010 se decide iniciar el estudio en 2011.

basegt=window(outdfcts, start=c(2011, 01), end=c(2020, 12), frequency=12)

#En caso de considerarse práctico, a continuación se introducen comandos para 
#exportar la información a Excel.

#----------Guardo en excel la base------------
#wb=createWorkbook("Var_google_trends.xlsx")
#addWorksheet(wb, "1")

#writeData(wb, sheet = 1, basegt)

#saveWorkbook(wb, "Var_google_trends.xlsx", overwrite = T)
#------------------------------------------------------



# @@@@@@  SCRIPT 2

# Cargo series de variables tradicionales. Las centro. 

#cargo series google.
#Uno ambas fuentes.

#Loop de gráficos a nivel de cada variable.
#Loop de FAC y FACP de cada serie a nivel.

#Realizo modelado autoarima, establezco vector de transformaciones.
#Ingreso (A mano) vector que establece los delay de publicación de cada variable

#Cargo IVF trimestral. 
#Gráfico, FAC, FACP. Modelo autoarima y diferencio.
#Cargo el bloque de variables tradicionales y las estandarizadas -----------------------------

tradicionales=openxlsx::read.xlsx("Variables.xlsx", sheet = 1, startRow=1, colNames=TRUE)   # 1er hoja del EXCEL

nombrestrad=colnames(tradicionales)

media_t=vector("list", length(nombrestrad))
varianza_t=vector("list", length(nombrestrad))
desvio_t=vector("list", length(nombrestrad))
tradicionalesc=vector("list", length(nombrestrad))

#Na.rm es true para eliminar los NA antes de calcular los momentos muestrales.


for (i in seq_along(nombrestrad)){
  media_t[[i]]=mean(tradicionales[[i]], na.rm = TRUE)
  varianza_t[[i]]=var(tradicionales[[i]], na.rm = TRUE)
  desvio_t[[i]]=sqrt(varianza_t[[i]])
  print(paste("Estandarizando", nombrestrad[[i]]))
  tradicionalesc[[i]]=(tradicionales[[i]]-media_t[[i]])/desvio_t[[i]]
}

trad=data.frame(matrix(unlist(tradicionalesc), nrow=204, ncol = length(nombrestrad), byrow=F))
colnames(trad)=nombrestrad

# Modelado autoarima de las series que integrarán los bloques tradicionales y google-----------------------
# Se excluye IVF, el cual será tratado a parte.

base=vector("list", length(nombrestrad)+length(gt))
tradicionalests=ts(trad, start = c(2004,01), end=c(2020,12), frequency = 12)

tradts=window(tradicionalests, start=c(2011,01), end=c(2020,12), frequency=12)

base=cbind(tradts, basegt)
gt_nombres=unlist(gt)

colnames(base)[1:length(nombrestrad)]=nombrestrad
colnames(base)[(length(nombrestrad)+1):(length(nombrestrad)+length(colnames(outdfc)))]=gt_nombres
base_df=data.frame(base)
base_df_ts=ts(base_df, start = c(2011,01), end=c(2020,12), frequency = 12)


VarARIMA_est_c=vector("list", length(base_df))

j=0
for (i in seq_along(base_df)){
  j=j+1
  print(paste("Modelando serie",j))
  VarARIMA_est_c[[i]]=auto.arima(
    base_df_ts[,i],
    test = "adf")
}

VarARIMA_est_c

#Elaboración del vector de transformaciones para el bloque de tradicionales---------------------------

ur=vector("list", length(base_df))
sur=vector("list", length(base_df))

for (i in seq_along(base_df)){
  
  ur[[i]]=VarARIMA_est_c[[i]]$arma[6]
  sur[[i]]=VarARIMA_est_c[[i]]$arma[7]
}

raiz_1=unlist(ur)
raiz_12=unlist(sur)


#ur=sur=0 trans=0   CODIGOS DEL PAQUETE NOWCAST - 0 estacionaria
#ur=sur=1 trans=4                                 4 diferencia regular y estacional
#ur=0 sur=1 trans=5                               5 diferencia estacional
#ur=1 sur=0 trans=2                               2 diferencia regular

trans=vector("list", length(base_df))

for (i in seq_along(base_df)){
  
  if (raiz_1[i]==raiz_12[i]){
    if (raiz_1[i]==0){
      print("Transformación 0")
      trans[i]=0
    }
    else if(raiz_1[i]==1){
      print("Transformación 4")
      trans[i]=4
    }
  }
  
  if(raiz_1[i]!=raiz_12[i]){
    if(raiz_1[i]==1){
      print("Transformación 2")
      trans[i]=2
    }
    else if(raiz_1[i]==0){
      print("Transformación 5")
      trans[i]=5
    }
  }  
} 
trans
transformaciones=unlist(trans)
raiz_1
raiz_12
transformaciones

View(VarARIMA_est_c)


# IVF------------------------------------
ivf=openxlsx::read.xlsx("IVF.xlsx", sheet = 1, startRow=1, colNames=TRUE)
typeof(ivf)
ivf=data.table(ivf$IVF, keep.rownames = TRUE)
colnames(ivf)="ivf"

date_q=seq(as.Date("2004/3/1"), as.Date("2021/06/30"), by="quarter")
par(mfrow = c(1,1))

#serie a nivel IVF-------

date_q=seq(as.Date("2004/3/1"), as.Date("2021/06/30"), by="quarter")

graficos_c <- 
  print(ggplot(ivf, aes(x=date_q, y=ivf)) +
          geom_area(fill="#2d709f", alpha=0.5)+
          geom_line(color="#2d709f") + 
          xlab(NULL) +
          ylab(NULL)+
          ggtitle(label="Índice de volumen físico (a nivel)", subtitle = "2016 base 100")+
          theme_ipsum() +
          theme(axis.text.x=element_text(angle=-45, hjust=0.5)+scale_x_date(date_labels = "%b-%Y", date_breaks= "1 year")))

par(mfrow = c(1,2))
#Grafico de todas las series --------------------

date_t=seq(as.Date("2011/3/1"), as.Date("2020/12/01"), by="quarter")
data_c=data.frame(date_t, base_df)

nombres_gt=unlist(gt)
nombres_todas=vector("list", ncol(base_df))

nombres_todas[1:length(nombrestrad)]=nombrestrad
nombres_todas[(length(nombrestrad)+1):ncol(base_df)]=nombres_gt
nombres=unlist(nombres_todas)


graf_nivel=vector("list", length(base_df))
graficos=vector("list", length(base_df))

j=0
for (i in seq_along(base_df)){
  j=j+1
  print(paste(j, "Graficando serie", nombres[i]))
  graf_nivel <- print(ggplot(data_c, aes(x=date_t, y=base_df[,i])) +
                        geom_line(color="#2d709f") + 
                        xlab(NULL) +
                        ylab(NULL)+
                        ggtitle(label=nombres[i])+
                        theme_ipsum() +
                        theme(axis.text.x=element_text(angle=90, hjust=0.5))+scale_x_date(date_labels = "%b-%Y", date_breaks= "1 year"))
}

#FAC y FACP IVF----------------------------
fac_ivfq=acf(ts(ivf,f=1), lag.max = 36, na.action = na.pass, main = paste("IVF FAC"))
facp_ivfq=pacf(ts(ivf,f=1), lag.max = 36, na.action = na.pass, main = paste("IVF FACP"))

#centramiento del IVF y conversión a tasas de crecimientos trimestrales anualizadas----

#crecimiento trimestral
#OBS:Las variables manipuladas deben ser ts para continuar.
ivf_ts=ts(ivf, start = c(2004, 01), end=c(2020, 4), frequency = 4)


#FAC y FACP IVF----------------------------
fac_ivfq=acf(ts(ivf,f=1), lag.max = 36, na.action = na.pass, main = paste("IVF FAC"))
facp_ivfq=pacf(ts(ivf,f=1), lag.max = 36, na.action = na.pass, main = paste("IVF FACP"))


#Estandarización del IVF ----------------
#Estadisticas básicas----------
#Los momentos muestrales son calculados en la muestra (2011-2020)

ivf_ts_=window(ivf_ts, start=c(2011,01))


media_IVF=mean(ivf_ts_, na.rm = TRUE)
varianza_IVF=vector("list", length = 1)
varianza_IVF[[1]]=var(ivf_ts_, na.rm = TRUE)


desvío_IVF=sqrt(varianza_IVF[[1]])

ivf_c=(ivf_ts_-media_IVF)/desvío_IVF[[1]]


#Gráfico de la serie ------------------------------

#ivf_c está estandarizada pero aún no es estacionaria.
ivf_c=window(ivf_c, start=c(2011, 01), frequency=4)

date_q_c=seq(as.Date("2011/3/1"), as.Date("2020/12/01"), by="quarter")
date_q_c
datos_q_c=data.frame(date_q_c, ivf_c)
graficos_c_c <- 
  print(ggplot(datos_q_c, aes(x=date_q_c, y=ivf_c)) +
          geom_area(fill="#8AFFCF", alpha=0.5)+
          geom_line(color="#8AFFCF") + 
          xlab(NULL) +
          ylab(NULL)+
          ggtitle(label="IVF estandarizado", subtitle = "2011-2020")+
          theme_ipsum() +
          theme(axis.text.x=element_text(angle=-45, hjust=1.5)+scale_x_date(date_labels = "%b-%Y", date_breaks= "1 year")))

par(mfrow = c(1,2))

fac_cre_IVF_a_c=acf(ts(ivf_c,f=1), lag.max = 36, na.action = na.pass, main = paste("Crecimiento trimestral"))

facp_cre_IVF_a_c=pacf(ts(ivf_c,f=1), lag.max = 36, na.action = na.pass, main = paste("IVF centrado"))

#Autoarima_IVF----------------------------------------------------

ivf_c=ts(ivf_c, start = c(2011,01), end=c(2020,4), frequency = 4)
mod_arima_c=auto.arima(ivf_c, test = "adf")

mod_arima_c
#'del anterior autoarima se desprende la necesidad de realizar diferencia 12 y 1 
#'a la serie de ivf_c con el fin de volverla estacionaria.

ivf_q=diff(diff(ivf_c,4))




# @@@@@@  SCRIPT 3


# Prepara los argumentos para dsps correr los modelos - delay, frecuencias, bloques

# Modelo 1: Solo tradicionales 2011-2020

#Depuración de la base de datos----------------------------------------


# Modifico el formato de datos para hacerlo compatible con el método, 
# el cual requiere la presencia de NA en los meses donde no se observa el PIB 
# trimestral


#llamo a ivf_q (diff 1 de la diff 4 del IVF estandarizado)--------------------
# comando qtr2month - toma serie trimestral y la convierte en mensual, poniendo el dato en mes 2 del trim y llenando restantes con  NA

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

#El bloque de las variables tradicionales también esta integrado por el IVF
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

dias=list(85, 30, 37, 34, 30, 37, 42, 16, 5, 16, 0) # estimacion de German
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


# Balanceo el panel---------------------------------------------------------------
# comando Bpanel - genera panel balanceado para las series en BaseM1EM_ts

BloquesM1=bloques

TransM1_EM=trans

BaseM1EM_ts=ts(base, start=c(2011, 01), end=c(2020,12), frequency = 12)

datosM1_EM <- Bpanel(base = BaseM1EM_ts, trans = TransM1_EM, NA.replace = F, na.prop = 1)

delay

frecuenciaM1_EM = c(4, rep(12, (ncol(datosM1_EM)-1)))




# @@@@@@  SCRIPT 4


#Resultados-----------------------------------------------------------

#'insumos
datosM1_EM


#Estimación de modelos------------------------------------------------


#Estimo los 3 modelos con 2 bloques (tradicionales y google)----------------------------------------------

modelos_M1M3 <- mclapply(1:3, function(x) {nowcast(formula = ivf_m ~ ., data = datosM1_EM, r = 1, p = x, 
                                                   method = "EM", blocks = bloques, frequency = frecuenciaM1_EM)})

# nowcast - calcula los modelos factores dinámicos por el método EM de los bloques tradicional y google
# datosM1_EM - panel balanceado
# mclappy - otra forma de hacer loop

#Guardo los nowcasts de los 3 primeros modelos.

resultados_M1M3=mclapply(1:3, function(x) {modelos_M1M3[[x]]$yfcst})


# Para realizar el calculo de la RMSE llamo a IVF
# Calculo del error para la serie trimestral

ivf

ivf_2021=openxlsx::read.xlsx("IVF_corregido_2016.xlsx", sheet = 1, startRow=1, colNames=TRUE)
ivf_2021_ts=ts(ivf_2021, start=c(2011, 01), end=c(2021, 02), frequency = 4)
ivf_2021_ts_c=(ivf_2021_ts-media_IVF)/desvío_IVF[[1]]


ivf_oos=diff(diff(ivf_2021_ts_c[,2],4))


# RMSE M1 a M3----------------------------

# Dentro de la muestra----------


EnMuestra_M1M3=mclapply(1:3, function(x) {window(resultados_M1M3[[x]][,2], start=c(2012,02), end=c(2020,4))})

resultados_M1M3_dentro=mclapply(1:3, function(x){rmse(window(ivf_oos, end=c(2020,04)), EnMuestra_M1M3[[x]])})

# Fuera de la muestra-----------------

FueraMuestra_M1M3=mclapply(1:3, function(x) {window(resultados_M1M3[[x]][,3], start=c(2021,01), end=c(2021,01))})

resultados_M1M3_fuera=mclapply(1:3, function(x){rmse(window(ivf_oos, start=c(2021,01), end=c(2021,01)), FueraMuestra_M1M3[[x]])})


# Grafico de los Nowcastings----------------------------------------

par(mfrow = c(1,1))

graficos_M1M3_ref=mclapply(1:3, function(x) {nowcast.plot(window(modelos_M1M3[[x]]))})



# Modelo 1 Modelo 1: EM sin agregación. VAR 1 en los factores dinámicos ----

delay_n=as.numeric(unlist(delay))





# @@@@@@  SCRIPT 5

# Cierre de mes, estadísticas monetarias------------------------
monetarias_bcu=openxlsx::read.xlsx("Variables.xlsx", sheet = 2, startRow=1, colNames=TRUE)

bcu=ts(monetarias_bcu[,2:18], start=c(2011, 01), end=c(2021, 09), frequency = 12)
bcu=window(bcu, end=c(2020,12))
dim(bcu)

# Delay bcu--------------------------------

delaybcu=vector("list", (length(monetarias_bcu)-1))
delaybcu=unlist(delaybcu)
delaybcu[1:(length(monetarias_bcu)-1)]=0


# Estandarización de las variables del nuevo bloque----------------------
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

# Autoarima BCU---------------------------------------------

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

# Creo el vector de transformaciones de las variables financieras-------------

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


# uno los bloques tradicionales y google al de variables monetarias del bcu---------------------------

x=colnames(base)
base_3bloques=cbind(datosM1_EM, bcuM10)
colnames(base_3bloques)[1]="ivf_m"
colnames(base_3bloques)[2:length(x)]=x[2:length(x)]
colnames(base_3bloques)[(length(x)+1):ncol(base_3bloques)]=colnames(bcu)



# Genero la matriz de bloques-------------------------------------------

# El bloque de las variables tradicionales también es integrado por el IVF


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

# Vector de retrasos de publicación en días-----------------------
 
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



# Agrego bloque de empleo-----------------------------------------
# 'Como delay de referencia para este bloque se considerará el promedio
# 'de días de retraso con el que fueron publicados los informes mensuales
# 'entre setiembre de 2019 y diciembre de 2020 (47 días). A la actualidad dicho retraso
# 'se ha visto sistemáticamente disminuido hasta el último disponible a noviembre
# 'de 2021 (37 días).

ine=openxlsx::read.xlsx("Variables.xlsx", sheet = 3, startRow=1, colNames=TRUE)

ine_ts=ts(ine, start=c(2006, 01), end=c(2020, 12), frequency = 12)
ine_ts=window(ine_ts,start=c(2011, 01), end=c(2020,12))
dim(ine_ts)

# Delay ine--------------------------------

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

# Autoarima INE---------------------------------------------

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

# Creo el vector de transformaciones de las variables del bloque ine-------------

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

# Balanceo la porción del panel correspondiente a las variables del ine----

ine_b <- Bpanel(base = ine_c_ts, trans = transformaciones_ine, NA.replace = F, aggregate = F, na.prop = 1)

x=colnames(base)
base_4bloques=cbind(base_3bloques, ine_b)
colnames(base_4bloques)[1:ncol(base_3bloques)]=colnames(base_3bloques)
colnames(base_4bloques)[(ncol(base_3bloques)+1):ncol(base_4bloques)]=colnames(ine_c_ts)


# Nowcasting sin variables de Google------------------------------------


# 'Elimino el bloque dedicado a los índices de búsqueda y chequeo el ajuste de
# 'los modelos resultantes.

# Datos sin google-------------------------------------------------------

base_sin_g_3=base_4bloques[,-((length(nombrestrad)+2):((length(nombrestrad)+1)+length(nombres_gt)))]

# Matriz de Bloques (incluye bloques: tradicionales, bcu, ine. Excluye Google)------------------------------------------------------

bloque_tradicionales_3s=vector("list", ncol(base_sin_g_3))
bloque_bcu_3s=vector("list",  ncol(base_sin_g_3))
bloque_ine_3s=vector("list",  ncol(base_sin_g_3))

# 'dentro del bloque de tradicionales aquí se incluye en PIB

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



# frecuencia--------------------

frecuencia_sin=c(4, rep(12, ncol(base_sin_g_3)-1))

# Delays---------------------------------------------------------------

dias
delay_tradicionales=unlist(dias)
delaybcu

delay_M4M6=c(delay_tradicionales, delaybcu, delayine)



# Serie de modelos con 3 bloques excluyendo bloque Google-----------------

modelos_M4M6 <- mclapply(1:3, function(x) {nowcast(formula = ivf_m ~ ., data = base_sin_g_3, r = 1, p = x, 
                                                   method = "EM", blocks = bloques_sin_google_3, frequency = frecuencia_sin)})

# Guardo los nowcasts de los 3 primeros modelos.
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





# @@@@@@  SCRIPT 6


# Análisis de robustez de las variables de Google Trends (leave one out)
# 'Para el siguiente análisis de robustez se procederá a estimar los modelos
# '1 al 6, en repetidas ocasiones, de de forma que en cada repetición se quite
# 'una de las variables de Google Trends.

# Leave one out Variables de Google trends-------------------------------
# 'Se procede a reestimar los modelos 1 al 12 dejando una variable de 
# 'Google Trends por fuera de cada estimación.


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


# Grupo de modelos 2----------------------------------------------


j=0
robustezM2=mclapply(1:length(gt_nombres), function(x){
  j=j+x
  print(j)
  nowcast(formula = ivf_m ~ ., data = basesM1[[x]], r = 1, p = 2, 
          method = "EM", blocks = bloquesM1[[x]], frequency = frecuenciaM1[[x]])})

# Grupo de modelos 3----------------------------------------------


j=0
robustezM3=mclapply(1:length(gt_nombres), function(x){
  j=j+x
  print(j)
  nowcast(formula = ivf_m ~ ., data = basesM1[[x]], r = 1, p = 3, 
          method = "EM", blocks = bloquesM1[[x]], frequency = frecuenciaM1[[x]])})

# Grupo de modelos 13----------------------------------------------


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


# Grupo de modelos 14----------------------------------------------
j=0
robustezM14=mclapply(1:length(gt_nombres), function(x){
  j=j+x
  print(j)
  nowcast(formula = ivf_m ~ ., data = basesM13[[x]], r = 1, p = 2, 
          method = "EM", blocks = bloquesM13[[x]], frequency = frecuenciaM13[[x]])})



# Grupo de modelos 15----------------------------------------------

j=0
robustezM15=mclapply(1:length(gt_nombres), function(x){
  j=j+x
  print(j)
  nowcast(formula = ivf_m ~ ., data = basesM13[[x]], r = 1, p = 3, 
          method = "EM", blocks = bloquesM13[[x]], frequency = frecuenciaM13[[x]])})


# Análisis de resultados----------------------------------------------------

# 'Variación del error de medición en la muestra, según variable eliminada.
# 'El objetivo será crear una matriz con los RMSE en la muestra.

# 'Modelos de referencia (incluyen todas las variables)


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


# 'Modelos "leave one out" sobre variables de Google

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

# Error absoluto de predicción a 1 trimestre---------------------------

# 'A continuación se replica el anterior ejercicio de construcción de una
# 'matriz con los errores absolutos.

error_abs <- matrix(0, nrow = 6, ncol = (length(nombres_gt)+1))
colnames(error_abs)[2:(length(nombres_gt)+1)] <- nombres_gt
colnames(error_abs)[1]="Ninguna Exc."
View(error_abs)
row.names(error_abs)=c("M1", "M2", "M3", "M13", "M14", "M15")

# Modelos de referencia


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


# Modelos Leave one out

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

# Cargo la matriz error_abs---------------------------------

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



# @@@@@@  SCRIPT 7

# RMSE comparado trimestral---------------------------


# 'Creo la matriz de RMSE en la muestra y fuera de la muestra para
# 'los 15 modelos. La matriz contendrá en su primera columna
# 'el error absoluto promedio de predicción dentro de la muestra, 
# 'en tanto la columna dos contendrá el error absoluto de la predicción
# 'del primer trimestre del 2021.


rmse_trimestrales=matrix(0, nrow = 16, ncol = 2)
colnames(rmse_trimestrales)[1:2] = c("RMSE en muestra","Error Absoluto fuera de muestra")
row.names(rmse_trimestrales)=c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10", "M11", "M12", "M13", "M14", "M15", "Arima")

resultados_M7M9_dentro
resultados_M7M9_fuera

resultados_M10M12_dentro
resultados_M10M12_fuera
#En muestra--------------------

rmse_trimestrales[1,1]=rmse_ref_dentro_M1
rmse_trimestrales[2,1]=rmse_ref_dentro_M2
rmse_trimestrales[3,1]=rmse_ref_dentro_M3

rmse_trimestrales[4,1]=resultados_M4M6_dentro[[1]]
rmse_trimestrales[5,1]=resultados_M4M6_dentro[[2]]
rmse_trimestrales[6,1]=resultados_M4M6_dentro[[3]]

rmse_trimestrales[7,1]=resultados_M7M9_dentro[[1]]
rmse_trimestrales[8,1]=resultados_M7M9_dentro[[2]]
rmse_trimestrales[9,1]=resultados_M7M9_dentro[[3]]

rmse_trimestrales[10,1]=resultados_M10M12_dentro[[1]]
rmse_trimestrales[11,1]=resultados_M10M12_dentro[[2]]
rmse_trimestrales[12,1]=resultados_M10M12_dentro[[3]]

rmse_trimestrales[13,1]=rmse_ref_dentro_M13
rmse_trimestrales[14,1]=rmse_ref_dentro_M14
rmse_trimestrales[15,1]=rmse_ref_dentro_M15


# Modelo de referencia: diff(diff(ivf,4))

library(forecast)
library(Metrics)

modelo_referencial=auto.arima(window(ivf_2021_ts_c, end=c(2020,04)))
referencia_autoarima=forecast(modelo_referencial,h=1)
  
dentro_arima=diff(diff(modelo_referencial$fitted,4))
resultados_dentro_arima=rmse(ivf_oos,dentro_arima)
resultados_fuera_arima=rmse(window(ivf_oos, start=c(2021,1), end=c(2021,1)), referencia_autoarima$mean)

rmse_trimestrales[16,1]=resultados_dentro_arima


# Fuera de la muestra--------------------

rmse_trimestrales[1,2]=rmse_ref_fuera_M1
rmse_trimestrales[2,2]=rmse_ref_fuera_M2
rmse_trimestrales[3,2]=rmse_ref_fuera_M3


rmse_trimestrales[4,2]=resultados_M4M6_fuera[[1]]
rmse_trimestrales[5,2]=resultados_M4M6_fuera[[2]]
rmse_trimestrales[6,2]=resultados_M4M6_fuera[[3]]


rmse_trimestrales[7,2]=resultados_M7M9_fuera[[1]]
rmse_trimestrales[8,2]=resultados_M7M9_fuera[[2]]
rmse_trimestrales[9,2]=resultados_M7M9_fuera[[3]]


rmse_trimestrales[10,2]=resultados_M10M12_fuera[[1]]
rmse_trimestrales[11,2]=resultados_M10M12_fuera[[2]]
rmse_trimestrales[12,2]=resultados_M10M12_fuera[[3]]


rmse_trimestrales[13,2]=rmse_ref_fuera_M13
rmse_trimestrales[14,2]=rmse_ref_fuera_M14
rmse_trimestrales[15,2]=rmse_ref_fuera_M15
rmse_trimestrales[16,2]=resultados_fuera_arima

View(rmse_trimestrales)

# FIN DEL PROGRAMA----------------------