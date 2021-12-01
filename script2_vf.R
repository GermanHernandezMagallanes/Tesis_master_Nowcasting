#Script 2: Cargo series de variables tradicionales. Las centro. 

#cargo series google.
#Uno ambas fuentes.

#Loop de gráficos a nivel de cada variable.
#Loop de FAC y FACP de cada serie a nivel.

#Realizo modelado autoarima, establezco vector de transformaciones.
#Ingreso (A mano) vector que establece los delay de publicación de cada variable

#Cargo IVF trimestral. 
#Gráfico, FAC, FACP. Modelo autoarima y diferencio.


#Defino directorio y cargo paquetes-------------------
getwd()
setwd("C:/Users/ecger/Desktop/Tesis_definitivo")
getwd()

library(openxlsx)
library(forecast)
library(data.table)
library(sweep)
library(nowcasting)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library("scales")
library(data.table)

#Cargo el bloque de variables tradicionales y las estadarizo-----------------------------

tradicionales=openxlsx::read.xlsx("Variables.xlsx", sheet = 1, startRow=1, colNames=TRUE)



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


#Modelado autoarima de las series que integrarán los bloques tradicionales y google-----------------------
#'Se excluye IVF, el cual será tratado a parte.

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


#ur=sur=0 trans=0
#ur=sur=1 trans=4
#ur=0 sur=1 trans=5
#ur=1 sur=0 trans=2

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
