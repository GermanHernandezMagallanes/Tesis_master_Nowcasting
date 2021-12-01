#RMSE comparado trimestral---------------------------

library(parallel)


#'Creo la matriz de RMSE en la muestra y fuera de la muestra para
#'los 15 modelos. La matriz contendrá en su primera columna
#'el error absoluto promedio de predicción dentro de la muestra, 
#'en tanto la columna dos contendrá el error absoluto de la predicción
#'del primer trimestre del 2021.


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




#Modelo de referencia: diff(diff(ivf,4))

library(forecast)
library(Metrics)


modelo_referencial=auto.arima(window(ivf_2021_ts_c, end=c(2020,04)))
referencia_autoarima=forecast(modelo_referencial,h=1)
  
dentro_arima=diff(diff(modelo_referencial$fitted,4))
resultados_dentro_arima=rmse(ivf_oos,dentro_arima)
resultados_fuera_arima=rmse(window(ivf_oos, start=c(2021,1), end=c(2021,1)), referencia_autoarima$mean)

rmse_trimestrales[16,1]=resultados_dentro_arima


#Fuera de la muestra--------------------
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


#Visualización-----------------------------------
library(kableExtra)
library(data.table)

library(knitr)
library(kableExtra)
library(tidyverse)


rmse_trimestrales_r=round(rmse_trimestrales, 4)

rmse_trimestrales_r %>%
  kbl(caption = "Error absoluto medio dentro y fuera de muestra por modelo") %>%
  kable_classic_2(full_width = F)%>%
kable_styling() %>%
  row_spec(which.min(rmse_trimestrales_r[,2]), bold = T, color = "white", background = "green")%>%
  row_spec(which.min(rmse_trimestrales_r[,1]), bold = T, color = "white", background = "green")%>%
  row_spec(which.max(rmse_trimestrales_r[,2]), bold = T, color = "white", background = "red")%>%
  row_spec(which.max(rmse_trimestrales_r[,1]), bold = T, color = "white", background = "red")
  

rmse_trimestrales_r%>%
  rownames_to_column("model") %>% 
  arrange(wt) %>%
  mutate(drat = cell_spec(drat, background=ifelse(drat > 3, "red", "green"))) %>% 
  kbl(booktabs = T, linesep = "", escape=FALSE) %>% 
  kable_paper(full_width = F) %>%
  column_spec(6, color = "white")
