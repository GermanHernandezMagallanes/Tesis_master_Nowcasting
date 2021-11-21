rm(list=ls())


getwd()
setwd("C:/Users/ecger/Desktop/Tesis_definitivo")
getwd()


library(openxlsx)
library(gtrendsR)
library(Rcpp)

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


google_mts=ts(outdf,start = c(2004,01), end=c(2020,12), frequency = 12)
outdfc=data.frame(outdf)
View(google_mts)

media_g=vector("list", length(gt))
varianza_g=vector("list", length(gt))
desvio_g=vector("list", length(gt))
gtc=vector("list", length(gt))
typeof(outdf[[1]])


outdfts=ts(outdf, start=c(2004, 01), end=c(2020, 12), frequency=12)
outdfts=window(outdfts, start=c(2011, 01), end=c(2020, 12), frequency=12)
outdf=data.frame(outdfts)

for (i in seq_along(gt)){
  outdf[[i]]=as.numeric(outdf[[i]])
  media_g[[i]]=mean(outdf[[i]], na.rm = FALSE)
  varianza_g[[i]]=var(outdf[[i]], na.rm = FALSE)
  desvio_g[[i]]=sqrt(varianza_g[[i]])
  print(paste("Centrando", gt[[i]]))
  gtc[[i]]=(outdf[[i]]-media_g[[i]])/desvio_g[[i]]
}

View(gtc)
outdfc=data.frame(matrix(unlist(gtc), nrow=length(outdf[[i]]), ncol = length(gtc), byrow=F))
colnames(outdfc)=gt
View(outdfc)

#Creo la serie de tiempo.

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

