#Usando el enlace https://ourworldindata.org/covid-vaccinations realice un script en R que permita obtener los datos y genere una tabla y una figura que ayude a entender la información
library(rvest)
bs<-read_html("https://ourworldindata.org/covid-vaccinations")
tabla<-html_table(bs,fill = T)
bx=tabla[[1]]




#Usando la librería de google trends en R, genere un gráfico para los últimos 15 meses en Bolivia, que compare la palabras; Remdesivir, dióxido de cloro, antígeno nasal, sputnik. Comente los resultados
library(gtrendsR)
library(dplyr)
library(ggplot2)
aux<-countries
?gtrends()
res <- gtrends(c("Remdesivir", "dióxido de cloro", "antígeno nasal", "sputnik"), geo = c("BO"),time="today 3-m")
plot(res)
bd<-res$interest_over_time
bd$hits<-as.numeric(gsub("<","",bd_cov$hits))
bdg=bd%>%group_by(keyword)
ggplot(bdg,aes(date,hits,colour=keyword))+geom_line()

##### 
#Usando la encuesta a hogares 2020 genere una tabla del promedio de edad o años de educación por área urbano/rural usando como objetos; dataframe, ffdf, bigmatrix. Compare el tiempo de las salidas
install.packages("readr")
library(readr)
library(dplyr)
library(foreign)
library(tictoc)
tic("data")
getwd()
setwd("C:/Users/rebal/Desktop/2020")
bdp=read.spss("EH2020_Persona.sav", to.data.frame = TRUE)
bdv=read.spss("EH2020_Vivienda.sav", to.data.frame = TRUE)
bdp%>%group_by(area)%>%summarise(A_Estudio_Prom=mean(aestudio,na.rm=T))
toc()
write_csv(bdp,file="C:/Users/rebal/Desktop/2020/bdp.csv", col_names = TRUE)
######
# libreria ff
tic("ff")
library(ff)
library(ffbase)
library(doBy)
system("mkdir ffdf")
options(fftempdir="C:/Users/rebal/Desktop/2020")
bd3<-read.csv.ffdf(file="C:/Users/rebal/Desktop/2020/bdp.csv",sep=",",header=T, first.rows=-1,colClasses=NA,VERBOSE=T)
bd3df<-as.data.frame(bd3)
summaryBy(aestudio~area,FUN=mean,data=bd3df,na.rm=T)
toc()
######
#libreria bigmemory
tic("big")
library(bigmemory)
bd1=bdp%>%filter(area=="Rural")%>%select(aestudio)
bd2=bdp%>%filter(area=="Urbana")%>%select(aestudio)
bd4<-as.big.matrix(as.matrix(bd1))
bd5<-as.big.matrix(as.matrix(bd2))
mean(bd4[,1],na.rm=T)
mean(bd5[,1],na.rm=T)
toc()
