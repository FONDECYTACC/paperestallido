
#only_letters <- function(x) { gsub("^([[:alpha:]]*).*$","\\1",x) }
#rstudioapi::getSourceEditorContext()$path

rm(list=ls());gc()
datasets<-
cbind(sem=rep(c(1,3),5),
      yr=c(15,15,16,16,17,17,18,18,19,19))

dt<-data.frame()
for (i in 1:10){
  dt2<-paste0("1 - Urg",as.character(datasets[i,2]),"_",as.character(datasets[i,1])," Data Creation.R")
  dt<-rbind(dt,dt2)
}
dt<-rbind(dt, "2 - Urg Data Cleaning.R")
dt<-rbind(dt, "3 - Time Series (tsdata) Data Creation.R")
dt<-rbind(dt, "4 - Time Series (tsdata) Data Cleaning.R")
colnames(dt)<-"sources"

for(i in 1:13){
  source(paste0(getwd(),"/",dt[i,]), encoding = 'UTF-8')
}
rm("urg_2019")
save.image(paste0(getwd(),"/","Procesos hasta 4_2.RData"))