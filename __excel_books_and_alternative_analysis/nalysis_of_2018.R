rm(list=ls());gc()
load(paste0(getwd(),"/","Procesos hasta 4.RData"))
rm("d9")
rm("urg_2019")
library(ggplot2)
library(dplyr)
library(RODBC)
d8 <- odbcConnectAccess("AtencionesUrgencia2018.mdb")
urg_2018 <- sqlFetch(d8, "AtencionesUrgencia2018")
odbcClose(d8)
rm("d8")

View(urg18_1)

urg_2018_3_alt<-
  urg_2018 %>% 
  dplyr::filter(idestablecimiento %in% c("09-100", "11-195", "12-100")) %>% 
  dplyr::filter(Idcausa %in% c(1,#total consultations
                               2,#respiratory consultations
                               12,#circulatory consultations
                               29,#diarrhea consultations
                               18,#trauma consultations
                               25, #total hospitalizations
                               7, #respiratory hospitalizations
                               22, #circulatory hospitalizations
                               23 #trauma hospitalizations
  )) %>%
  dplyr::rename("id"="idestablecimiento",
                "total_all"="Col01",
                "total_a1"="Col02",
                "total_1a4"="Col03",
                "total_5a14"="Col04",
                "total_15a64"="Col05",
                "total_65a"="Col06",
                "date"="fecha")

urg_2018_3_alt %>% 
  dplyr::filter(total_15a64==0) %>% 
  dplyr::mutate(date=readr::parse_datetime(date, "%d/%m/%Y")) %>% 
  dplyr::group_by(id,Idcausa) %>% 
  summarise(min=min(date), max=max(date), n=n()) %>% write.table("clipboard")

#_#_#_#_#_#_#_#_#_#_#_#_
#PLOT
#_#_#_#_#_#_#_#_#_#_#_#_

library(gridExtra)
library(gridExtra)

tabla1<-
  urg_2018_3_alt %>% 
  dplyr::filter(total_15a64==0) %>% 
  dplyr::mutate(date=readr::parse_datetime(date, "%d/%m/%Y")) %>% 
  dplyr::filter(Idcausa %in% c(1,2,12,18,29))%>% 
  dplyr::group_by(id,Idcausa) %>% 
  summarise(min=min(date), max=max(date), n=n()) %>% data.frame()

tabla1<-cbind(tabla1[1:5,],tabla1[6:10,])

fig<-
  urg_2018_3_alt %>% 
  dplyr::filter(total_15a64==0) %>% 
  dplyr::mutate(date=readr::parse_datetime(date, "%d/%m/%Y")) %>% 
  dplyr::mutate(fech_ing_week=format(date,'%V')) %>%
  dplyr::mutate(id=factor(id, labels= c("Complejo Hospitalario San Jose (09-100)", 
                                                  "Hospital de Urgencia Asistencia Publica (11-195)", 
                                                  "Hospital Del Salvador de Santiago (12-100)"))) %>% 

  dplyr::filter(Idcausa %in% c(1,2,12,18,29))%>% 
  dplyr::mutate(Idcausa=factor(Idcausa, labels=c("total consultations(1)",
                                                 "respiratory consultations(2)",
                                                 "circulatory consultations(12)",
                                                 "trauma consultations(18)",
                                                 "diarrhea consultations(29)"))) %>%
  dplyr::group_by(id,Idcausa,fech_ing_week) %>% 
  dplyr::summarise(n = n(), start=min(date)) %>% 
  ggplot2::ggplot(aes(x = fech_ing_week, y = n, color=Idcausa, group=Idcausa)) +
  geom_line(size=1) +
  facet_wrap(id~.,ncol=1)+
  sjPlot::theme_sjplot2() +
  ggtitle("Table 1. Dataset of Information of 2018, Cases with 0 consultations (from 15 to 64 years)\nby Cause and Institution")+
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8),limits=c(0,8))+
  labs(y="No. of Days in the week with 0 counts",
       x="Years & Weeks") + 
  theme(axis.text.x = element_text(vjust = 0.5,hjust = 0.5,angle = 60), plot.caption=element_text(hjust=0)) 



tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)),
                     base_size = 7, padding = unit(c(3, 4), "mm"))
tbl <- tableGrob(tabla1, rows=NULL,theme=tt)

png("Tab1_0_cons_2018.png", res =300, width=10, height= 10,units= "in")
grid.arrange(fig, tbl, 
             nrow = 2, heights = c(4, 1),
             as.table = TRUE)
dev.off()
#Consultas traumatiológicas
#Yo no utilcé diarrea como control
#

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#
#dplyr::mutate(Idcausa=factor(Idcausa, labels=c("total consultations(1)",
#                                               "respiratory consultations(2)",
#                                               "respiratory hospitalizations(7)",
#                                               "circulatory consultations(12)",
#                                               "trauma consultations(18)",
#                                               "circulatory hospitalizations(22)",
#                                               "trauma hospitalizations(23)",
#                                               "total hospitalizations(25)",
#                                               "diarrhea consultations(29)"))) %>%

tabla2<-
  urg_2018_3_alt %>% 
  dplyr::filter(total_15a64==0) %>% 
  dplyr::mutate(date=readr::parse_datetime(date, "%d/%m/%Y")) %>% 
  dplyr::filter(Idcausa %in% c(7,22,23,25))%>%
  dplyr::group_by(id,Idcausa) %>% 
  summarise(min=min(date), max=max(date), n=n()) %>% data.frame()

tabla2<-cbind(tabla2[1:6,],tabla2[7:12,])

fig2<-
  urg_2018_3_alt %>% 
  dplyr::filter(total_15a64==0) %>% 
  dplyr::mutate(date=readr::parse_datetime(date, "%d/%m/%Y")) %>% 
  dplyr::mutate(fech_ing_week=format(date,'%V')) %>%
  dplyr::mutate(id=factor(id, labels= c("Complejo Hospitalario San Jose (09-100)", 
                                        "Hospital de Urgencia Asistencia Publica (11-195)", 
                                        "Hospital Del Salvador de Santiago (12-100)"))) %>% 
  
  dplyr::filter(Idcausa %in% c(7,22,23,25))%>% 
  dplyr::mutate(Idcausa=factor(Idcausa, labels=c("respiratory hospitalizations(7)",
                                                 "circulatory hospitalizations(22)",
                                                 "trauma hospitalizations(23)",
                                                 "total hospitalizations(25)"))) %>%
  dplyr::group_by(id,Idcausa,fech_ing_week) %>% 
  dplyr::summarise(n = as.numeric(n()), start=min(date)) %>% 
  ggplot2::ggplot(aes(x = fech_ing_week, y = n, color=Idcausa, group=Idcausa)) +
  geom_line(size=1) +
  facet_wrap(id~.,ncol=1)+
  #ylim()+
  sjPlot::theme_sjplot2() +
  ggtitle("Table 2. Dataset of Information of 2018, Cases with 0 hospitalizations (from 15 to 64 years)\nby Cause and Institution")+
   scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8),limits=c(0,8))+
  labs(y="No. of Days in the week with 0 counts",
       x="Years & Weeks") + 
  theme(axis.text.x = element_text(vjust = 0.5,hjust = 0.5,angle = 60), plot.caption=element_text(hjust=0)) 

tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)),
                     base_size = 7, padding = unit(c(3, 4), "mm"))
tbl2 <- tableGrob(tabla2, rows=NULL,theme=tt)

png("Tab2_0_hosp_2018.png", res =300, width=10, height= 10,units= "in")
grid.arrange(fig2, tbl2, 
             nrow = 2, heights = c(4, 1),
             as.table = TRUE)
dev.off()
#Week 10	Mar. 5, 2018	Mar. 11, 2018
#Week 11	Mar. 12, 2018	Mar. 18, 2018
#Week 25	June 18, 2018	June 24, 2018
#Week 26	June 25, 2018	July 1, 2018
#"Ex posta central" registers almost no hospitalizations in march 5 & 12, and June 18 & 25 2018
# No tiene relación con los indicadores, ya que el paro se habría iniciado a mediados o finales de septiembre, aunque podría ser una causa

# 11 de sept: no hay insumos, colmed se pronuncia
# http://www.colegiomedico.cl/medicos-de-la-huap-se-manifiestan-por-falta-de-insumos/
# Falta de capacidad de urgencia, se ha sobrepasado la disponibilidad de ingresos disponibles para enfermos.
# https://www.emol.com/noticias/Nacional/2018/09/27/921926/Grupo-de-trabajadores-comenzo-paro-en-la-ex-Posta-Central-en-Santiago.html
# paro total de actividades, solo se atenderán casos de "riesgo vital". falta de capacidad de urgencia
# https://www.t13.cl/noticia/nacional/funcionarios-ex-posta-central-anuncian-paro-total-actividades

#_#_#_#_#_#_#_#_#_#_#_#_
#Reemplazo
#_#_#_#_#_#_#_#_#_#_#_#_

###_###_###_###_###_###_###_###_###_###_###_###_###_###_###_
#En la base de datos original, se define la semana de esta forma para colapsar:
###data$year_week <- strftime(as.Date(data$date), format = "%Y-W%V")
# fech_ing_week debiese hacer lo mismo.
###_###_###_###_###_###_###_###_###_###_###_###_###_###_###_

urg_2018_3_comp_inst<-
urg_2018 %>% 
  dplyr::filter(Idcausa %in% c(7, #respiratory hospitalizations
                              22, #circulatory hospitalizations
                               23, #trauma hospitalizations
                              25 #total hospitalizations
  )) %>%
  dplyr::rename("id"="idestablecimiento",
                "total_all"="Col01",
                "total_a1"="Col02",
                "total_1a4"="Col03",
                "total_5a14"="Col04",
                "total_15a64"="Col05",
                "total_65a"="Col06",
                "date"="fecha") %>% 
  dplyr::mutate(selected_inst= dplyr::case_when(id %in% c("09-100", "11-195", "12-100")~1,TRUE~0)) %>%
  dplyr::mutate(relevant_cases=dplyr::case_when(total_15a64==0 & selected_inst==1~1,TRUE~0)) %>% 
  dplyr::mutate(date=readr::parse_datetime(date, "%d/%m/%Y")) %>% 
  dplyr::mutate(fech_ing_week=format(date,'%V')) %>%
  dplyr::filter(fech_ing_week %in% c(10,11,25,26)) %>% 
  dplyr::group_by(fech_ing_week) %>% 
  dplyr::mutate(relevant_cases_sum=sum(relevant_cases,na.rm=T))%>% 
  dplyr::ungroup()

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#Interpolación lineal
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
urg_2018_3_alt_corr<-
urg_2018_3_alt %>% 
  dplyr::mutate(date=readr::parse_datetime(date, "%d/%m/%Y")) %>% 
  dplyr::mutate(fech_ing_week=format(date,'%V')) %>%
  dplyr::arrange(Idcausa,fech_ing_week)%>%
  dplyr::mutate(total_15a64= dplyr::case_when(id=="11-195"& fech_ing_week %in% c(10,11,25,26)~NA_integer_, TRUE~total_15a64))%>% 
  dplyr::mutate(total_all= dplyr::case_when(id=="11-195"& fech_ing_week %in% c(10,11,25,26)~NA_integer_, TRUE~total_all))%>%   
  dplyr::mutate(total_65a= dplyr::case_when(id=="11-195"& fech_ing_week %in% c(10,11,25,26)~NA_integer_, TRUE~total_65a))%>%
  dplyr::group_by(Idcausa,id) %>% 
  dplyr::mutate(total_15a64= zoo::na.aggregate(total_15a64,FUN=median))%>% 
  dplyr::mutate(total_all= zoo::na.aggregate(total_all,FUN=median))%>%   #na.locf(x,fromLast = FALSE) #nearest non-NA value
  dplyr::mutate(total_65a= zoo::na.aggregate(total_65a,FUN=median))%>%   #na.aggregate(dat,FUN = mean) #linear= na.approx
  dplyr::ungroup() #%>% 
  #dplyr::filter(fech_ing_week %in% c("09","10","11","24","25","26"), id=="11-195") %>% 
  #dplyr::select(1:4,starts_with("total"),fech_ing_week)
  #dplyr::filter(id=="11-195") %>% View()    
    
    tabla3<-
      urg_2018_3_alt_corr %>% 
    dplyr::filter(total_15a64==0) %>% 
    #dplyr::mutate(date=readr::parse_datetime(date, "%d/%m/%Y")) %>% 
    dplyr::filter(Idcausa %in% c(7,22,23,25))%>%
    dplyr::group_by(id,Idcausa) %>% 
    summarise(min=min(date), max=max(date), n=n()) %>% data.frame()
  
  tabla3<-cbind(tabla3[1:6,],tabla3[7:12,])
  
  fig3<-
    urg_2018_3_alt_corr %>% 
    dplyr::filter(total_15a64==0) %>% 
   # dplyr::mutate(date=readr::parse_datetime(date, "%d/%m/%Y")) %>% 
   # dplyr::mutate(fech_ing_week=format(date,'%V')) %>%
    dplyr::mutate(id=factor(id, labels= c("Complejo Hospitalario San Jose (09-100)", 
                                          "Hospital de Urgencia Asistencia Publica (11-195)", 
                                          "Hospital Del Salvador de Santiago (12-100)"))) %>% 
    
    dplyr::filter(Idcausa %in% c(7,22,23,25))%>% 
    dplyr::mutate(Idcausa=factor(Idcausa, labels=c("respiratory hospitalizations(7)",
                                                   "circulatory hospitalizations(22)",
                                                   "trauma hospitalizations(23)",
                                                   "total hospitalizations(25)"))) %>%
    dplyr::group_by(id,Idcausa,fech_ing_week) %>% 
    dplyr::summarise(n = as.numeric(n()), start=min(date)) %>% 
    ggplot2::ggplot(aes(x = fech_ing_week, y = n, color=Idcausa, group=Idcausa)) +
    geom_line(size=1) +
    facet_wrap(id~.,ncol=1)+
    #ylim()+
    sjPlot::theme_sjplot2() +
    ggtitle("Table 3. Dataset of Information of 2018, Cases with 0 hospitalizations (from 15 to 64 years)\nby Cause and Institution, after Interpolation of cases with the median")+
    scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8),limits=c(0,8))+
    labs(y="No. of Days in the week with 0 counts",
         x="Years & Weeks") + 
    theme(axis.text.x = element_text(vjust = 0.5,hjust = 0.5,angle = 60), plot.caption=element_text(hjust=0)) 
  
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)),
                       base_size = 7, padding = unit(c(3, 4), "mm"))
  tbl3 <- tableGrob(tabla3, rows=NULL,theme=tt)
  
  png("Tab3_0_hosp_2018_corr.png", res =300, width=10, height= 10,units= "in")
  grid.arrange(fig3, tbl3, 
               nrow = 2, heights = c(4, 1),
               as.table = TRUE)
  dev.off()    
    
  #:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
  #:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
  #:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
  
#Fill in missing months in AirPass with linear interpolation using na.approx().
#glimpse(urg_2018_3_alt )

#https://campus.datacamp.com/courses/manipulating-time-series-data-with-xts-and-zoo-in-r/merging-and-modifying-time-series?ex=7
#https://www.rdocumentation.org/packages/zoo/versions/1.8-8/topics/rollmean