rm(list=ls());gc()

load("G:/Mi unidad/linkabbddyscriptderpaperestallidosocial/Procesos hasta 4.RData")
tsdata3 <- read.csv("tsdata3.csv")


if(!require(plotly)){install.packages("plotly")}
if(!require(lubridate)){install.packages("lubridate")}
if(!require(htmlwidgets)){install.packages("htmlwidgets")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(gganimate)){install.packages("gganimate")}
if(!require(readr)){install.packages("readr")}
if(!require(stringr)){install.packages("stringr")}
if(!require(data.table)){install.packages("data.table")}
if(!require(DT)){install.packages("DT")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(lattice)){install.packages("lattice")}
if(!require(forecast)){install.packages("forecast")}
if(!require(zoo)){install.packages("zoo")}
if(!require(panelView)){install.packages("panelView")}
if(!require(janitor)){install.packages("janitor")}
if(!require(rjson)){install.packages("rjson")}
if(!require(estimatr)){install.packages("estimatr")} 
if(!require(CausalImpact)){install.packages("CausalImpact")}
if(!require(textreg)){install.packages("textreg")}
if(!require(sjPlot)){install.packages("sjPlot")}
if(!require(foreign)){install.packages("foreign")}
if(!require(tsModel)){install.packages("tsModel")}
if(!require(lmtest)){install.packages("lmtest")}
if(!require(Epi)){install.packages("Epi")}
if(!require(splines)){install.packages("splines")}
if(!require(vcd)){install.packages("vcd")}
if(!require(astsa)){install.packages("astsa")}
if(!require(forecast)){install.packages("forecast")}
if(!require(MASS)){install.packages("MASS")}
if(!require(ggsci)){install.packages("ggsci")}
if(!require(Hmisc)){install.packages("Hmisc")}
if(!require(compareGroups)){install.packages("compareGroups")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggforce)){install.packages("ggforce")}
if(!require(imputeTS)){install.packages("imputeTS")}
if(!require(doParallel)){install.packages("doParallel")}
if(!require(SCtools)){install.packages("SCtools")}
if(!require(MSCMT)){install.packages("MSCMT")}
# Calculate the number of cores
no_cores <- detectCores() - 1
cl<-makeCluster(no_cores)
registerDoParallel(cl)

Sys.setlocale(category = "LC_ALL", locale = "english")


#tsdata3 %>% glimpse()  # hosp_resp_ hosp_circ_ hosp_trauma_ hosp_total_
tsdata3_corr<-
  tsdata3 %>% 
  dplyr::mutate(date2=readr::parse_datetime(date, "%Y-%m-%d")) %>% 
  dplyr::mutate(fech_week=format(date2,'%V')) %>%
  dplyr::arrange(id,fech_week)%>%
  
  dplyr::mutate(hosp_resp_all= dplyr::case_when(id=="11-195" & year==2018 & fech_week %in% c(10,11,25,26)~NA_integer_, TRUE~hosp_resp_all))%>%
  dplyr::mutate(hosp_resp_15a64= dplyr::case_when(id=="11-195" & year==2018 & fech_week %in% c(10,11,25,26)~NA_integer_, TRUE~hosp_resp_15a64))%>% 
  dplyr::mutate(hosp_resp_65a= dplyr::case_when(id=="11-195" & year==2018 & fech_week %in% c(10,11,25,26)~NA_integer_, TRUE~hosp_resp_65a))%>%
  
  dplyr::mutate(hosp_circ_all= dplyr::case_when(id=="11-195" & year==2018 & fech_week %in% c(10,11,25,26)~NA_integer_, TRUE~hosp_circ_all))%>%  
  dplyr::mutate(hosp_circ_15a64= dplyr::case_when(id=="11-195" & year==2018 & fech_week %in% c(10,11,25,26)~NA_integer_, TRUE~hosp_circ_15a64))%>% 
  dplyr::mutate(hosp_circ_65a= dplyr::case_when(id=="11-195" & year==2018 & fech_week %in% c(10,11,25,26)~NA_integer_, TRUE~hosp_circ_65a))%>%
  
  dplyr::mutate(hosp_trauma_all= dplyr::case_when(id=="11-195" & year==2018 & fech_week %in% c(10,11,25,26)~NA_integer_, TRUE~hosp_trauma_all))%>%   
  dplyr::mutate(hosp_trauma_15a64= dplyr::case_when(id=="11-195" & year==2018 & fech_week %in% c(10,11,25,26)~NA_integer_, TRUE~hosp_trauma_15a64))%>%
  dplyr::mutate(hosp_trauma_65a= dplyr::case_when(id=="11-195" & year==2018 & fech_week %in% c(10,11,25,26)~NA_integer_, TRUE~hosp_trauma_65a))%>%
  
  dplyr::mutate(hosp_total_all= dplyr::case_when(id=="11-195" & year==2018 & fech_week %in% c(10,11,25,26)~NA_integer_, TRUE~hosp_total_all))%>%   dplyr::mutate(hosp_total_15a64= dplyr::case_when(id=="11-195" & year==2018 & fech_week %in% c(10,11,25,26)~NA_integer_, TRUE~hosp_total_15a64))%>%   
  dplyr::mutate(hosp_total_65a= dplyr::case_when(id=="11-195" & year==2018 & fech_week %in% c(10,11,25,26)~NA_integer_, TRUE~hosp_total_65a))%>%
  
  dplyr::group_by(id,year) %>% 
  ### REPLACE WITH THE MEAN 
  dplyr::mutate(hosp_resp_all= zoo::na.aggregate(hosp_resp_all,FUN=median))%>% 
  dplyr::mutate(hosp_resp_15a64= zoo::na.aggregate(hosp_resp_15a64,FUN=median))%>%   #na.locf(x,fromLast = FALSE) #nearest non-NA value
  dplyr::mutate(hosp_resp_65a= zoo::na.aggregate(hosp_resp_65a,FUN=median))%>%   #na.aggregate(dat,FUN = mean) #linear= na.approx
  
  dplyr::mutate(hosp_circ_all= zoo::na.aggregate(hosp_circ_all,FUN=median))%>% 
  dplyr::mutate(hosp_circ_15a64= zoo::na.aggregate(hosp_circ_15a64,FUN=median))%>%   #na.locf(x,fromLast = FALSE) #nearest non-NA value
  dplyr::mutate(hosp_circ_65a= zoo::na.aggregate(hosp_circ_65a,FUN=median))%>%   #na.aggregate(dat,FUN = mean) #linear= na.approx
  
  dplyr::mutate(hosp_trauma_all= zoo::na.aggregate(hosp_trauma_all,FUN=median))%>% 
  dplyr::mutate(hosp_trauma_15a64= zoo::na.aggregate(hosp_trauma_15a64,FUN=median))%>%   #na.locf(x,fromLast = FALSE) #nearest non-NA value
  dplyr::mutate(hosp_trauma_65a= zoo::na.aggregate(hosp_trauma_65a,FUN=median))%>%   #na.aggregate(dat,FUN = mean) #linear= na.approx
  
  dplyr::mutate(hosp_total_all= zoo::na.aggregate(hosp_total_all,FUN=median))%>% 
  dplyr::mutate(hosp_total_15a64= zoo::na.aggregate(hosp_total_15a64,FUN=median))%>%   #na.locf(x,fromLast = FALSE) #nearest non-NA value
  dplyr::mutate(hosp_total_65a= zoo::na.aggregate(hosp_total_65a,FUN=median))%>%   #na.aggregate(dat,FUN = mean) #linear= na.approx
  dplyr::ungroup() %>% 
  dplyr::select(-date2,-fech_week)

tsdata3_corr_cons <- tsdata3_corr %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(
    year = unique(year),
    month = unique(month),
    day = unique(day),
    cons_total_all = sum(cons_total_all),
    cons_total_15a64 = sum(cons_total_15a64),
    cons_total_65a = sum(cons_total_65a),
    cons_resp_all = sum(cons_resp_all),
    cons_resp_15a64 = sum(cons_resp_15a64),
    cons_resp_65a = sum(cons_resp_65a),
    cons_circ_all = sum(cons_circ_all),
    cons_circ_15a64 = sum(cons_circ_15a64),
    cons_circ_65a = sum(cons_circ_65a),
    cons_diarrea_all = sum(cons_diarrea_all),
    cons_diarrea_15a64 = sum(cons_diarrea_15a64),
    cons_diarrea_65a = sum(cons_diarrea_65a),
    cons_trauma_all = sum(cons_trauma_all),
    cons_trauma_15a64 = sum(cons_trauma_15a64),
    cons_trauma_65a = sum(cons_trauma_65a),
    hosp_total_all = sum(hosp_total_all),
    hosp_total_15a64 = sum(hosp_total_15a64),
    hosp_total_65a = sum(hosp_total_65a),
    hosp_resp_all = sum(hosp_resp_all),
    hosp_resp_15a64 = sum(hosp_resp_15a64),
    hosp_resp_65a = sum(hosp_resp_65a),
    hosp_circ_all = sum(hosp_circ_all),
    hosp_circ_15a64 = sum(hosp_circ_15a64),
    hosp_circ_65a = sum(hosp_circ_65a),
    hosp_trauma_all = sum(hosp_trauma_all),
    hosp_trauma_15a64 = sum(hosp_trauma_15a64),
    hosp_trauma_65a = sum(hosp_trauma_65a)
  )

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#PLOT TO SHOW HOW WE STABILIZED SHOCKS ON 2018
no_muestra=1
if(no_muestra==0){
t3a_2018_0_counts<-  
  tsdata3_corr %>% 
    dplyr::filter(year==2018) %>% 
    dplyr::mutate(date2=readr::parse_datetime(date, "%Y-%m-%d")) %>% 
    dplyr::mutate(fech_week=format(date2,'%V')) %>%
    dplyr::mutate(id=factor(id, labels= c("Complejo Hospitalario San Jose (09-100)", 
                                          "Hospital de Urgencia Asistencia Publica (11-195)", 
                                          "Hospital Del Salvador de Santiago (12-100)"))) %>% 
    pivot_longer(cols=cons_total_all:hosp_trauma_65a, names_to = "variable", values_to = "count") %>% 
    dplyr::filter(variable  %in% c("cons_total_15a64","cons_resp_15a64","cons_circ_15a64","cons_trauma_15a64","cons_diarrea_15a64")) %>% 
    dplyr::filter(count==0) %>%
    dplyr::group_by(id,fech_week,variable) %>% 
    dplyr::summarise(n = as.numeric(n()), start=min(date2)) %>% 
    dplyr::ungroup() %>% 
    ggplot2::ggplot(aes(x = fech_week, y = n, color=variable,group=variable)) +
    geom_point(size=3,aes(shape=variable)) +
    geom_line(size=1,alpha=.2) +
    facet_wrap(id~.,ncol=1)+
    #ylim()+
    sjPlot::theme_sjplot2() +
  ggtitle("Table 3b. Dataset of Information of 2018, Cases with 0 hospitalizations (from 15 to 64 years)\nby Cause and Institution")+
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8),limits=c(0,8))+
    labs(y="No. of Days in the week with 0 counts",
         x="Years & Weeks") + 
    theme(axis.text.x = element_text(vjust = 0.5,hjust = 0.5,angle = 60), plot.caption=element_text(hjust=0)) 
  ggsave("Tab 3a. After Interpolation. 2018.png",t3a_2018_0_counts, dpi =300, width=10, height= 10,units= "in")
  
  t3b_2015_0_counts<-  
    tsdata3_corr %>% 
    dplyr::filter(year==2015) %>% 
    dplyr::mutate(date2=readr::parse_datetime(date, "%Y-%m-%d")) %>% 
    dplyr::mutate(fech_week=format(date2,'%V')) %>%
    dplyr::mutate(id=factor(id, labels= c("Complejo Hospitalario San Jose (09-100)", 
                                          "Hospital de Urgencia Asistencia Publica (11-195)", 
                                          "Hospital Del Salvador de Santiago (12-100)"))) %>% 
    pivot_longer(cols=cons_total_all:hosp_trauma_65a, names_to = "variable", values_to = "count") %>% 
    dplyr::filter(variable  %in% c("cons_total_15a64","cons_resp_15a64","cons_circ_15a64","cons_trauma_15a64","cons_diarrea_15a64")) %>% 
    dplyr::filter(count==0) %>%
    dplyr::group_by(id,fech_week,variable) %>% 
    dplyr::summarise(n = as.numeric(n()), start=min(date2)) %>% 
    dplyr::ungroup() %>% 
    ggplot2::ggplot(aes(x = fech_week, y = n, color=variable,group=variable)) +
    geom_point(size=3,aes(shape=variable)) +
    geom_line(size=1,alpha=.2) +
    facet_wrap(id~.,ncol=1)+
    #ylim()+
    sjPlot::theme_sjplot2() +
    ggtitle("Table 3b. Dataset of Information of 2015, Cases with 0 hospitalizations (from 15 to 64 years)\nby Cause and Institution")+
    scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8),limits=c(0,8))+
    labs(y="No. of Days in the week with 0 counts",
         x="Years & Weeks") + 
    theme(axis.text.x = element_text(vjust = 0.5,hjust = 0.5,angle = 60), plot.caption=element_text(hjust=0)) 
  ggsave("Tab 3b. After Interpolation. 2015.png",t3b_2015_0_counts, dpi =300, width=10, height= 10,units= "in")
  
  t3c_2017_0_counts<-  
    tsdata3_corr %>% 
    dplyr::filter(year==2017) %>% 
    dplyr::mutate(date2=readr::parse_datetime(date, "%Y-%m-%d")) %>% 
    dplyr::mutate(fech_week=format(date2,'%V')) %>%
    dplyr::mutate(id=factor(id, labels= c("Complejo Hospitalario San Jose (09-100)", 
                                          "Hospital de Urgencia Asistencia Publica (11-195)", 
                                          "Hospital Del Salvador de Santiago (12-100)"))) %>% 
    pivot_longer(cols=cons_total_all:hosp_trauma_65a, names_to = "variable", values_to = "count") %>% 
    dplyr::filter(variable  %in% c("cons_total_15a64","cons_resp_15a64","cons_circ_15a64","cons_trauma_15a64","cons_diarrea_15a64")) %>% 
    dplyr::filter(count==0) %>%
    dplyr::group_by(id,fech_week,variable) %>% 
    dplyr::summarise(n = as.numeric(n()), start=min(date2)) %>% 
    dplyr::ungroup() %>% 
    ggplot2::ggplot(aes(x = fech_week, y = n, color=variable,group=variable)) +
    geom_point(size=3,aes(shape=variable)) +
    geom_line(size=1,alpha=.2) +
    facet_wrap(id~.,ncol=1)+
    #ylim()+
    sjPlot::theme_sjplot2() +
    ggtitle("Table 3b. Dataset of Information of 2017, Cases with 0 hospitalizations (from 15 to 64 years)\nby Cause and Institution")+
    scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8),limits=c(0,8))+
    labs(y="No. of Days in the week with 0 counts",
         x="Years & Weeks") + 
    theme(axis.text.x = element_text(vjust = 0.5,hjust = 0.5,angle = 60), plot.caption=element_text(hjust=0)) 
  ggsave("Tab 3c. After Interpolation. 2017.png",t3c_2017_0_counts, dpi =300, width=10, height= 10,units= "in")
}

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


#data15a64_rn_causal$cons_trauma ~ data15a64_rn_causal$cons_circ+ data15a64_rn_causal$cons_resp+ data15a64_rn_causal$difftrh,
ggplotly_series<-
  tsdata3_corr %>% 
  dplyr::mutate(date2=readr::parse_datetime(date, "%Y-%m-%d")) %>% 
  dplyr::mutate(fech_week=format(date2,'%V')) %>%
  dplyr::mutate(id=factor(id, labels= c("Complejo Hospitalario San Jose (09-100)", 
                                        "Hospital de Urgencia Asistencia Publica (11-195)", 
                                        "Hospital Del Salvador de Santiago (12-100)"))) %>% 
  group_by(id,year,fech_week) %>% 
  dplyr::mutate(cons_total_15a64=sum(cons_total_15a64,na.rm=T),
                cons_resp_15a64=sum(cons_resp_15a64,na.rm=T),
                cons_circ_15a64=sum(cons_circ_15a64,na.rm=T),
                cons_trauma_15a64=sum(cons_trauma_15a64,na.rm=T),
                cons_diarrea_15a64=sum(cons_diarrea_15a64,na.rm=T)
  ) %>% 
  slice_min(day) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(1:4,cons_total_15a64,cons_resp_15a64,cons_circ_15a64,cons_trauma_15a64,cons_diarrea_15a64,fech_week ) %>% 
  pivot_longer(cols=cons_total_15a64:cons_diarrea_15a64 , names_to = "variable", values_to = "count") %>% 
  dplyr::arrange(id,year,fech_week) %>% 
  dplyr::mutate(fech_week=as.numeric(fech_week)) %>% 
  #dplyr::filter(variable  %in% c("cons_total_15a64","cons_resp_15a64","cons_circ_15a64","cons_trauma_15a64","cons_diarrea_15a64")) %>% 
  # dplyr::group_by(year,yearweek)%>%
  #dplyr::summarise(median=median(hosp_trauma,na.rm=T))%>%
  ggplot() + #median
  facet_wrap(id~year, ncol = 3,strip.position="right") + 
  geom_line(aes(x=fech_week, y =count, group=variable, color=variable)) + 
  #geom_point(aes(x=fech_week, y =count, group=variable, color=variable,shape=variable),size=1) + 
  #geom_jitter(aes(y = hosp_trauma,x = isoweek, color = year),width = 0.5, alpha = 0.3)+
  theme_bw() + 
  theme(strip.background  = element_blank(),
        strip.text = element_text(face="bold", size=7))+
  ylab("Counts") + 
  xlab("Week Number") + 
  theme(strip.text.x = element_text(size = 8, face = "bold"),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        strip.background  = element_blank())+
  xlab("Week")+
  scale_x_continuous(
    breaks = seq(from = 1, to = 52, by =4),
    labels = seq(from = 1, to = 52, by =4),#,
    #  label = c("two", "four", "six")
  )
ggplotly(ggplotly_series) %>% 
  layout(showlegend = TRUE, legend = list(font = list(size = 9)))