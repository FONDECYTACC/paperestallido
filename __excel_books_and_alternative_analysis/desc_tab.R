load("G:/Mi unidad/linkabbddyscriptderpaperestallidosocial/Definitive_models.RData")

attr(data15a64_rn_ratio$rate,"label") <- "Rate of Trauma Hospitalizations per Trauma Consultations (x1,000 population)"
attr(data15a64_rn_ratio$rate_resp,"label") <- "Respiratory Hospitalizations per Respiratory Consultations (x1,000 population)"

data15a64_rn_ratio %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(cons_total_sum=sum(cons_total),hosp_total_sum=sum(hosp_total))


#data15a64_rn_ratio

data15a64_rn_ratio %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(cons_total_sum=sum(cons_total),hosp_total_sum=sum(hosp_total)) %>% write.table("clipboard")

data15a64_rn_ratio %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(cons_total_sum=sum(cons_total),hosp_total_sum=sum(hosp_total),
                   cons_total_p50=quantile(cons_total,.5),hosp_total_p50=quantile(hosp_total,.5)) %>% write.table("clipboard")


## TABLA S1.


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

attr(data15a64_rn_ratio_its$rate,"label") <- "Rate of Trauma Hospitalizations per Trauma Consultations (x1,000 population)"
attr(data15a64_rn_ratio_its$rate_resp,"label") <- "Respiratory Hospitalizations per Respiratory Consultations (x1,000 population)"

library(compareGroups)
#pvals <- compareGroups::getResults(table, "p.overall")
tab_1 <- compareGroups::compareGroups(tx ~ cons_total+ 
                                        cons_trauma+  
                                        cons_resp+ 
                                        cons_circ+ 
                                        hosp_total+
                                        hosp_trauma+
                                        hosp_resp+ 
                                        hosp_circ+
                                        rate+
                                        rate_resp, #27
                                      #method=rep(3,18),#rep(2,6),3,rep(2,2),rep(3,9)),
                                      method = c(hosp_total=2,
                                                 hosp_trauma=2,
                                                 hosp_resp=2, 
                                                 hosp_circ=2,
                                                 cons_total=2,
                                                 cons_trauma=2,
                                                 cons_circ=2,
                                                 cons_resp=2,
                                                 rate=2,
                                                 rate_resp=2), #time=NA, post=3,  exptime+   prevtrc=NA, prevrec=NA, prevtrh=NA, prevreh=NA, 
                                      data = data15a64_rn_ratio,
                                      include.miss = T,
                                      var.equal=T)
#,
#subset = age == "15-64")
#p.adjust(pvals, method = "BH")
restab1<-createTable(tab_1, show.n = F, show.p.overall = F)
compareGroups::export2md(restab1, 
                         first.strip = T, 
                         show.all = T,
                         hide.no = "no",
                         format="html",
                         position="center",
                         size=11,
                         caption= "Table S1. Summary descriptives table of Hospitalizations and Consultations 2015-2018 vs. 2019")%>% #,p.overall = "p-value"
  kableExtra::add_footnote(paste0("Note. Total weeks (",nrow(data15a64_wk),")"), notation = "none")%>%
  kableExtra::scroll_box(width = "100%", height = "375px")

## TABLA SUPLEMENTARIA

library(compareGroups)
#pvals <- compareGroups::getResults(table, "p.overall")
tab_s2 <- compareGroups::compareGroups(did ~ cons_total+ 
                                          cons_trauma+  
                                          cons_resp+ 
                                          cons_circ+ 
                                          hosp_total+
                                          hosp_trauma+
                                          hosp_resp+ 
                                          hosp_circ+
                                          rate+
                                          rate_resp, #27
                                       #method=rep(3,18),#rep(2,6),3,rep(2,2),rep(3,9)),
                                       method = c(hosp_total=2,
                                                  hosp_trauma=2,
                                                  hosp_resp=2, 
                                                  hosp_circ=2,
                                                  cons_total=2,
                                                  cons_trauma=2,
                                                  cons_circ=2,
                                                  cons_resp=2,
                                                  rate=2,
                                                  rate_resp=2), #time=NA, post=3,  exptime+   prevtrc=NA, prevrec=NA, prevtrh=NA, prevreh=NA, 
                                       data = data15a64_rn_ratio,
                                       include.miss = T,
                                       var.equal=T)
#,
#subset = age == "15-64")
#p.adjust(pvals, method = "BH")C
restabs2<-createTable(tab_s2, show.n = F, show.p.overall = F)
compareGroups::export2md(restabs2, 
                         first.strip = T, 
                         show.all = T,
                         hide.no = "yes",
                         format="html",
                         position="center",
                         size=11,
                         caption= "Table 1. Summary descriptives table of Hospitalizations and Consultations, Pre-Civil Unrest vs. Post-Civil Unrest")%>% #,p.overall = "p-value"
  kableExtra::add_footnote(paste0("Note. Total weeks (n=",nrow(data15a64_wk),")"), notation = "none")%>%
  kableExtra::scroll_box(width = "100%", height = "375px")

#header.labels = c(`0`= "Pre- Oct 18, 2020", 
#`1`="Post Oct 18, 2020"), size=9,col.names= c("Variables", "Pre- Oct 18, 2020",
#"Post Oct 18, 2020", "P-values", "N"),
#format="html",position="center",
#d
