# --------- accedo a los datos -----------
remove(list=ls())
library(readr)
establecimientos <- read_delim("establecimientos.csv", ";", escape_double = FALSE, trim_ws = TRUE)

View(establecimientos)
library(Hmisc)
urg_2018 <- mdb.get("AtencionesUrgencia2018.mdb")
View(urg_2018)

# --------- creo indicadores para las consultas --------
estab_RM <- establecimientos[establecimientos$codigo_region==13 & (establecimientos$nivel=="Terciario" | establecimientos$dependencia=="Privado"), "codigo_antiguo", drop=TRUE]
estab_RM <- as.factor(estab_RM)
columnas = c("idestablecimiento", "Col01", "Col02", "Col03", "Col04", "Col05", "Col06", "fecha")

# ------ inicio las consultas ---------------
consultas_total<- urg_2018[urg_2018$Idcausa==1 & urg_2018$idestablecimiento %in% estab_RM, columnas, drop=FALSE]
names(consultas_total) <- c ("id", "consultas_total_todos", "consultas_total_menor1", "consultas_total_1a4", "consultas_total_5a14", "consultas_total_15a64", "consultas_total_65ymas", "fecha")
consultas_respiratorio<- urg_2018[urg_2018$Idcausa==2 & urg_2018$idestablecimiento %in% estab_RM, columnas, drop=FALSE]
names(consultas_respiratorio) <- c ("id", "consultas_resp_todos", "consultas_resp_menor1", "consultas_resp_1a4", "consultas_resp_5a14", "consultas_resp_15a64", "consultas_resp_65ymas", "fecha")
consultas_circulatorio<- urg_2018[urg_2018$Idcausa==12 & urg_2018$idestablecimiento %in% estab_RM, columnas, drop=FALSE]
names(consultas_circulatorio) <- c ("id", "consultas_circ_todos", "consultas_circ_menor1", "consultas_circ_1a4", "consultas_circ_5a14", "consultas_circ_15a64", "consultas_circ_65ymas", "fecha")
consultas_diarrea<- urg_2018[urg_2018$Idcausa==29 & urg_2018$idestablecimiento %in% estab_RM, columnas, drop=FALSE]
names(consultas_diarrea) <- c ("id", "consultas_diarrea_todos", "consultas_diarrea_menor1", "consultas_diarrea_1a4", "consultas_diarrea_5a14", "consultas_diarrea_15a64", "consultas_diarrea_65ymas", "fecha")
consultas_trauma<- urg_2018[urg_2018$Idcausa==18 & urg_2018$idestablecimiento %in% estab_RM, columnas, drop=FALSE]
names(consultas_trauma) <- c ("id", "consultas_trauma_todos", "consultas_trauma_menor1", "consultas_trauma_1a4", "consultas_trauma_5a14", "consultas_trauma_15a64", "consultas_trauma_65ymas", "fecha")

hosp_total<- urg_2018[urg_2018$Idcausa==25 & urg_2018$idestablecimiento %in% estab_RM, columnas, drop=FALSE]
names(hosp_total) <- c ("id", "hosp_total_todos", "hosp_total_menor1", "hosp_total_1a4", "hosp_total_5a14", "hosp_total_15a64", "hosp_total_65ymas", "fecha")
hosp_respiratorio<- urg_2018[urg_2018$Idcausa==7 & urg_2018$idestablecimiento %in% estab_RM, columnas, drop=FALSE]
names(hosp_respiratorio) <- c ("id", "hosp_resp_todos", "hosp_resp_menor1", "hosp_resp_1a4", "hosp_resp_5a14", "hosp_resp_15a64", "hosp_resp_65ymas", "fecha")
hosp_circulatorio<- urg_2018[urg_2018$Idcausa==22 & urg_2018$idestablecimiento %in% estab_RM, columnas, drop=FALSE]
names(hosp_circulatorio) <- c ("id", "hosp_circ_todos", "hosp_circ_menor1", "hosp_circ_1a4", "hosp_circ_5a14", "hosp_circ_15a64", "hosp_circ_65ymas", "fecha")
hosp_trauma<- urg_2018[urg_2018$Idcausa==23 & urg_2018$idestablecimiento %in% estab_RM, columnas, drop=FALSE]
names(hosp_trauma) <- c ("id", "hosp_trauma_todos", "hosp_trauma_menor1", "hosp_trauma_1a4", "hosp_trauma_5a14", "hosp_trauma_15a64", "hosp_trauma_65ymas", "fecha")

# ------ ahora uno la BD ---------------
consultas <- merge(consultas_total, consultas_respiratorio)
consultas <- merge(consultas, consultas_circulatorio)
consultas <- merge(consultas, consultas_diarrea)
consultas <- merge(consultas, consultas_trauma)

hospitalizaciones <- merge(hosp_total, hosp_respiratorio)
hospitalizaciones <- merge(hospitalizaciones, hosp_circulatorio)
hospitalizaciones <- merge(hospitalizaciones, hosp_trauma)

BD_urg_2018_RM <- merge(consultas, hospitalizaciones)
library(foreign)
write.dta(BD_urg_2018_RM, "/Users/masterin/Documents/Universidad/Otros trabajos propios y de hospital/Bases de datos poblacionales/Atenciones de Urgencia Chile/BD_urg_2018_RM.dta")

# ----------- acotando hospitales x mediana ---------
library(dplyr)
medianas <- summarise(group_by(BD_urg_2018_RM, id), median = median(hosp_trauma_15a64))
entra <- medianas[medianas$median>0, "id"]
nombres <- establecimientos[establecimientos$codigo_antiguo %in% entra$id, "nombre"]
BD_urg_2018_RM_trauma <-BD_urg_2018_RM[BD_urg_2018_RM$id %in% entra$id, ]
BD_urg_2018_RM_trauma$fecha<- as.character.Date(BD_urg_2018_RM_trauma$fecha)
library(foreign)
write.dta(BD_urg_2018_RM_trauma, "/Users/masterin/Documents/Universidad/Otros trabajos propios y de hospital/Bases de datos poblacionales/Atenciones de Urgencia Chile/BD_urg_2018_RM-trauma.dta")
