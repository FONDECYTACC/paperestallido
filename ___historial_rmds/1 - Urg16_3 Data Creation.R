# -------- Set Working Directory --------
#setwd("G:/My Drive/Research Data")

# -------- Access Data --------
rm(list=setdiff(ls(), "dt"))
library(readr)
establecimientos <-
  read_delim("establecimientos.csv",
             ";",
             escape_double = FALSE,
             trim_ws = TRUE)

library(RODBC)
d6 <- odbcConnectAccess("AtencionesUrgencia2016.mdb")
urg_2016 <- sqlFetch(d6, "Atenciones Urgencia 2016")
odbcClose(d6)

# -------- Clean urg_2016 Columns --------
urg_2016$idestablecimiento <- urg_2016$IdEstablecimiento
urg_2016$Col01 <- urg_2016$Total
urg_2016$Col02 <- urg_2016$`Menores de 1 año`
urg_2016$Col03 <- urg_2016$`1-4 años`
urg_2016$Col04 <- urg_2016$`5-14 años`
urg_2016$Col05 <- urg_2016$`15-64 años`
urg_2016$Col06 <- urg_2016$`65 años y más`
urg_2016$Idcausa <- urg_2016$IdCausa


# -------- Establish Hospital ID Codes --------
estab_id <-
  establecimientos[establecimientos$codigo_antiguo == "09-100" |
                     establecimientos$codigo_antiguo == "11-195" |
                     establecimientos$codigo_antiguo == "12-100", "codigo_antiguo", drop =
                     TRUE]
estab_id <- as.factor(estab_id)
columns = c("idestablecimiento",
            "Col01",
            "Col02",
            "Col03",
            "Col04",
            "Col05",
            "Col06",
            "fecha")

# -------- Create Consultation Columns --------
cons_total <-
  urg_2016[urg_2016$Idcausa == 1 &
             urg_2016$idestablecimiento %in% estab_id, columns, drop = FALSE]
names(cons_total) <-
  c (
    "id",
    "cons_total_all",
    "cons_total_a1",
    "cons_total_1a4",
    "cons_total_5a14",
    "cons_total_15a64",
    "cons_total_65a",
    "date"
  )
cons_resp <-
  urg_2016[urg_2016$Idcausa == 2 &
             urg_2016$idestablecimiento %in% estab_id, columns, drop = FALSE]
names(cons_resp) <-
  c (
    "id",
    "cons_resp_all",
    "cons_resp_a1",
    "cons_resp_1a4",
    "cons_resp_5a14",
    "cons_resp_15a64",
    "cons_resp_65a",
    "date"
  )
cons_circ <-
  urg_2016[urg_2016$Idcausa == 12 &
             urg_2016$idestablecimiento %in% estab_id, columns, drop = FALSE]
names(cons_circ) <-
  c (
    "id",
    "cons_circ_all",
    "cons_circ_a1",
    "cons_circ_1a4",
    "cons_circ_5a14",
    "cons_circ_15a64",
    "cons_circ_65a",
    "date"
  )
cons_diarrea <-
  urg_2016[urg_2016$Idcausa == 29 &
             urg_2016$idestablecimiento %in% estab_id, columns, drop = FALSE]
names(cons_diarrea) <-
  c (
    "id",
    "cons_diarrea_all",
    "cons_diarrea_a1",
    "cons_diarrea_1a4",
    "cons_diarrea_5a14",
    "cons_diarrea_15a64",
    "cons_diarrea_65a",
    "date"
  )

library(dplyr)
cons_trauma <-
  urg_2016[urg_2016$Idcausa == 18  &
             urg_2016$idestablecimiento %in% estab_id, columns, drop = FALSE]
names(cons_trauma) <-
  c (
    "id",
    "cons_trauma_all",
    "cons_trauma_a1",
    "cons_trauma_1a4",
    "cons_trauma_5a14",
    "cons_trauma_15a64",
    "cons_trauma_65a",
    "date"
  )

# -------- Create Hospitalization Columns --------
hosp_total <-
  urg_2016[urg_2016$Idcausa == 25 &
             urg_2016$idestablecimiento %in% estab_id, columns, drop = FALSE]
names(hosp_total) <-
  c (
    "id",
    "hosp_total_all",
    "hosp_total_a1",
    "hosp_total_1a4",
    "hosp_total_5a14",
    "hosp_total_15a64",
    "hosp_total_65a",
    "date"
  )
hosp_resp <-
  urg_2016[urg_2016$Idcausa == 7 &
             urg_2016$idestablecimiento %in% estab_id, columns, drop = FALSE]
names(hosp_resp) <-
  c (
    "id",
    "hosp_resp_all",
    "hosp_resp_a1",
    "hosp_resp_1a4",
    "hosp_resp_5a14",
    "hosp_resp_15a64",
    "hosp_resp_65a",
    "date"
  )
hosp_circ <-
  urg_2016[urg_2016$Idcausa == 22 &
             urg_2016$idestablecimiento %in% estab_id, columns, drop = FALSE]
names(hosp_circ) <-
  c (
    "id",
    "hosp_circ_all",
    "hosp_circ_a1",
    "hosp_circ_1a4",
    "hosp_circ_5a14",
    "hosp_circ_15a64",
    "hosp_circ_65a",
    "date"
  )
hosp_trauma <-
  urg_2016[urg_2016$Idcausa == 23 &
             urg_2016$idestablecimiento %in% estab_id, columns, drop = FALSE]
names(hosp_trauma) <-
  c (
    "id",
    "hosp_trauma_all",
    "hosp_trauma_a1",
    "hosp_trauma_1a4",
    "hosp_trauma_5a14",
    "hosp_trauma_15a64",
    "hosp_trauma_65a",
    "date"
  )

# -------- Merge Columns Into One Dataset --------
cons <- merge(cons_total, cons_resp)
cons <- merge(cons, cons_circ)
cons <- merge(cons, cons_diarrea)
cons <- merge(cons, cons_trauma)

hosps <- merge(hosp_total, hosp_resp)
hosps <- merge(hosps, hosp_circ)
hosps <- merge(hosps, hosp_trauma)

urg16_3 <- merge(cons, hosps)

# -------- Write CSV File --------
library(foreign)
write_csv(urg16_3, "urg16_3.csv")
