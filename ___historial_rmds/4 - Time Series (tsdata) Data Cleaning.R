# -------- Set Working Directory --------
#setwd("G:/My Drive/Research Data")
library(tidyverse)
library(dplyr)
library(lubridate)
library(readr)

# -------- Access Data --------
tsdata_19_1 <- read.csv("tsdata_19_1.csv")
tsdata_19_3 <- read.csv("tsdata_19_3.csv")
tsdata1 <- read.csv("tsdata1.csv")
tsdata3 <- read.csv("tsdata3.csv")

# -------- Sum Hospital ID Cases - tsdata_19_1 --------
tsdata_19_1$date <- as.Date(tsdata_19_1$date)

tsdata_19_1_cons <- tsdata_19_1 %>%
  group_by(date) %>%
  summarise(
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

# -------- Sum Hospital ID Cases - tsdata_19_3 --------
tsdata_19_3$date <- as.Date(tsdata_19_3$date)

tsdata_19_3_cons <- tsdata_19_3 %>%
  group_by(date) %>%
  summarise(
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

# -------- Sum Hospital ID Cases - tsdata1 --------
tsdata1$date <- as.Date(tsdata1$date)

tsdata1_cons <- tsdata1 %>%
  group_by(date) %>%
  summarise(
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

# -------- Sum Hospital ID Cases - tsdata3 --------
tsdata3$date <- as.Date(tsdata3$date)

tsdata3_cons <- tsdata3 %>%
  group_by(date) %>%
  summarise(
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

# -------- Write Cleaned CSV Files --------
write_csv(tsdata_19_1_cons, "tsdata_19_1_cons.csv")
write_csv(tsdata_19_3_cons, "tsdata_19_3_cons.csv")
write_csv(tsdata1, "tsdata1.csv")
write_csv(tsdata3, "tsdata3.csv")
write_csv(tsdata1_cons, "tsdata1_cons.csv")
write_csv(tsdata3_cons, "tsdata3_cons.csv")