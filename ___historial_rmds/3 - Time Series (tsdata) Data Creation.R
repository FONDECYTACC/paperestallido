# -------- Set Working Directory --------
#setwd("G:/My Drive/Research Data")
library(tidyverse)
library(dplyr)
library(lubridate)

# -------- Access Data --------
library(readr)
urg15_1 <- read.csv("urg15_1.csv")
urg15_3 <- read.csv("urg15_3.csv")
urg16_1 <- read.csv("urg16_1.csv")
urg16_3 <- read.csv("urg16_3.csv")
urg17_1 <- read.csv("urg17_1.csv")
urg17_3 <- read.csv("urg17_3.csv")
urg18_1 <- read.csv("urg18_1.csv")
urg18_3 <- read.csv("urg18_3.csv")
urg19_1 <- read.csv("urg19_1.csv")
urg19_3 <- read.csv("urg19_3.csv")

# -------- Isolate urg19_1 Hospital IDs 11-195 & 12-100 --------
# Isolate Hospital ID 11-195
urg19_1_11195 <- urg19_1[urg19_1$id == "11-195", ]
urg19_1_11195$year <- as.character(urg19_1_11195$year)
urg19_1_11195 <- urg19_1_11195 %>%
  group_by(date) %>%
  summarise(
    id = id,
    year = year,
    month = month,
    day = day,
    cons_total_all = cons_total_all,
    cons_total_15a64 = cons_total_15a64,
    cons_total_65a = cons_total_65a,
    cons_resp_all = cons_resp_all,
    cons_resp_15a64 = cons_resp_15a64,
    cons_resp_65a = cons_resp_65a,
    cons_circ_all = cons_circ_all,
    cons_circ_15a64 = cons_circ_15a64,
    cons_circ_65a = cons_circ_65a,
    cons_diarrea_all = cons_diarrea_all,
    cons_diarrea_15a64 = cons_diarrea_15a64,
    cons_diarrea_65a = cons_diarrea_65a,
    cons_trauma_all = cons_trauma_all,
    cons_trauma_15a64 = cons_trauma_15a64,
    cons_trauma_65a = cons_trauma_65a,
    hosp_total_all = hosp_total_all,
    hosp_total_15a64 = hosp_total_15a64,
    hosp_total_65a = hosp_total_65a,
    hosp_resp_all = hosp_resp_all,
    hosp_resp_15a64 = hosp_resp_15a64,
    hosp_resp_65a = hosp_resp_65a,
    hosp_circ_all = hosp_circ_all,
    hosp_circ_15a64 = hosp_circ_15a64,
    hosp_circ_65a = hosp_circ_65a,
    hosp_trauma_all = hosp_trauma_all,
    hosp_trauma_15a64 = hosp_trauma_15a64,
    hosp_trauma_65a = hosp_trauma_65a
  )
urg19_1_11195 <- urg19_1_11195[, c(2, 1, 3:32)]

# Isolate Hospital ID 12-100
urg19_1_12100 <- urg19_1[urg19_1$id == "12-100", ]
urg19_1_12100$year <- as.character(urg19_1_12100$year)
urg19_1_12100 <- urg19_1_12100 %>%
  group_by(date) %>%
  summarise(
    id = id,
    year = year,
    month = month,
    day = day,
    cons_total_all = cons_total_all,
    cons_total_15a64 = cons_total_15a64,
    cons_total_65a = cons_total_65a,
    cons_resp_all = cons_resp_all,
    cons_resp_15a64 = cons_resp_15a64,
    cons_resp_65a = cons_resp_65a,
    cons_circ_all = cons_circ_all,
    cons_circ_15a64 = cons_circ_15a64,
    cons_circ_65a = cons_circ_65a,
    cons_diarrea_all = cons_diarrea_all,
    cons_diarrea_15a64 = cons_diarrea_15a64,
    cons_diarrea_65a = cons_diarrea_65a,
    cons_trauma_all = cons_trauma_all,
    cons_trauma_15a64 = cons_trauma_15a64,
    cons_trauma_65a = cons_trauma_65a,
    hosp_total_all = hosp_total_all,
    hosp_total_15a64 = hosp_total_15a64,
    hosp_total_65a = hosp_total_65a,
    hosp_resp_all = hosp_resp_all,
    hosp_resp_15a64 = hosp_resp_15a64,
    hosp_resp_65a = hosp_resp_65a,
    hosp_circ_all = hosp_circ_all,
    hosp_circ_15a64 = hosp_circ_15a64,
    hosp_circ_65a = hosp_circ_65a,
    hosp_trauma_all = hosp_trauma_all,
    hosp_trauma_15a64 = hosp_trauma_15a64,
    hosp_trauma_65a = hosp_trauma_65a
  )
urg19_1_12100 <- urg19_1_12100[, c(2, 1, 3:32)]

# Re-Bind Data with only ID 11-195 & 12-100
urg19_1_clean <- rbind(urg19_1_11195, urg19_1_12100)

# -------- Isolate urg19_3 Hospital IDs 11-195 & 12-100 --------
# Isolate Hospital ID 09-100
urg19_3$date <- as.Date(urg19_3$date)
urg19_3_09100 <- urg19_3[urg19_3$id == "09-100", ]
urg19_3_09100$year <- as.character(urg19_3_09100$year)
urg19_3_09100 <- urg19_3_09100 %>%
  group_by(date) %>%
  summarise(
    id = id,
    year = year,
    month = month,
    day = day,
    cons_total_all = cons_total_all,
    cons_total_15a64 = cons_total_15a64,
    cons_total_65a = cons_total_65a,
    cons_resp_all = cons_resp_all,
    cons_resp_15a64 = cons_resp_15a64,
    cons_resp_65a = cons_resp_65a,
    cons_circ_all = cons_circ_all,
    cons_circ_15a64 = cons_circ_15a64,
    cons_circ_65a = cons_circ_65a,
    cons_diarrea_all = cons_diarrea_all,
    cons_diarrea_15a64 = cons_diarrea_15a64,
    cons_diarrea_65a = cons_diarrea_65a,
    cons_trauma_all = cons_trauma_all,
    cons_trauma_15a64 = cons_trauma_15a64,
    cons_trauma_65a = cons_trauma_65a,
    hosp_total_all = hosp_total_all,
    hosp_total_15a64 = hosp_total_15a64,
    hosp_total_65a = hosp_total_65a,
    hosp_resp_all = hosp_resp_all,
    hosp_resp_15a64 = hosp_resp_15a64,
    hosp_resp_65a = hosp_resp_65a,
    hosp_circ_all = hosp_circ_all,
    hosp_circ_15a64 = hosp_circ_15a64,
    hosp_circ_65a = hosp_circ_65a,
    hosp_trauma_all = hosp_trauma_all,
    hosp_trauma_15a64 = hosp_trauma_15a64,
    hosp_trauma_65a = hosp_trauma_65a
  )
urg19_3_09100 <- urg19_3_09100[, c(2, 1, 3:32)]

# Isolate Hospital ID 11-195
urg19_3_11195 <- urg19_3[urg19_3$id == "11-195", ]
urg19_3_11195$year <- as.character(urg19_3_11195$year)
urg19_3_11195 <- urg19_3_11195 %>%
  group_by(date) %>%
  summarise(
    id = id,
    year = year,
    month = month,
    day = day,
    cons_total_all = cons_total_all,
    cons_total_15a64 = cons_total_15a64,
    cons_total_65a = cons_total_65a,
    cons_resp_all = cons_resp_all,
    cons_resp_15a64 = cons_resp_15a64,
    cons_resp_65a = cons_resp_65a,
    cons_circ_all = cons_circ_all,
    cons_circ_15a64 = cons_circ_15a64,
    cons_circ_65a = cons_circ_65a,
    cons_diarrea_all = cons_diarrea_all,
    cons_diarrea_15a64 = cons_diarrea_15a64,
    cons_diarrea_65a = cons_diarrea_65a,
    cons_trauma_all = cons_trauma_all,
    cons_trauma_15a64 = cons_trauma_15a64,
    cons_trauma_65a = cons_trauma_65a,
    hosp_total_all = hosp_total_all,
    hosp_total_15a64 = hosp_total_15a64,
    hosp_total_65a = hosp_total_65a,
    hosp_resp_all = hosp_resp_all,
    hosp_resp_15a64 = hosp_resp_15a64,
    hosp_resp_65a = hosp_resp_65a,
    hosp_circ_all = hosp_circ_all,
    hosp_circ_15a64 = hosp_circ_15a64,
    hosp_circ_65a = hosp_circ_65a,
    hosp_trauma_all = hosp_trauma_all,
    hosp_trauma_15a64 = hosp_trauma_15a64,
    hosp_trauma_65a = hosp_trauma_65a
  )
urg19_3_11195 <- urg19_3_11195[, c(2, 1, 3:32)]

# Isolate Hospital ID 12-100
urg19_3_12100 <- urg19_3[urg19_3$id == "12-100", ]
urg19_3_12100$year <- as.character(urg19_3_12100$year)
urg19_3_12100 <- urg19_3_12100 %>%
  group_by(date) %>%
  summarise(
    id = id,
    year = year,
    month = month,
    day = day,
    cons_total_all = cons_total_all,
    cons_total_15a64 = cons_total_15a64,
    cons_total_65a = cons_total_65a,
    cons_resp_all = cons_resp_all,
    cons_resp_15a64 = cons_resp_15a64,
    cons_resp_65a = cons_resp_65a,
    cons_circ_all = cons_circ_all,
    cons_circ_15a64 = cons_circ_15a64,
    cons_circ_65a = cons_circ_65a,
    cons_diarrea_all = cons_diarrea_all,
    cons_diarrea_15a64 = cons_diarrea_15a64,
    cons_diarrea_65a = cons_diarrea_65a,
    cons_trauma_all = cons_trauma_all,
    cons_trauma_15a64 = cons_trauma_15a64,
    cons_trauma_65a = cons_trauma_65a,
    hosp_total_all = hosp_total_all,
    hosp_total_15a64 = hosp_total_15a64,
    hosp_total_65a = hosp_total_65a,
    hosp_resp_all = hosp_resp_all,
    hosp_resp_15a64 = hosp_resp_15a64,
    hosp_resp_65a = hosp_resp_65a,
    hosp_circ_all = hosp_circ_all,
    hosp_circ_15a64 = hosp_circ_15a64,
    hosp_circ_65a = hosp_circ_65a,
    hosp_trauma_all = hosp_trauma_all,
    hosp_trauma_15a64 = hosp_trauma_15a64,
    hosp_trauma_65a = hosp_trauma_65a
  )
urg19_3_12100 <- urg19_3_12100[, c(2, 1, 3:32)]

# Re-Bind Data with only ID 09-100, 11-195 & 12-100
urg19_3_clean <- rbind(urg19_3_09100, urg19_3_11195, urg19_3_12100)

# -------- Control Dataset - 1KM --------
urg_control_1 <- rbind(urg15_1, urg16_1, urg17_1, urg18_1)

# -------- Control Dataset - 3KM --------
urg_control_3 <- rbind(urg15_3, urg16_3, urg17_3, urg18_3)

# -------- Write Time Series CSV Files --------
write_csv(urg_control_1, "tsdata1.csv")
write_csv(urg_control_3, "tsdata3.csv")
write_csv(urg19_1_clean, "tsdata_19_1.csv")
write_csv(urg19_3_clean, "tsdata_19_3.csv")