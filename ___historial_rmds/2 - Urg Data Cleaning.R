# -------- Set Working Directory --------
#setwd("G:/My Drive/Research Data")
library(tidyverse)
library(dplyr)
library(tidyverse)

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

#INCORPORÉ ESTA CONFIGURACIÓN
Sys.setlocale(category = "LC_ALL", locale = "english")
# -------- Separate Date into YMD Columns - Urg15 within 1KM --------
urg15_1$date <- as.Date(urg15_1$date, "%d/%m/%Y")
urg15_1 <- urg15_1 %>%
  separate(date, sep = "-", into = c("year", "month", "day"))
urg15_1$date <- as.Date(with(urg15_1, paste(year, month, day, sep = "-")))
urg15_1 <- urg15_1[, c(1, 59, 2:4, 5:58)]

# -------- Separate Date into YMD Columns - Urg16 within 1KM --------
urg16_1$date <- as.Date(urg16_1$date, "%d/%m/%Y")
urg16_1 <- urg16_1 %>%
  separate(date, sep = "-", into = c("year", "month", "day"))
urg16_1$date <- as.Date(with(urg16_1, paste(year, month, day, sep = "-")))
urg16_1 <- urg16_1[, c(1, 59, 2:4, 5:58)]

# -------- Separate Date into YMD Columns - Urg17 within 1KM --------
urg17_1$date <- as.Date(urg17_1$date, "%d/%m/%Y")
urg17_1 <- urg17_1 %>%
  separate(date, sep = "-", into = c("year", "month", "day"))
urg17_1$date <- as.Date(with(urg17_1, paste(year, month, day, sep = "-")))
urg17_1 <- urg17_1[, c(1, 59, 2:4, 5:58)]

# -------- Separate Date into YMD Columns - Urg18 within 1KM --------
urg18_1$date <- as.Date(urg18_1$date, "%d/%m/%Y")
urg18_1 <- urg18_1 %>%
  separate(date, sep = "-", into = c("year", "month", "day"))
urg18_1$date <- as.Date(with(urg18_1, paste(year, month, day, sep = "-")))
urg18_1 <- urg18_1[, c(1, 59, 2:4, 5:58)]

# -------- Separate Date into YMD Columns - Urg19 within 1KM --------
urg19_1$date <- as.Date(urg19_1$date, "%d/%m/%Y")
urg19_1 <- urg19_1 %>%
  separate(date, sep = "-", into = c("year", "month", "day"))
urg19_1$date <- as.Date(with(urg19_1, paste(year, month, day, sep = "-")))
urg19_1 <- urg19_1[, c(1, 59, 2:4, 5:58)]

# -------- Separate Date into YMD Columns - Urg15 within 3KM --------
urg15_3$date <- as.Date(urg15_3$date, "%d/%m/%Y")
urg15_3 <- urg15_3 %>%
  separate(date, sep = "-", into = c("year", "month", "day"))
urg15_3$date <- as.Date(with(urg15_3, paste(year, month, day, sep = "-")))
urg15_3 <- urg15_3[, c(1, 59, 2:4, 5:58)]

# -------- Separate Date into YMD Columns - Urg16 within 3KM --------
urg16_3$date <- as.Date(urg16_3$date, "%d/%m/%Y")
urg16_3 <- urg16_3 %>%
  separate(date, sep = "-", into = c("year", "month", "day"))
urg16_3$date <- as.Date(with(urg16_3, paste(year, month, day, sep = "-")))
urg16_3 <- urg16_3[, c(1, 59, 2:4, 5:58)]

# -------- Separate Date into YMD Columns - Urg17 within 3KM --------
urg17_3$date <- as.Date(urg17_3$date, "%d/%m/%Y")
urg17_3 <- urg17_3 %>%
  separate(date, sep = "-", into = c("year", "month", "day"))
urg17_3$date <- as.Date(with(urg17_3, paste(year, month, day, sep = "-")))
urg17_3 <- urg17_3[, c(1, 59, 2:4, 5:58)]

# -------- Separate Date into YMD Columns - Urg18 within 3KM --------
urg18_3$date <- as.Date(urg18_3$date, "%d/%m/%Y")
urg18_3 <- urg18_3 %>%
  separate(date, sep = "-", into = c("year", "month", "day"))
urg18_3$date <- as.Date(with(urg18_3, paste(year, month, day, sep = "-")))
urg18_3 <- urg18_3[, c(1, 59, 2:4, 5:58)]

# -------- Separate Date into YMD Columns - Urg19 within 3KM --------
urg19_3$date <- as.Date(urg19_3$date, "%d/%m/%Y")
urg19_3 <- urg19_3 %>%
  separate(date, sep = "-", into = c("year", "month", "day"))
urg19_3$date <- as.Date(with(urg19_3, paste(year, month, day, sep = "-")))
urg19_3 <- urg19_3[, c(1, 59, 2:4, 5:58)]

# -------- Write Cleaned Urg CSV Files --------
library(foreign)
write_csv(urg15_1, "urg15_1.csv")
write_csv(urg15_3, "urg15_3.csv")
write_csv(urg16_1, "urg16_1.csv")
write_csv(urg16_3, "urg16_3.csv")
write_csv(urg17_1, "urg17_1.csv")
write_csv(urg17_3, "urg17_3.csv")
write_csv(urg18_1, "urg18_1.csv")
write_csv(urg18_3, "urg18_3.csv")
write_csv(urg19_1, "urg19_1.csv")
write_csv(urg19_3, "urg19_3.csv")