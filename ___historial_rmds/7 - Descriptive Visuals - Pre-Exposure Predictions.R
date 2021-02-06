########################################################################
######## This file contains the R code used for the analysis of ########
########             our dataset used in the paper:             ########
##########  Time Series Regression to Evaluate Public Health  ##########
###########    Effect of the 2019 Chilean Social Protests    ###########
########################################################################

########## Install Packages Required for Analysis ##########
# install.packages("lmtest")
# install.packages("Epi")
# install.packages("tsModel")
# install.packages("vcd")
# install.packages("readxl")
# install.packages("astsa")
# install.packages("forecast")
# install.packages("ggplot2")
# install.packages("tseries")
# install.packages("lubridate")
# install.packages("dplyr")
# install.packages("MASS")
# install.packages("readr")
# install.packages("aweek")
# install.packages("reshape2")
# install.packages("ggsci")
# install.packages("gridExtra")
# install.packages("ggpubr")

########## Load Packages ##########
library("foreign")
library("tsModel")
library("lmtest")
library("Epi")
library("splines")
library("vcd")
library(readxl)
library(astsa)
library(forecast)
library(ggplot2)
library(tseries)
library(lubridate)
library(dplyr)
library(MASS)
library(readr)
library(aweek)
library(reshape2)
library(ggsci)
library(broom)
library(gridExtra)
library(ggpubr)

########## Read Data from CSV File ############
#setwd("G:/My Drive/Research Data")
data <- read.csv("pred_15-64_3km.csv")
data <- data[data$year == 2019, ]
data$date <- as.Date(data$date)

########## Moving Average Line Plots ##########
#### New Datasets ####
TC <-
  melt(
    data,
    id.vars = c("date", "did"),
    measure.vars = c("cons_trauma", "pred_TC_pre"),
    variable.name = "Group",
    value.name = "Cases"
  )
TC$Group <- as.character(TC$Group)
TC$Group[TC$Group == "cons_trauma"] <- "Actual"
TC$Group[TC$Group == "pred_TC_pre"] <- "Predicted"
TC$Group <- as.factor(TC$Group)
TC$Date <- TC$date
TC$Week <- TC$week
TC <- TC[-(1:2)]
TH <-
  melt(
    data,
    id.vars = c("date", "did"),
    measure.vars = c("hosp_trauma", "pred_TH_pre"),
    variable.name = "Group",
    value.name = "Cases"
  )

TH$Group <- as.character(TH$Group)
TH$Group[TH$Group == "hosp_trauma"] <- "Actual"
TH$Group[TH$Group == "pred_TH_pre"] <- "Predicted"
TH$Group <- as.factor(TH$Group)
TH$Date <- TH$date
TH$Week <- TH$week
TH <- TH[-(1:2)]

RC <-
  melt(
    data,
    id.vars = c("date", "did"),
    measure.vars = c("cons_resp", "pred_RC_pre"),
    variable.name = "Group",
    value.name = "Cases"
  )
RC$Group <- as.character(RC$Group)
RC$Group[RC$Group == "cons_resp"] <- "Actual"
RC$Group[RC$Group == "pred_RC_pre"] <- "Predicted"
RC$Group <- as.factor(RC$Group)
RC$Date <- RC$date
RC$Week <- RC$week
RC <- RC[-(1:2)]

RH <-
  melt(
    data,
    id.vars = c("date", "did"),
    measure.vars = c("hosp_resp", "pred_RH_pre"),
    variable.name = "Group",
    value.name = "Cases"
  )
RH$Group <- as.character(RH$Group)
RH$Group[RH$Group == "hosp_resp"] <- "Actual"
RH$Group[RH$Group == "pred_RH_pre"] <- "Predicted"
RH$Group <- as.factor(RH$Group)
RH$Date <- RH$date
RH$Week <- RH$week
RH <- RH[-(1:2)]
#### Comparison Graphs ####
TC_comp <- ggplot(TC, aes(x = Date, y = Cases, color = Group)) +
  geom_line(aes(y = rollmean(Cases, 7, fill = NA))) +
  xlim(as.Date("2019-08-01"), as.Date("2019-12-31")) +
  geom_vline(aes(xintercept = as.Date("2019-10-18"))) +
  annotate(
    "rect",
    xmin = as.Date("2019-10-18"),
    xmax = as.Date("2019-12-31"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.2
  ) +
  scale_color_jco() +
  labs(color = "") +
  ggtitle("Actual Vs. Predicted Cases") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  ylab("Consultations") +
  xlab("")

TH_comp <- ggplot(TH, aes(x = Date, y = Cases, color = Group)) +
  geom_line(aes(y = rollmean(Cases, 7, fill = NA))) +
  xlim(as.Date("2019-08-01"), as.Date("2019-12-31")) +
  geom_vline(aes(xintercept = as.Date("2019-10-18"))) +
  annotate(
    "rect",
    xmin = as.Date("2019-10-18"),
    xmax = as.Date("2019-12-31"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.2
  ) +
  scale_color_jco() +
  labs(color = "") +
  ggtitle("") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    legend.position = "none"
  ) +
  ylab("Hospitalizations") +
  xlab("")

RC_comp <- ggplot(RC, aes(x = Date, y = Cases, color = Group)) +
  geom_line(aes(y = rollmean(Cases, 7, fill = NA))) +
  xlim(as.Date("2019-08-01"), as.Date("2019-12-31")) +
  geom_vline(aes(xintercept = as.Date("2019-10-18"))) +
  annotate(
    "rect",
    xmin = as.Date("2019-10-18"),
    xmax = as.Date("2019-12-31"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.2
  ) +
  scale_color_jco() +
  labs(color = "") +
  ggtitle("Actual Vs. Predicted Cases") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
        ) +
  ylab("Consultations") +
  xlab("")

RH_comp <- ggplot(RH, aes(x = Date, y = Cases, color = Group)) +
  geom_line(aes(y = rollmean(Cases, 7, fill = NA))) +
  xlim(as.Date("2019-08-01"), as.Date("2019-12-31")) +
  geom_vline(aes(xintercept = as.Date("2019-10-18"))) +
  annotate(
    "rect",
    xmin = as.Date("2019-10-18"),
    xmax = as.Date("2019-12-31"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.2
  ) +
  scale_color_jco() +
  labs(color = "") +
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        legend.position = "none") +
  ylab("Hospitalizations") +
  xlab("")

########## Write CSV Files ##########
 write_csv(TC, "TC.csv")
 write_csv(TH, "TH.csv")
 write_csv(RC, "RC.csv")
 write_csv(RH, "RH.csv")
# Cumulative differences calculated using the CSV files above in Microsoft Excel, then read back in to R #

########## Cumulative Difference Line Plots ##########
TCu <- read.csv("TCu3.csv")
THu <- read.csv("THu3.csv")
RCu <- read.csv("RCu3.csv")
RHu <- read.csv("RHu3.csv")

TCu$Date <- as.Date(TCu$Date)
THu$Date <- as.Date(THu$Date)
RCu$Date <- as.Date(RCu$Date)
RHu$Date <- as.Date(RHu$Date)

TC_cum <- ggplot(TC, aes(x = Date, y = Cumulative)) +
  geom_line() +
  xlim(as.Date("2019-08-01"), as.Date("2019-12-31")) +
  geom_vline(aes(xintercept = as.Date("2019-10-18"))) +
  annotate(
    "rect",
    xmin = as.Date("2019-10-18"),
    xmax = as.Date("2019-12-31"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.2
  ) +
  scale_color_jco() +
  labs(color = "") +
  ggtitle("Cumulative Difference") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  ylab("") +
  xlab("")

TH_cum <- ggplot(TH, aes(x = Date, y = Cumulative)) +
  geom_line() +
  xlim(as.Date("2019-08-01"), as.Date("2019-12-31")) +
  geom_vline(aes(xintercept = as.Date("2019-10-18"))) +
  annotate(
    "rect",
    xmin = as.Date("2019-10-18"),
    xmax = as.Date("2019-12-31"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.2
  ) +
  scale_color_jco() +
  labs(color = "") +
  ggtitle("") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    legend.position = "none"
  ) +
  ylab("") +
  xlab("")

RC_cum <- ggplot(RC, aes(x = Date, y = Cumulative)) +
  geom_line() +
  xlim(as.Date("2019-08-01"), as.Date("2019-12-31")) +
  geom_vline(aes(xintercept = as.Date("2019-10-18"))) +
  annotate(
    "rect",
    xmin = as.Date("2019-10-18"),
    xmax = as.Date("2019-12-31"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.2
  ) +
  scale_color_jco() +
  labs(color = "") +
  ggtitle("Cumulative Difference") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
        ) +
  ylab("") +
  xlab("")

RH_cum <- ggplot(RH, aes(x = Date, y = Cumulative)) +
  geom_line() +
  xlim(as.Date("2019-08-01"), as.Date("2019-12-31")) +
  geom_vline(aes(xintercept = as.Date("2019-10-18"))) +
  annotate(
    "rect",
    xmin = as.Date("2019-10-18"),
    xmax = as.Date("2019-12-31"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.2
  ) +
  scale_color_jco() +
  labs(color = "") +
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        legend.position = "none") +
  ylab("") +
  xlab("")

########## Combine Plots ##########
comp <-
  ggarrange(
    TC_comp,
    TH_comp,
    RC_comp,
    RH_comp,
    ncol = 2,
    nrow = 2,
    common.legend = TRUE,
    legend = "top",
    label.x = "Date",
    label.y = "Cases"
  )
annotate_figure(
  comp,
  top = text_grob("Actual vs. Predicted Daily Cases in 2019", size = 14),
  bottom = text_grob("Date"),
  left = text_grob("Cases", rot = 90)
)

cum <-
  ggarrange(
    TC_cum,
    TH_cum,
    RC_cum,
    RH_cum,
    ncol = 2,
    nrow = 2,
    common.legend = TRUE,
    legend = "top",
    label.x = "Date",
    label.y = "Cases"
  )
annotate_figure(
  cum,
  top = text_grob(
    "Cumulative Difference in Daily Cases in 2019",
    size = 14
  ),
  bottom = text_grob("Date"),
  left = text_grob("Cases", rot = 90)
)

Tcomb <-
  ggarrange(
    TC_comp,
    TC_cum,
    TH_comp,
    TH_cum,
    ncol = 2,
    nrow = 2,
    common.legend = TRUE,
    legend = "top",
    label.x = "Date",
    label.y = "Cases"
  )
annotate_figure(
  Tcomb,
  top = text_grob("Daily Trauma Cases Within 3KM", size = 14),
  bottom = text_grob("Date"),
  left = text_grob("Cases", rot = 90)
)

Rcomb <-
  ggarrange(
    RC_comp,
    RC_cum,
    RH_comp,
    RH_cum,
    ncol = 2,
    nrow = 2,
    common.legend = TRUE,
    legend = "top",
    label.x = "Date",
    label.y = "Cases"
  )
annotate_figure(
  Rcomb,
  top = text_grob("Daily Respiratory Cases Within 3 KM", size = 14),
  bottom = text_grob("Date"),
  left = text_grob("Cases", rot = 90)
)