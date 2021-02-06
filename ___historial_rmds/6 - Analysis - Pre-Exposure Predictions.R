########################################################################
######## This file contains the R code used for the analysis of ########
########             our dataset used in the paper:             ########
##########  Time Series Regression to Evaluate Public Health  ########## 
###########    Effect of the 2019 Chilean Social Protests    ###########
########################################################################
remove(list = ls())

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

########## Load Packages ##########
library("foreign") ; library("tsModel") ; library("lmtest") ; library("Epi")
library("splines") ; library("vcd") ; library(readxl) ; library(astsa)
library(forecast) ; library(ggplot2) ;library(tseries) ; library(lubridate)
library(dplyr) ; library(MASS) ; library(readr) ; library(aweek) ; library(reshape2)
library(ggsci) ; library(broom)

########## Read Data from CSV File ############
#setwd("G:/My Drive/Research Data")
data15a64 <- read.csv(paste0(setwd(),"/pred_15-64_1km.csv")
data15a64 <- data15a64[data15a64$year == 2019, ]
########## Create "Week Since Start" Variable ########

pre.length <-
  c(1:(length(data15a64$date[data15a64$txtime == 0]) / 7))
week.pre <- rep(1, 7)
for (i in pre.length) {
  week.pre <- c(week.pre, rep(i + 1, 7))
}
week.pre <- week.pre[-(79:84)]

post.length <-
  c(1:(length(data15a64$date[data15a64$txtime == 1]) / 7))
week.post <- rep(1, 7)
for (j in post.length) {
  week.post <- c(week.post, rep(j + 1, 7))
}
week.post <- week.post[-(76:77)]

week <- c(week.pre, week.post)
data15a64$week <- week

remove(i)
remove(j)
remove(pre.length)
remove(post.length)
remove(week.pre)
remove(week.post)
remove(week)

########## Trauma ########## 
######## Consultations ########
##### New Dataset #####
TC_15a64 <-
  melt(
    data15a64,
    id.vars = c("date", "week", "did"),
    measure.vars = c("cons_trauma", "pred_TC_pre"),
    variable.name = "Group",
    value.name = "Cases"
  )

TC_15a64$Group <- as.character(TC_15a64$Group)
TC_15a64$Group[TC_15a64$Group == "cons_trauma"] <- "Actual"
TC_15a64$Group[TC_15a64$Group == "pred_TC_pre"] <- "Predicted"
TC_15a64$Group <- as.factor(TC_15a64$Group)
TC_15a64$Date <- TC_15a64$date
TC_15a64$Week <- TC_15a64$week
TC_15a64$Exposure <- TC_15a64$did
TC_15a64$Exposure[TC_15a64$Exposure == 0] <- "Pre-Exposure"
TC_15a64$Exposure[TC_15a64$Exposure == 1] <- "Post-Exposure"
TC_15a64 <- TC_15a64[-(1:3)]

##### Analysis #####
#### Overall ####
### Summary Stats ###
summary(TC_15a64$Cases[TC_15a64$Group == "Actual" & TC_15a64$Exposure == "Pre-Exposure"])
summary(TC_15a64$Cases[TC_15a64$Group == "Actual" & TC_15a64$Exposure == "Post-Exposure"])
summary(TC_15a64$Cases[TC_15a64$Group == "Predicted" & TC_15a64$Exposure == "Pre-Exposure"])
summary(TC_15a64$Cases[TC_15a64$Group == "Predicted" & TC_15a64$Exposure == "Post-Exposure"])
ggplot(TC_15a64, aes(Group, Cases, fill = Group)) +
  geom_boxplot() +
  ggtitle("Actual vs. Predicted Trauma Consultations") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.title = element_blank()
  ) +
  scale_color_jama() +
  facet_grid( ~ Exposure) +
  ggsave(paste("TC_15a64", "overall.png"), path = "G:/My Drive/Research Data/Final Results/Pre-Exposure/3KM/Model Summaries")

### Normality Test ###
with(TC_15a64, shapiro.test(Cases[Group == "Actual" & TC_15a64$Exposure == "Pre-Exposure"]))
with(TC_15a64, shapiro.test(Cases[Group == "Actual" & TC_15a64$Exposure == "Post-Exposure"]))
with(TC_15a64, shapiro.test(Cases[Group == "Predicted" & TC_15a64$Exposure == "Pre-Exposure"]))
with(TC_15a64, shapiro.test(Cases[Group == "Predicted" & TC_15a64$Exposure == "Post-Exposure"]))
### Mean Comparison Test ###
tcpret <- tidy(with(TC_15a64,
                    t.test(Cases[Group == "Actual" & TC_15a64$Exposure == "Pre-Exposure"], Cases[Group == "Predicted" & TC_15a64$Exposure == "Pre-Exposure"], alternative = "two.sided")))
with(TC_15a64,
     wilcox.test(Cases[Group == "Actual" & TC_15a64$Exposure == "Pre-Exposure"], Cases[Group == "Predicted" & TC_15a64$Exposure == "Pre-Exposure"], alternative = "two.sided"))
tcpost <- tidy(with(TC_15a64,
                    t.test(Cases[Group == "Actual" & TC_15a64$Exposure == "Post-Exposure"], Cases[Group == "Predicted" & TC_15a64$Exposure == "Post-Exposure"], alternative = "two.sided")))
with(TC_15a64,
     wilcox.test(Cases[Group == "Actual" & TC_15a64$Exposure == "Post-Exposure"], Cases[Group == "Predicted" & TC_15a64$Exposure == "Post-Exposure"], alternative = "two.sided"))

#### Weekly ####
### Summary Stats ###
capture.output(
  print(
    "Actual vs. Predicted Pre-Exposure Trauma Consultations (15-64) By Week"
  ),
  for (i in unique(TC_15a64$Week)) {
    act.sum <-
      with(TC_15a64, summary(Cases[Group == "Actual" &
                                         Week == i]))
    pred.sum <-
      with(TC_15a64, summary(Cases[Group == "Predicted" &
                                         Week == i]))
    print(paste("Week", i))
    print("    Actual")
    print(act.sum)
    print("    Predicted")
    print(pred.sum)
  },
  file = paste("summary stats", "TC_15a64", ".txt")
)

ggplot(TC_15a64, aes(as.factor(Week), Cases, fill = Group)) +
  geom_boxplot() +
  ggtitle("Actual vs. Predicted Pre-Exposure Trauma Consultations (15-64) By Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Week") +
  ggsave(paste("TC_15a64", "weekly.png"), path = "G:/My Drive/Research Data/Final Results/Pre-Exposure/3KM/Model Summaries")

### Normality Test ###
act.normality <- data.frame()
pred.normality <-data.frame()
for (i in unique(TC_15a64$Week)) {
  act.norm <-
    with(TC_15a64, shapiro.test(Cases[Group == "Actual" &
                                            Week == i]))
  act.normality <- rbind.data.frame(act.normality, act.norm)
  pred.norm <-
    with(TC_15a64, shapiro.test(Cases[Group == "Predicted" &
                                            Week == i]))
  pred.normality <- rbind.data.frame(pred.normality, pred.norm)
}
normality <- rbind(act.normality, pred.normality)
write_csv(normality, paste("shapiro_output", "TC_15a64", ".csv"))

### Mean Comparison Test ###
ttest <- data.frame()
for (i in unique(TC_15a64$Week)) {
  test <-
    tidy(with(TC_15a64,
              t.test(Cases[Group == "Actual" &
                             Week == i], Cases[Group == "Predicted" &
                                                 Week == i], alternative = "two.sided")))
  ttest <- rbind.data.frame(ttest, test)
}
write_csv(ttest, paste("test_output", "TC_15a64", ".csv"))

######## Hospitalizations ########
##### New Dataset #####
TH_15a64 <-
  melt(
    data15a64,
    id.vars = c("date", "week", "did"),
    measure.vars = c("hosp_trauma", "pred_TH_pre"),
    variable.name = "Group",
    value.name = "Cases"
  )

TH_15a64$Group <- as.character(TH_15a64$Group)
TH_15a64$Group[TH_15a64$Group == "hosp_trauma"] <- "Actual"
TH_15a64$Group[TH_15a64$Group == "pred_TH_pre"] <- "Predicted"
TH_15a64$Group <- as.factor(TH_15a64$Group)
TH_15a64$Date <- TH_15a64$date
TH_15a64$Week <- TH_15a64$week
TH_15a64$Exposure <- TH_15a64$did
TH_15a64$Exposure[TH_15a64$Exposure == 0] <- "Pre-Exposure"
TH_15a64$Exposure[TH_15a64$Exposure == 1] <- "Post-Exposure"
TH_15a64 <- TH_15a64[-(1:3)]

##### Analysis #####
#### Overall ####
### Summary Stats ###
summary(TH_15a64$Cases[TH_15a64$Group == "Actual" & TH_15a64$Exposure == "Pre-Exposure"])
summary(TH_15a64$Cases[TH_15a64$Group == "Actual" & TH_15a64$Exposure == "Post-Exposure"])
summary(TH_15a64$Cases[TH_15a64$Group == "Predicted" & TH_15a64$Exposure == "Pre-Exposure"])
summary(TH_15a64$Cases[TH_15a64$Group == "Predicted" & TH_15a64$Exposure == "Post-Exposure"])
ggplot(TH_15a64, aes(Group, Cases, fill = Group)) +
  geom_boxplot() +
  ggtitle("Actual vs. Predicted Pre-Exposure Trauma Hospitalizations") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.title = element_blank()
  ) +
  scale_color_jama() +
  facet_grid(~ Exposure) +
  ggsave(paste("TH_15a64", "overall.png"), path = "G:/My Drive/Research Data/Final Results/Pre-Exposure/3KM/Model Summaries")

### Normality Test ###
with(TH_15a64, shapiro.test(Cases[Group == "Actual" & TH_15a64$Exposure == "Pre-Exposure"]))
with(TH_15a64, shapiro.test(Cases[Group == "Actual" & TH_15a64$Exposure == "Post-Exposure"]))
with(TH_15a64, shapiro.test(Cases[Group == "Predicted" & TH_15a64$Exposure == "Pre-Exposure"]))
with(TH_15a64, shapiro.test(Cases[Group == "Predicted" & TH_15a64$Exposure == "Post-Exposure"]))
### Mean Comparison Test ###
thpret <- tidy(with(TH_15a64,
                    t.test(Cases[Group == "Actual" & TH_15a64$Exposure == "Pre-Exposure"], Cases[Group == "Predicted" & TH_15a64$Exposure == "Pre-Exposure"], alternative = "two.sided")))
with(TH_15a64,
     wilcox.test(Cases[Group == "Actual" & TH_15a64$Exposure == "Pre-Exposure"], Cases[Group == "Predicted" & TH_15a64$Exposure == "Pre-Exposure"], alternative = "two.sided"))
thpost <- tidy(with(TH_15a64,
                    t.test(Cases[Group == "Actual" & TH_15a64$Exposure == "Post-Exposure"], Cases[Group == "Predicted" & TH_15a64$Exposure == "Post-Exposure"], alternative = "two.sided")))
with(TH_15a64,
     wilcox.test(Cases[Group == "Actual" & TH_15a64$Exposure == "Post-Exposure"], Cases[Group == "Predicted" & TH_15a64$Exposure == "Post-Exposure"], alternative = "two.sided"))
#### Weekly ####
### Summary Stats ###
capture.output(
  print(
    "Actual vs. Predicted Pre-Exposure Trauma Hospitalizations (15-64) By Week"
  ),
  for (i in unique(TH_15a64$Week)) {
    act.sum <-
      with(TH_15a64, summary(Cases[Group == "Actual" &
                                         Week == i]))
    pred.sum <-
      with(TH_15a64, summary(Cases[Group == "Predicted" &
                                         Week == i]))
    print(paste("Week", i))
    print("    Actual")
    print(act.sum)
    print("    Predicted")
    print(pred.sum)
  },
  file = paste("summary stats", "TH_15a64", ".txt")
)
ggplot(TH_15a64, aes(as.factor(Week), Cases, fill = Group)) +
  geom_boxplot() +
  ggtitle("Actual vs. Predicted Pre-Exposure Trauma Hospitalizations (15-64) By Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Week") +
  ggsave(paste("TH_15a64", "weekly.png"), path = "G:/My Drive/Research Data/Final Results/Pre-Exposure/3KM/Model Summaries")


### Normality Test ###
act.normality <- data.frame()
pred.normality <-data.frame()
for (i in unique(TH_15a64$Week)) {
  act.norm <-
    with(TH_15a64, shapiro.test(Cases[Group == "Actual" &
                                            Week == i]))
  act.normality <- rbind.data.frame(act.normality, act.norm)
  pred.norm <-
    with(TH_15a64, shapiro.test(Cases[Group == "Predicted" &
                                            Week == i]))
  pred.normality <- rbind.data.frame(pred.normality, pred.norm)
}
normality <- rbind(act.normality, pred.normality)
write_csv(normality, paste("shapiro_output", "TH_15a64", ".csv"))

### Mean Comparison Test ###
ttest <- data.frame()
for (i in unique(TH_15a64$Week)) {
  test <-
    tidy(with(TH_15a64,
              t.test(Cases[Group == "Actual" &
                             Week == i], Cases[Group == "Predicted" &
                                                 Week == i], alternative = "two.sided")))
  ttest <- rbind.data.frame(ttest, test)
}
write_csv(ttest, paste("test_output", "TH_15a64", ".csv"))

########## Respiratory ##########
######## Consultations ########
##### New Dataset #####
RC_15a64 <-
  melt(
    data15a64,
    id.vars = c("date", "week", "did"),
    measure.vars = c("cons_resp", "pred_RC_pre"),
    variable.name = "Group",
    value.name = "Cases"
  )

RC_15a64$Group <- as.character(RC_15a64$Group)
RC_15a64$Group[RC_15a64$Group == "cons_resp"] <- "Actual"
RC_15a64$Group[RC_15a64$Group == "pred_RC_pre"] <- "Predicted"
RC_15a64$Group <- as.factor(RC_15a64$Group)
RC_15a64$Date <- RC_15a64$date
RC_15a64$Week <- RC_15a64$week
RC_15a64$Exposure <- RC_15a64$did
RC_15a64$Exposure[RC_15a64$Exposure == 0] <- "Pre-Exposure"
RC_15a64$Exposure[RC_15a64$Exposure == 1] <- "Post-Exposure"
RC_15a64 <- RC_15a64[-(1:3)]

##### Analysis #####
#### Overall ####
### Summary Stats ###
summary(RC_15a64$Cases[RC_15a64$Group == "Actual" & RC_15a64$Exposure == "Pre-Exposure"])
summary(RC_15a64$Cases[RC_15a64$Group == "Actual" & RC_15a64$Exposure == "Post-Exposure"])
summary(RC_15a64$Cases[RC_15a64$Group == "Predicted" & RC_15a64$Exposure == "Pre-Exposure"])
summary(RC_15a64$Cases[RC_15a64$Group == "Predicted" & RC_15a64$Exposure == "Post-Exposure"])
ggplot(RC_15a64, aes(Group, Cases, fill = Group)) +
  geom_boxplot() +
  ggtitle("Actual vs. Predicted Respiratory Consultations") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.title = element_blank()
  ) +
  scale_color_jama() +
  facet_grid( ~ Exposure) +
  ggsave(paste("RC_15a64", "overall.png"), path = "G:/My Drive/Research Data/Final Results/Pre-Exposure/3KM/Model Summaries")

### Normality Test ###
with(RC_15a64, shapiro.test(Cases[Group == "Actual" & RC_15a64$Exposure == "Pre-Exposure"]))
with(RC_15a64, shapiro.test(Cases[Group == "Actual" & RC_15a64$Exposure == "Post-Exposure"]))
with(RC_15a64, shapiro.test(Cases[Group == "Predicted" & RC_15a64$Exposure == "Pre-Exposure"]))
with(RC_15a64, shapiro.test(Cases[Group == "Predicted" & RC_15a64$Exposure == "Post-Exposure"]))
### Mean Comparison Test ###
rcpret <- tidy(with(RC_15a64,
                    t.test(Cases[Group == "Actual" & RC_15a64$Exposure == "Pre-Exposure"], Cases[Group == "Predicted" & RC_15a64$Exposure == "Pre-Exposure"], alternative = "two.sided")))
with(RC_15a64,
     wilcox.test(Cases[Group == "Actual" & RC_15a64$Exposure == "Pre-Exposure"], Cases[Group == "Predicted" & RC_15a64$Exposure == "Pre-Exposure"], alternative = "two.sided"))
rcpost <- tidy(with(RC_15a64,
                    t.test(Cases[Group == "Actual" & RC_15a64$Exposure == "Post-Exposure"], Cases[Group == "Predicted" & RC_15a64$Exposure == "Post-Exposure"], alternative = "two.sided")))
with(RC_15a64,
     wilcox.test(Cases[Group == "Actual" & RC_15a64$Exposure == "Post-Exposure"], Cases[Group == "Predicted" & RC_15a64$Exposure == "Post-Exposure"], alternative = "two.sided"))

#### Weekly ####
### Summary Stats ###
capture.output(
  print(
    "Actual vs. Predicted Pre-Exposure Respiratory Consultations (15-64) By Week"
  ),
  for (i in unique(RC_15a64$Week)) {
    act.sum <-
      with(RC_15a64, summary(Cases[Group == "Actual" &
                                         Week == i]))
    pred.sum <-
      with(RC_15a64, summary(Cases[Group == "Predicted" &
                                         Week == i]))
    print(paste("Week", i))
    print("    Actual")
    print(act.sum)
    print("    Predicted")
    print(pred.sum)
  },
  file = paste("summary stats", "RC_15a64", ".txt")
)
ggplot(RC_15a64, aes(as.factor(Week), Cases, fill = Group)) +
  geom_boxplot() +
  ggtitle("Actual vs. Predicted Pre-Exposure Respiratory Consultations (15-64) By Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Week") +
  ggsave(paste("RC_15a64", "weekly.png"), path = "G:/My Drive/Research Data/Final Results/Pre-Exposure/3KM/Model Summaries")


### Normality Test ###
act.normality <- data.frame()
pred.normality <-data.frame()
for (i in unique(RC_15a64$Week)) {
  act.norm <-
    with(RC_15a64, shapiro.test(Cases[Group == "Actual" &
                                            Week == i]))
  act.normality <- rbind.data.frame(act.normality, act.norm)
  pred.norm <-
    with(RC_15a64, shapiro.test(Cases[Group == "Predicted" &
                                            Week == i]))
  pred.normality <- rbind.data.frame(pred.normality, pred.norm)
}
normality <- rbind(act.normality, pred.normality)
write_csv(normality, paste("shapiro_output", "RC_15a64", ".csv"))

### Mean Comparison Test ###
ttest <- data.frame()
for (i in unique(RC_15a64$Week)) {
  test <-
    tidy(with(RC_15a64,
              t.test(Cases[Group == "Actual" &
                             Week == i], Cases[Group == "Predicted" &
                                                 Week == i], alternative = "two.sided")))
  ttest <- rbind.data.frame(ttest, test)
}
write_csv(ttest, paste("test_output", "RC_15a64", ".csv"))

######## Hospitalizations ########
##### New Dataset #####
RH_15a64 <-
  melt(
    data15a64,
    id.vars = c("date", "week", "did"),
    measure.vars = c("hosp_resp", "pred_RH_pre"),
    variable.name = "Group",
    value.name = "Cases"
  )

RH_15a64$Group <- as.character(RH_15a64$Group)
RH_15a64$Group[RH_15a64$Group == "hosp_resp"] <- "Actual"
RH_15a64$Group[RH_15a64$Group == "pred_RH_pre"] <- "Predicted"
RH_15a64$Group <- as.factor(RH_15a64$Group)
RH_15a64$Date <- RH_15a64$date
RH_15a64$Week <- RH_15a64$week
RH_15a64$Exposure <- RH_15a64$did
RH_15a64$Exposure[RH_15a64$Exposure == 0] <- "Pre-Exposure"
RH_15a64$Exposure[RH_15a64$Exposure == 1] <- "Post-Exposure"
RH_15a64 <- RH_15a64[-(1:3)]

##### Analysis #####
#### Overall ####
### Summary Stats ###
summary(RH_15a64$Cases[RH_15a64$Group == "Actual" & RH_15a64$Exposure == "Pre-Exposure"])
summary(RH_15a64$Cases[RH_15a64$Group == "Actual" & RH_15a64$Exposure == "Post-Exposure"])
summary(RH_15a64$Cases[RH_15a64$Group == "Predicted" & RH_15a64$Exposure == "Pre-Exposure"])
summary(RH_15a64$Cases[RH_15a64$Group == "Predicted" & RH_15a64$Exposure == "Post-Exposure"])
ggplot(RH_15a64, aes(Group, Cases, fill = Group)) +
  geom_boxplot() +
  ggtitle("Actual vs. Predicted Respiratory Hospitalizations") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.title = element_blank()
  ) +
  scale_color_jama() +
  facet_grid(~ Exposure) +
  ggsave(paste("RH_15a64", "overall.png"), path = "G:/My Drive/Research Data/Final Results/Pre-Exposure/3KM/Model Summaries")

### Normality Test ###
with(RH_15a64, shapiro.test(Cases[Group == "Actual" & RH_15a64$Exposure == "Pre-Exposure"]))
with(RH_15a64, shapiro.test(Cases[Group == "Actual" & RH_15a64$Exposure == "Post-Exposure"]))
with(RH_15a64, shapiro.test(Cases[Group == "Predicted" & RH_15a64$Exposure == "Pre-Exposure"]))
with(RH_15a64, shapiro.test(Cases[Group == "Predicted" & RH_15a64$Exposure == "Post-Exposure"]))
### Mean Comparison Test ###
rhpret <- tidy(with(RH_15a64,
                    t.test(Cases[Group == "Actual" & RH_15a64$Exposure == "Pre-Exposure"], Cases[Group == "Predicted" & RH_15a64$Exposure == "Pre-Exposure"], alternative = "two.sided")))
with(RH_15a64,
     wilcox.test(Cases[Group == "Actual" & RH_15a64$Exposure == "Pre-Exposure"], Cases[Group == "Predicted" & RH_15a64$Exposure == "Pre-Exposure"], alternative = "two.sided"))
rhpost <- tidy(with(RH_15a64,
                    t.test(Cases[Group == "Actual" & RH_15a64$Exposure == "Post-Exposure"], Cases[Group == "Predicted" & RH_15a64$Exposure == "Post-Exposure"], alternative = "two.sided")))
with(RH_15a64,
     wilcox.test(Cases[Group == "Actual" & RH_15a64$Exposure == "Post-Exposure"], Cases[Group == "Predicted" & RH_15a64$Exposure == "Post-Exposure"], alternative = "two.sided"))

#### Weekly ####
### Summary Stats ###
capture.output(
  print(
    "Actual vs. Predicted Pre-Exposure Respiratory Hospitalizations (15-64) By Week"
  ),
  for (i in unique(RH_15a64$Week)) {
    act.sum <-
      with(RH_15a64, summary(Cases[Group == "Actual" &
                                         Week == i]))
    pred.sum <-
      with(RH_15a64, summary(Cases[Group == "Predicted" &
                                         Week == i]))
    print(paste("Week", i))
    print("    Actual")
    print(act.sum)
    print("    Predicted")
    print(pred.sum)
  },
  file = paste("summary stats", "RH_15a64", ".txt")
)
ggplot(RH_15a64, aes(as.factor(Week), Cases, fill = Group)) +
  geom_boxplot() +
  ggtitle("Actual vs. Predicted Pre-Exposure Respiratory Hospitalizations (15-64) By Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Week") +
  ggsave(paste("RH_15a64", "weekly.png"), path = "G:/My Drive/Research Data/Final Results/Pre-Exposure/3KM/Model Summaries")


### Normality Test ###
act.normality <- data.frame()
pred.normality <-data.frame()
for (i in unique(RH_15a64$Week)) {
  act.norm <-
    with(RH_15a64, shapiro.test(Cases[Group == "Actual" &
                                            Week == i]))
  act.normality <- rbind.data.frame(act.normality, act.norm)
  pred.norm <-
    with(RH_15a64, shapiro.test(Cases[Group == "Predicted" &
                                            Week == i]))
  pred.normality <- rbind.data.frame(pred.normality, pred.norm)
}
normality <- rbind(act.normality, pred.normality)
write_csv(normality, paste("shapiro_output", "RH_15a64", ".csv"))

### Mean Comparison Test ###
ttest <- data.frame()
for (i in unique(RH_15a64$Week)) {
  test <-
    tidy(with(RH_15a64,
              t.test(Cases[Group == "Actual" &
                             Week == i], Cases[Group == "Predicted" &
                                                 Week == i], alternative = "two.sided")))
  ttest <- rbind.data.frame(ttest, test)
}
write_csv(ttest, paste("test_output", "RH_15a64", ".csv"))
