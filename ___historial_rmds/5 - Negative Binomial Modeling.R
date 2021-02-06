########################################################################
######## This file contains the R code used for the analysis of ########
########             our dataset used in the paper:             ########
##########  Time Series Regression to Evaluate Public Health  ##########
###########    Effect of the 2019 Chilean Social Protests    ###########
########################################################################
#remove(list = ls())

########## Install Packages Required for Analysis ########
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

load(paste0(getwd(),"/","Procesos hasta 4.RData"))

########## Load Packages ########
library("foreign") ; library("tsModel") ; library("lmtest") ; library("Epi")
library("splines") ; library("vcd") ; library(readxl) ; library(astsa)
library(forecast) ; library(ggplot2) ;library(tseries) ; library(lubridate)
library(dplyr) ; library(MASS) ; library(readr); library("ggsci")

########## Read Data from CSV File ########
#setwd("G:/My Drive/Research Data")
data.control <- read.csv("tsdata3.csv")
data.tx <- read.csv("tsdata_19_3.csv")
data <- rbind(data.control, data.tx)
remove(data.control)
remove(data.tx)

########## This dataset includes the following variables: ########
# date
# year
# month
# day = Day of Month
# cons_total_all = Total Consultations - All Age Groups
# cons_total_15a64 = Total Consultations - Ages 15-64
# cons_total_65a = Total Consultations - Ages 65+
# cons_resp_all = Respiratory Consultations - All Age Groups
# cons_resp_15a64  = Respiratory Consultations - Ages 15-64
# cons_resp_65a = Respiratory Consultations - Ages 65+
# cons_circ_all = Circulatory Consultations - All Age Groups
# cons_circ_15a64 = Circulatory Consultations - Ages 15-64
# cons_circ_65a = Circulatory Consultations - Ages 65+
# cons_trauma_all = Trauma Consultations - All Age Groups
# cons_trauma_15a64 = Trauma Consultations - Ages 15-64
# cons_trauma_65a = Trauma Consultations - Ages 65+
# cons_diarrea_all = Diarrhea Consultations - All Age Groups
# cons_diarrea_15a64 = Diarrhea Consultations - Ages 15-64
# cons_diarrea_65a = Diarrhea Consultations - Ages 65+
# hosp_total_all = Total Hospitalizations - All Age Groups
# hosp_total_15a64 = Total Hospitalizations - Ages 15-64
# hosp_total_65a = Total Hospitalizations - Ages 65+
# hosp_resp_all = Respiratory Hospitalizations - All Age Groups
# hosp_resp_15a64 = Respiratory Hospitalizations - Ages 15-64
# hosp_resp_65a = Respiratory Hospitalizations - Ages 65+
# hosp_circ_all = Circulatory Hospitalizations - All Age Groups
# hosp_circ_15a64 = Circulatory Hospitalizations - Ages 15-64
# hosp_circ_65a = Circulatory Hospitalizations - Ages 65+
# hosp_trauma_all = Trauma Hospitalizations - All Age Groups
# hosp_trauma_15a64 = Trauma Hospitalizations - Ages 15-64
# hosp_trauma_65a = Trauma Hospitalizations - Ages 65+

########## Create Dummy Variables ########

## Dummy Variables for Age Group ##
data15a64 <- data %>%
  group_by(date) %>%
  summarise(
    year = year,
    month = month,
    day = day,
    cons_total = cons_total_15a64,
    cons_resp = cons_resp_15a64,
    cons_circ = cons_circ_15a64,
    cons_diarrea = cons_diarrea_15a64,
    cons_trauma = cons_trauma_15a64,
    hosp_total = hosp_total_15a64,
    hosp_resp = hosp_resp_15a64,
    hosp_circ = hosp_circ_15a64,
    hosp_trauma = hosp_trauma_15a64,
    age = "15-64"
  )

data65a <- data %>%
  group_by(date) %>%
  summarise(
    year = year,
    month = month,
    day = day,
    cons_total = cons_total_65a,
    cons_resp = cons_resp_65a,
    cons_circ = cons_circ_65a,
    cons_diarrea = cons_diarrea_65a,
    cons_trauma = cons_trauma_65a,
    hosp_total = hosp_total_65a,
    hosp_resp = hosp_resp_65a,
    hosp_circ = hosp_circ_65a,
    hosp_trauma = hosp_trauma_65a,
    age = "65+"
  )

data <- rbind(data15a64, data65a)
data$age <- as.factor(data$age)
remove(data15a64)
remove(data65a)

## Other Dummy Variables ##

data$exptime <- ifelse(data$month >=8, 1, 0)
data$txtime <- ifelse(data$month >= 10 & data$day >= 18 | data$month >= 11, 1, 0)
data$tx <- ifelse(data$year == "2019", 1, 0)
data$did <- data$txtime * data$tx

data$time <- 0
data$time[data$exptime == 1] <- seq(by = 1, from = 1, to = 153)
data$post <- 0
data$post[data$did == 1] <- seq(by = 1, to = 87) #no me funciona length(data$post[data$did == 1])= 150

data <- data[data$date != "2016-02-29", ]
data$yearday <- strftime(lubridate::parse_date_time(data$date, c("%Y-%m-%d"),exact=T), format = "%j")
data$yearday <- as.numeric(data$yearday)
data$yearday[data$year == 2016 & data$date > as.Date("2016-02-28")] <- data$yearday[data$year == 2016 & data$date > as.Date("2016-02-28")] - 1

Sys.setlocale(category = "LC_ALL", locale = "english")
data$weekday <- weekdays(as.Date(data$date))
data$weekday <- ordered(data$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

data$sig <- ifelse(data$month == 10 & data$day == 6 | data$month == 10 & data$day == 18 | data$month == 10 & data$day == 25, 1, 0)

## Dummy Variable for Dependent Variable of Choice Prior Day Value ##
data$prevtrc[data$age == "15-64"] <- c(NA, data$cons_trauma[data$age == "15-64"][-1825])
data$prevtrc[data$age == "65+"] <- c(NA, data$cons_trauma[data$age == "65+"][-1825])
data$prevrec[data$age == "15-64"] <- c(NA, data$cons_resp[data$age == "15-64"][-1825])
data$prevrec[data$age == "65+"] <- c(NA, data$cons_resp[data$age == "65+"][-1825])
data$prevtrh[data$age == "15-64"] <- c(NA, data$hosp_trauma[data$age == "15-64"][-1825])
data$prevtrh[data$age == "65+"] <- c(NA, data$hosp_trauma[data$age == "65+"][-1825])
data$prevreh[data$age == "15-64"] <- c(NA, data$hosp_resp[data$age == "15-64"][-1825])
data$prevreh[data$age == "65+"] <- c(NA, data$hosp_resp[data$age == "65+"][-1825])

data$difftrc <- data$cons_total - data$cons_trauma
data$difftrh <- data$hosp_total - data$hosp_trauma
data$diffrec <- data$cons_total - data$cons_resp
data$diffreh <- data$hosp_total - data$hosp_resp

data$offset <- data$cons_total + data$hosp_total

########## Dataset Formation for Experiment Time Frame ########
## Dataset During Experiment Time Only ##
data <- data[data$exptime == 1, ]
## Dataset for 15-64 Age Group ##
data15a64 <- data[data$age == "15-64", ]
## Dataset for 65+ Age Group ##
data65a <- data[data$age == "65+", ]

## Pre-Exposure Dataset Excluding 2019
#datapre <- datanew[datanew$txtime == 0 & datanew$tx == 0, ]
## Pre-Exposure Dataset 2019
#datapre19 <- datanew[datanew$txtime == 0 & datanew$tx == 1, ]
## Post-Exposure Dataset Excluding 2019
#datapost <- datanew[datanew$txtime == 1 & datanew$tx == 0, ]
## Post-Exposure Dataset 2019
#datapost19 <- datanew[datanew$txtime == 1 & datanew$tx == 1, ]
########## Descriptive Analyses ########
#### Trauma Consultations ####
### Scatter Plots ###
## Drop Leap Year Day, Plot Daily Data, Facet by Age & Year ##
ggplot(data15a64, aes(x = yearday, y = cons_trauma)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = rollmean(cons_trauma, k = 7, fill = NA)), color = "blue", size = 1) +
  facet_wrap(~ year, nrow = 2) +
  geom_vline(xintercept = 291, linetype = 1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Day of Year") +
  ylab("Cases") +
  ggtitle("Trauma Consultations (August-December)")

## Plot Day-of-Week Data, Facet by Year & Age ##
ggplot(data15a64, aes(x = weekday, y = cons_trauma, fill = weekday)) +
  geom_boxplot() +
  facet_wrap(~ year, nrow = 2) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_color_jco() +
  xlab("") +
  ylab("Cases") +
  ggtitle("Trauma Consultations (August-December)")

## Plot Daily Data, Facet by Day of Week & Age ##
ggplot(data15a64, aes(x = yearday, y = cons_trauma)) +
  geom_point() +
  geom_smooth(span = 0.1) +
  facet_wrap(~ age + weekday, nrow = 2) +
  geom_vline(xintercept = 291, linetype = 3) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Day of Year") +
  ylab("Cases") +
  ggtitle("Trauma Consultations (August-December)")+
  theme_bw()#agreguéfondo blanco

## Compare 2019 Data (Red) to Mean Control Data (Blue), Facet by Age ##
data$tx <- as.factor(data$tx)
ggplot(data, aes(x = yearday, y = cons_trauma, color = tx)) +
  geom_smooth(span = 0.1, se = FALSE) +
  #  geom_smooth(data = data[data$tx == 1,], span = 0.1, se = FALSE, col = "red") +
  facet_wrap(~ age, nrow = 2) +
  geom_vline(xintercept = 291, linetype = 3) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Day of Year") +
  ylab("Cases") +
  ggtitle("Trauma Consultations (August-December)") +
  scale_color_discrete(name = "", labels = c("Control (2015-2018", "Treatment (2019)"))
ggsave("trauma consultations,year year_trauma and tx 2015-2018 vs 2019 (treatment).png")

### Summarize Data by Age - Pre- & Post-Exposure ###
summary(data$cons_total[data$txtime == 1 & data$tx == 0 & data$age == "15-64"])
summary(data$cons_total[data$did == 1 & data$age == "15-64"])
summary(data$cons_total[data$txtime == 1 & data$tx == 0 & data$age == "65+"])
summary(data$cons_total[data$did == 1 & data$age == "65+"])

summary(data$cons_total[data$txtime == 0 & data$tx == 0 & data$age == "15-64"])
summary(data$cons_total[data$txtime == 0 & data$tx == 1 & data$age == "15-64"])
summary(data$cons_total[data$txtime == 0 & data$tx == 0 & data$age == "65+"])
summary(data$cons_total[data$txtime == 0 & data$tx == 1 & data$age == "65+"])

########## Fitting Models to Data ##########
######## Trauma ########
####### Consultations ######
###### 15-64 ######
##### Pre-Exposure #####
#### Model A with Simple Linear Regression Modeling ####
data15a64$consdiff <- data15a64$cons_total - (data15a64$cons_trauma + data15a64$cons_resp)
modelA <- lm(cons_trauma ~ offset(log(offset)) + year + month + day + weekday + yearday + prevtrc + difftrc, data15a64[data15a64$tx == 0 & data15a64$txtime == 0,])
anova(modelA)
summary(modelA)
checkresiduals(modelA)

#### Model B with Poisson Regression Modeling ####
modelB <- glm(cons_trauma ~ cons_diarrea + year + month + day + weekday + yearday + prevtrc, family = poisson, data15a64[data15a64$tx == 0 & data15a64$txtime == 0,])
summary(modelB)
summary(modelB)$dispersion
round(ci.lin(modelB, Exp = T), 3)
checkresiduals(modelB) #lo agregué para ver qué pasa

### Autocorrelation Testing ###
resB <- residuals(modelB,type="deviance")
plot(rep(1:66, 4),resB,pch=19,cex=0.7,col=grey(0.6),
     main="Residuals over time",ylab="Deviance residuals",xlab="Date")
abline(h=0,lty=2,lwd=2)
par(mfrow = c(2,1))
acf(resB)
pacf(resB)
checkresiduals(modelB)
par(mfrow = c(1,1))

#### Model C with Quasi-Poisson Regression Modeling ####
modelC <- glm(cons_trauma ~ cons_diarrea + year + month + day + weekday + yearday + prevtrc, family = quasipoisson, data15a64[data15a64$tx == 0 & data15a64$txtime == 0,])
summary(modelC)
summary(modelC)$dispersion
round(ci.lin(modelC, Exp = T), 3)

### Autocorrelation Testing ###
resC <- residuals(modelC,type="deviance")
plot(rep(1:66, 4),resC,pch=19,cex=0.7,col=grey(0.6),
     main="Residuals over time",ylab="Deviance residuals",xlab="Date")
abline(h=0,lty=2,lwd=2)
par(mfrow = c(2,1))
acf(resC)
pacf(resC)
checkresiduals(modelC)
par(mfrow = c(1,1))

data15a64$consdiff = data15a64$cons_total - data15a64$cons_trauma
data15a64$hospdiff = data15a64$hosp_total - data15a64$hosp_trauma
#### Model D with Negative Binomial Regression Modeling ####
modelD <-
  glm.nb(
    cons_trauma ~ offset(log(offset)) + year + as.Date(date) + month + day + weekday + yearday + prevtrc + difftrc + hosp_trauma,
    data15a64[data15a64$tx == 0 & data15a64$txtime == 0, ]
  )
summary(modelD)
summary(modelD)$dispersion
round(ci.lin(modelD, Exp = T), 3)

### Autocorrelation Testing ###
resD <- residuals(modelD,type="deviance")
plot(rep(1:78, 4),resD,pch=19,cex=0.7,col=grey(0.6),
     main="Residuals over time",ylab="Deviance residuals",xlab="Date")
abline(h=0,lty=2,lwd=2)
par(mfrow = c(2,1))
acf(resD)
pacf(resD)
checkresiduals(modelD)
par(mfrow = c(1,1))

### Add Predicted values to Dataset ###
data15a64$pred_TC_pre <- predict(modelD, type = "response", data15a64)

### Plot Predicted vs. Expected Values ###
ggplot(data15a64, aes(x = yearday, y = pred_TC_pre)) +
  geom_line(col = "blue") +
  geom_point(aes(y = cons_trauma), cex = 0.75) +
  geom_smooth(aes(y = cons_trauma), cex = 0.75, span = 0.1) +
  facet_wrap(~year, nrow = 2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Day of Year") +
  ylab("Cases") +
  xlim(213, 279) +
  ggtitle("Trauma Consultations with Negative Binomial Regression Model (15-64)")
ggsave("Trauma Consultations with Negative Binomial Regression Model (15-64).png")

##### Post-Exposure #####
#### Model A with Simple Linear Regression Modeling ####
modelA <- lm(cons_trauma ~ cons_diarrea + year + month + day + weekday + yearday + prevtrc, data15a64[data15a64$tx == 0 & data15a64$txtime == 1,])
anova(modelA)
summary(modelA)
checkresiduals(modelA)

#### Model B with Poisson Regression Modeling ####
modelB <- glm(cons_trauma ~ cons_diarrea + year + month + day + weekday + yearday + prevtrc, family = poisson(), data15a64[data15a64$tx == 0 & data15a64$txtime == 1,])
summary(modelB)
summary(modelB)$dispersion
round(ci.lin(modelB, Exp = T), 3)
checkresiduals(modelB) #LM test = 43.235, df = 16, p-value = 0.0002576

#### Model C with Quasi-Poisson Regression Modeling ####
modelC <- glm(cons_trauma ~ cons_diarrea + year + month + day + weekday + yearday + prevtrc, family = quasipoisson(), data15a64[data15a64$tx == 0 & data15a64$txtime == 1,])
summary(modelC)
summary(modelC)$dispersion
round(ci.lin(modelC, Exp = T), 3)

#### Model D with Negative Binomial Regression Modeling ####
modelD <-
  glm.nb(
    cons_trauma ~ offset(log(offset)) + year + as.Date(date) + month + day + weekday + yearday + prevtrc + difftrc + hosp_trauma,
    data15a64[data15a64$tx == 0 & data15a64$txtime == 1, ]
  )
summary(modelD)
summary(modelD)$dispersion
round(ci.lin(modelD, Exp = T), 3)

### Autocorrelation Testing ###
resD <- residuals(modelD,type="deviance")
plot(rep(1:75, 4),resD,pch=19,cex=0.7,col=grey(0.6),
     main="Residuals over time",ylab="Deviance residuals",xlab="Date")
abline(h=0,lty=2,lwd=2)
par(mfrow = c(2,1))
acf(resD)
pacf(resD)
checkresiduals(modelD)
par(mfrow = c(1,1))

## Predict values based on Quasi-Poisson ##
data15a64$pred_TC_post <- predict(modelD, type = "response", data15a64)

ggplot(data15a64, aes(x = yearday, y = pred_TC_post)) +
  geom_line(col = "blue") +
  geom_point(aes(y = cons_trauma), cex = 0.75) +
  geom_smooth(aes(y = cons_trauma), cex = 0.75, span = 0.1) +
  facet_wrap(~year, nrow = 2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Day of Year") +
  ylab("Cases") +
  xlim(280, 365) +
  ggtitle("Trauma Consultations with Negative Binomial Regression Model (15-64)")

####### Hospitalizations ######
###### 15-64 ######
##### Pre-Exposure #####
#### Model A with Simple Linear Regression Modeling ####
modelA <- lm(hosp_trauma ~ hosp_circ + year + month + day + weekday + yearday + prevtrh, data15a64[data15a64$tx == 0 & data15a64$txtime == 0,])
anova(modelA)
summary(modelA)
checkresiduals(modelA)

#### Model B with Poisson Regression Modeling ####
modelB <- glm(hosp_trauma ~ hosp_circ + year + month + day + weekday + yearday + prevtrh, family = poisson, data15a64[data15a64$tx == 0 & data15a64$txtime == 0,])
summary(modelB)
summary(modelB)$dispersion
round(ci.lin(modelB, Exp = T), 3)

### Autocorrelation Testing ###
resB <- residuals(modelB,type="deviance")
plot(rep(1:66, 4),resB,pch=19,cex=0.7,col=grey(0.6),
     main="Residuals over time",ylab="Deviance residuals",xlab="Date")
abline(h=0,lty=2,lwd=2)
par(mfrow = c(2,1))
acf(resB)
pacf(resB)
checkresiduals(modelB)
par(mfrow = c(1,1))

#### Model C with Quasi-Poisson Regression Modeling ####
modelC <- glm(hosp_trauma ~ hosp_circ + year + month + day + weekday + yearday + prevtrh, family = quasipoisson, data15a64[data15a64$tx == 0 & data15a64$txtime == 0,])
summary(modelC)
summary(modelC)$dispersion
round(ci.lin(modelC, Exp = T), 3)

### Autocorrelation Testing ###
resC <- residuals(modelC,type="deviance")
plot(rep(1:66, 4),resC,pch=19,cex=0.7,col=grey(0.6),
     main="Residuals over time",ylab="Deviance residuals",xlab="Date")
abline(h=0,lty=2,lwd=2)
par(mfrow = c(2,1))
acf(resC)
pacf(resC)
checkresiduals(modelC)
par(mfrow = c(1,1))

#### Model D with Negative Binomial Regression Modeling ####
modelD <-
  glm.nb(
    hosp_trauma ~ offset(log(offset)) + year + as.Date(date) + month + day + weekday + yearday + prevtrh + difftrh + cons_trauma,
    data15a64[data15a64$tx == 0 & data15a64$txtime == 0, ]
  )
summary(modelD)
summary(modelD)$dispersion
round(ci.lin(modelD, Exp = T), 3)

### Autocorrelation Testing ###
resD <- residuals(modelD,type="deviance")
plot(rep(1:66, 4),resD,pch=19,cex=0.7,col=grey(0.6),
     main="Residuals over time",ylab="Deviance residuals",xlab="Date")
abline(h=0,lty=2,lwd=2)
par(mfrow = c(2,1))
acf(resD)
pacf(resD)
checkresiduals(modelD)
par(mfrow = c(1,1))

### Add Predicted values to Dataset ###
data15a64$pred_TH_pre <- predict(modelD, type = "response", data15a64)

### Plot Predicted vs. Expected Values ###
ggplot(data15a64, aes(x = yearday, y = pred_TH_pre)) +
  geom_line(col = "blue") +
  geom_point(aes(y = hosp_trauma), cex = 0.75) +
  geom_smooth(aes(y = hosp_trauma), cex = 0.75, span = 0.1) +
  facet_wrap(~year, nrow = 2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Day of Year") +
  ylab("Cases") +
  xlim(213, 279) +
  ggtitle("Trauma Hospitalizations with Negative Binomial Regression Model (15-64)")

##### Post-Exposure #####
#### Model A with Simple Linear Regression Modeling ####
modelA <- lm(hosp_trauma ~ hosp_circ + year + month + day + weekday + yearday + prevtrh, data15a64[data15a64$tx == 0 & data15a64$txtime == 1,])
anova(modelA)
summary(modelA)
checkresiduals(modelA)

#### Model B with Poisson Regression Modeling ####
modelB <- glm(hosp_trauma ~ hosp_circ + year + month + day + weekday + yearday + prevtrh, family = poisson(), data15a64[data15a64$tx == 0 & data15a64$txtime == 1,])
summary(modelB)
summary(modelB)$dispersion
round(ci.lin(modelB, Exp = T), 3)
checkresiduals(modelB)

#### Model C with Quasi-Poisson Regression Modeling ####
modelC <- glm(hosp_trauma ~ hosp_circ + year + month + day + weekday + yearday + prevtrh, family = quasipoisson(), data15a64[data15a64$tx == 0 & data15a64$txtime == 1,])
summary(modelC)
summary(modelC)$dispersion
round(ci.lin(modelC, Exp = T), 3)

#### Model D with Negative Binomial Regression Modeling ####
modelD <-
  glm.nb(
    hosp_trauma ~ offset(log(offset)) + year + as.Date(date) + month + day + weekday + yearday + prevtrh + difftrh + cons_trauma,
    data15a64[data15a64$tx == 0 & data15a64$txtime == 1, ]
  )
summary(modelD)
summary(modelD)$dispersion
round(ci.lin(modelD, Exp = T), 3)

### Autocorrelation Testing ###
resD <- residuals(modelD,type="deviance")
plot(rep(1:87, 4),resD,pch=19,cex=0.7,col=grey(0.6),
     main="Residuals over time",ylab="Deviance residuals",xlab="Date")
abline(h=0,lty=2,lwd=2)
par(mfrow = c(2,1))
acf(resD)
pacf(resD)
checkresiduals(modelD)
par(mfrow = c(1,1))

## Predict values based on Quasi-Poisson ##
data15a64$pred_TH_post <- predict(modelD, type = "response", data15a64)

ggplot(data15a64, aes(x = yearday, y = pred_TH_post)) +
  geom_line(col = "blue") +
  geom_point(aes(y = hosp_trauma), cex = 0.75) +
  geom_smooth(aes(y = hosp_trauma), cex = 0.75, span = 0.1) +
  facet_wrap(~year, nrow = 2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Day of Year") +
  ylab("Cases") +
  xlim(280, 365) +
  ggtitle("Trauma Hospitalizations with Negative Binomial Regression Model (15-64)")

######## Respiratory ########
####### Consultations ######
###### 15-64 ######
##### Pre-Exposure #####
#### Model A with Simple Linear Regression Modeling ####
modelA <- lm(cons_resp ~ cons_circ + year + month + day + weekday + yearday + prevrec, data15a64[data15a64$tx == 0 & data15a64$txtime == 0,])
anova(modelA)
summary(modelA)
checkresiduals(modelA)

#### Model B with Poisson Regression Modeling ####
modelB <- glm(cons_resp ~ cons_circ + year + month + day + weekday + yearday + prevrec, family = poisson, data15a64[data15a64$tx == 0 & data15a64$txtime == 0,])
summary(modelB)
summary(modelB)$dispersion
round(ci.lin(modelB, Exp = T), 3)

### Autocorrelation Testing ###
resB <- residuals(modelB,type="deviance")
plot(rep(1:66, 4),resB,pch=19,cex=0.7,col=grey(0.6),
     main="Residuals over time",ylab="Deviance residuals",xlab="Date")
abline(h=0,lty=2,lwd=2)
par(mfrow = c(2,1))
acf(resB)
pacf(resB)
checkresiduals(modelB)
par(mfrow = c(1,1))

#### Model C with Quasi-Poisson Regression Modeling ####
modelC <- glm(cons_resp ~ cons_circ + year + month + day + weekday + yearday + prevrec, family = quasipoisson, data15a64[data15a64$tx == 0 & data15a64$txtime == 0,])
summary(modelC)
summary(modelC)$dispersion
round(ci.lin(modelC, Exp = T), 3)

### Autocorrelation Testing ###
resC <- residuals(modelC,type="deviance")
plot(rep(1:66, 4),resC,pch=19,cex=0.7,col=grey(0.6),
     main="Residuals over time",ylab="Deviance residuals",xlab="Date")
abline(h=0,lty=2,lwd=2)
par(mfrow = c(2,1))
acf(resC)
pacf(resC)
checkresiduals(modelC)
par(mfrow = c(1,1))

#### Model D with Negative Binomial Regression Modeling ####
modelD <-
  glm.nb(
    cons_resp ~ offset(log(offset)) + year + as.Date(date) + month + day + weekday + yearday + prevrec + diffrec + hosp_resp,
    data15a64[data15a64$tx == 0 & data15a64$txtime == 0, ]
  )
summary(modelD) #Residual deviance: 275.2  on 251  degrees of freedom
summary(modelD)$dispersion
round(ci.lin(modelD, Exp = T), 3)

### Autocorrelation Testing ###
resD <- residuals(modelD,type="deviance")
plot(rep(1:66, 4),resD,pch=19,cex=0.7,col=grey(0.6),
     main="Residuals over time",ylab="Deviance residuals",xlab="Date")
abline(h=0,lty=2,lwd=2)
par(mfrow = c(2,1))
acf(resD)
pacf(resD)
checkresiduals(modelD)
par(mfrow = c(1,1))

### Add Predicted values to Dataset ###
data15a64$pred_RC_pre <- predict(modelD, type = "response", data15a64)

### Plot Predicted vs. Expected Values ###
ggplot(data15a64, aes(x = yearday, y = pred_RC_pre)) +
  geom_line(col = "blue") +
  geom_point(aes(y = cons_resp), cex = 0.75) +
  geom_smooth(aes(y = cons_resp), cex = 0.75, span = 0.1) +
  facet_wrap(~year, nrow = 2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Day of Year") +
  ylab("Cases") +
  xlim(213, 279) +
  ggtitle("Respiratory Consultations with Negative Binomial Regression Model (15-64)")

##### Post-Exposure #####
#### Model A with Simple Linear Regression Modeling ####
modelA <- lm(cons_resp ~ cons_circ + year + month + day + weekday + yearday + prevrec, data15a64[data15a64$tx == 0 & data15a64$txtime == 1,])
anova(modelA)
summary(modelA)
checkresiduals(modelA)

#### Model B with Poisson Regression Modeling ####
modelB <- glm(cons_resp ~ cons_circ + year + month + day + weekday + yearday + prevrec, family = poisson(), data15a64[data15a64$tx == 0 & data15a64$txtime == 1,])
summary(modelB)
summary(modelB)$dispersion
round(ci.lin(modelB, Exp = T), 3)
checkresiduals(modelB)

#### Model C with Quasi-Poisson Regression Modeling ####
modelC <- glm(cons_resp ~ cons_circ + year + month + day + weekday + yearday + prevrec, family = quasipoisson(), data15a64[data15a64$tx == 0 & data15a64$txtime == 1,])
summary(modelC)
summary(modelC)$dispersion
round(ci.lin(modelC, Exp = T), 3)

#### Model D with Negative Binomial Regression Modeling ####
modelD <-
  glm.nb(
    cons_resp ~ offset(log(offset)) + year + as.Date(date) + month + day + weekday + yearday + prevrec + diffrec + hosp_resp,
    data15a64[data15a64$tx == 0 & data15a64$txtime == 1, ]
  )
summary(modelD)
summary(modelD)$dispersion
round(ci.lin(modelD, Exp = T), 3)

### Autocorrelation Testing ###
resD <- residuals(modelD,type="deviance")
plot(rep(1:87, 4),resD,pch=19,cex=0.7,col=grey(0.6),
     main="Residuals over time",ylab="Deviance residuals",xlab="Date")
abline(h=0,lty=2,lwd=2)
par(mfrow = c(2,1))
acf(resD)
pacf(resD)
checkresiduals(modelD)
par(mfrow = c(1,1))

## Predict values based on Quasi-Poisson ##
data15a64$pred_RC_post <- predict(modelD, type = "response", data15a64)

ggplot(data15a64, aes(x = yearday, y = pred_RC_post)) +
  geom_line(col = "blue") +
  geom_point(aes(y = cons_resp), cex = 0.75) +
  geom_smooth(aes(y = cons_resp), cex = 0.75, span = 0.1) +
  facet_wrap(~year, nrow = 2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Day of Year") +
  ylab("Cases") +
  xlim(280, 365) +
  ggtitle("Respiratory Consultations with Negative Binomial Regression Model (15-64)")

####### Hospitalizations ######
###### 15-64 ######
##### Pre-Exposure #####
#### Model A with Simple Linear Regression Modeling ####
modelA <- lm(hosp_resp ~ hosp_circ + year + month + day + weekday + yearday + prevreh, data15a64[data15a64$tx == 0 & data15a64$txtime == 0,])
anova(modelA)
summary(modelA)
checkresiduals(modelA)

#### Model B with Poisson Regression Modeling ####
modelB <- glm(hosp_resp ~ hosp_circ + year + month + day + weekday + yearday + prevreh, family = poisson, data15a64[data15a64$tx == 0 & data15a64$txtime == 0,])
summary(modelB)
summary(modelB)$dispersion
round(ci.lin(modelB, Exp = T), 3)

### Autocorrelation Testing ###
resB <- residuals(modelB,type="deviance")
plot(rep(1:66, 4),resB,pch=19,cex=0.7,col=grey(0.6),# no me funciona. Ver por qué piensa que hay 4 rep de 1 a 66. length(rep(1:66, 4))=264 vs length(resB)=312, DELTA=48
     main="Residuals over time",ylab="Deviance residuals",xlab="Date")
abline(h=0,lty=2,lwd=2)
par(mfrow = c(2,1))
acf(resB)
pacf(resB)
checkresiduals(modelB)
par(mfrow = c(1,1))

#### Model C with Quasi-Poisson Regression Modeling ####
modelC <- glm(hosp_resp ~ hosp_circ + year + month + day + weekday + yearday + prevreh, family = quasipoisson, data15a64[data15a64$tx == 0 & data15a64$txtime == 0,])
summary(modelC)
summary(modelC)$dispersion
round(ci.lin(modelC, Exp = T), 3)

### Autocorrelation Testing ###
resC <- residuals(modelC,type="deviance")
plot(rep(1:66, 4),resC,pch=19,cex=0.7,col=grey(0.6),
     main="Residuals over time",ylab="Deviance residuals",xlab="Date")
abline(h=0,lty=2,lwd=2)
par(mfrow = c(2,1))
acf(resC)
pacf(resC)
checkresiduals(modelC)
par(mfrow = c(1,1))

#### Model D with Negative Binomial Regression Modeling ####
modelD <-
  glm.nb(
    hosp_resp ~ offset(log(offset)) + year + as.Date(date) + month + day + weekday + yearday + prevreh + diffreh + cons_resp,
    data15a64[data15a64$tx == 0 & data15a64$txtime == 0, ]
  )
summary(modelD) #Residual deviance: 275.2  on 251  degrees of freedom
summary(modelD)$dispersion
round(ci.lin(modelD, Exp = T), 3)

### Autocorrelation Testing ###
resD <- residuals(modelD,type="deviance")
plot(rep(1:66, 4),resD,pch=19,cex=0.7,col=grey(0.6),
     main="Residuals over time",ylab="Deviance residuals",xlab="Date")
abline(h=0,lty=2,lwd=2)
par(mfrow = c(2,1))
acf(resD)
pacf(resD)
checkresiduals(modelD)
par(mfrow = c(1,1))

### Add Predicted values to Dataset ###
data15a64$pred_RH_pre <- predict(modelD, type = "response", data15a64)

### Plot Predicted vs. Expected Values ###
ggplot(data15a64, aes(x = yearday, y = pred_RH_pre)) +
  geom_line(col = "blue") +
  geom_point(aes(y = hosp_resp), cex = 0.75) +
  geom_smooth(aes(y = hosp_resp), cex = 0.75, span = 0.1) +
  facet_wrap(~year, nrow = 2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Day of Year") +
  ylab("Cases") +
  xlim(213, 279) +
  ggtitle("Respiratory Hospitalizations with Negative Binomial Regression Model (15-64)")

##### Post-Exposure #####
#### Model A with Simple Linear Regression Modeling ####
modelA <- lm(hosp_resp ~ hosp_circ + year + month + day + weekday + yearday + prevreh, data15a64[data15a64$tx == 0 & data15a64$txtime == 1,])
anova(modelA)
summary(modelA)
checkresiduals(modelA)

#### Model B with Poisson Regression Modeling ####
modelB <- glm(hosp_resp ~ hosp_circ + year + month + day + weekday + yearday + prevreh, family = poisson(), data15a64[data15a64$tx == 0 & data15a64$txtime == 1,])
summary(modelB)
summary(modelB)$dispersion
round(ci.lin(modelB, Exp = T), 3)
checkresiduals(modelB)

#### Model C with Quasi-Poisson Regression Modeling ####
modelC <- glm(hosp_resp ~ hosp_circ + year + month + day + weekday + yearday + prevreh, family = quasipoisson(), data15a64[data15a64$tx == 0 & data15a64$txtime == 1,])
summary(modelC)
summary(modelC)$dispersion
round(ci.lin(modelC, Exp = T), 3)

#### Model D with Negative Binomial Regression Modeling ####
modelD <-
  glm.nb(
    hosp_resp ~ offset(log(offset)) + year + as.Date(date) + month + day + weekday + yearday + prevreh + diffreh + cons_resp,
    data15a64[data15a64$tx == 0 & data15a64$txtime == 1, ]
  )
summary(modelD)
summary(modelD)$dispersion
round(ci.lin(modelD, Exp = T), 3)

### Autocorrelation Testing ###
resD <- residuals(modelD,type="deviance")
plot(rep(1:87, 4),resD,pch=19,cex=0.7,col=grey(0.6),
     main="Residuals over time",ylab="Deviance residuals",xlab="Date")
abline(h=0,lty=2,lwd=2)
par(mfrow = c(2,1))
acf(resD)
pacf(resD)
checkresiduals(modelD)
par(mfrow = c(1,1))

## Predict values based on Quasi-Poisson ##
data15a64$pred_RH_post <- predict(modelD, type = "response", data15a64)

ggplot(data15a64, aes(x = yearday, y = pred_RH_post)) +
  geom_line(col = "blue") +
  geom_point(aes(y = hosp_resp), cex = 0.75) +
  geom_smooth(aes(y = hosp_resp), cex = 0.75, span = 0.1) +
  facet_wrap(~year, nrow = 2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Day of Year") +
  ylab("Cases") +
  xlim(280, 365) +
  ggtitle("Respiratory Hospitalizations with Negative Binomial Regression Model (15-64)")

########## Write New Data File ##########
write_csv(data15a64, "pred_15-64_3km.csv")
