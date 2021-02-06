set.seed(2125)
x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)
y <- rpois(100, 1.2 * x1)
y[71:100] <- y[71:100] + 10
data <- cbind(y, x1)
pre.period <- c(1, 70)
post.period <- c(71, 100)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddLocalLevel(list(), y)
bsts.model <- bsts(y ~ x1, ss, family="poisson", niter = 1000)


"cons_trauma ~ cons_diarrea + year + month + day + weekday + yearday + prevtrc"

impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)

install.packages("CausalImpact")
library(CausalImpact)
library(dplyr)
x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)
y <- 1.2 * x1 + rnorm(100)
y[71:100] <- y[71:100] + 10
data <- cbind(y, x1)


data15a64[data15a64$tx == 0 & data15a64$txtime == 0,]

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

rm(list=ls());gc()
load(paste0(getwd(),"/","Procesos hasta 4.RData"))


library(CausalImpact)
library(zoo)
stocks.zoo <- stocks[,c(1,3,2)] %>% 
  read.zoo()
times <- seq(start(stocks.zoo), end(stocks.zoo), by = 'day')
stocks.zoo <- merge(stocks.zoo, zoo(,times), all = TRUE) %>% na.locf()
impact <- CausalImpact(data = stocks.zoo, 
                       pre.period = c(first(stocks$date), oilSpillDate-1),
                       post.period = c(oilSpillDate, last(stocks$date)))
summary(impact)