
```{r setting_prev_a_ts_set, echo=T, cache= T, paged.print=TRUE, warning=F, eval=T}
#number of times that data was collected per year by using the frequency parameter in the ts( )

tsData1 <- ts(data15a64_rn$hosp_trauma, frequency=1) #a cada unidad temporal le asigno una observación, por lo que asume que la unidad temporal considerada es el día
tsData4 <- ts(data15a64_rn$hosp_trauma, frequency=4) #a cada unidad temporal le asigno una observación, por lo que asume que la unidad temporal considerada es el día
tsData52.4 <- ts(data15a64_rn$hosp_trauma, frequency=52.4)#a cada unidad temporal le asigno una observación, por lo que asume que la unidad temporal considerada es la semana (262) #53+53+52+52+52
tsData17.5 <- ts(data15a64_rn$hosp_trauma, frequency=17.5)#a cada unidad temporal le asigno una observación, por lo que asume que la unidad temporal considerada es la semana (262) #53+53+52+52+52
tsData_m <-msts(data15a64_rn$hosp_trauma, seasonal.periods=c(4,52.5)) #a cada unidad temporal le asigno una observación, por lo que asume que la unidad temporal considerada es el día
#tsData48 <- ts(data15a64_rn$hosp_trauma, frequency=48)
#tsData262 <- ts(data15a64_rn$hosp_trauma, frequency=data15a64%>%dplyr::mutate(concat=paste0(year,"_",yearweek))%>%distinct(concat)%>% nrow())

gg0 <- list()
gg0[1] <- "tsData4"
gg0[2] <- "tsData17.5"
gg0[3] <- "tsData52.4"
invisible(c("frequency :the number of observations per “cycle” (normally a year, but sometimes a week, a day or an hour). "))
invisible(c("The “frequency” is the number of observations before the seasonal pattern repeats."))
```

## Explore Decomposition {.tabset .tabset-fade}

<br>
  
  We explored several additive and multiplicative decomposition of the data, depending on the number of observations assigned to each time point. We decomposed the time series in three alternatives: one assigning 4 obs. to each temporal unity (as Monthly Series), a second assigning 17.5 obs. (as Quarterly Series) to each temporal unity, and another assumed 52.4 obs. to each temporal unity (as Yearly Series).

<br>
  
  ```{r setting_prev_a_decomp, echo=T, cache= T, paged.print=TRUE, warning=F, eval=T,results='asis'}
#https://otexts.com/fpp2/decomposition.html
headings <- c('=4obs.', '=17.5obs.', '=52.4obs.','=Multiple(4 & 52.4)')

for (i in 1:length(gg0)) {
  cat("### ",headings[i],"\n")
  plot(decompose(get(gg0[[i]])))
  plot(decompose(get(gg0[[i]]), type="multiplicative"))
  cat('\n\n')
}

#https://pkg.robjhyndman.com/forecast/reference/mstl.html
cat("### ",headings[4],"\n")
mstl(tsData_m, lambda = NULL) %>% autoplot()+
  theme_bw()

#Aditivo: Seasonal+Trend+Random
#Multiplicativo:Seasonal*Trend*Random
#El multiplicativo asume que la interacción no es constante ni los componentes son consistentes
#
## Observed: los datos actuales
## Trend: el movimiento general hacia arriba o hacia abajo de los puntos de datos
## Seasonal: cualquier patrón mensual / anual de los puntos de los datos
## Random – parte inexplicable de los datos

#Determinar
## Tendencia: a largo plazo
## Estacional/Periodico: cuando la serie está influenciada por un periodo fijo y conocido (dia mes o semana)
## Cíclico= cuando hay subidas y caidas que no corresponden a un periodo fio
```

<br>
  
  ## Correlograms
  
  <br>
  
  The visual inspection of the spectral characteristics of the series permitted us to appreciate an important upward trend, and a seasonality determined by yearly combined with weekly cycles.

<br>
  
  ```{r setting_prev_b_ACF, echo=T, cache= T, paged.print=TRUE, warning=F, eval=T, fig.align='center', fig.cap= "Figure 2. Correlograms of Series"}
#acf() calcula la función de autocorrelación simple de una serie temporal, y pacf() la función de autocorrelación parcial. En ambos casos, por defecto se muestra el gráfico con bandas de confianza al 95%.
ggAcf(data15a64_rn$hosp_trauma,20,main="3.a.Autocorreation Plot of the Series of Trauma Hospitalizations")+ #data15a64_rn$hosp_trauma
  theme_sjplot()+
  theme(plot.caption=element_text(hjust = 0))+
  labs(x="Number of Lags",y="ACF",caption="Note. Dotted Blue Line= 95% CI")
lags_acf<-ggAcf(tsData1,20)$data %>% dplyr::arrange(desc(abs(Freq)))%>% slice(1:10)%>% dplyr::select(lag)
ggPacf(data15a64_rn$hosp_trauma,20,main="3.b.Partial Autocorreation Plot of the Series of Trauma Hospitalizations")+ #data15a64_rn$hosp_trauma
  theme_sjplot()+
  theme(plot.caption=element_text(hjust = 0))+
  labs(x="Number of Lags",y="ACF",caption="Note. Dotted Blue Line= 95% CI")
lags_pacf<-ggPacf(tsData1,20)$data %>% dplyr::arrange(desc(abs(Freq)))%>% slice(1:10)%>% dplyr::select(lag)

#tseries::adf.test(tsData)
#Augmented Dickey-Fuller Test gives a p-value of 0.01, so we have enough evidence to reject null hypothesis of non-stationarity.

#The test statistic is much bigger than the 1% critical value, indicating that the null hypothesis is rejected. That is, the data are not stationary. We can difference the data, and apply the test again.


#Se requiere para que las estimaciones de los parámetros sean útiles. De otra forma, no se podrían calcular medias y variancias conforme la serie va creciendo

#LOS MODELOS ARCH Y GARCH NO REQUIEREN CONSISTENCIA EN LA VARIANZA
#eing able to control the lags in our test, allows us to avoid a stationarity test that is too complex to be supported by our data.

#https://nwfsc-timeseries.github.io/atsa-labs/sec-boxjenkins-aug-dickey-fuller.html

#todos los parámetros son sig., por lo que son eestacionarios
```

<br>
  
  As seen in the correlograms and in the multiple decompositions, the series contains an upward trend, leading to think that the mean was not constant over time, and there could be evidence of seasonallity. This could be interpreted as that the series were not stationary. However, the Dickey-Fuller test indicated the presence of stationarity (`r round(tseries::adf.test(tsData1)$statistic,2)`, p\<`r round(tseries::adf.test(tsData1)$p.value,3)`).

<br>
  
  
  
  
  
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
  #:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
  #:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
  #:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
  #:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
  #:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:

  

## Time-series models & Back-testing approach

<br>
  
  We decided to automatically select the model with the better adjustment to the pre-intervention periods.

<br>
  
  ```{r setting_prev_c_compare_models, echo=T, cache= T, paged.print=TRUE, warning=F, eval=T, fig.align='center', fig.cap= "Figure 3. Comparison of Forecasting Methods, in Mean Square Error (MSE)"}
fets <- function(x, h) {
  forecast(ets(x), h = h)
}
farima <- function(x, h) {
  forecast(auto.arima(x), h=h)
}
fsnaive <- function(x, h) {
  forecast(snaive(x), h=h)
}

# Compute CV errors for ETS as e1
e1 <- tsCV(data15a64_rn[which(data15a64_rn$did==0),"hosp_trauma"], fets, h=rn_summ_data15a64$diff)
# Compute CV errors for ARIMA as e2
e2 <- tsCV(data15a64_rn[which(data15a64_rn$did==0),"hosp_trauma"], farima, h=rn_summ_data15a64$diff)

e3 <- tsCV(data15a64_rn[which(data15a64_rn$did==0),"hosp_trauma"], naive, h=rn_summ_data15a64$diff)

e4 <- tsCV(data15a64_rn[which(data15a64_rn$did==0),"hosp_trauma"], ses, h=rn_summ_data15a64$diff)

e5 <- tsCV(data15a64_rn[which(data15a64_rn$did==0),"hosp_trauma"], holt, h=rn_summ_data15a64$diff)

e6 <- tsCV(data15a64_rn[which(data15a64_rn$did==0),"hosp_trauma"], holt, seasonal="additive", damped=T, h=rn_summ_data15a64$diff)

mse1 <- colMeans(e1^2, na.rm = TRUE)
mse2 <- colMeans(e2^2, na.rm = TRUE)
mse3 <- colMeans(e3^2, na.rm = TRUE)
mse4 <- colMeans(e4^2, na.rm = TRUE)
mse5 <- colMeans(e5^2, na.rm = TRUE)
mse6 <- colMeans(e6^2, na.rm = TRUE)

h<- 1:rn_summ_data15a64$diff
mse_comb<- data.frame(cbind(h,mse1,mse2,mse3,mse4,mse5,mse6))

mse_comb%>% 
  melt(id=1)%>%
  ggplot(aes(x = h, y = value,shape=variable, color=variable))+
  geom_point(size=4)+
  #geom_line(aes(linetype=variable))+
  theme_sjplot()+
  labs(x="Forecast Horizon",y="MSE",caption=paste0("Note. "),color  ="Forecasting\nMethods", shape = "Forecasting\nMethods")+   
  scale_color_manual(values=c("#999999", "midnightblue","#E69F00", "darkred","salmon","darkslategrey"), 
                     name="Forecasting\nMethods",
                     #breaks=c("ctrl", "trt1", "trt2"),
                     labels=c("ETS", "ARIMA", "Naive","SES","Holt","HW-add","HW-mult"))+
  scale_shape_manual(values=c(0:6)+15,name="Forecasting\nMethods")+
  guides(colour = guide_legend(override.aes = list(shape = c(0:5)+15, color = c("#999999", "midnightblue","#E69F00", "darkred","salmon","darkslategrey"))),shape="none")+
  scale_x_continuous(limits=c(1,rn_summ_data15a64$diff),breaks=seq(1,rn_summ_data15a64$diff,by=1))

#ARIMA models
###some are stationary
###do not have exponential smoothing counterparts
###use if you see autocorrelation in the data, i.e. the past data explains the present data well

#ETS models
#ETS(A, N, N): Simple exponential smoothing with additive errors 'A'/'M' stands for whether you add the errors on or multiply the errors on the point forecsats
###are not stationary
###use exponential smoothing
###use if there is a trend and/or seasonality in the data, as this model explicitly models these components
##Automatically chooses a model by default
##Can handle any combination of trend, seasonality and damping
##Produces prediction intervals for every model
##Ensures the parameters are admissible (equivalent to invertible)
##Produces an object of class ets

ets1<-ets(tsData1)
#round(ets1$aicc,0)
#Myth that ARIMA models are more general than exponential smoothing.
#Linear exponential smoothing models all special cases of ARIMA models.
#Non-linear exponential smoothing models have no equivalent ARIMA counterparts.
#Many ARIMA models have no exponential smoothing counterparts.
#ETS models all non-stationary. Models with seasonality or non-damped trend (or both) have two unit roots; all other models have one unit root.

#your data is integer, then applying ARIMA models might not be appropriate
```

<br>
  
  Exponential smoothing and ARIMA models are the two most widely-used approaches to time series forecasting, and provide complementary approaches to the problem

<br>
  
  We computed the cross-validation errors of different methods, using data from the pre-intervention period. The first method, the Naive (`r round(mean(e3^2, na.rm=TRUE),2)`) had the greatest errors, while exponential smoothing methods (Error Trend and Seasonality, ETS) -that identified multiplicative errors but no trend nor seasonallity (`r as.character(ets1$method)`)- shown greater average error (MSE=`r round(mean(e1^2, na.rm=TRUE),2)`), than the SES (Simple Exponential Smoothing) (`r round(mean(e4^2, na.rm=TRUE),2)`) and ARIMA methods (MSE= `r round(mean(e2^2, na.rm=TRUE),2)`). This is why we proceeded with the ARIMA models.

<br>
  
  Since we lack of experience fitting time series models manually, the optimal model selected is the one with the better adjusted indices.

<br>
  
  ```{r setting_prev_d, echo=T, cache= T, paged.print=TRUE, warning=F, eval=T}
#x1<-as.numeric(ts_data15a64_rn[,"offset"])
#Decisión de ocupar automáticos: https://robjhyndman.com/talks/MelbourneRUG.pdf

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

# Autocorrelación= p d q para modelos ARIMA
# Análisis espectral= comportamiento cíclico, separando componentes ruidosos
# Estimación y descomposición= ajuste estacional, serie de c componentes con características

#p= 	order of the autoregressive part;
#d=  	degree of first differencing involved;
#q= 	order of the moving average part.

tsData <- ts(data15a64_rn$hosp_trauma, frequency=1) #a cada unidad temporal le asigno una observación, por lo que asume que la unidad temporal considerada es el día

#https://nbviewer.jupyter.org/github/dafiti/causalimpact/blob/master/examples/getting_started.ipynb
#https://stackoverflow.com/questions/30303680/how-to-impose-restrictions-on-predictions-made-using-a-bayesian-structural-time

#ets(zoo(data.frame(data15a64_rn[,"hosp_trauma"])))
ARIMA_fit<-auto.arima(data.frame(data15a64_rn[1:post.period[1],"hosp_trauma"]), trace=TRUE,stepwise = F)#, method='CSS')
```

<br>
  
  The selected model with the best indices was the ARIMA(1, 1, 2), which means that it you are describing some response variable (Y) by combining a 1st order Auto-Regressive model and a 2nd order Moving Average model. The 'I' (differencing) part of the model (the Integrative part) it signifies a model where we were not taking the difference between response variable data: non-stationary data of 1 (MA moving average) (AICc=`r round(ARIMA_fit$aicc)`;BIC= `r round(ARIMA_fit$bic,2)`).

<br>
  
  ```{r setting_prev_d_plot_fitted_vs_actual, echo=T, cache= T, paged.print=TRUE, warning=F, eval=T, fig.align='center', fig.cap= "Figure 5. Actual linear trend vs. Fitted Line (ARIMA 1,1,2)",fig.height=12}
#will be to fit a model to the data, and then use the forecast() function to produce forecasts from that model
data_forecast_arima112<-forecast(ARIMA_fit, h=rn_summ_data15a64$diff,bootstrap=5000)
#ts(data15a64_rn[1:post.period[1],"hosp_trauma"]) %>% ets() %>% forecast(h=rn_summ_data15a64$diff,bootstrap=5000) %>% autoplot()

fitted_and_forecast_arima<-c(as.numeric(data_forecast_arima112$fitted),as.numeric(data_forecast_arima112$mean))
actual_vs_fitted_and_forecast_arima<-cbind(actual=data15a64_rn[,c("date","did","rn","hosp_trauma")],
                                           arima=data.frame(fitted_and_forecast_arima),
                                           lo_80=c(rep(NA,post.period[1]),as.numeric(data_forecast_arima112$lower[,1])),
                                           lo_95=c(rep(NA,post.period[1]),as.numeric(data_forecast_arima112$lower[,2])),
                                           up_80=c(rep(NA,post.period[1]),as.numeric(data_forecast_arima112$upper[,1])),
                                           up_95=c(rep(NA,post.period[1]),as.numeric(data_forecast_arima112$upper[,2])))

### MAPE (mean absolute percentage error)
MAPE_prev <- actual_vs_fitted_and_forecast_arima%>%
  #dplyr::mutate(actual.date=as.Date(actual.year_week))%>%
  dplyr::filter(actual.hosp_trauma!=0)%>% #had 0 2019-08-06 
  dplyr::filter(actual.did==0)%>%
  summarise(MAPE=mean(abs(actual.hosp_trauma-fitted_and_forecast_arima)/actual.hosp_trauma,na.rm=T))

MAPE_post <- actual_vs_fitted_and_forecast_arima%>%
  dplyr::filter(actual.did==1)%>%
  summarise(MAPE=mean(abs(actual.hosp_trauma-fitted_and_forecast_arima)/actual.hosp_trauma))

post_did<-actual_vs_fitted_and_forecast_arima[which(actual_vs_fitted_and_forecast_arima$actual.did ==1),]

lenfinal<-NULL
for (i in 15:19){lenfinal<-c(lenfinal,paste0("Q",1:4,"\n",i))}

actual_vs_fitted_and_forecast_arima%>%
  dplyr::mutate(actual.date=as.Date(actual.date))%>%
  ggplot()+
  geom_line(aes(x=actual.rn, y=actual.hosp_trauma), color="grey30")+
  geom_line(aes(x=actual.rn, y=fitted_and_forecast_arima), color="red", size=1)+
  geom_vline(xintercept =253, linetype="dotted", color = "blue", size=1.5)+
  geom_ribbon(aes(x= actual.rn, ymin = lo_80, ymax = up_80), fill = "brown", alpha = .6)+
  geom_ribbon(aes(x= actual.rn, ymin = lo_95, ymax = up_95), fill = "red", alpha = .3)+
  theme_sjplot()+
  theme(plot.caption=element_text(hjust = 0))+
  labs(x="Date",y="Trauma Hospitalizations",caption=paste0("Note. Red line= Fitted line; Gray lines= Actual Trauma Hospitalizations;\nVertical line= 2020-10-18;\n",paste0("MAPE  in post-intervention period=",round(100*MAPE_post,1),"%")))+
  xlab("")+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(size=10))+
  scale_x_continuous(
    breaks = seq(from = 1, to = nrow(actual_vs_fitted_and_forecast_arima), by =12),
    label = actual_vs_fitted_and_forecast_arima[which(actual_vs_fitted_and_forecast_arima$actual.rn %in% seq(from = 1, to = nrow(actual_vs_fitted_and_forecast_arima), by =12)),"actual.date"])+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))+
  facet_zoom(xlim = c(253, nrow(actual_vs_fitted_and_forecast_arima)))

#ggforce::facet_zoom(x = actual.date>=as.Date("2019-10-18") & actual.date<as.Date("2019-12-31"))
```

<br>
  
  As seen in the Figure, the model seemed to fit well, however it shows a MAPE (mean absolute percentage error) of `r paste0(round(100*MAPE_prev,1),"%")` in the pre-intervention period.

<br>
  
  ### Characteristics of the ARIMA model selected
  
  We checked the ARIMA model, residuals and autocorrelations of the model. The following plot shows a general overview of the characteristics of the model.

<br>
  
  ```{r setting_prev_e_check_resid1_general, echo=T, cache= T, paged.print=TRUE, warning=F, eval=T, fig.align='center', fig.cap= "Figure 6. Residuals of the Model"}
checkresiduals(ARIMA_fit)
#ARIMA(2,1,2); #ARIMA(1,1,2); #ARIMA(2,1,2); # ARIMA(2,1,3)
```

<br>
  
  The selected model was the most parsimonious one among the possible candidates using `auto.arima` function, and the mean seem to be close to 0 (`r paste0("M= ",round(mean(ARIMA_fit$residuals),3))`), but we could observe that errors may not be distributed independently `r paste0("(W=",as.numeric(round(shapiro.test(ARIMA_fit$residuals)$statistic,3)),"; p.value=",as.numeric(round(shapiro.test(ARIMA_fit$residuals)$p.value,3)),")")`. However, residuals were not significantly different from white noise series (`r paste0("Chi-square ","(",Box.test(ARIMA_fit$residuals,lag=10, fitdf=0, type="Lj")$df,")=",round(Box.test(ARIMA_fit$residuals,lag=10, fitdf=0, type="Lj")$statistic,2)," p=",round(Box.test(ARIMA_fit$residuals,lag=10, fitdf=0, type="Lj")$p.value,3))`).

Below we may see a detailed figure of autocorrelation and partial autocorrelation plots.

<br>
  
  ```{r setting_prev_e_check_resid2, echo=T, cache= T, paged.print=TRUE, warning=F, eval=T, fig.align='center', fig.cap= "Figure 7. Correlograms of Residuals"}
invisible(c("IGUAL LOS RESIDUOS NO SE DISTIRBUYEN NORMALMENTE!!!!!!!!!!!!!!!!!!!!!!!!!"))

#One simple "sanity check" would be to find series where your forecast is higher than the highest historical observation, or higher than the historical 90% quantile. This often signals trouble, except possibly for very new products that are ramping up.
#Lag 6, 7 y 18 tienen spikes. 
# El residuo no es ruido blanco

#acf() calcula la función de autocorrelación simple de una serie temporal, y pacf() la función de autocorrelación parcial. En ambos casos, por defecto se muestra el gráfico con bandas de confianza al 95%.
ggAcf(ARIMA_fit$residuals,20,main="6.a.Autocorreation Plot of the residuals of the ARIMA model")+ #data15a64_rn$hosp_trauma
  theme_sjplot()+
  theme(plot.caption=element_text(hjust = 0))+
  labs(x="Number of Lags",y="ACF",caption="Note. Dotted Blue Line= 95% CI")+
  ylim(-.25,.25)
lags_acf<-ggAcf(ARIMA_fit$residuals,400)$data %>% dplyr::arrange(desc(abs(Freq)))%>% slice(1:10)%>% dplyr::select(lag)
ggPacf(ARIMA_fit$residuals,20,main="6.b.Partial Autocorreation Plot of the residuals of the ARIMA model")+ #data15a64_rn$hosp_trauma
  theme_sjplot()+
  theme(plot.caption=element_text(hjust = 0))+
  labs(x="Number of Lags",y="ACF",caption="Note. Dotted Blue Line= 95% CI")+
  ylim(-.25,.25)
lags_pacf<-ggPacf(ARIMA_fit$residuals,400)$data %>% dplyr::arrange(desc(abs(Freq)))%>% slice(1:10)%>% dplyr::select(lag)
# ggforce::facet_zoom(x = lag >=5 & lag <=10)+
# ggforce::facet_zoom(x = lag >=20 & lag <=25)
```

<br>
  
  As shown in the autoccorelation plots, the selected model may still be correlated with some lagged values. Particularly, there were some significant correlations in the residuals series, such as with `r lags_acf[1,1]`,`r  lags_acf[2,1]`,`r lags_acf[3,1]`,`r lags_acf[4,1]`,`r lags_acf[5,1]`,`r lags_acf[6,1]`,`r lags_acf[7,1]`,`r lags_acf[8,1]`,`r lags_acf[9,1]`, and `r lags_acf[10,1]`. Focusing on indirect effects, the partial autocorrelation function also identifies autocorrelations that were greater than the correlation bar.

<br>
  
  ```{r setting_prev_f_resid_vs_fit, echo=T, cache= T, paged.print=TRUE, warning=F, eval=T, fig.align='center', fig.cap= "Figure 8. Scatter-plot of Residuals vs. Fitted ARIMA model"}
#plot(,,type="p",pch=9, col="blue",xlab="Fitted",ylab="Resid.")
#abline(h=0,lty=2,lwd=2)

cbind(Fitted = ARIMA_fit$fitted,
      Residuals=ARIMA_fit$residuals) %>%
  as.data.frame() %>%
  ggplot(aes(x=Fitted, y=Residuals)) + 
  geom_point()+
  theme_sjplot()+
  #ylim(-20,20)+
  #xlim(0,20)+
  geom_hline(aes(yintercept = 0),linetype = 2)

#tsData %>% urca::ur.kpss() %>% summary()
```

<br>
  
  As shown in the Figure above, no pattern was found between residuals and predicted values, excepting for some outliers.

<br>
  
  ```{r setting_prev_g_tbats_multiple_time_seasonality, echo=T, cache= T, paged.print=TRUE, warning=F, eval=F, fig.cap="Figure 3. NO LO OCUPO"}

tsData52.4%>% tbats()
#TBATS(1, {1,1}, -, {<52.4,6>})= el primer 1 significa que no es necesario hace trasformación box-cox; los 1 y 1 significa que hay un error ARMA de 1 y 1; no hay amortiguación/damping--> lo final dice de los términos fourier; se seleccionaron 6
#no se ven muchos retrasos

tsData_m%>% tbats()%>% forecast()%>% autoplot()
```

<br>
  
  
  
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
  
# Interrupted Time Series Analysis
  
  We estimated the difference in post treatment and pre-treatment periods while controlling for covariates and a lagged depentent variable through a Type-2 Sum Squares ANCOVA Lagged Dependent Variable model. The model also accounts for baseline levels and trends present in the data, allowing us to attribute significant changes to the interruption. The F-statistics were calculated using a bootstrap model of `r print(clus_iter/2)` replications.

<br>
  
  ::: {style="border: 1px solid #ddd; padding: 5px; overflow-y: scroll; height:350px; overflow-x: scroll; width:100%"}
```{r itsa1, echo=T, cache= T, paged.print=TRUE, warning=F, eval=T, fig.show="hide"}
#tsData_gastos$trend
#Using the inputted variables, a Type-2 Sum Squares ANCOVA Lagged Dependent Variable model is fitted which estimates the difference in means between interrupted and non-interrupted time periods, while accounting for the lag of the dependent variable and any further specified covariates.
#Typically such analyses use Auto-regressive Integrated Moving Average (ARIMA) models to handle the serial dependence of the residuals of a linear model, which is estimated either as part of the ARIMA process or through a standard linear regression modeling process [9,17]. All such time series methods enable the effect of the event to be separated from general trends and serial dependencies in time, thereby enabling valid statistical inferences to be made about whether an intervention has had an effect on a time series.
#it uses Type-2 Sum Squares ANCOVA Lagged Dependent Variable model
#ITSA model da cuenta de observaciones autocorrelacionadas e impactos dinámicos mediante una regresión de deltas en rezagados. Una vez que se incorporan en el modelo, se controlan. 
#residual autocorrelation assumptions
#TSA allows the model to account for baseline levels and trends present in the data therefore allowing us to attribute significant changes to the interruption
#RDestimate(all~agecell,data=metro_region,cutpoint = 21)

#Ver con CHANGEPOINT
##http://www.lancs.ac.uk/~killick/Pub/KillickEckley2011.pdf
data15a64_rn2_its<-
  data15a64_rn2 %>% 
  dplyr::mutate(did=factor(did))

itsa_metro_region_quar<-
  its.analysis::itsa.model(time = "rn", depvar = "hosp_resp",data=data15a64_rn2,
                           interrupt_var = "did", covariates = c("hosp_circ", "hosp_trauma","diffrec"), 
                           alpha = 0.05,no.plots = FALSE, bootstrap = TRUE, Reps = clus_iter/2, print = TRUE) 

print(itsa_metro_region_quar)

#This function runs and reports post-estimation tests on fits from the itsa.model function, and generates four plots.
#Main tests are whether two key ANCOVA assumptions are met, and an additional autocorrelation test for the time series framework.
#The Shaprio-Wilks test examines the residuals from the fitted model for abnormality. A p-value less than alpha indicates abnormal residuals.
#The Levene’s Test makes sure that there are equal variances between the treated groups. A p-value less than alpha indicates heterogeneous variances.
#A QQ-Norm and Boxplot are generated with the test results overlaid (respectively), with a Residualv Fitted and Autocorrelation Function Plot also generated.
#The results of bootstrap estimations in itsa.model will be plotted, unless argument is switched to FALSE.
#Default is to generate plots and summary table, but plots may be overridden using no.plots argument.
#Default alpha value for post-estimation statistics is 0.05, test results will suggest potential presence of problems at higher values (and also at higher levels relative to a user-inputted alpha), but user discretion is needed (examined in tandam with the Residuals v Fitted plot).
```
:::
  
  <br>
  
  ```{r itsa_plot,echo=T,fig.align='center', fig.pos='H', eval=T, message=FALSE, warning=F, fig.align='center', fig.cap= "Figure 12. Interrupted Time-Series Analysis of Respiratory Hospitalizations in Inervention Period (2019-10-18)", error=T}
ggplot_itsa<-itsa_metro_region_quar$itsa.plot

print(ggplot_itsa)
```

<br>
  
  There were non-significant differences in the average respiratory hospitalizations (F=`r round(itsa_metro_region_quar$booted.ints$'Mean F-value',2)[1]`, p=`r round(itsa_metro_region_quar$booted.ints$'P-value',3)[1]`). The model explained `r scales::percent(itsa_metro_region_quar$adjr_sq)` of the variances. Must note that there were no autocorrelation evidence, but there were normal residuals (p= `r itsa_metro_region_quar$shapiro.test`). Tukey HSD test showed non-significant differences `r paste0("(diff=",round(itsa_metro_region_quar$tukey.result$'x$interrupt_var'[1],2),",p=",sprintf("%4.4f",itsa_metro_region_quar$tukey.result$'x$interrupt_var'[4]),")")` between pre-period (`r paste0("M=",sprintf("%4.2f",itsa_metro_region_quar$group.means[1,3]),", SD=",sprintf("%4.2f",itsa_metro_region_quar$group.means[1,4]))`) and post-intervention period (`r paste0("M=",sprintf("%4.2f",itsa_metro_region_quar$group.means[2,3]),", SD=",sprintf("%4.2f",itsa_metro_region_quar$group.means[2,4]))`).

<br>
  
  # Synthetic Control
  
  Next, we compared the data of respiratory hospitalizations in 2019, to the hospitalizations of years from 2015 to 2018, estimating a counterfactual based on the weeks previous to the event (from week 1 to 42).

<br>
  
  ```{r exp_plot_lines_log_synth, echo=T, cache= T, paged.print=TRUE, warning=F, fig.height=14,eval=T, fig.align='center', fig.cap= "Figure 13. Linear trends of Weekly Respiratory Hospitalizations by Years (Median and IQRs)"}
ggplotly_series<-
  data15a64_rn2%>%
  # dplyr::group_by(year,yearweek)%>%
  #dplyr::summarise(median=median(hosp_trauma,na.rm=T))%>%
  ggplot() + #median
  #geom_point() + 
  #geom_line() +
  #geom_jitter(aes(y = hosp_trauma,x = isoweek, color = year),width = 0.5, alpha = 0.3)+
  facet_wrap(~year, ncol = 1,strip.position="right") + 
  theme_bw() + 
  theme(strip.background  = element_blank(),
        strip.text = element_text(face="bold", size=7))+
  theme_bw() + 
  ylab("Counts") + 
  xlab("Week Number") + 
  theme(strip.text.x = element_text(size = 8, face = "bold"),
        axis.text.y = element_blank(),
        legend.position = "none",
        plot.caption=element_text(hjust = 0),
        strip.background  = element_blank())+
  geom_smooth(aes(y = hosp_trauma,x = isoweek, color = year),stat = 'summary', color = 'red', fill = 'red', alpha = 0.2, 
              fun.data = median_hilow, fun.args = list(conf.int = 0.5))+
  geom_smooth(aes(y = hosp_circ,x = isoweek, color = year),stat = 'summary', color = 'darkblue', fill = 'navyblue', alpha = 0.2, 
              fun.data = median_hilow, fun.args = list(conf.int = 0.5))+
  geom_smooth(aes(y = hosp_resp,x = isoweek, color = year),stat = 'summary', color = 'violet', fill = 'violet', alpha = 0.2, 
              fun.data = median_hilow, fun.args = list(conf.int = 0.5))+
  geom_smooth(aes(y = diffrec,x = isoweek, color = year),stat = 'summary', color = 'darkgreen', fill = 'green', alpha = 0.2, 
              fun.data = median_hilow, fun.args = list(conf.int = 0.5))+
  #  geom_smooth(aes(y = hosp_total,x = yearweek, color = year),stat = 'summary', color = 'gray80', fill = 'gray80', alpha = 0.2, 
  #             fun.data = median_hilow, fun.args = list(conf.int = 0.5))+
  labs(caption="Note. Year 2015 & 2016 had a length of 53 weeks.Red= Trauma Hospitalizations; Blue= Circulatory System Hospitalizations;\nViolet= Respiratory System Hospitalizations; Green= Diff. Between Respiratory and Total Consultations")+
  xlab("Week")+
  scale_x_continuous(
    breaks = seq(from = 1, to = 53, by =4)#,
    #  label = c("two", "four", "six")
  )
ggplotly(ggplotly_series)%>% 
  layout(annotations = 
           list(x = .5, y = -0.04, text = "Note. Year 2015 & 2016 had a length of 53 weeks.Red= Trauma Hospitalizations; Blue= Circulatory System Hospitalizations;\n Violet= Respiratory System Hospitalizations; Green= Diff. Between Respiratory and Total Consultations", 
                showarrow = F, xref='paper', yref='paper', 
                #xanchor='center', yanchor='auto', xshift=0, yshift=-0,
                font=list(size=7, color="darkblue"))
  )
```

<br>
  
  We observed that linear trends shows very different behaviors. However, circulatory hospitalizations seems to show resemblances in the trends with respiratory hospitalizations.

<br>
  
  ```{r exp_plot_lines_log_synth_std_diff, echo=T, cache= T, paged.print=TRUE, warning=F, fig.height=14,eval=T, fig.align='center', fig.cap= "Figure 14. Linear trends of Std. Differences of Variables, Between 2015-2018 vs. 2019, Before Week 43"}
ggplotly_trends<-data15a64_rn2%>%
  dplyr::filter(isoweek<43) %>%
  #escalar los datos por todos l
  dplyr::group_by(isoweek) %>% 
  dplyr::mutate_at(.vars=vars(hosp_trauma,hosp_circ, hosp_resp, diffrec), 
                   .funs = funs(`zw`=scale(.)))%>%
  dplyr::select(year,hosp_trauma_zw,hosp_circ_zw, hosp_resp_zw, diffrec_zw) %>% 
  dplyr::ungroup()%>%
  dplyr::mutate(treat.var=ifelse(year==2019,1,0))%>%
  dplyr::group_by(treat.var,isoweek)%>%
  dplyr::summarise_at(.vars=vars(hosp_trauma_zw,hosp_circ_zw, hosp_resp_zw, diffrec_zw),
                      .funs = funs(mean = mean(.,na.rm=T)))%>%
  dplyr::ungroup()%>%
  tidyr::pivot_longer(cols = ends_with("_mean"),names_to="variable", values_to="value",values_drop_na = F)%>%
  dplyr::group_by(isoweek, variable)%>%
  dplyr::mutate(difference = max(value) - min(value))%>%
  dplyr::filter(treat.var==0)%>%
  # dplyr::mutate(sig=if_else(variable %in% c("T21_AnyOpioid_zw_mean", "T21_PercRecHighDosage_zw_mean", "T57_Total_zw_mean"),0,1,0))%>%
  #  dplyr::mutate(sig=factor(sig))%>%
  dplyr::ungroup()%>%
  dplyr::rename("Outcome"="variable")%>%
  ggplot(aes(x=isoweek,y=difference, color=Outcome))+
  geom_line(size=1)+
  geom_point(size=2) +
  labs(y = "Std. Differences", x = "Week Number")+ 
  sjPlot::theme_sjplot2() +
  #scale_y_continuous(labels = scales::percent,limits=c(0, .999))+
  #scale_color_manual(values=brewer.pal(n = 7, name = "Moonrise2"))+ #"Pastel1"
  scale_color_manual(values=RColorBrewer::brewer.pal(n=4,name="Pastel2"))+ #"Pastel1"
  #  scale_fill_manual(values = wes_palette(n=7, name="Darjeeling2"))+
  #facet_grid(sig~., labeller= to_string, switch = "y")+
  theme(legend.title = element_text(size= 10.5, colour = "black", family="Arial"), legend.position='bottom', 
        legend.text=element_text(size=10, family="Arial"))+
  theme(plot.caption = element_text(hjust = 0, face = "italic", family="Arial"))+
  scale_x_continuous("Quarters & Years",breaks=seq(1,42,by=4))+
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(strip.background=element_rect(fill="white"),
        strip.text.y = element_text(face="bold", family="Arial"))+
  guides(color = guide_legend(ncol = 2, nrow = 4, byrow = TRUE))
ggplotly(ggplotly_trends) %>% 
  layout(legend = list(
    orientation = "h", y = -0.2
  )
  )
```

<br>
  
  Considering the differences shown in the figure above, we conserved all the covariates in order to generate a synthetic control.

<br>
  
  ```{r synth1, echo=T, cache= T, paged.print=TRUE, warning=F,eval=T}
#https://uclspp.github.io/PUBL0050/seminar6.html
library("Synth")
data15a64_rn3<-data15a64_rn2 %>% 
  dplyr::filter(isoweek!=53)
#data15a64_rn3[which(data15a64_rn3$isoweek==53),"did"]<-1
#table(data15a64_rn2$isoweek,data15a64_rn2$did)

dataprep.out <-
  dataprep(data15a64_rn3,
           predictors = c("hosp_circ", "hosp_trauma","diffrec"),
           dependent     = "hosp_resp",
           unit.variable = "year",
           time.variable = "isoweek",
           treatment.identifier  = 2019,
           special.predictors = list(
             list("hosp_resp", 1, "median"),
             list("hosp_resp", 11, "median"),
             list("hosp_resp", 21, "median"),
             list("hosp_resp", 41, "median")),
           predictors.op="median",
           time.predictors.prior = c(1:42),
           time.optimize.ssr= c(1:42),
           controls.identifier   = c(2015:2018),
           time.plot             = c(1:52))

# Run synth
synth.out <- synth(dataprep.out,method = "All")

synth2.out <- MSCMT::improveSynth(synth.out,dataprep.out,seed=2125)

print(synth.tables   <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth2.out)
)

#https://rpubs.com/danilofreire/synth
#https://towardsdatascience.com/causal-inference-using-synthetic-control-the-ultimate-guide-a622ad5cf827

synth.tables = synth.tab(dataprep.res = dataprep.out,
                         synth.res = synth2.out)
names(synth.tables)

synth.tables$tab.pred

#To calculate the difference between the real trauma hospitalizations in 2019 and the synthetic control as follows:
gaps = dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth2.out$solution.w)
#plot(gaps,type="l")
```

<br>
  
  ```{r synth2, echo=T, cache= T, paged.print=TRUE, warning=F,eval=T, fig.show="hide", fig.cap= "Figure 15. Coefficients of SCM"}
no_mostrar=1
if(no_mostrar==0){
  path.plot(synth.res    = synth2.out,
            dataprep.res = dataprep.out,
            Ylab         = c("Y"),
            Xlab         = c("Week of the Year"),
            Ylim         = c(3, 5),
            Legend       = c("2019","2015 to 2018"),
            Legend.position = c("topleft")
  )
  abline(v   = 43,
         lty = 2)
}

gaps.plot(synth.res = synth2.out, dataprep.res = dataprep.out, Ylab = "Respiratory Hospitalizations", Xlab= "Week Number")
```

<br>
  
  ```{r synth3, echo=T, cache= T, paged.print=TRUE, warning=F, eval=T, fig.align='center', fig.cap= "Figure 16. Comparison of Linear Trends of Respiratory Hospitalizations in 2019 vs. Synthetic Counterfactual Based on 2015-2018"}

plot.df = data.frame(dataprep.out$Y0plot%*%synth2.out$solution.w)

years <- as.numeric(row.names(plot.df))
plot.df = rbind(data.frame(year=years,y=plot.df$w.weight,unit='Synthetic'),
                data.frame(year=years,y=data15a64_rn3$hosp_resp[data15a64_rn3$year==2019 & data15a64_rn3$isoweek %in% years],unit='2019')
)
ggplot(plot.df,aes(x=year,y=y,color=unit)) + geom_line() + ylab("") + xlab("Week Number") + scale_color_manual(values=c('black','red')) +
  ggtitle('Respiratory Hospitalizations') +
  theme_sjplot2()+
  guides(color=guide_legend(title="Series"))+
  geom_vline(xintercept = 43,linetype="longdash")+
  labs(caption="Note. Vertical Line, Posterior Week to October 18th")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", family="Arial"))
```

<br>
  
  We compared several controls as a placebo test.

<br>
  
  ```{r synth4, echo=T, cache= T, paged.print=TRUE, warning=F,eval=T}
library(SCtools)
#https://cran.r-project.org/web/packages/SCtools/SCtools.pdf

placebos<-generate.placebos(
  dataprep.out,
  synth.out,
  Sigf.ipop = 5,
  strategy = "sequential"
)
mspe_test(placebos, discard.extreme = FALSE, mspe.limit = 20)

```

```{r synth5_plot_mspe_placebos, echo=T, cache= T, paged.print=TRUE, warning=F, eval=T, fig.align='center', fig.cap= "Figure 17. Ratio of MSPE of Placebos"}
mspe.plot(
  placebos,
  discard.extreme = FALSE,
  mspe.limit = 20,
  plot.hist = FALSE,
  title = NULL,
  xlab = "Post/Pre MSPE ratio",
  ylab = NULL
)
```

```{r synth6_plot_placebos, echo=T, cache= T, paged.print=TRUE, warning=F, eval=T, fig.align='center', fig.cap= "Figure 18. Comparison of Linear Trends of Placebos"}
plot_placebos(
  tdf = placebos,
  discard.extreme = FALSE,
  mspe.limit = 20,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  alpha.placebos = 1,
)
```

# Codebook Deployment

```{r prepare_codebook, echo=T, error=T,eval=T}

invisible(c("DataExp"))
#https://boxuancui.github.io/DataExplorer/
#DataExplorer::create_report(coronavirus_iso3c_8)

mostrar="no"
if(mostrar=="si"){
  ExPanDaR::ExPanD(df = codebook_data,
                   export_nb_option = F,  #para proteger la base de datos
                   title= "Exploration of Dataset", 
                   abstract= "Summarised information of the variables. ID: " ,
                   df_name= "", 
                   # df_def= df_def,
                   cs_id = "iso_code",
                   key_phrase = "ags", #proteger la bd
                   store_encrypted= T) 
}

# to import an SPSS file from the same folder uncomment and edit the line below
# codebook_data <- rio::import("mydata.sav")
# for Stata
# codebook_data <- rio::import("mydata.dta")
# for CSV
# codebook_data <- rio::import("mydata.csv")

# omit the following lines, if your missing values are already properly labelled
codebook_data <- detect_missing(data15a64_rn,
                                only_labelled = TRUE, # only labelled values are autodetected as
                                # missing
                                negative_values_are_missing = FALSE, # negative values are missing values
                                ninety_nine_problems = FALSE,   # 99/999 are missing values, if they
                                # are more than 5 MAD from the median
)

# If you are not using formr, the codebook package needs to guess which items
# form a scale. The following line finds item aggregates with names like this:
# scale = scale_1 + scale_2R + scale_3R
# identifying these aggregates allows the codebook function to
# automatically compute reliabilities.
# However, it will not reverse items automatically.
codebook_data <- detect_scales(codebook_data)

tryCatch(
  {
    save.image(paste0(getwd(),"/","Procesos hasta 5_hosp_resp.RData"))
    #rio::export(codebook_data, "C:/Users/andre/Dropbox/Covid-19_2020/Article_SecondManuscript/LT Environmental analysis/Databases/merged_data_post_ago.dta")
  },
  error = function(e){
    save.image(paste0(getwd(),"/","Procesos hasta 5_hosp_resp.RData"))
    #rio::export(codebook_data, "C:/Users/CISS Fondecyt/Dropbox/Covid-19_2020/Article_SecondManuscript/LT Environmental analysis/Databases/merged_data_post_ago.dta")
  }
)
sessionInfo()
```

```{r codebook_final, echo=T,error=T,eval=F}
codebook::codebook(codebook_data)
```  