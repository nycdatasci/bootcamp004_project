library(Quandl)
library(seasonal)
library(forecast)
library(ggplot2)
library(lubridate)
library(AnomalyDetection)
library(tseries)
#install.packages("devtools")
#devtools::install_github("twitter/AnomalyDetection")
#library(AnomalyDetection)

#http://www.r-bloggers.com/plotting-forecast-objects-in-ggplot-part-1-extracting-the-data/
funggcast<-function(dn,fcast){ 
   require(zoo) #needed for the 'as.yearmon()' function
   
   en<-max(time(fcast$mean)) #extract the max date used in the forecast
   
   #Extract Source and Training Data
   ds<-as.data.frame(window(dn,end=en))
   names(ds)<-'observed'
   ds$date<-as.Date(time(window(dn,end=en)))
   
   #Extract the Fitted Values (need to figure out how to grab confidence intervals)
   dfit<-as.data.frame(fcast$fitted)
   dfit$date<-as.Date(time(fcast$fitted))
   names(dfit)[1]<-'fitted'
   
   ds<-merge(ds,dfit,all.x=T) #Merge fitted values with source and training data
   
   #Exract the Forecast values and confidence intervals
   dfcastn<-as.data.frame(fcast)
   dfcastn$date<-as.Date(as.yearmon(row.names(dfcastn)))
   names(dfcastn)<-c('forecast','lo80','hi80','lo95','hi95','date')
   
   pd<-merge(ds,dfcastn,all.x=T) #final data.frame for use in ggplot
   return(pd)
   
}
#https://www.quandl.com/data/EIA/STEO_DSTCPUS_M-Diesel-Fuel-Consumption-Monthly
monthly_consumption_ts <- Quandl("EIA/STEO_DSTCPUS_M", api_key="f8nfVc_FqSSHBTBVRcYb", type="ts")
monthly_consumption_ts
c <- seas(monthly_consumption_ts)
final(c)
plot(c)
summary(c)
#inspect(c)

#https://www.quandl.com/data/FRED/GASDESM-US-Diesel-Sales-Price
diesel_monthly_price <- Quandl("FRED/GASDESM", api_key="f8nfVc_FqSSHBTBVRcYb", type="ts")

#diesel_weekly_price <- Quandl("EIA/PET_EMD_EPD2D_PTE_NUS_DPG_W", api_key="f8nfVc_FqSSHBTBVRcYb",  type="ts")
#diesel_weekly_price <- as.ts(diesel_weekly_price)
#diesel_monthly_price <- diesel_weekly_price
#diesel_monthly_price

c <- seas(diesel_monthly_price)
final(c)
plot(c)
summary(c)
#inspect(c)

#https://www.quandl.com/data/WSJ/DIESEL_ULS-Diesel-Fuel-15-ppm-S-NY-harbor-ultra
#####Check on it----error
WLS_Diesel <- Quandl("WSJ/DIESEL_ULS", api_key="f8nfVc_FqSSHBTBVRcYb", type="ts")
#WLS_Diesel
c <- seas(WLS_Diesel)
final(c)
plot(c)
summary(c)
#inspect(c)

diesel_monthly_price_df <- Quandl("FRED/GASDESM", api_key="f8nfVc_FqSSHBTBVRcYb")
diesel_monthly_price_df

stl = stl(diesel_monthly_price, "periodic")
plot(stl, main = "Seasonal Decomposition of National Monthly")

#Observing the effects of centered moving averages on the Nile dataset.
ylim = c(min(diesel_monthly_price), max(diesel_monthly_price))
plot(diesel_monthly_price, main = "Raw Time Series", ylim = ylim)
plot(ma(diesel_monthly_price, 3), main = "Centered Moving Averages (k = 3)", ylim = ylim)
plot(ma(diesel_monthly_price, 7), main = "Centered Moving Averages (k = 7)", ylim = ylim)
plot(ma(diesel_monthly_price, 15), main = "Centered Moving Averages (k = 15)", ylim = ylim)

plot(diesel_monthly_price, main = "Centered Moving Averages\nMonhtly Diesel Price Data", ylim = ylim)
lines(ma(diesel_monthly_price, 3), col = "red", lwd = 2)
lines(ma(diesel_monthly_price, 7), col = "green", lwd = 2)
lines(ma(diesel_monthly_price, 15), col = "blue", lwd = 2)
legend("topright",
       c("Raw Data", "k = 3", "k = 7", "k = 15"),
       col = c("black", "red", "green", "blue"),
       lwd = c(1, 2, 2, 2))

ndiffs(diesel_monthly_price)

difftsData = diff(diesel_monthly_price, differences = 1)
plot(difftsData, main = "Weekly Diesel Prices\n1 Difference")

adf.test(difftsData)

# Augmented Dickey-Fuller Test
# 
# data:  difftsData
# Dickey-Fuller = -5.9428, Lag order = 6, p-value = 0.01
# alternative hypothesis: stationary

par(mfrow=c(2, 1))
Acf(difftsData)
Pacf(difftsData)
par(mfrow=c(1,1))

initial.fit = Arima(diesel_monthly_price, order = c(1, 1, 1))
initial.fit
summary(initial.fit)
i_arima <- accuracy(initial.fit)
   
(1 - pnorm(abs(initial.fit$coef)/sqrt(diag(initial.fit$var.coef))))*2

plot(as.vector(fitted(initial.fit)), initial.fit$residuals, #Constant variance seems ok.
     main = "Residual Plot")
abline(h = 0, lty = 2)

qqnorm(initial.fit$residuals) #Normality of the errors appears to be fine.
qqline(initial.fit$residuals)

Acf(initial.fit$residuals) #No significant autocorrelations.
Pacf(initial.fit$residuals) #No significant partial autocorrelations.

Box.test(initial.fit$residuals, type = "Ljung-Box")

future.values = forecast(initial.fit, 30, level = c(60, 80, 95))
future.values
plot(future.values)

auto.arima(diesel_monthly_price)

i_auto <- auto.arima(diesel_monthly_price, approximation = FALSE)


fx.plot <- ggplot(data = diesel_monthly_price_df, aes(x=DATE,y=VALUE)) +
  geom_line(color = "#FAB521")+
  theme(panel.background = element_rect(fill="#393939"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour="white", size=0.1),
        panel.grid.minor = element_line(colour="white", size=0.1)) + 
  xlab("Date") +
  ylab("Diesel Prices") +
  ggtitle("Weekly US Average Diesel Prices")
fx.plot

diesel_monthly_price_df$YEAR <- year(diesel_monthly_price_df$DATE)

qplot(VALUE, data = diesel_monthly_price_df, geom = "density", fill = as.factor(YEAR)) +
  ggtitle("Weekly US Average Diesel Prices")

qplot(VALUE, data = diesel_monthly_price_df, geom = "histogram", fill = as.factor(YEAR)) +
  ggtitle("Weekly US Average Diesel Prices")

p1 <- ggplot(diesel_monthly_price_df, aes(as.factor(YEAR), VALUE))
p1 + geom_boxplot(aes(fill = as.factor(YEAR))) +
  ggtitle("Weekly US Average Diesel Prices")


posixDate <- as.POSIXct(diesel_monthly_price_df$DATE)
posixDate
diesel_monthly_price_d <- data.frame(posixDate, diesel_monthly_price_df$VALUE)
diesel_monthly_price_d
fx.ad <- AnomalyDetectionVec(diesel_monthly_price_d$diesel_monthly_price_df.VALUE, 
                             max_anoms=0.3, 
                             direction='both', 
                             plot=TRUE, 
                             period=30)

fx.ad
# plot the anomalies
fx.ad$plot

#BSE 
tsdisplay(diesel_monthly_price)
seasonplot(diesel_monthly_price,col=rainbow(30),year.labels=TRUE,season.labels=TRUE,ylab="Weekly Price",xlab="Date")

## Multiple seasonal periods 
diesel_prices.msts<-msts(diesel_monthly_price,seasonal.periods=12,ts.frequency=7,start=1994)
plot(diesel_prices.msts)
str(diesel_prices.msts)

## Fit a Linear Model with Time Series componenets of Trend and Seasonality
## use Additive if data set has CONSTANT seasonality Variation
## Use Multiplicative if data set has INCREASING or DECREASING Seasonality variation
diesel_prices.fitlinearAdditive<-tslm(diesel_monthly_price~trend+season,lambda=NULL)
diesel_prices.fitlinearMultiplicative<-tslm(diesel_monthly_price~trend*season,lambda=NULL)
diesel_prices.linearforecastMultiplicative<-forecast(diesel_prices.fitlinearMultiplicative,h=24)
plot(diesel_prices.linearforecastMultiplicative)
plot(residuals(diesel_prices.linearforecastMultiplicative))
a_tslm <- accuracy(diesel_prices.linearforecastMultiplicative)

## Forecast using Means for next 24 months
diesel_prices.fcastMean<-meanf(diesel_monthly_price,h=24,lambda=NULL)
plot(diesel_prices.fcastMean)
plot(residuals(diesel_prices.fcastMean))
a_meanf <- accuracy(diesel_prices.fcastMean)

## NAIVE models can  used for random walk data (ARIMA (0,1,0) or for random walk data with seasonality (ARIMA(0,0,0)(0,1,0)m))
diesel_prices.naive<- naive(diesel_monthly_price, h=24, level=c(80,95), fan=FALSE, lambda=NULL)
plot(diesel_prices.naive)
plot(residuals(diesel_prices.naive))
a_naive <- accuracy(diesel_prices.naive)

diesel_prices.snaive <- snaive(diesel_monthly_price, h=2*frequency(diesel_monthly_price), level=c(80,95), fan=FALSE, lambda=NULL)
plot(diesel_prices.snaive)
plot(residuals(diesel_prices.snaive))
a_snaive <- accuracy(diesel_prices.snaive)

## Using Neural Networks for Forecasting for next 24 months
##Feed-forward neural networks with a single hidden layer and lagged inputs for forecasting univariate time series
## NN model is suitable only for AR models. Eg. ARIMA (p,0,0)(P,0,0)- But not for Moving Average models
## Our Dataset is basically AR portion- Check ACF and PACF graphs above
diesel_prices.fitNN<-nnetar(diesel_monthly_price)
diesel_prices.forecastNN<-forecast(diesel_prices.fitNN,h=24)
plot(diesel_prices.forecastNN)
plot(residuals(diesel_prices.forecastNN))
a_nnetar <- accuracy(diesel_prices.forecastNN)

## THETA Method Forecastfor next 24 months - Equivalent to simple exponential smoothing with drift
diesel_prices.forecastTheta<-thetaf(diesel_monthly_price,h=24,level=c(80,95))
plot(diesel_prices.forecastTheta)
plot(residuals(diesel_prices.forecastTheta))
a_thetaf <- accuracy(diesel_prices.forecastTheta)

##TBATS model (Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components)
diesel_prices.fitTBATS<-tbats(diesel_monthly_price)
diesel_prices.forecastTBATS<-forecast(diesel_prices.fitTBATS,h=24)
plot(diesel_prices.forecastTBATS)
plot(residuals(diesel_prices.forecastTBATS))
a_tbats <- accuracy(diesel_prices.forecastTBATS)

diesel_prices.TBATScomponents<-tbats.components(diesel_prices.fitTBATS)
diesel_prices.TBATScomponents
plot(diesel_prices.TBATScomponents)

##Cubic Smoothing Spline Forecasting for next 24 months
## Suitable fpor Moving Average Model Data Sets - equivalent to ARIMA(0,2,2)
diesel_prices.forecastCubic<-splinef(diesel_monthly_price,h=24)
plot(diesel_prices.forecastCubic)
summary(diesel_prices.forecastCubic)
plot(residuals(diesel_prices.forecastCubic))
a_splinef <- accuracy(diesel_prices.forecastCubic)

## Three more Exponenetial Smoothening Forecasts for next 24 months
## SES Type
diesel_prices.forecastsSES<-ses(diesel_monthly_price,h=24)
plot(diesel_prices.forecastsSES)
plot(residuals(diesel_prices.forecastsSES))
a_ses <- accuracy(diesel_prices.forecastsSES)

##HOLT Type
diesel_prices.forecastsHOLT<-holt(diesel_monthly_price,h=24)
plot(diesel_prices.forecastsHOLT)
plot(residuals(diesel_prices.forecastsHOLT))
a_holt <- accuracy(diesel_prices.forecastsHOLT)

##HW Type - Multiplicative Seasonal  (Additive ca aslo be used based on dataset)
diesel_prices.forecastsHW<-hw(diesel_monthly_price, h=24, seasonal="multiplicative")
plot(diesel_prices.forecastsHW)
plot(residuals(diesel_prices.forecastsHW))
a_hw <- accuracy(diesel_prices.forecastsHW)

## Forecasts using SEASONAL DUMMY VARIABLES using dummy variables and fourier series
## This can be used for ARIMA, LM or TSLM
##In our case we will be using TSLM
plot(stock)
##Create new seasonal dummy variables
## Last month December is taken for control group
diesel_prices.dummyTSLM<-tslm(diesel_monthly_price~season)
## Residual plot
tsdisplay(residuals(diesel_prices.dummyTSLM))
## Forecast for next 24 months
diesel_prices.forecastDummyTSLM<-forecast(diesel_prices.dummyTSLM,h=24)
plot(diesel_prices.forecastDummyTSLM)
plot(residuals(diesel_prices.forecastDummyTSLM))
##Accuracy test
a_dummytslm <- accuracy(diesel_prices.forecastDummyTSLM)

##Now lets use Fourier Transforms with 3 maximum order of fourier terms
fourierdummy<-fourier(diesel_monthly_price,3)
diesel_prices.fourierTSLM<-tslm(diesel_monthly_price~fourierdummy)
diesel_prices.forecastFourier<-forecast(diesel_prices.fourierTSLM,data.frame(fourierdummy=I(fourierf(diesel_monthly_price,3,24))))
plot(diesel_prices.forecastFourier)
plot(residuals(diesel_prices.forecastFourier))
## Similar above syntax can be used for Seasonal Dummy variables above also
##Accuracy Test
a_fourier <- accuracy(diesel_prices.forecastFourier)

## Random Walk Forecast with drift model for next 24 months
diesel_prices.forecastRandomWalk<-rwf(diesel_monthly_price,h=24,drift=TRUE, lambda=TRUE)
plot(diesel_prices.forecastRandomWalk)
plot(residuals(diesel_prices.forecastRandomWalk))
a_rwf <- accuracy(diesel_prices.forecastRandomWalk)

##Forecast Using STRUCTURAL TIMESERIES MODELS for next 24 months
diesel_prices.fitStructTS<-StructTS(diesel_monthly_price,"level")
diesel_prices.forecastStructTS<-forecast(diesel_prices.fitStructTS,h=24)
plot(diesel_prices.forecastStructTS)
plot(residuals(diesel_prices.forecastStructTS))
##Accuracy test
a_structts <- accuracy(diesel_prices.forecastStructTS)

## Forecast for next 24 months using HOLT-WINTERS method
diesel_prices.fitHOLTWinter<-HoltWinters(diesel_monthly_price)
diesel_prices.forecastHoltWinter<-forecast(diesel_prices.fitHOLTWinter)
plot(diesel_prices.forecastHoltWinter)
plot(residuals(diesel_prices.forecastHoltWinter))
##Accuracy Test
a_holtwinters <- accuracy(diesel_prices.forecastHoltWinter)

##Exponential smoothing state space model (ETS)
diesel_prices.fitETS<-ets(diesel_monthly_price)
diesel_prices.forecastETS<-forecast(diesel_prices.fitETS,h=24)
plot(diesel_prices.forecastETS,plot.type="single",ylab="",col=1:3)
plot(residuals(diesel_prices.forecastETS))
a_ets <- accuracy(diesel_prices.forecastETS)

# ##Log Likelihood to check validity and accuracy of the model
# logLik(diesel_prices.fitETS)
# ##Simulate for next 24 months in Detail
# diesel_prices.simulateETS<-simulate(diesel_prices.fitETS,24)
# plot(diesel_prices.simulateETS,col="blue")
# #plot(residuals(diesel_prices.simulateETS))
# ##Accuracy Test
# accuracy(diesel_prices.forecastETS)

##Taylor’s Double-Seasonal Holt-Winters method to forecast for next 24 months
# diesel_prices.forecastDSHW<-dshw(diesel_monthly_price)
# plot(diesel_prices.forecastDSHW,plot.type="single",ylab="",col=1:3)
# plot(residuals(diesel_prices.forecastDSHW))
# accuracy(diesel_prices.forecastDSHW)
##NOT possible for our dataset as it doen not have multiple seasonal components

##Forecasts for intermittent demand using Croston’s method - Simple Exponenetial Smoothing
diesel_prices.forecastCroston<-croston(diesel_monthly_price,h=24,alpha=0.1)
plot(diesel_prices.forecastCroston)
plot(residuals(diesel_prices.forecastCroston))
## Residual graph is not stationary, Hence not a propoer model for fit
##Accuracy test
a_croston <- accuracy(diesel_prices.forecastCroston)
## Autoregressive type in ARIMA for forecasting for next 24 months
diesel_prices.forecastAR<-forecast(ar(diesel_monthly_price),h=24)
plot(diesel_prices.forecastAR)
plot(residuals(diesel_prices.forecastAR))
##Simulate for next 24 months in Detail
#plot(simulate(ar(stock),24))
##Accuracy Test
a_ar <- accuracy(diesel_prices.forecastAR)

##BATS model (Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components)
diesel_prices.fitBATS<-bats(diesel_monthly_price)
diesel_prices.forecastBats<-forecast(diesel_prices.fitBATS,h=24)
plot(diesel_prices.forecastBats)
plot(residuals(diesel_prices.forecastBats))##Accuracy Test
a_bats <- accuracy(diesel_prices.forecastBats)

##ARIMA MODELS - ONLY FOR UNIVARIATE DATASETS
## No. of Differences required to convert Non-Stationary Series into Stationary Series
## Estimate No. of First Differences- 95% confidence interval and max difference allowable is 2
ndiffs(diesel_monthly_price,alpha=0.05,test=c("kpss","adf","pp"),max.d=2)
## Answer is 1, Hence only 1 differencing is required for stationarity
## Estimate No. of Seasonal Differences
nsdiffs(diesel_monthly_price, m=frequency(diesel_monthly_price), test=c("ocsb","ch"), max.D=2)
## Answer is 0, Hence NO seasonal differencing required
##To Find best possible ARIMA model-Returns best ARIMA model according to either AIC, AIC or BIC value
diesel_prices.findbestmodel<-auto.arima(diesel_monthly_price)
plot(forecast(diesel_prices.findbestmodel,h=24))
plot(residuals(forecast(diesel_prices.findbestmodel),h=24))
a_auto.arima <- accuracy(diesel_prices.findbestmodel)

## Return the ORDER for ARIMA or ARFIMA model
Order=arimaorder(diesel_prices.findbestmodel)
Order
##Best best model has a order of (1,1,1)
##Fit ARIMA (1,1,1)
diesel_prices.fitArima<-Arima(diesel_monthly_price,order=c(1,1,1))
diesel_prices.forecastArima<-forecast(diesel_prices.fitArima,24)
plot(diesel_prices.forecastArima)
plot(residuals(diesel_prices.forecastArima))
a_arima <- accuracy(diesel_prices.findbestmodel)

##Simulate for next 24 months in Detail
#plot(simulate(diesel_prices.fitArima),24)
##Returns one-step forecasts for the data used in fitting the ARIMA model
diesel_prices.fittedArimaData<-fitted(diesel_prices.fitArima)
diesel_prices.fittedArimaData
plot(fitted(diesel_prices.fitArima))
##One can check the residuals and MSE using original data and fitted data
Residuals<-diesel_prices.fittedArimaData-stock
par(mfrow=c(3,1))
plot(stock)
plot(diesel_prices.fittedArimaData)
plot(Residuals)
MSE<-mean(Residuals^2)
MSE
## MSE is very high;, Suitable model to be selected to reduce it
##Accuracy Test
a_fittedarima <- accuracy(diesel_prices.fittedArimaData)

##Fit a fractionally differenced ARFIMA model
library(fracdiff)
diesel_prices.fitFracdiff<-fracdiff(diesel_monthly_price)
diesel_prices.forecastFracdiff<-forecast(diesel_prices.fitFracdiff,h=24)
plot(diesel_prices.forecastFracdiff)
plot(residuals(diesel_prices.forecastFracdiff))
a_fracdiff <- accuracy(diesel_prices.forecastFracdiff)

##Residuals are not stationary using Fracdiff Model
diesel_prices.fitArfima<-arfima(diesel_monthly_price)
diesel_prices.forecastArfima<-forecast(diesel_prices.fitArfima,h=24)
plot(diesel_prices.forecastArfima)
plot(residuals(diesel_prices.forecastArfima))
tsdisplay(residuals(diesel_prices.fitArfima))
##Accuracy test
a_arfima <- accuracy(diesel_prices.forecastArfima)


##Decompopsiton Method
diesel_prices.fitDecompose<-decompose(diesel_monthly_price,type="multiplicative")
plot(diesel_prices.fitDecompose)
##Return Seasonal adjusted data by removing seasonal component
diesel_prices.seasonadj<-seasadj(diesel_prices.fitDecompose)
diesel_prices.seasonadj
plot(diesel_prices.seasonadj)
##MOving Average at order of 12 months
diesel_prices.ma<-ma(diesel_monthly_price,order=12)
plot(diesel_prices.ma)
# plot(residuals(diesel_prices.ma))
# accuracy(diesel_prices.ma)


plot(decompose(diesel_prices.ma,type="multiplicative"))
plot(seasadj(decompose(diesel_prices.ma,type="multiplicative")))


#http://ellisp.github.io/blog/2016/01/30/hybrid-forecasts/
# library(devtools)
# install_github("robjhyndman/forecast") # development version needed sorry
# library(forecast)
# 
 source("myhybridforecast.R")
# 
# devtools::install_github("ellisp/forecastHybrid/pkg")
# 
# library(forecastHybrid)
fc <- hybridf(diesel_monthly_price)
par(mfrow = c(3, 1), bty = "l")
plot(fc)
plot(fc$fc_ets)
plot(fc$fc_aa)
str(fc)
a_forecasthybrid <- accuracy(fc)

str(diesel_prices.snaive)
accuracy(diesel_prices.snaive)
residuals(diesel_prices.snaive)

diesel_prices.snaive$method
 #stlf

diesel_prices.fitNN
str(diesel_prices.fitArima)

diesel_prices.fitArima$coef
diesel_prices.fitArima$aic
diesel_prices.fitArima$arma
#diesel_prices.fitArima$residuals
diesel_prices.fitArima$aicc
diesel_prices.fitArima$bic
diesel_prices.fitArima$nobs
diesel_prices.fitArima$sigma2
diesel_prices.fitArima$var.coef

summary(diesel_prices.fitArima)
accuracy(diesel_prices.fitArima)
coef(diesel_prices.fitArima)
plot(diesel_prices.fitArima)

a <- accuracy(diesel_prices.fitArima)
str(a)
a[0]
a[1:7]
a[[2]]
dimnames(a)
dim(a)
colnames(a)
n <- c("model", colnames(a))
n
x <- c("test", "test2", a[1:7])
y <- c("test22", "test222", a[1:7])
x
dim(a)
nodata <- data.frame(n)
nodata
nodata <- as.data.frame(setNames(n))
model_summary <- data.frame( "ModelName" = character(0), "Model" = character(0), 
                   "ME" = numeric(0),  "RMSE" = numeric(0),  "MAE" = numeric(0),
                   "MPE" = numeric(0),  "MAPE" = numeric(0),  "MASE" = numeric(0),
                   "ACF1" = numeric(0)
                   )
model_summary
model_summary = rbind(model_summary,x)
model_summary = rbind(model_summary,y)
model_summary
z = as.data.frame(x)
z = rbind(z, y)
z

a_tslm
a_meanf
a_naive
a_snaive
a_nnetar
a_thetaf
a_tbats
a_splinef
a_ses
a_holt
a_hw
a_dummytslm
a_fourier
a_rwf
a_structts
a_holtwinters
a_ets
a_croston
a_ar
a_bats
a_auto.arima
a_arima
a_fracdiff
a_arfima
a_ma
i_auto
i_arima

#http://www.seasonal.website/seasonal.html


a_summary <-  data.frame(t(c("tslm",    a_tslm[1:7])))
a_summary <- rbind(a_summary, data.frame(t(c("meanf",   a_meanf[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("naive",   a_naive[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("snaive",  a_snaive[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("nnetar",  a_nnetar[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("thetaf",  a_thetaf[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("tbats",   a_tbats[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("splinef",         a_splinef[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("ses",     a_ses[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("holt",    a_holt[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("hw",      a_hw[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("dummytslm",       a_dummytslm[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("fourier",         a_fourier[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("rwf",     a_rwf[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("structts",        a_structts[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("holtwinters",     a_holtwinters[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("ets",     a_ets[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("croston",         a_croston[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("ar",      a_ar[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("bats",    a_bats[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("auto.arima",      a_auto.arima[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("arima",   a_arima[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("fracdiff",        a_fracdiff[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("arfima",  a_arfima[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("ma",      a_ma[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("i_auto",  i_auto[1:7]))))
a_summary <- rbind(a_summary, data.frame(t(c("i_arima",         i_arima[1:7]))))


write.csv(a_summary, file = "diesel_monthly_algo_summary.csv")
library(plyr)

a_summary <- rename(a_summary, c("X1"="Model", "X2"= "ME", "X3"= "RMSE", "X4"= "MAE", 
                    "X5"= "MPE", "X6"= "MAPE", "X7"= "MASE", "X8"= "ACF1"))

a_summary

a_summary$ME <- as.numeric(as.character(a_summary$ME))
a_summary$RMSE <- as.numeric(as.character(a_summary$RMSE))
a_summary$ACF1 <- as.numeric(as.character(a_summary$ACF1))
ggplot(a_summary, aes(x = a_summary$Model, y = a_summary$RMSE)) + theme_bw() + geom_bar(stat = "identity")
ggplot(a_summary, aes(x = a_summary$Model, y = a_summary$ACF1)) + theme_bw() + geom_bar(stat = "identity")

