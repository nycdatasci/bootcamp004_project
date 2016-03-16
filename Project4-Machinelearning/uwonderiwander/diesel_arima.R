library(forecast)
library(tseries)
library(xts)

#setwd("/Users/satishjoshi/DataBootcamp/ipython")
setwd("/Users/satishjoshi/DataBootcamp/bootcamp004_project/Project4-Machinelearning/uwonderiwander")
diesel_data_ts=read.csv("weekly_diesel.csv",  stringsAsFactors=FALSE)

diesel_data_ts$Date <- as.Date(as.character(diesel_data_ts$Date),format="%d-%b-%y")

dates <- diesel_data_ts$Date
national <- diesel_data_ts$EMD_EPD2D_PTE_NUS_DPG
values <- diesel_data_ts[, -1]
weekly <- diesel_data_ts$EMD_EPD2D_PTE_NUS_DPG
diesel_xts <- as.xts(values, order.by = as.Date(dates, "%d-%b-%y"))
diesel_weekly <- as.xts(weekly, order.by = as.Date(dates, "%d-%b-%y"))

diesel_national <- as.xts(national, order.by = as.Date(dates, "%d-%b-%y"))
diesel_national_monthly <- to.monthly(diesel_national)
dim(diesel_national_monthly)
start(diesel_national)
end(diesel_national)
str(diesel_national_monthly)
par(mfrow=c(2,1))



ts_vec = diesel_national_monthly$diesel_national.Open
x = as.matrix(ts_vec)
dim(x)
x = as.vector(unname(x))
tsData = ts(x, start=c(1994, 3), end=c(2016, 2), frequency = 12)
tsData
stl = stl(tsData, "periodic")
plot(stl, main = "Seasonal Decomposition of National Monthly")

x = as.matrix(diesel_national)
x = as.vector(unname(x))
tsData = ts(x, start=c(1994, 3, 21), end=c(2016, 2, 22), frequency = 7)
tsData
class(tsData)
stl = stl(tsData, "periodic")
plot(stl, main = "Seasonal Decomposition of National Weekly")

#Observing the effects of centered moving averages on the Nile dataset.
par(mfrow=c(4,1))

ylim = c(min(tsData), max(tsData))
plot(tsData, main = "Raw Time Series", ylim = ylim)
plot(ma(tsData, 3), main = "Centered Moving Averages (k = 3)", ylim = ylim)
plot(ma(tsData, 7), main = "Centered Moving Averages (k = 7)", ylim = ylim)
plot(ma(tsData, 15), main = "Centered Moving Averages (k = 15)", ylim = ylim)

par(mfrow=c(1,1))

plot(tsData, main = "Centered Moving Averages\nWeekly Diesel Price Data", ylim = ylim)
lines(ma(tsData, 3), col = "red", lwd = 2)
lines(ma(tsData, 7), col = "green", lwd = 2)
lines(ma(tsData, 15), col = "blue", lwd = 2)
legend("topleft",
       c("Raw Data", "k = 3", "k = 7", "k = 15"),
       col = c("black", "red", "green", "blue"),
       lwd = c(1, 2, 2, 2))

ndiffs(tsData)

difftsData = diff(tsData, differences = 1)
plot(difftsData, main = "Weekly Diesel Prices\n1 Difference")

adf.test(difftsData)

# Augmented Dickey-Fuller Test
# 
# data:  difftsData
# Dickey-Fuller = -3.8444, Lag order = 5, p-value = 0.01873
# alternative hypothesis: stationary

par(mfrow=c(2, 1))
Acf(difftsData)
Pacf(difftsData)
par(mfrow=c(1,1))

initial.fit = Arima(tsData, order = c(1, 1, 1))
initial.fit
summary(initial.fit)
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

auto.arima(tsData)

auto.arima(tsData, approximation = FALSE)

# > auto.arima(tsData)
# Series: tsData 
# ARIMA(1,1,4)                    
# 
# Coefficients:
#   ar1     ma1     ma2     ma3     ma4
# -0.6689  1.2725  0.6095  0.5733  0.4510
# s.e.   0.1150  0.1113  0.1282  0.1355  0.0837
# 
# sigma^2 estimated as 5.629e-05:  log likelihood=530.77
# AIC=-1049.55   AICc=-1048.97   BIC=-1031.36
# > auto.arima(tsData, approximation = FALSE)
# Series: tsData 
# ARIMA(4,1,2)(1,0,0)[7] with drift         
# 
# Coefficients:
#   ar1      ar2     ar3      ar4      ma1      ma2    sar1   drift
# 1.0147  -0.0005  0.2682  -0.4134  -0.5632  -0.3353  0.1637  0.0012
# s.e.  0.1588   0.2358  0.1319   0.0761   0.1733   0.1679  0.0935  0.0006
# 
# sigma^2 estimated as 5.441e-05:  log likelihood=533.39
# AIC=-1048.79   AICc=-1047.53   BIC=-1021.51
#https://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting/forums/t/8033/simple-models-and-straight-averages

tsData[is.na(tsData)] <- 0
model <- tslm(tsData ~ trend + season)
fc <- forecast(model)
future.values2 = forecast(fc, 30, level = c(60, 80, 95))
future.values2
plot(future.values2)

                                  

# diesel_xts_monthly <- to.monthly(diesel_xts)
# 
# start(diesel_xts)
# end(diesel_xts)
# summary(diesel_xts)
# plot(diesel_xts)
# 
# start(diesel_xts_monthly)
# end(diesel_xts_monthly)
# summary(diesel_xts_monthly)
# plot(diesel_xts_monthly)
# stl(diesel_xts_monthly)
# 
# stl(diesel_weekly)
# head(as.ts(diesel_weekly))
# as.data.frame(diesel_xts)
# as.data.frame(diesel_xts_monthly)
# 
# # seasonplot(as.ts(diesel_weekly), ylab="$ million", xlab="Year", 
# #            main="Seasonal plot: antidiabetic drug sales", 
# #            year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19)
# # monthplot(diesel_weekly,ylab="$ million",xlab="Month",xaxt="n",
# #           main="Seasonal deviation plot: antidiabetic drug sales")
# # axis(1,at=1:12,labels=month.abb,cex=0.8)
# # str(diesel_data_ts)
# plot(jitter(diesel_data_ts$EMD_EPD2D_PTE_NUS_DPG), jitter(diesel_data_ts$EMD_EPD2D_PTE_R10_DPG), xlab="US Average", ylab="East Coast")
# pairs(diesel_data_ts[,-1], pch=19)
# 
# lag.plot(diesel_weekly, lags=2, do.lines=TRUE)
# Acf(diesel_weekly)
# #Acf(diesel_xts)
# 
# beerfit1 <- meanf(diesel_weekly, h=11)
# beerfit2 <- naive(diesel_weekly, h=11)
# beerfit3 <- snaive(diesel_weekly, h=11)
# plot(beerfit1, plot.conf=FALSE, 
#      main="Forecasts for quarterly beer production")
# lines(beerfit2$mean,col=2)
# lines(beerfit3$mean,col=3)
# 
# plot(diesel_weekly, main="Dow Jones Index (daily ending 15 Jul 94)", 
#      ylab="", xlab="Day")
# res <- residuals(naive(diesel_weekly))
# plot(res, main="Residuals from naive method", 
#      ylab="", xlab="Day")
# Acf(res, main="ACF of residuals")
# hist(res, nclass="FD", main="Histogram of residuals")
# 
# Box.test(res, lag=10, fitdf=0)
# Box.test(res,lag=10, fitdf=0, type="Lj")
# 
# forecast(diesel_weekly)
#legend("topleft",lty=1,col=c(4,2,3),
#       legend=c("Mean method","Naive method","Seasonal naive method"))

# dygraph(diesel_xts, main = "Diesel Prices Across Regions") %>% 
#   dySeries("EMD_EPD2D_PTE_NUS_DPG", label = "US Average") %>%
#   dySeries("EMD_EPD2D_PTE_R10_DPG", label = "East Coast") %>%
#   dySeries("EMD_EPD2D_PTE_R1X_DPG", label = "New England") %>%
#   dySeries("EMD_EPD2D_PTE_R1Y_DPG", label = "Central Altlantic") %>%
#   dySeries("EMD_EPD2D_PTE_R1Z_DPG", label = "Lower Atlantic") %>%
#   dySeries("EMD_EPD2D_PTE_R20_DPG", label = "Mid West") %>%
#   dySeries("EMD_EPD2D_PTE_R30_DPG", label = "Gulf Coast") %>%
#   dySeries("EMD_EPD2D_PTE_R40_DPG", label = "Rocky Mountain") %>%
#   dySeries("EMD_EPD2D_PTE_R50_DPG", label = "West Coast") %>%
#   dySeries("EMD_EPD2D_PTE_SCA_DPG", label = "California") %>%
#   dyShading(from = "2008-2-11", to = "2009-1-5", color = "#CCEBD6") %>%
#   dyShading(from = "2014-9-14", to = "2015-2-16", color = "#FFE6E6") %>%
#   #  http://www.infoplease.com/world/events/2008/jun.html  
#   dyEvent("2008-02-01", "Economy Loses Jobs for the First Time in 52 Months", labelLoc = "bottom") %>%
#   dyOptions(stackedGraph = TRUE) %>%
#   dyRangeSelector() %>%
#   dyLegend(show = "follow")
