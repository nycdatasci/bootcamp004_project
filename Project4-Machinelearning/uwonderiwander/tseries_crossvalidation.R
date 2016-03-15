

#http://www.r-bloggers.com/a-practical-introduction-to-garch-modeling/
#http://yunus.hacettepe.edu.tr/~iozkan/eco665/archgarch.html
#https://talksonmarkets.files.wordpress.com/2012/09/time-series-analysis-with-arima-e28093-arch013.pdf
#http://ptrckprry.com/course/forecasting/lecture/archfit.html
#http://www.noveltyjournals.com/journal/IJNRMME/Issue-1-January-2015-April-2015/0

#http://www.r-bloggers.com/time-series-cross-validation-4-forecasting-the-sp-500/
#http://www.r-bloggers.com/functional-and-parallel-time-series-cross-validation/
#devtools::install_github('zachmayer/cv.ts', force=TRUE)
#Setup
#rm(list = ls(all = TRUE))
#setwd('path.to/cv.ts')

#Load Packages
library(tseries)
library(forecast)
library(snow)
library(Quandl)
library(quantmod)

require(forecast)
require(doParallel)
setwd("/Users/satishjoshi/DataBootcamp/bootcamp004_project/Project4-Machinelearning/uwonderiwander/cv.ts")
source('R/cv.ts.R')
source('R/forecast_helper_funs.R')

#https://www.quandl.com/data/EIA/PET_EMD_EPD2D_PTE_NUS_DPG_W-U-S-No-2-Diesel-Retail-Prices-Weekly
diesel_weekly_price <- Quandl("EIA/PET_EMD_EPD2D_PTE_NUS_DPG_W", api_key="f8nfVc_FqSSHBTBVRcYb",  type="ts")
diesel_weekly_price <- as.ts(diesel_weekly_price)
diesel_weekly_price
#Download S&P 500 data and adjust from splits/dividends
# getSymbols('^GSPC', from='1990-01-01')
# GSPC <- adjustOHLC(GSPC, symbol.name='^GSPC')
# 
# #Calculate monthly returns
# GSPC <- to.monthly(GSPC, indexAt='lastof')
# GSPC <- Cl(GSPC)
# 
# #Convert from xts to ts
# GSPC <- ts(GSPC, start=c(1990,1), frequency=12)

#GSPC <- diesel_monthly_price
GSPC <- diesel_weekly_price
GSPC
#Start a cluster to speed up cross validaiton
cl <- makeCluster(4, type='SOCK')
registerDoParallel(cl)

#Define cross validation parameters
myControl <- tseriesControl(
   minObs=60,
   stepSize=1, 
   maxHorizon=12, 
   fixedWindow=TRUE,
   preProcess=FALSE,
   ppMethod='guerrero',
   summaryFunc=tsSummary
)

#Forecast using several models
result_naive <- cv.ts(GSPC, naiveForecast, myControl)
myControl$preProcess <- TRUE
result_autoarima <- cv.ts(GSPC, auto.arimaForecast, myControl, ic='bic')
result_ets <- cv.ts(GSPC, etsForecast, myControl, ic='bic')

#Stop cluster 
stopCluster(cl)

#Plot error
require(reshape2)
require(ggplot2)
plotData <- data.frame(
   horizon=1:12
   ,naive =result_naive$results$MAPE[1:12]
   ,arima=result_autoarima$results$MAPE[1:12]
   ,ets=result_ets$results$MAPE[1:12]
)
plotData <- melt(plotData, id.vars='horizon', value.name='MAPE', variable.name='model')
ggplot(plotData, aes(horizon, MAPE, color=model)) + geom_line()


plotData <- data.frame(
   horizon=1:12
   ,naive =result_naive$results$RMSE[1:12]
   ,arima=result_autoarima$results$RMSE[1:12]
   ,ets=result_ets$results$RMSE[1:12]
)
plotData <- melt(plotData, id.vars='horizon', value.name='RMSE', variable.name='model')
ggplot(plotData, aes(horizon, RMSE, color=model)) + geom_line()

result_naive$results
