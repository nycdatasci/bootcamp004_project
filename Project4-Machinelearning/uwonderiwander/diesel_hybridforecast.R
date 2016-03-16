#source("/Users/satishjoshi/DataBootcamp/forecastHybrid/pkg/R/hybridf.R")

#source("https://raw.githubusercontent.com/ellisp/forecast/dev/R/hybridf.R")

#https://github.com/ellisp/forecastHybrid
library(Quandl)
library(seasonal)
library(forecast)
library(ggplot2)
library(lubridate)
library(AnomalyDetection)
library(tseries)

#https://www.quandl.com/data/FRED/GASDESM-US-Diesel-Sales-Price
diesel_monthly_price <- Quandl("FRED/GASDESM", api_key="f8nfVc_FqSSHBTBVRcYb", type="ts")

hybridf <- function(x, 
                    # parameters to pass to forecast.ets() and forecast.Arima():
                    h = ifelse(frequency(x) > 1, 2 * frequency(x), 10),
                    level = c(80, 95),
                    fan = FALSE,
                    simulate = FALSE,   
                    bootstrap.ets = FALSE,
                    bootstrap.aa = FALSE,  
                    npaths = 5000,
                    
                    # parameters for both ets() and auto.arima()
                    lambda = NULL,
                    biasadj = FALSE,
                    ic = c("aicc", "aic", "bic"),
                    
                    # parameters to pass to ets()
                    model = "ZZZ",
                    damped = NULL,
                    alpha = NULL, 
                    beta = NULL,
                    gamma = NULL,
                    phi = NULL,
                    additive.only = FALSE,
                    lower = c(rep(0.0001, 3), 0.8),
                    upper = c(rep(0.9999, 3), 0.98),
                    opt.crit = c("lik", "amse", "mse", "sigma", "mae"),
                    nmse = 3,
                    bounds = c("both", "usual", "admissible"),
                    restrict = TRUE,
                    allow.multiplicative.trend = FALSE,
                    
                    # parameters to pass to auto.arima:
                    d = NA,
                    D = NA,
                    max.p = 5,
                    max.q = 5,
                    max.P = 2,
                    max.Q = 2,
                    max.order = 5,
                    max.d = 2,
                    max.D = 1,
                    start.p = 2,
                    start.q = 2,
                    start.P = 1,
                    start.Q = 1,
                    stationary = FALSE,
                    seasonal = TRUE,
                    stepwise = TRUE,
                    trace = FALSE,
                    approximation = (length(x) > 100 | frequency(x) >12),
                    # xreg = NULL, # not yet implemented, only univariate allowed for now
                    test = c("kpss", "adf", "pp"),
                    seasonal.test = c("ocsb", "ch"),
                    allowdrift = TRUE,
                    allowmean = TRUE,
                    parallel = FALSE,
                    num.cores = 2){
   
   
   mod1 <- ets(x, model = model, damped = damped,
               alpha = alpha, beta = beta, gamma = gamma, phi = phi,
               additive.only = additive.only, lower = lower, upper = upper,
               opt.crit = opt.crit, nmse = nmse, bounds = bounds,
               restrict = restrict, allow.multiplicative.trend = allow.multiplicative.trend,
#               lambda = lambda, biasadj = biasadj, ic = ic)
   lambda = lambda,  ic = ic)

   
   mod2 <- auto.arima(x,
                      d = d, D = D, max.p = max.p, max.q = max.q, max.Q = max.Q,
                      max.order = max.order, max.d = max.d, max.D = max.D,
                      start.p = start.p, start.q = start.q, start.P = start.P, start.Q=start.Q,
                      stationary = stationary, seasonal = seasonal, stepwise = stepwise, trace = trace,
                      approximation = approximation, test = test, seasonal.test = seasonal.test,
                      allowdrift = allowdrift, allowmean = allowmean, 
                      parallel = parallel, num.cores = num.cores,
#                      lambda = lambda, biasadj = biasadj, ic = ic)
   lambda = lambda,  ic = ic)

   fc1 <- forecast(mod1, h = h, level = level, fan = fan, simulate = simulate,
                   bootstrap = bootstrap.ets, npaths = npaths, lambda = lambda,
                   biasadj = biasadj)
   
   fc2 <- forecast(mod2, h = h, level = level, fan = fan, 
                   bootstrap = bootstrap.aa, npaths = npaths, lambda = lambda,
                   biasadj = biasadj)
   
   fc_comb <- list()
   fc_comb$model <- list(mod1 = mod1, mod2 = mod2)
   fc_comb$method <- paste("hybrid of", fc1$method, "and", fc2$method)
   fc_comb$mean <- (fc1$mean + fc2$mean) / 2
   fc_comb$lower <- fc1$lower
   fc_comb$lower[fc_comb$lower > fc2$lower] <- fc2$lower[fc_comb$lower > fc2$lower]
   fc_comb$upper <- fc1$upper
   fc_comb$upper[fc_comb$upper < fc2$upper] <- fc2$upper[fc_comb$upper < fc2$upper]
   fc_comb$level <- level
   fc_comb$x <- x
   fc_comb$fitted <- (fc1$fitted + fc2$fitted) / 2
   fc_comb$residuals <- fc_comb$fitted - fc_comb$x
   fc_comb$fc_ets <- fc1
   fc_comb$fc_aa <- fc2
   class(fc_comb) <- "forecast"
   
   return(fc_comb)
}

fc <- hybridf(diesel_monthly_price)
par(mfrow = c(3, 1), bty = "l")
plot(fc)
plot(fc$fc_ets)
plot(fc$fc_aa)