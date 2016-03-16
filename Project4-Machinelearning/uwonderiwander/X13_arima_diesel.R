#http://www.seasonal.website/seasonal.html
library(Quandl)
library(seasonal)
library(shiny)

#https://www.quandl.com/data/FRED/GASDESM-US-Diesel-Sales-Price
diesel_monthly_price <- Quandl("FRED/GASDESM", api_key="f8nfVc_FqSSHBTBVRcYb", type="ts")

diesel_monthly_price.seas <- seas(
   x = diesel_monthly_price,
   transform.function = "log",
   x11 = "",
   regression.variables = c("const"),
   arima.model = "(0 1 1)12"
)

final(diesel_monthly_price.seas)
plot(diesel_monthly_price.seas)
summary(diesel_monthly_price.seas)


monthplot(diesel_monthly_price.seas)
monthplot(diesel_monthly_price.seas, choice = "irregular")
pacf(resid(diesel_monthly_price.seas))
spectrum(diff(resid(diesel_monthly_price.seas)))
plot(density(resid(diesel_monthly_price.seas)))
qqnorm(resid(diesel_monthly_price.seas))
identify(diesel_monthly_price.seas)
#inspect(diesel_monthly_price.seas)
str(diesel_monthly_price.seas)
