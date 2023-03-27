# Install necessary packages and load libraries.
install.packages('quantmod')
install.packages('xts')
install.packages('PerformanceAnalytics')
install.packages('rugarch')
install.packages('rmgarch')
install.packages('tseries')
install.packages('fImport')
install.packages('httr')
install.packages('RCurl')
install.packages('rjson')
install.packages('parsedate')

library(fImport)
library(httr)
library(forecast)
library(RCurl)
library(rjson)
library(parsedate)
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(rugarch)
library(rmgarch)
library(tseries)
library(readr)
library(Acf)
library(vars)

# Reading the CSV files with TS data.
GBP <- read_csv("Desktop/GBP.csv", 
                col_types = cols(Date = col_date(format = "%Y-%m-%d")))

EUR <- read_csv("Desktop/EUR.csv", 
                col_types = cols(Date = col_date(format = "%Y-%m-%d")))

# I am only interested in Date column, in Close column and in you, so I
# will get rid of the rest.
GBP <- GBP[-c(7, 6, 4, 3, 2)]
EUR <- EUR[-c(7, 6, 4, 3, 2)]

# Converting the Close column to numeric.
GBP$Close <- as.numeric(GBP$Close)
EUR$Close <- as.numeric(EUR$Close)

# What I am doing here is the following: R has a special treatment for 
# time series objects, so you can treat them as regular data.frames. Thus,
# I have to transform EUR and GBP to xts objects, and in order to do that,
# I have to change Date column from date format to POSIXct format.
GBP$Date <- as.POSIXct(GBP$Date)
EUR$Date <- as.POSIXct(EUR$Date)
GBP <- xts(GBP$Close, order.by=GBP$Date)
EUR <- xts(EUR$Close, order.by=EUR$Date)

# Some charts to see how the data is behaving.
chartSeries(EUR, theme = chartTheme("white"))
chartSeries(GBP, theme = chartTheme("white"))
chartSeries(log(EUR), theme = chartTheme("white"))
chartSeries(log(GBP), theme = chartTheme("white"))

# Let's beging with the analysis of both series. First, we'll see if the
# data is behaving normally. We'll try with the logs as well.
hist(na.omit(GBP))
jarque.bera.test(na.omit(GBP))

hist(na.omit(log(GBP)))
jarque.bera.test(na.omit(log(GBP)))

hist(EUR)
jarque.bera.test(na.omit(EUR))

hist(na.omit(log(EUR)))
jarque.bera.test(na.omit(log(EUR)))

# Neither of the two series are normal because we are rejecting
# the null hipothesis. Let's see about stationarity.
adf.test(na.omit(GBP))
adf.test(na.omit(log(GBP)))

adf.test(na.omit(EUR))
adf.test(na.omit(log(EUR)))

# Calculating the returns as the log differences
rGBP <- CalculateReturns(log(GBP))
rEUR <- CalculateReturns(log(EUR))

# Plotting the transformed series
chartSeries(rGBP, theme = chartTheme("white"))
chartSeries(rEUR, theme = chartTheme("white"))

# You can also plot both series at the same time.
returns = cbind(rEUR, rGBP)
plot(returns, col = c('green4', 'purple4'))

# The series look now more stationary. Let's check
# that and normality again.
hist(na.omit(rGBP))
jarque.bera.test(na.omit(rGBP))

hist(na.omit(rEUR))
jarque.bera.test(na.omit(rEUR))

adf.test(na.omit(rGBP))
adf.test(na.omit(rEUR))

# Series are not normal but they are now stationary.
# Autocorrelaction function and the partial autocorrelation function.
Acf(rGBP, lwd = 4, pch = 16, col = c('green4'))
Acf(rGBP, lwd = 4, pch = 16, col = c('purple4'), type = 'partial')

Acf(rGBP, lwd = 4, pch = 16, col = c('green4'))
Acf(rEUR, lwd = 4, pch = 16, col = c('purple4'), type = 'partial')

# The autocorrelation function tells you the correlation between points
# separated by various time lags.
# The partial autocorrelation at lag k is the correlation that results 
# after removing the effect of any correlations due to the terms at shorter 
# lags.

# They both are decreasing in a sinusoidal way, which means the both 
# process are ARMA(p,q), this is, a combination of an autorregressive
# and moving averages processes of order p and q respectively. However,
# this is not 100% certain.


# VAR model beacuse we have two variables, the euro-dollar exchange rate
# and the pound-dollar exchange rate. VAR (Vector Autorregressive Models) 
# are multivariate time series models. First, we will select the
# number of lags according to multiple criteria: Akaike, Schwarz, HQ, etc.
VARselect(na.omit(returns), lag.max=8,
          type="const")[["selection"]]

# If we take into account Akaike, the optimal number of lags in our model is 7.
var_reg <- VAR(na.omit(returns), p=7, type="const")
summary(var_reg)

# Most coefficient are not significant.
# It doesn't mean that the model is bad. Furthermore, the model is
# globally relevant. Let's test now if the model has serial correlation.
serial.test(var_reg, lags.pt=10, type="PT.asymptotic")

# Residuals of the model are nor serially correlated
# Let's check heterokedasticity.
arch.test(var_reg, lags.multi = 10, multivariate.only = TRUE)

# We reject the null hypotesis, and thus the model does suffer from heterokedasticity.

# Let's check about Granger Causality.
causality(var_reg, cause = 'rEUR')
causality(var_reg, cause = 'rGBP')

# It seems that the euro exchange rate does not cause the pound
# exchange rate, but that the pound does cause the euro This gives our model
# some credibility.

# Now, we will analyze the impulse response functions. These IRF will
# answer to the following question: which will happen to EUR/USD if the
# GBP/USD experiments a positive shock (and viceversa)?
irf_GBP <- irf(var_reg, impulse = 'rEUR', response = 'rGBP', 
               n.ahead = 20, boot = T)
plot(irf_GBP, ylab = 'GBP/USD', main = 'Shock from EUR/USD')

irf_EUR <- irf(var_reg, impulse = 'rGBP', response = 'rEUR', 
               n.ahead = 20, boot = T)
plot(irf_EUR, ylab = 'EUR/USD', main = 'Shock from GBP/USD')

# We see that if the EUR suffers a positive shock, then GBP goes up 
# but then it will stabilize in approximately 20 days. Something similar
# goes for the EUR if the GBP suffers a positive shock: it goes up, then
# it goes down and the stabilizes.

# Finally, we'll do the forecasting. Let's make a prediction for the next 100 days.
var_forecast <- predict(var_reg, n.ahead = 100, ci = 0.95)
fanchart(var_forecast, names = 'rGBP')
fanchart(var_forecast, names = 'rEUR')

# The predictions made by the time series models are not
# very revealing. In short, you can say that according to this model,
# EUR and GBP exchange rates will be stable over the next 100 days, or
# something like that. In any case, I don't think that the professor
# wants you to make a revealing discovering, but to make sure that 
# you know how to use tools for economic forecasting such as these.
