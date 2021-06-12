data = read.table("C:/Users/Taha/Desktop/SEM6/Adv DA/Arima_airmiles_dataset.csv",header = T,sep = ",")
str(data)

#Data to time series
library(stats)
data_ts=ts(data, frequency = 12, start = c(1996,1))
data_ts

#Plotting time series
par(mfrow=c(1,1))
plot(data_ts)

#Decomposing time series into trend,seasonality and randomness assuming additive model
data_ts_components= decompose(data_ts, type = "additive")
plot(data_ts_components)

#ACF and PACF of data
par(mfrow=c(1,1))
plot.ts(data_ts)
acf(data_ts, lag.max=20)
pacf(data_ts, lag.max=20)


#ACF and PACF plots show trend and seasonility
#Performing differences to make the cure stationary
#calculating "d "for Trend
par(mfrow = c(1, 1))
data_ts_diff1 =diff(data_ts, differences=1)
plot(data_ts_diff1)

#Checking ACF and PACF for after differencing for above values 
par(mfrow=c(1,2))
acf(data_ts_diff1, lag.max=20)
pacf(data_ts_diff1, lag.max=20)

#Performing seasonal differencing and checking for value of D
par(mfrow = c(1, 1))
data_ts_seas_diff1=diff(data_ts, lag = 12, differences=1)
plot(data_ts_seas_diff1)

#Checking ACF and PACF for seasonally differenced data
par(mfrow = c(1, 2))
acf(data_ts_seas_diff1)
pacf(data_ts_seas_diff1)

#Calculating ndiffs and nsdiffs using forecast
#install.packages("forecast")
library("forecast")
ndiffs(data_ts)
nsdiffs(data_ts)

#........Differencing seasonally differenced data again.......#
par(mfrow = c(1, 1))
seas_diff=diff(data_ts_seas_diff1, differences=1)
plot(seas_diff)
par(mfrow = c(1, 2))
acf(seas_diff)
pacf(seas_diff)

#Implementing ARIMA model
model= Arima(data_ts, order = c(1,1,1),seasonal = c(0,1,1), include.drift = FALSE)

# Checking residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(model$residuals, lag.max = 24)
pacf(model$residuals, lag.max = 24)
Box.test(model$residuals, lag=24, type="Ljung-Box")

#Forecasting using Arima model
par(mfrow = c(1, 1))
data_ts_forecast=forecast(model,h= 36)
plot(data_ts_forecast)