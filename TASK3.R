#Install required Packages
install.packages(c("TTR","forecast","tseries"))
#Loading Libraries
lapply(c("TTR","forecast","tseries","e1071"),library,character.only = TRUE)
#Importing file for time series analysis
apple<-read.csv("/Users/bharath/Desktop/R_Studio/Task 3 - Time Series Modelling/Share Price/stocks/AAPL.CSV",header=TRUE,stringsAsFactors = FALSE)
#Initial data exploration
str(apple)
#Check for null values
colSums(is.na(apple))
#Descriptive stats of data
summary(apple)
#First 10 rows of data
head(apple,10)
#Last 10 rows of data
tail(apple,10)
#We use 252 as freq because stock markets typically don't operate on weekends or holidays.
tsclose <- ts(apple$Close, start = c(1980,12),frequency=252)
plot(tsclose, main = "Time Series of Close Prices", xlab = "Time", ylab = "Close Price")
#Decomposing the time series object
tsclosecomponents<-decompose(tsclose)
#Plotting the Decomposed Time series Object
plot(tsclosecomponents)
#Log Transformed Holt-Winters Additive Model
logtsclose <- log(tsclose)
logtscloseforecast <- HoltWinters(logtsclose)
logtscloseforecast
#Plotting Observed vs Fitted Values
plot(logtscloseforecast)
#Forecasting for next 5 years
logtsforecasts2<-forecast(logtscloseforecast,h=1260)
#Plotting for forecasted values
plot(logtsforecasts2)
#Ljung Box Test
Box.test(logtsforecasts2$residuals, lag=20, type="Ljung-Box")
#Time series plot
plot.ts(logtsforecasts2$residuals)
#Function for plotting Histogram for residuals of time series models.
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
#Remove any null values if present.
logtsforecasts2$residuals <- logtsforecasts2$residuals[!is.na(logtsforecasts2$residuals)]
#Histogram Plot for forecast errors
plotForecastErrors(logtsforecasts2$residuals) 
#Mean of residuals
mean(logtsforecasts2$residuals)
#Differencing to make the time series stationary
tsclose1 <- diff(tsclose, differences=1)
plot(tsclose1)
#Augmented dickey fuller test to check for stationary
adf.test(tsclose1)
## Plot a correlogram
acf(tsclose1, lag.max=20) 
acf(tsclose1, lag.max=20, plot=FALSE)
# plot a partial correlogram
pacf(tsclose1, lag.max=20) 
# get the partial autocorrelation
pacf(tsclose1, lag.max=20, plot=FALSE) 
# Fit an ARIMA model
tsclosearima1 <- arima(tsclose,order=c(1,1,1))
tsclosearima1
#Plotting forecast values in ARIMA model
plot(tsclosearima1)
# Extract residuals
residuals <- residuals(tsclosearima1)
# Ljung-Box test
Box.test(residuals, type="Ljung-Box")
# Generate ARIMA forecast for next 5 years (1260 trading days)
arima_forecast <- forecast(tsclosearima1, h = 1260)
# Plot the ARIMA forecast
plot(arima_forecast, main = "ARIMA Forecast", xlab = "Time", ylab = "Close Price")
#Remove any null values if present.
arima_forecast$residuals <- arima_forecast$residuals[!is.na(arima_forecast$residuals)]
#Histogram Plot for forecast errors
plotForecastErrors(arima_forecast$residuals) 
skewness(arima_forecast$residuals)
# Apply Exponential Smoothing State Space Model (ETS) ignoring Seasonality
ets_model <- ets(tsclose)
# Summary of the fitted ETS model
summary(ets_model)
# Forecasting the next 5 years (1260 periods)
ets_forecast <- forecast(ets_model, h = 1260)
plot(ets_forecast, main = "ETS Forecast", xlab = "Time", ylab = "Close Price")
ets_forecast$residuals <- ets_forecast$residuals[!is.na(ets_forecast$residuals)]
plotForecastErrors(ets_forecast$residuals)
mean(ets_forecast$residuals)
# Ljung-Box test
Box.test(ets_forecast$residuals, type="Ljung-Box")






