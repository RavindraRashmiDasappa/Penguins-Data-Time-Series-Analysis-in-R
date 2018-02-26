#Reading Data
data <- read.csv(file="/Users/rahmi/Desktop/Penguins/_penguin.csv", stringsAsFactors = FALSE)

#Viewing the data
#View(data)

#checking the type of data
class(data)

#Converting dataframe into time series data
library(tidyr)
data %>% separate(Month, c("Year", "Month"), "M")
data <- subset(data, select = -Month )
print(data)

myts <- ts(data, start=c(2000, 01), end=c(2006, 12), frequency=12) 
print(myts)

#This tells you that the data series is in a time series format
start(myts)
end(myts)
frequency(myts)

#summary of data
summary(myts)

#The number of penguins distributed across the spectrum
plot(myts)
abline(reg=lm(myts~time(myts)))
#Trends observed
#1. The count of penguins and variance is decreasing

#To print the cycle across years
cycle(myts)

#This will aggregate the cycles and display a year on year trend
plot(aggregate(myts,FUN=mean))

#Box plot across months will give us a sense on seasonal effect
boxplot(myts~cycle(myts))

#Trends Observed
#1. The number of penguins trend varied across every month
#2. Variance and mean are varying 
#3. Mean is high in the month of December 
#4. There is a sudden decrease in the month of June that may be due to irregular environmental
#issues or the penguins migrate to the other place during those months.

#Before performing Augmented Dickey-Fuller Test to see if the data is stationarized
install.packages('tseries') 
library(tseries)
adf.test(diff(log(myts)), alternative=c("stationary","explosive"), k=0)

#plotting  "q"
acf(diff(log(myts)))

#plotting "p"
pacf(diff(log(myts)))
#Here, the partial auto correlation function returns a negative tail. 
#This is suggestive that the time series data needs extra differencing. 
#You can continue on to do things like difference the mean, use a Box-Cox Transformation, 
#or any number of other methods, but here I will choose to use the auto.arima function to aid in the forecasting.
#This function comes from the forecast package so we will need to call its library.

library(forecast)
auto.arima(myts)

fit <- arima(log(myts), c(2, 1, 0),seasonal = list(order = c(0, 1, 1), period = 12))
pred <- predict(fit,n.ahead=10*12)
pred1 <- 2.718^pred$pred
ts.plot(myts,2.718^pred$pred, log = "y", lty = c(1,3))

#Testing
datawide <- ts(data,frequency = 12,start =c(2000, 01), end=c(2005, 12) )
fit <- arima(log(datawide), c(2, 1, 0),seasonal = list(order = c(0, 1, 1), period = 12))
pred <- predict(fit,n.ahead=10*12)
pred1 <- 2.718^pred$pred

data1<- head(pred1,12)
predicted_2006<- round(data1,digits=0)

original_2006 <- tail(myts,12)

ts.plot(myts,2.718^pred$pred, log = "y", lty = c(1,3))
