library("Quandl")
Quandl.auth("TOKEN")
Flu_dataset<-Quandl("GOOGLEORG/FLUCOUNTRY", trim_start="2002-12-29", trim_end="2014-07-27", collapse="weekly")
help(as.Date)	
Flu_dataset$Date = as.Date(Flu_dataset$Date, "%m/%d/%Y")
plot(Flu_dataset$Canada ~ Flu_dataset$Date, main="Flu Trends Compared", xlab='Time', ylab='Cases / Week', type='l', col='blue')
lines(Flu_dataset$'South Africa' ~ Flu_dataset$Date, xlab='Time', ylab='Cases / Week', col='green')
lines (Flu_dataset$Austria ~ Flu_dataset$Date, xlab='Time', ylab='Cases / Week', type='l', col='red') 
legend('topleft', c("Canada","Austria", "South Africa"), lty=1, col=c("blue", "red", "green"), bty='l', cex=1.25, box.lwd = 1.2, box.col = "black")

#Since true equals 1 the "row" of the dataframe is summed with true false variables based on if the variable is null.  If it is then it is counted.
missingCount = apply(Flu_dataset, MARGIN=1, function(x){return (sum(is.na(x)))})
cleanedFluData = Flu_dataset[Flu_dataset$Date > as.Date('2005-12-31'),]
cleanedFluData$World = rowMeans(cleanedFluData[,-which(names(cleanedFluData) == "Date")], na.rm=TRUE)
plot(cleanedFluData$World ~ cleanedFluData$Date, main="Aggregated Flu Trends",xlab='Time', ylab='Cases / Week', type='l', col='blue')
#Splits plots into a 4 row two column matrix
par(mfrow=c(4, 2))
plot.new()
#weekly differencing of the average value computed
cleanedFluData$diff_52 = c(diff(cleanedFluData$World, lag=52), rep(0,52))
plot (cleanedFluData$diff_52 ~ cleanedFluData$Date, main="Once Differenced Flu Data lag=52", xlab='Time', ylab='Cases / Week', type='l', col='brown')
acf(cleanedFluData$diff_52, lag.max = 160, main="ACF for lag=(52)", col='brown')
cleanedFluData$diff_52.1 = c(diff(diff(cleanedFluData$World, lag=52)), rep(0,53))
plot (cleanedFluData$diff_52.1 ~ cleanedFluData$Date, main="Twice Differenced Flu Data lag=(52,1)", xlab='Time', ylab='Cases / Week', type='l', col='gray')
acf(cleanedFluData$diff_52.1, lag.max = 160, main="ACF for lag=(51, 1)", col='gray')
cleanedFluData$diff_1 = c(diff(cleanedFluData$World), rep(0,1))
plot (cleanedFluData$diff_1 ~ cleanedFluData$Date, main="Once Differenced Flu Data lag=1", xlab='Time', ylab='Cases / Week', type='l', col='orange')
cleanedFluData$diff_1.1 = c(diff(diff(cleanedFluData$World)), rep(0,2))
plot (cleanedFluData$diff_1.1 ~ cleanedFluData$Date, main="Twice Differenced Flu Data lag=(1,1)", xlab='Time', ylab='Cases / Week', type='l', col='purple')
plot.new()
par(mfrow=c(2, 1)) #set to 2-by-1
acf(cleanedFluData$diff_1.1, lag.max = 160, main="ACF Lag=(1,1)")
pacf(cleanedFluData$diff_1.1, lag.max = 160, main="PACF (Partial ACF) Lag=(1,1)")
par(mfrow=c(1, 1))
plot.new()
flu_arima = arima(cleanedFluData$World, 
            seasonal = list(order = c(0, 2, 2), period = 52), 
            order = c(1,0,0), method="CSS-ML")
ahead=104
flu_fcast = predict(flu_arima, n.ahead= ahead)