library(forecast)
setwd("/Users/jasonchen/Desktop/Jason_Purdue/Courses/22Spring/Module3/SASOptimization")
water.data <- read.csv("water.csv")
str(water.data)
cooling <- water.data[1:93,]
main <- water.data[94:nrow(water.data),]
names(cooling)[3] <- "Cgallons"
names(main)[3] <- "Mgallons"

weekgallonsraw <- cbind(cooling,main)[,c(2,3,6)] %>%
  summarise(weekgallons = Cgallons+Mgallons)

weekgallons.data<- cbind(cooling,weekgallonsdata)[c(2,4)]

weekgallons.ts <- ts(weekgallons.data$weekgallons,
                       start = c(2020, 13), end = c(2022, 1), freq = 52)

plot(weekgallons.ts, ylab="Weekly gallon usage ",main="Weekly Gallon Usage Time Series",xlab="date")
Acf(weekgallons.ts,lag.max=52, main='Shipments of Household Appliances ACF')
nValid <- 10                               # set holdout size
nTrain <- length(weekgallons.ts) - nValid    # set training size
train.ts <- window(weekgallons.ts, start = c(2020, 13), end = c(2020, 12+nTrain)) #partion data
valid.ts <- window(weekgallons.ts, start = c(2020, 12 + nTrain + 1), end = c(2020, 12 + nTrain + nValid))

options(scipen = 1)        # change output numbers to non-scientific format
#train model
train.lms <- tslm(train.ts ~ trend + season)
train.lm <- tslm(train.ts ~ trend)
train.qms <- tslm(train.ts ~ trend +I(trend^2)+ season)
train.qm <- tslm(train.ts ~ trend +I(trend^2))
train.explms <- tslm(train.ts ~ trend + season,lamda=0)
train.explm <- tslm(log(train.ts) ~ trend )
train.explms <- tslm(log(train.ts) ~ trend + season)
train.explm <- tslm(train.ts ~ trend,lamda=0)
summary(train.lms)
summary(train.lm)
summary(train.qms)
summary(train.qm)
summary(train.explms)
summary(train.expqms)
#forecast test set
valid.lm<-forecast(train.lm, h=nValid)
valid.qm<-forecast(train.qm, h=nValid)
valid.explm<-forecast(train.explm, h=nValid)
valid.lms<-forecast(train.lms, h=nValid)
valid.qms<-forecast(train.qms, h=nValid)
valid.explms<-forecast(train.explms, h=nValid)
valid.expqms<-forecast(train.expqms, h=nValid)
#lm
plot(valid.lm,ylab="Weekly gallon usage ",main="Weekly gallon usage w/ Linear trend model",xlab="date")
lines(valid.lm$fitted, lwd = 2, col = "blue")
lines(valid.ts,lty=2)
legend("bottomleft", c("Weekly gallon usage on training set", "Weekly gallon usage on Validation set", 
                       "Linear trend model on training set", "Linear trend model prediction"),
       col=c("black", "black", "blue","deepskyblue"),lty=c(1,2,1,1), lwd=c(1,1,2,1), bty = "n", cex=0.8)

#qm
plot(valid.qm,ylab="Weekly gallon usage ",main="Weekly gallon usage w/ Quadratic trend model",xlab="date")
lines(valid.qm$fitted, lwd = 2, col = "blue")
lines(valid.ts,lty=2)
legend("bottomleft", c("Weekly gallon usage on training set", "Weekly gallon usage on Validation set", 
                       "Quadratic trend model on training set", "Quadratic trend model prediction"),
       col=c("black", "black", "blue","deepskyblue"),lty=c(1,2,1,1), lwd=c(1,1,2,1), bty = "n", cex=0.8)
#explm
plot(valid.explm,ylab="Weekly gallon usage ",main="Weekly gallon usage w/ Exponential trend model",xlab="date")
lines(valid.explm$fitted, lwd = 2, col = "blue")
lines(valid.ts,lty=2)
legend("bottomleft", c("Weekly gallon usage on training set", "Weekly gallon usage on Validation set", 
                       "Exponential trend model on training set", "Exponential trend model prediction"),
       col=c("black", "black", "blue","deepskyblue"),lty=c(1,2,1,1), lwd=c(1,1,2,1), bty = "n", cex=0.8)
#lms
plot(valid.lms,ylab="Weekly gallon usage ",main="Weekly gallon usage w/ Linear trend model with seasonality",xlab="date")
lines(valid.lms$fitted, lwd = 2, col = "blue")
lines(valid.ts,lty=2)
legend("bottomleft", c("Weekly gallon usage on training set", "Weekly gallon usage on Validation set", 
                       "Linear trend model with seasonality on training set", "Linear trend model with seasonality prediction"),
       col=c("black", "black", "blue","deepskyblue"),lty=c(1,2,1,1), lwd=c(1,1,2,1), bty = "n", cex=0.8)
#qms
plot(valid.qms,ylab="Weekly gallon usage ",main="Weekly gallon usage w/ Quadratic trend model with seasonality",xlab="date")
lines(valid.qms$fitted, lwd = 2, col = "blue")
lines(valid.ts,lty=2)
legend("bottomleft", c("Weekly gallon usage on training set", "Weekly gallon usage on Validation set", 
                       "Quadratic trend model with seasonality on training set", "Quadratic trend model with seasonality prediction"),
       col=c("black", "black", "blue","deepskyblue"),lty=c(1,2,1,1), lwd=c(1,1,2,1), bty = "n", cex=0.8)
#explms
plot(valid.explms,ylab="Weekly gallon usage ",main="Weekly gallon usage w/ Exponential trend model with seasonality",xlab="date")
lines(valid.explms$fitted, lwd = 2, col = "blue")
lines(valid.ts,lty=2)
legend("bottomleft", c("Weekly gallon usage on training set", "Weekly gallon usage on Validation set", 
                       "Exponential trend model with seasonality on training set", "Exponential trend model with seasonality prediction"),
       col=c("black", "black", "blue","deepskyblue"),lty=c(1,2,1,1), lwd=c(1,1,2,1), bty = "n", cex=0.8)
#ARIMA211
plot(arimapredict,ylab="Weekly gallon usage ",main="Weekly gallon usage w/ ARIMA211",xlab="date")
lines(arimapredict$fitted, lwd = 2, col = "blue")
lines(valid.ts,lty=2)
legend("bottomleft", c("Weekly gallon usage on training set", "Weekly gallon usage on Validation set", 
                       "ARIMA211 on training set", "ARIMA211 prediction"),
       col=c("black", "black", "blue","deepskyblue"),lty=c(1,2,1,1), lwd=c(1,1,2,1), bty = "n", cex=0.8)
#tbatspredict
plot(tbatspredict,ylab="Weekly gallon usage ",main="Weekly gallon usage w/ Exponential smoothing with seasonality TBATS model",xlab="date")
lines(tbatspredict$fitted, lwd = 2, col = "blue")
lines(valid.ts,lty=2)
legend("bottomleft", c("Weekly gallon usage on training set", "Weekly gallon usage on Validation set", 
                       "Exponential smoothing with seasonality TBATS model on training set", "Exponential smoothing with seasonality TBATS model prediction"),
       col=c("black", "black", "blue","deepskyblue"),lty=c(1,2,1,1), lwd=c(1,1,2,1), bty = "n", cex=0.8)

plot(valid.explms,ylab="Water",main="Water w/ LMS exp")
lines(valid.explms$fitted, lwd = 2, col = "blue")
lines(valid.ts)
legend(0,2020.5, c("Shipments on training set", "Shipments on Validation set", "Centered Moving Average on training set", "Trailing Moving Average on training set"),
       col=c("black", "black", "dark green","blue"),lty=c(1,2,1,1), lwd=c(1,1,2,1), bty = "n", cex=1.2)
plot(valid.expqms,ylab="Water",main="Water w/ QMS exp")
lines(valid.lms$fitted, lwd = 2, col = "blue")
lines(valid.ts)
plot(valid.lms,ylab="Weekly gallon usage ",main="Weekly gallon usage w/ LMS",xlab="date")
lines(valid.lms$fitted, lwd = 2, col = "blue")
lines(valid.ts,lty=2)
legend("bottomleft", c("Weekly gallon usage on training set", "Weekly gallon usage on Validation set", 
                       "Linear trend model on training set", "Linear trend model prediction"),
       col=c("black", "black", "dark green","deepskyblue"),lty=c(1,2,1,1), lwd=c(1,1,2,1), bty = "n", cex=0.8)
plot(valid.qms,ylab="Water",main="Water w/ QMS")
lines(valid.lms$fitted, lwd = 2, col = "blue")
lines(valid.ts)
# derive forecast accuracies on holdout 
accuracy(valid.lm,valid.ts)
accuracy(valid.qm,valid.ts)
accuracy(valid.lms,valid.ts)
accuracy(valid.qms,valid.ts)
accuracy(valid.explm,valid.ts)
accuracy(valid.explms,valid.ts)
accuracy(valid.expqms,valid.ts)

#tbats model
xx <- tbats(train.ts)
tbatspredict <- forecast(xx, h = nValid)

accuracy(tbatspredict,valid.ts)
plot(tbatspredict,ylab="Water",main="Water w/ tbat")
lines(tbatspredict$fitted, lwd = 2, col = "blue") 
lines(valid.ts)
watertbat <- tbats(weekgallons.ts)
watertbattbatspredict <- forecast(watertbat, h = 4)

#arima model
arima_optimal <-auto.arima(train.ts,ic="aic",trace=TRUE)
arimapredict <- forecast(arima_optimal,h=10)
plot(arimapredict,ylab="Water",main="Water w/ arima211")
lines(arimapredict$fitted, lwd = 2, col = "blue") 
lines(valid.ts)
accuracy(arimapredict,valid.ts)
pacf(ts(arima_optimal$residuals))

train.arima1 <-Arima(train.ts, order=c(0,2,2))
arimapredict1 <- forecast(train.arima1,h=10)
accuracy(arimapredict1,valid.ts)
plot(arimapredict1,ylab="Water",main="Water w/ arima022")
lines(arimapredict1$fitted, lwd = 2, col = "blue")
lines(valid.ts)
train.arima2 <- Arima(weekgallons.ts, order=c(0,2,2))
arimapredict2 <- forecast(train.arima2,h=4)
accuracy(arimapredict1,valid.ts)




