library(forecast)
setwd("C:/Users/tempt/Desktop/MSBA/Spring2022/BAN673/BB/Case_study/Case_study3")
walmart.data<-read.csv("673_case2.csv")
head(walmart.data)
tail(walmart.data)
#Create time series data set
#Develop data partition with the validation partition of 16 periods and the rest for the training partition.
walmart.ts<-ts(walmart.data$Revenue,start=c(2005,1),end=c(2021,3),freq=4)
nValid <- 16 
nTrain <- length(walmart.ts) - nValid
train.ts <- window(walmart.ts, start = c(2005, 1), end = c(2005, nTrain))
valid.ts <- window(walmart.ts, start = c(2005, nTrain + 1), 
                   end = c(2005, nTrain + nValid))

                 #1. Identify time series predictability.
#a. Using the AR(1) model for the historical data, Provide and explain the AR(1) model

walmart.ar1<- Arima(walmart.ts, order = c(1,0,0))
summary(walmart.ar1)

ar1<-0.8862
s.e.<-0.0626
null_mean<-1
alpha<-0.05
z.stat<-(ar1-null_mean)/s.e.
z.stat
p.value<-pnorm(z.stat)
p.value
if (p.value<alpha){ 
  "Reject null hyothesis"
}else{
"Accept null hypothesis"
}

#b Using the first differencing (lag1) of the historical data and Acf() function, provide in the 
#report the autocorrelation plot of the first differencing (lag1) with the maximum of 8 lags

diff.walmart.revenue<-diff(walmart.ts,lag=1)
diff.walmart.revenue
Acf(diff.walmart.revenue,lag.max=8,main="Autocorrelation for walmart revenue")

    #2. Apply the two-level forecast with regression model and AR model for residuals

#a)develop a regression model with quadratic trend and seasonality
train.trend.season<-tslm(train.ts ~ trend + I(trend^2) + season)
summary(train.trend.season)
train.trend.season.pred <- forecast(train.trend.season, h = nValid, level = 0)
train.trend.season.pred

#b) Identify the regression model's residuals for the training period &
#use the Acf() function with the maximum of 8 lags

Acf(train.trend.season.pred$residuals, lag.max = 8, 
    main = "Autocorrelation for Walmart revenue Training Residuals")
Acf(valid.ts - train.trend.season.pred$mean, lag.max = 8, 
    main = "Autocorrelation for Walmart revenue Validation Residuals")

#c)Develop an AR(1) model for the regression residuals
res.ar1 <- Arima(train.trend.season$residuals, order = c(1,0,0))
summary(res.ar1)
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred
# Use the Acf() function for the residuals of the AR(1) model
Acf(res.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Walmart revenue Training Residuals of Residuals")
Acf(valid.ts-res.ar1.pred$mean, lag.max = 8, 
    main = "Autocorrelation for Walmart revenue Validation Residuals of Residuals")

#d)Create a two-level forecasting model (regression model with quadratic trend and 
#seasonality + AR(1) model for residuals) for the validation period
valid.two.level.pred <- train.trend.season.pred$mean + res.ar1.pred$mean

valid.df <- data.frame(valid.ts, train.trend.season.pred$mean, 
                       res.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Walmart revenue", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df

#e) Develop a two-level forecast (regression model with quadratic trend and seasonality & 
#AR(1) model for residuals) for the entire data set 
tot.trend.season <- tslm(walmart.ts ~ trend + I(trend^2) + season)
tot.trend.season.pred <- forecast(tot.trend.season, h = 5, level = 0)
tot.residual.ar1 <- Arima(tot.trend.season$residuals, order = c(1,0,0))
tot.residual.ar1.pred <- forecast(tot.residual.ar1, h = 5, level = 0)
Acf(tot.residual.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Walmart revenue Residuals of Residuals for Entire Data Set")
tot.trend.season.ar1.pred <- tot.trend.season.pred$mean + tot.residual.ar1.pred$mean
#create table
table.df <- data.frame(tot.trend.season.pred$mean, 
                       tot.residual.ar1.pred$mean, tot.trend.season.ar1.pred)
names(table.df) <- c("Reg.Forecast", "AR(1).residual.Forecast","Combined.Forecast")
table.df

        #3. Use ARIMA Model and Compare Various Methods.

#a) Use Arima() function to fit ARIMA(1,1,1)(1,1,1) model for the training data set
train.arima.seas <- Arima(train.ts, order = c(1,1,1), 
                          seasonal = c(1,1,1)) 
summary(train.arima.seas)
train.arima.seas.pred <- forecast(train.arima.seas, h = nValid, level = 0)
train.arima.seas.pred

#b) Use the auto.arima() function to develop an ARIMA model using the training data set
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)
# use this model to forecast revenue in the validation period
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

#c) compare performance measures of the two ARIMA models in 3a and 3b
round(accuracy(train.arima.seas.pred, valid.ts), 3)
round(accuracy(train.auto.arima.pred, valid.ts), 3)
#ARIMA(1,1,1)(1,1,1) has better accuracy compared to auto arima model

#d)Use two ARIMA models from 3a and 3b for the entire data set & forecast for future
tot.arima.seas <- Arima(walmart.ts, order = c(1,1,1), 
                    seasonal = c(1,1,1))
summary(tot.arima.seas)
tot.arima.seas.pred <- forecast(tot.arima.seas, h = 5, level = 0)

tot.auto.arima <- auto.arima(walmart.ts)
summary(tot.auto.arima)
tot.auto.arima.pred <- forecast(tot.auto.arima, h = 5, level = 0)
#create table
qtr<-c('2021-Q4','2022-Q1','2022-Q2','2022-Q3','2022-Q4')
table.df <- data.frame(qtr,tot.arima.seas.pred$mean,tot.auto.arima.pred$mean)
names(table.df) <- c("Quarter","ARIMA.Forecast", "Auto.ARIMA.Forecast")
table.df

#e)compare performance measures of the forecasting models for the entire data set

round(accuracy(tot.trend.season.pred$fitted, walmart.ts), 3) #regression model with quadratic trend & seasonality
round(accuracy((tot.trend.season.pred$fitted+tot.residual.ar1$fitted), walmart.ts), 3) # two-level model (with AR(1) model for residuals)
round(accuracy(tot.arima.seas.pred$fitted, walmart.ts), 3)#ARIMA(1,1,1)(1,1,1)model
round(accuracy(tot.auto.arima.pred$fitted, walmart.ts), 3)#auto ARIMA model
round(accuracy((snaive(walmart.ts))$fitted,walmart.ts),3) #seasonal naive model

# plot 2 best models
plot(walmart.ts, 
     xlab = "Yr-Qtr", ylab = "Revenue", ylim = c(73800, 147250), 
     xaxt = "n",  xlim=c(2005,2021.75),cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,
     main = "Walmart revenue", lwd = 2) 
lines((tot.trend.season.pred$fitted+tot.residual.ar1$fitted), col = "blue")
lines(tot.arima.seas.pred$fitted, col = "red", lwd = 2)
legend("bottomright", legend=c('Actual data','Two level model','Arima'), col=c('black','blue','red'),lty = 1:1,lwd = 3,text.font=2,box.lty=0,cex=1)
