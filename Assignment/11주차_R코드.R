library(forecast)
library(ggplot2)

# 1번
# (a)Create  a  time  plot  of  the  differenced  series.

WalMartStock <- read.csv("C:/Users/ok762/Documents/DataScience/WalMartStock.csv", stringsAsFactors=FALSE)

WalMartStock = WalMartStock[, c("Date","Close")]
print(WalMartStock)

#일별 데이터인데 종가 데이터다 보니깐 주말이나 공휴일 등 휴장에 대한 날짜가 빠져있어서 frequency를 행의 개수로 해야함.
# start는 start=c(2001,2,5) 말고 c(2001)로 해야함.
WalMartStock_ts <- ts(WalMartStock$Close, start=c(2001), frequency=248)

yrange = (WalMartStock_ts)
plot(WalMartStock_ts, xlab = "Months", ylab = "Closed Prices", bty = "l", xaxt = "n", yaxt = "n", main = "Walmart Stock")
lines(WalMartStock_ts, bty = "l")

axis(1,at=seq(2001,2001+11/12,1/12), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
axis(2,at=seq(40,60,5),labels = format(seq(40.0,60.0,5)),las = 2)

# lag는 10까지 함
Acf(WalMartStock_ts, lag.max = 10)

Acf(diff(WalMartStock_ts), lag.max = 10)


plot(diff(WalMartStock_ts), main = "Time Plot of Differenced Series")


#(b)Which  of  the  following  is/are  relevant  for  testing  whether this  stock  is  a  random  walk?


# (c)Recreate the AR(1) model output for the Close price series shown in the left of Table 7.4. 
#   Does the AR model indicate that this is a random walk? Explain how you reached your conclusion.


fit <- Arima(WalMartStock_ts, order=c(1,0,0))
fit
fit_df <- Arima(diff(WalMartStock_ts), order=c(1,0,0))
fit_df

#(d) What  are  the  implications  of  ﬁnding  that  a  time  series  is  a random  walk?Choose  the  correct  statement(s)  below.

#============================================================================================================================
# 2번 Souvenir Sales data – ARIMA

# (a) Run  a  regression  model  with  log(Sales)  as  the  output variable  and  with  a  
#     linear  trend  and  monthly  predictors. Remember  to  ﬁt  only  the  training  period.  Use  this  model  to 
#     forecast  the  sales  in  February  2002. (선형회귀 모델)


souvenir <- read.csv("C:/Users/ok762/Documents/DataScience/SouvenirSales.csv")

souvenir_ts <-ts(souvenir$Sales, start =  c(1995, 1), end = c(2001, 12), freq = 12)
yrange = range(souvenir_ts)

plot(c(1995,2001),yrange,type="n",ylab="Sales",xlab="Year",bty="l",xaxt="n",yaxt="n")
lines(souvenir_ts,bty="l")

axis(1,at=seq(1995,2002,1),labels=format(seq(1995,2002,1)))
axis(2,at=seq(0,110000,10000),labels=format(seq(0,110,10)),las=2)


nValid <- 12
nTrain <- length(souvenir_ts) - nValid

souvenir_train <- window(souvenir_ts,end=c(1995,nTrain))
souvenir_valid <- window(souvenir_ts,start=c(1995,nTrain+1))

# log를 취해줍니다.
souvenir_Log <- tslm(log(souvenir_train) ~ trend + season)
summary(souvenir_Log)

print(souvenir_Log)
# 2002년 2월을 예측하기 위해서 souvenir_Log를 이용해서 계산해 줍니다.
souvenir_Log_Forecast <- forecast(souvenir_Log,h=nValid)
feb2002Forecast <- souvenir_Log$coefficients["(Intercept)"] + souvenir_Log$coefficients["trend"]*86 + souvenir_Log$coefficients["season2"]
exp(feb2002Forecast)


# (b)Create an ACF plot until lag-15 for the forecast errors. Now ﬁt an AR model with lag-2 [ARIMA(2, 0, 0)] to the forecast errors.

# lag를 15까지 해서 만들기기
sovenir_ACF <- Acf(souvenir_Log$residuals,lag.max=15)
sovenir_ACF
# ARIMA 모델 만들기
Arima_Model <- Arima(souvenir_Log$residuals,order=c(2,0,0))
Arima_Model

# forecast 함수를 이용해서 예측
logregre_Forecast <- forecast(souvenir_Log, h=nValid)
logregre_Forecast

erForecast <- forecast(Arima_Model, h=nValid)
erForecast

Forecast_exp <- exp(logregre_Forecast$mean) + exp(erForecast$mean)
Forecast_exp


#============================================================================================================================
# 3번 Walmart Sales data – 외부 변수 관련

#(a)
WalMartStore<- read.csv("C:/Users/ok762/Documents/DataScience/WalmartStore1Dept72.csv")

print(WalMartStore)

WalMartStore_weekly = WalMartStore[, c("Date","Weekly_Sales")]

WalMartStore_ts <- ts(WalMartStore_weekly$Weekly_Sales, start=c(2010), frequency=52)

yrange = (WalMartStore_ts)
plot(WalMartStore_ts, xlab = "Year", ylab = "Weekly_Sales", bty = "l", xaxt = "n", yaxt = "n", main = "Walmart Stock")
lines(WalMartStore_ts, bty = "l")


axis(1,at=seq(2010,2013), labels=c("2010","2011","2012","2013"))
axis(2,at=seq(10000,200000,50000),labels = format(seq(10000.0,200000.0,50000)),las = 2)
# (b)

WalMartStore_re = WalMartStore[, c("Weekly_Sales","Temperature","Fuel_Price","CPI","Unemployment")]

cor(WalMartStore_re)

plot(WalMartStore_re, main = "Scatter plot Matrix")


#(e)



nTrain <- 91
nValid <- 52
print(nTrain)


train.ts <- window(WalMartStore_ts, start = c(2010), end = c(2010, nTrain))
valid.ts <- window(WalMartStore_ts, start = c(2010, nTrain + 1), end = c(2010, nTrain + nValid))

WalMartStore.lm <- tslm(train.ts ~ trend + I(trend^2))
WalMartStore.lm.pred <- forecast(WalMartStore.lm, h = nValid, level = 0,method="naive")


plot(WalMartStore.lm.pred, ylim = c(10000,200000), ylab = "Weekly_Sales",xlab = "Year",bty = "l",
     xaxt = "n", xlim = c(2010,2013),main = "", flty = 2)
axis(1, at = seq(1995,2002,1), labels = format(seq(1995,2002,1)))
lines(WalMartStore.lm $fitted,lwd = 2)
lines(valid.ts)

#forecast의 accuracy 함수를 이용하면 RMSE와 MAPE를 구할 수 있다. 
accuracy(WalMartStore.lm)


# (f)

WalMartStore_IsHoliday = WalMartStore[, c("Date","Weekly_Sales","IsHoliday")]


# 수치형으로 바꿔주기기
WalMart_IsHoliday<-transform(WalMartStore_IsHoliday,
                             IsHoliday = ifelse(IsHoliday =="TRUE", 1, 0))

WalMartStoreIsHoliday_ts <- ts(WalMart_IsHoliday$Weekly_Sales, start=c(2010), frequency=52)

nTrain <- 91
nValid <- 52
print(nTrain)


train.ts <- window(WalMartStoreIsHoliday_ts, start = c(2010), end = c(2010, nTrain))
valid.ts <- window(WalMartStoreIsHoliday_ts, start = c(2010, nTrain + 1), end = c(2010, nTrain + nValid))

WalMartStore.lm <- tslm(train.ts ~ trend + I(trend^2))
WalMartStore.lm.pred <- forecast(WalMartStore.lm, h = nValid, level = 0,method="naive")


plot(WalMartStore.lm.pred, ylim = c(10000,200000), ylab = "Weekly_Sales",xlab = "Year",bty = "l",
     xaxt = "n", xlim = c(2010,2013),main = "", flty = 2)
axis(1, at = seq(1995,2002,1), labels = format(seq(1995,2002,1)))
lines(WalMartStore.lm $fitted,lwd = 2)
lines(valid.ts)
#forecast의 accuracy 함수를 이용하면 RMSE와 MAPE를 구할 수 있다. 
accuracy(WalMartStore.lm)
