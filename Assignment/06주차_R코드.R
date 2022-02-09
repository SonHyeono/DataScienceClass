#if Korean is broken -> Rstudio => File -> click Reopen with Encoding.. and change UTF-8

library(knitr)
library(forecast)

#데이터 불러오기 
data <- read.csv("DepartmentStoreSales.csv",encoding="utf-8")

#시계열 데이터 생성 
dataTS <- ts(data$Sales, start=c(1,1), frequency=4)

#평가 기간  = 4 (마지막 4분기)
validLength <- 4
#train 기간 (마지막 4분기를 제외한 나머지 기간)
trainLength <- length(dataTS) - validLength

#훈련용 데이터와 평가용 데이터 분리 
Train <- window(dataTS, end=c(1, trainLength))
Valid <- window(dataTS, start=c(1, trainLength+1))

#훈련용 데이터로 모델 생성 
#trend, season 요소 사용 
modelExpo <- tslm(Train ~ trend + season, lambda=0)
summary(modelExpo)

# plot 부분 
yrange=range(dataTS)
plot(c(1, 20), yrange, type="n", xlab="분기", ylab="판매량", bty="l", xaxt="n", yaxt="n")
lines(as.vector(dataTS), bty="l")
axis(1, at=seq(1,20,1),labels=format(seq(1,20,1)))
axis(2, at=seq(45000,110000,5000), labels=format(seq(45,110,5)), las=2)
lines(as.vector(modelExpo$fitted), col="red")
legend("topleft", c("실제 데이터","모델 적용"), lty=c(1,1), col=c("black","red"), bty="n")


#평가 부분 
Forecasts <- forecast(modelExpo, h=validLength)
Forecasts

#plot 부분 
yrange=range(dataTS)
plot(c(1, 10), yrange, type="n", xlab="분기", ylab="판매량", bty="l", xaxt="n", yaxt="n")
title("판매량 예측")
lines(dataTS, bty="l")
axis(1, at=seq(1,10,1),labels=format(seq(1,10,1)))
axis(2, at=seq(45000,110000,5000), labels=format(seq(45,110,5)), las=2)
lines(modelExpo$fitted, col="red")
lines(Forecasts$mean, col="blue", lty=2)
legend("topleft", c("실제 데이터", "모델 적용", "예측값"), lty=c(1,1,2), col=c("black","red","blue"), bty="n")

#적합도 plot 
yrange=range(dataTS)
plot(c(1, 20), yrange, type="n", xlab="분기", ylab="판매량", bty="l", xaxt="n", yaxt="n")
lines(as.vector(dataTS), bty="l")
axis(1, at=seq(1,20,1),labels=format(seq(1,20,1)))
axis(2, at=seq(45000,110000,5000), labels=format(seq(45,110,5)), las=2)
lines(as.vector(modelExpo$fitted), col="red")
legend("topleft", c("실제 데이터 ","모델 적용"), lty=c(1,1), col=c("black","red"), bty="n")

#예측 오차 plot 
modelvalues <- c(modelExpo$fitted,Forecasts$mean)
residualvalues <- dataTS - modelvalues
plot(as.vector(residualvalues), xlab="분기",ylab="잔차", type="o",bty="l")
title("잔차 그래프") 

#2차추세 모델 
salesQuadratic <- tslm(Train ~ trend + I(trend^2) + season)
summary(salesQuadratic)

#평가 부분 (2차추세 모델 )
Forecasts2 <- forecast(salesQuadratic, h=validLength)
Forecasts2

#예측 오차 plot (2차추세 모델 )
modelvalues <- c(salesQuadratic$fitted,Forecasts2$mean)
residualvalues <- dataTS - modelvalues
plot(as.vector(residualvalues), xlab="분기",ylab="잔차", type="o",bty="l")
title("잔차(2차추세 모델)")
