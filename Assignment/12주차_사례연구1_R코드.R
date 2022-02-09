library(forecast)
library(ggplot2)
library(caret)
library(fpp3)

#transit <- read.csv("C:/Users/ok762/Documents/DataScience/bicup2006.csv",header=TRUE,skip = 2)
#Historic <- read.csv("C:/Users/ok762/Documents/DataScience/Historic_Information.csv",header=TRUE,skip = 2) 
# 엑셀 내에서 데이터셋에서3번째 줄부터 시작하기에 skip =2 를 함.

transit <- read.csv("C:/Users/ok762/Documents/DataScience/bicup2006.csv",header=TRUE,stringsAsFactors=FALSE)
Historic <- read.csv("C:/Users/ok762/Documents/DataScience/Historic_Information.csv",header=TRUE,stringsAsFactors=FALSE) 

transit = transit[,c("DATE","TIME","DEMAND")]

Historic_ts = ts(Historic$DEMAND, 
             start= c(0), end= c(21), 
             frequency = 63)


  plot(Historic_ts, xlab = "Time", bty= "l", xaxt = "n", ylim = c(0, 150), ylab = "DEMAND",                                
     xlim= c(0,23), main = "DEMAND 2005 March")
axis(1, at = seq(0,20),labels = format(as.factor(unique(Historic$DATE))), las=2)
axis(1, at = seq(21,23),labels = format(as.factor(unique(transit$DATE))), las=2)


trainLength <- 1008
validLength <- length(Historic_ts) - trainLength

#훈련용 데이터와 평가용 데이터 분리 
Train <- window(Historic_ts, end=c(0, trainLength))
Valid <- window(Historic_ts, start=c(0, trainLength+1),end = c(0,trainLength+validLength))

# --------------------------------------------------------------------------------------------

# 1 번

# 선형 추세 모델 -> 가법적 계절성 모델 : linear season 모델 / 계절성 패턴의 반복을 이용
modelExpo <- tslm(Train ~ season)
summary(modelExpo)


# plot 부분 

plot(Historic_ts, xlab = "Time", bty= "l", xaxt = "n", ylim = c(0, 150), ylab = "DEMAND",                                
     xlim= c(0,23), main = "DEMAND 2005 March")
axis(1, at = seq(0,20),labels = format(as.factor(unique(Historic$DATE))), las=2)
axis(1, at = seq(21,23),labels = format(as.factor(unique(transit$DATE))), las=2)

lines(as.vector(modelExpo$fitted), col="red")
legend("topleft", c("실제 데이터","모델 적용"), lty=c(1,1), col=c("black","red"), bty="n")


#평가 부분  (189를 더한 이유는 future 부분을 기록하기 위해서입니다.)
train_Forecasts = forecast(modelExpo, h=validLength + 189, level = 0)

accuracy(train_Forecasts,Valid)

# 그래프 그려보기(가법적 계절성을 이용해서)
plot(train_Forecasts, xlab = "Time", bty= "l", xaxt = "n", ylim = c(0, 150), ylab = "DEMAND",                                
     xlim= c(0,23), main = "DEMAND 2005 March")
axis(1, at = seq(0,20),labels = format(as.factor(unique(Historic$DATE))), las=2)
axis(1, at = seq(21,23),labels = format(as.factor(unique(transit$DATE))), las=2)

# Train
lines(modelExpo$fitted.values, lwd= 2, col="blue")

# Valid
lines(Valid)

abline(v=16, col="red", lwd=3)       
abline(v=21, col="red", lwd=3)       

text(6,150,"Train", cex = 1.25)
text(18,150,"Valid", cex = 1.25)
text(22.5,150,"Future", cex = 1.25)

arrows(0, 140, 16, 140, code = 3, length = 0.1, lwd = 1, angle = 30) 
arrows(16, 140, 21, 140, code = 3, length = 0.1, lwd = 1, angle = 30) 
arrows(21, 140, 24, 140, code = 3, length = 0.1, lwd = 1, angle = 30) 

# --------------------------------------------------------------------------------------------
# 2번

# 제곱 추세를 가지는 가법적 계절 모델 

#2차 추세 모델
historic_Quadratic <- tslm(Train ~ trend + I(trend^2) + season)
summary(historic_Quadratic)


plot(Historic_ts, xlab = "Time", bty= "l", xaxt = "n", ylim = c(0, 150), ylab = "DEMAND",                                
     xlim= c(0,23), main = "DEMAND 2005 March")
axis(1, at = seq(0,20),labels = format(as.factor(unique(Historic$DATE))), las=2)
axis(1, at = seq(21,23),labels = format(as.factor(unique(transit$DATE))), las=2)

lines(as.vector(historic_Quadratic$fitted), col="red")
legend("topleft", c("실제 데이터","모델 적용"), lty=c(1,1), col=c("black","red"), bty="n")

#평가 부분 (189를 더한 이유는 future 부분을 기록하기 위해서입니다.)
train_Forecasts = forecast(historic_Quadratic, h=validLength + 189, level = 0)

accuracy(train_Forecasts,Valid)



# 그래프 그려보기(2차 추세모델을 이용해서)
plot(train_Forecasts, xlab = "Time", bty= "l", xaxt = "n", ylim = c(0, 150), ylab = "DEMAND",                                
     xlim= c(0,23), main = "DEMAND 2005 March")
axis(1, at = seq(0,20),labels = format(as.factor(unique(Historic$DATE))), las=2)
axis(1, at = seq(21,23),labels = format(as.factor(unique(transit$DATE))), las=2)

# Train
lines(historic_Quadratic$fitted.values, lwd= 2, col="blue")

# Valid
lines(Valid)

abline(v=16, col="red", lwd=3)       
abline(v=21, col="red", lwd=3)       

text(6,150,"Train", cex = 1.25)
text(18,150,"Valid", cex = 1.25)
text(22.5,150,"Future", cex = 1.25)

arrows(0, 140, 16, 140, code = 3, length = 0.1, lwd = 1, angle = 30) 
arrows(16, 140, 21, 140, code = 3, length = 0.1, lwd = 1, angle = 30) 
arrows(21, 140, 24, 140, code = 3, length = 0.1, lwd = 1, angle = 30) 



# --------------------------------------------------------------------------------------------
# 3번
#  주말데이터를 데이터 셋에서 제거하고 모델로 예측을 해보겠습니다.


Historic_weekday <- subset(Historic,DATE!="05-Mar-05" &DATE!="06-Mar-05" &DATE!="12-Mar-05" &DATE!="13-Mar-05" &DATE!="19-Mar-05" &DATE!="20-Mar-05")

Historic_ts = ts(Historic_weekday$DEMAND, 
                 start= c(0), end= c(15), 
                 frequency = 63)


plot(Historic_ts, xlab = "Time", bty= "l", xaxt = "n", ylim = c(0, 150), ylab = "DEMAND",                                
     xlim= c(0,17), main = "DEMAND 2005 March")
axis(1, at = seq(0,14),labels = format(as.factor(unique(Historic_weekday$DATE))), las=2)
axis(1, at = seq(15,17),labels = format(as.factor(unique(transit$DATE))), las=2)


trainLength <- 819
validLength <- length(Historic_ts) - trainLength

#훈련용 데이터와 평가용 데이터 분리 
Train <- window(Historic_ts, end=c(0, trainLength))
Valid <- window(Historic_ts, start=c(0, trainLength+1),end = c(0,trainLength+validLength))


#2차 추세 모델
historic_Quadratic <- tslm(Train ~ trend + I(trend^2) + season)
summary(historic_Quadratic)


plot(Historic_ts, xlab = "Time", bty= "l", xaxt = "n", ylim = c(0, 150), ylab = "DEMAND",                                
     xlim= c(0,17), main = "DEMAND 2005 March")
axis(1, at = seq(0,14),labels = format(as.factor(unique(Historic_weekday$DATE))), las=2)
axis(1, at = seq(15,17),labels = format(as.factor(unique(transit$DATE))), las=2)


lines(as.vector(historic_Quadratic$fitted), col="red")
legend("topleft", c("실제 데이터","모델 적용"), lty=c(1,1), col=c("black","red"), bty="n")

#평가 부분 (189를 더한 이유는 future 부분을 기록하기 위해서입니다.)
train_Forecasts = forecast(historic_Quadratic, h=validLength + 189, level = 0)

accuracy(train_Forecasts,Valid)



# 그래프 그려보기(2차 추세모델을 이용해서)
plot(train_Forecasts, xlab = "Time", bty= "l", xaxt = "n", ylim = c(0, 150), ylab = "DEMAND",                                
     xlim= c(0,17), main = "DEMAND 2005 March")
axis(1, at = seq(0,14),labels = format(as.factor(unique(Historic_weekday$DATE))), las=2)
axis(1, at = seq(15,17),labels = format(as.factor(unique(transit$DATE))), las=2)


# Train
lines(historic_Quadratic$fitted.values, lwd= 2, col="blue")

# Valid
lines(Valid)

abline(v=13, col="red", lwd=3)       
abline(v=15, col="red", lwd=3)       

text(6,150,"Train", cex = 1.25)
text(14,150,"Valid", cex = 1.25)
text(16,150,"Future", cex = 1.25)

arrows(0, 140, 13, 140, code = 3, length = 0.1, lwd = 1, angle = 30) 
arrows(13, 140, 15, 140, code = 3, length = 0.1, lwd = 1, angle = 30) 
arrows(15, 140, 17, 140, code = 3, length = 0.1, lwd = 1, angle = 30) 


