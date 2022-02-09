#3주차 C조


# 1번: 
#답: 학습용 데이터(1995~2000)와 평가용 데이터(2001)으로 나눈 이유는 
#    원칙직으로 평가용기간의 길이는 예측할 기간인 Forecast horizon(예측범위)과 유사한 기간을 선택하기 때문 입니다.

#2번 :
#답: 분석가가 12달만 평가용 기간으로 잡은 이유는 
#    평가용 데이터 기간은 최소한 모든 계절을 한번씩, 한주기 이상을 포함하도록 하여야 하기 때문입니다.

#3번:
#답: naive forecast(단순예측)을 평가용 데이터 기간에서 하게되면, 12달을 주기로 가정을 했을때 2002년도의 
#    데이터는 2001년도의 데이터로 합니다. 즉 2002년도의 1월달은 2001년의 1월 값과 동일한 값으로 가지게 됩니다.

#4번:

library(forecast)
Souven <- read.csv("SouvenirSales.csv")

Souventir.ts <- ts(Souven$Sales, start =  c(1995, 1), end = c(2001, 12), freq = 12)

nValid <- 12
nTrain <- length(Souventir.ts)-nValid
print(nTrain)


train.ts <- window(Souventir.ts, start = c(1995, 1), end = c(1995, nTrain))
valid.ts <- window(Souventir.ts, start = c(1995, nTrain + 1), end = c(1995, nTrain + nValid))


Souventir.lm <- tslm(train.ts ~ trend + I(trend^2))
Souventir.lm.pred <- forecast(Souventir.lm, h = nValid, level = 0,method="naive")


plot(Souventir.lm.pred, ylim = c(4500,120000), ylab = "Sales",xlab = "Time",bty = "l",
     xaxt = "n", xlim = c(1995,2002.25),main = "", flty = 2)
axis(1, at = seq(1995,2002,1), labels = format(seq(1995,2002,1)))
lines(Souventir.lm$fitted,lwd = 2)
lines(valid.ts)

#forecast의 accuracy 함수를 이용하면 RMSE와 MAPE를 구할 수 있다. 
accuracy(Souventir.lm)


#5번

names(Souventir.lm.pred)
Souventir.lm.pred$residuals
#여기서 residuals가 잔차입니다.
valid.ts <- Souventir.lm.pred$mean



hist(Souventir.lm.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")


#6번 
# 실제 최종 모델은 Souvenir(기념품)의 데이터를 예측하는 것이기에 실제 판매를 위한 공급과 상관있는 데이터 분석임으로
# 오차에 의해 발생되는 비용의  최소화를 목적으로 예측모델을 최적화 해야 합니다. 
# 히스토그램을 살펴보면 오차의 Frequency가 (-10000 ~ 0)에서 많은 것으로 보아 과소 예측(Under-estimate)인 것으로 
# 추측해 볼수 있습니다.

