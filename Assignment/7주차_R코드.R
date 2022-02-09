
library(forecast)
library(ggplot2)

DepStoreSales <- read.csv("DepartmentStoreSales.csv", stringsAsFactors=FALSE)
str(DepStoreSales)
head(DepStoreSales)
tail(DepStoreSales)

Dep_ts <- ts(DepStoreSales$Sales, start=c(1), frequency=4)

y = range(Dep_ts)
plot(c(1, 7), y, type = "n", xlab = "Year", ylab = "Quarterly Sales", bty = "l", ylim = c(20000, 120000), xaxt = "n", yaxt = "n")

lines(Dep_ts, bty = "l")

axis(1, at = seq(1, 7), labels = format(seq(1, 7)))
axis(2, at = seq(20000, 120000, 20000), labels = format(seq(20000, 120000, 20000)), las = 0)

abline(v=6)
arrows(1, 105000, 6, 105000, code=3, length=0.1)
text(10, 108000, "Training")

abline(v=7)
arrows(6, 105000, 7, 105000, code=3, length=0.1)
text(6.5, 108000, "Validation")

validLength <- 4
trainLength <- length(Dep_ts) - validLength

Dep_train <- window(Dep_ts, end = c(1, trainLength))
Dep_valid <- window(Dep_ts, start = c(1, trainLength+1))
# Raw plots
plot(Dep_train)
plot(Dep_valid)


## 5 b i
Dep_hw <- ets(Dep_train, model = "ZZZ",  alpha = 0.2, beta = 0.15, gamma = 0.05)
Dep_hwFor <- forecast(Dep_hw, h=validLength, level=0)

plot(c(1, 7), c(48000,114500), type="n", xlab="Year",  ylab="Quarterly Department Store Sales", bty="l", xaxt="n", yaxt="n")

axis(1, at=seq(1,7,1), labels=format(seq(1,7,1)))
axis(2, at=seq(48000, 114500, 9500), labels=format(seq(48000, 114500, 9500)), las=0)
abline(v=6)
arrows(1, 105000, 6, 105000, code=3, length=0.1)

text(3, 108000, "Training")
abline(v=7)
arrows(6, 105000, 7, 105000, code=3, length=0.1)
text(6.5, 108000, "Validation")

lines(Dep_ts, bty="l")

lines(Dep_hwFor$fitted, lwd = 2, col = "blue")
lines(Dep_hwFor$mean, lwd = 2, col = "blue", lty = 2)

Dep_hwFor


# 5 b ii
accuracy(Dep_hwFor$mean[1:2], Dep_valid[1:2])

library(fable)

fets <- function(x, h)
  forecast(ets(x), h = h)
farima <- function(x, h)
  forecast(ARIMA(x), h = h)

e1 <- tsCV(Dep_train, fets, h = 1)
e2 <- tsCV(Dep_train, farima, h = 1)

mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)

Dep_train %>% ets() %>% forecast() %>% autoplot()


# 5 c
plot(c(1, 7), c(48000,114500), type="n", xlab="Year",  ylab="Quarterly Department Store Sales", bty="l", xaxt="n", yaxt="n")

axis(1, at=seq(1,7,1), labels=format(seq(1,7,1)))
axis(2, at=seq(48000, 114500, 9500), labels=format(seq(48000, 114500, 9500)), las=0)
abline(v=6)
arrows(1, 105000, 6, 105000, code=3, length=0.1)
text(3, 108000, "Training")
abline(v=7)
arrows(6, 105000, 7, 105000, code=3, length=0.1)
text(6.5, 108000, "Validation")

lines(Dep_ts, bty="l")

lines(Dep_hwFor$fitted, lwd = 2, col = "blue")
lines(Dep_hwFor$mean, lwd = 2, col = "blue", lty = 2)


# 5 d 
difflag1.ts <- diff(Dep_ts, lag = 1)
difflag4.ts <- diff(Dep_ts, lag = 4)
difflag1_4.ts <-diff(diff(Dep_ts, lag = 4), lag = 1)

par(mfrow = c(3,2))
plot(Dep_ts, ylab="Sales", xlab="Year", main="Original Series")
plot (difflag1.ts, ylab="Lag 1", xlab="Year", main="Lag 1 to remove trend")
plot (difflag4.ts, ylab="Lag 4", xlab="Year", main="Lag 4 to remove quarterly seasonality")
plot (difflag1_4.ts, ylab="Lag 1 and 4", xlab="Year", main="Double Differenced Series")


plot((diff(diff(Dep_ts, lag = 1), lag = 4)), ylab="Lag 1 and 4", xlab="Year", main="Double Differenced Series opposite order")


# 5 e

ddif_train <- window(difflag1_4.ts, end=c(1, trainLength))
ddif_valid <- window(difflag1_4.ts, start=c(1,trainLength+1), end=c(1,trainLength+validLength))

par(mfrow = c(1,2))
plot(ddif_train)
plot(ddif_valid)

par(mfrow = c(1,1))

pointForecasts <- meanf(ddif_train, h=4)
pointForecasts


realForecasts <- vector()

for (i in 1:validLength) {
  if(i == 1) {
    realForecasts[i] <- pointForecasts$mean[i] + Dep_train[(trainLength+i)-validLength] + (Dep_train[trainLength] - Dep_train[trainLength - validLength])
  } else {
    realForecasts[i] <- pointForecasts$mean[i] + Dep_train[(trainLength+i)-validLength] + (realForecasts[i-1] - Dep_train[trainLength+i-1-validLength])
  }
}

realForecasts


plot(realForecasts, type = 'l', bty = "l")
plot(c(1, 7), c(48000,114500), type="n", xlab="Year",  ylab="Quarterly Department Store Sales", bty="l", xaxt="n", yaxt="n")

axis(1, at=seq(1,7,1), labels=format(seq(1,7,1)))
axis(2, at=seq(48000, 114500, 9500), labels=format(seq(48000, 114500, 9500)), las=0)
abline(v=6)
arrows(1, 105000, 6, 105000, code=3, length=0.1)
text(3, 108000, "Training")
abline(v=7)
arrows(6, 105000, 7, 105000, code=3, length=0.1)
text(6.5, 108000, "Validation")

lines(Dep_ts, bty="l")
lines(seq(6, 6.75, .25), realForecasts, col="blue", lwd=2, lty=2)
realForecasts[1:2]

