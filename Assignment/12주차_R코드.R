library(forecast)
library(ggplot2)
library(caret)

# 1번

PME <- read.csv("C:/Users/ok762/Documents/DataScience/PowderyMildewEpidemic.csv", stringsAsFactors=FALSE)
print(PME)

PME = PME[, c("Year","Outbreak","Max.temp","Rel.humidity")]

PME <-transform(PME, Outbreak = ifelse(Outbreak =="Yes", 1, 0))
PME


# 3번 산점도 그리기

plot(PME$Max.temp ~ PME$Rel.humidity,  ylab = "Max.temp",xlab = "Rel.humidity", 
     col = c("red","blue")[PME$Outbreak +1], bty = "l",main = "Scatter plot",pch=19)
legend(80,33, c("No", "Yes"), col = c("red","blue"),pch = 19, bty = "l")


plot(PME$Rel.humidity ~ PME$Max.temp, ylab = "Rel.humidity",xlab = "Max.temp", 
     col = c("red","blue")[PME$Outbreak +1], bty = "l", main = "Scatter plot",pch=19)
legend(30,92, c("No", "Yes"), col = c("red","blue"), pch = 19, bty = "l")

# 4번 단순예측

naiveForecasts <- PME$Outbreak[(length(PME$Outbreak)-1-3) : (length(PME$Outbreak)-1)]
naiveForecasts

library(caret)
predict <- as.factor(naiveForecasts)
actual <- as.factor(PME$Outbreak[(length(PME$Outbreak)-3):length(PME$Outbreak)])
confusionMatrix(predict, actual , positive=c("1"))

# 5번 분할 해서 1995년 예측

train <- PME[1:8, ]
train

Log.PME <- glm(Outbreak ~ Max.temp + Rel.humidity, data=train, family="binomial")

summary(Log.PME)

pred <- predict(Log.PME,PME[9:12,],type="response")
pred

confusionMatrix(as.factor(ifelse(pred > 0.5, 1, 0)), as.factor(PME[9:12,]$Outbreak), positive=c("1"))


# 6번 1996, 1997년, 2000년을 예측, 7번은 예측 정확도 인데 같이 구하겠음.

#1996
train <- PME[1:9, ]
                
Log.PME <- glm(Outbreak ~ Max.temp + Rel.humidity, data=train, family="binomial")

summary(Log.PME)

pred <- predict(Log.PME,PME[10:12,],type="response")
pred


confusionMatrix(as.factor(ifelse(pred > 0.5, 1, 0)), as.factor(PME[10:12,]$Outbreak), positive=c("1"))

#1997
train <- PME[1:10, ]

Log.PME <- glm(Outbreak ~ Max.temp + Rel.humidity, data=train, family="binomial")

summary(Log.PME)

pred <- predict(Log.PME,PME[11:12,],type="response")
pred

predict <- as.factor(ifelse(pred > 0.5, 1, 0))
print(predict)
actual <- as.factor(PME[11:12,]$Outbreak)
print(actual)
test <- factor(c("1","0"))
levels(test) <- c("1","0")
confusionMatrix(predict, test , positive=c("1"))

#2000
train <- PME[1:11, ]

Log.PME <- glm(Outbreak ~ Max.temp + Rel.humidity, data=train, family="binomial")

summary(Log.PME)

pred <- predict(Log.PME,PME[12:12,],type="response")
pred

test <- factor(c("0"))
levels(test) <- c("0")
confusionMatrix(as.factor(ifelse(pred > 0.5, 1, 0)), test, positive=c("1"))


# 10번


