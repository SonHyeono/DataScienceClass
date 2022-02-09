library(forecast)
library(ggplot2)
library(caret)

tourism <- read.csv("C:/Users/ok762/Documents/DataScience/tourism_data.csv", stringsAsFactors=FALSE)
tourism
length(tourism)

library(RColorBrewer)

plot(tourism$Y34)

# Assignment - 1
plot(tourism[1:10],col=brewer.pal(3,"Set1"))
plot(tourism[11:20],col=brewer.pal(3,"Set1"))
plot(tourism[21:30],col=brewer.pal(3,"Set1"))
plot(tourism[31:40],col=brewer.pal(3,"Set1"))
plot(tourism[41:50],col=brewer.pal(3,"Set1"))
plot(tourism[51:60],col=brewer.pal(3,"Set1"))
plot(tourism[61:70],col=brewer.pal(3,"Set1"))
plot(tourism[71:80],col=brewer.pal(3,"Set1"))
plot(tourism[81:90],col=brewer.pal(3,"Set1"))
plot(tourism[91:100],col=brewer.pal(3,"Set1"))
plot(tourism[101:110],col=brewer.pal(3,"Set1"))
plot(tourism[111:120],col=brewer.pal(3,"Set1"))
# ...
plot(tourism[501:510],col=brewer.pal(3,"Set1"))
plot(tourism[511:518],col=brewer.pal(3,"Set1"))

#na.omit(tourism$Y1)
tourism20 = ts(tourism$Y20, 
                 start= c(1968), end= c(2010), 
                 frequency = 1)

yrange = (tourism20)
plot(tourism20, xlab = "Year", ylab = "Y20", bty = "l", xaxt = "n", yaxt = "n", main = "Tourism$Y20")
axis(1, at = seq(1968,2010), las=1)
axis(2,at=seq(0,4000000,1000000),labels = format(seq(0.0,4000000.0,1000000)),las=1)


nValid <- 4
nTrain <- length(tourism20) - nValid
#훈련용 데이터와 평가용 데이터 분리 
tourism20_train <- window(tourism20,end=c(1968,nTrain))
tourism20_valid <- window(tourism20,start=c(1968,nTrain+1))

tourism20_train

naive_mod20 <- naive(tourism20_train, h = 4)
summary(naive_mod20)

plot(tourism20, main="Tourism$Y20", xlab="Year", ylab="Y20")
lines(naive_mod20$mean,col="red")
legend("topleft", c("naive forecast valid"), lty=c(1,1), col=c("red"), bty="n")

# valid summary 계산

naive_mod20_val <- naive(tourism20_valid, h = 4)
summary(naive_mod20_val)




# Y152에 대해서 naive forecast 


tourism152 = ts(tourism$Y152, 
               start= c(1968), end= c(2010), 
               frequency = 1)

nValid <- 4
nTrain <- length(tourism152) - nValid
#훈련용 데이터와 평가용 데이터 분리 
tourism152_train <- window(tourism152,end=c(1968,nTrain))
tourism152_valid <- window(tourism152,start=c(1968,nTrain+1))

tourism152_train

naive_mod152 <- naive(tourism152_train, h = 4)
summary(naive_mod152)

plot(tourism152, main="Tourism$Y152", xlab="Year", ylab="Y152")
lines(naive_mod152$mean,col="red")
legend("topleft", c("naive forecast valid"), lty=c(1,1), col=c("red"), bty="n")

# valid summary 계산

naive_mod152_val <- naive(tourism20_valid, h = 4)
summary(naive_mod152_val)



# Y510에 대해서 naive forecast 


tourism510 = ts(tourism$Y510, 
                start= c(1968), end= c(2010), 
                frequency = 1)

nValid <- 4
nTrain <- length(tourism510) - nValid
#훈련용 데이터와 평가용 데이터 분리 
tourism510_train <- window(tourism510,end=c(1968,nTrain))
tourism510_valid <- window(tourism510,start=c(1968,nTrain+1))

tourism510_train

naive_mod510 <- naive(tourism510_train, h = 4)
summary(naive_mod510)

plot(tourism510, main="Tourism$Y510", xlab="Year", ylab="Y510")
lines(naive_mod510$mean,col="red")
legend("topleft", c("naive forecast valid"), lty=c(1,1), col=c("red"), bty="n")

# valid summary 계산

naive_mod510_val <- naive(tourism20_valid, h = 4)
summary(naive_mod510_val)

#========================================================
# 7번 MAPE와 MASE 짝 짓기

# Y20
group<- c(1,2)
x <- c(accuracy(naive_mod20)[5],accuracy(naive_mod20)[6])
y <- c(accuracy(naive_mod20_val)[5],accuracy(naive_mod20_val)[6])
dat <- cbind(group,x,y)
plot(formula=y~x, data=dat,main ="Y20 MAPE and MASE",
               col=c("blue","red")[group])  #group에 따라 색상 지정
legend("topleft", c("MAPE","MASE"),  fill=c("blue","red"), bty="l")

# Y152
group<- c(1,2)
x <- c(accuracy(naive_mod152)[5],accuracy(naive_mod152)[6])
y <- c(accuracy(naive_mod152_val)[5],accuracy(naive_mod152_val)[6])
dat <- cbind(group,x,y)
plot(formula=y~x, data=dat,main ="Y152 MAPE and MASE",
     col=c("blue","red")[group])  #group에 따라 색상 지정
legend("topleft", c("MAPE","MASE"),  fill=c("blue","red"), bty="l")

# Y510
group<- c(1,2)
x <- c(accuracy(naive_mod510)[5],accuracy(naive_mod510)[6])
y <- c(accuracy(naive_mod510_val)[5],accuracy(naive_mod510_val)[6])
dat <- cbind(group,x,y)
plot(formula=y~x, data=dat,main ="Y510 MAPE and MASE",
     col=c("blue","red")[group])  #group에 따라 색상 지정
legend("topleft", c("MAPE","MASE"),  fill=c("blue","red"), bty="l")


plot(tourism510, main="Tourism$Y510", xlab="Year", ylab="Y510")

