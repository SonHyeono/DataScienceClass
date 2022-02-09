# 3가지 사례에 대한 공통가정-1)2010~2020년의 월별데이터(freq=12)
#                            2)length=120
#                            3)sesonality는 sin함수로 가정
#                            3)impact는 2018년 1월에 발생

# 함수생성
# length:계열의 길이 level:평균값 inc:linear/log구분  method:결합방법
My_ts <- function(length=120, level, inc, method)
{
  index = seq(1,length)
  
  Level_compo = ts(c(rep(level, max(index))),
                   start = c(2010, 1), end = c(2020, 1), freq = 12)
  
  if(inc == 0) #inc=0이면 log사용
    Trend_compo = ts(floor(log(index)), 
                     start = c(2010, 1), end = c(2020, 1), freq = 12)
  else         #inc=0이아니라면 linear사용
    Trend_compo = ts(floor(index * inc - 1000), 
                     start = c(2010, 1), end = c(2020, 1), freq = 12)
  
  Seasonal_compo = ts(floor(50 * sin(2*pi/12 * index)), 
                      start = c(2010, 1), end = c(2020, 1), freq = 12)
  
  Noise_compo = ts(rnorm(max(index), mean = 0, sd = 1) * 30, 
                   start = c(2010, 1), end = c(2020, 1), freq = 12)
  
  Impact = ts(-200* dnorm(index, mean = 96, sd = 0.5, log = FALSE), 
              start = c(2010, 1), end = c(2020, 1), freq = 12)
  
  if(method==1) #Additive 결합
    result = ts(Level_compo + Seasonal_compo + Trend_compo + Noise_compo
                + Impact, start = c(2010, 1), end = c(2020, 1), freq = 12) 
  else          #Multiplicative 결합
    result = ts(Level_compo * Seasonal_compo * Trend_compo * (Noise_compo
                                                              + Impact), start = c(2010, 1), end = c(2020, 1), 
                freq = 12)
  return(result) # return(plot(result))하면 result그래프시각화
  
}

# 1) 10년간 누가바 판매량(level:50000,기울기추세 log,결합방법 Add)
My_ts(,50000,0,1) #log으로 하니깐 큰 변화가 없는거같다.

# 2) 10년간 CGV 관람객수(level:2000000,기울기추세-1,결합방법 Add)
My_ts(,2000000,-1,1) #꾸준히 감소

# 3) 10년간 포르쉐 판매량(level:90000, 기울기추세2.0,결합방법 Multi)
My_ts(,90000,2,2) 
#결합방법을 multi로 했을 때 음수값이 나올수밖에없어서 그래프해석이 어려움..
#multi에대한 자료도 없는듯..
#add로 했을 때 대폭상승함을 볼수있다.






#예제 2번
library('ggplot2')
data <- read.csv(file = "C:/data/DepartmentStoreSales.csv", header = TRUE)

ggplot(data=data, aes(x=Quarter, y=Sales)) + geom_line() + scale_y_continuous(labels = scales::comma)


#예제 2번
data <- read.csv(file = "C:/data/DepartmentStoreSales.csv", header = TRUE)

df = data.frame(data)
apply(df,2,mean)


index = 1:24 # quarter

# Level Component
Level_compo = ts(c(rep(65000, max(index))), 
                 start = 1, end = 24)

plot(Level_compo, ylab = "Level", xlab = "Time", bty = "l", 
     xaxt = "n")

axis(1, at = seq(1, 24, 1), labels = format(seq(1, 24, 1), digits = 2))


# Trend Component
Trend_compo = ts(floor(index * 1.3 - 90), 
                 start = 1, end = 24)

plot(Trend_compo, ylab = "Trend", xlab = "Time", bty = "l", 
     xaxt = "n")
axis(1, at = seq(1, 24, 1), labels = format(seq(1, 24, 1), digits = 2))

# Seasonal Component

Seasonal_compo = ts(floor(50 * sin(2*pi/12 * index )), 
                    start = 1, end = 24)

plot(Seasonal_compo, ylab = "Seasonal", xlab = "Time", bty = "l", 
     xaxt = "n")
axis(1, at = seq(1, 24, 1), labels = format(seq(1, 24, 1), digits = 2))


# Noise Component: normal 
Noise_compo = ts(rnorm(max(index), mean = 0, sd = 1) * 30, 
                 start = 1, end = 24)

plot(Noise_compo, ylab = "Noise(normal)", xlab = "Time", bty = "l", 
     xaxt = "n")
axis(1, at = seq(1, 25, 1), labels = format(seq(1, 25, 1), digits = 2))







#예제 5번
library(dplyr)
data2 <- read.csv(file = "C:/data/SouvenirSales.csv",header=TRUE)

df %>% summarize(data2, da = mean(Sales))

tsdata <- ts(data2,start = c(1995, 1), end = c(2002, 1), freq = 12)


ts.plot(tsdata)

#밑의 코드는 log 취한 것

index = seq(1,84)

tsdata2 = ts(floor(log(index)), 
            start = c(1995, 1), end = c(2002, 1), freq = 12)

ts.plot(tsdata2)

#trend의 차이

# Trend Component
Trend_compo = ts(floor(index * 1.3 - 50), 
                 start = c(1995, 1), end = c(2002, 1), freq = 12)

plot(Trend_compo, ylab = "Trend", xlab = "Time", bty = "l", 
     xaxt = "n")
axis(1, at = seq(1995, 2002, 1), 
     labels = format(seq(1995, 2002, 1), digits = 2))



