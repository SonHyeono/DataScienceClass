library(fpp3)
library(fable)

print(us_change)

us_change %>% autoplot(Consumption)

# acf와 pacf의 plot
us_change  %>%
  gg_tsdisplay(Consumption, plot_type='partial')

# 1차 차분
#us_change  %>%
#  gg_tsdisplay(difference(Consumption), plot_type='partial')


#ARIMA 파라미터 X
fit <- us_change %>%  model(ARIMA(Consumption))
report(fit)

#ARIMA 파라미터 값들을 RDocumentation을 참고해서 채운 것.
fit <- us_change %>%  model(ARIMA(Consumption,
                                  approximation=FALSE,stepwise=FALSE,greedy = TRUE))
report(fit)

#ARIMA 모델을 acf와 pacf plot을 보면서 AR과 MR의 급수를 생각
fit <- us_change %>%  model(ARIMA(Consumption ~ pdq(3,0,0)))
report(fit)

fit <- us_change %>%  model(ARIMA(Consumption ~ pdq(0,0,3)))
report(fit)



fit <- us_change %>%  model(ARIMA(Consumption ~ pdq(0,1,3)))
report(fit)

fit <- us_change %>%  model(ARIMA(Consumption ~ pdq(3,1,0)))
report(fit)