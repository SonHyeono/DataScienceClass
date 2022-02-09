
library(fpp3)

# 1. us_gasoline
us_gasoline %>% autoplot(Barrels)

us_gasoline %>% ACF(Barrels) %>% autoplot()
#lag 1의 차분을 취해보기
us_gasoline %>% autoplot(difference(Barrels))
us_gasoline %>% ACF(difference(Barrels)) %>% autoplot()


us_gasoline %>% autoplot(
  log(Barrels) %>% difference(12)%>% difference(1)
)
 


# 2. canadian_gas

canadian_gas %>% autoplot(Volume)

canadian_gas %>% ACF(Volume) %>% autoplot()
#lag 1의 차분을 취해보기

canadian_gas %>% autoplot(difference(Volume))
canadian_gas %>% ACF(difference(Volume)) %>% autoplot()

canadian_gas %>% autoplot(
  log(Volume) %>% difference(12)
)


canadian_gas %>% autoplot(
  log(Volume) %>% difference(12)%>% difference(1)
)
