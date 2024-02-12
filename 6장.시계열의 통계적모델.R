getwd()
rm(list = ls())

# 데이터 로드 ==== 
demand <- read.csv("D:\\Practical Time Series Analysis\\Daily Demand Forecasting Orders.csv",
                   check.names = FALSE)

library(forecast)
target_data= demand['Banking orders (2)']
# ACF, PACF 간단히 보는 코드
forecast::ggtsdisplay(target_data)

str(demand)
# AR 모형====
ar(demand[['Banking orders (2)']], method = 'mle')
## =>  분석 결과 3차수를 선택하라고 함

est <- arima(x = demand[['Banking orders (2)']], order = c(3, 0, 0))
est.1 <- arima(x = demand[['Banking orders (2)']], order = c(3, 0, 0),
               fixed = c(0, NA, NA, NA))
est.1
## AR 모형의 적합성 검정 ==== 
## 륭-박스 검정을 통해 잔차의 자기상관 확인 -> 잔차에 자기상관이 있으면 정상성 만족X, 추가로 모델링이 필요함을 의미
Box.test(est.1$residuals, lag = 10, type = 'Ljung', fitdf = 3)

## AR의 모형의 예측 ====
plot(demand[['Banking orders (2)']], type = 'l')
# AR(3) 모형 예측값
lines(fitted(est.1), col = 3, lwd = 2) 

est1_3 = fitted(est.1, h=3)
est1_5 = fitted(est.1, h=5)
est1_10 = fitted(est.1, h=10)
est1_20 = fitted(est.1, h=20)
est1_30 = fitted(est.1, h=30)

var(est1_3, na.rm=T)
var(est1_5, na.rm=T)
var(est1_10, na.rm=T)
var(est1_20, na.rm=T)
var(est1_30, na.rm=T)

# MA ====
acf(demand[['Banking orders (2)']])
ma.est <- arima(x = demand[['Banking orders (2)']], order = c(0, 0, 9),
               fixed = c(0, 0, NA, rep(0, 5), NA, NA))
ma.est


# ARIMA ====
## 시뮬레이션 코드 생략 (ACF, PACF를 보고 알아서 찹찹 구하라는 뜻) -> 현실성 떨어짐..
## auto.arima ====
### 왜 stepwise = F 걸어놓는지 모르겠음.....
auto.arima(demand[['Banking orders (2)']], stepwise = F,max.p = 3, max.q = 9)

# VAR ====
## 예시랑 똑같이 맞추려면 12번째, 13번째 (Banking orders (2), Banking orders(3)을 가져와야 함)
plot(demand[,c(12)], type = 'l')
plot(demand[,c(13)], type = 'l')
library(vars)
# install.packages("vars")
VARselect(demand[,9:10], lag.max = 4, type = "const")
est.var <- VAR(demand[,c(12,13)], p=3, type = "const")
est.var
par(mfrow = c(2,1))
plot(demand[,12], type = "l")
lines(fitted(est.var)[,1], col =2, lty=2)
plot(demand[,13], type = "l")
lines(fitted(est.var)[,2], col =2, lty=2)

par(mfrow = c(2,1))
acf(demand[,12]-fitted(est.var)[,1])
acf(demand[,13]- fitted(est.var)[,2])
## 포트맨토 검정 ====
serial.test(est.var, lags.pt=8, type = "PT.asymptotic")
