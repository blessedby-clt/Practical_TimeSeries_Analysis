getwd()
rm(list = ls())
demand <- read.csv("C:/Users/sinra/Downloads/archive/Daily Demand Forecasting Orders.csv",
                   check.names = FALSE)
str(demand)
ar(demand[['Banking orders (2)']], method = 'mle')

est <- arima(x = demand[['Banking orders (2)']], order = c(3, 0, 0))
est.1 <- arima(x = demand[['Banking orders (2)']], order = c(3, 0, 0),
               fixed = c(0, NA, NA, NA))
est.1

Box.test(est.1$residuals, lag = 10, type = 'Ljung', fitdf = 3)

library(forecast)
target_data= demand['Banking orders (2)']
forecast::ggtsdisplay(target_data)

plot(demand[['Banking orders (2)']], type = 'l')
lines(fitted(est.1, h = 3), col = 3, lwd = 2)

acf(target_data)

# MA
ma.est <- arima(x = demand[['Banking orders (2)']], order = c(0, 0, 9),
               fixed = c(0, 0, NA, rep(0, 5), NA, NA))
ma.est

# 파라미터의 선택

set.seed(1017)
y = arima.sim(n=1000, list(ar = c(0.8, -0.4), ma = c(-0.7)))
ggtsdisplay(y)                             

auto.arima(demand[['Banking orders (2)']], stepwise = F,max.p = 3, max.q = 9)

# VAR
demand[,c(9, 10)]
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

serial.test(est.var, lags.pt=8, type = "PT.asymptotic")
