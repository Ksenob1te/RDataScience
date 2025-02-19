# ARMA - Autoregressive Moving Average
# ARIMA - Autoregressive Integrated Moving Average

# p - number of autoregressive terms
# d - number of nonseasonal differences
# q - number of moving-average terms

# partial correlation can be computed as the square root of the reduction in variance that is achieved by adding X3 to the regression of Y on X1 and X2.

eps <- scan("./lab8/jj.dat")
eps_ts <- ts(eps, start = c(1960, 1), frequency = 4)

plot(eps_ts,
     main = "Quarterly EPS for Johnson & Johnson (1960-1980)",
     xlab = "Time",
     ylab = "EPS",
     col = "blue",
     lwd = 2)

eps_diff <- diff(eps_ts)
plot(eps_diff,
     main = "First Difference of EPS",
     xlab = "Time",
     ylab = "Difference in EPS",
     col = "darkgreen",
     lwd = 2)


eps_log <- log10(eps_ts)

plot(eps_log,
     main = "Log10 of Quarterly EPS",
     xlab = "Time",
     ylab = "log10(EPS)",
     col = "purple",
     lwd = 2)

eps_log_diff <- diff(eps_log)
plot(eps_log_diff,
     main = "First Difference of log10(EPS)",
     xlab = "Time",
     ylab = "Difference in log10(EPS)",
     col = "brown",
     lwd = 2)

acf(eps_log_diff, main = "ACF of Differenced log10(EPS)")
# I think as we can see as 0.25 lag is over -0.5 it may be overdifferenciated
pacf(eps_log_diff, main = "PACF of Differenced log10(EPS)")

# now we treat this as static, thus using d = 0 as it doesnt need future differencing
model0 <- arima(eps_log, order = c(0, 0, 0))
model0
# Autoregressive Integrated Moving Average

# p and q are amount of items, not lag difference
model1 <- arima(eps_log, order = c(4, 0, 0))
model1
model2 <- arima(eps_log, order = c(4, 0, 3))
model2
model3 <- arima(eps_log, order = c(0, 0, 16))
model3
model4 <- arima(eps_log, order = c(1, 0, 16))

aic_values <- c(model1$aic, model2$aic, model3$aic, model4$aic)
names(aic_values) <- c("ARIMA(4, 0, 0)", "ARIMA(4, 0, 1)", "ARIMA(0, 0, 16)", "ARIMA(1, 0, 16)")
print(aic_values)

set.seed(123)

# AR(1)
ts.sim_AR1 <- arima.sim(n = 10000, list(ar = c(0.9)))
plot(ts.sim_AR1, main = "Simulated AR(1) Process", col = "darkred")
acf(ts.sim_AR1, main = "ACF of AR(1) Process")
pacf(ts.sim_AR1, main = "PACF of AR(1) Process")

# AR(2)
ts.sim_AR2 <- arima.sim(n = 10000, list(ar = c(0.9, -0.5)))
plot(ts.sim_AR2, main = "Simulated AR(2) Process", col = "darkred")
acf(ts.sim_AR2, main = "ACF of AR(2) Process")
pacf(ts.sim_AR2, main = "PACF of AR(2) Process")

# AR(3)
ts.sim_AR3 <- arima.sim(n = 10000, list(ar = c(0.9, -0.5, 0.2)))
plot(ts.sim_AR3, main = "Simulated AR(3) Process", col = "darkred")
acf(ts.sim_AR3, main = "ACF of AR(3) Process")
pacf(ts.sim_AR3, main = "PACF of AR(3) Process")

# AR(4)
ts.sim_AR4 <- arima.sim(n = 10000, list(ar = c(0.9, -0.5, 0.2, -0.3)))
plot(ts.sim_AR4, main = "Simulated AR(4) Process", col = "darkred")
acf(ts.sim_AR4, main = "ACF of AR(4) Process")
pacf(ts.sim_AR4, main = "PACF of AR(4) Process")

# MA(1)
ts.sim_MA1 <- arima.sim(n = 10000, list(ma = c(-1.9)))
plot(ts.sim_MA1, main = "Simulated MA(1) Process", col = "blue")
acf(ts.sim_MA1, main = "ACF of MA(1) Process")
pacf(ts.sim_MA1, main = "PACF of MA(1) Process")

# MA(2)
ts.sim_MA2 <- arima.sim(n = 10000, list(ma = c(-1.9, 1.7)))
plot(ts.sim_MA2, main = "Simulated MA(2) Process", col = "blue")
acf(ts.sim_MA2, main = "ACF of MA(2) Process")
pacf(ts.sim_MA2, main = "PACF of MA(2) Process")

# MA(3)
ts.sim_MA3 <- arima.sim(n = 10000, list(ma = c(-1.9, 1.7, -1.5)))
plot(ts.sim_MA3, main = "Simulated MA(3) Process", col = "blue")
acf(ts.sim_MA3, main = "ACF of MA(3) Process")
pacf(ts.sim_MA3, main = "PACF of MA(3) Process")

# MA(4)
ts.sim_MA4 <- arima.sim(n = 10000, list(ma = c(-1.9, 1.7, -1.5, 1.5)))
plot(ts.sim_MA4, main = "Simulated MA(4) Process", col = "blue")
acf(ts.sim_MA4, main = "ACF of MA(4) Process")
pacf(ts.sim_MA4, main = "PACF of MA(4) Process")
