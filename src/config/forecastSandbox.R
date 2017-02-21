

aa <- auto.arima(ts1)
Pacf(ts1)
plot(forecast(aa))
batsFit <- bats(ts1)
plot(forecast(batsFit))


# strip aggregatedTime from the year:
#v <- stripTimeUnits(v, seriesTimeFreq)

# create ts object:
#freq <- deltatToHigherFreq(seriesTimeFreq)
ts1 <- ts(v$aggregatedVec, 
          start = c(as.numeric(format(v$minDate[1], "%Y")), 
                    as.numeric(format(v$minDate[1], "%m")),
                    as.numeric(format(v$minDate[1], "%d"))), 
          frequency = round(1/seriesTimeFreq))
plot.ts(ts1)
tsdisplay(ts1)
Acf(ts1)
Pacf(ts1) 
#bats:
fit <- tbats(ts1)
plot.bats(fit)
fit$seasonal
plot(forecast(fit,h=100))
#arima:
autoArima <- arfima(ts1)
arimaorder(autoArima)
autoArima2 <- auto.arima(ts1)
#state-space model:
stateSpace <- bats(ts1)
plot(residuals(stateSpace))
plot(forecast(stateSpace))

plot(decompose(ts1))
plot(stl(vec, s.window = "periodic"))
#tslm:
tslmModel <- tslm(ts1 ~ trend + season)
plot(residuals(tslmModel))
CV(tslmModel)
