
## Examples from:
## https://www.otexts.org/fpp/4/8

install.packages('fpp')
require(fpp)

par(mfrow=c(1,2))
fit.ex3 <- lm(consumption ~ income, data=usconsumption)
plot(usconsumption, ylab="% change in consumption and income",
     plot.type="single", col=1:2, xlab="Year")
legend("topright", legend=c("Consumption","Income"),
       lty=1, col=c(1,2), cex=.9)
plot(consumption ~ income, data=usconsumption,
     ylab="% change in consumption", xlab="% change in income")
abline(fit.ex3)
summary(fit.ex3)

fcast <- forecast(fit.ex3, newdata=data.frame(income=c(-1,1)))
par(mfrow=c(1,1))
plot(fcast, ylab="% change in consumption", xlab="% change in income")

fit.ex4 <- tslm(austa ~ trend)
f <- forecast(fit.ex4, h=5,level=c(80,95))
plot(f, ylab="International tourist arrivals to Australia (millions)",
     xlab="t")
lines(fitted(fit.ex4),col="blue")
summary(fit.ex4)

par(mfrow=c(2,2))
res3 <- ts(resid(fit.ex3),s=1970.25,f=4)
plot.ts(res3,ylab="res (Consumption)")
abline(0,0)
Acf(res3)
res4 <- resid(fit.ex4)
plot(res4,ylab="res (Tourism)")
abline(0,0)
Acf(res4)

