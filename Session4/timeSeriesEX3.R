
#Time Series Analysis with R - Part 1
#http://www.statoek.wiso.uni-goettingen.de/veranstaltungen/zeitreihen/sommer03/ts_r_intro.pdf
##############################################################
##############################################################

#Data sets available in package astsa
#Lesson for those just starting to use R for time series analysis

##############################################################

#########################
## Install astsa package

#astsa: Applied Statistical Time Series Analysis
#Data sets and scripts for Time Series Analysis and Its Applications: With R Examples by 
#Shumway and Stoffer, 3rd edition

install.packages("astsa")
require(astsa)

#########################
## Explore dataset

data(jj)

par(mfrow=c(1,1))
plot(jj, type='l', lwd=2, xlab='year', 
     'ylab'='$', main='JJ Data')

#Investigate distributional properties of time series data
hist(diff(jj), prob=T, col='red')
lines(density(diff(jj)), lwd=2)
#include for comparison the density of the normal distribution
mu <- mean(diff(jj))
sigma <- sd(diff(jj))
x <- seq(-4, 4, length=100)
y <- dnorm(x, mu, sigma)
lines(x, y, lwd=2, col='blue')

# Comparison of the time series with the normal distribution with the quantile-quantile plot
qqnorm(diff(jj))
abline(0,1)
#NOTE: Ideal case (i.e. normally distributed observations) is given when the observations lie on the line

# Testing data normality
#Shapiro-Test (note: does not take into account skewness, etc.)
#only have to specify the data which is to be tested, not a distribution
shapiro.test(diff(log(jj)))

##########################################################
## Linear Filtering of Time Series

#Traditional time series analysis breaks down time series data into:
# *a trent, T
# *a seasonal component, S
# *a remainder, e

#Trend can be obtained from linear filters on the time series
# * simple class of linear filters are moving averages with equal weights
#To create weekly (a=2 (5 times)), monthly (a=12 (25 times)), quarterly (a=40 (81 times)):
install.packages("ts")
library(ts)
plot(jj,type="l")
tui.1 <- filter(jj,filter=rep(1/2,2))
tui.2 <- filter(jj,filter=rep(1/10,10))
tui.3 <- filter(jj,filter=rep(1/30,30))
lines(tui.1,col="red")
lines(tui.2,col="purple")
lines(tui.3,col="blue")

##########################################################
## Decomposition of Time Series
#NOTE: Using nonparametric regression techniques can help evaluate trend of a time sereies.
#stl() performs a seasonal decomposition of a given time series by determing the trend using loess regression and then calculating the seasonal component and the residuals from teh differences X-T
plot(stl(log(jj), s.window='periodic'))


##########################################################
## Regression Analysis


##########################################################
## Exponential Smoothing and Prediction of Time Series
#A natural estimate for predicting the next value of a given time series, xt, at the period t=T is to take weighted sums of past observations
#It seems reasonable to weight recent observations more than observations from the past (use geometric weights)
#Exponential smoothing in its basic form (the term “exponential” comes from
#the fact that the weights decay exponentially) should only be used for time series
#with no systematic trend and/or seasonal components. It has been generalized
#to the “Holt–Winters”–procedure in order to deal with time series containg trend
#and seasonal variation. In this case, three smoothing parameters are required,
#namely alpha (for the level), beta (for the trend) and gammma (for the seasonal variation)
HoltWinters(jj)
plot(jj)
hwF <- HoltWinters(jj)$fitted
lines(hwF[,1], col='red')

#predict() is a generic function for predictions from various modls
jj.hw <- HoltWinters(jj)

predict(jj.hw, n.ahead=12)
plot(jj, xlim=c(1960,1993), ylim=c(0,50))
lines(predict(jj.hw, n.ahead=48), col=2)


##########################################################
## ARIMA Models

#Forecasting based on ARIMA (autoregressive integrated moving averages) models, commonly know as the Box–Jenkins approach, comprises following stages:
# * Model identification
# * Parameter estimation
# * Diagnostic checking

## Analysis of Autocorrelations (ACF) and Partial Autocorrelations (PACF)
#A first step in analyzing time series is to examine the autocorrelations (ACF) and partial autocorrelations (PACF)
#the order of "pure" AR and MA processes can be identified from the ACF and PACF as shown below:
sim.ar<-arima.sim(list(ar=c(0.4,0.4)),n=1000)     #simulates observations of of an ARIMA(2,0,0)-model (AR(2) model)
sim.ma<-arima.sim(list(ma=c(0.6,-0.4)),n=1000)      #simulates observations of of a MA(2) model
#ARs - autoregressive terms (lags of the stationarized series in the forecasting equation)
#MAs - moving average terms (lags of the forecast errors)
par(mfrow=c(2,2))
acf(sim.ar,main="ACF of AR(2) process")
acf(sim.ma,main="ACF of MA(2) process")
pacf(sim.ar,main="PACF of AR(2) process")
pacf(sim.ma,main="PACF of MA(2) process")


## Parameter Estimation of ARIMA Models
#After specifying the order of the ARIMA model, the function arima() can be used to esimate parameters
(fit <- arima(jj, order=c(2,0,0)))
fit$coef     #coefficients
fit$residuals    #residuals
fit$aic     #Akaike Information Criterion (AIC)
#first step in diagnostic checking of fitted models is to analyze the residuals from the fit for any signs of non-randomness
tsdiag(fit)     #plot of residuals, autocorrelation of the residuals and the p-values of the Ljung-Box statistic for the first 10 lags

#derived from the idea that the residuals of a “correctly specified” model are independently distributed. If the residuals are not, then they come from a miss–specified model
Box.test(fit$residuals, lag=1)

## Prediction of ARIMA Models
#after a model has been identified and its parameters have been estimated, predict future values of a time series
#predict() can be used to predict future values of the levels under the model
jj.pred <- predict(fit, n.ahead=8)
#contains predicted values (jj.pred$pred) and the standard errors of the prediction jj.pred$se
par(mfrow=c(1,1))
plot(jj,xlim=c(1960,1984),ylim=c(0,20))
lines(jj.pred$pred,col="red")
#uses an approximate confidence interval (95%) of the prediction (prediction +/- 2*SE), you can plot data, predicted values and an approximate CI
lines(jj.pred$pred+2*jj.pred$se,col="red",lty=3)
lines(jj.pred$pred-2*jj.pred$se,col="red",lty=3)

