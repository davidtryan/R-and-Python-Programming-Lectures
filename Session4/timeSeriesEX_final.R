## Example taken from:
## Time Series Analysis and Its Applications: With R Examples
## http://www.stat.pitt.edu/stoffer/tsa3/R_toot.htm
## http://www.stat.pitt.edu/stoffer/tsa3/AppR.pdf
## Time Series Analysis with R - Part 1
## http://www.statoek.wiso.uni-goettingen.de/veranstaltungen/zeitreihen/sommer03/ts_r_intro.pdf
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

install.packages("forecast")
require(forecast)

#########################
## Explore dataset

data(jj)
print (jj)

str(jj)   #NOTE: This is a time-series element (a vector, not a matrix or dataframe)
summary(jj)   #Again, this summary represents the entirety of the data and not each column

dim(jj)
length(jj)

jjm <- as.matrix(jj)
dim(jjm)
jjdf <- data.frame(jj)  
dim(jjdf)   #Note how the format changes - we should keep time-series as time-series data!


#########################
## Constructing a time series object

#set digits to print to 2 instead of the default of 7
options(digits=2)

#Construct time series object that begins in 2293 in June
zardoz = ts(rnorm(48), start=c(2293,6), frequency=12)
print (zardoz)

#window() can be used to get a part of a time series object (i.e. Jan 2294 to Dec 2295)
(oz = window(zardoz, start=c(2294,1), end=c(2295,12)))   #NOTE: Enclose variable assignment in parenthesis to print output

#########################
## Exploring data

options(digits=6)

#Fraction of year
head(time(jj))
head(time(zardoz))

#Cycle of year
cycle(jj)
cycle(zardoz)

#Simple plots
par(mfrow=c(1,1))
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(jj, ylab="Earnings per Share", main="J & J")   
plot(jj, type="o", col="blue", lty="dashed", ylab='Earnings per Share', main="J & J")
plot(diff(log(jj)), main="logged and diffed")   #Lagged and iterated differences of logged values of jj dataset

#####
#Investigate distributional properties of time series data

#Lag example
x <- ts(1:5)
cbind(x, lag(x), lag(x,-1))
ts.intersect(x, lag(x,1), lag(x,-1))

#Difference of a series example (diff)
#diff(x) = xt - xt-1
#diff(x,2) = xt - xt-2
######

#Using plot.ts and ts.plot to generate more advanced time series plots
######
#ts creates a time series object
#as.ts coerces as object into a time series
#is.ts tests whether an object is a time series
######
x = -5:5                  # sequence of integers from -5 to 5
y = 5*cos(x)
par(mfrow=c(3,2))         # multifigure setup: 3 rows, 2 cols
#basic scatterplots
plot(x, main="plot(x)")
plot(x, y, main="plot(x,y)")
#plot.ts
plot.ts(x, main="plot.ts(x)")
plot.ts(x, y, main="plot.ts(x,y)")
#ts.plot
ts.plot(x, main="ts.plot(x)")
ts.plot(ts(x), ts(y), col=1:2, main="ts.plot(x,y)")  # note- x and y are ts objects 

##J&J Data
#basic scatterplots
plot(as.matrix(time(jj)), main='plot(x)')
plot(as.matrix(time(jj)), as.matrix(jj), main='plot(x,y)')
#plot.ts
plot.ts(as.matrix(time(jj)), main='plot.ts(x)')
plot.ts(as.matrix(time(jj)), as.matrix(jj), main='plot.ts(x,y)')
#ts.plot
ts.plot(as.matrix(time(jj)), main="ts.plot(x)")
ts.plot(ts(as.matrix(jj), frequency=4), col=1:2, main="ts.plot(x,y)")  # note- x and y are ts objects 

#NOTE: if your data is a time-series, plot will produce a time plot; otherwise plot.ts() will coerce data into time plot
par(mfrow=c(1,1))
plot(jj)

#########################
## Advanced operations on manipulatinog / working with the data

#####
#Filtering and smoothing time series data
#?filter
#fjj(t) = 1/8*j(t-2) + 1/4*jj(t-1) + 1/4*jj(t) + 1/4*jj(t+1) + 1/8*j(t+2) 
k = c(.5,1,1,1,.5)            # k is the vector of weights
(k = k/sum(k))       
fjj = filter(jj, sides=2, k)

par(mfrow=c(1,1))
plot(jj)
lines(fjj, col="red")         # adds line representing filtering/smoothing operation
lines(lowess(jj), col="blue", lty="dashed")           # adds a lowess smoother (uses locally-weighted polynomial regression)

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
    tui.2 <- filter(jj,filter=rep(1/5,5))
    tui.2b <- filter(jj,filter=k)
    tui.3 <- filter(jj,filter=rep(1/30,30))
    lines(tui.1,col="red")
    lines(tui.2,col="purple")
    lines(tui.2b,col="green")
    lines(tui.3,col="blue")

#####
#Lagged and iterated differences of logged values of jj dataset
dljj = diff(log(jj))        # difference the logged data
head(dljj)
plot(dljj)                  # plot it (not shown)
# Testing data normality
#Shapiro-Test (note: does not take into account skewness, etc.)
#only have to specify the data which is to be tested, not a distribution
shapiro.test(dljj)          # Shapiro-Wilk normality test
#?shapiro.test
#http://www.real-statistics.com/tests-normality-and-symmetry/statistical-tests-normality-symmetry/shapiro-wilk-test/
#If p<alpha, reject the null hypothesis and conclude that the data are not normally distributed

    #Demonstration of creating a random distribution
    #NOTE: rnorm - Density, distribution function, quantile function and random generation 
    #for the normal distribution with mean equal to mean and standard deviation equal to sd.
    #NOTE: set.seed(x) allows you to set the seed so taht you can reproduce results
    par(mfrow=c(2,2))
    hist(rnorm(48))
    hist(rnorm(100))
    hist(rnorm(10000))
    hist(rnorm(10000, mean=5, sd=3))

par(mfrow=c(2,1))
#Histogram and distribution of JJ data
hist(dljj, prob=TRUE, 12, col='red')
lines(density(dljj), lwd=2)
#include for comparison the density of the normal distribution
mu <- mean(dljj)
sigma <- sd(dljj)
x <- seq(-4, 4, length=1000)
y <- dnorm(x, mu, sigma)
lines(x, y, lwd=2, col='blue')

# Comparison of the time series with the normal distribution with the quantile-quantile plot
qqnorm(dljj)             # normal Q-Q plot  
qqline(dljj)             # add a line    
#NOTE: The q-q plot provides a visual comparison of the sample quantiles to the corresponding theoretical quantiles. 
#In general, if the points in a q-q plot depart from a straight line, then the assumed distribution is called into 
#question.
#NOTE: Ideal case (i.e. normally distributed observations) is given when the observations lie on the line

    ##Compare to data that was not logged
    # Comparison of the time series with the normal distribution with the quantile-quantile plot
    qqnorm(diff(jj))
    abline(0,1)
    #NOTE: Ideal case (i.e. normally distributed observations) is given when the observations lie on the line        
        
#####
#Examine dljj correlation

#grid of scatterplots
lag.plot(dljj, 9, do.lines=FALSE)    #large positive correlation at 4 and 8; negative correlations at 1, 3, 5, 9
lag1.plot(dljj, 9)
# x = cos(2*pi*1:100/4) + .2*rnorm(100)
# plot.ts(x)
# dev.new()
# lag.plot(x, 4)
# lag.plot(x, 4, do.lines=FALSE)

par(mfrow=c(2,1)) 
#ACF = autocorrelation function
#Autocorrelation, also known as serial correlation, is the cross-correlation of a signal with 
#itself. Informally, it is the similarity between observations as a function of the time lag 
#between them. It is a mathematical tool for finding repeating patterns 
acf(ts(dljj, freq=4), 20)
#PACF = partial autocorrelcation function
pacf(dljj, 20)
acf2(dljj)
#Note that the LAG axis is in terms of frequency, so 1,2,3,4,5 correspond to lags 4,8,12,16,20 because frequency=4 
#here. If you don't like this type of labeling, you can replace dljj in any of the above by ts(dljj, freq=1); e.g., 
#acf(ts(dljj, freq=1), 20) 

## Decomposition of Time Series
#NOTE: Using nonparametric regression techniques can help evaluate trend of a time sereies.
#stl() performs a seasonal decomposition of a given time series by determing the trend using loess regression and then calculating the seasonal component and the residuals from teh differences X-T
plot(dog <- stl(log(jj), s.window='periodic'))

#########################
## Fit regression to time series data
#log(jj) = beta*time + alpha1*Q1 + alpha2*Q2 + alpha3*Q3 + alpha4*Q4 + epsilon (Q = 1, 2, 3, or 4)

Q = factor(cycle(jj))
trend = time(jj)-1970    #centering the results around 1970
reg = lm(log(jj)~0+trend+Q, na.action=NULL)  # run the regression without an intercept (na retains time series attributes)
summary(reg)

#view model matrix with dummy variable
model.matrix(reg)

#plot observations of logistic regression model
par(mfrow=c(1,1))
plot(log(jj), type="o")   # the data in black with little dots 
lines(fitted(reg), col=2) # the fitted values in bloody red - or use lines(reg$fitted, col=2)
lines(reg$fitted.values, col='blue')

#plot residuals and the ACF of the residuals
par(mfrow=c(2,1))
plot(resid(reg))      # residuals - reg$resid is same as resid(reg) 
acf(resid(reg),20)    # acf of the resids 
#NOTE: ignore 0-lag correlation as it is always 1
##NOTE: Residuals are not all white!! Have to use special care and steps when using lm() to fit lagged regressions

#To use lm() to fit lagged regressions, tie series together using ts.intersect. Otherwise, they will not be
#aligned properly
# require(astsa)
# data(cmort, part)
# ded = ts.intersect(cmort,part,part4=lag(part,-4))              # align the series first
# fit = lm(cmort~part+part4, data=ded, na.action=NULL)           # now the regression will work
# summary(fit)   

ded = ts.intersect(log(jj),0,trend,Q)
reg = lm(log(jj)~0+trend+Q, data=ded, na.action=NULL)
summary(reg)
#view model matrix with dummy variable
model.matrix(reg)
#plot observations of logistic regression model
par(mfrow=c(1,1))
plot(log(jj), type="o")   # the data in black with little dots 
lines(fitted(reg), col=2) # the fitted values in bloody red - or use lines(reg$fitted, col=2)
lines(reg$fitted.values, col='blue')
#plot residuals and the ACF of the residuals
par(mfrow=c(2,1))
plot(resid(reg))      # residuals - reg$resid is same as resid(reg) 
acf(resid(reg),20)

#####
install.packages("dynlm")
require(dynlm)                          # load the package
fit = dynlm(cmort~part + lag(part,-4))  # assumes cmort and part are ts objects, which they are
# fit = dynlm(cmort~part + L(part,4))  is the same thing.
summary(fit)

reg = dynlm(log(jj)~0+trend+Q)
reg = dynlm(log(jj)~0+ts(trend)+cycle(jj))
summary(fit)


#########################
## Exponential Smoothing and Prediction of Time Series
#A natural estimate for predicting the next value of a given time series, xt, at the period t=T is to take weighted sums of past observations
#It seems reasonable to weight recent observations more than observations from the past (use geometric weights)
#Exponential smoothing in its basic form (the term "exponential" comes from
#the fact that the weights decay exponentially) should only be used for time series
#with no systematic trend and/or seasonal components. It has been generalized
#to the Holt-Winters-procedure in order to deal with time series containg trend
#and seasonal variation. In this case, three smoothing parameters are required,
#namely alpha (for the level), beta (for the trend) and gammma (for the seasonal variation)

#Exponential smoothing and ARIMA models are the two most widely-used approaches to time series forecasting, and provide complementary approaches to the problem. 

HoltWinters(jj)
plot(jj)
hwF <- HoltWinters(jj)$fitted
lines(hwF[,1], col='red')

#predict() is a generic function for predictions from various modls
jj.hw <- HoltWinters(jj)

predict(jj.hw, n.ahead=12)
plot(jj, xlim=c(1960,1993), ylim=c(0,50))
lines(predict(jj.hw, n.ahead=48), col=2)


#########################
## Creating ARIMA models
#https://www.otexts.org/fpp/8
#https://onlinecourses.science.psu.edu/stat510/node/48
#ARIMA = Auto-Regressive Integrated Moving Average
#ARIMA models provide another approach to time series forecasting. Exponential smoothing and ARIMA models 
#are the two most widely-used approaches to time series forecasting, and provide complementary approaches 
#to the problem. While exponential smoothing models were based on a description of trend and seasonality 
#in the data, ARIMA models aim to describe the autocorrelations in the data.
#ARs - autoregressive terms (lags of the stationarized series in the forecasting equation)
# * The autoregressive model specifies that the output variable depends linearly on its own previous values.
# * The autoregressive term for the variable is a lagged value of the variable (ex: a lag 1 autoregressive term is xt-1)
#MAs - moving average terms (lags of the forecast errors)
# A moving average term in a time series model is a past error (multiplied by a coefficient)
    #Forecasting based on ARIMA (autoregressive integrated moving averages) models, commonly know as the Box-Jenkins approach, comprises following stages:
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


# some AR1s 
x1 = arima.sim(list(order=c(1,0,0), ar=.9), n=100) 
x2 = arima.sim(list(order=c(1,0,0), ar=-.9), n=100)
par(mfrow=c(2,1))
plot(x1, main=(expression(AR(1)~~~phi==+.9)))
plot(x2, main=(expression(AR(1)~~~phi==-.9)))
acf2(x1, 20)
acf2(x2, 20)

# an MA1  
x = arima.sim(list(order=c(0,0,1), ma=.8), n=100)
par(mfrow=c(1,1))
plot(x, main=(expression(MA(1)~~~theta==.8)))
acf2(x)

# an AR2 
x = arima.sim(list(order=c(2,0,0), ar=c(1,-.9)), n=100) 
plot(x, main=(expression(AR(2)~~~phi[1]==1~~~phi[2]==-.9)))
acf2(x)
    par(mfrow=c(2,1))
    acf(sim.ar,main="ACF of AR(2) process")
    pacf(sim.ar,main="PACF of AR(2) process")

# an ARIMA(1,1,1) 
x = arima.sim(list(order=c(1,1,1), ar=.9, ma=-.5), n=200)
plot(x, main=(expression(ARIMA(1,1,1)~~~phi==.9~~~theta==-.5)))
acf2(x, 30)

#########################
## ARIMA Estimation

#Fit an ARIMA model to some simulated data (with diagnostics and forecasting)
set.seed(500)
x = arima.sim(list(order=c(1,0,1), ar=.9, ma=-.5), n=100)
(x.fit = arima(x, order = c(1, 0, 1)))
#NOTE: In the output here, intercept really should be mean
tsdiag(x.fit, gof.lag=20)
#NOTE: The Ljung-Box statistic graph is incorrect here (use sarima)
#Alternative approach
(x.fit.sa = sarima(x, 1, 0, 1))
x.fit.sa

    ## Parameter Estimation of ARIMA Models
    #After specifying the order of the ARIMA model, the function arima() can be used to esimate parameters
    (fit <- arima(log(jj), order=c(2,0,1)))
    fit$coef     #coefficients
    fit$residuals    #residuals
    fit$aic     #Akaike Information Criterion (AIC)
    #first step in diagnostic checking of fitted models is to analyze the residuals from the fit for any signs of non-randomness
    #NOTE: In the output here, intercept really should be mean
    tsdiag(fit)     #plot of residuals, autocorrelation of the residuals and the p-values of the Ljung-Box statistic for the first 10 lags
    tsdiag(fit, gof.lag=20)
    #NOTE: The Ljung-Box statistic graph is incorrect here (use sarima)
    
    #derived from the idea that the residuals of a "correctly specified" model are independently distributed. If the residuals are not, then they come from a miss-specified model
    Box.test(fit$residuals, lag=1)

    #Alternative approach
    (fit.sa = sarima(log(jj), 2, 0, 1))
    fit.sa

#Forecasting 10 ahead
(x.fit = arima(x, order = c(1, 0, 1)))
x.fore = predict(x.fit, n.ahead=10)  
U = x.fore$pred + 2*x.fore$se
L = x.fore$pred - 2*x.fore$se
minx=min(x,L)
maxx=max(x,U)
ts.plot(x,x.fore$pred,col=1:2, ylim=c(minx,maxx))
lines(U, col="blue", lty="dashed")
lines(L, col="blue", lty="dashed") 
#Alternative approach
sarima.for(x, 10, 1, 0, 1)

    
    ## Prediction of ARIMA Models
    #after a model has been identified and its parameters have been estimated, predict future values of a time series
    #predict() can be used to predict future values of the levels under the model
    jj.pred <- predict(fit, n.ahead=8)
    #contains predicted values (jj.pred$pred) and the standard errors of the prediction jj.pred$se
    par(mfrow=c(1,1))
    plot(log(jj),xlim=c(1960,1984),ylim=c(0,3.5))
    lines(jj.pred$pred,col="red")
    #uses an approximate confidence interval (95%) of the prediction (prediction +/- 2*SE), you can plot data, predicted values and an approximate CI
    lines(jj.pred$pred+2*jj.pred$se,col="red",lty=3)
    lines(jj.pred$pred-2*jj.pred$se,col="red",lty=3)

    par(mfrow=c(1,1))
    plot(jj,xlim=c(1960,1984),ylim=c(0,20))
    lines(exp(jj.pred$pred),col="red")
    #uses an approximate confidence interval (95%) of the prediction (prediction +/- 2*SE), you can plot data, predicted values and an approximate CI
    lines(exp(jj.pred$pred)+exp(2*jj.pred$se),col="red",lty=3)
    lines(exp(jj.pred$pred)-exp(2*jj.pred$se),col="red",lty=3)

    #Alternative plot of the same fit information
    plot(forecast(fit))

    #Alternative approach
    sarima.for(log(jj), 8, 2, 0, 1)


#######################################
## Auto-ARIMA

eeadj <- seasadj(stl(jj, s.window="periodic"))
plot(eeadj)
tsdisplay(diff(eeadj),main="")

####AUTO-ARIMA
(fit.auto <- auto.arima(jj))
plot(forecast(fit.auto, h=20))

#Confirm auto.arima
(fit.autotest <- arima(jj, order=c(1,1,2), seasonal=list(order=c(0,1,0))))
plot(forecast(fit.autotest, h=20))

(fit.autotest <- Arima(jj, order=c(1,1,2), seasonal=list(order=c(0,1,0))))
plot(forecast(fit.autotest, h=20))

##View fits
Acf(residuals(fit.auto))
acf(residuals(fit.auto))
Box.test(residuals(fit.auto), lag=24, fitdf=4, type="Ljung")
plot(forecast(fit.auto))




##########################
## Simulation
ar1 <- -0.79
# ar2 <- 0.0173
ma1 <- -0.097
ma2 <- -0.39
r0 <- 11.61


r.sims <- matrix(rep(c(jj[77:84],rep(0,84)),1000),nrow=(84+length(jj[77:84])), ncol=1000)
e <- matrix(rnorm(92*1000, mean = 0, sd = 0.176), 92, 1000) 
e <- matrix(rnorm(92*1000), 92, 1000) 

#A(2,0,1) = a1*yt-1 + a2*yt-2 + b1*et-1
#A(1,1,2) = yt-1 + a1*(yt-1 - yt-2) - ma1*et-1 - ma2*et-2
r.sims[1] <- r0 + ar1*(r0) - ma1*e[1]
r.sims[2] <- r0 + ar1*(r.sims[1]-r0) - ma1*e[2] - ma2*e[1]

r.sims[1] <- ar1*r0 + e[1] + ma1*e[1] # I replaced e[0] by e[1]
r.sims[2] <- ar1*r.sims[1] + ar2*r0 + e[2] + ma1*e[1]

r.sims <- matrix(rep(c(jj[77:84],rep(0,84)),1000),nrow=(84+length(jj[77:84])), ncol=1000)
for(j in 1:1000){
  e <- rnorm(92, mean = 0, sd = sqrt(0.176))  
  for(i in 9:92){ # I replaced 1 by 3
    r.sims[i,j] <- ar1*(r.sims[i-1,j]-r.sims[i-2,j]-r.sims[i-5,j]+r.sims[i-6,j]) + r.sims[i-1,j] + r.sims[i-4,j] - r.sims[i-5,j] - ma1*e[i] - ma2*e[i-1]
  }
}

par(mfrow=c(2,1))
plot(forecast(fit.auto, h=84), ylim=c(0,75), xlim=c(1960,2002))
cl <- rainbow(1000)
cl <- cl[1000:1]
# plot(jj, xlim=c(1960,2002), ylim=c(0,65))
plot(forecast(fit.auto, h=84), ylim=c(0,75), xlim=c(1960,2002))
for (i in 1:1000) {
  lines(ts(r.sims[9:92,i], start=1981, freq=4), col=cl[i])
}
#Arima forecast lines
lines(forecast(fit.auto, h=84)$mean, col='blue', lwd=2)
lines(ts(forecast(fit.auto, h=84)$lower[85:168], start=1981, freq=4), col='black', lwd=2)
lines(ts(forecast(fit.auto, h=84)$upper[85:168], start=1981, freq=4), col='black', lwd=2)
legend("topleft", lty=c(1,1), c('mean', '5% / 95%'), col=c('blue', 'black'), lwd=c(2,2), cex=0.6)



par(mfrow=c(2,1))
plot(forecast(fit.auto, h=84), ylim=c(0,75), xlim=c(1960,2002))
cl <- rainbow(1000)
cl <- cl[1000:1]
# plot(jj, xlim=c(1960,2002), ylim=c(0,65))
plot(forecast(fit.auto, h=84), ylim=c(0,75), xlim=c(1960,2002))
for (i in 1:1000) {
  lines(ts(r.sims[9:92,i], start=1981, freq=4), col=cl[i])
}
#Simulation median, mean and quartiles
means <- ts(apply(r.sims[9:92,], 1, function(x) mean(x)), start=1981, freq=4)
medians <- ts(apply(r.sims[9:92,], 1, function(x) median(x)), start=1981, freq=4)
library(stats)
lower5 <- ts(apply(r.sims[9:92,], 1, function(x) quantile(x, probs=c(0.05, 0.95))[1]), start=1981, freq=4)
upper5 <- ts(apply(r.sims[9:92,], 1, function(x) quantile(x, probs=c(0.05, 0.95))[2]), start=1981, freq=4)
lower25 <- ts(apply(r.sims[9:92,], 1, function(x) quantile(x)[2]), start=1981, freq=4)
upper25 <- ts(apply(r.sims[9:92,], 1, function(x) quantile(x)[4]), start=1981, freq=4)
#Draw lines              
lines(means, col='navy', lwd=2)
lines(medians, col='purple', lwd=1, lty=3)
lines(lower5, col='navy', lwd=2, lty=12)
lines(upper5, col='navy', lwd=2, lty=12)
lines(lower25, col='grey', lwd=2)
lines(upper25, col='grey', lwd=2)
#Arima confidence bounds
lines(ts(forecast(fit.auto, h=84)$lower[85:168], start=1981, freq=4), col='black', lwd=2)
lines(ts(forecast(fit.auto, h=84)$upper[85:168], start=1981, freq=4), col='black', lwd=2)
legend("topleft", lty=c(1,3,12,1,1), c('mean', 'median', '5% / 95%', '25% / 75%', 'Arima 5% / 95%'), col=c('navy', 'purple', 'navy', 'grey', 'black'), lwd=c(2,1,2,2,2), cex=0.6)


