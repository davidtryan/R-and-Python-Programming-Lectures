## Example taken from:
## Time Series Analysis and Its Applications: With R Examples
## http://www.stat.pitt.edu/stoffer/tsa3/R_toot.htm
## http://www.stat.pitt.edu/stoffer/tsa3/AppR.pdf
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

#Demonstration of creating a random distribution
#NOTE: rnorm - Density, distribution function, quantile function and random generation 
#for the normal distribution with mean equal to mean and standard deviation equal to sd.
#NOTE: set.seed(x) allows you to set the seed so taht you can reproduce results
par(mfrow=c(2,2))
hist(rnorm(48))
hist(rnorm(100))
hist(rnorm(10000))
hist(rnorm(10000, mean=5, sd=3))

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

######
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
#fjj(t) = ⅛ jj(t-2) + ¼ jj(t-1) + ¼ jj(t) + ¼ jj(t+1) + ⅛ jj(t+2) 
k = c(.5,1,1,1,.5)            # k is the vector of weights
(k = k/sum(k))       
fjj = filter(jj, sides=2, k)

par(mfrow=c(1,1))
plot(jj)
lines(fjj, col="red")         # adds line representing filtering/smoothing operation
lines(lowess(jj), col="blue", lty="dashed")           # adds a lowess smoother (uses locally-weighted polynomial regression)

#####
#Lagged and iterated differences of logged values of jj dataset
dljj = diff(log(jj))        # difference the logged data
head(dljj)
plot(dljj)                  # plot it (not shown)
shapiro.test(dljj)          # Shapiro-Wilk normality test
#?shapiro.test
#http://www.real-statistics.com/tests-normality-and-symmetry/statistical-tests-normality-symmetry/shapiro-wilk-test/
#If p<alpha, reject the null hypothesis and conclude that the data are not normally distributed

par(mfrow=c(2,1))
hist(dljj, prob=TRUE, 12)
lines(density(dljj))
qqnorm(dljj)             # normal Q-Q plot  
qqline(dljj)             # add a line    
#NOTE: The q-q plot provides a visual comparison of the sample quantiles to the corresponding theoretical quantiles. 
#In general, if the points in a q-q plot depart from a straight line, then the assumed distribution is called into 
#question.

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
acf(ts(dljj, freq=4), 20)
#PACF = partial autocorrelcation function
pacf(dljj, 20)
acf2(dljj)
#Note that the LAG axis is in terms of frequency, so 1,2,3,4,5 correspond to lags 4,8,12,16,20 because frequency=4 
#here. If you don't like this type of labeling, you can replace dljj in any of the above by ts(dljj, freq=1); e.g., 
#acf(ts(dljj, freq=1), 20) 

plot(dog <- stl(log(jj), "per")) 

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
## Creating ARIMA models
#https://www.otexts.org/fpp/8
#ARIMA = Auto-Regressive Integrated Moving Average
#ARIMA models provide another approach to time series forecasting. Exponential smoothing and ARIMA models 
#are the two most widely-used approaches to time series forecasting, and provide complementary approaches 
#to the problem. While exponential smoothing models were based on a description of trend and seasonality 
#in the data, ARIMA models aim to describe the autocorrelations in the data.
#ARs - autoregressive terms (lags of the stationarized series in the forecasting equation)
#MAs - moving average terms (lags of the forecast errors)

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
(x.fit = sarima(x, 1, 0, 1))
x.fit

#Forecasting 10 ahead
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

