
# http://www.econ.uiuc.edu/~econ472/tutorial10.html

#this is a simple version of the Granger-Newbold spurious regression simulation
n=100
u<-rnorm(n)
v<-rnorm(n)
y<-rep(0,n)
x<-rep(0,n)
for(i in 2:n) {
  x[i] <- x[i-1] + u[i] 
  y[i] <- y[i-1] + v[i]
}

#you can also use filter(u,1,"recursive") here instead (somewhat more efficiently)
par(mfrow=c(1,2))
t <- 1:n
plot(c(t,t),c(x,y),type="n")
lines(t,x,col="red")
lines(t,y,col="blue")
plot(x,y)
fit <- lm(y~x)
abline(fit)
#make a title with the t-statistic of the slope coefficient of the fit
title(paste("t=",format(round(summary(fit)$coef["x","t value"],2))))
#End copying here.