## Simple R operations
############################
############################

## Here we will experiment with the different basic functional capabilities of R
## in order to explore its features and functionality.  

############################

## Simple math
1+5
5*13
4^2
(4+3)^3
sqrt(2)
log(100)
log10(100)
sqrt(-1)  #NOTE: Warnings are produced when R cannot calculuate something or when a calculation produces NaNs

############################

## Creating and assigning values to variables
x <- c()
x <- 4
y <- 3
t <- 5+14
y <- t
y <- 3

############################

## Basic operations with variables
x*y
z <- x*y

print (sprintf("My value of z is now %i",z))

#Reminding ourselves of the variables we have in our environment
ls()

#Combining values / variables into a vector with dimensions greater than 1
a <- c(x,y,z)
print(a)    #Note: This is the same as just a
str(a)
class(a)
dim(a)
length(a)

#Extending a vector
a <- c(a, 100)
print (a)
a <- c(a, a)
print (a)
aStore <- a

#Binding vectors
arbind <- rbind (a, a)
print (arbind)
dim(arbind)
length(arbind)
acbind <- cbind (a, a)
print (acbind)
dim(acbind)
length(acbind)

#Using if statements with vectors and variables
a <- aStore
print(a)
lengthA <- length(a)
if (a[lengthA]>=100) {
  a[lengthA] <- 99
}
print(a)

#Using for loops with vectors
a <- aStore
print(a)
for (i in 1:lengthA) {
  a[i] <- i*2
}
print (a)

a <- aStore
print(a)
for (i in 1:lengthA) {
  if (a[i]<10) {
    a[i] <- 10
  } else if (a[i]>=100) {
    a[i] <- 99
  }
}
print (a)


############################

## Plotting variables and their values
plot(aStore)

############################

## Creating data frames
#(Data frame definition from R help:
#This function creates data frames, tightly coupled collections of variables which share many 
#of the properties of matrices and of lists, used as the fundamental data structure by most of 
#R's modeling software)
help (data.frame)

a <- c(16,12,44)
b <- data.frame(a)
print (b)
rownames(b)
colnames(b)

row.names(b) <- c('son','daughter','father')
names(b) <- c('age')
print (b)

class(b)
summary(b)

barplot(height=b$age, names.arg=row.names(b))

############################

## Installing and using new packages
install.packages('ggplot2')
library(ggplot2)

a <- c(16,12,44)
b <- cbind(a, c('son','daughter','father'))
print (b)
b <- data.frame(b)
colnames(b) <- c('age', 'family_member')
print (b)

#Alternative way of constructing the same data frame (with cbind)
bAlt <- data.frame(a)
colnames(bAlt) <- c('age')
bAlt$family_member <- factor(c('son','daughter','father'))
print (bAlt)

gg <- ggplot(data=b, aes(x=family_member, y=age)) + geom_bar(fill='red',stat="identity")
gg
gg <- gg + ggtitle("Family Member Ages (years)")
gg
