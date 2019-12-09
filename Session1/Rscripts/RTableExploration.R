## Working with tables in R
############################
############################

## Here we will experiment with the different basic functional capabilities of R
## in order to explore its features and functionality.  

rm(list=ls())

############################

## Loading external data
salary2 <- read.table('Documents/Work_KAP/RandPython_20150212/Session1/salary.txt', header=T)
print (salary2)

airquality2 <- read.table('Documents/Work_KAP/RandPython_20150212/Session1/airquality.csv',   header=T, sep=',')
print (airquality2)
View (airquality2)

############################

## Examining data in tables

####Challenger Data####
library(alr3)     #loading data in a packages
print (challeng)

#Reassign R data to a variable that we can work with and alter as needed
challenger <- challeng

#Take a look at the first 10 rows of the Challenger data set
print (challenger[1:10,])
head (challenger[1:10,])

str (challenger)
dim (challenger)
summary (challenger)

####Salary Data####
salary2 <- read.table('Documents/Work_KAP/RandPython_20150212/Session1/salary.txt', header=T)
head (salary2)

#Note: we can read in data and reassign values of names at the same time in a single command
salary2 <- read.table('Documents/Work_KAP/RandPython_20150212/Session1/salary.txt', header=T, 
                      col.names=c('test1', 'test2' , 'test3', 'test4', 'test5', 'test6'))
head (salary2)

############################

## Working with data in tables (querying, modifying, altering)

#Reassign names in the data table
head (challenger[1:10,])
names(challenger) <- c('Temperature', 'Pressure', 'Failure', 'n', 'Erosion', 'BlowBy', 'Damage', 'Date')
head (challenger[1:10,])

#Accessing a data frame column (or entity)
challenger[,8]
challenger$Date

#Reformatting entries in a column
challenger$Date <- as.Date(challenger$Date,format = "%m/%d/%y")
print (challenger)

############################

## Obtaining data subsets

##### Subsets of a vector of interest
#Looking at specific variables of interest
print (airquality2$Temp)
       
## First element
airquality2$Temp[1]

## All but first element
airquality2$Temp[-1]

## Elements 5 through 10
airquality2$Temp[5:10]

## Elements 5 and 7
airquality2$Temp[c(5,7)]

## Temperature for Ozone measurements greater than 75
airquality2$Temp[airquality2$Ozone>75]
## or
airQualSub <- with(airquality2, Temp[Ozone>75])
print (airQualSub)
airQualSub[!is.na(with(airquality2, Temp[Ozone>75]))]   #Note: Shows only values that are not NA


##### Subsets of the whole table
## First row
airquality2[1,]

## Second column
airquality2[,2]

## Some rows and columns
airquality2[3:7, 2:4]

## Columns by name
airquality2[, c("Ozone","Temp","Month")]

#Subset of rows with only columns of interest
airquality2[5:15, c("Ozone", "Temp", "Month")]

#Subset of airquality data from only the month of May
airquality[airquality2$Month==5,]

#Putting only subsets of interest into a new data frame
mayData <- airquality[airquality2$Month==5,]
head (mayData)

############################

## Obtaining data summaries and statistics

#Summary statistics
mean (airquality$Temp)
median (airquality$Temp)
var (airquality$Temp)
sd (airquality$Temp)
mean (airquality$Temp[airquality$Month==5])
with (airquality2, mean(Temp[Month==5]))
hiOzone <- with(airquality2, Ozone>100)
mean(airquality2$Ozone[hiOzone])
mean(airquality2$Ozone[hiOzone], na.rm=TRUE)
hiOzone <- airquality2[with(airquality2, Ozone>100),c('Ozone')]   #Note: alternate way to get the same value
mean(hiOzone)
mean(hiOzone, na.rm=TRUE)

#Using 'if' to manipulate data
hiOzone_imp <- hiOzone
print(hiOzone)
for (i in 1:length(hiOzone)) {
  if (is.na(hiOzone[i])) {
    hiOzone_imp[i] <- mean(airquality2$Ozone, na.rm=TRUE)
  }
}
print(hiOzone_imp)

hiOzone_imp <- hiOzone
hiOzone_imp <- unlist(lapply(hiOzone, function(x) if (is.na(x)) {mean(airquality2$Ozone, na.rm=TRUE)} else {x}))
print(hiOzone_imp)

#Summary statistics for multiple variables with 'for'
for (i in 1:dim(airquality2)[2]) {
  mV <- mean(airquality2[,i])
  print (sprintf("The mean of %s is %f", colnames(airquality2)[i], mV))
}

#Creating summary statistics for multiple variables with 'apply'
lapply(airquality2, function(x) sd(x))
unlist(lapply(airquality2, function(x) sd(x)))

#Summary tables
salary2 <- salary
print(salary2)
table(salary2$Rank, salary2$Degree)
table(salary2$Sex, salary2$Degree)

#Change degree values from binary to Yes/No and recreate summary tables
salary2$Degree <- factor(salary2$Degree,
                         labels=c("Yes","No"))
head(salary2)
table(salary2$Rank, salary2$Degree)
table(salary2$Sex, salary2$Degree)

#Create summary tables by creating cutoffs or bins
salary2$SalaryGrp <- cut(salary2$Salary, c(0,17500,25000,35000))     
table(salary2$SalaryGrp)
     

############################

## Basic Data Visualization

## scatterplot
plot(salary2$Rank, salary2$Salary)
plot(airquality2$Temp, airquality2$Solar.R)
plot(airquality2$Ozone, airquality2$Solar.R)
par(mfrow=c(1,2))
plot(airquality2$Temp, airquality2$Solar.R, main="Temp vs. Solar")
plot(airquality2$Ozone, airquality2$Solar.R, main="Ozone vs. Solar")

## boxplot
par(mfrow=c(1,1))
plot(salary2$Degree, salary2$Rank, xlab=c("Degree"), ylab=c("Rank"))
plot(factor(airquality2$Month), airquality2$Ozone, xlab=c("Month (numeric)"), ylab=c("Ozone level"),
     main=c('Boxplots of Ozone Levels in Each Month'))

## stacked barplot
rankCounts <- table(salary2$Rank, salary2$Degree)
barplot(rankCounts, main="Salary Distribution by Rank and Degree",
        xlab="Degree", ylab="Counts", col=c("darkblue","red","darkgreen"),
        legend = rownames(rankCounts), , beside = TRUE,
        args.legend = list(x="topleft"))


## Visualization with alternative packages
install.packages('ggplot2')
library(ggplot2)

aqSelect <- airquality2[,c('Month', 'Ozone')]
aqSelect$Month <- factor(aqSelect$Month) 

gg <- ggplot(data=aqSelect, aes(x=Month, y=Ozone)) + geom_bar(fill='blue',stat="identity")
gg
gg <- gg + ggtitle("Ozone Levels vs. Month")
gg
gg <- gg + scale_x_discrete(breaks=c(5,6,7,8,9), labels=c("May","June","July","August","September"))
gg

