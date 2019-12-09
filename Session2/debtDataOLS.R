#######################
## Debt Data Example
#######################

#Read in the data
debtData <- read.csv('~/Documents/Work_KAP/RandPython_20150212/Examples_Koch/Example1/Example_Data.csv')

#Look at variable names
names(debtData)

#Create a table of summary statistics through a for loop 
#(generate summaries for each column and put them in a table)
desStats <- matrix(NA, nrow=ncol(debtData), ncol=5)
for (i in 1:length(names(debtData))) {
  desStats[i,] <- c(length(which(debtData[,i]!="NA")), mean(debtData[,i], na.rm=TRUE), sd(debtData[,i],na.rm=TRUE),
                    min(debtData[,i], na.rm=TRUE), max(debtData[,i], na.rm=TRUE))  
}
rownames(desStats) <- names(debtData)
colnames(desStats) <- c("obs", "Mean", "Std. Dev", "Min", "Max")

print(desStats)

#Perform a OLS Regression to investigate the relationship between the percentage of families in total 
#households and federal debt as a percentage of GDP. 
olsReg <- lm(debt_gdp ~ family_porp, data=debtData)
summary(olsReg)
par(mfrow=c(2,2))
plot(olsReg)

#Regression shows a very significant negative relationship
#An increase in the family-household ratio by one-tenth of a percentage point is related 
#to a decrease in debt as a ratio of GDP by 25.3 percentage points. This very sensitive 
#relationship may have been the result of relatively little variation in the family_porp 
#variable relative to the debt_gdp variable

#The high significance may be due to autocorrelation, which tends to inflate the T-values 
#and decrease the P-values when using time series in OLS. This can be investigated further.



###############################################
###############################################
## Reference example: Longley's Economic Regression Data

#A macroeconomic data set which provides a well-known example for a highly collinear regression. 

require(stats); require(graphics)
## give the data set in the form it is used in S-PLUS:
longley.x <- data.matrix(longley[, 1:6])
longley.y <- longley[, "Employed"]
pairs(longley, main = "longley data")
summary(fm1 <- lm(Employed ~ ., data = longley))
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0),
            mar = c(4.1, 4.1, 2.1, 1.1))
plot(fm1)
par(opar)
