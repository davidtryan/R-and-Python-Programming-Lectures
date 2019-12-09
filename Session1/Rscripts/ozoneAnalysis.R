## EPA data examples
############################
############################

## This data is pulled from the EPA
## http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/download_files.html#Daily
## Contains information on ozone, pollutants, wind, temperature, etc.

############################
############################

#Clear workspace
rm(list=ls())

############################
## Read in relevant data

ozoneDaily2014 <- read.csv('Documents/Work_KAP/RandPython_20150212/Examples_public/EPA/hourly_44201_2014.csv')
ozoneHourly2014 <- read.csv('Documents/Work_KAP/RandPython_20150212/Examples_public/EPA/hourly_44201_2014.csv')

## Examining the tables
###
#ozoneDaily2014
##
dim (ozoneDaily2014)
str (ozoneDaily2014)
head (ozoneDaily2014)
tail (ozoneDaily2014)
View (head (ozoneDaily2014))
View (ozoneDaily2014)
View (tail (ozoneDaily2014))
names(ozoneDaily2014)
summary (ozoneDaily2014)

###
#ozoneHourly2014
##
dim (ozoneHourly2014)
str (ozoneHourly2014)
head (ozoneHourly2014)
tail (ozoneHourly2014)
View (head (ozoneHourly2014))
View (ozoneHourly2014)
View (tail (ozoneHourly2014))
names(ozoneHourly2014)
summary (ozoneHourly2014)

#####
## NOTE: Let us focus on Daily information for now (much smaller file!)
#####

## Pulling out specific information from the table for further analysis

#Summary information for specific variables
#Factor variable (character)
summary(ozoneDaily2014$County.Name)
summary(ozoneDaily2014$County.Name, maxsum=10)
#Numeric variables
summary(ozoneDaily2014$Sample.Measurement)

#Look at the states represented in this data set and their frequency of occurence
table (ozoneDaily2014$State.Code)

#Find the mean of the sample ozone measurement level value for state 006
with(ozoneDaily2014, mean(Sample.Measurement[State.Code==6]))

#Look at an example entry in the table (specific analytic record)
ozoneDaily2014[1,]
View (ozoneDaily2014[1,])

#Look at the first 100 records with respect to variables of interest
sampNums <- sample(1:dim(ozoneDaily2014)[1], 100, replace=F)
ozoneDaily2014[sampNums,c('County.Code', 'Parameter.Name', 'Sample.Measurement', 'POC')]
#Create a subset of the data with the variables of interest only
ozoneDaily2014_sub <- ozoneDaily2014[sampNums,c('County.Code', 'Parameter.Name', 'Sample.Measurement', 'POC', 'Units.of.Measure')]
print(ozoneDaily2014_sub)

############################
## Manipulating Data
levels(ozoneDaily2014_sub$Units.of.Measure)
levels(ozoneDaily2014_sub$Units.of.Measure)[levels(ozoneDaily2014_sub$Units.of.Measure)=="Parts per million"] <- "ppm"
print(ozoneDaily2014_sub)

############################
## Creating custom data frames with important information about tables
#Creating a summary report with mean and standard deviations sample of measurements for each county code
allCountyCodes <- as.numeric(names(table(ozoneDaily2014$County.Code)))

countCodeMeans <- c()
countCodeSDs <- matrix(NA, nrow=length(allCountyCodes), ncol=1)
for (i in 1:length(allCountyCodes)) {
  countCodeMean_tmp <- mean(ozoneDaily2014$County.Code==allCountyCodes[i], na.rm=TRUE)
  countCodeSD_tmp <- sd(ozoneDaily2014$County.Code==allCountyCodes[i], na.rm=TRUE)
  countCodeMeans <- c(countCodeMeans, countCodeMean_tmp)
  countCodeSDs[i] <- countCodeSD_tmp
}

countCodeSumStats <- data.frame(cbind(allCountyCodes, countCodeMeans, countCodeSDs))
colnames(countCodeSumStats) <- c('County.Code', 'MeanOzone', 'SDOzone')
print(countCodeSumStats)

############################
## Creating plots from custom data frames with important information about tables

par(mar=c(5,4,4,2))
barplot(countCodeSumStats$MeanOzone, names.arg=countCodeSumStats$County.Code, las=2)

orderedCCSS <- countCodeSumStats[order(countCodeSumStats$MeanOzone, decreasing=TRUE),]
barplot(orderedCCSS$MeanOzone[1:20], names.arg=orderedCCSS$County.Code[1:20], las=2, main="Top 20 County Code Mean Ozone Measurements",
        xlab='County.Code', ylab='MeanOzone')

## Normalizing data and replotting
normaloCCSS <- orderedCCSS
normaloCCSS$MeanOzone <- (orderedCCSS$MeanOzone-min(orderedCCSS$MeanOzone)) / (max(orderedCCSS$MeanOzone)-min(orderedCCSS$MeanOzone))
print (normaloCCSS)
summary (normaloCCSS$MeanOzone)
summary (orderedCCSS$MeanOzone)
barplot(normaloCCSS$MeanOzone[1:20], names.arg=normaloCCSS$County.Code[1:20], las=2, main="Top 20 County Code Mean Ozone Measurements",
        xlab='County.Code', ylab='Normalized MeanOzone')

#Create informative list on table variables
colNames <- as.character(names(ozoneDaily2014))
colNums <- 1:length(colNames)
colTypes <- as.character(lapply(ozoneDaily2014, function(x) class(x)[1]))
varsNA <- as.numeric(colSums(is.na(ozoneDaily2014)))
varsBlank <- apply(ozoneDaily2014, 2, function(x) length(which((x==""))))
vars2 <- data.frame(cbind(colNums, colNames, colTypes, varsNA, varsBlank, 
                          paste(round(100*((varsNA+varsBlank)/dim(ozoneDaily2014)[1]),2), "%", sep="")))
colnames(vars2) <- c("VariableNumber","VariableNames", "DataClass", 
                     "NumberNA", "NumberBlank", "MissingPercentage") 
print (vars2)
print (knitr::kable(vars2, align='l', caption="Table Variables", padding=0))


####ASSIGNMENT

## Applying something (summary of all columns)
##Create a master summary table
# Create a table with a row for each variable
# Create columns representing descriptive information for numeric variables. Include:
## 1) Variable class
## 2) Min
## 3) Mean
## 4) Max
## 5) Standard deviation

#Create histogram plots for each character/factor variable and print out a table of its top 10 most common values

