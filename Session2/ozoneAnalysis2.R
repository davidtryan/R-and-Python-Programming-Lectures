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

ozoneDaily2014 <- read.csv('Documents/Work_KAP/RandPython_20150212/Examples_public/EPA/daily_44201_2014.csv')
ozoneDaily2014 <- read.csv('C:/Users/davidryan/Desktop/daily_44201_2014.csv')

ozoneHourly2014 <- read.csv('Documents/Work_KAP/RandPython_20150212/Examples_public/EPA/hourly_44201_2014.csv')
ozoneHoury2014 <- read.csv('C:/Users/davidryan/Desktop/hourly_44201_2014.csv')
tempHourly2014 <- read.csv('C:/Users/davidryan/Desktop/hourly_TEMP_2014.csv')

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

                  ############################
                  ## Generate data-type specific summaries for each variable in the table
                  
                  print (vars2$DataClass)
                  vars2_num <- names(vars2$DataClass[vars2$DataClass %in% c('numeric')])
                  vars2_int <- names(vars2$DataClass[vars2$DataClass %in% c('integer')])
                  vars2_log <- names(vars2$DataClass[vars2$DataClass %in% c('logical')])
                  vars2_fac <- names(vars2$DataClass[vars2$DataClass %in% c('factor')])
                  
                  summary(ozoneDaily2014$Sample.Measurement)
                  
                  varUniqueTable <- sort(unlist (lapply (ozoneDaily2014, function(x) length(unique(x)))))
                  #Split variables into classification variables with numeric labels and true numeric variables
                  #Parse out and appropriately label date and time variables from factors and variables with name or code in their name
                  
                  ## I know that the state codes and names and county codes are classifiers with numeric lables, so I
                  # will make the cutoff 137 (anything larger than that will be a true numeric variable)
                  
                  library("reshape2")
                  
                  varList <- vars2_num
                  
                  varList <- colnames(ozoneDaily2014)
                  for (i in 1:length(varList)) {
                    if (length(grep('Name',varList[i]))>=1 || length(grep('Code',varList[i]))>=1) {
                    #if it contains name or code (character variable with numeric name)
                    print (sprintf("%i) Name/Code Variable %s", i, varList[i]))
                    exploreTableGraph(i, advanced="NO")
                      
                    #Date/time variables
                    } else if (length(grep('Date',varList[i]))>=1) {
                      print (sprintf("%i) Date Variable %s", i, varList[i]))
                      print (summary(as.Date(ozoneDaily2014[,i])))    
                    } else if (length(grep('Time',varList[i]))>=1) {
                      print (sprintf("%i) Time Variable %s", i, varList[i]))
                      print (summary(ozoneDaily2014[,i]))
                    } else if (varList[i] %in% c(vars2_int, vars2_num)) {
                      print (sprintf("%i) Numeric Variable %s", i, varList[i]))
                      print (summary(ozoneDaily2014[,i]))
                    } else {
                      print (sprintf("%i) OTHER Variable %s", i, varList[i]))
                    }
                  }
                  
                  
                  x <- factor(c("9:00 PM","12:15 AM", "12:15 AM" ))
                  y <- as.POSIXct(x, format = "%I:%M %p", tz = "GMT") #read help("strptime")
                  
                  as.POSIXct(ozoneDaily2014$Time.Local[1], format = "%I:%M", tz = "GMT") #read help("strptime")
                  
                  install.packages("chron")
                  library(chron)
                  times(strftime(y, format = "%H:%M:%S", tz = "GMT"))
                  
                  
                  
                  exploreTableGraph <- function(i, advanced) {
                    varTable <- melt(table(ozoneDaily2014[,i]), maxsum=length(unique(ozoneDaily2014[,i])))
                    colnames(varTable) <- c("cat", "freq")
                    varTableOrd <- data.frame(cbind(as.character(varTable[order(-(varTable$freq)),][1:min(length(varTable$freq),10),1]), 
                                                    varTable[order(-(varTable$freq)),][1:min(length(varTable$freq),10),2]))
                    colnames(varTableOrd) <- colnames(varTable)
                    varTableOrd$freq <- as.numeric(as.character(varTableOrd$freq))
                    
                    ##### Plot #####
                    
                    varTableOrd_plot <- varTableOrd[!(varTableOrd$cat) %in% c("NA's", "N/A", "", "None", "none", "NONE"),]
                    varTableOrd_plot$cat <- sapply(1, function(x) sprintf("#%s %s", rownames(varTableOrd_plot), 
                                                                          strtrim(as.character(varTableOrd_plot$cat), 25)))            
                    
                    varTableOrd_miss <- varTableOrd[(varTableOrd$cat) %in% c("NA's", "N/A", "", "None", "none", "NONE"),]
                    
                    orderList <- as.character(varTableOrd_plot$cat)
                    varTableOrd_plot <- transform (varTableOrd_plot, cat=factor(cat, levels=orderList))
                    colnames(varTableOrd_plot) <- c('cat', 'freq')
                    
                    sumTabNames <- c('type:', '# of unique values:', '% NA / None / Blank:')
                    sumTabVals <- c(class(ozoneDaily2014[,i]), length(unique(ozoneDaily2014[,i])), 
                                    paste0(round(sum(varTableOrd_miss[,2])/length(ozoneDaily2014[,i])*100,4), '%'))
                    sumTab <- cbind(sumTabNames, sumTabVals)
                    colnames(sumTab) <- c()
                    sumTab <- cbind(c(sumTab[1:2,1],sprintf("# %s:",as.character(varTableOrd_miss[,1])),sumTab[3,1]), 
                                    c(sumTab[1:2,2],varTableOrd_miss[,2],sumTab[3,2]))
                    
                    g2 <- ggplot(data=varTableOrd_plot, aes(x=cat,  y=freq, fill=cat)) + geom_bar(stat='identity')
                    g2 <- g2 + theme(axis.text.x=element_text(angle=90, hjust=1), axis.title.x=element_blank(), 
                                     axis.title.y=element_blank(), legend.position='none')
                    g2 <- g2 + ggtitle(varList[i])
                    
                  #   print(varTable)
                    print(sumTab)
                    print(varTableOrd)  
                    print (g2)
                    
                    ##OPTIONAL
                    if (advanced=="YES") {
                      t1 <- tableGrob(data.frame(sumTab), cols=c('X1', 'X2'), gpar.coretext=gpar(fontsize=8, lineheight=1, cex=0.8),
                                      gpar.coltext=gpar(fontsize=8, lineheight=1, cex=0.8), 
                                      show.rownames=FALSE, show.colnames=FALSE,
                                      equal.height=TRUE, padding.v = unit(1.65, 'mm'), , padding.h = unit(5, 'mm'), 
                                      core.just='left', vjust='center', 
                                      gpar.corefill=gpar(fill=rgb(255, 255, 255, maxColorValue=255), alpha=1, col=NA),
                                      show.box=TRUE, separator='black')
                      
                      varTableOrd$cat <- sapply(1, function(x) strtrim(as.character(varTableOrd$cat), 35)) 
                      varTableOrd$cat <- sapply(lapply(varTableOrd$cat, strwrap, width=25), paste, collapse="\n")
                      
                      t2 <- tableGrob(varTableOrd, cols=c('cat', 'freq'), gpar.coretext=gpar(fontsize=8, lineheight=1, cex=0.8),
                                      gpar.coltext=gpar(fontsize=8, lineheight=1, cex=0.8),
                                      gpar.rowtext=gpar(fontsize=8, lineheight=1, cex=0.8),
                                      show.rownames=TRUE, show.colnames=TRUE,
                                      equal.height=TRUE, padding.v = unit(1.65, 'mm'), padding.h = unit(5, 'mm'), core.just='left',
                                      gpar.corefill=gpar(fill=rgb(255, 255, 255, maxColorValue=255), alpha=1, col=NA))              
                      
                      grid.arrange(g2, arrangeGrob(t1, t2, ncol=1, heights=c(0.3, 0.7)), ncol=2)
                    }
                  }



# #if unique length is less than something
#   if ((vars2$DataClass[i] %in% c('numeric', 'integer')) & length(unique(ozoneDaily2014[,i]))>15) {
#     print (sprintf('1) Numeric variable - %s %s %f',rownames(vars2)[i],vars2$DataClass[i],ozoneDaily2014[1,i]))
#   } else if ((vars2$DataClass[i] %in% c('numeric', 'integer')) & length(unique(ozoneDaily2014[,i]))<=15) {
#     print (sprintf('2) %s %s %f',rownames(vars2)[i],vars2$DataClass[i],ozoneDaily2014[1,i]))
#   } else if (vars2$DataClass[i] %in% c("POSIXct", "POSIXt")) {
#     print (sprintf('3) %s %s %i',rownames(vars2)[i],vars2$DataClass[i],ozoneDaily2014[1,i]))
#   } else if (c('TRUE') %in% (c('Y', 'N') %in% names(summary(ozoneDaily2014[,i])) & length(unique(ozoneDaily2014[,i]))<=4) & length(unique(ozoneDaily2014[,i]))<=4) {
#     print (sprintf('4) %s %s %i',rownames(vars2)[i],vars2$DataClass[i],ozoneDaily2014[1,i]))
#   } else {
#     print (sprintf('5) %s %s %s',rownames(vars2)[i],vars2$DataClass[i],ozoneDaily2014[1,i]))
#   }
# }







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











############################
## Using ANOVA to see if counties have different ozone levels

## Use t-tests



############################      
## Create samples

##Generate Samples
sample_holdout <- sampleGen(masterEnv1_sub)
sample_test <- sampleGen(masterEnv1_sub)

entryNums <- 1:nrow(masterEnv1)
sampNums_holdout <- sample(1:nrow(ozoneDaily2014), 0.1*nrow(ozoneDaily2014), replace=F)
ozoneDaily2014_holdout <- ozoneDaily2014[sampNums_holdout,]

sampNums_train <- entryNums[!entryNums %in% sampNums_holdout]
sampNums_train <- sample(sampNums_train, 0.4*(nrow(ozoneDaily2014)-length(sampNums_holdout)), replace=F)
ozoneDaily2014_train <- ozoneDaily2014[sampNums_train,]

sampNums_test <- entryNums[!entryNums %in% c(sampNums_holdout, sampNums_train)]
sampNums_test <- sample(sampNums_test, 0.3*(nrow(ozoneDaily2014)-length(sampNums_holdout)), replace=F)
ozoneDaily2014_test <- ozoneDaily2014[sampNums_test,]

sampNums_val <- entryNums[!entryNums %in% c(sampNums_holdout, sampNums_train, sampNums_test)]
ozoneDaily2014_val <- ozoneDaily2014[sampNums_val,]

sampleGen <- function(datatable) {
    
  entryNums <- 1:nrow(datatable)
  sampNums_holdout <- sample(entryNums, 0.1*nrow(datatable), replace=F)
  sample_holdout <- datatable[sampNums_holdout,]
#   samples[1] <- sample_holdout
  
  sampNums_train <- entryNums[!entryNums %in% sampNums_holdout]
  sampNums_train <- sample(sampNums_train, 0.4*(nrow(datatable)-length(sampNums_holdout)), replace=F)
  sample_train <- datatable[sampNums_train,]
#   samples[2] <- sample_train
  
  sampNums_test <- entryNums[!entryNums %in% c(sampNums_holdout, sampNums_train)]
  sampNums_test <- sample(sampNums_test, 0.3*(nrow(datatable)-length(sampNums_holdout)), replace=F)
  sample_test <- datatable[sampNums_test,]
#   return(sample_test)
  
  sampNums_val <- entryNums[!entryNums %in% c(sampNums_holdout, sampNums_train, sampNums_test)]
  sample_val <- datatable[sampNums_val,]    
#   return(sample_val)
  
  samples <- list(sample_holdout, sample_train, sample_test, sample_val)
  return(samples)
}

sampOzone <- sampleGen(ozoneDaily2014)


so2Daily2014 <- read.csv('Documents/Work_KAP/RandPython_20150212/Examples_public/EPA/daily_42401_2014.csv')
coDaily2014 <- read.csv('Documents/Work_KAP/RandPython_20150212/Examples_public/EPA/daily_42101_2014.csv')
no2Daily2014 <- read.csv('Documents/Work_KAP/RandPython_20150212/Examples_public/EPA/daily_42602_2014.csv')
tempDaily2014 <- read.csv('Documents/Work_KAP/RandPython_20150212/Examples_public/EPA/daily_TEMP_2014.csv')

so2Daily2014 <- read.csv('C:/Users/davidryan/Desktop/daily_42401_2014.csv')
coDaily2014 <- read.csv('C:/Users/davidryan/Desktop/daily_42101_2014.csv')
no2Daily2014 <- read.csv('C:/Users/davidryan/Desktop/daily_42602_2014.csv')
tempDaily2014 <- read.csv('C:/Users/davidryan/Desktop/daily_TEMP_2014.csv')

parameters <- c("State.Name", "County.Name", "CBSA.Name", "Address", "Parameter.Name", 
                "Date.Local", "Arithmetic.Mean", "X1st.Max.Value", "X1st.Max.Hour", "Units.of.Measure")
ozoneDaily2014_VA <- ozoneDaily2014[ozoneDaily2014$State.Name=="Virginia",parameters]
so2Daily2014_VA <- so2Daily2014[so2Daily2014$State.Name=="Virginia",parameters]
coDaily2014_VA <- coDaily2014[coDaily2014$State.Name=="Virginia",parameters]
no2Daily2014_VA <- no2Daily2014[no2Daily2014$State.Name=="Virginia",parameters]
tempDaily2014_VA <- tempDaily2014[tempDaily2014$State.Name=="Virginia",parameters]

cadd <- Reduce(intersect, list(ozoneDaily2014[,c("Address")], so2Daily2014[,c("Address")], coDaily2014[,c("Address")], no2Daily2014[,c("Address")], tempDaily2014[,c("Address")]))

ozoneDaily2014_cadd <- ozoneDaily2014[ozoneDaily2014$Address %in% cadd,]
so2Daily2014_cadd <- so2Daily2014[so2Daily2014$Address %in% cadd,]
coDaily2014_cadd <- coDaily2014[coDaily2014$Address %in% cadd,]
no2Daily2014_cadd <- no2Daily2014[no2Daily2014$Address %in% cadd,]
tempDaily2014_cadd <- tempDaily2014[tempDaily2014$Address %in% cadd,]

cadd2 <- Reduce(intersect, list(ozoneDaily2014_cadd[,c("Date.Local")], so2Daily2014_cadd[,c("Date.Local")], 
                                coDaily2014_cadd[,c("Date.Local")], no2Daily2014_cadd[,c("Date.Local")], 
                                tempDaily2014_cadd[,c("Date.Local")]))

ozoneDaily2014_cadd <- ozoneDaily2014_cadd[ozoneDaily2014_cadd$Date.Local %in% cadd2,]
so2Daily2014_cadd <- so2Daily2014_cadd[so2Daily2014_cadd$Date.Local %in% cadd2,]
coDaily2014_cadd <- coDaily2014_cadd[coDaily2014_cadd$Date.Local %in% cadd2,]
no2Daily2014_cadd <- no2Daily2014_cadd[no2Daily2014_cadd$Date.Local %in% cadd2,]
tempDaily2014_cadd <- tempDaily2014_cadd[tempDaily2014_cadd$Date.Local %in% cadd2,]

parameters2 <- c("State.Name", "Address", "Date.Local", "Parameter.Name", "Units.of.Measure", 
                 "Arithmetic.Mean", "X1st.Max.Value", "X1st.Max.Hour")
so2Daily2014_cadd <- so2Daily2014_cadd[,parameters2]
colnames(so2Daily2014_cadd) <- c("State.Name", "Address", "Date.Local", "so2.Parameter.Name", "so2.Units.of.Measure", 
                                 "so2.Arithmetic.Mean", "so2.X1st.Max.Value", "so2.X1st.Max.Hour")
masterEnv1 <- merge(ozoneDaily2014_cadd[,parameters2], so2Daily2014_cadd, by=c('State.Name', 'Address', 'Date.Local'))
coDaily2014_cadd <- coDaily2014_cadd[,parameters2]
colnames(coDaily2014_cadd) <- c("State.Name", "Address", "Date.Local", "co.Parameter.Name", "co.Units.of.Measure", 
                                 "co.Arithmetic.Mean", "co.X1st.Max.Value", "co.X1st.Max.Hour")
masterEnv1 <- merge(masterEnv1, coDaily2014_cadd, by=c('State.Name', 'Address', 'Date.Local'))
no2Daily2014_cadd <- no2Daily2014_cadd[,parameters2]
colnames(no2Daily2014_cadd) <- c("State.Name", "Address", "Date.Local", "no2.Parameter.Name", "no2.Units.of.Measure", 
                                 "no2.Arithmetic.Mean", "no2.X1st.Max.Value", "no2.X1st.Max.Hour")
masterEnv1 <- merge(masterEnv1, no2Daily2014_cadd, by=c('State.Name', 'Address', 'Date.Local'))
tempDaily2014_cadd <- tempDaily2014_cadd[,parameters2]
colnames(tempDaily2014_cadd) <- c("State.Name", "Address", "Date.Local", "temp.Parameter.Name", "temp.Units.of.Measure", 
                                 "temp.Arithmetic.Mean", "temp.X1st.Max.Value", "temp.X1st.Max.Hour")
masterEnv1 <- merge(masterEnv1, tempDaily2014_cadd, by=c('State.Name', 'Address', 'Date.Local'))

dim(masterEnv1)

masterEnv1_sub <- masterEnv1[,c("State.Name","Address","Date.Local","Arithmetic.Mean","so2.Arithmetic.Mean","co.Arithmetic.Mean","no2.Arithmetic.Mean","temp.Arithmetic.Mean")]
colnames(masterEnv1_sub) <- c("State.Name","Address","Date.Local","ozone.Arithmetic.Mean","so2.Arithmetic.Mean","co.Arithmetic.Mean","no2.Arithmetic.Mean","temp.Arithmetic.Mean")

##Generate Samples
samples <- sampleGen(masterEnv1_sub)
sample_holdout <- data.frame(samples[1])
sample_test <- data.frame(samples[2])
sample_train <- data.frame(samples[3])
sample_val <- data.frame(samples[4])











# parameters2 <- c("State.Name", "Date.Local","Arithmetic.Mean", "X1st.Max.Value", "X1st.Max.Hour", "Parameter.Name", "Units.of.Measure")
# masterEnv1 <- rbind(ozoneDaily2014[,parameters2], so2Daily2014[,parameters2], coDaily2014[,parameters2], no2Daily2014[,parameters2], tempDaily2014[,parameters2])
# 
# masterEnv1 <- masterEnv1
#   
# masterEnv <- tempDaily2014
# masterEnv$
# 
# parameters2 <- c("Address","State.Name", "Date.Local","Arithmetic.Mean", "X1st.Max.Value", "X1st.Max.Hour", "Parameter.Name", "Units.of.Measure")
# 
# masterEnv <- rbind(ozoneDaily2014[,parameters2], so2Daily2014[,parameters2], coDaily2014[,parameters2], no2Daily2014[,parameters2], tempDaily2014[,parameters2])

measNames <- colnames(sample_test)[-(1:3)]

for (i in 1:length(measNames)) {
  par(mfrow=c(1,2))
  plot(table(sample_test[,measNames[i]]),
       xlab=measNames[i], ylab='freq', main=sprintf('Histogram of %s', measNames[i]))
  plot(sample_test[,measNames[i]], sample_test[,c("temp.Arithmetic.Mean")],
       xlab=measNames[i], ylab='Temp', main=sprintf('Temp vs. %s', measNames[i]))
}

sample_test_norm <- sample_test
sample_test_norm[,measNames] <- lapply(sample_test[,measNames], function(x) ((x-min(x))/((max(x)-min(x)))))

for (i in 1:length(measNames)) {
  par(mfrow=c(1,2))
  plot(table(sample_test_norm[,measNames[i]]),
       xlab=measNames[i], ylab='freq', main=sprintf('Histogram of %s', measNames[i]))
  plot(sample_test_norm[,measNames[i]], sample_test_norm[,c("temp.Arithmetic.Mean")],
       xlab=measNames[i], ylab='Temp', main=sprintf('Temp vs. %s', measNames[i]))
}

plot(sample_test_norm[,measNames])

############################
## Create model of ozone based on pollutants (for washington, dc area vs. for general area)

# Inference in the multiple regression setting is typically performed in a number of
# steps. We begin by testing whether the explanatory variables collectively have an
# effect on the response variable
# If we can reject this hypothesis, we continue by testing whether the individual
# regression coefficients are significant while controlling for the other variables in
# the model.

ivs <- measNames[!measNames %in% c("temp.Arithmetic.Mean")]

tempLM1 <- lm(temp.Arithmetic.Mean ~ ozone.Arithmetic.Mean+so2.Arithmetic.Mean+co.Arithmetic.Mean+no2.Arithmetic.Mean, data=sample_test_norm)
summary(tempLM1)

# anova(tempLM4)

# The output shows that F = 441.8 (p < 2.2e-16), indicating that we should clearly
# reject the null hypothesis that the variables collectively have no
# effect on Temperature.

tempLM2 <- lm(temp.Arithmetic.Mean ~ ozone.Arithmetic.Mean+so2.Arithmetic.Mean+no2.Arithmetic.Mean, data=sample_test_norm)
summary(tempLM2)

anova(tempLM2, tempLM1)

# The output shows the results of the partial F-test. Since F=23.708 (p-value=1.128e-06)
# we can reject the null hypothesis.
# It appears that the variable CO does contribute significant
# information to the temperature once the other variables have been taken
# into consideration.

par(mfrow=c(2,2))
plot(tempLM1)

confint(tempLM1, level=0.95)
tempLM1_fitvals <- fitted(tempLM1)
par(mfrow=c(1,2))
plot(sample_test_norm$ozone.Arithmetic.Mean, sample_test_norm$temp.Arithmetic.Mean)
plot(sample_test_norm$ozone.Arithmetic.Mean, fitted(tempLM1), xlim=c(0,1), ylim=c(0,1))

# Calculate Relative Importance for Each Predictor
install.packages('relaimpo')
library(relaimpo)
calc.relimp(tempLM1,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples) 
boot <- boot.relimp(tempLM1, b = 1000, type = c("lmg", 
                                            "last", "first", "pratt"), rank = TRUE, 
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result





############################
## Refining model

ivs <- measNames[!measNames %in% c("temp.Arithmetic.Mean")]

plot(sample_test_norm[,ivs])    #NOTE: Strange outlier in SO2 data

sample_test_norm$so2.Arithmetic.Mean[which(sample_test_norm$so2.Arithmetic.Mean==max(sample_test_norm$so2.Arithmetic.Mean))] <- mean(sample_test_norm$so2.Arithmetic.Mean)
plot(sample_test_norm[,ivs])

tempLM1_adj <- lm(temp.Arithmetic.Mean ~ ozone.Arithmetic.Mean+so2.Arithmetic.Mean+co.Arithmetic.Mean+no2.Arithmetic.Mean, data=sample_test_norm)
summary(tempLM1_adj)

summary(tempLM1)

##Residual standard error: decreases from 0.1494 to 0.1491
##Multiple R-squared: increases from 0.06543 to 0.06878
##Adjusted R-squared: increases from 0.06528 to 0.06863
##F-statistic: increases from 441.8 to 466.1

tempLM2_adj <- lm(temp.Arithmetic.Mean ~ ozone.Arithmetic.Mean+so2.Arithmetic.Mean+no2.Arithmetic.Mean, data=sample_test_norm)
summary(tempLM2_adj)

anova(tempLM2, tempLM1)

# The output shows the results of the partial F-test. Since F=23.708 (p-value=1.128e-06)
# we can reject the null hypothesis.
# It appears that the variable CO does contribute significant
# information to the temperature once the other variables have been taken
# into consideration.

par(mfrow=c(2,2))
plot(tempLM1_adj)

confint(tempLM1_adj, level=0.95)
tempLM1_adj_fitvals <- fitted(tempLM1_adj)
par(mfrow=c(1,2))
plot(sample_test_norm$ozone.Arithmetic.Mean, sample_test_norm$temp.Arithmetic.Mean)
plot(sample_test_norm$ozone.Arithmetic.Mean, fitted(tempLM1_adj), xlim=c(0,1), ylim=c(0,1))

# Calculate Relative Importance for Each Predictor
install.packages('relaimpo')
library(relaimpo)
calc.relimp(tempLM1_adj,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples) 
boot <- boot.relimp(tempLM1_adj, b = 1000, type = c("lmg", 
                                                "last", "first", "pratt"), rank = TRUE, 
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result


############################   
##Predicting

# Obtain a 95% confidence interval 
ci95 <- data.frame(predict(tempLM1,sample_test_norm[,ivs],interval="confidence"))

# Obtain a 95% prediction interval
pi95 <- predict(tempLM1,sample_test_norm[,ivs],interval="prediction")

############################      
## Plotting example

tempSO2_adj <- lm(temp.Arithmetic.Mean ~ so2.Arithmetic.Mean, data=sample_test_norm)

par(mfrow=c(1,1))
plot(temp.Arithmetic.Mean ~ so2.Arithmetic.Mean, data=sample_test_norm)

# predicts + interval
sample_train_norm <- lapply(sample_train[,measNames], function(x) ((x-min(x))/((max(x)-min(x)))))
newx <- sample_train_norm$so2.Arithmetic.Mean
preds <- predict(tempSO2_adj, newdata = data.frame(so2.Arithmetic.Mean=newx), 
                 interval = 'confidence')

# plot
plot(temp.Arithmetic.Mean ~ so2.Arithmetic.Mean, data=sample_test_norm)
# add fill
# plot(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]))
polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = 'gray', border = "red", density=c(10))
# polygon(c(df$x,rev(df$x)),c(df$L,rev(df$U)),col = "grey75", border = FALSE)
# model
abline(tempSO2_adj)
# intervals
lines(newx, preds[ ,3], lty = 'dashed', col = 'red')
lines(newx, preds[ ,2], lty = 'dashed', col = 'red')


############################      
## Time series forecasting
      
############################
## Montecarlo simulation
      
############################
## Saving plots and pdfs
      
############################
## Model report generation
      
############################
## Knitr / markdown generation

# Use PCA to confirm selection of variables
# Use regression + one other modeling type (linear? logistic? neural net?)
      
      ## FUnction general / calling (breaking up programs / optimizing code)

# (subsets and bootstrap)

