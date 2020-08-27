#Assignment 1


#NAME : AYUSH KUMAR
#NETID: ayk19003

setwd("C:/Users/kumar/Downloads/Courses/project/R/Assignment1")


###Question 1 opening data
townList <- read.csv("List_of_Towns.csv")
realEstate <- read.csv("Real_Estate_Sales_2014-2016.csv")




###Question 1 merging data
#To extract County from townList and Merge it with realEstate, we have to find
# a common column in both the datases
#In order to merge the datasets townList and realEstate,
#we can use the column common in both the datsets, i.e., Town
#Using merge function.
#The merged dataset is named as mergedSet

mergedData <- merge(x = townList,
                   y = realEstate,
                   by.x = "Town",
                   by.y = "Town")
#mergedDataset has 17 columns




###Question 2 Filtering out missing data
#In order to find missing data, we can check the summary to see which columns have missing data
summary(mergedData)

#As we can see, ResidentialType, Address and SalesAmount have NA's
#To check unidentified column types, using str()
str(mergedData)
#we are not able to identify the data type of date recorded
CheckMissingData <- mergedData[is.na(mergedData$DateRecorded),]
#Shows there are no missing values, which is not the case
#So we'll set the date fromat using lubridate

library("lubridate")
#We'll use lubridate to change the format of dates in DateRecorded column
mergedData$DateRecorded = lubridate::mdy(mergedData$DateRecorded)

missingDataCheck1 <- mergedData[is.na(mergedData$DateRecorded),]
# We got 6 observations with missing values in DateRecorded column
missingDataCheck2 <- mergedData[is.na(mergedData$SaleAmount),]
# We got 5283 observations with missing values in SaleAmount column
missingDataCheck3 <- mergedData[is.na(mergedData$ResidentialType),]
# We got 11905 observations with missing values in ResidentialType column
missingDataCheck4 <- mergedData[is.na(mergedData$Address),]
# We got 2 observations with missing values in Address column



###Question 3
#Replacing missing dateRecorded data 
missingDataCheck1$DateRecorded = "01-01-2014"
#which(is.na(missingDataCheck1$DateRecorded))
missingDataCheck1$DateRecorded = lubridate::mdy(missingDataCheck1$DateRecorded)

#Replacing missing Sales Amount data
missingDataCheck2$SaleAmount = missingDataCheck2$AssessedValue

#Adding subset to the dataseet
clean = mergedData[is.na(mergedData$DateRecorded)==F,]
clean = clean[is.na(clean$SaleAmount)==F,]
#combining the subsets
mergedSetFinal = rbind(clean,missingDataCheck1,missingDataCheck2)

###Question 4 Binning
mergedSetFinal$PropertyValue = ifelse(mergedSetFinal$AssessedValue <= 300000,"LowRange",
                                 ifelse(mergedSetFinal$AssessedValue > 800000,"HighRange","MidRange"))

###Question5 measure of central tendancy and variability
#Using describe function to get statistical values
salesAmountDesc = psych::describe(mergedSetFinal$SaleAmount)
salesAmountDesc
AssessedValDesc = psych::describe(mergedSetFinal$AssessedValue)
AssessedValDesc


mergedSetFinal$populationValDesc = gsub(",","",mergedSetFinal$Population..in.2010.)
mergedSetFinal$populationValDesc1 = as.numeric(mergedSetFinal$populationValDesc)
populationValDesc2 = psych::describe(mergedSetFinal$populationValDesc1)
populationValDesc2

###Question 6
library(ggplot2)
par(mfrow=c(2,2))
# top left
plot(density(mergedSetFinal$ListYear, main="List Year", xlab = "List Year"))
# top right
hist(mergedSetFinal$ListYear, main="List Year",xlab = "List Year")
# bottom left
boxplot(mergedSetFinal$ListYear, main= "List Year", xlab = "List Year")
# bottom right
hist(mergedSetFinal$ListYear, xlab = "List Year")
lines(mergedSetFinal$ListYear,xlab = "List Year", type = "l", 
      col = "red" , lty = 2 , lwd = 1.5)



###Question 7
install.packages("plyr")
library("plyr")
#To get the frequency table, I used count function to calculate # properties per town
frequencyTown = count(mergedSetFinal, "Town")
frequencyTown

#Multidimensional frequency table
#Again using count function to get the multidimensional frequency table
multiDimFreq = count(mergedSetFinal, c('Town','County','ListYear'))
multiDimFreq

#Contingency table
tableC = table(mergedSetFinal$PropertyValue)
tableC
#tablec gives the count per category, now we need percentage
#To calculate percentage, first I get the sum of the row
tableSum <- data.frame(rbind(tableC))
sum1 = tableSum$HighRange + tableSum$LowRange + tableSum$MidRange
#Calculating percentage per column by dividing each entry with sum
col1 = (tableSum$HighRange/sum1)*100
col2 = (tableSum$LowRange/sum1)*100
col3 = (tableSum$MidRange/sum1)*100
#Final table
finalTable <- cbind(col1, col2, col3)
finalTable




