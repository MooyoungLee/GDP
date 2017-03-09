### GDP Case Study 1
### Data Cleaning
### Mooyoung Lee 3/6/2017

# Data download

library(downloader)

download("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", destfile = "gdp.csv")
download("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv", destfile = "stat.csv")

gdp = read.csv("gdp.csv", sep = ",",stringsAsFactors = FALSE, header = FALSE)
stat = read.csv("stat.csv", sep = ",",stringsAsFactors = FALSE, header = TRUE)

# Clean gdp table
gdpClean = gdp[, colSums(is.na(gdp)) != nrow(gdp)]                              # Delete columns with NA values only
colnames(gdpClean) = c("CountryCode", gdpClean[4,2:3], "GDPMilDollar", "Note")  # Assign column names
gdpNote = gdpClean[239:241,2]                                                   # Saving the original NOTE on the bottom of the table in case
gdpClean = gdpClean[nchar(gdpClean[,1])>0,]                                     # Delete all emplty rows
gdpClean$GDPMilDollar = as.numeric(gsub(",","",gdpClean$GDPMilDollar))          # Saving GDP values as numeric
gdpClean$Ranking = as.numeric(gdpClean$Ranking)                                 # Saving Ranking values as numeric

# Merge data tables

gdpStat = merge(gdpClean, stat, by = intersect("CountryCode", "CountryCode"), all = TRUE) # Merging by contury code and sorting

# Question 1: How many of the IDs match?
q1_noIdMatch = sum(!is.na(match(gdpClean$CountryCode, stat$CountryCode)))  # Number of ID matching

# Question 2: Waht is the 13th conuntry in the resulting data frame?
gdpStatSort = gdpStat[order(gdpStat$GDPMilDollar),]         # Sorting gdpStat table by GDP
q2_13thLowGdp = gdpStatSort$Long.Name[13]                                   # the 13th lowest GDP country

# Question 3: Avg GDP rankings of "High income: OECD" and "High income: nonOECD" groups?
library(tidyr)
gdpStatSpread = spread(gdpStatSort, Income.Group, GDPMilDollar) # Spreading GPD value into multiple Income group columns
q3_avgRank.HiOecd = mean(gdpStatSpread$Ranking[gdpStatSpread$`High income: OECD`>0], na.rm=TRUE) # Avg GDP ranking of "High income: OECD"
q3_avgRank.HiNonOecd =mean(gdpStatSpread$Ranking[gdpStatSpread$`High income: nonOECD`>0], na.rm=TRUE) # Avg GDP ranking of "High income: OECD"
q3_avgRank = c(q3_avgRank.HiOecd, q3_avgRank.HiNonOecd) # Grouping two answers in Question3 together

# Question 4: Show distribution of GDP by income group
# dim(gdpStatSpread)                    # Checking dimensions of the spreaded table
gdpIncomeGather = gather(subset(gdpStatSpread, select =  c(35:39)), "Income.Group", "GDPMilDollar", na.rm=TRUE)  # Gathering only useful Income Group data
library(ggplot2)

ggplot(data = gdpIncomeGather, mapping = aes(x = Income.Group, y = GDPMilDollar, color =Income.Group ))+geom_boxplot() # GDP Boxplot by Income Group

ggplot(data = gdpIncomeGather)+
                    geom_histogram(mapping = aes(x = GDPMilDollar, fill = Income.Group), binwidth = 100000, position = "dodge")  # GDP Histogram by Income Group

ggplot(data = gdpIncomeGather)+
                    geom_histogram(mapping = aes(x = GDPMilDollar, fill = Income.Group), binwidth = 250000, position = "dodge") +
                    facet_wrap(~ Income.Group, nrow = 2)           # GDP Histogram by Income Group individually plotted

# Question 5: Summary Stats of GDP by income groups
q5_gdpSummary = summary(gdpStatSpread[,35:39]) # Summary Stat of GDP by income Groups

# Question 6a: Spread GDP Ranking values into 5 quantile groups. Make a table versus Income.Group.

gdpRankSpread = spread(gdpStatSort, Income.Group, Ranking)  # Spreading GPD Ranking into multiple Income group columns
gdpRankSpread = gdpRankSpread[,35:39]                       # Selecting only income group columns

rankSampleNum = colSums(!is.na(gdpRankSpread))              # Count ranking values for each income group
# incomeNames = colnames(gdpRankSpread)                       # Checking income group names

q6_rankIncomeTable = data.frame("High income: nonOECD" = NA, 
                             "High income: OECD" = NA, 
                             "Low income" = NA, 
                             "Lower middle income" = NA, 
                             "Upper middle income" = NA, 
                             void = numeric(max(rankSampleNum)) )        # Creating an NA data frame
q6_rankIncomeTable = subset(rankIncomeTable, select = -void)                # Deleting void column; used for sizing the table              
for (i in 1:5){
                    rankIncomeTable[1:rankSampleNum[i],i] = gdpRankSpread[!is.na(gdpRankSpread[,i]),i]
}                                                                               # Copying Ranking values into corresponding Income groups

# Question 6b: How many countries are Lower middle income but among the 38 nations with highest GDP?

q6_LMIRankAmong38 = sum(rankIncomeTable[,"Lower.middle.income"] <= 38) # Number of countries among top 38 ranking



