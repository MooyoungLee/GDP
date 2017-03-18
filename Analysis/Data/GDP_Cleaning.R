### GDP Case Study 1
### Data Cleaning   
### Mooyoung Lee 3/11/2017

## Cleaning GDP table
gdpClean = gdp[, colSums(is.na(gdp)) != nrow(gdp)]                              # Delete columns with NA values only
colnames(gdpClean) = c("CountryCode", gdpClean[4,2:3], "GDPMilDollar", "Note")  # Assign column names
gdpNote = gdpClean[239:241,2]                                                   # Save the original NOTE on the bottom of the table in case
gdpClean = gdpClean[nchar(gdpClean[,1])>0,]                                     # Delete all emplty rows
gdpClean$GDPMilDollar = as.numeric(gsub(",","",gdpClean$GDPMilDollar))          # Save GDP values as numeric
gdpClean$Ranking = as.numeric(gdpClean$Ranking)                                 # Save Ranking values as numeric

## Merging data tables
gdpStat = merge(gdpClean, stat, by = intersect("CountryCode", "CountryCode"), all = TRUE) # Merge by contury code and sorting
gdpStatSort = gdpStat[order(gdpStat$GDPMilDollar),]                                       # Sort gdpStat table by GDP

## Manipulating tables in order to answer questions #3-#6
gdpStatSpread = spread(gdpStatSort, Income.Group, GDPMilDollar)                 # Spread GPD value into multiple Income group columns
gdpIncomeGather = gather(subset(gdpStatSpread, select =  c(35:39)),
                         "Income.Group", "GDPMilDollar", na.rm=TRUE)            # Gathering only useful Income Group data
gdpRankIncome = gdpStatSort[,c("Economy", "Ranking", "Income.Group")]           # Subset Ranking and Income group
gdpRankIncome = gdpRankIncome[!is.na(gdpRankIncome$Ranking),]                   # Select NA values from Ranking column
gdpRankIncome = gdpRankIncome[order(gdpRankIncome$Ranking),]                    # Sort ascending order