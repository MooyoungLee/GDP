### GDP Case Study 1
### Data Analysis
### Mooyoung Lee 3/11/2017

## Question 1: How many of the IDs match?
q1_noIdMatch = sum(!is.na(match(gdpClean$CountryCode, stat$CountryCode)))       # Number of ID matching
print(paste("ID Matching from Data Merging: ", q1_noIdMatch))
# Country list that exist from both data files
CountryMatching = gdpClean[match(gdpClean$CountryCode, stat$CountryCode)>0,c("CountryCode","Economy")]
CountryMatching = na.omit(CountryMatching)

# Country list that are existing only on GDP data file
CountryNotMatchingGDP = gdpClean[is.na(match(gdpClean$CountryCode, stat$CountryCode)), c("CountryCode","Economy")]

# Country list that are existing only on STAT data file
CountryNotMatchingSTAT = stat[is.na(match(stat$CountryCode, gdpClean$CountryCode)), c("CountryCode","Long.Name")]

# Showing countries that is matching(green) and not matching(red) from merging process
cat("Countries that is matching(green) and not matching(red) from merging process (Figure below)\n")
map(resolution = 0)
map('world', regions=CountryMatching[,1], fill=TRUE, col='green', add = TRUE)
map('world', regions=CountryMatching[,2], fill=TRUE, col='green', add = TRUE)
# map('world', regions=CountryNotMatchingGDP[,1], fill=TRUE, col='red', add = TRUE)
map('world', regions=CountryNotMatchingGDP[,2], fill=TRUE, col='red', add = TRUE)
# map('world', regions=CountryNotMatchingSTAT[,1], fill=TRUE, col='orange', add = TRUE)
# map('world', regions=CountryNotMatchingSTAT[,2], fill=TRUE, col='orange', add = TRUE)

## Question 2: Waht is the 13th conuntry in the resulting data frame?
gdpRank = rank(gdpStatSort$Ranking, ties.method = c("average"))                                   # Rank the Ranking value
q2_13thLowGdp = gdpStatSort$Long.Name[gdpRank==gdpRank[13]]                                       # the 13th lowest GDP country; considered tie rank
print(paste("13th Lowest GDP Country: ", q2_13thLowGdp))

# Plotting the 13 lowest GDP countries
gdpStatSort2 = gdpStatSort[1:13,c("Economy", "GDPMilDollar")]
ggbar_13LowGDP = ggplot(gdpStatSort2, aes(x = factor(Economy, levels=unique(as.character(Economy))), 
                         y = GDPMilDollar))+
                    geom_bar(stat = 'identity', fill = rainbow(13))+
                    xlab("Country")+
                    ylab("GDP, Mil-Dollar")+
                    theme(axis.text.x = element_text(face="bold", color="black",size=12, angle=90))+
                    ggtitle("GDP: Lowest 13 Countries")+
                    geom_text(aes(label = c("1st","2nd","3rd", paste(c(4:13),"th")), vjust = "outward", hjust = "outward"))
cat("Countries within the 13th lowest GDP Ranking (Below Plot)")
print(ggbar_13LowGDP)

## Question 3: Avg GDP rankings of "High income: OECD" and "High income: nonOECD" groups?
q3_avgRank.HiOecd = mean(gdpStatSpread$Ranking[gdpStatSpread$`High income: OECD`>0], na.rm=TRUE)                        # Avg GDP ranking of "High income: OECD"
q3_avgRank.HiNonOecd =mean(gdpStatSpread$Ranking[gdpStatSpread$`High income: nonOECD`>0], na.rm=TRUE)                   # Avg GDP ranking of "High income: OECD"
q3_avgRank = c(q3_avgRank.HiOecd, q3_avgRank.HiNonOecd)                                                                 # Group two answers together

## Question 4: Show distribution of GDP by income group
q4_box = ggplot(data = gdpIncomeGather, mapping = aes(x = Income.Group, y = GDPMilDollar, color =Income.Group ))+geom_boxplot()  # GDP Boxplot by Income Group

# q4_HistDodge = ggplot(data = gdpIncomeGather)+
#                     geom_histogram(mapping = aes(x = GDPMilDollar, fill = Income.Group), 
#                                    binwidth = 100000, position = "dodge")             # GDP Histogram by Income Group

q4_HistIndiv = ggplot(data = gdpIncomeGather)+
                    geom_histogram(mapping = aes(x = GDPMilDollar, fill = Income.Group), 
                                   binwidth = 250000, position = "dodge") +
                    facet_wrap(~ Income.Group, nrow = 2)                                                                                    # GDP Histogram by Income Group individually plotted

cat("Box Plot: GDP by Income Group\n")
print(q4_box)
cat("Histogram: GDP by Income Group\n")
print(q4_HistIndiv)

## Question 5: Summary Stats of GDP by income groups
q5_gdpSummary = summary(gdpStatSpread[,35:39])                                  # Summary Stat of GDP by income Groups
cat("********************************************************************************\n")
cat("GDP Summary Table\n")
cat("********************************************************************************\n")
print(q5_gdpSummary)

cat("Total GDP by Income.Group\n")
gdpTotal = colSums(gdpStatSpread[,35:39], na.rm = TRUE)
print(gdpTotal)
cat("Total GDP by Income.Group (RATIO)\n")
gdpTotalRatio = colSums(gdpStatSpread[,35:39], na.rm = TRUE)/sum(gdpStatSpread[,35:39], na.rm = TRUE)
print(gdpTotalRatio)
cat("GDP Ratio: U.S. vs. Rest OECD Countries\n")
countryOECD = na.omit(gdpStatSpread[,c("Economy","High income: OECD")])
countryOECD = countryOECD[order(countryOECD$`High income: OECD`, decreasing = TRUE),]
gdpOECDoverUS = countryOECD$`High income: OECD`/countryOECD[1,2]
barplot(gdpOECDoverUS, main = "OECD Country GDP Ratio to U.S. GDP",
        xlab = "OECD Country GDP Rank",
        col = rainbow(nrow(countryOECD)),
        legend = countryOECD$Economy[1:10])
cat("GDP RAtio: U.S. GDP vs. All other OECD Country Total\n")
print(c(1,sum(gdpOECDoverUS)-1))
cat("GDP RAtio: U.S. GDP vs. All other Nations\n")
print(c(countryOECD[1,2],sum(gdpStatSpread[,35:39], na.rm = TRUE)-countryOECD[1,2])/countryOECD[1,2])
noCountryGroup = colSums(!is.na(gdpStatSpread[,35:39]))
cat("Total Country Number by Income Group\n")
print(noCountryGroup)


## Question 6a: Spread GDP Ranking values into 5 quantile groups. Make a table versus Income.Group.
RIvector = c(1:nrow(gdpRankIncome))                                             # A vector has a same length as GDP table w/o NAs
RowSplit = split(RIvector, cut(RIvector, quantile(RIvector, probs = seq(0, 1, by = 0.2)),
                               include.lowest=TRUE))                            # Cut row numbers into 5 groups

for (i in 1:5){
                    assign(paste("RankQ",i, sep = ""), spread(gdpRankIncome[unlist(RowSplit[i]),],Income.Group, Ranking))
}                                                                               # Assign 5 quantile tables into RankQ1~5 tables.                                                                             # Copying Ranking values into corresponding Income groups;neat spreading

cat("********************************************************************************\n")
cat("GDP Ranking: 5 Quantile Groups\n")
cat("********************************************************************************\n")
cat("GDP Ranking Q1:\n")
print(RankQ1)
cat("GDP Ranking Q2:\n")
print(RankQ2)
cat("GDP Ranking Q3:\n")
print(RankQ3)
cat("GDP Ranking Q4:\n")
print(RankQ4)
cat("GDP Ranking Q5:\n")
print(RankQ5)

cat("GDP Summary for the First Quantile group\n")
q1Stat = summary(RankQ1)
print(q1Stat)

# High income: OECD Countries from each continent
oecdRegion = gdpStat[gdpStat$Income.Group=="High income: OECD", c("CountryCode","Economy","Region")]
oecdRegion = na.omit(oecdRegion)


## Selecting OECD countries in Europe and Plotting
oecdEurope = spread(oecdRegion, Region, CountryCode)
oecdEurope = na.omit(oecdEurope[,c(1,3)])


# getting the map
oecdEuropeCoord = geocode(oecdEurope[,1])                                       # Get coordinates of each country
df = oecdEuropeCoord                                                            # define data frame
map_oecdEurope <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = 3,
                      maptype = "roadmap", scale = 2)

# plotting the map with some points on it
MapEuropeOECD = ggmap(map_oecdEurope) +
                    geom_point(data = df, aes(x = lon, y = lat, fill = "red", alpha = 0.8), size = 5, shape = 21) +
                    guides(fill=FALSE, alpha=FALSE, size=FALSE)


cat("OECD countries in Europe (Figure Below)\n")
print(MapEuropeOECD)

## Question 6b: How many countries are in Lower middle income group but among the top 38 GDP nations?
q6_LMIRankAmong38 = sum(gdpStatSort$Ranking <= 38 & gdpStatSort$Income.Group =="Lower middle income", na.rm = TRUE)       # Number of countries among top 38 ranking
cat("Number of Countries in Lower middle income group and GDP Rank within 38th\n")
print(q6_LMIRankAmong38)

## Identifying the five countries from the above question
LowMidHigh38 = na.omit(gdpStat$Long.Name[gdpStat$Income.Group=="Lower middle income" & gdpStat$Ranking <=38])
# getting the map
df = geocode(LowMidHigh38)                                                      # Get coordinates of each country
map_LowMidGdpWithin38 = get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = 2,
                      maptype = "roadmap", scale = 2)

# plotting the map with some points on it
Map5countryQ6 = ggmap(map_LowMidGdpWithin38) +
                    geom_point(data = df, aes(x = lon, y = lat, fill = "red", alpha = 0.8), size = 5, shape = 21) +
                    guides(fill=FALSE, alpha=FALSE, size=FALSE)

cat("Countries are in Lower middle income group and GDP Rank within 38th (Figure Below)\n")
print(Map5countryQ6)
