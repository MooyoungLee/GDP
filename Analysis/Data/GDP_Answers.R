### GDP Case Study 1
### Data Analysis
### Mooyoung Lee 3/11/2017

## Case Study Answers

# Question 1: How many of the IDs match?
print(paste("ID Matching: ", q1_noIdMatch))

# Question 2: Waht is the 13th conuntry in the resulting data frame?
print(paste("13th Country: ", q2_13thLowGdp))

# Question 3: Avg GDP rankings of "High income: OECD" and "High income: nonOECD" groups?
print(paste("AVG Ranking (High OECD, High nonOECD) : ",round(q3_avgRank[1],2),", ", round(q3_avgRank[2],2)))

# Question 4: Show distribution of GDP by income group
cat("Box Plot: GDP by Income Group\n")
print(q4_box)
cat("Histogram: GDP by Income Group\n")
print(q4_HistIndiv)

# Question 5: Summary Stats of GDP by income groups
cat("********************************************************************************\n")
cat("GDP Summary Table\n")
cat("********************************************************************************\n")
print(q5_gdpSummary)

# Question 6a: Spread GDP Ranking values into 5 quantile groups. Make a table versus Income.Group.
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

# Question 6b: How many countries are Lower middle income but among the 38 nations with highest GDP?
cat("Number of Countries in Lower middle income group and GDP Rank within 38th\n")
print(q6_LMIRankAmong38)
