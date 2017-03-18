### GDP Case Study 1
### Data Importing
### Mooyoung Lee 3/11/2017

## Downloading GDP and Country detail information from an online source, which is given for this Case Study.

download("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", destfile = "gdp.csv")              # Download GDP data
download("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv", destfile = "stat.csv") # Download country stats

gdp = read.csv("gdp.csv", sep = ",",stringsAsFactors = FALSE, header = FALSE)   # Open GDP file
stat = read.csv("stat.csv", sep = ",",stringsAsFactors = FALSE, header = TRUE)  # Open country stat file