#new share eligiblity

#my hypothesis:
# i think what you do is find the average size of a household in a county, 
# then calculate a "FPL" for the average size of the houses, 
# then convert the FPL percentages to actual dollar values, 
# then you find how many houses are in that size of a bracket by 
# interpolating the income brackets, and then divide that by the total number of houses. i think that works.

# family vs. non-family households should be weighted
# based on this: https://www.kff.org/wp-content/uploads/sites/2/2013/09/412630-opting-in-medicaid.pdf


library(readr)
library(waldo)
#sizeInfo has the size of households, incomeInfo has income brackets based on taxes (i think)
sizeInfo <- read_csv("rawData/ShareEligibility Data/sizeInfo5Y.csv", na = c("-"))
sizeLabels <- read_csv("rawData/ShareEligibility Data/sizeInfoLabels.csv")
incomeInfo <- read_csv("rawData/ShareEligibility Data/incomeInfo5Y.csv", na = c("-"))
incomeLabels <- read_csv("rawData/ShareEligibility Data/incomeInfoLabels.csv")
povertyLine2014 <- read_delim("rawData/ShareEligibility Data/poverty line 2014.tsv", delim = "\t", escape_double = FALSE, trim_ws = TRUE, skip = 2)
parentvsnonparentInfo <- read_tsv("rawData/ShareEligibility Data/uninsured adults newly eligible for Medicaid by the 1000s.tsv", skip = 1)

# "S1101_C01_002E","Total!!Estimate!!Average household size"
# "S1101_C02_002E","Married-couple family household!!Estimate!!Average household size"
# "S1101_C03_002E","Male householder, no wife present, family household!!Estimate!!Average household size"
# "S1101_C04_002E","Female householder, no husband present, family household!!Estimate!!Average household size"
# "S1101_C05_002E","Nonfamily household!!Estimate!!Average household size"

# "S1101_C01_001E","Total!!Estimate!!Total households"
# "S1101_C02_001E","Married-couple family household!!Estimate!!Total households"
# "S1101_C03_001E","Male householder, no wife present, family household!!Estimate!!Total households"
# "S1101_C04_001E","Female householder, no husband present, family household!!Estimate!!Total households"
# "S1101_C05_001E","Nonfamily household!!Estimate!!Total households"

#check to make sure total is a combination of the other four
x <- weighted.mean(c(mean(sizeInfo$S1101_C02_002E, na.rm=TRUE),
                mean(sizeInfo$S1101_C03_002E, na.rm=TRUE),
                mean(sizeInfo$S1101_C04_002E, na.rm=TRUE),
                mean(sizeInfo$S1101_C05_002E, na.rm=TRUE)), 
              c(mean(sizeInfo$S1101_C02_001E, na.rm=TRUE),
                mean(sizeInfo$S1101_C03_001E, na.rm=TRUE),
                mean(sizeInfo$S1101_C04_001E, na.rm=TRUE),
                mean(sizeInfo$S1101_C05_001E, na.rm=TRUE)))
print(x)
mean(sizeInfo$S1101_C01_002E)
#close enough, the NAs might be the discrepancy

# find the average size of a household in a county OF THOSE WHO MEDICAID EXPANDED TO
#1) make weighted averages of a parent's household size by combining C02, C03, and C04 for each county
#2) use the parentvsnonparentInfo state information to find how much to weight parents vs nonparents in each county
#3 weight #1 by #2

#1) make weighted averages of parent size and nonparent size
#look at S1101_C01_005E

#SKIPPING THIS FOR NOW, just settling for total average of each county
calculationDataframe <- data.frame(GEO_ID = sizeInfo$GEO_ID, avgHouseholdSize = sizeInfo$S1101_C01_002E)
