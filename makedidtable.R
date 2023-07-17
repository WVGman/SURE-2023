library(did)
library(waldo)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

hrtDisease <- read_csv("./formattedData/formattedAtlasDatawStateSorted.csv")
logarithmEmployment <- read_csv("./formattedData/formattedLogEmployment.csv")
logarithmPopulation <- read_csv("./formattedData/formattedLogPopulation.csv")
obesity <- read_csv("./formattedData/nationwideObesityByYear1999-2018.csv")
percentNewlyEligible <- read_csv("./formattedData/formattedPercentNewlyEligible.csv")
medicaidYearOfExpansion <- read_csv("./formattedData/medicaidAllYearsOfExpansion.csv")
statesFips <- read_csv("./formattedData/statesAndFIPS.csv")
percentInsured <- read_csv("./formattedData/percentInsured.csv")
#hrtDisease 2006-2019, logEmploy & logPop 2010-2021, percentNewlyEligible 2008-2020
#ideally i could easily get data for employment and population for before 2010, but apparently not
years <- 2010:2019 #for comparison in treated
yearsFormatted <- paste("X", 10:19, sep="") #for accessing year columns in datasets
medianIncome <- read_csv("./formattedData/medianIncomePerYear2010-2021.csv")

#remove useless columns, standardize names
hrtDisease[2:3] <- NULL 
names(hrtDisease) <- c("GEO_ID", "X06","X07", "X08", "X09", paste("X", 10:19, sep=""))
names(percentInsured) <- c("GEO_ID", "X06","X07", "X08", "X09", paste("X", 10:20, sep=""))

#create geo_ids list
GEO_IDs <- intersect(intersect(intersect(hrtDisease$GEO_ID, logarithmEmployment$GEO_ID), intersect(logarithmPopulation$GEO_ID, percentNewlyEligible$GEO_ID)), intersect(percentInsured$GEO_ID, medianIncome$GEO_ID))
#check the counties that didn't make the cut (mostly alaskan census areas, us territories, state data that I didn't remove)
symdiffGEO_IDs <- symdiff(symdiff(symdiff(hrtDisease$GEO_ID, logarithmEmployment$GEO_ID), symdiff(logarithmPopulation$GEO_ID, percentNewlyEligible$GEO_ID)), symdiff(percentInsured$GEO_ID, medianIncome$GEO_ID))

#combine medicaidYearOfExpansion and statesFips
medicaidYearOfExpansion$fip <- integer(50)
for(x in 1:length(medicaidYearOfExpansion$state)){
  medicaidYearOfExpansion$fip[x] <- statesFips$FIP[statesFips$state==medicaidYearOfExpansion$state[x]]
}

#sort the tables
hrtDisease <- arrange(hrtDisease, GEO_ID)
percentInsured <- arrange(percentInsured, GEO_ID)
logarithmPopulation <- arrange(logarithmPopulation, GEO_ID)
logarithmEmployment <- arrange(logarithmEmployment, GEO_ID)
percentNewlyEligible <- arrange(percentNewlyEligible, GEO_ID)
medianIncome <- arrange(medianIncome, GEO_ID)
GEO_IDs <- sort(GEO_IDs)

GEO_IDs <- GEO_IDs[GEO_IDs %/% 1000 %in% statesFips$FIP] #

#pretty much since you know all the GEO_IDs are in all the tables, and you know the tables are sorted
#you can just increment forwards in every table until you hit the right geo, effectively skipping any extras
#and there's no need to search for anything
#add_row is the main time-sucker, there's probably a better way but im lazy and unknowledgable
hrtIndex <- 1
insurIndex <- 1
popIndex <- 1
employIndex <- 1
eligIndex <- 1
incomeIndex <- 1
didTable <- data.frame(GEO_ID = integer(), YEAR = integer(), TREATED=logical(), YEAR_TREATED = integer(), REL_YEAR=integer(), HRTDISEASE = numeric(), INSURANCERATE = numeric(), LOGPOP = numeric(), LOGEMPLOY = numeric(), OBESITY = numeric(), PERCELIGIBLE = numeric(), MEDIANINCOME = numeric())
for(geo in GEO_IDs){
  for(yearIndex in 1:length(years)){
    
    #gets year treated
    year_treated <- medicaidYearOfExpansion$year[medicaidYearOfExpansion$fip==(geo %/% 1000)]
    if(year_treated == 999999){
      year_treated <- 0 #requirement of the att_gt function
    }
    
    #creates a 0 or 1 in treated if the county has been treated in that year, creates rel_year which finds the difference between the current year and the treated year
    if(medicaidYearOfExpansion$year[medicaidYearOfExpansion$fip==(geo %/% 1000)] != 999999 & medicaidYearOfExpansion$year[medicaidYearOfExpansion$fip==(geo %/% 1000)] < 2020){
      rel_year <- years[yearIndex] - medicaidYearOfExpansion$year[medicaidYearOfExpansion$fip==(geo %/% 1000)]
      treated <- rel_year >= 0
    } else {
      rel_year <- Inf
      treated <- FALSE
    }
    heartValue <- numeric();
    while(hrtDisease$GEO_ID[hrtIndex] != geo & hrtIndex < 4000){
      hrtIndex <- hrtIndex + 1
    }
    heartValue <- hrtDisease[[yearsFormatted[yearIndex]]][hrtIndex]
    
    insurValue <- numeric();
    while(percentInsured$GEO_ID[insurIndex] != geo & insurIndex < 4000){
      insurIndex <- insurIndex + 1
    }
    insurValue <- percentInsured[[yearsFormatted[yearIndex]]][insurIndex]
    
    popValue <- numeric();
    while(logarithmPopulation$GEO_ID[popIndex] != geo & popIndex < 4000){
      popIndex <- popIndex + 1
    }
    popValue <- logarithmPopulation[[yearsFormatted[yearIndex]]][popIndex]
    
    employValue <- numeric();
    while(logarithmEmployment$GEO_ID[employIndex] != geo & employIndex < 4000){
      employIndex <- employIndex + 1
    }
    employValue <- logarithmEmployment[[yearsFormatted[yearIndex]]][employIndex]
    
    #combine population with obesity, convert obesity from percent to a number, multiply by the population, then log it
    obesityValue <- obesity[[as.character(years[yearIndex])]][1]/100
    
    eligValue <- numeric();
    while(percentNewlyEligible$GEO_ID[eligIndex] != geo & eligIndex < 4000){
      eligIndex <- eligIndex + 1
    }
    eligValue <- percentNewlyEligible[[yearsFormatted[yearIndex]]][eligIndex]
    
    incomeValue <- numeric()
    while(medianIncome$GEO_ID[incomeIndex] != geo & incomeIndex < 4000){
      incomeIndex <- incomeIndex + 1
    }
    incomeValue <- medianIncome[[yearsFormatted[yearIndex]]][incomeIndex]
    
    
    didTable <- didTable %>% add_row(GEO_ID = geo, YEAR = years[yearIndex], TREATED = treated, YEAR_TREATED = year_treated, REL_YEAR = rel_year, HRTDISEASE = heartValue, INSURANCERATE = insurValue, LOGPOP = popValue, LOGEMPLOY = employValue, OBESITY = obesityValue, PERCELIGIBLE = eligValue, MEDIANINCOME = incomeValue)
  }
  print(geo)
}

write_csv(didTable, r"(./formattedData/DidTable.csv)")

