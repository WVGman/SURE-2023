#cleaning obesity data
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(waldo)

obesityList <- sapply(list.files(full.names=TRUE, path = "./rawData/Obesity from Diabetes Atlas"), read_csv, USE.NAMES = TRUE, skip = 2)
obesityList[18] <- NULL #remove link.txt

#check to make sure they have the same fips
# for(x in 1:length(obesityList)){
#   for(y in x:length(obesityList)){
#     print(waldo::compare(sort(obesityList[[x]]$CountyFIPS), sort(obesityList[[y]]$CountyFIPS)))
#   }
# }
#yippee!

#sort each year by the FIPS
sorted <- lapply(obesityList, arrange, CountyFIPS)

#generate column names
cols <- paste("X", substr(as.character(2004:2020), 3, nchar(as.character(2004:2020))), sep="")

#create framework for final 
obesity <- data.frame(cntyfip = sorted[[1]]$CountyFIPS, state = sorted[[1]]$State, county = sorted[[1]]$County)
for(x in 1:length(cols)){
  obesity[cols[x]] <- sorted[[x]]$Percentage
}

# write_csv(obesity, "./formattedData/obesityRates.csv")

#remove all rows that have "No Data"
for(col in cols){
    obesity <- obesity %>% 
    filter(obesity[col] != "No Data")
}

write_csv(obesity, "./formattedData/obesityRates.csv")
