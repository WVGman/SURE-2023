#cleaning obesity data
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(waldo)

obesityList <- sapply(list.files(full.names=TRUE, path = "./rawData/Obesity from Diabetes Atlas"), read_csv, USE.NAMES = TRUE, skip = 2)
obesityList[18] <- NULL #remove link.txt

#check to make sure they have the same fips
for(x in 1:length(obesityList)){
  for(y in x:length(obesityList)){
    print(waldo::compare(sort(obesityList[[x]]$CountyFIPS), sort(obesityList[[y]]$CountyFIPS)))
  }
}
#yippee!

#sort each year by the FIPS
sorted <- lapply(obesityList, arrange, CountyFIPS)

#generate column names
cols <- paste("X", substr(as.character(2004:2020), 3, nchar(as.character(2004:2020))), sep="")

#create framework for final 
obesity <- data.frame(cntyfip = numeric(), state = character(), county = character())
for(col in cols){
  obesity[col] = character()
}

#add the data (i don't remember lol)
for(row in sorted[[1]]){
  obesity <- rbind(obesity, data.frame)
}

years <- 2004:2020
for(x in 1:length(sorted)){
  
}