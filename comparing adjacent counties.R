#the idea is to compare adjacent, or geographically-near counties that are different in treatment
#for example, kentucky expanded medicaid, but tennessee did not.
#if we compare the counties that touch the kentucky-tennessee border,
#theoretically a lot of the confounding variables have a much smaller effect,
#like differences in demographics or healthcare dynamics that are state-specific

library(jsonlite)
library(stringr)
library(dplyr)
library(readr)
readIn <- read_json(r"(.\adjacentCountiesData\tennessee & virginia vs kentucky.txt)")

#create a standardized list of counties based on the JSON file
countiesList <- readIn$groups[[1]]$paths
counties <- data.frame(countyName = character(), stateAbb = character())
for(county in countiesList){
  split <- str_split(county, "__")
  counties <- counties %>% add_row(countyName = split[[1]][1], stateAbb = split[[1]][2])
}

FIPSInfo <- read_delim("adjacentCountiesData/www2.census.gov_geo_docs_reference_codes2020_national_county2020.txt", 
              delim = "|", escape_double = FALSE, trim_ws = TRUE)

#remove "County" from the end of each
FIPSInfo$COUNTYNAME <- str_remove(FIPSInfo$COUNTYNAME, " County$")

#create a combined fips column
FIPSInfo$COMBINEDFIPS <- as.numeric(paste(sep = "", FIPSInfo$STATEFP, FIPSInfo$COUNTYFP))

#construct a list of matching FIPS just based on state and county matches
#prints what counties aren't found so you can manually search later
fipMatches <- list()
for(i in 1:length(counties[[1]])){
  FIP <- -1
  state <- counties[i,2]
  county <- counties[i,1]
  # print(paste("searching for", state, county))
  filter <- FIPSInfo %>% filter(STATE == state) %>% filter(COUNTYNAME == county)
  #a match was found if the length is 1
  if(dim(filter)[1] == 1){
    # print(paste("match found", filter$COMBINEDFIPS[1]))
    FIP <- filter$COMBINEDFIPS[1]
  } else {
    print(paste("no match found for" , county, state))
  }
  fipMatches <- c(fipMatches, FIP)
}
counties$fipMatches <- fipMatches
#adding norton's FIP 
counties$fipMatches[39] <- 51720

################################################
#trying to plot counties
library(usmap)
library(ggplot2)
plot_usmap(regions = "counties", include = counties$fipMatches) + 
  labs(title = "counties i chose",
       subtitle = "wowza!") + 
  theme(panel.background=element_blank())
#wow, actually really easy!
