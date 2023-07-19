#IMPORTANT NOTE: this script was generalized to import multiple files at the same time, see "comparing adjacent counties separate script.R"

#the idea is to compare adjacent, or geographically-near counties that are different in treatment
#for example, kentucky expanded medicaid, but tennessee did not.
#if we compare the counties that touch the kentucky-tennessee border,
#theoretically a lot of the confounding variables have a much smaller effect,
#like differences in demographics or healthcare dynamics that are state-specific

library(jsonlite)
library(stringr)
library(dplyr)
library(readr)
# readIn <- read_json(r"(.\adjacentCountiesData\tennessee & virginia vs kentucky.txt)")
readIn <- read_json(r"(.\adjacentCountiesData\fullStateComparisons\tennessee vs kentucky full.txt)")

#create a standardized list of counties based on the MapChart JSON file
#comes from https://www.mapchart.net/usa-counties.html
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
counties$fipMatches <- as.numeric(fipMatches)
#adding norton's FIP 
# counties$fipMatches[39] <- 51720
counties$fipMatches[counties$countyName=="Van_Buren"] <- 47175

################################################
#trying to plot counties
library(usmap)
library(ggplot2)
plot_usmap(regions = "counties", include = counties$fipMatches) + 
  labs(title = "counties i chose",
       subtitle = "wowza!") + 
  theme(panel.background=element_blank())
#wow, actually really easy!

#change it to include the full states in the cohort, and fill the correct counties
countyFill <- data.frame(fips = counties$fipMatches, values = 1)
plot_usmap(data = countyFill, regions = "states", include = unique(counties$stateAbb)) + 
  labs(title = "counties i chose",
       subtitle = "wowza!") + 
  theme(panel.background=element_blank()) + plot_usmap(regions="states", include = unique(counties$stateAbb))

# RShowDoc("mapping", package = "usmap")
# RShowDoc("advanced-mapping", package = "usmap")

#to have contrasting state & county lines: https://stackoverflow.com/a/60084849
#(will work on later)

#########################################
#creating plots for HDM and insurance levels based on the MapQuest group
# hrtDisease <- read_csv("./formattedData/formattedAtlasDatawStateSorted.csv")
# percentInsured <- read_csv("./formattedData/percentInsured.csv")
didTable <- read_csv("./formattedData/DidTable.csv")

# hrtDiseasefiltered <- filter(hrtDisease, cnty_fips %in% counties$fipMatches)
# percentInsuredfiltered <- filter(percentInsured, combinedfips %in% counties$fipMatches)

#filter didTable to only include counties in the MapQuest group
didTablefiltered <- filter(didTable, GEO_ID %in% counties$fipMatches)

#split them based on their expansion status
# unique(didTablefiltered$YEAR_TREATED)
expandedList <- filter(didTablefiltered, YEAR_TREATED == 2014)
notExpandedList <- filter(didTablefiltered, (YEAR_TREATED > 2018 | YEAR_TREATED == 0))
paste("# of unintentional exclusions:", dim(didTablefiltered)[1] - dim(expandedList)[1] - dim(notExpandedList)[1])

expandedList$GROUP <- "Expanded Medicaid"
notExpandedList$GROUP <- "Did Not Expand Medicaid"

combinedList <- rbind(expandedList, notExpandedList)
combinedList$YEAR <- as.character(combinedList$YEAR) #fixes graph


years <- unique(combinedList$YEAR)
hdmPlotSeries <- data.frame(year = character(), meanValue = numeric(), sd = numeric(), group = character())
groups <- c("Expanded Medicaid", "Did Not Expand Medicaid")
for(y in years){
  for(g in groups){
    filteredByYearGroup <- combinedList %>% filter(YEAR == y & GROUP == g)
    hdmPlotSeries <- add_row(hdmPlotSeries, year = y, meanValue = mean(filteredByYearGroup$HRTDISEASE), sd = sd(filteredByYearGroup$HRTDISEASE), group = g)
  }
}

# ggplot(hdmPlotSeries, aes(x = year, y = meanValue, group = group))

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#0072B2", "#D55E00", "#999999")

ggplot(hdmPlotSeries, aes(x = year, y = meanValue, group = group, color = group)) + geom_line(linewidth = 2.4) + geom_point(
  aes(fill = group), 
  size = 5, 
  pch = 21, # Type of point that allows us to have both color (border) and fill.
  colour = "#FFFFFF", 
  stroke = 1 # The width of the border, i.e. stroke.
) + labs(title = paste(sep="", "Average Heart Disease Mortality of Selected Counties Based on Medicaid Expansion Status, 2010-2019"), 
         x = "Year", 
         y = "Age-standardized Mortality Rate per 100,000") + theme(plot.title = element_text(size=8)
         ) + scale_fill_manual(values=cbPalette) + scale_colour_manual(values=cbPalette)
#ok... weird HDM plot, looks like theres a huge SD for both groups, so i guess it's time to make some maps to see what's happening

years <- unique(combinedList$YEAR)
insurancePlotSeries <- data.frame(year = character(), meanValue = numeric(), group = character())
groups <- c("Expanded Medicaid", "Did Not Expand Medicaid")
for(y in years){
  for(g in groups){
    filteredByYearGroup <- combinedList %>% filter(YEAR == y & GROUP == g)
    insurancePlotSeries <- add_row(insurancePlotSeries, year = y, meanValue = mean(filteredByYearGroup$INSURANCERATE), group = g)
  }
}

ggplot(insurancePlotSeries, aes(x = year, y = meanValue, group = group, color = group)) + geom_line(linewidth = 2.4) + geom_point(
  aes(fill = group), 
  size = 5, 
  pch = 21, # Type of point that allows us to have both color (border) and fill.
  colour = "#FFFFFF", 
  stroke = 1 # The width of the border, i.e. stroke.
) + labs(title = paste(sep="", "Average Percent of People Covered by Insurance of Selected Counties Based on Medicaid Expansion Status, 2010-2019"), 
         x = "Year", 
         y = "% Covered by Insurance") + theme(plot.title = element_text(size=8)
         ) + scale_fill_manual(values=cbPalette) + scale_colour_manual(values=cbPalette)

############################################3
#plotting HDM and insurance on maps


#bing AI code btw, fun!
#find the change in HDM from 2010 to 2019, then plot it on the map
# Filter the rows where YEAR is either 2010 or 2019
df <- filter(combinedList, YEAR %in% c(2010,2019))

# Group by GEO_ID and calculate the difference in HRTDISEASE between 2010 and 2019
df <- df %>%
  group_by(GEO_ID) %>%
  summarize(change = (HRTDISEASE[YEAR == 2019] - HRTDISEASE[YEAR == 2010])/HRTDISEASE[YEAR == 2010] * 100)

# Rename the columns for plot_usmap
df <- df %>%
  rename(fips = GEO_ID, values = change)

plot_usmap(data = df, regions = "states", include = unique(counties$stateAbb)) + 
  labs(title = "HDM Change from 2010 to 2019 by County",
       subtitle = "wowza!") + 
  theme(panel.background=element_blank()) + scale_fill_continuous(
    low = "green", high = "red", name = "% Change in HDM")  + theme(legend.position = "right")

#same, but for insurance instead
# Filter the rows where YEAR is either 2010 or 2019
df <- filter(combinedList, YEAR %in% c(2010,2019))

# Group by GEO_ID and calculate the difference in HRTDISEASE between 2010 and 2019
df <- df %>%
  group_by(GEO_ID) %>%
  summarize(change = (INSURANCERATE[YEAR == 2019] - INSURANCERATE[YEAR == 2010])/INSURANCERATE[YEAR == 2010] * 100)

# Rename the columns for plot_usmap
df <- df %>%
  rename(fips = GEO_ID, values = change)

plot_usmap(data = df, regions = "states", include = unique(counties$stateAbb)) + 
  labs(title = "Insurance Coverage Change from 2010 to 2019 by County",
       subtitle = "wowza!") + 
  theme(panel.background=element_blank()) + scale_fill_continuous(
    low = "red", high = "green", name = "% Change in Insurance Coverage")  + theme(legend.position = "right")

#also plot the treatment (just for being obvious)

treatment <- combinedList %>% group_by(GEO_ID) %>% summarize(expanded = ifelse((sum(TREATED) > 4), "Expanded", "Did Not Expand")) %>% rename(fips = GEO_ID, values = expanded)
plot_usmap(data = treatment, regions = "states", include = unique(counties$stateAbb)) + 
  labs(title = "Medicaid Expansion Status by County",
       subtitle = "wowza!") + 
  theme(panel.background=element_blank()) + theme(legend.position = "right")


library(synthdid)
#trying out a synth did to see what difference it makes
#in summary: not much
#https://synth-inference.github.io/synthdid/articles/synthdid.html
combinedList[combinedList$YEAR_TREATED==2019,]$TREATED <- 0
setup <- synthdid::panel.matrices(as.data.frame(combinedList), unit = 1, time = 2, treatment = 3, outcome = 6)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
print(summary(tau.hat))
synthdid_plot(tau.hat)
synthdid_plot(tau.hat, overlay = 1)





###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
#I genera





