library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
hrtDisease <- read_csv("./formattedData/formattedAtlasDatawStateSorted.csv")
didTable <- read_csv("./formattedData/DidTable.csv")
medicaidYearOfExpansion <- read_csv("./formattedData/medicaidAllYearsOfExpansion.csv")
statesFips <- read_csv("./formattedData/statesAndFIPS.csv")

#combine medicaidYearOfExpansion and statesFips
medicaidYearOfExpansion$fip <- integer(50)
for(x in 1:length(medicaidYearOfExpansion$state)){
  medicaidYearOfExpansion$fip[x] <- statesFips$FIP[statesFips$state==medicaidYearOfExpansion$state[x]]
}

years <- 2010:2019
averages <- data.frame(YEAR = numeric(), NONTREATED = numeric(), TREATED = numeric())
for(year in years){
  averages <- averages %>% add_row(YEAR = year, 
                                   NONTREATED = mean((didTable %>% filter(YEAR == year, REL_YEAR == Inf))$HRTDISEASE),
                                   TREATED = mean((didTable %>% filter(YEAR == year, REL_YEAR != Inf))$HRTDISEASE))
}

relYearRange <- -4:4
staggeredAverages <- list()
for(relYear in relYearRange){
  staggeredAverages <- c(staggeredAverages, mean((didTable %>% filter(REL_YEAR == relYear))$HRTDISEASE)) 
}

plot(-4:4, staggeredAverages)


averagesLong <- pivot_longer(averages, 2:3, names_to = "group", values_to="averages")
averagesLong$YEAR <- as.character(averagesLong$YEAR)

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

# ggplot(didTable, aes(x = YEAR, y = HRTDISEASE, group = TREATED, color=TREATED)) + geom_col()
ggplot(averagesLong, aes(x = YEAR, y = averages, group = group, color = group)) + geom_line(linewidth = 2.4) + geom_point(
  aes(fill = group),
  size = 5,
  pch = 21, # Type of point that allows us to have both color (border) and fill.
  colour = "#FFFFFF",
  stroke = 1 # The width of the border, i.e. stroke.
) + labs(title = paste(sep="", "Average Heart Disease Mortality by Expansion Status, 2010-2019 (3 year cohorts)"),
         x = "Year",
         y = "Age-Adjusted Mortality Rate per 100,000") + theme(plot.title = element_text(size=9)
         ) + scale_fill_manual(values=cbPalette) + scale_colour_manual(values=cbPalette) + coord_cartesian(ylim = c(0,80))

#making a plot of the average change from the year before a county's expansion to the years after

avgDidTable <- didTable %>% filter(REL_YEAR != Inf)
avgDidTable$HRTDISEASEDIFF <- numeric(length(avgDidTable$HRTDISEASE))
for(rowNum in 1:length(avgDidTable$HRTDISEASE)){
  if(rowNum %% 100 == 0){
    print(rowNum)
  }
  if(avgDidTable[rowNum,]$REL_YEAR > -1){
    avgDidTable[rowNum,]$HRTDISEASEDIFF <- ((avgDidTable[rowNum,]$HRTDISEASE - avgDidTable[rowNum-(avgDidTable[rowNum,]$REL_YEAR - -1),]$HRTDISEASE)/avgDidTable[rowNum-(avgDidTable[rowNum,]$REL_YEAR - -1),]$HRTDISEASE)*100
  }
}

relYearRange <- -4:4
staggeredAverages <- list()
for(relYear in relYearRange){
  staggeredAverages <- c(staggeredAverages, mean((avgDidTable %>% filter(REL_YEAR == relYear))$HRTDISEASEDIFF)) 
}

relYearRange <- 0:4
for(x in relYearRange){
  y <- (avgDidTable %>% filter(REL_YEAR == x))$HRTDISEASEDIFF
  z <- y[!y %in% boxplot.stats(y)$out]
  hist(z)
  print(x)
  print(summary(z))
}


