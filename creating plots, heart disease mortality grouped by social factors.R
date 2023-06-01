library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
hrtDisease <- read_csv("./formattedData/formattedAtlasDatawStateSorted.csv")
percentInsured <- read_csv("./formattedData/percentInsured.csv")
obesity <- read_csv("./socialFactorData/obesity % 2019.csv")

#remove missing values
obesity <- obesity %>% filter(Value>0)

#remove stuff that they don't both have (mostly counties that stopped existing or US territories, etc)
obesity <- filter(obesity, cnty_fips %in% hrtDisease$cnty_fips)
hrtDisease <- filter(hrtDisease, cnty_fips %in% obesity$cnty_fips)

#create histogram of social factor to aid in grouping them
ggplot(obTibble, aes(x = Value)) + geom_histogram(data=obesity)

obesityGroups <- split(obesity, cut(obesity$Value, 4))

groups <- obesityGroups

#from the groups, create a series that contains the year, the mean value of heart disease in each group, and the group that the data comes from

plotSeries = data.frame(year=character(), mean=numeric(), groups=character())
years <- names(hrtDisease)[4:17]
for(x in 1:length(groups)){
  for(year in years){
    mean <- mean(filter(hrtDisease, cnty_fips %in% groups[[x]]$cnty_fips)[[year]])[1]
    plotSeries <- rbind(plotSeries, data.frame(year=year, mean=mean, group=names(groups)[[x]]))
  }
}

ggplot(plotSeries, aes(x = year, y = mean, group = group, color = group)) + geom_line(linewidth = 2.4) + geom_point(
  aes(fill = group), 
  size = 5, 
  pch = 21, # Type of point that allows us to have both color (border) and fill.
  colour = "#FFFFFF", 
  stroke = 1 # The width of the border, i.e. stroke.
) + labs(title = "Average HDM of County-Cohort Groups Based on Obesity in 2019, 2006-2018", 
         x = "Year", 
         y = "Age-standardized Mortality Rate per 100,000") + theme(plot.title = element_text(size=10))
