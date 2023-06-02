library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
hrtDisease <- read_csv("./formattedData/formattedAtlasDatawStateSorted.csv")
percentInsured <- read_csv("./formattedData/percentInsured.csv")
obesity <- read_csv("./socialFactorData/obesity % 2019.csv")
internet <- read_csv("./socialFactorData/% without broadband internet, 2016-2020 5year.csv")
diploma <- read_csv("./socialFactorData/% without high school diploma 2016-2020 5year.csv")
diabetes <- read_csv("./socialFactorData/diagnosed diabetes % 2019.csv")
cholesterol <- read_csv("./socialFactorData/high cholesterol % 2019.csv")
incinequality <- read_csv("./socialFactorData/income inequality (gini index), 2016-2020 5year.csv")
income <- read_csv("./socialFactorData/median household income (grouped) 2020.csv")
inactivity <- read_csv("./socialFactorData/physical inactivity % 2019.csv")
poverty <- read_csv("./socialFactorData/poverty % 2020.csv")
smoker <- read_csv("./socialFactorData/smoker status % 2019.csv")
urbanrural <- read_csv("./socialFactorData/urban-rural status 2013.csv")

factorName <- "% Obesity 2019"
socialFactor <- obesity

#remove missing values (-1)
socialFactor <- socialFactor %>% filter(Value>0)

#remove stuff that they don't both have (mostly counties that stopped existing or US territories, etc)
socialFactor <- filter(socialFactor, cnty_fips %in% hrtDisease$cnty_fips)
hrtDisease <- filter(hrtDisease, cnty_fips %in% socialFactor$cnty_fips)

#create histogram of social factor to aid in grouping them
ggplot(socialFactor, aes(x = Value)) + geom_histogram(data=socialFactor)

# groups <- split(socialFactor, cut(dig.lab = 10, socialFactor$Value, 4))
groups <- split(socialFactor, Hmisc::cut2(socialFactor$Value, g=4)) #even sized groups
# special for urban-rural -> groups <- split(socialFactor, as.factor(socialFactor$Value))

#from the groups, create a series that contains the year, the mean value of heart disease in each group, and the group that the data comes from

plotSeries = data.frame(year=character(), mean=numeric(), groups=character())
years <- names(hrtDisease)[4:17]
yearsNormal <- as.character((2006:2019))
countYear <- 1
for(x in 1:length(groups)){
  countYear <- 1
  for(year in years){
    mean <- mean(filter(hrtDisease, cnty_fips %in% groups[[x]]$cnty_fips)[[year]])[1]
    plotSeries <- rbind(plotSeries, data.frame(year=yearsNormal[countYear], mean=mean, group=names(groups)[[x]]))
    countYear <- countYear + 1
  }
}

ggplot(plotSeries, aes(x = year, y = mean, group = group, color = group)) + geom_line(linewidth = 2.4) + geom_point(
  aes(fill = group), 
  size = 5, 
  pch = 21, # Type of point that allows us to have both color (border) and fill.
  colour = "#FFFFFF", 
  stroke = 1 # The width of the border, i.e. stroke.
) + labs(title = paste(sep="", "Average HDM of County-Cohort Groups Based on ", factorName, ", 2006-2018"), 
         x = "Year", 
         y = "Age-standardized Mortality Rate per 100,000") + theme(plot.title = element_text(size=9))
