library(did)
library(dplyr)
library(readr)
library(Hmisc)
library(ggplot2)
didTable <- read_csv("./formattedData/DidTable.csv")
didTable$TREATED <- as.numeric(didTable$TREATED)

didTable$POP <- exp(didTable$LOGPOP)
didTable$EMPLOY <- exp(didTable$LOGEMPLOY)
as.data.frame(didTable) -> table

years <- 2010:2019
statTable <- data.frame(year = numeric(), meanPop = numeric(), medianPop = numeric(), sdPop = numeric(), meanEmploy = numeric(), medianEmploy = numeric(), group = character())
for(year in years){
  # # tCut <- split(filter(table, YEAR == year), cut2(filter(table, YEAR == 2010)$POP, g=10))
  # tCut <- split(filter(table, YEAR == year), cut2(filter(table, YEAR == 2010)$POP, c(0, 10000, 100000, 1000000)))
  # for(t in 1:length(tCut)){
  #   mean <- mean(tCut[[t]]$POP)
  #   median <- median(tCut[[t]]$POP)
  #   popTable <- popTable %>% add_row(year = year, meanPop = mean, medianPop = median, group = names(tCut)[t])
  # }
  meanP <- mean(filter(table, YEAR == year)$POP)
  medianP <- median(filter(table, YEAR == year)$POP)
  sdP <- sd(filter(table, YEAR == year)$POP)
  meanE <- mean(filter(table, YEAR == year)$EMPLOY)
  medianE <- median(filter(table, YEAR == year)$EMPLOY)
  statTable <- statTable %>% add_row(year = year, meanPop = meanP, medianPop = medianP, sdPop = sdP, meanEmploy = meanE, medianEmploy = medianE)
}

statTable$year <- as.character(statTable$year)

ggplot(statTable, aes(x = year, y = meanPop)) + geom_line()
ggplot(statTable, aes(x = year, y = medianPop, group = group)) + geom_line()

ggplot(statTable, aes(x = year, y = meanEmploy)) + geom_line()
ggplot(statTable, aes(x = year, y = medianEmploy, group = group)) + geom_line()

ggplot(statTable, aes(x = year, y = meanPop, group = 1)) + geom_line(linewidth = 2.4, color = "red") + geom_point(
  fill = "red",
  size = 5, 
  pch = 21, # Type of point that allows us to have both color (border) and fill.
  colour = "#FFFFFF", 
  stroke = 1 # The width of the border, i.e. stroke.
) + labs(title = paste(sep="", "Average Population of US Counties, 2010-2019"), 
         x = "Year", 
         y = "Average Population") + theme(plot.title = element_text(size=14)) + coord_cartesian(ylim = c(70000,120000))
                                                                    
ggplot(statTable, aes(x = year, y = medianPop, group = 1)) + geom_line(linewidth = 2.4, color = "lightblue") + geom_point(
  fill = "lightblue",
  size = 5, 
  pch = 21, # Type of point that allows us to have both color (border) and fill.
  colour = "#FFFFFF", 
  stroke = 1 # The width of the border, i.e. stroke.
) + labs(title = paste(sep="", "Median Population of US Counties, 2010-2019"), 
         x = "Year", 
         y = "Median Population") + theme(plot.title = element_text(size=14))

ggplot(statTable, aes(x = year, y = meanEmploy, group = 1)) + geom_line(linewidth = 2.4, color = "red") + geom_point(
  fill = "red",
  size = 5, 
  pch = 21, # Type of point that allows us to have both color (border) and fill.
  colour = "#FFFFFF", 
  stroke = 1 # The width of the border, i.e. stroke.
) + labs(title = paste(sep="", "Average Number of People Employed in US Counties, 2010-2019"), 
         x = "Year", 
         y = "Average Number Employed") + theme(plot.title = element_text(size=14)) + coord_cartesian(ylim = c(70000,110000))

ggplot(statTable, aes(x = year, y = medianEmploy, group = 1)) + geom_line(linewidth = 2.4, color = "lightblue") + geom_point(
  fill = "lightblue",
  size = 5, 
  pch = 21, # Type of point that allows us to have both color (border) and fill.
  colour = "#FFFFFF", 
  stroke = 1 # The width of the border, i.e. stroke.
) + labs(title = paste(sep="", "Median Number of People Employed of US Counties, 2010-2019"), 
         x = "Year", 
         y = "Median Number Employed") + theme(plot.title = element_text(size=14))

#find how many counties went up in population, how many went down
startandend <- filter(table, YEAR == 2010 | YEAR == 2019)
up <- 0
theSame <- 0
down <- 0
change <- data.frame(GEO_ID = numeric(), change = numeric(), changePercent = numeric())
for(i in seq(1, length(startandend$GEO_ID), 2)){
  x <- startandend$POP[i+1] - startandend$POP[i]
  y <- (startandend$POP[i+1] - startandend$POP[i])/startandend$POP[i] * 100
  change <- change %>% add_row(GEO_ID = startandend$GEO_ID[i], change = x, changePercent = y)
}

change <- arrange(change, change)
change$num <- 1:length(change$GEO_ID)

ggplot(change, aes(x = num, y=change)) + geom_point()

change <- arrange(change, changePercent)
change$num <- 1:length(change$GEO_ID)
ggplot(change, aes(x = num, y=changePercent, color = changePercent)) + geom_point(
          ) + labs(title = "% Change of Population in Individual US Counties from 2010 to 2019 ", 
                    x = "Sorted Ranking of Change from Lowest Change to Highest Change", 
                    y = "Percent Change",
                   color = "Percent Change of Population") + theme(plot.title = element_text(size=14)) + scale_color_gradient2(low = "darkred", high="darkgreen", mid = "lightyellow", midpoint = 0)