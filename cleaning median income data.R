#cleaning median income data
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(waldo)

incomeList <- sapply(list.files(full.names=TRUE, pattern = "Data.csv?", path = "./rawData/Median Income Census"), read_csv, USE.NAMES = TRUE, skip = 1)

#select only the county IDs and the median income
#had to split the command in two because for some reason they changed which column means what starting in 2017 and changed the CODES too like what why wouldn't you add them at the end??
incomeListSimple <- c(lapply(incomeList[1:7], select, c("Geography", `Median income (dollars)!!Estimate!!Households`)),lapply(incomeList[-(1:7)], select, c("Geography", `Estimate!!Median income (dollars)!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households`)))
remove(incomeList)

#convert the median income columns (2nd column) to numerics
incomeListSimple <- lapply(incomeListSimple, mutate_at, 2, as.numeric)

#use inner_join, a function which combines two data frames by merging a column,
#successfully on every column, so for example, it uses inner_join on columns 1 and 2
#then on the result of that and column 3, the result of that and column 4, etc
incomeFinishedTable <- reduce(incomeListSimple, inner_join, by = "Geography")
names(incomeFinishedTable) <- c("GEO_ID", paste("X", c(10:21), sep=""))

#holy cow that was so much easier than using loops

#drop na values
sum(is.na(incomeFinishedTable))
incomeFinishedTable <- drop_na(incomeFinishedTable)

#turn GEO_IDs into numbers
incomeFinishedTable$GEO_ID <- as.numeric(substring(incomeFinishedTable$GEO_ID, 10))

write_csv(incomeFinishedTable, "./formattedData/medianIncomePerYear2010-2021.csv")

#making basic plot

incomePlot <- incomeFinishedTable %>% pivot_longer(names_to = "name", cols = X10:X21) %>% group_by(name) %>% summarize(mean(value)) %>% rename(year = name, means = "mean(value)")
incomePlot$year <- 2010:2021
incomePlot <- filter(incomePlot, year < 2019)
incomePlot$year <- as.character(incomePlot$year)

ggplot(incomePlot, aes(x = year, y = means, group = 1)) + geom_line(linewidth = 2.4, color = "darkblue") + geom_point(
  fill = "darkblue",
  size = 5, 
  pch = 21, # Type of point that allows us to have both color (border) and fill.
  colour = "#FFFFFF", 
  stroke = 1 # The width of the border, i.e. stroke.
) + labs(title = paste(sep="", "Median Income of All Counties, 2010-2018"), 
         x = "Year", 
         y = "Median Income (2021 US Dollars)") + theme(plot.title = element_text(size=14))
