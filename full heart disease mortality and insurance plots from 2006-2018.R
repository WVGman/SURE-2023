#find full insurance and heart disease mortality plots from 2006-2018

#first part (heart disease & insurance by treatment):
#code mostly taken from 'generate table 2.R' and 'create parallel trend graphs.R' from the Spring 2023 repo (terribly named files BTW)

#create the tables: 'generate table 2.R'
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
hrtDisease <- read_csv("./formattedData/formattedAtlasDatawStateSorted.csv")
percentInsured <- read_csv("./formattedData/percentInsured.csv")

# statesList <- read_csv("C:/Users/Owner/My Drive/RAP/saves/apr 3/states.csv", col_names = FALSE)
statesList <- read_csv("./formattedData/statesAndFIPS.csv")
# medicaidExpansionwrongway <- read_csv("C:/Users/Owner/My Drive/RAP/saves/apr 3/medicaidexpansion.csv", col_names = FALSE)
medicaidExpansionwrongway <- read_csv("./formattedData/medicaidExpansionStatus.csv", col_names = FALSE)
medicaidExpansion <- pivot_longer(medicaidExpansionwrongway, 2:length(medicaidExpansionwrongway), values_to="states")
medicaidExpansion$name <- NULL
names(medicaidExpansion) <- c("group", "state")
medicaidExpansion <- drop_na(medicaidExpansion)

states <- unique(hrtDisease$state)
valsToRemove <- states[!(states %in% statesList$state)] 
#just a check ^^^, excluding: american samoa, DC, virgin islands (county), guam, Northern Mariana Islands, puerto rico
states <- statesList$state

groups <- unique(medicaidExpansion$group)
notadopted <- filter(medicaidExpansion, group == "notadopted")
adoptedbutnotimplemented <- filter(medicaidExpansion, group == "adoptedbutnotimplemented")
adoptedafterJan14 <- filter(medicaidExpansion, group == groups[3])

#groups get defined
totalnotadopted <- c(notadopted$state, adoptedbutnotimplemented$state)
adoptedOnTime <- states[!(states %in% totalnotadopted)
                        &!(states %in% adoptedafterJan14$state)]

adoptedHrtDisease <- filter(hrtDisease, state %in% adoptedOnTime)
adoptedInsurance <- filter(percentInsured, combinedfips %in% adoptedHrtDisease$cnty_fips)
notAdoptedHrtDisease <- filter(hrtDisease, state %in% totalnotadopted)
notAdoptedInsurance <- filter(percentInsured, combinedfips %in% notAdoptedHrtDisease$cnty_fips)

avgTable <- function(table){
  for(i in 2:length(table)){
    table[[i]] <- mean(table[[i]], na.rm = TRUE)
  }
  table[1,2:length(table)]
}

sdTable <- function(table){
  for(i in 2:length(table)){
    table[[i]] <- sd(table[[i]], na.rm = TRUE)
  }
  table[1,2:length(table)]
}

adoptedHrtDisease[2:3] <- NULL #remove display_name & state info, since it isn't needed now
adoptedInsurance$X20 <- NULL #remove 2020 data since hrtdisease doesn't have it
names(adoptedHrtDisease) <- names(adoptedInsurance)

ahrtDiseaseAvg <- avgTable(adoptedHrtDisease)
ahrtDiseaseSD <- sdTable(adoptedHrtDisease)
apercentInsuredAvg <- avgTable(adoptedInsurance)
apercentInsuredSD <- sdTable(adoptedInsurance)

TableAdopted <- as_tibble(rbind(ahrtDiseaseAvg, ahrtDiseaseSD, apercentInsuredAvg, apercentInsuredSD))
TableAdopted$rows <- c("hrtDiseaseAvg", "hrtDiseaseSD", "percentInsuredAvg", "percentInsuredSD")
TableAdopted <- TableAdopted %>% relocate(rows, .before=X06)


notAdoptedHrtDisease[2:3] <- NULL #remove display_name & state info, since it isn't needed now
notAdoptedInsurance$X20 <- NULL #remove 2020 data since hrtdisease doesn't have it
names(notAdoptedHrtDisease) <- names(notAdoptedInsurance)

nhrtDiseaseAvg <- avgTable(notAdoptedHrtDisease)
nhrtDiseaseSD <- sdTable(notAdoptedHrtDisease)
npercentInsuredAvg <- avgTable(notAdoptedInsurance)
npercentInsuredSD <- sdTable(notAdoptedInsurance)

TableNotAdopted <- as_tibble(rbind(nhrtDiseaseAvg, nhrtDiseaseSD, npercentInsuredAvg, npercentInsuredSD))
TableNotAdopted$rows <- c("hrtDiseaseAvg", "hrtDiseaseSD", "percentInsuredAvg", "percentInsuredSD")
TableNotAdopted <- TableNotAdopted %>% relocate(rows, .before=X06)

names(TableAdopted) <- gsub("X", "20", names(TableAdopted)) ## create actual years for columns
# path <- r"(c:\Users\Owner\My Drive\RAP\figures)"
# write_csv(TableAdopted, file.path(path, "tableadopted.csv"))

names(TableNotAdopted) <- gsub("X", "20", names(TableNotAdopted)) ## create actual years for columns
# write_csv(TableNotAdopted, file.path(path, "tablenotadopted.csv"))


#create the plots: 'create parallel trend graphs.R'

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# path <- r"(c:\Users\Owner\My Drive\RAP\figures)"
# TableAdopted <- read_csv(paste(path,"\\tableadopted.csv",sep=""))
# TableNotAdopted <- read_csv(paste(path,"\\tablenotadopted.csv",sep=""))

series <- data.frame()
adoptedGroup <- pivot_longer(TableAdopted[3,], "2006":"2019", names_to="year")
adoptedGroup$type <- "Expanded"
notAdoptedGroup <- pivot_longer(TableNotAdopted[3,], "2006":"2019", names_to="year")
notAdoptedGroup$type <- "Did Not Expand"
series <- rbind(adoptedGroup, notAdoptedGroup)

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

ggplot(series, aes(x = year, y = value, group = type, color = type)) + geom_line(linewidth = 2.4) + geom_point(
  aes(fill = type), 
  size = 5, 
  pch = 21, # Type of point that allows us to have both color (border) and fill.
  colour = "#FFFFFF", 
  stroke = 1 # The width of the border, i.e. stroke.
) + labs(title = "Comparing Average Insurance Coverage of Medicare Expansion vs. Non-Expansion States, 2006-2019", 
         x = "Year", 
         y = "Average Percent Insured")  + theme(plot.title = element_text(size=10)
    ) + scale_fill_manual(values=cbPalette) + scale_colour_manual(values=cbPalette)

series2 <- data.frame()
adoptedGroup <- pivot_longer(TableAdopted[1,], "2006":"2018", names_to="year")
adoptedGroup$type <- "Expanded"
notAdoptedGroup <- pivot_longer(TableNotAdopted[1,], "2006":"2018", names_to="year")
notAdoptedGroup$type <- "Did Not Expand"
series2 <- rbind(adoptedGroup, notAdoptedGroup)

ggplot(series2, aes(x = year, y = value, group = type)) + geom_line(linewidth = 1) + geom_point(
  aes(shape = type), 
  size = 3, 
  colour = "black", 
  stroke = 1 # The width of the border, i.e. stroke.
) + scale_shape_manual(values=c(15, 16)) + labs(title = "Comparing Heart Disease Mortality Rates of Medicare Expansion vs. Non-Expansion States, 2006-2018", 
         x = "Year", 
         y = "Age-standardized Mortality Rate per 100,000")  + theme(plot.title = element_text(size=10)
    ) + scale_fill_manual(values=cbPalette) + scale_colour_manual(values=cbPalette)


########################################################3
#second part: heart disease and insurance by cohort
