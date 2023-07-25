library(dplyr)
library(tibble)
library(zoo)
#fixing nationwide obesity data, interpolating for every other year
obesity <- read_csv("rawData/Nationwide Obesity from NCHS/data.csv", skip = 1)

evenYears <- seq(from = 2000, to = 2018, by = 2)
evenYearsT <- as_tibble(matrix(NA, ncol=length(evenYears)))
names(evenYearsT) <- evenYears
for(i in length(obesity):1){
  obesity <- obesity %>% add_column(evenYearsT[,i], .after=i)
}

names(obesity) <- 1999:2018

obesityLong <- data.frame(year = names(obesity), obesity = as.numeric(obesity[1,]), severeObesity = as.numeric(obesity[2,]))

obesityLong$obesity[20] = obesityLong$obesity[19] + (obesityLong$obesity[19] - obesityLong$obesity[17])/2
obesityLong$severeObesity[20] = obesityLong$severeObesity[19] + (obesityLong$severeObesity[19] - obesityLong$severeObesity[17])/2

obesityLong <- obesityLong %>%
  mutate(obesity = na.approx(obesity)) %>%
  mutate(severeObesity = na.approx(severeObesity))

#add 2019, assuming no change from the previous year
obesityLong <- obesityLong %>% add_row(data.frame(year = as.character(2019), obesity = obesityLong$obesity[20], severeObesity = obesityLong$severeObesity[20]))

plot(obesityLong$obesity)
plot(obesityLong$severeObesity)

row.names(obesityLong) <- obesityLong$year
obesityLong$year <- NULL
obesityFinal <- as.data.frame(t(obesityLong))

write.csv(obesityFinal, "./formattedData/nationwideObesityByYear1999-2018.csv")

#plot obesity
obesityLong$year <- row.names(obesityLong)
# ggplot(obesityLong, aes(x = year, y = obesity)) + geom_point()
# 
# #convert year to be e.g. '02, but i need to make a factor in the correct order, otherwise '99 is after everything else
# obesityLong$year <- paste(sep="", "'", substr(obesityLong$year, 3, 4))
# obesityLong$year <- factor(obesityLong$year, levels = obesityLong$year)

#remove years up to 2010
obesityLong <- obesityLong %>% filter(year >= 2010 & year < 2019)

ggplot(obesityLong, aes(x = year, y = obesity, group = 1)) + geom_line(linewidth = 2.4, color = "darkblue") + geom_point(
  fill = "darkblue", 
  size = 5, 
  pch = 21, # Type of point that allows us to have both color (border) and fill.
  colour = "#FFFFFF", 
  stroke = 1 # The width of the border, i.e. stroke.
) + labs(title = "Nationwide Obesity Prevalence, 2010-2018", 
         x = "Year", 
         y = "Nationwide Obesity Prevalence (%)") + theme(plot.title = element_text(size=12))
