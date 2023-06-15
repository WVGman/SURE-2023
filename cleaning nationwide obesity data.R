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

obesityLong <- obesityLong %>% add_row(data.frame(year = as.character(2019), obesity = (2*obesityLong$obesity[19])-obesityLong$obesity[18], severeObesity = (2*obesityLong$severeObesity[19])-obesityLong$severeObesity[18]))

plot(obesityLong$obesity)
plot(obesityLong$severeObesity)

row.names(obesityLong) <- obesityLong$year
obesityLong$year <- NULL
obesityFinal <- as.data.frame(t(obesityLong))

write.csv(obesityFinal, "./formattedData/nationwideObesityByYear1999-2018.csv")
