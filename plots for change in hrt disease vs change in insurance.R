#scatterrrr plot for comparing change in heart disease vs change in insurance
library(dplyr)
library(readr)
library(ggplot2)
didTable <- read_csv("./formattedData/DidTable.csv")

summaryTable <- filter(didTable, YEAR %in% c(2010,2019))

# Group by GEO_ID and calculate the difference in HRTDISEASE between 2010 and 2019
summaryTable <- summaryTable %>%
  group_by(GEO_ID) %>%
  summarize(hrtChange = (HRTDISEASE[YEAR == 2019] - HRTDISEASE[YEAR == 2010])/HRTDISEASE[YEAR == 2010] * 100, insurChange = (INSURANCERATE[YEAR == 2019] - INSURANCERATE[YEAR == 2010])/INSURANCERATE[YEAR == 2010] * 100, expanded = ifelse(YEAR_TREATED == 0, "Didn't Expand", "Expanded"))

ggplot(summaryTable, aes(x = hrtChange, y = insurChange)) + geom_point(alpha=.5, size = 1) + facet_grid(~ expanded)

#usmap plot bb

library(usmap)
usmapTable <- summaryTable %>% rename(fips = GEO_ID)
plot_usmap(regions = c("states"), data = usmapTable, values = "hrtChange", linewidth = .1) + labs(title = "Heart Disease Mortality Change from 2010 to 2019 by County") +
  theme(panel.background=element_blank()) + scale_fill_gradient2(low = "#005696", high = "#8E063B", mid = "lightgrey", midpoint = 0, name = "% Change in HDM", limits = c(-55, 80)) + theme(legend.position = "right")

plot_usmap(regions = c("states"), data = usmapTable, values = "insurChange", linewidth = .1) + labs(title = "Insurance Coverage Change from 2010 to 2019 by County") +
  theme(panel.background=element_blank()) + scale_fill_gradient2(low = "#8E063B", high = "#005696", mid = "lightgrey", midpoint = 0, name = "% Change in Insurance", limits = c(-41, 41)) + theme(legend.position = "right")
