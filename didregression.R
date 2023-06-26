library(did)
library(readr)
# didTable <- read_csv(r"(C:\Users\Owner\My Drive\RAP\data\finalDIDTable2.csv)")
# didTable <- read_csv(r"(C:\Users\Owner\My Drive\RAP\data\finalDIDTable3.csv)")
# didTable <- read_csv(r"(./test/testnewDidTable.csv)")
didTable <- read_csv("./formattedData/DidTable.csv")
didTable$TREATED <- as.numeric(didTable$TREATED)
as.data.frame(didTable) -> table

table$STATE <- table$GEO_ID %/% 1000

data(mpdta) -> test

time.periods <- 10 #10 years

out <- att_gt(yname = "HRTDISEASE",
              gname = "YEAR_TREATED",
              idname = "GEO_ID",
              tname = "YEAR",
              data = didTable,
              est_method = "reg",
              xformla = ~LOGPOP + LOGEMPLOY,
              control_group="notyettreated"
              )

# summary(out)
# ggdid(out)
agged <- aggte(out, type = "dynamic")
ggdid(agged)

# t <- aggte(formula = HRTDISEASE ~ TREATED | YEAR, id = GEO_ID, data = didTable)
library(did2s)
# first_stage = ~ 0 | region^time + group^time + group^region
reg <- did2s(table, 
      yname = "HRTDISEASE",
      first_stage = ~ LOGPOP + LOGEMPLOY + OBESITY | GEO_ID + YEAR,
      # first_stage = ~ 0 | GEO_ID + YEAR,
      second_stage = ~i(REL_YEAR, ref= c(Inf)),
      treatment = "TREATED",
      cluster_var = "GEO_ID", verbose = TRUE)


#     bootstrap = TRUE,
# n_bootstraps = 50
#       second_stage = ~i(TREATED),
#first_stage = ~ 0 | GEO_ID^YEAR + PERCELIGIBLE^YEAR + GEO_ID^PERCELIGIBLE + LOGPOP + LOGEMPLOY,

fixest::coefplot(
  reg,
  main = "Event study: Staggered treatment",
  xlab = "Relative time to treatment",
  col = "steelblue", ref.line = -0.5
)
esttable(reg)
# summary(reg)

###############################################
#splitting the did regression by low, medium, and high insurance rates in 2019
#based on "combining obesity and heart disease plots.R"
percentInsured <- read_csv("./formattedData/percentInsured.csv")

percentInsured <- filter(percentInsured, combinedfips %in% didTable$GEO_ID)

#split percentInsured into equally-sized groups based on  this    V and V (g is the number of groups)
# groups <- split(percentInsured, cut(dig.lab = 3, percentInsured$X19, 3)) #even length groups
groups <- split(percentInsured, Hmisc::cut2(percentInsured$X06, g=3)) #even sized groups
# groups <- split(percentInsured, cut(dig.lab = 3, percentInsured$X06, c(0,75,85,100))) #original groups

# names(groups) <- c("Low Insurance (64.2-86.8%)","Medium Insurance (86.8-91.2%)","High Insurance (91.2-97.6%)")
names(groups) <- paste(sep = "", c("Low", "Medium", "High"), " Insurance (", substr(names(groups),2,5), "-", substr(names(groups), 7, 10), "%) in 2006")


for(groupIndex in 1:length(groups)){
  groupedTable <- table %>% filter(GEO_ID %in% groups[[groupIndex]]$combinedfips)
  reg <- did2s(groupedTable, 
               yname = "HRTDISEASE",
               first_stage = ~ LOGPOP + LOGEMPLOY + OBESITY | GEO_ID + YEAR,
               # first_stage = ~ 0 | GEO_ID + YEAR,
               second_stage = ~i(REL_YEAR, ref= c(Inf)),
               treatment = "TREATED",
               cluster_var = "GEO_ID", verbose = TRUE)
  fixest::coefplot(
    reg,
    main = paste("Event study: Staggered treatment for ", names(groups)[groupIndex]),
    xlab = "Relative time to treatment",
    col = "steelblue", ref.line = -0.5
  )
}

for(x in 1:length(groups)){
  print(names(groups)[x])
  groupedTable <- table %>% filter(GEO_ID %in% groups[[x]]$combinedfips)
  print("number of non-expansion counties:")
  print(dim(groupedTable %>% filter(REL_YEAR == Inf))[1]/10)
  print("number of expansion counties:")
  print(dim(groupedTable %>% filter(REL_YEAR != Inf))[1]/10)
}
