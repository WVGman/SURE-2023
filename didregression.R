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


# looking at bacondecomp

library(bacondecomp)

#bacon decomp without controls

df_bacon <- bacon(HRTDISEASE ~ TREATED,
                  data = table,
                  id_var = "GEO_ID",
                  time_var = "YEAR")

ggplot(df_bacon) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  geom_point()

write_csv(df_bacon, "./baconDecomp/baconDecompNoControls.csv")

#bacon decomp with pop and employ, no obesity

df_bacon_controls <- bacon(HRTDISEASE ~ TREATED + LOGPOP + LOGEMPLOY,
                  data = table,
                  id_var = "GEO_ID",
                  time_var = "YEAR")

ggplot(df_bacon_controls$two_by_twos) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  geom_point()

write_csv(as.data.frame(df_bacon_controls[1:2]), "./baconDecomp/baconDecompControlsNoObesityotherTwo.csv")
write_csv(df_bacon_controls$two_by_twos, "./baconDecomp/baconDecompControlsNoObesity2x2.csv")

#bacon decomp with pop, employ, and obesity

df_bacon_controls <- bacon(HRTDISEASE ~ TREATED + LOGPOP + LOGEMPLOY + OBESITY,
                           data = table,
                           id_var = "GEO_ID",
                           time_var = "YEAR")

# using did2s event_study to compare all the estimators

table <- table %>% filter(YEAR_TREATED < 2020)

#without covariates

studies <- event_study(data = table,
                       yname = "HRTDISEASE",
                       idname = "GEO_ID",
                       gname = "YEAR_TREATED",
                        tname = "YEAR",
                       estimator = "all")

plot_event_study(studies)

#with covariates, no obesity



studiesCo <- event_study(data = table,
                       yname = "HRTDISEASE",
                       idname = "GEO_ID",
                       gname = "YEAR_TREATED",
                       tname = "YEAR",
                       estimator = "all",
                       xformla = ~ LOGPOP + LOGEMPLOY)

plots <- plot_event_study(studiesCo)

for(i in 1:length(x)){
  
}

studiesallCo <- event_study(data = table,
                         yname = "HRTDISEASE",
                         idname = "GEO_ID",
                         gname = "YEAR_TREATED",
                         tname = "YEAR",
                         estimator = "all",
                         xformla = ~ LOGPOP + LOGEMPLOY + OBESITY)

plot_event_study(studiesallCo)

print(studies$estimate - studiesCo$estimate)

studiesCo <- event_study(data = table,
                         yname = "HRTDISEASE",
                         idname = "GEO_ID",
                         gname = "YEAR_TREATED",
                         tname = "YEAR",
                         estimator = "all",
                         xformla = ~ LOGPOP + LOGEMPLOY)

plot_event_study(studiesCo)


########################
#synthetic control
library(synthdid)
#trying out a synth did to see what difference it makes
#https://synth-inference.github.io/synthdid/articles/synthdid.html
synthTable <- filter(table, YEAR_TREATED == 0 | YEAR_TREATED == 2014 | YEAR_TREATED > 2018)
synthTable[synthTable$YEAR_TREATED>2018,]$TREATED <- 0
setup <- synthdid::panel.matrices(as.data.frame(synthTable), unit = 1, time = 2, treatment = 3, outcome = 6)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
print(x <- summary(tau.hat))
synthdid_plot(tau.hat)
synthdid_plot(tau.hat, overlay = 1)
y <- synthdid_controls(tau.hat)
synthdid_units_plot(tau.hat)
