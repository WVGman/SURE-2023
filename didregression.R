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
