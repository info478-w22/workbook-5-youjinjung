# Workbook 6: analyze NHANES data

# Set up
library(survey)
library(Hmisc)

demo <- sasxport.get("DEMO_I.XPT")
alco <- sasxport.get("ALQ_I.XPT")

nhanes <- merge(x = demo, y = alco, by = 'seqn', all = TRUE)

# overall US population
# weight_sum represents the total US population in 2015-2016.
weight_sum <- sum(nhanes$wtint2yr, na.rm = TRUE)

# Analysis
# In ALQ151, we want to 2 to be 0, and we want to ignore 7 and 9
nhanes$alq151[nhanes$alq151 == 2]<- 0 
nhanes$alq151[nhanes$alq151 == 7]<- NA
nhanes$alq151[nhanes$alq151 == 9]<- NA

# create a survey design
nhanes_survey <- svydesign(
  id = ~sdmvpsu,
  nest = TRUE,
  strata = ~sdmvstra,
  weights = ~wtint2yr,
  data = nhanes
)

nhances_mean <- svymean(~alq151, nhanes_survey, na.rm = TRUE)

mean_by_gender <- svyby(~alq151, ~riagendr, nhanes_survey, svymean)