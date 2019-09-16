# ------------------------------------- #
#         Mozzie Phase 2 Data           #
#            Initial Cleaning           #
#          September 12, 2019           #
#            K. Sumner                  #
# ------------------------------------- #

#### -------- load libraries -------- ####
library(tidyverse)
library(haven)
library(lubridate)


#### -------- read in the data sets --------- ####

# read in the phase 2 data sets for monthly and sick visits
hum_monthly_data = read_dta("OneDrive - University of North Carolina at Chapel Hill/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Cohort data through August July 2019/Monthly_VariablesLabled31stJuly2019_deID.dta")
hum_sick_data = read_dta("OneDrive - University of North Carolina at Chapel Hill/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Cohort data through August July 2019/SickVisit_VariablesLabled31stJuly2019_deID.dta")

# read in second annual household survey data sets
annual_socdem_p2_data = read_dta("OneDrive - University of North Carolina at Chapel Hill/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Cohort data through August July 2019/2Annual_DemoVariablesLabled31stJuly2019_deID.dta")
annual_householdvar_p2_data = read_dta("OneDrive - University of North Carolina at Chapel Hill/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Cohort data through August July 2019/2Annual_HH_VariablesLabled31stJuly2019_deID.dta")



#### ------ clean the monthly and sick data -------- ####

# ------ monthly p2 data

# look at the variables
colnames(hum_monthly_data)

# today
summary(hum_monthly_data$today)
# originally not in date format but a character with MDY
# recoded in date format
newdate = mdy(hum_monthly_data$today)
head(newdate)
head(hum_monthly_data$today)
summary(newdate)
str(newdate)
hum_monthly_data$today = newdate

# village_name
summary(as.factor(hum_monthly_data$village_name))
table(hum_monthly_data$village_name, useNA = "always")
# 1=kinesamo, 3=maruti, 5=sitabicha
str(hum_monthly_data$village_name)
# recoded as factor with village names as levels
hum_monthly_data$village_name = factor(hum_monthly_data$village_name,levels = c(1,3,5), labels = c("Kinesamo", "Maruti","Sitabicha"))
table(hum_monthly_data$village_name, useNA = "always")

# gender
table(hum_monthly_data$gender, useNA = "always")
# assuming that 1 is male, 2 is female
str(hum_monthly_data$gender)
# recode as a factor
hum_monthly_data$gender = factor(hum_monthly_data$gender,levels = c(1,2), labels = c("male","female"))
table(hum_monthly_data$gender, useNA = "always")

# relationship - don't need, remove
hum_monthly_data$relationship <- NULL

# other_relationship - don't need, remove
hum_monthly_data$other_relationship <- NULL

# slept_home
table(hum_monthly_data$slept_home, useNA = "always")
# assuming that 1 is yes, 2 is no
str(hum_monthly_data$slept_home)
# recode as a factor
hum_monthly_data$slept_home = factor(hum_monthly_data$slept_home,levels = c(1,2), labels = c("yes","no"))
table(hum_monthly_data$slept_home, useNA = "always")

# slept_usual
table(hum_monthly_data$slept_usual, useNA = "always")
# assuming that 1 is yes, 2 is no
str(hum_monthly_data$slept_usual)
# recode as a factor
hum_monthly_data$slept_usual = factor(hum_monthly_data$slept_usual,levels = c(1,2), labels = c("yes","no"))
table(hum_monthly_data$slept_usual, useNA = "always")

# slept_net
table(hum_monthly_data$slept_net, useNA = "always")
# assuming that 1 is yes, 2 is no
str(hum_monthly_data$slept_net)
# recode as a factor
hum_monthly_data$slept_net = factor(hum_monthly_data$slept_net,levels = c(1,2), labels = c("yes","no"))
table(hum_monthly_data$slept_net, useNA = "always")

# time_under_net
table(hum_monthly_data$time_under_net, useNA = "always")
# recode blanks and . to missing
hum_monthly_data$time_under_net[hum_monthly_data$time_under_net == ""] = NA
hum_monthly_data$time_under_net[hum_monthly_data$time_under_net == "."] = NA
str(hum_monthly_data$time_under_net)
# check recode
table(hum_monthly_data$time_under_net, useNA = "always")

# time_out_net
table(hum_monthly_data$time_out_net, useNA = "always")
# recode blanks and . to missing
hum_monthly_data$time_out_net[hum_monthly_data$time_out_net == ""] = NA
hum_monthly_data$time_out_net[hum_monthly_data$time_out_net == "."] = NA
str(hum_monthly_data$time_out_net)
# check recode
table(hum_monthly_data$time_out_net, useNA = "always")

# slept_times
table(hum_monthly_data$slept_times, useNA = "always")
str(hum_monthly_data$slept_times)
# looks good, doesn't need to be recoded

# time_to_bed
table(hum_monthly_data$time_to_bed, useNA = "always")
str(hum_monthly_data$time_to_bed)
# no missing, looks good

# to_bed_sp
table(hum_monthly_data$to_bed_sp, useNA = "always")
# assuming that 1 is indoors, 2 outdoors, 3 other
str(hum_monthly_data$to_bed_sp)
# recode as a factor
hum_monthly_data$to_bed_sp = factor(hum_monthly_data$to_bed_sp,levels = c(1,2,3), labels = c("indoors","outdoors","other"))
table(hum_monthly_data$to_bed_sp, useNA = "always")

# to_bed_oth
table(hum_monthly_data$to_bed_oth, useNA = "always")
str(hum_monthly_data$to_bed_oth)
# recode blanks and . to missing
hum_monthly_data$to_bed_oth[hum_monthly_data$to_bed_oth == ""] = NA
hum_monthly_data$to_bed_oth[hum_monthly_data$to_bed_oth == "."] = NA
# check recode
table(hum_monthly_data$to_bed_oth, useNA = "always")

# travelled
table(hum_monthly_data$travelled, useNA = "always")
str(hum_monthly_data$travelled)
# assuming that 1 is yes, 2 is no
# recode as a factor
hum_monthly_data$travelled = factor(hum_monthly_data$travelled,levels = c(1,2), labels = c("yes","no"))
table(hum_monthly_data$travelled, useNA = "always")

# to_where
table(hum_monthly_data$to_where, useNA = "always")
str(hum_monthly_data$to_where)
# recode blanks to missing
hum_monthly_data$to_where[hum_monthly_data$to_where == ""] = NA
hum_monthly_data$to_where[hum_monthly_data$to_where == "."] = NA
# need to standardize the coding for places but not sure what the correct coding would be
# make a factor
hum_monthly_data$to_where = as.factor(hum_monthly_data$to_where)
table(hum_monthly_data$to_where, useNA = "always")

# nights
table(hum_monthly_data$nights, useNA = "always")
str(hum_monthly_data$nights)
# looks good, clean

# slp_travelled
table(hum_monthly_data$slp_travelled, useNA = "always")
str(hum_monthly_data$slp_travelled)
# assuming that 1 is every night, 2 is sometimes, 3 is not at all, 4 is don't remember
# recode as a factor
hum_monthly_data$slp_travelled = factor(hum_monthly_data$slp_travelled,levels = c(1,2,3,4), labels = c("every night","sometimes","not at all","don't remember"))
table(hum_monthly_data$slp_travelled, useNA = "always")

# blood_spot
table(hum_monthly_data$blood_spot, useNA = "always")
str(hum_monthly_data$blood_spot)
# assuming that 1 is yes, 2 is no
# recode as a factor
hum_monthly_data$blood_spot = factor(hum_monthly_data$blood_spot,levels = c(1,2), labels = c("yes","no"))
table(hum_monthly_data$blood_spot, useNA = "always")

# mal_illness
table(hum_monthly_data$mal_illness, useNA = "always")
str(hum_monthly_data$mal_illness)
# assuming that 1 is yes, 2 is no
# recode as a factor
hum_monthly_data$mal_illness = factor(hum_monthly_data$mal_illness,levels = c(1,2), labels = c("yes","no"))
table(hum_monthly_data$mal_illness, useNA = "always")  

# act_illness
table(hum_monthly_data$act_illness, useNA = "always")
str(hum_monthly_data$act_illness)
# recode blanks and . to missing
hum_monthly_data$act_illness[hum_monthly_data$act_illness == ""] = NA
hum_monthly_data$act_illness[hum_monthly_data$act_illness == "."] = NA
# check the recode
table(hum_monthly_data$act_illness, useNA = "always")

# act_ill_other
table(hum_monthly_data$act_ill_other, useNA = "always")
str(hum_monthly_data$act_ill_other)
# standardize coding for responses
hum_monthly_data$act_ill_other[hum_monthly_data$act_ill_other == "Did nothing"] = "nothing"
hum_monthly_data$act_ill_other[hum_monthly_data$act_ill_other == "Didn't take any action"] = "nothing"
hum_monthly_data$act_ill_other[hum_monthly_data$act_ill_other == "Had not taken action yet"] = "nothing"
hum_monthly_data$act_ill_other[hum_monthly_data$act_ill_other == "Non"] = "nothing"
hum_monthly_data$act_ill_other[hum_monthly_data$act_ill_other == "None"] = "nothing"
hum_monthly_data$act_ill_other[hum_monthly_data$act_ill_other == ""] = NA
hum_monthly_data$act_ill_other[hum_monthly_data$act_ill_other == "."] = NA
# check recoding
table(hum_monthly_data$act_ill_other, useNA = "always")
# make a factor
hum_monthly_data$act_ill_other = as.factor(hum_monthly_data$act_ill_other)

# blood_test
table(hum_monthly_data$blood_test, useNA = "always")
str(hum_monthly_data$blood_test)
# assuming that 1 is yes, 2 is no, 3 is don't remember/don't know
# recode as a factor
hum_monthly_data$blood_test = factor(hum_monthly_data$blood_test,levels = c(1,2,3), labels = c("yes","no","don't know/don't remember"))
table(hum_monthly_data$blood_test, useNA = "always")

# test_type
table(hum_monthly_data$test_type, useNA = "always")
str(hum_monthly_data$test_type)
# assuming that 1 is rdt, 2 microscopy, 3 don't know/don't remember, 4 other
# recode as a factor
hum_monthly_data$test_type = factor(hum_monthly_data$test_type,levels = c(1,2,3,4), labels = c("rdt","microscopy","don't know/don't remember","other"))
table(hum_monthly_data$test_type, useNA = "always")

# test_type_oth
table(hum_monthly_data$test_type_oth, useNA = "always")
str(hum_monthly_data$test_type)
# recode some of the entries
hum_monthly_data$test_type_oth[hum_monthly_data$test_type_oth == ""] = NA
hum_monthly_data$test_type_oth[hum_monthly_data$test_type_oth == "."] = NA
hum_monthly_data$test_type_oth[hum_monthly_data$test_type_oth == "RDT - Microscopy"] = "rdt and microscopy"
hum_monthly_data$test_type_oth[hum_monthly_data$test_type_oth == "RDT and microscopy"] = "rdt and microscopy"
# check the recode 
table(hum_monthly_data$test_type_oth, useNA = "always")

# exrdt_test
table(hum_monthly_data$exrdt_test, useNA = "always")
str(hum_monthly_data$exrdt_test)
# assuming that 1 is yes, 2 is no
# recode as a factor
hum_monthly_data$exrdt_test = factor(hum_monthly_data$exrdt_test,levels = c(1,2), labels = c("yes","no"))
table(hum_monthly_data$exrdt_test, useNA = "always")

# conf_test
table(hum_monthly_data$conf_test, useNA = "always")
str(hum_monthly_data$conf_test)
# assuming that 1 is yes, 2 is no
# recode as a factor
hum_monthly_data$conf_test = factor(hum_monthly_data$conf_test,levels = c(1,2), labels = c("yes","no"))
table(hum_monthly_data$conf_test, useNA = "always")

# test_date
table(hum_monthly_data$test_date, useNA = "always")
str(hum_monthly_data$test_date)
hum_monthly_data$test_date[hum_monthly_data$test_date == ""] = NA
hum_monthly_data$test_date[hum_monthly_data$test_date == "."] = NA
head(hum_monthly_data$test_date)
hum_monthly_data$test_date = mdy(hum_monthly_data$test_date)
head(hum_monthly_data$test_date)
str(hum_monthly_data$test_date)

# test_result
table(hum_monthly_data$test_result, useNA = "always")
str(hum_monthly_data$test_result)
# assuming that 1 is positive, 2 is negative, 3 is don't know/don't remember
# recode as a factor
hum_monthly_data$test_result = factor(hum_monthly_data$test_result,levels = c(1,2,3), labels = c("positive","negative","don't know/don't remember"))
table(hum_monthly_data$test_result, useNA = "always")

# test_obs
table(hum_monthly_data$test_obs, useNA = "always")
str(hum_monthly_data$test_obs)
# assuming that 1 is yes, 2 is no
# recode as a factor
hum_monthly_data$test_obs = factor(hum_monthly_data$test_obs,levels = c(1,2), labels = c("yes","no"))
table(hum_monthly_data$test_obs, useNA = "always")

# blood_test1
table(hum_monthly_data$blood_test1, useNA = "always")
str(hum_monthly_data$blood_test1)
# assuming that 1 is yes, 2 is no, 3 is don't know/don't remember
# recode as a factor
hum_monthly_data$blood_test1 = factor(hum_monthly_data$blood_test1,levels = c(1,2,3), labels = c("yes","no","don't know/don't remember"))
table(hum_monthly_data$blood_test1, useNA = "always")

# conf_test1
table(hum_monthly_data$conf_test1, useNA = "always")
str(hum_monthly_data$conf_test1)
# assuming that 1 is yes, 2 is no
# recode as a factor
hum_monthly_data$conf_test1 = factor(hum_monthly_data$conf_test1,levels = c(1,2), labels = c("yes","no"))
table(hum_monthly_data$conf_test1, useNA = "always")

# test_date1
table(hum_monthly_data$test_date1, useNA = "always")
str(hum_monthly_data$test_date1)
hum_monthly_data$test_date1[hum_monthly_data$test_date1 == ""] = NA
hum_monthly_data$test_date1[hum_monthly_data$test_date1 == "."] = NA
head(hum_monthly_data$test_date1)
hum_monthly_data$test_date1 = mdy(hum_monthly_data$test_date1)
head(hum_monthly_data$test_date1)
str(hum_monthly_data$test_date1)

# test_result1
table(hum_monthly_data$test_result1, useNA = "always")
str(hum_monthly_data$test_result1)
# assuming that 1 is positive, 2 is negative, 3 is don't know/don't remember
# recode as a factor
hum_monthly_data$test_result1 = factor(hum_monthly_data$test_result1,levels = c(1,2,3), labels = c("positive","negative","don't know/don't remember"))
table(hum_monthly_data$test_result1, useNA = "always")

# test_obs1
table(hum_monthly_data$test_obs1, useNA = "always")
str(hum_monthly_data$test_obs1)
# assuming that 1 is yes, 2 is no
# recode as a factor
hum_monthly_data$test_obs1 = factor(hum_monthly_data$test_obs1,levels = c(1,2), labels = c("yes","no"))
table(hum_monthly_data$test_obs1, useNA = "always")

# blood_test2
table(hum_monthly_data$blood_test2, useNA = "always")
str(hum_monthly_data$blood_test2)
# assuming that 1 is yes, 2 is no, 3 is don't know/don't remember
# recode as a factor
hum_monthly_data$blood_test2 = factor(hum_monthly_data$blood_test2,levels = c(1,2,3), labels = c("yes","no","don't know/don't remember"))
table(hum_monthly_data$blood_test2, useNA = "always")

# conf_test2
table(hum_monthly_data$conf_test2, useNA = "always")
str(hum_monthly_data$conf_test2)
# assuming that 1 is yes, 2 is no
# recode as a factor
hum_monthly_data$conf_test2 = factor(hum_monthly_data$conf_test2,levels = c(1,2), labels = c("yes","no"))
table(hum_monthly_data$conf_test2, useNA = "always")

# test_date2
table(hum_monthly_data$test_date2, useNA = "always")
str(hum_monthly_data$test_date2)
hum_monthly_data$test_date2[hum_monthly_data$test_date2 == ""] = NA
hum_monthly_data$test_date2[hum_monthly_data$test_date2 == "."] = NA
head(hum_monthly_data$test_date2)
hum_monthly_data$test_date2 = mdy(hum_monthly_data$test_date2)
head(hum_monthly_data$test_date2)
str(hum_monthly_data$test_date2)

# test_result2
table(hum_monthly_data$test_result2, useNA = "always")
str(hum_monthly_data$test_result2)
# assuming that 1 is positive, 2 is negative, 3 is don't know/don't remember
# recode as a factor
hum_monthly_data$test_result2 = factor(hum_monthly_data$test_result2,levels = c(1,2,3), labels = c("positive","negative","don't know/don't remember"))
table(hum_monthly_data$test_result1, useNA = "always")
  
  