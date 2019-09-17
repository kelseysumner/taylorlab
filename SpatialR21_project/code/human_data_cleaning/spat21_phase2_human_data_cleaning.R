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



#### ------ clean the monthly and sick data and annual_socdem_p2_data that we will use as baseline data -------- ####

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
str(hum_monthly_data$test_type_oth)
# recode some of the entries
hum_monthly_data$test_type_oth[hum_monthly_data$test_type_oth == ""] = NA
hum_monthly_data$test_type_oth[hum_monthly_data$test_type_oth == "."] = NA
hum_monthly_data$test_type_oth[hum_monthly_data$test_type_oth == "RDT - Microscopy"] = "rdt and microscopy"
hum_monthly_data$test_type_oth[hum_monthly_data$test_type_oth == "RDT and microscopy"] = "rdt and microscopy"
# check the recode 
table(hum_monthly_data$test_type_oth, useNA = "always")

# test_type_oth1
table(hum_monthly_data$test_type_oth1, useNA = "always")
str(hum_monthly_data$test_type_oth1)
hum_monthly_data$test_type_oth1 <- NULL

# test_type_oth2
table(hum_monthly_data$test_type_oth2, useNA = "always")
str(hum_monthly_data$test_type_oth2)
hum_monthly_data$test_type_oth2 <- NULL

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
table(hum_monthly_data$test_result2, useNA = "always")
  
# test_obs2
table(hum_monthly_data$test_obs2, useNA = "always")
str(hum_monthly_data$test_obs2)
# assuming that 1 is yes, 2 is no
# recode as a factor
hum_monthly_data$test_obs2 = factor(hum_monthly_data$test_obs2,levels = c(1,2), labels = c("yes","no"))
table(hum_monthly_data$test_obs2, useNA = "always")

# ill_med
table(hum_monthly_data$ill_med, useNA = "always")
str(hum_monthly_data$ill_med)
# assuming that 1 is yes, 2 is no, 3 is don't know/don't remember
# recode as a factor
hum_monthly_data$ill_med = factor(hum_monthly_data$ill_med,levels = c(1,2,3), labels = c("yes","no","don't know/don't remember"))
table(hum_monthly_data$ill_med, useNA = "always")

# medicine
table(hum_monthly_data$medicine, useNA = "always")
str(hum_monthly_data$medicine)
# recode the blanks and . to missing
hum_monthly_data$medicine[hum_monthly_data$medicine == ""] = NA
hum_monthly_data$medicine[hum_monthly_data$medicine == "."] = NA
# check the recode
table(hum_monthly_data$medicine, useNA = "always")
# many responses that are broken down into separate variables later in the dta set

# med_oth
table(hum_monthly_data$med_oth, useNA = "always")
str(hum_monthly_data$med_oth)
# recode the blanks and . to missing
hum_monthly_data$med_oth[hum_monthly_data$med_oth == ""] = NA
hum_monthly_data$med_oth[hum_monthly_data$med_oth == "."] = NA
# check the recode
table(hum_monthly_data$med_oth, useNA = "always")
# need to go through and standardize coding for medicines
# make a factor
hum_monthly_data$med_oth = as.factor(hum_monthly_data$med_oth)

# med_source
table(hum_monthly_data$med_source, useNA = "always")
str(hum_monthly_data$med_source)
# recode the blanks and . to missing
hum_monthly_data$med_source[hum_monthly_data$med_source == ""] = NA
hum_monthly_data$med_source[hum_monthly_data$med_source == "."] = NA
# check the recode
table(hum_monthly_data$med_source, useNA = "always")

# med_source_Oth
table(hum_monthly_data$med_source_Oth, useNA = "always")
str(hum_monthly_data$med_source_Oth)
# looks like all observations missing
# recode the . to missing
hum_monthly_data$med_source_Oth[hum_monthly_data$med_source_Oth == "."] = NA
# check the recode
table(hum_monthly_data$med_source_Oth, useNA = "always")
# looks like all observations missing
# remove from data set
hum_monthly_data$med_source_Oth <- NULL

# med_date
table(hum_monthly_data$med_date, useNA = "always")
str(hum_monthly_data$med_date)
# recode the blanks and . to missing
hum_monthly_data$med_date[hum_monthly_data$med_date == ""] = NA
hum_monthly_data$med_date[hum_monthly_data$med_date == "."] = NA
# check the recode
table(hum_monthly_data$med_date, useNA = "always")
# change the variable to date format
newdate2 = mdy(hum_monthly_data$med_date)
newdate2[c(18,23,35,38,40,43)]
hum_monthly_data$med_date[c(18,23,35,38,40,43)]
summary(newdate2)
str(newdate2)
hum_monthly_data$med_date = newdate2

# tablets
table(hum_monthly_data$tablets, useNA = "always")
str(hum_monthly_data$tablets)
# assuming that 1 is yes, 2 is no, 3 is don't know/don't remember
# recode as a factor
hum_monthly_data$tablets = factor(hum_monthly_data$tablets,levels = c(1,2,3), labels = c("yes","no","don't know/don't remember"))
table(hum_monthly_data$tablets, useNA = "always")

# recover
table(hum_monthly_data$recover, useNA = "always")
str(hum_monthly_data$recover)
# looks good, clean, 99 indicates still recovering

# mal_likely
table(hum_monthly_data$mal_likely, useNA = "always")
str(hum_monthly_data$mal_likely)
# assuming that 1 is very likely, 2 is likely, 3 is 50-50, 4 is unlikely, 5 is very unlikely, 6 is don't know, 7 is no response
# recode as a factor
hum_monthly_data$mal_likely= factor(hum_monthly_data$mal_likely,levels = c(1,2,3,4,5,6,7), labels = c("very likely","likely","50-50","unlikely","very unlikely","don't know","no response"))
table(hum_monthly_data$mal_likely, useNA = "always")

# mal_rdt
table(hum_monthly_data$mal_rdt, useNA = "always")
str(hum_monthly_data$mal_rdt)
# assuming that 1 is yes, 2 is no
# recode as a factor
hum_monthly_data$mal_rdt = factor(hum_monthly_data$mal_rdt,levels = c(1,2), labels = c("yes","no"))
table(hum_monthly_data$mal_rdt, useNA = "always")

# mrdt
table(hum_monthly_data$mrdt, useNA = "always")
str(hum_monthly_data$mrdt)
# assuming that 1 is yes, 2 is no, 3 is don't know/don't remember
# recode as a factor
hum_monthly_data$mrdt = factor(hum_monthly_data$mrdt,levels = c(1,2,3), labels = c("yes","no","don't know/don't remember"))
table(hum_monthly_data$mrdt, useNA = "always")

# mrdt_n
table(hum_monthly_data$mrdt_n, useNA = "always")
str(hum_monthly_data$mrdt_n)
# assuming that 1 is very likely, 2 is likely, 3 is 50-50, 4 is unlikely, 5 is very unlikely, 6 is don't know, 7 is no response
# recode as a factor
hum_monthly_data$mrdt_n= factor(hum_monthly_data$mrdt_n,levels = c(1,2,3,4,5,6,7), labels = c("very likely","likely","50-50","unlikely","very unlikely","don't know","no response"))
table(hum_monthly_data$mrdt_n, useNA = "always")

# mrdt_p
table(hum_monthly_data$mrdt_p, useNA = "always")
str(hum_monthly_data$mrdt_p)
# assuming that 1 is very likely, 2 is likely, 3 is 50-50, 4 is unlikely, 5 is very unlikely, 6 is don't know, 7 is no response
# recode as a factor
hum_monthly_data$mrdt_p= factor(hum_monthly_data$mrdt_p,levels = c(1,2,3,4,5,6,7), labels = c("very likely","likely","50-50","unlikely","very unlikely","don't know","no response"))
table(hum_monthly_data$mrdt_p, useNA = "always")

# malaria_likely
table(hum_monthly_data$malaria_likely, useNA = "always")
str(hum_monthly_data$malaria_likely)
# looks good, clean, 99 indicates don't know

# malaria_al
table(hum_monthly_data$malaria_al, useNA = "always")
str(hum_monthly_data$malaria_al)
# assuming that 1 is very likely, 2 is likely, 3 is 50-50, 4 is unlikely, 5 is very unlikely, 6 is don't know, 7 is no response
# recode as a factor
hum_monthly_data$malaria_al = factor(hum_monthly_data$malaria_al,levels = c(1,2,3,4,5,6,7), labels = c("very likely","likely","50-50","unlikely","very unlikely","don't know","no response"))
table(hum_monthly_data$malaria_al, useNA = "always")

# key
table(hum_monthly_data$key, useNA = "always")
str(hum_monthly_data$key)
length(is.na(as.factor(hum_monthly_data$key))==T)
head(hum_monthly_data$key)

# HH_ID
table(hum_monthly_data$HH_ID, useNA = "always")
str(hum_monthly_data$HH_ID)
# keep the variable as a character for now because will be used to create a unique ID for each person eventually

# memID
table(hum_monthly_data$memID, useNA = "always")
str(hum_monthly_data$memID)
# keep the variable as a character for now because will be used to create a unique ID for each person eventually

# medicine_ACT
table(hum_monthly_data$medicine_ACT, useNA = "always")
str(hum_monthly_data$medicine_ACT)
# coded with 0 as no and 1 as yes
# recode as a factor
hum_monthly_data$medicine_ACT = factor(hum_monthly_data$medicine_ACT,levels = c(1,0), labels = c("yes","no"))
table(hum_monthly_data$medicine_ACT, useNA = "always")

# medicine_Qui
table(hum_monthly_data$medicine_Qui, useNA = "always")
str(hum_monthly_data$medicine_Qui)
# coded with 0 as no and 1 as yes
# recode as a factor
hum_monthly_data$medicine_Qui = factor(hum_monthly_data$medicine_Qui,levels = c(1,0), labels = c("yes","no"))
table(hum_monthly_data$medicine_Qui, useNA = "always")

# medicine_SP
table(hum_monthly_data$medicine_SP, useNA = "always")
str(hum_monthly_data$medicine_SP)
# coded with 0 as no and 1 as yes
# recode as a factor
hum_monthly_data$medicine_SP = factor(hum_monthly_data$medicine_SP,levels = c(1,0), labels = c("yes","no"))
table(hum_monthly_data$medicine_SP, useNA = "always")

# medicine_OACT
table(hum_monthly_data$medicine_OACT, useNA = "always")
str(hum_monthly_data$medicine_OACT)
# coded with 0 as no and 1 as yes
# recode as a factor
hum_monthly_data$medicine_OACT = factor(hum_monthly_data$medicine_OACT,levels = c(1,0), labels = c("yes","no"))
table(hum_monthly_data$medicine_OACT, useNA = "always")

# medicine_AMO
table(hum_monthly_data$medicine_AMO, useNA = "always")
str(hum_monthly_data$medicine_AMO)
# coded with 0 as no and 1 as yes
# recode as a factor
hum_monthly_data$medicine_AMO = factor(hum_monthly_data$medicine_AMO,levels = c(1,0), labels = c("yes","no"))
table(hum_monthly_data$medicine_AMO, useNA = "always")

# medicine_SPT
table(hum_monthly_data$medicine_SPT, useNA = "always")
str(hum_monthly_data$medicine_SPT)
# coded with 0 as no and 1 as yes
# recode as a factor
hum_monthly_data$medicine_SPT = factor(hum_monthly_data$medicine_SPT,levels = c(1,0), labels = c("yes","no"))
table(hum_monthly_data$medicine_SPT, useNA = "always")

# medicine_CIP
table(hum_monthly_data$medicine_CIP, useNA = "always")
str(hum_monthly_data$medicine_CIP)
# coded with 0 as no and 1 as yes
# recode as a factor
hum_monthly_data$medicine_CIP = factor(hum_monthly_data$medicine_CIP,levels = c(1,0), labels = c("yes","no"))
table(hum_monthly_data$medicine_CIP, useNA = "always")

# medicine_PAN
table(hum_monthly_data$medicine_PAN, useNA = "always")
str(hum_monthly_data$medicine_PAN)
# coded with 0 as no and 1 as yes
# recode as a factor
hum_monthly_data$medicine_PAN = factor(hum_monthly_data$medicine_PAN,levels = c(1,0), labels = c("yes","no"))
table(hum_monthly_data$medicine_PAN, useNA = "always")

# medicine_DNT
table(hum_monthly_data$medicine_DNT, useNA = "always")
str(hum_monthly_data$medicine_DNT)
# coded with 0 as no and 1 as yes
# recode as a factor
hum_monthly_data$medicine_DNT = factor(hum_monthly_data$medicine_DNT,levels = c(1,0), labels = c("yes","no"))
table(hum_monthly_data$medicine_DNT, useNA = "always")

# medicine_OTH
table(hum_monthly_data$medicine_OTH, useNA = "always")
str(hum_monthly_data$medicine_OTH)
# coded with 0 as no and 1 as yes
# recode as a factor
hum_monthly_data$medicine_OTH = factor(hum_monthly_data$medicine_OTH,levels = c(1,0), labels = c("yes","no"))
table(hum_monthly_data$medicine_OTH, useNA = "always")

# act_illness_a
table(hum_monthly_data$act_illness_a, useNA = "always")
str(hum_monthly_data$act_illness_a)
# coded with 0 as no and 1 as yes
# recode as a factor
hum_monthly_data$act_illness_a = factor(hum_monthly_data$act_illness_a,levels = c(1,0), labels = c("yes","no"))
table(hum_monthly_data$act_illness_a, useNA = "always")

# act_illness_b
table(hum_monthly_data$act_illness_b, useNA = "always")
str(hum_monthly_data$act_illness_b)
# coded with 0 as no and 1 as yes
# recode as a factor
hum_monthly_data$act_illness_b = factor(hum_monthly_data$act_illness_b,levels = c(1,0), labels = c("yes","no"))
table(hum_monthly_data$act_illness_b, useNA = "always")

# act_illness_c
table(hum_monthly_data$act_illness_c, useNA = "always")
str(hum_monthly_data$act_illness_c)
# coded with 0 as no and 1 as yes
# recode as a factor
hum_monthly_data$act_illness_c = factor(hum_monthly_data$act_illness_c,levels = c(1,0), labels = c("yes","no"))
table(hum_monthly_data$act_illness_c, useNA = "always")

# act_illness_d
table(hum_monthly_data$act_illness_d, useNA = "always")
str(hum_monthly_data$act_illness_d)
# coded with 0 as no and 1 as yes
# recode as a factor
hum_monthly_data$act_illness_d = factor(hum_monthly_data$act_illness_d,levels = c(1,0), labels = c("yes","no"))
table(hum_monthly_data$act_illness_d, useNA = "always")

# act_illness_e
table(hum_monthly_data$act_illness_e, useNA = "always")
str(hum_monthly_data$act_illness_e)
# coded with 0 as no and 1 as yes
# recode as a factor
hum_monthly_data$act_illness_e = factor(hum_monthly_data$act_illness_e,levels = c(1,0), labels = c("yes","no"))
table(hum_monthly_data$act_illness_e, useNA = "always")

# act_illness_f
table(hum_monthly_data$act_illness_f, useNA = "always")
str(hum_monthly_data$act_illness_f)
# coded with 0 as no and 1 as yes
# recode as a factor
hum_monthly_data$act_illness_f = factor(hum_monthly_data$act_illness_f,levels = c(1,0), labels = c("yes","no"))
table(hum_monthly_data$act_illness_f, useNA = "always")

# act_illness_g
table(hum_monthly_data$act_illness_g, useNA = "always")
str(hum_monthly_data$act_illness_g)
# coded with 0 as no and 1 as yes
# recode as a factor
hum_monthly_data$act_illness_g = factor(hum_monthly_data$act_illness_g,levels = c(1,0), labels = c("yes","no"))
table(hum_monthly_data$act_illness_g, useNA = "always")

# act_illness_h
table(hum_monthly_data$act_illness_h, useNA = "always")
str(hum_monthly_data$act_illness_h)
# coded with 0 as no and 1 as yes
# recode as a factor
hum_monthly_data$act_illness_h = factor(hum_monthly_data$act_illness_h,levels = c(1,0), labels = c("yes","no"))
table(hum_monthly_data$act_illness_h, useNA = "always")

# act_illness_i
table(hum_monthly_data$act_illness_i, useNA = "always")
str(hum_monthly_data$act_illness_i)
# coded with 0 as no and 1 as yes
# recode as a factor
hum_monthly_data$act_illness_i = factor(hum_monthly_data$act_illness_i,levels = c(1,0), labels = c("yes","no"))
table(hum_monthly_data$act_illness_i, useNA = "always")

# act_illness_j
table(hum_monthly_data$act_illness_j, useNA = "always")
str(hum_monthly_data$act_illness_j)
# coded with 0 as no and 1 as yes
# recode as a factor
hum_monthly_data$act_illness_j = factor(hum_monthly_data$act_illness_j,levels = c(1,0), labels = c("yes","no"))
table(hum_monthly_data$act_illness_j, useNA = "always")

# act_illness_k
table(hum_monthly_data$act_illness_k, useNA = "always")
str(hum_monthly_data$act_illness_k)
# coded with 0 as no and 1 as yes
# recode as a factor
hum_monthly_data$act_illness_k = factor(hum_monthly_data$act_illness_k,levels = c(1,0), labels = c("yes","no"))
table(hum_monthly_data$act_illness_k, useNA = "always")

# today2
table(hum_monthly_data$today2, useNA = "always")
str(hum_monthly_data$today2)
# compare this to the today variable
head(hum_monthly_data$today2)
head(hum_monthly_data$today)
tail(hum_monthly_data$today)
tail(hum_monthly_data$today2)
identical(hum_monthly_data$today2,hum_monthly_data$today)
summary(hum_monthly_data$today2)
summary(hum_monthly_data$today)
table(hum_monthly_data$today, useNA = "always")
str(hum_monthly_data$today)
# rename to stata today2 variable because same as today
hum_monthly_data$today2 <- NULL

# age_y_hum_monthly_data
summary(as.factor(hum_monthly_data$age_years))
table(hum_monthly_data$age_years, useNA = "always")
# check the recode
str(hum_monthly_data$age_years)
# recode the variable name to be unique
names(hum_monthly_data)[names(hum_monthly_data) == "age_years"] <- "age_y_hum_monthly_data"

# age_m_hum_monthly_data
summary(as.factor(hum_monthly_data$age_months))
table(hum_monthly_data$age_months, useNA = "always")
str(hum_monthly_data$age_months)
# looks good, clean
# recode the variable name to be unique
names(hum_monthly_data)[names(hum_monthly_data) == "age_months"] <- "age_m_hum_monthly_data"

# unq_memID
table(hum_monthly_data$unq_memID, useNA = "always")
# test that unq_memID was previously made correctly
test_unqmemID = paste0(hum_monthly_data$HH_ID,"_",hum_monthly_data$memID)
identical(hum_monthly_data$unq_memID,test_unqmemID)
for (i in 1:length(test_unqmemID)){
  if (!(identical(test_unqmemID[i],hum_monthly_data$unq_memID[i]))){
    print(paste0(test_unqmemID[i],"_",hum_monthly_data$unq_memID[i]))
  }
}
# looks like K09_14 was miscoded as K09_P for one of the data sets
# replace unq_memID with K09_14
# remove the K09_P entry
hum_monthly_data = hum_monthly_data[-which(hum_monthly_data$unq_memID=="K09_P"),]
# retest that the unq_memIDs are unique
test_unqmemID = paste0(hum_monthly_data$HH_ID,"_",hum_monthly_data$memID)
identical(hum_monthly_data$unq_memID,test_unqmemID)
# now are identical and correct




# ------------- hum_sick_data

# today2
table(hum_sick_data$today2, useNA = "always")
str(hum_sick_data$today2)
# looks good
hum_sick_data = rename(hum_sick_data,"today"="today2")

# village_name
table(hum_sick_data$village_name, useNA = "always")
str(hum_sick_data$village_name)
attr(hum_sick_data$village_name, "labels") 
# 1 is kinesamo, 2 maruti, 3 sitabicha
# recode as a factor
hum_sick_data$village_name = factor(hum_sick_data$village_name,levels = c(1,3,5), labels = c("Kinesamo","Maruti","Sitabicha"))
table(hum_sick_data$village_name, useNA = "always")

# gender 
table(hum_sick_data$gender, useNA = "always")
str(hum_sick_data$gender)
attr(hum_sick_data$gender, "labels") 
# 1 is male, 2 female
# recode as a factor
hum_sick_data$gender = factor(hum_sick_data$gender,levels = c(1,2), labels = c("male","female"))
table(hum_sick_data$gender, useNA = "always")

# relationship
# don't need, remove
hum_sick_data$relationship <- NULL

# other_relationship
# don't need, remove
hum_sick_data$other_relationship <- NULL

# mrdt_n
table(hum_sick_data$mrdt_n, useNA = "always")
str(hum_sick_data$mrdt_n)
attr(hum_sick_data$mrdt_n, "labels") 
# 1 is very likely, 2 likely, 3 50-50, 4 unlikely, 5 very unlikely, 6 don't know, 7 no response
# recode as a factor
hum_sick_data$mrdt_n = factor(hum_sick_data$mrdt_n,levels = c(1,2,3,4,5,6,7), labels = c("very likely","likely","50-50","unlikely","very unlikely","don't know","no response"))
table(hum_sick_data$mrdt_n, useNA = "always")

# mrdt_p
table(hum_sick_data$mrdt_p, useNA = "always")
str(hum_sick_data$mrdt_p)
attr(hum_sick_data$mrdt_p, "labels") 
# 1 is very likely, 2 likely, 3 50-50, 4 unlikely, 5 very unlikely, 6 don't know, 7 no response
# recode as a factor
hum_sick_data$mrdt_p = factor(hum_sick_data$mrdt_p,levels = c(1,2,3,4,5,6,7), labels = c("very likely","likely","50-50","unlikely","very unlikely","don't know","no response"))
table(hum_sick_data$mrdt_p, useNA = "always")

# malaria_al
table(hum_sick_data$malaria_al, useNA = "always")
str(hum_sick_data$malaria_al)
attr(hum_sick_data$malaria_al, "labels") 
# 1 is very likely, 2 likely, 3 50-50, 4 unlikely, 5 very unlikely, 6 don't know, 7 no response
# recode as a factor
hum_sick_data$malaria_al = factor(hum_sick_data$malaria_al,levels = c(1,2,3,4,5,6,7), labels = c("very likely","likely","50-50","unlikely","very unlikely","don't know","no response"))
table(hum_sick_data$malaria_al, useNA = "always")

# history
table(hum_sick_data$history, useNA = "always")
str(hum_sick_data$history)
attr(hum_sick_data$history, "labels") 
# 1 is yes, 2 no, 3 don't know
# recode as a factor
hum_sick_data$history = factor(hum_sick_data$history,levels = c(1,2,3), labels = c("yes","no","don't know"))
table(hum_sick_data$history, useNA = "always")

# complaints
table(hum_sick_data$complaints, useNA = "always")
str(hum_sick_data$complaints)
# list of different complaints/symptoms

# comp_oth
table(hum_sick_data$comp_oth, useNA = "always")
str(hum_sick_data$comp_oth)
# recode . and blanks to missingness
hum_sick_data$comp_oth[hum_sick_data$comp_oth == ""] = NA
hum_sick_data$comp_oth[hum_sick_data$comp_oth == "."] = NA
# check the recode
table(hum_sick_data$comp_oth, useNA = "always")
# list of many different symptoms outside the original symptom list, leaving as a chracter for now

# illness_d
table(hum_sick_data$illness_d, useNA = "always")
str(hum_sick_data$illness_d)
# looks good, clean

# severe
table(hum_sick_data$severe, useNA = "always")
str(hum_sick_data$severe)
attr(hum_sick_data$severe, "labels") 
# 1 is very severe, 2 severe, 3 moderate, 4 mild, 5 very mild
# recode as a factor
hum_sick_data$severe = factor(hum_sick_data$severe,levels = c(1,2,3,4,5), labels = c("very severe","severe","moderate","mild","very mild"))
table(hum_sick_data$severe, useNA = "always")

# medic
table(hum_sick_data$medic, useNA = "always")
str(hum_sick_data$medic)
attr(hum_sick_data$medic, "labels") 
# 1 is yes, 2 no
# recode as a factor
hum_sick_data$medic = factor(hum_sick_data$medic,levels = c(1,2), labels = c("yes","no"))
table(hum_sick_data$medic, useNA = "always")

# medicine
table(hum_sick_data$medicine, useNA = "always")
str(hum_sick_data$medicine)
attr(hum_sick_data$medicine, "labels") 
# recode blanks to missing
hum_sick_data$medicine[hum_sick_data$medicine == ""] = NA
hum_sick_data$medicine[hum_sick_data$medicine == "."] = NA
# check the recode
table(hum_sick_data$medicine, useNA = "always")

# med_oth
table(hum_sick_data$med_oth, useNA = "always")
str(hum_sick_data$med_oth)
# recode . and blanks to missing
hum_sick_data$med_oth[hum_sick_data$med_oth == ""] = NA
hum_sick_data$med_oth[hum_sick_data$med_oth == "."] = NA
# check the recode
table(hum_sick_data$med_oth, useNA = "always")

# consent - have you taken any medicine before coming for a test
table(hum_sick_data$consent, useNA = "always")
str(hum_sick_data$consent)
# 1 is yes, 2 no
# recode as a factor
hum_sick_data$consent = factor(hum_sick_data$consent,levels = c(1,2), labels = c("yes","no"))
table(hum_sick_data$consent, useNA = "always")

# mal_likely
table(hum_sick_data$mal_likely, useNA = "always")
str(hum_sick_data$mal_likely)
attr(hum_sick_data$mal_likely, "labels") 
# 1 is very likely, 2 likely, 3 50-50, 4 unlikely, 5 very unlikely, 6 don't know, 7 no response
# recode as a factor
hum_sick_data$mal_likely = factor(hum_sick_data$mal_likely,levels = c(1,2,3,4,5,6,7), labels = c("very likely","likely","50-50","unlikely","very unlikely","don't know","no response"))
table(hum_sick_data$mal_likely, useNA = "always")

# rdt_rst
table(hum_sick_data$rdt_rst, useNA = "always")
str(hum_sick_data$rdt_rst)
attr(hum_sick_data$rdt_rst, "labels") 
# 1 is positive, 2 negative, 3 invalid
# recode as a factor
hum_sick_data$rdt_rst = factor(hum_sick_data$rdt_rst,levels = c(1,2,3), labels = c("positive","negative","invalid"))
table(hum_sick_data$rdt_rst, useNA = "always")

# ill_mal
table(hum_sick_data$ill_mal, useNA = "always")
str(hum_sick_data$ill_mal)
attr(hum_sick_data$ill_mal, "labels") 
# 1 is very likely, 2 likely, 3 50-50, 4 unlikely, 5 very unlikely, 6 don't know, 7 no response
# recode as a factor
hum_sick_data$ill_mal = factor(hum_sick_data$ill_mal,levels = c(1,2,3,4,5,6,7), labels = c("very likely","likely","50-50","unlikely","very unlikely","don't know","no response"))
table(hum_sick_data$ill_mal, useNA = "always")

# taken_al
table(hum_sick_data$taken_al, useNA = "always")
str(hum_sick_data$taken_al)
attr(hum_sick_data$taken_al, "labels") 
# 1 is yes, 2 no, don't know/don't remember
# recode as a factor
hum_sick_data$taken_al = factor(hum_sick_data$taken_al,levels = c(1,2,3), labels = c("yes","no","don't know/don't remember"))
table(hum_sick_data$taken_al, useNA = "always")

# prescription
table(hum_sick_data$prescription, useNA = "always")
str(hum_sick_data$prescription)
attr(hum_sick_data$prescription, "labels") 
# 1 is prescribed, 2 referred
# recode as a factor
hum_sick_data$prescription = factor(hum_sick_data$prescription,levels = c(1,2), labels = c("prescribed","referred"))
table(hum_sick_data$prescription, useNA = "always")

# why
table(hum_sick_data$why, useNA = "always")
str(hum_sick_data$why)
attr(hum_sick_data$why, "labels") 
# recode blanks to missing
hum_sick_data$why[hum_sick_data$why == ""] = NA
hum_sick_data$why[hum_sick_data$why == "."] = NA
# check the recode
table(hum_sick_data$why, useNA = "always")

# key
table(hum_sick_data$key, useNA = "always")
str(hum_sick_data$key)

# HH_ID
table(hum_sick_data$HH_ID, useNA = "always")
str(hum_sick_data$HH_ID)
# looks good, clean

# fever
table(hum_sick_data$fever, useNA = "always")
str(hum_sick_data$fever)
attr(hum_sick_data$fever, "labels") 
# 0 is no, 1 yes
# recode as a factor
hum_sick_data$fever = factor(hum_sick_data$fever,levels = c(1,0), labels = c("yes","no"))
table(hum_sick_data$fever, useNA = "always")

# aches
table(hum_sick_data$Aches, useNA = "always")
str(hum_sick_data$Aches)
attr(hum_sick_data$Aches, "labels") 
# 0 is no, 1 yes
# recode as a factor
hum_sick_data$Aches = factor(hum_sick_data$Aches,levels = c(1,0), labels = c("yes","no"))
table(hum_sick_data$Aches, useNA = "always")

# vomiting
table(hum_sick_data$Vomiting, useNA = "always")
str(hum_sick_data$Vomiting)
attr(hum_sick_data$Vomiting, "labels") 
# 0 is no, 1 yes
# recode as a factor
hum_sick_data$Vomiting = factor(hum_sick_data$Vomiting,levels = c(1,0), labels = c("yes","no"))
table(hum_sick_data$Vomiting, useNA = "always")

# diarrhea
table(hum_sick_data$Diarrhea, useNA = "always")
str(hum_sick_data$Diarrhea)
attr(hum_sick_data$Diarrhea, "labels") 
# 0 is no, 1 yes
# recode as a factor
hum_sick_data$Diarrhea = factor(hum_sick_data$Diarrhea,levels = c(1,0), labels = c("yes","no"))
table(hum_sick_data$Diarrhea, useNA = "always")

# Chills
table(hum_sick_data$Chills, useNA = "always")
str(hum_sick_data$Chills)
attr(hum_sick_data$Chills, "labels") 
# 0 is no, 1 yes
# recode as a factor
hum_sick_data$Chills = factor(hum_sick_data$Chills,levels = c(1,0), labels = c("yes","no"))
table(hum_sick_data$Chills, useNA = "always")

# congestion
table(hum_sick_data$congestion, useNA = "always")
str(hum_sick_data$congestion)
attr(hum_sick_data$congestion, "labels") 
# 0 is no, 1 yes
# recode as a factor
hum_sick_data$congestion = factor(hum_sick_data$congestion,levels = c(1,0), labels = c("yes","no"))
table(hum_sick_data$congestion, useNA = "always")

# Cough
table(hum_sick_data$Cough, useNA = "always")
str(hum_sick_data$Cough)
attr(hum_sick_data$Cough, "labels") 
# 0 is no, 1 yes
# recode as a factor
hum_sick_data$Cough = factor(hum_sick_data$Cough,levels = c(1,0), labels = c("yes","no"))
table(hum_sick_data$Cough, useNA = "always")

# Other
table(hum_sick_data$Other, useNA = "always")
str(hum_sick_data$Other)
attr(hum_sick_data$Other, "labels") 
# 0 is no, 1 yes
# recode as a factor
hum_sick_data$Other = factor(hum_sick_data$Other,levels = c(1,0), labels = c("yes","no"))
table(hum_sick_data$Other, useNA = "always")

# medicine_ACT
table(hum_sick_data$medicine_ACT, useNA = "always")
str(hum_sick_data$medicine_ACT)
attr(hum_sick_data$medicine_ACT, "labels") 
# 0 is no, 1 yes
# recode as a factor
hum_sick_data$medicine_ACT = factor(hum_sick_data$medicine_ACT,levels = c(1,0), labels = c("yes","no"))
table(hum_sick_data$medicine_ACT, useNA = "always")

# medicine_Qui
table(hum_sick_data$medicine_Qui, useNA = "always")
str(hum_sick_data$medicine_Qui)
attr(hum_sick_data$medicine_Qui, "labels") 
# 0 is no, 1 yes
# recode as a factor
hum_sick_data$medicine_Qui = factor(hum_sick_data$medicine_Qui,levels = c(1,0), labels = c("yes","no"))
table(hum_sick_data$medicine_Qui, useNA = "always")

# medicine_SP
table(hum_sick_data$medicine_SP, useNA = "always")
str(hum_sick_data$medicine_SP)
attr(hum_sick_data$medicine_SP, "labels") 
# 0 is no, 1 yes
# recode as a factor
hum_sick_data$medicine_SP = factor(hum_sick_data$medicine_SP,levels = c(1,0), labels = c("yes","no"))
table(hum_sick_data$medicine_SP, useNA = "always")

# medicine_OACT
table(hum_sick_data$medicine_OACT, useNA = "always")
str(hum_sick_data$medicine_OACT)
attr(hum_sick_data$medicine_OACT, "labels") 
# 0 is no, 1 yes
# recode as a factor
hum_sick_data$medicine_OACT = factor(hum_sick_data$medicine_OACT,levels = c(1,0), labels = c("yes","no"))
table(hum_sick_data$medicine_OACT, useNA = "always")

# medicine_AMO
table(hum_sick_data$medicine_AMO, useNA = "always")
str(hum_sick_data$medicine_AMO)
attr(hum_sick_data$medicine_AMO, "labels") 
# 0 is no, 1 yes
# recode as a factor
hum_sick_data$medicine_AMO = factor(hum_sick_data$medicine_AMO,levels = c(1,0), labels = c("yes","no"))
table(hum_sick_data$medicine_AMO, useNA = "always")

# medicine_SPT
table(hum_sick_data$medicine_SPT, useNA = "always")
str(hum_sick_data$medicine_SPT)
attr(hum_sick_data$medicine_SPT, "labels") 
# 0 is no, 1 yes
# recode as a factor
hum_sick_data$medicine_SPT = factor(hum_sick_data$medicine_SPT,levels = c(1,0), labels = c("yes","no"))
table(hum_sick_data$medicine_SPT, useNA = "always")

# medicine_CIP
table(hum_sick_data$medicine_CIP, useNA = "always")
str(hum_sick_data$medicine_CIP)
attr(hum_sick_data$medicine_CIP, "labels") 
# 0 is no, 1 yes
# recode as a factor
hum_sick_data$medicine_CIP = factor(hum_sick_data$medicine_CIP,levels = c(1,0), labels = c("yes","no"))
table(hum_sick_data$medicine_CIP, useNA = "always")

# medicine_PAN
table(hum_sick_data$medicine_PAN, useNA = "always")
str(hum_sick_data$medicine_PAN)
attr(hum_sick_data$medicine_PAN, "labels") 
# 0 is no, 1 yes
# recode as a factor
hum_sick_data$medicine_PAN = factor(hum_sick_data$medicine_PAN,levels = c(1,0), labels = c("yes","no"))
table(hum_sick_data$medicine_PAN, useNA = "always")

# medicine_DNT
table(hum_sick_data$medicine_DNT, useNA = "always")
str(hum_sick_data$medicine_DNT)
attr(hum_sick_data$medicine_DNT, "labels") 
# 0 is no, 1 yes
# recode as a factor
hum_sick_data$medicine_DNT = factor(hum_sick_data$medicine_DNT,levels = c(1,0), labels = c("yes","no"))
table(hum_sick_data$medicine_DNT, useNA = "always")

# medicine_OTH
table(hum_sick_data$medicine_OTH, useNA = "always")
str(hum_sick_data$medicine_OTH)
attr(hum_sick_data$medicine_OTH, "labels") 
# 0 is no, 1 yes
# recode as a factor
hum_sick_data$medicine_OTH = factor(hum_sick_data$medicine_OTH,levels = c(1,0), labels = c("yes","no"))
table(hum_sick_data$medicine_OTH, useNA = "always")

# memID
table(hum_sick_data$memID, useNA = "always")
str(hum_sick_data$memID)
# there is a P which is odd but otherwise looks clean

# unq_memID
table(hum_sick_data$unq_memID, useNA = "always")
# test that unq_memID was previously made correctly
test_unqmemID = paste0(hum_sick_data$HH_ID,"_",hum_sick_data$memID)
identical(test_unqmemID,hum_sick_data$unq_memID)
for (i in 1:length(test_unqmemID)){
  if (!(identical(test_unqmemID[i],hum_sick_data$unq_memID[i]))){
    print(paste0(test_unqmemID[i],"_",hum_sick_data$unq_memID[i]))
  }
}
table(test_unqmemID, useNA = "always")
# looks fine

# age_y_hum_sick_data
summary(as.factor(hum_sick_data$age_years))
table(hum_sick_data$age_years, useNA = "always")
# check the recode
str(hum_sick_data$age_years)
# recode the variable name to be unique
names(hum_sick_data)[names(hum_sick_data) == "age_years"] <- "age_y_hum_sick_data"

# age_m_hum_sick_data
summary(as.factor(hum_sick_data$age_months))
table(hum_sick_data$age_months, useNA = "always")
str(hum_sick_data$age_months)
# looks good, clean
# recode the variable name to be unique
names(hum_sick_data)[names(hum_sick_data) == "age_months"] <- "age_m_hum_sick_data"

# age_type_hum_sick_data
summary(as.factor(hum_sick_data$age_type))
hum_sick_data$age_type = factor(hum_sick_data$age_type,levels = c(1,2), labels = c("below one year (<1 year)","one year and above (=> 1 year)"))
table(hum_sick_data$age_type, useNA = "always")
str(hum_sick_data$age_type)
# looks good, clean
# recode the variable name to be unique
names(hum_sick_data)[names(hum_sick_data) == "age_type"] <- "age_type_hum_sick_data"




# -------- annual_socdem_p2_data

# look at the colnames
colnames(annual_socdem_p2_data)

# q13f_relationship
table(annual_socdem_p2_data$q13f_relationship, useNA = "always")
str(annual_socdem_p2_data$q13f_relationship)
attr(annual_socdem_p2_data$q13f_relationship, "labels") 
# don't need, remove
annual_socdem_p2_data$q13f_relationship <- NULL

# oth_relat
table(annual_socdem_p2_data$oth_relat, useNA = "always")
str(annual_socdem_p2_data$oth_relat)
attr(annual_socdem_p2_data$oth_relat, "labels") 
# don't need remove,
annual_socdem_p2_data$oth_relat <- NULL

# educ_level
table(annual_socdem_p2_data$educ_level, useNA = "always")
str(annual_socdem_p2_data$educ_level)
attr(annual_socdem_p2_data$educ_level, "labels") 
annual_socdem_p2_data$educ_level = factor(annual_socdem_p2_data$educ_level,levels = c(1,2,3,4,5,6,7,8,9,10), labels = c("None","Pre-primary","Some Primary","Finished Primary","Some Secondary","Finished Secondary","Some post-secondary","Finished postsecondary","Other","Not applicable"))
table(annual_socdem_p2_data$educ_level, useNA = "always")

# oth_educ_level
table(annual_socdem_p2_data$oth_educ_level, useNA = "always")
# recode blanks and . to missing
annual_socdem_p2_data$oth_educ_level[annual_socdem_p2_data$oth_educ_level == ""] = NA
annual_socdem_p2_data$oth_educ_level[annual_socdem_p2_data$oth_educ_level == "."] = NA
annual_socdem_p2_data$oth_educ_level[annual_socdem_p2_data$oth_educ_level == "N/a"] = NA
table(annual_socdem_p2_data$oth_educ_level, useNA = "always")

# employment
table(annual_socdem_p2_data$employment, useNA = "always")
str(annual_socdem_p2_data$employment)
attr(annual_socdem_p2_data$employment, "labels") 
# make a factor
annual_socdem_p2_data$employment = factor(annual_socdem_p2_data$employment,levels = c(1,2,3,4,5,6,7,8,9), labels = c("Employed","Unemployed","Self Employed","Skilled Manual Laborer","Unskilled Manual Laborer","Farmer","Not applicable","Retired","Other"))
table(annual_socdem_p2_data$employment, useNA = "always")

# oth_emp
table(annual_socdem_p2_data$oth_emp, useNA = "always")
str(annual_socdem_p2_data$oth_emp)
attr(annual_socdem_p2_data$oth_emp, "labels") 
# recode blanks and . to missing
annual_socdem_p2_data$oth_emp[annual_socdem_p2_data$oth_emp == ""] = NA
annual_socdem_p2_data$oth_emp[annual_socdem_p2_data$oth_emp == "."] = NA
table(annual_socdem_p2_data$oth_emp, useNA = "always")

# sleep
table(annual_socdem_p2_data$sleep, useNA = "always")
str(annual_socdem_p2_data$sleep)
attr(annual_socdem_p2_data$sleep, "labels") 
# make a factor
annual_socdem_p2_data$sleep = factor(annual_socdem_p2_data$sleep,levels = c(1,2), labels = c("Yes","No"))
table(annual_socdem_p2_data$sleep, useNA = "always")

# know_rdt
table(annual_socdem_p2_data$know_rdt, useNA = "always")
str(annual_socdem_p2_data$know_rdt)
attr(annual_socdem_p2_data$know_rdt, "labels") 
# make a factor
annual_socdem_p2_data$know_rdt = factor(annual_socdem_p2_data$know_rdt,levels = c(1,2), labels = c("Yes","No"))
table(annual_socdem_p2_data$know_rdt, useNA = "always")

# mal_test
table(annual_socdem_p2_data$mal_test, useNA = "always")
str(annual_socdem_p2_data$mal_test)
attr(annual_socdem_p2_data$mal_test, "labels") 
# make a factor
annual_socdem_p2_data$mal_test = factor(annual_socdem_p2_data$mal_test,levels = c(1,2,3), labels = c("Yes","No","Don't know/don't remember"))
table(annual_socdem_p2_data$mal_test, useNA = "always")

# rdt_result_n
table(annual_socdem_p2_data$rdt_result_n, useNA = "always")
str(annual_socdem_p2_data$rdt_result_n)
attr(annual_socdem_p2_data$rdt_result_n, "labels") 
# make a factor
annual_socdem_p2_data$rdt_result_n = factor(annual_socdem_p2_data$rdt_result_n,levels = c(1,2,3,4,5,6,7), labels = c("Very likely","Likely","50-50","Unlikely","Very Unlikely","Don't Know","No response"))
table(annual_socdem_p2_data$rdt_result_n, useNA = "always")

# rdt_result_p
table(annual_socdem_p2_data$rdt_result_p, useNA = "always")
str(annual_socdem_p2_data$rdt_result_p)
attr(annual_socdem_p2_data$rdt_result_p, "labels") 
# make a factor
annual_socdem_p2_data$rdt_result_p = factor(annual_socdem_p2_data$rdt_result_p,levels = c(1,2,3,4,5,6,7), labels = c("Very likely","Likely","50-50","Unlikely","Very Unlikely","Don't Know","No response"))
table(annual_socdem_p2_data$rdt_result_p, useNA = "always")

# malaria_likely
table(annual_socdem_p2_data$malaria_likely, useNA = "always")
str(annual_socdem_p2_data$malaria_likely)
attr(annual_socdem_p2_data$malaria_likely, "labels") 
# looks good

# malaria_al
table(annual_socdem_p2_data$malaria_al, useNA = "always")
str(annual_socdem_p2_data$malaria_al)
attr(annual_socdem_p2_data$malaria_al, "labels") 
# make a factor
annual_socdem_p2_data$malaria_al = factor(annual_socdem_p2_data$malaria_al,levels = c(1,2,3,4,5,6,7), labels = c("Very likely","Likely","50-50","Unlikely","Very Unlikely","Don't Know","No response"))
table(annual_socdem_p2_data$malaria_al, useNA = "always")

# HH_ID
table(annual_socdem_p2_data$HH_ID, useNA = "always")
str(annual_socdem_p2_data$HH_ID)
attr(annual_socdem_p2_data$HH_ID, "labels") 
# looks good

# memID
table(annual_socdem_p2_data$memID, useNA = "always")
str(annual_socdem_p2_data$memID)
# looks good, leave as a character for merging

# today
table(annual_socdem_p2_data$today, useNA = "always")
str(annual_socdem_p2_data$today, UseNA = "always")
# check with today2 and remove today2 because incorrect
table(annual_socdem_p2_data$today2, useNA = "always")
annual_socdem_p2_data$today2 <- NULL
# recode funky feb 23, 2019 entry
annual_socdem_p2_data$today[annual_socdem_p2_data$today == "Feb 23, 2019"] = "2019-02-23"
table(annual_socdem_p2_data$today, useNA = "always")
# now change to date format
annual_socdem_p2_data$today = ymd(annual_socdem_p2_data$today)
table(annual_socdem_p2_data$today, useNA = "always")
str(annual_socdem_p2_data$today)

# unq_memID
table(annual_socdem_p2_data$unq_memID, useNA = "always")
# test that unq_memID was previously made correctly
test_unqmemID = paste0(annual_socdem_p2_data$HH_ID,"_",annual_socdem_p2_data$memID)
identical(test_unqmemID,annual_socdem_p2_data$unq_memID)
for (i in 1:length(test_unqmemID)){
  if (!(identical(test_unqmemID[i],annual_socdem_p2_data$unq_memID[i]))){
    print(paste0(test_unqmemID[i],"_",annual_socdem_p2_data$unq_memID[i]))
  }
}
table(test_unqmemID, useNA = "always")
# looks fine

# gender
table(annual_socdem_p2_data$gender, useNA = "always")
str(annual_socdem_p2_data$gender, UseNA = "always")
# make a factor
annual_socdem_p2_data$gender = factor(annual_socdem_p2_data$gender,levels = c(1,2), labels = c("Male","Female"))
table(annual_socdem_p2_data$gender, useNA = "always")

# uniq_key
length(which(is.na(annual_socdem_p2_data$uniq_key)))
str(annual_socdem_p2_data$uniq_key)
# looks good

# age
table(annual_socdem_p2_data$age, useNA = "always")
str(annual_socdem_p2_data$age, UseNA = "always")
# assume is in years, looks good but missing for 2 people

# date_enr
table(annual_socdem_p2_data$date_enr, useNA = "always")
str(annual_socdem_p2_data$date_enr, UseNA = "always")
# make in date format
annual_socdem_p2_data$date_enr = mdy(annual_socdem_p2_data$date_enr)
table(annual_socdem_p2_data$date_enr, useNA = "always")
str(annual_socdem_p2_data$date_enr, UseNA = "always")

# study_status
table(annual_socdem_p2_data$study_status, useNA = "always")
str(annual_socdem_p2_data$study_status, UseNA = "always")
# make a factor
annual_socdem_p2_data$study_status = factor(annual_socdem_p2_data$study_status,levels = c(1,2,3), labels = c("Continuing","Exited","Other specify"))
table(annual_socdem_p2_data$study_status, useNA = "always")

# status_oth
table(annual_socdem_p2_data$status_oth, useNA = "always")
str(annual_socdem_p2_data$status_oth, UseNA = "always")
annual_socdem_p2_data$status_oth[annual_socdem_p2_data$status_oth == ""] = NA

# month_exited
table(annual_socdem_p2_data$month_exited, useNA = "always")
str(annual_socdem_p2_data$month_exited, UseNA = "always")
annual_socdem_p2_data$month_exited[annual_socdem_p2_data$month_exited == ""] = NA



#### -------- make variables that are not unique have unique names ------------ ####

# gender
names(hum_monthly_data)[names(hum_monthly_data) == "gender"] <- "gender_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "gender"] <- "gender_hum_sick_data"
names(annual_socdem_p2_data)[names(annual_socdem_p2_data) == "gender"] <- "gender_hum_socdem2_data"

# key
names(hum_monthly_data)[names(hum_monthly_data) == "key"] <- "key_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "key"] <- "key_hum_sick_data"

# mal_likely
names(hum_monthly_data)[names(hum_monthly_data) == "mal_likely"] <- "mal_likely_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "mal_likely"] <- "mal_likely_hum_sick_data"
names(annual_socdem_p2_data)[names(annual_socdem_p2_data) == "mal_likely"] <- "mal_likely_hum_socdem2_data"

# malaria_al
names(hum_monthly_data)[names(hum_monthly_data) == "malaria_al"] <- "malaria_al_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "malaria_al"] <- "malaria_al_hum_sick_data"
names(annual_socdem_p2_data)[names(annual_socdem_p2_data) == "malaria_al"] <- "malaria_al_hum_socdem2_data"

# med_oth
names(hum_monthly_data)[names(hum_monthly_data) == "med_oth"] <- "med_oth_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "med_oth"] <- "med_oth_hum_sick_data"

# medicine
names(hum_monthly_data)[names(hum_monthly_data) == "medicine"] <- "medicine_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "medicine"] <- "medicine_hum_sick_data"

# medicine_ACT
names(hum_monthly_data)[names(hum_monthly_data) == "medicine_ACT"] <- "medicine_ACT_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "medicine_ACT"] <- "medicine_ACT_hum_sick_data"

# medicine_AMO
names(hum_monthly_data)[names(hum_monthly_data) == "medicine_AMO"] <- "medicine_AMO_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "medicine_AMO"] <- "medicine_AMO_hum_sick_data"

# medicine_CIP
names(hum_monthly_data)[names(hum_monthly_data) == "medicine_CIP"] <- "medicine_CIP_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "medicine_CIP"] <- "medicine_CIP_hum_sick_data"

# medicine_DNT
names(hum_monthly_data)[names(hum_monthly_data) == "medicine_DNT"] <- "medicine_DNT_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "medicine_DNT"] <- "medicine_DNT_hum_sick_data"

# medicine_OACT
names(hum_monthly_data)[names(hum_monthly_data) == "medicine_OACT"] <- "medicine_OACT_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "medicine_OACT"] <- "medicine_OACT_hum_sick_data"

# medicine_OTH
names(hum_monthly_data)[names(hum_monthly_data) == "medicine_OTH"] <- "medicine_OTH_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "medicine_OTH"] <- "medicine_OTH_hum_sick_data"

# medicine_PAN
names(hum_monthly_data)[names(hum_monthly_data) == "medicine_PAN"] <- "medicine_PAN_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "medicine_PAN"] <- "medicine_PAN_hum_sick_data"

# medicine_Qui
names(hum_monthly_data)[names(hum_monthly_data) == "medicine_Qui"] <- "medicine_Qui_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "medicine_Qui"] <- "medicine_Qui_hum_sick_data"

# medicine_SP
names(hum_monthly_data)[names(hum_monthly_data) == "medicine_SP"] <- "medicine_SP_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "medicine_SP"] <- "medicine_SP_hum_sick_data"

# medicine_SPT
names(hum_monthly_data)[names(hum_monthly_data) == "medicine_SPT"] <- "medicine_SPT_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "medicine_SPT"] <- "medicine_SPT_hum_sick_data"

# mrdt_n
names(hum_monthly_data)[names(hum_monthly_data) == "mrdt_n"] <- "mrdt_n_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "mrdt_n"] <- "mrdt_n_hum_sick_data"

# mrdt_p
names(hum_monthly_data)[names(hum_monthly_data) == "mrdt_p"] <- "mrdt_p_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "mrdt_p"] <- "mrdt_p_hum_sick_data"

# today
names(hum_monthly_data)[names(hum_monthly_data) == "today"] <- "today_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "today"] <- "today_hum_sick_data"

# village_name
names(hum_sick_data)[names(hum_sick_data) == "village_name"] <- "village_name_hum_sick_data"
names(hum_monthly_data)[names(hum_monthly_data) == "village_name"] <- "village_name_hum_monthly_data"








#### --------- export each separate data set as a CSV or RDS file --------- ####

# for hum_monthly_data
write_csv(hum_monthly_data, "Desktop/hum_monthly_data_v2_17SEP2019.csv")
write_rds(hum_monthly_data, "Desktop/hum_monthly_data_v2_17SEP2019.RDS")

# for hum_sick_data
write_csv(hum_sick_data, "Desktop/hum_sick_data_v2_17SEP2019.csv")
write_rds(hum_sick_data, "Desktop/hum_sick_data_v2_17SEP2019.RDS")

# for hum_socdem2_data
write_csv(annual_socdem_p2_data, "Desktop/hum_socdem2_data_v2_17SEP2019.csv")
write_rds(annual_socdem_p2_data, "Desktop/hum_socdem2_data_v2_17SEP2019.RDS")




