# ----------------------------------------- #
#        Spat21 Data Set Cleaning           #
#                Human Data                 #
#            November 26, 2018              #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)


#### --------- read in human data ----------------- ####
# read in the human data sets (have to import as stata data sets)
hum_monthly_data = MozzieMonthlyData_to_31July2018_deID
hum_sleeping_data = MozzieSleepingSpaces_Baseline2017_deID_Dec2018
hum_table_household_data = MozzieHHMembershipTable_Baseline2017_deID
hum_ann_household_data = MozzieAnnualHouseholdData_Baseline2017_deID
hum_sick_data = MozzieSickVisitData_to_31July2018_deID_Dec2018

# look at summaries of all the data sets
summary(hum_monthly_data)
summary(hum_sleeping_data)
summary(hum_table_household_data)
summary(hum_ann_household_data)
summary(hum_sick_data)

# output a csv file of all the variable names
names1 = names(hum_monthly_data)
names2 = names(hum_sleeping_data)
names3 = names(hum_table_household_data)
names4 = names(hum_ann_household_data)
names5 = names(hum_sick_data)
allnames = c(names1,names2,names3,names4,names5)
allnames = data.frame(allnames)
write_csv(allnames,"spat21_data_dictionary.csv")



#### --------- clean each variable in human data sets ----------------- ####

## -------- hum_monthly_data

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

# time_out_net
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
# need to standardize the coding for places but not sure what the correct coding would be
# make a factor
hum_monthly_data$to_where = as.factor(hum_monthly_data$to_where)

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

# test_type_oth
table(hum_monthly_data$test_type_oth, useNA = "always")
str(hum_monthly_data$test_type_oth)
# recode the blanks and . to missing
hum_monthly_data$test_type_oth[hum_monthly_data$test_type_oth == ""] = NA
hum_monthly_data$test_type_oth[hum_monthly_data$test_type_oth == "."] = NA
hum_monthly_data$test_type_oth[hum_monthly_data$test_type_oth == "RDT - Microscopy"] = "rdt and microscopy"
hum_monthly_data$test_type_oth[hum_monthly_data$test_type_oth == "RDT and microscopy"] = "rdt and microscopy"
# check the recode
table(hum_monthly_data$test_type_oth, useNA = "always")
# make a factor
hum_monthly_data$test_type_oth = as.factor(hum_monthly_data$test_type_oth)

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

# date_today
table(hum_monthly_data$date_today, useNA = "always")
str(hum_monthly_data$date_today)
# compare this to the today variable
head(hum_monthly_data$date_today)
head(hum_monthly_data$today)
tail(hum_monthly_data$today)
tail(hum_monthly_data$date_today)
identical(hum_monthly_data$date_today,hum_monthly_data$today)
summary(hum_monthly_data$date_today)
summary(hum_monthly_data$today)
table(hum_monthly_data$today, useNA = "always")
str(hum_monthly_data$today)
# rename to stata date_today variable because same as today
hum_monthly_data$date_today <- NULL

# create a new variable for the unique member id (unq_memID)
# this variable combines the HH_ID and memID variables to create a unique identifier for each person in the data set
hum_monthly_data$unq_memID = paste0(hum_monthly_data$HH_ID,"_",hum_monthly_data$memID)
# check the variable creation
table(hum_monthly_data$unq_memID, useNA = "always")
table(hum_monthly_data$HH_ID, useNA = "always")
table(hum_monthly_data$memID, useNA = "always")
head(hum_monthly_data$unq_memID)
head(hum_monthly_data$HH_ID)
head(hum_monthly_data$memID)



## -------- hum_sleeping_data

# today
summary(hum_sleeping_data$today)
head(hum_sleeping_data$today)
str(hum_sleeping_data$today)
# originally not in date format but a character with MDY
# recoded in date format
newdate3 = ymd(hum_sleeping_data$today)
head(newdate3)
head(hum_sleeping_data$today)
summary(newdate3)
str(newdate3)
hum_sleeping_data$today = newdate3

# roof
table(hum_sleeping_data$roof, useNA = "always")
str(hum_sleeping_data$roof)
# coded with 1 as thatched, 2 mabati, 3 cement, 4 tiles, 5 no roof/open, 6 other
# recode as a factor
hum_sleeping_data$roof = factor(hum_sleeping_data$roof,levels = c(1,2,3,4,5,6), labels = c("thatched","mabati","cement","tiles","no roof/open","other"))
table(hum_sleeping_data$roof, useNA = "always")

# oth_roof
table(hum_sleeping_data$oth_roof, useNA = "always")
# all missing so remove from data set
hum_sleeping_data$oth_roof <- NULL

# floor
table(hum_sleeping_data$floor, useNA = "always")
str(hum_sleeping_data$floor)
# coded with 1 as mud/earth, 2 cement, 3 tiles, 4 wooden planks, 5 other
# recode as a factor
hum_sleeping_data$floor = factor(hum_sleeping_data$floor,levels = c(1,2,3,4,5), labels = c("mud/earth","cement","tiles","wooden planks","other"))
table(hum_sleeping_data$floor, useNA = "always")

# oth_floor
table(hum_sleeping_data$oth_floor, useNA = "always")
# all observations missing so removed from data set
hum_sleeping_data$oth_floor <- NULL

# wall
table(hum_sleeping_data$wall, useNA = "always")
str(hum_sleeping_data$wall)
# coded with 1 as mud/earth, 2 mabati, 3 cement/blocks, 4 bricks, 5 wooden planks, 6 stone, 7 other
# recode as a factor
hum_sleeping_data$wall = factor(hum_sleeping_data$wall,levels = c(1,2,3,4,5,6,7), labels = c("mud/earth","mabati","cement/blocks","bricks","wooden planks","stone","other"))
table(hum_sleeping_data$wall, useNA = "always")

# oth_wall
table(hum_sleeping_data$oth_wall, useNA = "always")
# all observations missing so removed from data set
hum_sleeping_data$oth_wall <- NULL

# gap
table(hum_sleeping_data$gap, useNA = "always")
str(hum_sleeping_data$gap)
# coded with 1 as yes, 2 no, 3 can't tell, 4 not applicable (ie no roof)
# recode as a factor
hum_sleeping_data$gap = factor(hum_sleeping_data$gap,levels = c(1,2,3,4), labels = c("yes","no","can't tell","not applicable (ie no roof)"))
table(hum_sleeping_data$gap, useNA = "always")

# gap_net
table(hum_sleeping_data$gap_net, useNA = "always")
str(hum_sleeping_data$gap_net)
# coded with 1 as all, 2 some, 3 none, 4 not applicable because no windows
# recode as a factor
hum_sleeping_data$gap_net = factor(hum_sleeping_data$gap_net,levels = c(1,2,3,4), labels = c("all","some","none","not applicable because no windows"))
table(hum_sleeping_data$gap_net, useNA = "always")

# sleeping_s
table(hum_sleeping_data$sleeping_s, useNA = "always")
str(hum_sleeping_data$sleeping_s)
# coded with 1 as yes, 2 as no
# recode as a factor
hum_sleeping_data$sleeping_s = factor(hum_sleeping_data$sleeping_s,levels = c(1,2), labels = c("yes","no"))
table(hum_sleeping_data$sleeping_s, useNA = "always")

# parent_key
table(hum_sleeping_data$parent_key, useNA = "always")
str(hum_sleeping_data$parent_key)

# net
table(hum_sleeping_data$net, useNA = "always")
str(hum_sleeping_data$net)
# coded with 1 as yes, 2 as no
# recode as a factor
hum_sleeping_data$net = factor(hum_sleeping_data$net,levels = c(1,2), labels = c("yes","no"))
table(hum_sleeping_data$net, useNA = "always")

# net_hang
table(hum_sleeping_data$net_hang, useNA = "always")
str(hum_sleeping_data$net_hang)
# coded with 1 as yes, 2 as no
# recode as a factor
hum_sleeping_data$net_hang = factor(hum_sleeping_data$net_hang,levels = c(1,2), labels = c("yes","no"))
table(hum_sleeping_data$net_hang, useNA = "always")

# net_used
table(hum_sleeping_data$net_used, useNA = "always")
str(hum_sleeping_data$net_used)
# coded with 1 as yes, 2 as no
# recode as a factor
hum_sleeping_data$net_used = factor(hum_sleeping_data$net_used,levels = c(1,2), labels = c("yes","no"))
table(hum_sleeping_data$net_used, useNA = "always")

# net_times
table(hum_sleeping_data$net_times, useNA = "always")
str(hum_sleeping_data$net_times)
# coded as numeric, looks good, clean

# net_notused
table(hum_sleeping_data$net_notused, useNA = "always")
str(hum_sleeping_data$net_notused)
# coded with 1 as yes, 2 as no
# recode as a factor
hum_sleeping_data$net_notused = factor(hum_sleeping_data$net_notused,levels = c(1,2), labels = c("yes","no"))
table(hum_sleeping_data$net_notused, useNA = "always")

# when_net_notused
table(hum_sleeping_data$when_net_notused, useNA = "always")
str(hum_sleeping_data$when_net_notused)
# recode the blanks and . to missing
hum_sleeping_data$when_net_notused[hum_sleeping_data$when_net_notused == ""] = NA
hum_sleeping_data$when_net_notused[hum_sleeping_data$when_net_notused == "."] = NA
hum_sleeping_data$when_net_notused[hum_sleeping_data$when_net_notused == "Duting dry season"] = "During dry season"
hum_sleeping_data$when_net_notused[hum_sleeping_data$when_net_notused == "It is always in use"] = "Always in use"
hum_sleeping_data$when_net_notused[hum_sleeping_data$when_net_notused == "N/a"] = "Not applicable"
hum_sleeping_data$when_net_notused[hum_sleeping_data$when_net_notused == "N/A"] = "Not applicable"
hum_sleeping_data$when_net_notused[hum_sleeping_data$when_net_notused == "NA"] = "Not applicable"
hum_sleeping_data$when_net_notused[hum_sleeping_data$when_net_notused == "Non"] = "Not applicable"
hum_sleeping_data$when_net_notused[hum_sleeping_data$when_net_notused == "None"] = "Not applicable"
hum_sleeping_data$when_net_notused[hum_sleeping_data$when_net_notused == "Only yesterday when she niticed sone bedbugs so she put the mattress on the floor hence did not use the net"] = "Found bedbugs in bed so moved mattress to floor"
hum_sleeping_data$when_net_notused[hum_sleeping_data$when_net_notused == "Used always"] = "Always in use"
hum_sleeping_data$when_net_notused[hum_sleeping_data$when_net_notused == "Went funeral"] = "During funeral"
hum_sleeping_data$when_net_notused[hum_sleeping_data$when_net_notused == "When is very hot"] = "When is hot"
hum_sleeping_data$when_net_notused[hum_sleeping_data$when_net_notused == "When on a journey"] = "If travelled"
hum_sleeping_data$when_net_notused[hum_sleeping_data$when_net_notused == "When she is at school"] = "When at school"
hum_sleeping_data$when_net_notused[hum_sleeping_data$when_net_notused == "When gone to boarding school"] = "When at school"
hum_sleeping_data$when_net_notused[hum_sleeping_data$when_net_notused == "When the weather is hot and mosquitoes are fewer"] = "When is hot"
hum_sleeping_data$when_net_notused[hum_sleeping_data$when_net_notused == "When there are no mosquiotoes"] = "When there are no mosquitoes"
hum_sleeping_data$when_net_notused[hum_sleeping_data$when_net_notused == "Whrn tired and has not tied up the net"] = "When tired and has not tied up the net"
# check the recode
table(hum_sleeping_data$when_net_notused, useNA = "always")
# make a factor
hum_sleeping_data$when_net_notused = as.factor(hum_sleeping_data$when_net_notused)

# d_sleeping_space
table(hum_sleeping_data$d_sleeping_space, useNA = "always")
str(hum_sleeping_data$d_sleeping_space)
# coded with 1 as mattress on bedframe, 2 mattress on floor, 3 matt, blanket or other bedding on floor, 4 sitting room furniture, 5 other
# recode as a factor
hum_sleeping_data$d_sleeping_space = factor(hum_sleeping_data$d_sleeping_space,levels = c(1,2,3,4,5), labels = c("mattress on bedframe","mattress on floor","matt, blanket or other bedding on floor","sitting room furniture","other"))
table(hum_sleeping_data$d_sleeping_space, useNA = "always")

# d_sleeping_spoth
table(hum_sleeping_data$d_sleeping_spoth, useNA = "always")
str(hum_sleeping_data$d_sleeping_spoth)
# recode the blanks and . to missing
hum_sleeping_data$d_sleeping_spoth[hum_sleeping_data$d_sleeping_spoth == ""] = NA
hum_sleeping_data$d_sleeping_spoth[hum_sleeping_data$d_sleeping_spoth == "."] = NA
# check the recode
table(hum_sleeping_data$d_sleeping_spoth, useNA = "always")
# all observations missing, remove from data set
hum_sleeping_data$d_sleeping_spoth <- NULL

# d_net_hanging
table(hum_sleeping_data$d_net_hanging, useNA = "always")
str(hum_sleeping_data$d_net_hanging)
# recode the blanks and . to missing
hum_sleeping_data$d_net_hanging[hum_sleeping_data$d_net_hanging == ""] = NA
hum_sleeping_data$d_net_hanging[hum_sleeping_data$d_net_hanging == "."] = NA
# check the recode
table(hum_sleeping_data$d_net_hanging, useNA = "always")

# d_net_hangoth
table(hum_sleeping_data$d_net_hangoth, useNA = "always")
str(hum_sleeping_data$d_net_hangoth)
# recode the blanks and . to missing
hum_sleeping_data$d_net_hangoth[hum_sleeping_data$d_net_hangoth == ""] = NA
hum_sleeping_data$d_net_hangoth[hum_sleeping_data$d_net_hangoth == "."] = NA
# check the recode
table(hum_sleeping_data$d_net_hangoth, useNA = "always")
# all observations missing, remove from data set
hum_sleeping_data$d_net_hangoth <- NULL

# when_net_acquired
table(hum_sleeping_data$when_net_acquired, useNA = "always")
str(hum_sleeping_data$when_net_acquired)
# recode the blanks and . to missing
hum_sleeping_data$when_net_acquired[hum_sleeping_data$when_net_acquired == ""] = NA
hum_sleeping_data$when_net_acquired[hum_sleeping_data$when_net_acquired == "."] = NA
# check the recode
table(hum_sleeping_data$when_net_acquired, useNA = "always")
# change to a date variable
datenew = mdy(hum_sleeping_data$when_net_acquired)
head(datenew)
head(hum_sleeping_data$when_net_acquired)
summary(datenew)
summary(hum_sleeping_data$when_net_acquired)
hum_sleeping_data$when_net_acquired = datenew
str(hum_sleeping_data$when_net_acquired)

# treated
table(hum_sleeping_data$treated, useNA = "always")
str(hum_sleeping_data$treated)
# coded with 1 as yes, 2 as no
# recode as a factor
hum_sleeping_data$treated = factor(hum_sleeping_data$treated,levels = c(1,2), labels = c("yes","no"))
table(hum_sleeping_data$treated, useNA = "always")

# pay
table(hum_sleeping_data$pay, useNA = "always")
str(hum_sleeping_data$pay)
# coded with 1 as yes, 2 as no
# recode as a factor
hum_sleeping_data$pay = factor(hum_sleeping_data$pay,levels = c(1,2), labels = c("yes","no"))
table(hum_sleeping_data$pay, useNA = "always")

# how_much
table(hum_sleeping_data$how_much, useNA = "always")
str(hum_sleeping_data$how_much)
# coded numerically, looks good

# treat
table(hum_sleeping_data$treat, useNA = "always")
str(hum_sleeping_data$treat)
# coded with 1 as yes, 2 as no
# recode as a factor
hum_sleeping_data$treat = factor(hum_sleeping_data$treat,levels = c(1,2), labels = c("yes","no"))
table(hum_sleeping_data$treat, useNA = "always")

# chemical
table(hum_sleeping_data$chemical, useNA = "always")
str(hum_sleeping_data$chemical)
# recode the blanks and . to missing
hum_sleeping_data$chemical[hum_sleeping_data$chemical == ""] = NA
hum_sleeping_data$chemical[hum_sleeping_data$chemical == "."] = NA
# check the recode
table(hum_sleeping_data$chemical, useNA = "always")
# all observations missing, remove from data set
hum_sleeping_data$chemical <- NULL

# treat_last
table(hum_sleeping_data$treat_last, useNA = "always")
str(hum_sleeping_data$treat_last)
# recode the blanks and . to missing
hum_sleeping_data$treat_last[hum_sleeping_data$treat_last == ""] = NA
hum_sleeping_data$treat_last[hum_sleeping_data$treat_last == "."] = NA
# check the recode
table(hum_sleeping_data$treat_last, useNA = "always")
# all observations missing, remove from data set
hum_sleeping_data$treat_last <- NULL

# wash
table(hum_sleeping_data$wash, useNA = "always")
str(hum_sleeping_data$wash)
# coded with 1 as yes, 2 as no
# recode as a factor
hum_sleeping_data$wash = factor(hum_sleeping_data$wash,levels = c(1,2), labels = c("yes","no"))
table(hum_sleeping_data$wash, useNA = "always")

# wash_often
table(hum_sleeping_data$wash_often, useNA = "always")
str(hum_sleeping_data$wash_often)
# coded with 1 as weekly, 2 as monthly, 3 as several times per year, 4 as once per year
# recode as a factor
hum_sleeping_data$wash_often = factor(hum_sleeping_data$wash_often,levels = c(1,2,3,4), labels = c("weekly","monthly","several times per year","once per year"))
table(hum_sleeping_data$wash_often, useNA = "always")

# holes
table(hum_sleeping_data$holes, useNA = "always")
str(hum_sleeping_data$holes)
# coded with 1 as none, 2 as <5, 3 as 5 to 10, 4 as >10, 5 as don't know/could not see the net
# recode as a factor
hum_sleeping_data$holes = factor(hum_sleeping_data$holes,levels = c(1,2,3,4,5), labels = c("None","<5","5-10",">10","Don't know/could not see the net"))
table(hum_sleeping_data$holes, useNA = "always")

# loc_holes
table(hum_sleeping_data$loc_holes, useNA = "always")
str(hum_sleeping_data$loc_holes)
# coded with 1 as top, 2 as sides, 3 as both top & sides
# recode as a factor
hum_sleeping_data$loc_holes = factor(hum_sleeping_data$loc_holes,levels = c(1,2,3), labels = c("top","sides","both top & sides"))
table(hum_sleeping_data$loc_holes, useNA = "always")

# hole_size
table(hum_sleeping_data$hole_size, useNA = "always")
str(hum_sleeping_data$hole_size)
# coded with 1 as smaller than a coin, 2 larger than a coin, 3 larger than a hand
# recode as a factor
hum_sleeping_data$hole_size = factor(hum_sleeping_data$hole_size,levels = c(1,2,3), labels = c("smaller than a coin","larger than a coin","larger than a hand"))
table(hum_sleeping_data$hole_size, useNA = "always")

# HH_ID
table(hum_sleeping_data$HH_ID, useNA = "always")
str(hum_sleeping_data$HH_ID)
# looks good, keep as a character

# net_floor
table(hum_sleeping_data$net_floor, useNA = "always")
str(hum_sleeping_data$net_floor)
# coded with 0 as no, 1 as yes
# recode as a factor
hum_sleeping_data$net_floor = factor(hum_sleeping_data$net_floor,levels = c(1,0), labels = c("yes","no"))
table(hum_sleeping_data$net_floor, useNA = "always")

# net_N_floor
table(hum_sleeping_data$net_N_floor, useNA = "always")
str(hum_sleeping_data$net_N_floor)
# coded with 0 as no, 1 as yes
# recode as a factor
hum_sleeping_data$net_N_floor = factor(hum_sleeping_data$net_N_floor,levels = c(1,0), labels = c("yes","no"))
table(hum_sleeping_data$net_N_floor, useNA = "always")

# net_p_fixed
table(hum_sleeping_data$net_p_fixed, useNA = "always")
str(hum_sleeping_data$net_p_fixed)
# coded with 0 as no, 1 as yes
# recode as a factor
hum_sleeping_data$net_p_fixed = factor(hum_sleeping_data$net_p_fixed,levels = c(1,0), labels = c("yes","no"))
table(hum_sleeping_data$net_p_fixed, useNA = "always")

# net_floor
table(hum_sleeping_data$net_t_fixed, useNA = "always")
str(hum_sleeping_data$net_t_fixed)
# coded with 0 as no, 1 as yes
# recode as a factor
hum_sleeping_data$net_t_fixed = factor(hum_sleeping_data$net_t_fixed,levels = c(1,0), labels = c("yes","no"))
table(hum_sleeping_data$net_t_fixed, useNA = "always")

# net_Other
table(hum_sleeping_data$net_Other, useNA = "always")
str(hum_sleeping_data$net_Other)
# coded with 0 as no, 1 as yes
# recode as a factor
hum_sleeping_data$net_Other = factor(hum_sleeping_data$net_Other,levels = c(1,0), labels = c("yes","no"))
table(hum_sleeping_data$net_Other, useNA = "always")

# bld_slpid
table(hum_sleeping_data$bld_slpid, useNA = "always")
str(hum_sleeping_data$bld_slpid)
# not sure what this variable is
# recode the blanks to missing
hum_sleeping_data$bld_slpid[hum_sleeping_data$bld_slpid == ""] = NA
# check the recode
table(hum_sleeping_data$bld_slpid, useNA = "always")

# bldid
table(hum_sleeping_data$bldid, useNA = "always")
str(hum_sleeping_data$bldid)
# recode the blanks to missing
hum_sleeping_data$bldid[hum_sleeping_data$bldid == ""] = NA
# check the recode
table(hum_sleeping_data$bldid, useNA = "always")

# slpid
table(hum_sleeping_data$slpid, useNA = "always")
str(hum_sleeping_data$slpid)
# recode the blanks to missing
hum_sleeping_data$slpid[hum_sleeping_data$slpid == ""] = NA
# check the recode
table(hum_sleeping_data$slpid, useNA = "always")

# hh_mem_name1
table(hum_sleeping_data$hh_mem_name1, useNA = "always")
str(hum_sleeping_data$hh_mem_name1)
# recode the blanks to missing
hum_sleeping_data$hh_mem_name1[hum_sleeping_data$hh_mem_name1 == ""] = NA
# check the recode
table(hum_sleeping_data$hh_mem_name1, useNA = "always")

# hh_mem_name2
table(hum_sleeping_data$hh_mem_name2, useNA = "always")
str(hum_sleeping_data$hh_mem_name2)
# recode the blanks to missing
hum_sleeping_data$hh_mem_name2[hum_sleeping_data$hh_mem_name2 == ""] = NA
# check the recode
table(hum_sleeping_data$hh_mem_name2, useNA = "always")

# hh_mem_name3
table(hum_sleeping_data$hh_mem_name3, useNA = "always")
str(hum_sleeping_data$hh_mem_name3)
# recode the blanks to missing
hum_sleeping_data$hh_mem_name3[hum_sleeping_data$hh_mem_name3 == ""] = NA
# check the recode
table(hum_sleeping_data$hh_mem_name3, useNA = "always")

# hh_mem_name4
table(hum_sleeping_data$hh_mem_name4, useNA = "always")
str(hum_sleeping_data$hh_mem_name4)
# recode the blanks to missing
hum_sleeping_data$hh_mem_name4[hum_sleeping_data$hh_mem_name4 == ""] = NA
# check the recode
table(hum_sleeping_data$hh_mem_name4, useNA = "always")

# hh_mem_name5
table(hum_sleeping_data$hh_mem_name5, useNA = "always")
str(hum_sleeping_data$hh_mem_name5)
# recode the blanks to missing
hum_sleeping_data$hh_mem_name5[hum_sleeping_data$hh_mem_name5 == ""] = NA
# check the recode
table(hum_sleeping_data$hh_mem_name5, useNA = "always")

# hh_mem_name6
table(hum_sleeping_data$hh_mem_name6, useNA = "always")
str(hum_sleeping_data$hh_mem_name6)
# recode the blanks to missing
hum_sleeping_data$hh_mem_name6[hum_sleeping_data$hh_mem_name6 == ""] = NA
# check the recode
table(hum_sleeping_data$hh_mem_name6, useNA = "always")

# unq_slp_space_id
# look at the bld_slpid
table(hum_sleeping_data$bld_slpid, useNA = "always")
length(which(is.na(hum_sleeping_data$bld_slpid)))
# look at the HH_ID
table(hum_sleeping_data$HH_ID, useNA = "always")
length(which(is.na(hum_sleeping_data$HH_ID)))
# merge the two variables togther with a /
hum_sleeping_data$unq_slp_space_id = ifelse(is.na(hum_sleeping_data$bld_slpid),NA,paste0(hum_sleeping_data$HH_ID,"/",hum_sleeping_data$bld_slpid))
# look at the new variable to check
table(hum_sleeping_data$unq_slp_space_id, useNA = "always")
head(hum_sleeping_data$HH_ID)
head(hum_sleeping_data$bld_slpid)
head(hum_sleeping_data$unq_slp_space_id)
tail(hum_sleeping_data$HH_ID)
tail(hum_sleeping_data$bld_slpid)
tail(hum_sleeping_data$unq_slp_space_id)


#--------------- hum_table_household_data

# today
summary(hum_table_household_data$today)
head(hum_table_household_data$today)
str(hum_table_household_data$today)
# originally not in date format but a character with MDY
# recoded in date format
newdate4 = ymd(hum_table_household_data$today)
head(newdate4)
head(hum_table_household_data$today)
summary(newdate4)
str(newdate4)
hum_table_household_data$today = newdate4
summary(hum_table_household_data$today)

# age_type
table(hum_table_household_data$age_type, useNA = "always")
str(hum_table_household_data$age_type)
# coded with 1 as below one year (<1 year), 2 as one year and above (=>1 year)
# recode as a factor
hum_table_household_data$age_type = factor(hum_table_household_data$age_type,levels = c(1,2), labels = c("below one year (<1 year)","one year and above (=> 1 year)"))
table(hum_table_household_data$age_type, useNA = "always")

# age_y
table(hum_table_household_data$age_y, useNA = "always")
str(hum_table_household_data$age_y)
# recode the . to missing and >85 to 85
hum_table_household_data$age_y[hum_table_household_data$age_y == "."] = NA
hum_table_household_data$age_y[hum_table_household_data$age_y == ">85"] = 85
# change to numeric format
hum_table_household_data$age_y = as.numeric(hum_table_household_data$age_y)
# check the recode
table(hum_table_household_data$age_y, useNA = "always")
summary(hum_table_household_data$age_y)

# age_m
table(hum_table_household_data$age_m, useNA = "always")
str(hum_table_household_data$age_m)
# coding looks good, clean

# educ_level
table(hum_table_household_data$educ_level, useNA = "always")
str(hum_table_household_data$educ_level)
attr(hum_table_household_data$educ_level, "labels") 
# 1 is none, 2 pre-primary, 3 some primary, 4 finished primary, 5 some secondary, 6 finished secondary,
# 7 some post-secondary, 8 finished post-secondary, 9 other, 10 not applicable
# recode as a factor
hum_table_household_data$educ_level = factor(hum_table_household_data$educ_level,levels = c(1,2,3,4,5,6,7,8,9,10), labels = c("none","pre-primary","some primary","finished primary","some secondary","finished secondary","some post-secondary","finished post-secondary","other","not applicable"))
table(hum_table_household_data$educ_level, useNA = "always")

# oth_educ_level
table(hum_table_household_data$oth_educ_level, useNA = "always")
str(hum_table_household_data$oth_educ_level)
# recode some to missing
hum_table_household_data$oth_educ_level[hum_table_household_data$oth_educ_level == "."] = NA
hum_table_household_data$oth_educ_level[hum_table_household_data$oth_educ_level == ""] = NA
hum_table_household_data$oth_educ_level[hum_table_household_data$oth_educ_level == "N/a"] = NA
# check the recode
table(hum_table_household_data$oth_educ_level, useNA = "always")

# employment
table(hum_table_household_data$employment, useNA = "always")
str(hum_table_household_data$employment)
attr(hum_table_household_data$employment, "labels") 
# 1 is employed, 2 unemployed, 3 self employed, 4 skilled manual laborer, 5 unskilled manual laborer,
# 6 farmer, 7 not applicable, 8 retired, 9 other
# recode as a factor
hum_table_household_data$employment = factor(hum_table_household_data$employment,levels = c(1,2,3,4,5,6,7,8,9), labels = c("employed","unemployed","self employed","skilled manual laborer","unskilled manual laborer","farmer","not applicable","retired","other"))
table(hum_table_household_data$employment, useNA = "always")

# oth_emp
table(hum_table_household_data$oth_emp, useNA = "always")
str(hum_table_household_data$oth_emp)
# recode some to missing or student
hum_table_household_data$oth_emp[hum_table_household_data$oth_emp == "."] = NA
hum_table_household_data$oth_emp[hum_table_household_data$oth_emp == ""] = NA
hum_table_household_data$oth_emp[hum_table_household_data$oth_emp == "Pupil"] = "Student"
# check the recode
table(hum_table_household_data$oth_emp, useNA = "always")
# recode as factor
hum_table_household_data$oth_emp = as.factor(hum_table_household_data$oth_emp)

# sleep
table(hum_table_household_data$sleep, useNA = "always")
str(hum_table_household_data$sleep)
attr(hum_table_household_data$sleep, "labels") 
# 1 is yes, 2 no
# recode as a factor
hum_table_household_data$sleep = factor(hum_table_household_data$sleep,levels = c(1,2), labels = c("yes","no"))
table(hum_table_household_data$sleep, useNA = "always")

# know_rdt
table(hum_table_household_data$know_rdt, useNA = "always")
str(hum_table_household_data$know_rdt)
attr(hum_table_household_data$know_rdt, "labels") 
# 1 is yes, 2 no
# recode as a factor
hum_table_household_data$know_rdt = factor(hum_table_household_data$know_rdt,levels = c(1,2), labels = c("yes","no"))
table(hum_table_household_data$know_rdt, useNA = "always")

# mal_test
table(hum_table_household_data$mal_test, useNA = "always")
str(hum_table_household_data$mal_test)
attr(hum_table_household_data$mal_test, "labels") 
# 1 is yes, 2 no, 3 don't know/don't remember
# recode as a factor
hum_table_household_data$mal_test = factor(hum_table_household_data$mal_test,levels = c(1,2,3), labels = c("yes","no","don't know/don't remember"))
table(hum_table_household_data$mal_test, useNA = "always")

# rdt_results_n
table(hum_table_household_data$rdt_result_n, useNA = "always")
str(hum_table_household_data$rdt_result_n)
attr(hum_table_household_data$rdt_result_n, "labels") 
# 1 is very likely, 2 likely, 3 50-50, 4 unlikely, 5 very unlikely, 6 don't know, 7 no response
# recode as a factor
hum_table_household_data$rdt_result_n = factor(hum_table_household_data$rdt_result_n,levels = c(1,2,3,4,5,6,7), labels = c("very likely","likely","50-50","unlikely","very unlikely","don't know","no response"))
table(hum_table_household_data$rdt_result_n, useNA = "always")

# rdt_results_p
table(hum_table_household_data$rdt_result_p, useNA = "always")
str(hum_table_household_data$rdt_result_p)
attr(hum_table_household_data$rdt_result_p, "labels") 
# 1 is very likely, 2 likely, 3 50-50, 4 unlikely, 5 very unlikely, 6 don't know, 7 no response
# recode as a factor
hum_table_household_data$rdt_result_p = factor(hum_table_household_data$rdt_result_p,levels = c(1,2,3,4,5,6,7), labels = c("very likely","likely","50-50","unlikely","very unlikely","don't know","no response"))
table(hum_table_household_data$rdt_result_p, useNA = "always")

# malaria_likely
table(hum_table_household_data$malaria_likely, useNA = "always")
str(hum_table_household_data$malaria_likely)
# looks good, clean

# malaria_al
table(hum_table_household_data$malaria_al, useNA = "always")
str(hum_table_household_data$malaria_al)
attr(hum_table_household_data$malaria_al, "labels") 
# this variable coding from stata does not appear to match how it should be coded based on the questionnaire
# the levels do match, so recoding based off of questionnaire choices
# 1 is very likely, 2 likely, 3 50-50, 4 unlikely, 5 very unlikely, 6 don't know, 7 no response
# recode as a factor
hum_table_household_data$malaria_al = factor(hum_table_household_data$malaria_al,levels = c(1,2,3,4,5,6,7), labels = c("very likely","likely","50-50","unlikely","very unlikely","don't know","no response"))
table(hum_table_household_data$malaria_al, useNA = "always")

# parent_key
table(hum_table_household_data$parent_key, useNA = "always")
str(hum_table_household_data$parent_key)

# key
table(hum_table_household_data$key, useNA = "always")
str(hum_table_household_data$key)

# HH_ID
table(hum_table_household_data$HH_ID, useNA = "always")
str(hum_table_household_data$HH_ID)
# looks good, clean

# memID
table(hum_table_household_data$memID, useNA = "always")
str(hum_table_household_data$memID)
# looks good, clean

# unq_memID
table(hum_table_household_data$unq_memID, useNA = "always")
str(hum_table_household_data$unq_memID)
# looks good, clean



# ------------------ hum_ann_household_data

# today
summary(hum_ann_household_data$today2)
head(hum_ann_household_data$today2)
str(hum_ann_household_data$today2)
# originally not in date format but a character with MDY
# recoded in date format
newdate5 = ymd(hum_ann_household_data$today2)
head(newdate5)
head(hum_ann_household_data$today2)
summary(newdate5)
str(newdate5)
hum_ann_household_data$today2 = newdate5
summary(hum_ann_household_data$today2)

# roof
table(hum_ann_household_data$roof, useNA = "always")
str(hum_ann_household_data$roof)
attr(hum_ann_household_data$roof, "labels") 
# 1 is thatched, 2 mabati, 3 cement, 4 tiles, 5 no roof/open, 6 other
# recode as a factor
hum_ann_household_data$roof = factor(hum_ann_household_data$roof,levels = c(1,2,3,4,5,6), labels = c("thatched","mabati","cement","tiles","no roof/open","other"))
table(hum_ann_household_data$roof, useNA = "always")

# roof
table(hum_ann_household_data$floor, useNA = "always")
str(hum_ann_household_data$floor)
attr(hum_ann_household_data$floor, "labels") 
# 1 is mud/earth, cement, tiles, wooden planks, other
# recode as a factor
hum_ann_household_data$floor = factor(hum_ann_household_data$floor,levels = c(1,2,3,4,5), labels = c("mud/earth","cement","tiles","wooden planks","other"))
table(hum_ann_household_data$floor, useNA = "always")

# wall
table(hum_ann_household_data$wall, useNA = "always")
str(hum_ann_household_data$wall)
attr(hum_ann_household_data$wall, "labels") 
# 1 is mud/earth, 2 mabati, 3 cement/blocks, 4 bricks, 5 wooden planks, 6 stone, 7 other
# recode as a factor
hum_ann_household_data$wall = factor(hum_ann_household_data$wall,levels = c(1,2,3,4,5,6,7), labels = c("mud/earth","mabati","cement/blocks","bricks","wooden planks","stone","other"))
table(hum_ann_household_data$wall, useNA = "always")

# gap
table(hum_ann_household_data$gap, useNA = "always")
str(hum_ann_household_data$gap)
attr(hum_ann_household_data$gap, "labels") 
# 1 is yes, 2 no, 3 can't tell, 4 not applicable (ie no roof)
# recode as a factor
hum_ann_household_data$gap = factor(hum_ann_household_data$gap,levels = c(1,2,3,4), labels = c("yes","no","can't tell","not applicable (ie no roof)"))
table(hum_ann_household_data$gap, useNA = "always")

# gap_net
table(hum_ann_household_data$gap_net, useNA = "always")
str(hum_ann_household_data$gap_net)
attr(hum_ann_household_data$gap_net, "labels") 
# 1 is all, 2 some, 3 none, 4 not applicable because no windows
# recode as a factor
hum_ann_household_data$gap_net = factor(hum_ann_household_data$gap_net,levels = c(1,2,3,4), labels = c("all","some","none","not applicable because no windows"))
table(hum_ann_household_data$gap_net, useNA = "always")

# sleep_s
table(hum_ann_household_data$sleeping_s, useNA = "always")
str(hum_ann_household_data$sleeping_s)
attr(hum_ann_household_data$sleeping_s, "labels") 
# 1 is yes, 2 no
# recode as a factor
hum_ann_household_data$sleeping_s = factor(hum_ann_household_data$sleeping_s,levels = c(1,2), labels = c("yes","no"))
table(hum_ann_household_data$sleeping_s, useNA = "always")

# parent_key
table(hum_ann_household_data$parent_key, useNA = "always")
str(hum_ann_household_data$parent_key)

# HH_ID
table(hum_ann_household_data$HH_ID, useNA = "always")
str(hum_ann_household_data$HH_ID)
# looks good, clean

# today
table(hum_ann_household_data$today, useNA = "always")
str(hum_ann_household_data$today)
# compare this with today2
head(hum_ann_household_data$today)
head(hum_ann_household_data$today2)
summary(hum_ann_household_data$today)
summary(hum_ann_household_data$today2)
# remove today, because same as today2 that is already correctly formatted
hum_ann_household_data$today <- NULL

# village_name
table(hum_ann_household_data$village_name, useNA = "always")
str(hum_ann_household_data$village_name)
attr(hum_ann_household_data$village_name, "labels") 
# 1 is Kinesamo, 2 Maruti, 3 Sitabicha
# recode as a factor
hum_ann_household_data$village_name = factor(hum_ann_household_data$village_name,levels = c(1,3,5), labels = c("Kinesamo","Maruti","Sitabicha"))
table(hum_ann_household_data$village_name, useNA = "always")

# hh_buildings
table(hum_ann_household_data$hh_buildings, useNA = "always")
str(hum_ann_household_data$hh_buildings)
# looks coded correctly, clean

# hh_buildings_sp
table(hum_ann_household_data$hh_building_sp, useNA = "always")
str(hum_ann_household_data$hh_building_sp)
# looks coded correctly, clean

# dinner_time
table(hum_ann_household_data$dinner_time, useNA = "always")
str(hum_ann_household_data$dinner_time)
# change the AM values to PM values
hum_ann_household_data$dinner_time[hum_ann_household_data$dinner_time == "7:00:00 AM"] = "7:00:00 PM"
hum_ann_household_data$dinner_time[hum_ann_household_data$dinner_time == "7:12:00 AM"] = "7:12:00 PM"
# check the recode
table(hum_ann_household_data$dinner_time, useNA = "always")

# travel_m
table(hum_ann_household_data$travel_m, useNA = "always")
str(hum_ann_household_data$travel_m)
# looks good, clean

# travel_how
table(hum_ann_household_data$travel_how, useNA = "always")
str(hum_ann_household_data$travel_how)
attr(hum_ann_household_data$travel_how, "labels") 
# 1 is on foot, 2 bicycle, 3 motorbike, 4 matatu, 5 private vehicle, 6 donkey, 7 other
# recode as a factor
hum_ann_household_data$travel_how = factor(hum_ann_household_data$travel_how,levels = c(1,2,3,4,5,6,7), labels = c("on foot","bicycle","motorbike","matatu","private vehicle","donkey","other"))
table(hum_ann_household_data$travel_how, useNA = "always")

# travel_other
table(hum_ann_household_data$travel_other, useNA = "always")
str(hum_ann_household_data$travel_other)
# all observations missing, remove from data set
hum_ann_household_data$travel_other <- NULL

# facility_kind
table(hum_ann_household_data$facility_kind, useNA = "always")
str(hum_ann_household_data$facility_kind)
attr(hum_ann_household_data$facility_kind, "labels") 
# 1 is hospital, 2 dispensary, 3 health centre
# recode as a factor
hum_ann_household_data$facility_kind = factor(hum_ann_household_data$facility_kind,levels = c(1,2,3), labels = c("hospital","dispensary","health centre"))
table(hum_ann_household_data$facility_kind, useNA = "always")

# water_source
table(hum_ann_household_data$water_source, useNA = "always")
str(hum_ann_household_data$water_source)
attr(hum_ann_household_data$water_source, "labels") 
# 1 is piped water/public tap/borehole, 3 unprotected well, 3 protected well, 4 protected spring, 5 unprotected spring, 6 rain water, 7 river water, 8 other
# recode as a factor
hum_ann_household_data$water_source = factor(hum_ann_household_data$water_source,levels = c(1,2,3,4,5,6,7,8), labels = c("piped water/public tap/borehole","unprotected well","protected well","protected spring","unprotected spring","rain water","river water","other"))
table(hum_ann_household_data$water_source, useNA = "always")

# water_source_other
table(hum_ann_household_data$water_source_other, useNA = "always")
str(hum_ann_household_data$water_source_other)
# all values missing, remove from data set
hum_ann_household_data$water_source_other <- NULL

# household_items
table(hum_ann_household_data$household_items, useNA = "always")
str(hum_ann_household_data$household_items)
# Character w/ multiple responses (broken down into separate variables later in dataset)

# no_of_cows
table(hum_ann_household_data$no_of_cows, useNA = "always")
str(hum_ann_household_data$no_of_cows)
# looks good, clean

# no_of_sheep
table(hum_ann_household_data$no_of_sheep, useNA = "always")
str(hum_ann_household_data$no_of_sheep)
# looks good, clean

# no_of_goats
table(hum_ann_household_data$no_of_goats, useNA = "always")
str(hum_ann_household_data$no_of_goats)
# looks good, clean

# no_of_pigs
table(hum_ann_household_data$no_of_pigs, useNA = "always")
str(hum_ann_household_data$no_of_pigs)
# looks good, clean

# toilet_kd
table(hum_ann_household_data$toilet_kd, useNA = "always")
str(hum_ann_household_data$toilet_kd)
attr(hum_ann_household_data$toilet_kd, "labels") 
# 1 is flush or pour flush toilet, 2 VIP/ventilated improved pit, 3 pit latrine with slab, 4 pit latrine without slab, 5 composing toilet, 6 bucket toilet, 7 no facility/bush/field, 8 other
# recode as a factor
hum_ann_household_data$toilet_kd = factor(hum_ann_household_data$toilet_kd,levels = c(1,2,3,4,5,6,7,8), labels = c("flush or pour flush toilet","VIP/ventilated improved pit","pit latrine with slab","pit latrine without slab","composing toilet","bucket toilet","no facility/bush/field","other"))
table(hum_ann_household_data$toilet_kd, useNA = "always")

# toilet_oth
table(hum_ann_household_data$toilet_oth, useNA = "always")
str(hum_ann_household_data$toilet_oth)
# all observations missing, remove from data set
hum_ann_household_data$toilet_oth <- NULL

# fuel_hh
table(hum_ann_household_data$fuel_hh, useNA = "always")
str(hum_ann_household_data$fuel_hh)
attr(hum_ann_household_data$fuel_hh, "labels") 
# 1 is liquefied petroleum gas, 2 paraffin/kerosene, 3 charcoal, 4 firewood, 5 dung, 6 biogas, 7 crop residue, 8 other
# recode as a factor
hum_ann_household_data$fuel_hh = factor(hum_ann_household_data$fuel_hh,levels = c(1,2,3,4,5,6,7,8), labels = c("liquefied petroleum gas","paraffin/kerosene","charcoal","firewood","dung","biogas","crop residue","other"))
table(hum_ann_household_data$fuel_hh, useNA = "always")

# fuel_oth
table(hum_ann_household_data$fuel_oth, useNA = "always")
str(hum_ann_household_data$fuel_oth)
# all observations missing, remove from data set
hum_ann_household_data$fuel_oth <- NULL

# own_land
table(hum_ann_household_data$own_land, useNA = "always")
str(hum_ann_household_data$own_land)
attr(hum_ann_household_data$own_land, "labels") 
# 1 is yes, 2 no
# recode as a factor
hum_ann_household_data$own_land = factor(hum_ann_household_data$own_land,levels = c(1,2), labels = c("yes","no"))
table(hum_ann_household_data$own_land, useNA = "always")

# land_size
table(hum_ann_household_data$land_size, useNA = "always")
str(hum_ann_household_data$land_size)
attr(hum_ann_household_data$land_size, "labels") 
# 1 is yes, 2 no
# recode as a factor
hum_ann_household_data$land_size = factor(hum_ann_household_data$land_size,levels = c(1,2,3), labels = c("acres","hectares","square feet (xx by xx)"))
table(hum_ann_household_data$land_size, useNA = "always")

# land_acres
table(hum_ann_household_data$land_acres, useNA = "always")
str(hum_ann_household_data$land_acres)
# looks good, clean
# note assuming: 99 = don't know

# land_hectares
table(hum_ann_household_data$land_hectares, useNA = "always")
str(hum_ann_household_data$land_hectares)
# all observations missing, remove from data set
hum_ann_household_data$land_hectares <- NULL

# land_size_sqft
table(hum_ann_household_data$land_size_sqft, useNA = "always")
str(hum_ann_household_data$land_size_sqft)
# all observations missing, remove from data set
hum_ann_household_data$land_size_sqft <- NULL

# Electricity
table(hum_ann_household_data$Electricity, useNA = "always")
str(hum_ann_household_data$Electricity)
attr(hum_ann_household_data$Electricity, "labels") 
# 0 no, 1 yes
# recode as a factor
hum_ann_household_data$Electricity = factor(hum_ann_household_data$Electricity,levels = c(1,0), labels = c("yes","no"))
table(hum_ann_household_data$Electricity, useNA = "always")

# Television
table(hum_ann_household_data$Television, useNA = "always")
str(hum_ann_household_data$Television)
attr(hum_ann_household_data$Television, "labels") 
# 0 no, 1 yes
# recode as a factor
hum_ann_household_data$Television = factor(hum_ann_household_data$Television,levels = c(1,0), labels = c("yes","no"))
table(hum_ann_household_data$Television, useNA = "always")

# Refrigerator
table(hum_ann_household_data$Refrigerator, useNA = "always")
str(hum_ann_household_data$Refrigerator)
attr(hum_ann_household_data$Refrigerator, "labels") 
# 0 no, 1 yes
# recode as a factor
hum_ann_household_data$Refrigerator = factor(hum_ann_household_data$Refrigerator,levels = c(1,0), labels = c("yes","no"))
table(hum_ann_household_data$Refrigerator, useNA = "always")

# Radio
table(hum_ann_household_data$Radio, useNA = "always")
str(hum_ann_household_data$Radio)
attr(hum_ann_household_data$Radio, "labels") 
# 0 no, 1 yes
# recode as a factor
hum_ann_household_data$Radio = factor(hum_ann_household_data$Radio,levels = c(1,0), labels = c("yes","no"))
table(hum_ann_household_data$Radio, useNA = "always")

# Mobile_phone
table(hum_ann_household_data$Mobile_phone, useNA = "always")
str(hum_ann_household_data$Mobile_phone)
attr(hum_ann_household_data$Mobile_phone, "labels") 
# 0 no, 1 yes
# recode as a factor
hum_ann_household_data$Mobile_phone = factor(hum_ann_household_data$Mobile_phone,levels = c(1,0), labels = c("yes","no"))
table(hum_ann_household_data$Mobile_phone, useNA = "always")

# Motorcycle
table(hum_ann_household_data$Motorcycle, useNA = "always")
str(hum_ann_household_data$Motorcycle)
attr(hum_ann_household_data$Motorcycle, "labels") 
# 0 no, 1 yes
# recode as a factor
hum_ann_household_data$Motorcycle = factor(hum_ann_household_data$Motorcycle,levels = c(1,0), labels = c("yes","no"))
table(hum_ann_household_data$Motorcycle, useNA = "always")

# car_truck
table(hum_ann_household_data$car_truck, useNA = "always")
str(hum_ann_household_data$car_truck)
attr(hum_ann_household_data$car_truck, "labels") 
# 0 no, 1 yes
# recode as a factor
hum_ann_household_data$car_truck = factor(hum_ann_household_data$car_truck,levels = c(1,0), labels = c("yes","no"))
table(hum_ann_household_data$car_truck, useNA = "always")

# Bank_account
table(hum_ann_household_data$Bank_account, useNA = "always")
str(hum_ann_household_data$Bank_account)
attr(hum_ann_household_data$Bank_account, "labels") 
# 0 no, 1 yes
# recode as a factor
hum_ann_household_data$Bank_account = factor(hum_ann_household_data$Bank_account,levels = c(1,0), labels = c("yes","no"))
table(hum_ann_household_data$Bank_account, useNA = "always")

# Bank_account
table(hum_ann_household_data$None, useNA = "always")
str(hum_ann_household_data$None)
attr(hum_ann_household_data$None, "labels") 
# 0 no, 1 yes
# recode as a factor
hum_ann_household_data$None = factor(hum_ann_household_data$None,levels = c(1,0), labels = c("yes","no"))
table(hum_ann_household_data$None, useNA = "always")


# ------------- hum_sick_data

# today
table(hum_sick_data$today, useNA = "always")
str(hum_sick_data$today)
summary(hum_sick_data$today)
head(hum_sick_data$today)
# originally not in date format but a character with MDY
# recoded in date format
newdate_sick = mdy(hum_sick_data$today)
head(newdate_sick)
head(hum_sick_data$today)
summary(newdate_sick)
str(newdate_sick)
hum_sick_data$today = newdate_sick
summary(hum_sick_data$today)

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

# date_today
table(hum_sick_data$date_today, useNA = "always")
str(hum_sick_data$date_today)
# remove, because already have date variable formatted
hum_sick_data$date_today <- NULL

# PID
table(hum_sick_data$PID, useNA = "always")
str(hum_sick_data$PID)
# change variable name from PID to unq_memID
names(hum_sick_data)[names(hum_sick_data) == "PID"] <- "unq_memID"
# check the rename
names(hum_sick_data)


#### -------- make variables that are not unique have unique names ------------ ####

# floor
names(hum_ann_household_data)[names(hum_ann_household_data) == "floor"] <- "floor_hum_ann_household_data"
names(hum_sleeping_data)[names(hum_sleeping_data) == "floor"] <- "floor_hum_sleeping_data"

# gap
names(hum_ann_household_data)[names(hum_ann_household_data) == "gap"] <- "gap_hum_ann_household_data"
names(hum_sleeping_data)[names(hum_sleeping_data) == "gap"] <- "gap_hum_sleeping_data"

# gap_net
names(hum_ann_household_data)[names(hum_ann_household_data) == "gap_net"] <- "gap_net_hum_ann_household_data"
names(hum_sleeping_data)[names(hum_sleeping_data) == "gap_net"] <- "gap_net_hum_sleeping_data"

# gender
names(hum_monthly_data)[names(hum_monthly_data) == "gender"] <- "gender_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "gender"] <- "gender_hum_sick_data"

# key
names(hum_monthly_data)[names(hum_monthly_data) == "key"] <- "key_hum_monthly_data"
names(hum_table_household_data)[names(hum_table_household_data) == "key"] <- "key_hum_table_household_data"
names(hum_sick_data)[names(hum_sick_data) == "key"] <- "key_hum_sick_data"

# mal_likely
names(hum_monthly_data)[names(hum_monthly_data) == "mal_likely"] <- "mal_likely_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "mal_likely"] <- "mal_likely_hum_sick_data"

# malaria_al
names(hum_monthly_data)[names(hum_monthly_data) == "malaria_al"] <- "malaria_al_hum_monthly_data"
names(hum_sick_data)[names(hum_sick_data) == "malaria_al"] <- "malaria_al_hum_sick_data"
names(hum_table_household_data)[names(hum_table_household_data) == "malaria_al"] <- "malaria_al_hum_table_household_data"

# malaria_likely
names(hum_monthly_data)[names(hum_monthly_data) == "malaria_likely"] <- "malaria_likely_hum_monthly_data"
names(hum_table_household_data)[names(hum_table_household_data) == "malaria_likely"] <- "malaria_likely_hum_table_household_data"

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

# parent_key
names(hum_ann_household_data)[names(hum_ann_household_data) == "parent_key"] <- "parent_key_hum_ann_household_data"
names(hum_sleeping_data)[names(hum_sleeping_data) == "parent_key"] <- "parent_key_hum_sleeping_data"
names(hum_table_household_data)[names(hum_table_household_data) == "parent_key"] <- "parent_key_hum_table_household_data"

# roof
names(hum_ann_household_data)[names(hum_ann_household_data) == "roof"] <- "roof_hum_ann_household_data"
names(hum_sleeping_data)[names(hum_sleeping_data) == "roof"] <- "roof_hum_sleeping_data"

# sleeping_s
names(hum_ann_household_data)[names(hum_ann_household_data) == "sleeping_s"] <- "sleeping_s_hum_ann_household_data"
names(hum_sleeping_data)[names(hum_sleeping_data) == "sleeping_s"] <- "sleeping_s_hum_sleeping_data"

# today
names(hum_ann_household_data)[names(hum_ann_household_data) == "today2"] <- "today_hum_ann_household_data"
names(hum_sleeping_data)[names(hum_sleeping_data) == "today"] <- "today_hum_sleeping_data"
names(hum_monthly_data)[names(hum_monthly_data) == "today"] <- "today_hum_monthly_data"
names(hum_table_household_data)[names(hum_table_household_data) == "today"] <- "today_hum_table_household_data"
names(hum_sick_data)[names(hum_sick_data) == "today"] <- "today_hum_sick_data"

# village_name
names(hum_ann_household_data)[names(hum_ann_household_data) == "village_name"] <- "village_name_hum_ann_household_data"
names(hum_sick_data)[names(hum_sick_data) == "village_name"] <- "village_name_hum_sick_data"
names(hum_monthly_data)[names(hum_monthly_data) == "village_name"] <- "village_name_hum_monthly_data"

# wall
names(hum_ann_household_data)[names(hum_ann_household_data) == "wall"] <- "wall_hum_ann_household_data"
names(hum_sleeping_data)[names(hum_sleeping_data) == "wall"] <- "wall_hum_sleeping_data"


#### --------- export each separate data set as a CSV or RDS file --------- ####

# export each separate data file as a RDS and CSV
# for hum_ann_household_data
write_csv(hum_ann_household_data, "hum_ann_household_data_19DEC2018.csv")
write_rds(hum_ann_household_data, "hum_ann_household_data_19DEC2018.RDS")
# for hum_sleeping_data
write_csv(hum_sleeping_data, "hum_sleeping_data_19DEC2018.csv")
write_rds(hum_sleeping_data, "hum_sleeping_data_19DEC2018.RDS")
# for hum_monthly_data
write_csv(hum_monthly_data, "hum_monthly_data_19DEC2018.csv")
write_rds(hum_monthly_data, "hum_monthly_data_19DEC2018.RDS")
# for hum_table_household_data
write_csv(hum_table_household_data, "hum_table_household_data_19DEC2018.csv")
write_rds(hum_table_household_data, "hum_table_household_data_19DEC2018.RDS")
# for hum_sleeping_data
write_csv(hum_sick_data, "hum_sick_data_19DEC2018.csv")
write_rds(hum_sick_data, "hum_sick_data_19DEC2018.RDS")















