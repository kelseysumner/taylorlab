# ------------------------------------ #
#       MESA Clean Meta Data Set       #
#         September 27, 2018           #
#             K. Sumner                #
# ------------------------------------ #

#### ----- load the necessary libraries ----- ####
library(tidyverse)


#### ----- read in all data sets ----- ####
# read in the taylor lab updated inventory (.dta stata format)
lab_inventory = taylor_lab_Oct6
# read in the updated MESA meta data set with the new lab inventory amounts 
mesa_data = Full_dataset_all_members_v12_correctedJune2018_matchedlab_deid
# read in the data set of the original mesa data set variables (just the variable names)
mesa_vars = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/mesa_meta_data_variables.csv")


#### --------------- CLEAN META DATA SET ------------------- ####

# look at the mesa_data summary
dim(mesa_data)
colnames(mesa_data)
summary(mesa_data)

# subset the mesa data set to the variable of interest only
# pull out the variables in the mesa_vars data set with a "Y" in the second column
mesa_vars_first = mesa_vars[which(mesa_vars$`Include Variable In Analyses`=="Y"),]
mesa_vars_onecol = mesa_vars_first[["Variable"]]
# add the new variable names to the mesa_vars data set (from the latest MESA data set)
mesa_var_list = append(mesa_vars_onecol,c("interview_date","alternate_idmatchcard","mem_rdt_idmatchcard","found","casechild",
                  "studyid_case_controldata","case_control_childdata","mem_rdt_idindata","labid_old","labid","labid_original",
                  "gdnaplate","gdnacolumn","gdnarow","dbsbox","dbsbag","net_used_lastnight"))
# remove RDT variables not in latest MESA data set
rdt_vars = c("total_household_RDTs","total_obs_in_RDTfile","total_positive_RDTs","total_negative_RDTs",
             "positive_asymptomatic","positive_symptomatic","total_pos_asymptomatic","total_pos_symptomatic")
mesa_var_list = mesa_var_list[! mesa_var_list %in% rdt_vars]
# subset the data set to just have the variables of interest for the analyses
mesa_data = mesa_data[,mesa_var_list]

# look at a new summary of the data set
summary(mesa_data)

#### starting cleaning the mesa_data
## studyid_case_control
mesa_data$studyid_case_control = as.character(mesa_data$studyid_case_control)
summary(mesa_data$studyid_case_control)
mesa_data$studyid_case_control[mesa_data$studyid_case_control == ""] = NA
table(mesa_data$studyid_case_control, useNA = "always")
mesa_data$studyid_case_control = as.factor(mesa_data$studyid_case_control)

## person
table(mesa_data$person,useNA = "always")
str(mesa_data$person)

## key_household
# remove this variable
mesa_data$key_household <- NULL

## location
table(mesa_data$location,useNA = "always")
# clean up the names of the variables (chose spelling of each location that occurred most often in data set)
mesa_data$location[mesa_data$location == "Bikoli"] = "Bokoli"
mesa_data$location[mesa_data$location == "Chetembe"] = "Chetambe"
mesa_data$location[mesa_data$location == "maraka"] = "Maraka"
mesa_data$location[mesa_data$location == "matulo"] = "Matulo"
mesa_data$location[mesa_data$location == "miendo"] = "Miendo"
mesa_data$location[mesa_data$location == "mihuu"] = "Mihuu"
mesa_data$location[mesa_data$location == "Mihu"] = "Mihuu"
mesa_data$location[mesa_data$location == "misikhu"] = "Misikhu"
mesa_data$location[mesa_data$location == "Misikkhu"] = "Misikhu"
mesa_data$location[mesa_data$location == "Misukhu"] = "Misikhu"
mesa_data$location[mesa_data$location == "muchi"] = "Muchi"
mesa_data$location[mesa_data$location == "ndivisi"] = "Ndivisi"
mesa_data$location[mesa_data$location == "sirende"] = "Sirende"
mesa_data$location[mesa_data$location == "Sitikhi"] = "Sitikho"
mesa_data$location[mesa_data$location == "sitikho"] = "Sitikho"
mesa_data$location[mesa_data$location == "Sotikho"] = "Sitikho"
mesa_data$location[mesa_data$location == "township"] = "Township"
mesa_data$location[mesa_data$location == "We buye"] = "Webuye"
mesa_data$location[mesa_data$location == "webuye"] = "Webuye"
mesa_data$location[mesa_data$location == "WEBUYE"] = "Webuye"
mesa_data$location[mesa_data$location == ""] = NA
# check renaming
table(mesa_data$location,useNA = "always")
str(mesa_data$location)
mesa_data$location = as.factor(mesa_data$location)

## sublocation
mesa_data$sublocation = as.factor(mesa_data$sublocation)
table(mesa_data$sublocation, useNA = "always")
# decided to delete this variable
mesa_data$sublocation <- NULL

## village
mesa_data$village = as.factor(mesa_data$village)
table(mesa_data$village,useNA = "always")
levels(mesa_data$village) # 332 levels (decide to not recode at moment - will if we end up using it)

## hse within 05 km (houses within 0.5 km)
table(mesa_data$hse_within_05km, useNA = "always")
str(mesa_data$hse_within_05km)

## case_control_child (individual-level - whether case/control child)
table(mesa_data$case_control_child, useNA = "always")
# 1 if a case/control child, 0 if not
str(mesa_data$case_control_child)
# make a factor
mesa_data$case_control_child = factor(mesa_data$case_control_child,levels = 0:1, labels = c("no", "yes"))

## non_case_control_child (individual-level - family member of case/control child)
table(mesa_data$non_case_control_child, useNA = "always")
# 1 if family member of case/control child, 0 if not
str(mesa_data$non_case_control_child)
# make a factor
mesa_data$non_case_control_child = factor(mesa_data$non_case_control_child,levels = 0:1, labels = c("no", "yes"))

## case_household
table(mesa_data$case_household, useNA = "always")
# 1 is case household, 0 if not
str(mesa_data$case_household)
# make a factor
mesa_data$case_household = factor(mesa_data$case_household,levels = 0:1, labels = c("no", "yes"))

## control_household
table(mesa_data$control_household, useNA = "always")
# 1 is control household, 0 if not
str(mesa_data$control_household)
# make a factor
mesa_data$control_household= factor(mesa_data$control_household,levels = 0:1, labels = c("no", "yes"))

## survey_day
# going to remove and just use interview day variable
mesa_data$survey_day <- NULL

## case_child
table(mesa_data$case_child, useNA = "always")
# 1 is case child, 0 if not
str(mesa_data$case_child)
# make a factor
mesa_data$case_child = factor(mesa_data$case_child,levels = 0:1, labels = c("no", "yes"))

## control_child
table(mesa_data$control_child, useNA = "always")
# 1 is control child, 0 if not
str(mesa_data$control_child)
# make a factor
mesa_data$control_child = factor(mesa_data$control_child,levels = 0:1, labels = c("no", "yes"))

## age_case_child
summary(mesa_data$age_case_child)
str(mesa_data$age_case_child)

## case_child_educ_lvl
mesa_data$case_child_educ_lvl = as.factor(mesa_data$case_child_educ_lvl)
table(mesa_data$case_child_educ_lvl, useNA = "always")
# don't think this variable will be useful so remove it
mesa_data$case_child_educ_lvl <- NULL

## case_child_emplmnt
# remove this from the data set
mesa_data$case_child_emplmnt <- NULL

## case_child_rlshp_hhhead
# remove this from the data set
mesa_data$case_child_rlshp_hhhead <- NULL

## case_child_sleepout_bgm
# remove this from the data set
mesa_data$case_child_sleepout_bgm <- NULL

## days_case_child_sleepout_bgm
# remove this from the data set
mesa_data$days_case_child_sleepout_bgm <- NULL

## where_case_child_sleepout_bgm
# remove this from the data set
mesa_data$where_case_child_sleepout_bgm <- NULL

## any_other_mem_case
table(mesa_data$any_other_mem_case, useNA = "always")
# is a 1 if there's another member of the household that had malaria, 0 otherwise
# recode the 25 "" to NA
mesa_data$any_other_mem_case[mesa_data$any_other_mem_case == ""] = NA
table(mesa_data$any_other_mem_case, useNA = "always")
mesa_data$any_other_mem_case = as.numeric(mesa_data$any_other_mem_case)
str(mesa_data$any_other_mem_case)
# make a factor
mesa_data$any_other_mem_case = factor(mesa_data$any_other_mem_case,levels = 0:1, labels = c("no", "yes"))

## gps_coordinates_Latitude
table(mesa_data$gps_coordinates_Latitude, useNA = "always")
mesa_data$gps_coordinates_Latitude[mesa_data$gps_coordinates_Latitude == ""] = NA
table(mesa_data$gps_coordinates_Latitude, useNA = "always")
str(mesa_data$gps_coordinates_Latitude)

## gps_coordinates_Longitude
table(mesa_data$gps_coordinates_Longitude, useNA = "always")
mesa_data$gps_coordinates_Longitude[mesa_data$gps_coordinates_Longitude == ""] = NA
table(mesa_data$gps_coordinates_Longitude, useNA = "always")
str(mesa_data$gps_coordinates_Longitude)

## gps_coordinates_Accuracy
table(mesa_data$gps_coordinates_Accuracy, useNA = "always")
mesa_data$gps_coordinates_Accuracy[mesa_data$gps_coordinates_Accuracy == ""] = NA
table(mesa_data$gps_coordinates_Accuracy, useNA = "always")
str(mesa_data$gps_coordinates_Accuracy)
mesa_data$gps_coordinates_Accuracy = as.numeric(mesa_data$gps_coordinates_Accuracy)

## gps_coordinates_Altitude
table(mesa_data$gps_coordinates_Altitude, useNA = "always")
mesa_data$gps_coordinates_Altitude[mesa_data$gps_coordinates_Altitude == ""] = NA
table(mesa_data$gps_coordinates_Altitude, useNA = "always")
str(mesa_data$gps_coordinates_Altitude)
mesa_data$gps_coordinates_Altitude = as.numeric(mesa_data$gps_coordinates_Altitude)

## mem_age
table(mesa_data$mem_age, useNA = "always")
str(mesa_data$mem_age)
# recoded 525, 999, 9999 to NA because impossible ages
# assuming that ages reported in years
mesa_data$mem_age[mesa_data$mem_age == 525] = NA
mesa_data$mem_age[mesa_data$mem_age == 999] = NA
mesa_data$mem_age[mesa_data$mem_age == 9999] = NA
table(mesa_data$mem_age, useNA = "always")
summary(mesa_data$mem_age)

## mem_edc_lvl 
# decided to remove the variable
mesa_data$mem_edc_lvl <- NULL

## mem_emp
# decided to remove the variable
mesa_data$mem_emp <- NULL

## mem_sleepout_bgm
# decided to remove the variable
mesa_data$mem_sleepout_bgm <- NULL

## days_mem_sleepout_bgm
# decided to remove the variable
mesa_data$days_mem_sleepout_bgm <- NULL

## where_mem_sleepout_bgm
# decided to remove the variable
mesa_data$where_mem_sleepout_bgm <- NULL

## mem_rshp_hhhead
# decided to remove the variable
mesa_data$mem_rshp_hhhead <- NULL

## any_other_mem
summary(as.factor(mesa_data$any_other_mem))
# not sure what this variable means so removing it
mesa_data$any_other_mem <- NULL

## total_in_hh_roster
table(mesa_data$total_in_hh_roster)
str(mesa_data$total_in_hh_roster)

## setofmembers_rdt_details
# remove this variable
mesa_data$setofmembers_rdt_details <- NULL

## mem_consent_rdt
table(mesa_data$mem_consent_rdt,useNA = "always")
mesa_data$mem_consent_rdt[mesa_data$mem_consent_rdt == ""] = NA
str(mesa_data$mem_consent_rdt)
mesa_data$mem_consent_rdt = as.factor(mesa_data$mem_consent_rdt)
# note: probably won't use this variable

## mem_rdt_results
table(mesa_data$mem_rdt_results, useNA = "always")
mesa_data$mem_rdt_results[mesa_data$mem_rdt_results == ""] = NA
str(mesa_data$mem_rdt_results)
mesa_data$mem_rdt_results = as.factor(mesa_data$mem_rdt_results)

## mem_symptoms_today
table(mesa_data$mem_symptoms_today, useNA = "always")
summary(as.factor(mesa_data$mem_symptoms_today))
mesa_data$mem_symptoms_today[mesa_data$mem_symptoms_today == ""] = NA
# make a list of all those without malaria symptoms
fever_list = list("high fever","fever","high temperature","headache,high fever","fever,headache","high fever,headache","abdominal pain with high fever","coughs n fever","dizziness , weak n fever-wants sun bath","dullness, inactive & high temperature","fever & cough","fever & dullness, not active.","fever & headache","fever & not active","fever & stomachache","fever and headache","fever and running nose","fever headache","fever last night","fever loose of energy","fever, headache","fever, headache n rashy mouth","fever, neusea","fever,general body weakness","general body weakness, high temperature")
# create a new variable whether the person had a fever or not that day
mesa_data$mem_has_fever = ifelse(is.na(mesa_data$mem_symptoms_today)==T,NA,ifelse(mesa_data$mem_symptoms_today %in% fever_list,1,0)) # 1 if had fever, 0 if don't
table(mesa_data$mem_symptoms_today,mesa_data$mem_has_fever, useNA = "always")
table(mesa_data$mem_has_fever, useNA = "always")
str(mesa_data$mem_has_fever)
# make a factor
mesa_data$mem_has_fever = factor(mesa_data$mem_has_fever,levels = 0:1, labels = c("no", "yes"))

## mem_taking_al_hx
table(mesa_data$mem_taking_al_hx, useNA = "always")
mesa_data$mem_taking_al_hx[mesa_data$mem_taking_al_hx == ""] = NA
mesa_data$mem_taking_al_hx[mesa_data$mem_taking_al_hx == "no"] = 0
mesa_data$mem_taking_al_hx[mesa_data$mem_taking_al_hx == "yes"] = 1
# coded it as a 0 for no and 1 for yes
str(mesa_data$mem_taking_al_hx)
mesa_data$mem_taking_al_hx = as.numeric(mesa_data$mem_taking_al_hx)
# make a factor
mesa_data$mem_taking_al_hx = factor(mesa_data$mem_taking_al_hx,levels = 0:1, labels = c("no", "yes"))

## mem_referred_fac_bld
table(mesa_data$mem_referred_fac_bld, useNA = "always")
mesa_data$mem_referred_fac_bld[mesa_data$mem_referred_fac_bld == ""] = NA
mesa_data$mem_referred_fac_bld = as.factor(mesa_data$mem_referred_fac_bld)
# didn't clean because probably won't use and many levels but keeping in dataset for now

## mem_facility
table(mesa_data$mem_facility, useNA = "always")
# decided to exclude from data set
mesa_data$mem_facility <- NULL

## any_other_mem_rdt
table(mesa_data$any_other_mem_rdt, useNA = "always")
mesa_data$any_other_mem_rdt[mesa_data$any_other_mem_rdt == ""] = NA
mesa_data$any_other_mem_rdt = as.factor(mesa_data$any_other_mem_rdt)
str(mesa_data$any_other_mem_rdt)

## rdt_key
# removed from data set
mesa_data$rdt_key <- NULL

## merge_hsehld_rdt
table(mesa_data$merge_hsehld_rdt, useNA="always")
# remove from data set - was just a variable used to merge data sets
mesa_data$merge_hsehld_rdt <- NULL

## mems_absent_rdt
table(mesa_data$mems_absent_rdt, useNA = "always")
# remove from data set
mesa_data$mems_absent_rdt <- NULL

## memsabstrdt8
# remove from data set
mesa_data$memsabstrdt8 <- NULL

## memsabstrdt9
# remove from data set
mesa_data$memsabstrdt9 <- NULL

## mem_rdt_no
# remove from data set
mesa_data$mem_rdt_no <- NULL

## mem_rdt_id
summary(as.factor(mesa_data$mem_rdt_id))
mesa_data$mem_rdt_id[mesa_data$mem_rdt_id == ""] = NA
str(mesa_data$mem_rdt_id)

## mem_consent_rdt_NA
summary(as.factor(mesa_data$mem_consent_rdt_NA))
# this is those who were not applicable for the rdt consent
str(mesa_data$mem_consent_rdt_NA)

## RDT_missing
# no RDT or no result
table(mesa_data$RDT_missing, useNA = "always")
str(mesa_data$RDT_missing)
# make a factor
mesa_data$RDT_missing = factor(mesa_data$RDT_missing,levels = 0:1, labels = c("no", "yes"))

## RDT_negative
# negative rdt test
table(mesa_data$RDT_negative, useNA = "always")
str(mesa_data$RDT_negative)
# make a factor
mesa_data$RDT_negative = factor(mesa_data$RDT_negative,levels = 0:1, labels = c("no", "yes"))

## RDT_positive
# positive rdt test
table(mesa_data$RDT_positive, useNA = "always")
str(mesa_data$RDT_positive)
# make a factor
mesa_data$RDT_positive = factor(mesa_data$RDT_positive,levels = 0:1, labels = c("no", "yes"))

## No_symptoms
table(mesa_data$No_symptoms, useNA = "always")
str(mesa_data$No_symptoms)
# removing this variable because we have a better variable made for symptoms (if have fever or not)
mesa_data$No_symptoms <- NULL

## dry_season
# if the sample is collected during the dry season or not
table(mesa_data$dry_season, useNA = "always")
str(mesa_data$dry_season)
# make a factor
mesa_data$dry_season = factor(mesa_data$dry_season,levels = 0:1, labels = c("no", "yes"))

## total_households_500m
# households within 500 m and 6 months of case/control
table(mesa_data$total_households_500m, useNA = "always")
str(mesa_data$total_households_500m)

## total_people_500m
# people within 500 m and 6 months of case/control
table(mesa_data$total_people_500m, useNA = "always")
str(mesa_data$total_people_500m)

## total_under_net_500m
# total number of people sleeping under nets within 500m
table(mesa_data$total_under_net_500m, useNA = "always")
str(mesa_data$total_under_net_500m)

## total_not_under_net_500m
# total number of people NOT sleeping under nets within 500m
table(mesa_data$total_not_under_net_500m, useNA = "always")
str(mesa_data$total_not_under_net_500m)

## pct_under_net_500m
# percent of all neighors sleeping under nets within 500m
table(mesa_data$pct_under_net_500m, useNA = "always")
summary(mesa_data$pct_under_net_500m)
str(mesa_data$pct_under_net_500m)

## pct_w_atlest1_net
# remove this variable from the dataset - don't think it's needed
mesa_data$pct_w_atlest1_net <- NULL

## total_larval_sites
# total larval sites
table(mesa_data$total_larval_sites, useNA = "always")
summary(mesa_data$total_larval_sites)
str(mesa_data$total_larval_sites)

## total_sites_w_larvae
# total sites with larvae present
table(mesa_data$total_sites_w_larvae, useNA = "always")
summary(mesa_data$total_sites_w_larvae)
str(mesa_data$total_sites_w_larvae)

## min_distance_w_larvae
# minimum distance to site with larvae present (in m?)
table(mesa_data$min_distance_w_larvae, useNA = "always")
summary(mesa_data$min_distance_w_larvae)
str(mesa_data$min_distance_w_larvae)

## Malariaendemic
table(mesa_data$Malariaendemic, useNA = "always")
mesa_data$Malariaendemic[mesa_data$Malariaendemic == ""] = NA
mesa_data$Malariaendemic[mesa_data$Malariaendemic == "no"] = 0
mesa_data$Malariaendemic[mesa_data$Malariaendemic == "yes"] = 1
# coded it as a 0 for no and 1 for yes
str(mesa_data$Malariaendemic)
mesa_data$Malariaendemic = as.numeric(mesa_data$Malariaendemic)
# make a factor
mesa_data$Malariaendemic = factor(mesa_data$Malariaendemic,levels = 0:1, labels = c("no", "yes"))

## date
# don't know what this date is and using interview date as the date for sample collection
# remove this variable
mesa_data$date <- NULL

## ctrl_pct_dead
# note sure what this is (control percent dead?)
# might have to do with mosquitoes
summary(mesa_data$ctrl_pct_dead)
str(mesa_data$ctrl_pct_dead)

## ctrl_pct_KD
# note sure what this is (control percent killed?)
# might have to do with mosquitoes
summary(mesa_data$ctrl_pct_KD)
str(mesa_data$ctrl_pct_KD)

## pct_dead
# note sure what this is (percent dead out of everyone?)
# might have to do with mosquitoes
summary(mesa_data$pct_dead)
str(mesa_data$pct_dead)

## adj_pct_dead
# might have to do with mosquitoes
summary(mesa_data$adj_pct_dead)
str(mesa_data$adj_pct_dead)

## pct_KD
# might have to do with mosquitoes
summary(mesa_data$pct_KD)
str(mesa_data$pct_KD)

## adj_pct_KD
# might have to do with mosquitoes
summary(mesa_data$adj_pct_KD)
str(mesa_data$adj_pct_KD)

## FailKD
# might have to do with mosquitoes
summary(mesa_data$FailKD)
str(mesa_data$FailKD)

## FailMortality
# might have to do with mosquitoes
summary(mesa_data$FailMortality)
str(mesa_data$FailMortality)

## anoph_bloodfed
summary(mesa_data$anoph_bloodfed)
str(mesa_data$anoph_bloodfed)

## anoph_gravid
summary(mesa_data$anoph_gravid)
str(mesa_data$anoph_gravid)

## anoph_halfgravid
summary(mesa_data$anoph_halfgravid)
str(mesa_data$anoph_halfgravid)

## anoph_undetermined
summary(mesa_data$anoph_undetermined)
str(mesa_data$anoph_undetermined)

## anoph_unfed
summary(mesa_data$anoph_unfed)
str(mesa_data$anoph_unfed)

## total_anoph
summary(mesa_data$total_anoph)
str(mesa_data$total_anoph)

## mean_anoph_night
summary(mesa_data$mean_anoph_night)
str(mesa_data$mean_anoph_night)

## numberofdays
# not clear what this variable is and probably won't need it
# remove from data set
summary(mesa_data$numberofdays)
str(mesa_data$numberofdays)
mesa_data$numberofdays <- NULL

## _Mozziemerge2
# remove from data set, don't need
mesa_data$`_Mozziemerge2`<- NULL

## mozzie_sample
# not sure what this data set is
summary(mesa_data$mozzie_sample)
table(mesa_data$mozzie_sample, useNA = "always")
str(mesa_data$mozzie_sample)
# remove from data set
mesa_data$mozzie_sample <- NULL

## anopheles_unfed
summary(mesa_data$anopheles_unfed)
table(mesa_data$anopheles_unfed, useNA = "always")
str(mesa_data$anopheles_unfed)
# why are these numbers different from the anoph_unfed?

## anopheles_fed
summary(mesa_data$anopheles_fed)
table(mesa_data$anopheles_fed, useNA = "always")
str(mesa_data$anopheles_fed)
# why are these numbers different from the anoph_bloodfed?

## anopheles_missing
summary(mesa_data$anopheles_missing)
table(mesa_data$anopheles_missing, useNA = "always")
str(mesa_data$anopheles_missing)
# not sure why this variable is needed 
# remove from data set
mesa_data$anopheles_missing <- NULL

## total_anopheles
summary(mesa_data$total_anopheles)
table(mesa_data$total_anopheles, useNA = "always")
str(mesa_data$total_anopheles)
# why are these numbers different from the total_anoph?

## total_members_RDT_positive
summary(mesa_data$total_members_RDT_positive)
table(mesa_data$total_members_RDT_positive, useNA = "always")
str(mesa_data$total_members_RDT_positive)

## sex
summary(mesa_data$sex)
table(mesa_data$sex, useNA = "always")
mesa_data$sex[mesa_data$sex == ""] = NA
mesa_data$sex[mesa_data$sex == "female"] = 0
mesa_data$sex[mesa_data$sex == "male"] = 1
# coded it as a 0 for female and 1 for male
str(mesa_data$sex)
mesa_data$sex = as.numeric(mesa_data$sex)
# make a factor
mesa_data$sex = factor(mesa_data$sex,levels = 0:1, labels = c("female", "male"))

## gen
# this is the person's gender
summary(mesa_data$gen)
table(mesa_data$gen, useNA = "always")
# this variable has a lot more missing so will just use sex
# remove this variable from the dataset
mesa_data$gen <- NULL

## interview_date
# this is the date variable to use
summary(mesa_data$interview_date)
table(mesa_data$interview_date, useNA = "always")
mesa_data$interview_date[mesa_data$interview_date == ""] = NA
str(mesa_data$interview_date)
# change from a character to a date format
library(lubridate)
mesa_data$interview_date = dmy(mesa_data$interview_date)
str(mesa_data$interview_date)

## alternate_idmatchcard
summary(as.factor(mesa_data$alternate_idmatchcard))
table(mesa_data$alternate_idmatchcard, useNA = "always")
mesa_data$alternate_idmatchcard[mesa_data$alternate_idmatchcard == ""] = NA
mesa_data$alternate_idmatchcard[mesa_data$alternate_idmatchcard == "??"] = NA
mesa_data$alternate_idmatchcard[mesa_data$alternate_idmatchcard == "."] = NA
mesa_data$alternate_idmatchcard[mesa_data$alternate_idmatchcard == "not sure what label says"] = NA
mesa_data$alternate_idmatchcard[mesa_data$alternate_idmatchcard == "the card may be  mislabeled"] = "MISLABELED"
mesa_data$alternate_idmatchcard[mesa_data$alternate_idmatchcard == "these cards may be mislabelled 0391"] = "MISLABELED"
# note sure if this variable will be useful
str(mesa_data$alternate_idmatchcard)

## mem_rdt_idmatchcard
summary(as.factor(mesa_data$mem_rdt_idmatchcard))
table(mesa_data$mem_rdt_idmatchcard, useNA = "always")
mesa_data$mem_rdt_idmatchcard[mesa_data$mem_rdt_idmatchcard == ""] = NA
str(mesa_data$mem_rdt_idmatchcard)

## found
summary(as.factor(mesa_data$found))
table(mesa_data$found, useNA = "always")
mesa_data$found[mesa_data$found == ""] = NA
mesa_data$found[mesa_data$found == "?"] = NA
mesa_data$found[mesa_data$found == "C"] = 1
mesa_data$found[mesa_data$found == "x"] = 1
mesa_data$found[mesa_data$found == "X"] = 1
mesa_data$found[mesa_data$found == "n"] = 0
mesa_data$found[mesa_data$found == "N"] = 0
# recoded to 1 if found, 0 if not found
str(mesa_data$found)
mesa_data$found = as.numeric(mesa_data$found)
# make a factor
mesa_data$found = factor(mesa_data$found,levels = 0:1, labels = c("no", "yes"))

## casechild
summary(as.factor(mesa_data$casechild))
table(mesa_data$casechild, useNA = "always")
# these look like miscellaneous notes about case child
# remove from data set
mesa_data$casechild <- NULL

## studyid_case_controldata
summary(as.factor(mesa_data$studyid_case_controldata))
mesa_data$studyid_case_controldata[mesa_data$studyid_case_controldata == ""] = NA
table(mesa_data$studyid_case_controldata, useNA = "always")
# for some reason this variable has a lot more missing data tahn the studyid_case_control variable
# what's the difference between the two variables?
# delete this variable for now
mesa_data$studyid_case_controldata <- NULL

## case_control_childdata
summary(as.factor(mesa_data$case_control_childdata))
# delete this variable for now
mesa_data$case_control_childdata <- NULL

## mem_rdt_idindata
summary(as.factor(mesa_data$mem_rdt_idindata))
# remove from data set
mesa_data$mem_rdt_idindata <- NULL

## labid_old
# remove this variable
mesa_data$labid_old <- NULL

## labid
table(mesa_data$labid, useNA = "always")
mesa_data$labid[mesa_data$labid == ""] = NA
summary(as.factor(mesa_data$labid))
length(unique(mesa_data$labid))
# looks like all labid values are unique so will use this as our unique identifier
str(mesa_data$labid)

## labid_original
# remove this variable
mesa_data$labid_original <- NULL

## gdnaplate
table(mesa_data$gdnaplate, useNA = "always")
str(mesa_data$gdnaplate)

## gdnacolumn
table(mesa_data$gdnacolumn, useNA = "always")
str(mesa_data$gdnacolumn)

## gdnarow
table(mesa_data$gdnarow, useNA = "always")
mesa_data$gdnarow[mesa_data$gdnarow == ""] = NA
str(mesa_data$gdnarow)

## dbsbox
table(mesa_data$dbsbox, useNA = "always")
mesa_data$dbsbox[mesa_data$dbsbox == ""] = NA
str(mesa_data$dbsbox)

## dbsbag
table(mesa_data$dbsbag, useNA = "always")
mesa_data$dbsbag[mesa_data$dbsbag == ""] = NA
str(mesa_data$dbsbag)

## net_used_lastnight
table(mesa_data$net_used_lastnight, useNA = "always")
mesa_data$net_used_lastnight[mesa_data$net_used_lastnight == ""] = NA
mesa_data$net_used_lastnight[mesa_data$net_used_lastnight == "yes"] = 1
mesa_data$net_used_lastnight[mesa_data$net_used_lastnight == "no"] = 0
# 1 for yes, 0 for no
str(mesa_data$net_used_lastnight)
mesa_data$net_used_lastnight = as.numeric(mesa_data$net_used_lastnight)
# make a factor
mesa_data$net_used_lastnight = factor(mesa_data$net_used_lastnight,levels = 0:1, labels = c("no", "yes"))

## make a variable that identifies if someone is a case, a control, a case household member or
# a control household member
person_type = ifelse(is.na(mesa_data$case_child)==T | is.na(mesa_data$control_child)==T | is.na(mesa_data$case_household)==T | is.na(mesa_data$control_household)==T,NA,
                     ifelse(mesa_data$case_child == "yes",0,
                            ifelse(mesa_data$control_child == "yes",1,
                                   ifelse(mesa_data$case_child != "yes" & mesa_data$case_household == "yes",2,
                                          ifelse(mesa_data$control_child != "yes" & mesa_data$control_household == "yes",3,NA)))))
table(person_type, useNA = "always")
length(which(is.na(mesa_data$case_child)==T | is.na(mesa_data$control_child)==T | is.na(mesa_data$case_household)==T | is.na(mesa_data$control_household)==T))
length(which(mesa_data$case_child == "yes"))
length(which(mesa_data$control_child == "yes"))
length(which(mesa_data$case_child != "yes" & mesa_data$case_household == "yes"))
length(which(mesa_data$control_child != "yes" & mesa_data$control_household == "yes"))
table(person_type,mesa_data$case_child)
table(person_type,mesa_data$control_child)
table(person_type,mesa_data$control_household)
table(person_type,mesa_data$case_household)
# 0 is a case child, 1 is a control child, 2 is a case household, 3 is a control household
# make this variable a factor
mesa_data$person_type = factor(person_type,levels = 0:3, labels = c("case child", "control child","case household member","control household member"))
str(person_type)

## change all column names to lowercase for each of coding in the future
colnames(mesa_data) = tolower(colnames(mesa_data))

## write out the new data set
write_csv(mesa_data,"mesa_data_clean.csv")



