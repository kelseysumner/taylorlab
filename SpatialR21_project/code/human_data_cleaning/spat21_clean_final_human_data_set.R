# ----------------------------------------- #
#  Clean Final Merged Data Set for Spat21   #
#                Human Data                 #
#               May 28, 2019                #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(tableone)
library(stringr)


#### -------- read in the final merged data set ------------- ####

human_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/spat21_human_merged_all_data_21MAY2019.RDS")



#### -------- look at the remaining variables and clean ------- ####

# look at the column names
colnames(human_merged_data)
# 135 variables in final data set

# make a data frame of the colnames
colname_df = data.frame(colnames(human_merged_data))
colname_df
# write_csv(colname_df,"colname_df.csv")

# look at each variable and make sure it's clean

# today_hum_monthly_data
summary(human_merged_data$today_hum_monthly_data)
str(human_merged_data$today_hum_monthly_data)

# village_name_hum_monthly_data
table(human_merged_data$village_name_hum_monthly_data, useNA = "always")
str(human_merged_data$village_name_hum_monthly_data)

# village_name_hum_monthly_data
table(human_merged_data$village_name_hum_monthly_data, useNA = "always")
str(human_merged_data$village_name_hum_monthly_data)
# village_all_data
table(human_merged_data$village_all_data, useNA = "always")
str(human_merged_data$village_all_data)
# village_name_hum_sick_data
table(human_merged_data$village_name_hum_sick_data, useNA = "always")
str(human_merged_data$village_name_hum_sick_data)
# remove village_all_data
human_merged_data$village_all_data <- NULL
# create a new village variable based on sample name final
length(which(is.na(human_merged_data$sample_name_final))) # no missing
village_name = rep(NA,nrow(human_merged_data))
for (i in 1:nrow(human_merged_data)){
  first_name = str_split(human_merged_data$sample_name_final[i],"-")[[1]]
  first_letter = str_split(first_name,"")[[1]][1]
  if (first_letter == "K"){
    village_name[i] = "Kinesamo"
  }
  if (first_letter == "M"){
    village_name[i] = "Maruti"
  }
  if (first_letter == "S"){
    village_name[i] = "Sitabicha"
  }
}
table(village_name,useNA="always")
# add village name to data set
human_merged_data$village_name = village_name
# remove other village variables
human_merged_data$village_name_hum_monthly_data <- NULL
human_merged_data$village_name_hum_sick_data <- NULL
# check output for village_name one more time
sum(str_detect(human_merged_data$sample_name_final,"K")) # 995
sum(str_detect(human_merged_data$sample_name_final,"M")) # 894
sum(str_detect(human_merged_data$sample_name_final,"S")) # 1030
# all looks good

# gender
table(human_merged_data$gender_hum_monthly_data, useNA="always")
table(human_merged_data$gender_hum_sick_data, useNA = "always")
# check if there's any case wehre male and female didn't match
gender_df = data.frame(monthly_gender=human_merged_data$gender_hum_monthly_data,sick_gender=human_merged_data$gender_hum_sick_data)
gender_df = na.omit(gender_df)
identical(gender_df$monthly_gender,gender_df$sick_gender)
# if sick data gender is any different, default to monthly data gender
gender = ifelse(!(is.na(human_merged_data$gender_hum_monthly_data)),human_merged_data$gender_hum_monthly_data,human_merged_data$gender_hum_sick_data)
table(gender,useNA = "always")
table(human_merged_data$gender_hum_monthly_data)
table(human_merged_data$gender_hum_sick_data)
# make gender variable a factor
human_merged_data$gender = factor(gender,level=c(1,2),labels=c("male","female"))
table(human_merged_data$gender, useNA = "always")
# remove the old gender variables
human_merged_data$gender_hum_monthly_data <- NULL
human_merged_data$gender_hum_sick_data <- NULL

# ages
table(human_merged_data$age_type, useNA = "always")
table(human_merged_data$age_y, useNA = "always")
table(human_merged_data$age_m, useNA = "always")
# there are 47 observations for babies that ended up turning 1 during the study period
# check that those that turn 1 are the same with the 1 year entries
age_df = human_merged_data %>%
  filter(!(is.na(age_m)) | age_y == 1) %>%
  select(unq_memID,age_m,age_y)






