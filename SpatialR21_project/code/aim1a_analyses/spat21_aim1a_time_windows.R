# -------------------------------------- #
#           Spat21/Mozzie Study          #
#           Look at time windows         #
#                 Aim 1A                 #
#               Human Data               #
#            Mozzie Phase 3              #
#                K. Sumner               #
#             February 9, 2021           #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(tidyverse)
library(survminer)
library(survival)
library(ggalluvial)
library(gridExtra)
library(coxme)


#### ------- read in the data sets -------- ####

# read in the primary data set
survival_data_primary = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_primary_survival_format_19NOV2020.rds")


# change the age category coding
# for primary data
table(survival_data_primary$age_cat_baseline)
survival_data_primary$new_age_cat_baseline = ifelse(survival_data_primary$age_cat_baseline== ">15 years",">15 years","15 years or less")
table(survival_data_primary$new_age_cat_baseline,useNA = "always")
survival_data_primary$new_age_cat_baseline = as.factor(survival_data_primary$new_age_cat_baseline)
survival_data_primary$new_age_cat_baseline = relevel(survival_data_primary$new_age_cat_baseline,ref="15 years or less")


#### ------- recode the survival data primary to just look at when people first leave the study --------- ####

##  primary data set

# look when each person first lost to follow-up

# create a unique list of people
person_list = unique(survival_data_primary$unq_memID)

# now pull out the first time each person becomes days until event becomes > than the day before 
# (so it's the first time  they were LTFU or had event)
end_date = rep(length(person_list))
person = rep(length(person_list))
for (i in 1:length(person_list)){
  end_found = 0
  for (j in 1:nrow(survival_data_primary)){
    if (j > 1){
      if (survival_data_primary$unq_memID[j] == person_list[i]){
        if (survival_data_primary$days_until_event[j-1] < survival_data_primary$days_until_event[j] &
            end_found == 0){
          end_date[i] = survival_data_primary$sample_id_date[j]
          person[i] = survival_data_primary$unq_memID[j]
          end_found = 1
        }
      }
    }
  }
}
end_dates_df = data.frame(person,end_date)
end_dates_df$end_date = as_date(end_dates_df$end_date, origin = lubridate::origin)


### NEED TO FINISH THIS ---- START BACK HERE
# now need to check that end dates are correct and fill in missing end dates









#### ------ make plot of symptomatic infections over time for aim 1A ------- ####

symptomatic_months = final_data %>%
  filter(main_outcome_primary_case_def == "symptomatic infection")
symptomatic_months$month_year = lubridate::floor_date(ymd(symptomatic_months$sample_id_date),"month")
table(symptomatic_months$month_year,useNA="always")
symptomatic_df = symptomatic_months %>%
  select(month_year) %>%
  group_by(month_year) %>%
  summarise(n=n())
test_plot = ggplot(data=symptomatic_df,aes(x=month_year,y=n)) +
  geom_bar(stat="identity") +
  theme_bw() +
  ylab("Number of symptomatic infections") +
  xlab("Month") +
  scale_x_date(date_breaks="1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
test_plot
ggsave(test_plot, filename="/Users/kelseysumner/Desktop/aim1a_symp_infections_over_time.png", device="png",
       height=6, width=11, units="in", dpi=500)



