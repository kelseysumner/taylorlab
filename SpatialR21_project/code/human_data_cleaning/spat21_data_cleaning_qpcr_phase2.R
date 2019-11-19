# ----------------------------------------- #
#   Spat21 Data Set Cleaning - qPCR Data    #
#                Human Data                 #
#              Mozzie Phase 2               #
#             November 19, 2019             #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(stringr)
library(broom)
library(ggplot2)


#### ---------- load in the qpcr data set ---------- ####

# read in the human DBS qpcr data set
qpcr_data = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Cohort data through August July 2019/raw_data/clean Mozzie P2 DBS Plate B1-B31 Compiled 11-19-19.csv")

# look at a summary of the data set
summary(qpcr_data)
str(qpcr_data)

#### ------- clean up the pcr data some ------- ####

# remove entries where the sample id was missing or it was 3d7 culture
qpcr_data = qpcr_data[-which(is.na(qpcr_data$`Sample Name`)),]
qpcr_data = qpcr_data[-which(str_detect(qpcr_data$`Sample Name`,"cult")),]



#### ------ standardize the qpcr curves ------ ####

# look at the column names
names(qpcr_data)

# pull out a table of each of the qpcr plates and their correspond Pf standards
# note: only pulled out standards 1-8 because don't want to use 1-10
qpcr_plates_table = qpcr_data %>%
  filter(!(is.na(`Experiment Name`))) %>%
  group_by(`Experiment Name`) %>%
  select(`Experiment Name`,pfr364Std1a,pfr364Std1b,pfr364Std2a,pfr364Std2b,pfr364Std3a,pfr364Std3b,
         pfr364Std4a,pfr364Std4b,pfr364Std5a,pfr364Std5b,pfr364Std6a,pfr364Std6b,pfr364Std7a,pfr364Std7b,
         pfr364Std8a,pfr364Std8b)
qpcr_plates_table = unique(qpcr_plates_table)

# add columns to the table with the concentration of the parasites
qpcr_plates_table$Std1a_x = 2000
qpcr_plates_table$Std1b_x = 2000
qpcr_plates_table$Std2a_x = 1000
qpcr_plates_table$Std2b_x = 1000
qpcr_plates_table$Std3a_x = 200
qpcr_plates_table$Std3b_x = 200
qpcr_plates_table$Std4a_x = 100
qpcr_plates_table$Std4b_x = 100
qpcr_plates_table$Std5a_x = 20
qpcr_plates_table$Std5b_x = 20
qpcr_plates_table$Std6a_x = 10
qpcr_plates_table$Std6b_x = 10
qpcr_plates_table$Std7a_x = 2
qpcr_plates_table$Std7b_x = 2
qpcr_plates_table$Std8a_x = 1
qpcr_plates_table$Std8b_x = 1

# recode everything labeled "undetermined" or 0 as NA
qpcr_plates_table[qpcr_plates_table == "Undetermined" | qpcr_plates_table == 0] = NA

# run a linear regression model with the concentrations as the x values and ct values as the y value for the standards
# run this model for each plate
model1 = lm(as.numeric(unlist(qpcr_plates_table[1,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[1,18:33]))), data=qpcr_plates_table)
model2 = lm(as.numeric(unlist(qpcr_plates_table[2,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[2,18:33]))), data=qpcr_plates_table)
model3 = lm(as.numeric(unlist(qpcr_plates_table[3,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[3,18:33]))), data=qpcr_plates_table)
model4 = lm(as.numeric(unlist(qpcr_plates_table[4,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[4,18:33]))), data=qpcr_plates_table)
model5 = lm(as.numeric(unlist(qpcr_plates_table[5,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[5,18:33]))), data=qpcr_plates_table)
model6 = lm(as.numeric(unlist(qpcr_plates_table[6,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[6,18:33]))), data=qpcr_plates_table)
model7 = lm(as.numeric(unlist(qpcr_plates_table[7,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[7,18:33]))), data=qpcr_plates_table)
model8 = lm(as.numeric(unlist(qpcr_plates_table[8,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[8,18:33]))), data=qpcr_plates_table)
model9 = lm(as.numeric(unlist(qpcr_plates_table[9,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[9,18:33]))), data=qpcr_plates_table)
model10 = lm(as.numeric(unlist(qpcr_plates_table[10,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[10,18:33]))), data=qpcr_plates_table)
model11 = lm(as.numeric(unlist(qpcr_plates_table[11,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[11,18:33]))), data=qpcr_plates_table)
model12 = lm(as.numeric(unlist(qpcr_plates_table[12,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[12,18:33]))), data=qpcr_plates_table)
model13 = lm(as.numeric(unlist(qpcr_plates_table[13,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[13,18:33]))), data=qpcr_plates_table)
model14 = lm(as.numeric(unlist(qpcr_plates_table[14,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[14,18:33]))), data=qpcr_plates_table)
model15 = lm(as.numeric(unlist(qpcr_plates_table[15,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[15,18:33]))), data=qpcr_plates_table)
model16 = lm(as.numeric(unlist(qpcr_plates_table[16,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[16,18:33]))), data=qpcr_plates_table)

# combine the models in a data frame
m1_df = bind_cols(tidy(model1), confint_tidy(model1)) %>% mutate(model_name="M1: plate1-2") %>% mutate(r_value = summary(model1)$r.squared)
m2_df = bind_cols(tidy(model2), confint_tidy(model2)) %>% mutate(model_name="M2: plate3-4") %>% mutate(r_value = summary(model2)$r.squared)
m3_df = bind_cols(tidy(model3), confint_tidy(model3)) %>% mutate(model_name="M3: plate5-6") %>% mutate(r_value = summary(model3)$r.squared)
m4_df = bind_cols(tidy(model4), confint_tidy(model4)) %>% mutate(model_name="M4: plate7-8") %>% mutate(r_value = summary(model4)$r.squared)
m5_df = bind_cols(tidy(model5), confint_tidy(model5)) %>% mutate(model_name="M5: plate9-10") %>% mutate(r_value = summary(model5)$r.squared)
m6_df = bind_cols(tidy(model6), confint_tidy(model6)) %>% mutate(model_name="M6: plate11-12") %>% mutate(r_value = summary(model6)$r.squared)
m7_df = bind_cols(tidy(model7), confint_tidy(model7)) %>% mutate(model_name="M7: plate13-14") %>% mutate(r_value = summary(model7)$r.squared)
m8_df = bind_cols(tidy(model8), confint_tidy(model8)) %>% mutate(model_name="M8: plate15-16") %>% mutate(r_value = summary(model8)$r.squared)
m9_df = bind_cols(tidy(model9), confint_tidy(model9)) %>% mutate(model_name="M9: plate17-18") %>% mutate(r_value = summary(model9)$r.squared)
m10_df = bind_cols(tidy(model10), confint_tidy(model10)) %>% mutate(model_name="M10: plate19-20") %>% mutate(r_value = summary(model10)$r.squared)
m11_df = bind_cols(tidy(model11), confint_tidy(model11)) %>% mutate(model_name="M11: plate21-22") %>% mutate(r_value = summary(model11)$r.squared)
m12_df = bind_cols(tidy(model12), confint_tidy(model12)) %>% mutate(model_name="M12: plate23-24") %>% mutate(r_value = summary(model12)$r.squared)
m13_df = bind_cols(tidy(model13), confint_tidy(model13)) %>% mutate(model_name="M13: plate25-26") %>% mutate(r_value = summary(model13)$r.squared)
m14_df = bind_cols(tidy(model14), confint_tidy(model14)) %>% mutate(model_name="M14: plate27-28") %>% mutate(r_value = summary(model14)$r.squared)
m15_df = bind_cols(tidy(model15), confint_tidy(model15)) %>% mutate(model_name="M15: plate29-30") %>% mutate(r_value = summary(model15)$r.squared)
m16_df = bind_cols(tidy(model16), confint_tidy(model16)) %>% mutate(model_name="M16: plate31") %>% mutate(r_value = summary(model16)$r.squared)
model_results_df = bind_rows(m1_df,m2_df,m3_df,m4_df,m5_df,m6_df,m7_df,m8_df,m9_df,m10_df,m11_df,m12_df,m13_df,
                             m14_df,m15_df,m16_df)

# rename the slope value
model_results_df[2,1] <- "slope"
model_results_df[4,1] <- "slope"
model_results_df[6,1] <- "slope"
model_results_df[8,1] <- "slope"
model_results_df[10,1] <- "slope"
model_results_df[12,1] <- "slope"
model_results_df[14,1] <- "slope"
model_results_df[16,1] <- "slope"
model_results_df[18,1] <- "slope"
model_results_df[20,1] <- "slope"
model_results_df[22,1] <- "slope"
model_results_df[24,1] <- "slope"
model_results_df[26,1] <- "slope"
model_results_df[28,1] <- "slope"
model_results_df[30,1] <- "slope"
model_results_df[32,1] <- "slope"

# remove the columns that aren't needed
model_results_df$std.error <- NULL
model_results_df$statistic <- NULL
model_results_df$p.value <- NULL
model_results_df$conf.low <- NULL
model_results_df$conf.high <- NULL

# switch this dataframe from long to wide format
model_results_simplified = spread(data=model_results_df,key=term,value=estimate)

# rename the intercept column
colnames(model_results_simplified)[colnames(model_results_simplified) == '(Intercept)'] <- 'intercept'

# write out the data frame
write_csv(model_results_simplified,"Desktop/phase2_spat21_model_results_simplified_human_dbs.csv")


#### ------- check the model output --------- ####

# pull out a table of each of the qpcr plates and their correspond Pf standards
# note: only pulled out standards 1-8 because don't want to use 1-10
qpcr_plates_table = qpcr_data %>%
  filter(!(is.na(`Experiment Name`))) %>%
  group_by(`Experiment Name`) %>%
  select(`Experiment Name`,pfr364Std1a,pfr364Std1b,pfr364Std2a,pfr364Std2b,pfr364Std3a,pfr364Std3b,
         pfr364Std4a,pfr364Std4b,pfr364Std5a,pfr364Std5b,pfr364Std6a,pfr364Std6b,pfr364Std7a,pfr364Std7b,
         pfr364Std8a,pfr364Std8b,pfr364Std9a,pfr364Std9b,pfr364Std10a,pfr364Std10b)
qpcr_plates_table = unique(qpcr_plates_table)

# add columns to the table with the concentration of the parasites
qpcr_plates_table$Std1a_x = 2000
qpcr_plates_table$Std1b_x = 2000
qpcr_plates_table$Std2a_x = 1000
qpcr_plates_table$Std2b_x = 1000
qpcr_plates_table$Std3a_x = 200
qpcr_plates_table$Std3b_x = 200
qpcr_plates_table$Std4a_x = 100
qpcr_plates_table$Std4b_x = 100
qpcr_plates_table$Std5a_x = 20
qpcr_plates_table$Std5b_x = 20
qpcr_plates_table$Std6a_x = 10
qpcr_plates_table$Std6b_x = 10
qpcr_plates_table$Std7a_x = 2
qpcr_plates_table$Std7b_x = 2
qpcr_plates_table$Std8a_x = 1
qpcr_plates_table$Std8b_x = 1
qpcr_plates_table$Std9a_x = 0.2
qpcr_plates_table$Std9b_x = 0.2
qpcr_plates_table$Std10a_x = 0.1
qpcr_plates_table$Std10b_x = 0.1

# recode everything labeled "undetermined" or 0 as NA
qpcr_plates_table[qpcr_plates_table == "Undetermined" | qpcr_plates_table == 0] = NA

# run a linear regression model with the concentrations as the x values and ct values as the y value for the standards
# run this model for each plate
model1 = lm(as.numeric(unlist(qpcr_plates_table[1,2:21])) ~ log(as.numeric(unlist(qpcr_plates_table[1,22:41]))), data=qpcr_plates_table)
summary(model1)
plot(log(as.numeric(unlist(qpcr_plates_table[1,22:41]))),as.numeric(unlist(qpcr_plates_table[1,2:21])))
# log10 model
# run this model for each plate
model10 = lm(as.numeric(unlist(qpcr_plates_table[1,2:21])) ~ log10(as.numeric(unlist(qpcr_plates_table[1,22:41]))), data=qpcr_plates_table)
summary(model10)
plot(log10(as.numeric(unlist(qpcr_plates_table[1,22:41]))),as.numeric(unlist(qpcr_plates_table[1,2:21])))
# a second log10 model check
model102 = lm(as.numeric(unlist(qpcr_plates_table[2,2:21])) ~ log10(as.numeric(unlist(qpcr_plates_table[2,22:41]))), data=qpcr_plates_table)
summary(model102)
plot(log10(as.numeric(unlist(qpcr_plates_table[1,22:41]))),as.numeric(unlist(qpcr_plates_table[1,2:21])))
# try another model
model2 = glm(as.numeric(unlist(qpcr_plates_table[1,2:21])) ~ log(as.numeric(unlist(qpcr_plates_table[1,22:41]))), data=qpcr_plates_table, gaussian("log"))
summary(model2)
# try another model
model3 = glm(as.numeric(unlist(qpcr_plates_table[1,2:21])) ~ as.numeric(unlist(qpcr_plates_table[1,22:41])), data=qpcr_plates_table, gaussian("log"))
summary(model3)
# try another model
model4 = glm(as.numeric(unlist(qpcr_plates_table[1,2:21])) ~ as.numeric(unlist(qpcr_plates_table[1,22:41])), data=qpcr_plates_table, gaussian("identity"))
summary(model4)
# compare this model output to what was produced by the program
qpcr_original = qpcr_data %>%
  filter(!(is.na(`Experiment Name`))) %>%
  group_by(`Experiment Name`) %>%
  select(`Experiment Name`,`pfr364Y-Intercept`,pfr364R_,pfr364Slope)
qpcr_original = unique(qpcr_original)
# look at a plot of the x and y correlation
plot(as.numeric(unlist(qpcr_plates_table[1,22:41])),as.numeric(unlist(qpcr_plates_table[1,2:21])))


#### -------- standardize the qpcr values for each plate --------- #####

# first clear the working directory
# read in the merged data set
# read in the human DBS qpcr data set
qpcr_data = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Cohort data through August July 2019/raw_data/clean Mozzie P2 DBS Plate B1-B31 Compiled 11-19-19.csv")
# read in the new model results table for the standards 1-2000 p/uL (with experiment name manually added in in Excel)
model_results = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Cohort data through August July 2019/raw_data/phase2_spat21_model_results_simplified_human_dbs.csv")

# remove entries where the sample id was missing or it was 3d7 culture
qpcr_data = qpcr_data[-which(is.na(qpcr_data$`Sample Name`)),]
qpcr_data = qpcr_data[-which(str_detect(qpcr_data$`Sample Name`,"cult")),]


# change the column names for model_results to represent the new standardization
colnames(model_results)[colnames(model_results) == 'r_value'] <- 'r_value_std'
colnames(model_results)[colnames(model_results) == 'intercept'] <- 'intercept_std'
colnames(model_results)[colnames(model_results) == 'slope'] <- 'slope_std'

# now merge in the model_results with the qpcr human data for spat21
final_qpcr_merge = left_join(qpcr_data,model_results,by="Experiment Name")

# check the merge
table(final_qpcr_merge$r_value_std, useNA = "always")
table(final_qpcr_merge$`pfr364R²`, useNA = "always")
table(final_qpcr_merge$intercept_std, useNA = "always")
table(final_qpcr_merge$`pfr364Y-Intercept`,useNA = "always")

# make sure the qpcr values are numeric and change "Undetermined" to NA to represent missing because undetectable
# make sure all the qpcr values with CT values ==0  are also changed to NA
# first check how many are 0, undetermined, and missing
length(which(final_qpcr_merge$HbtubCT1 == "Undetermined" | final_qpcr_merge$HbtubCT1 == "0" | final_qpcr_merge$HbtubCT1 == "0.000")) # 31
length(which(final_qpcr_merge$HbtubCT2 == "Undetermined" | final_qpcr_merge$HbtubCT2 == "0" | final_qpcr_merge$HbtubCT2 == "0.000")) # 26
length(which(final_qpcr_merge$pfr364CT1 == "Undetermined" | final_qpcr_merge$pfr364CT1 == "0" | final_qpcr_merge$pfr364CT1 == "0.000")) # 1927
length(which(final_qpcr_merge$pfr364CT2 == "Undetermined" | final_qpcr_merge$pfr364CT2 == "0" | final_qpcr_merge$pfr364CT2 == "0.000")) # 1917
length(which(is.na(final_qpcr_merge$HbtubCT1))) # 0
length(which(is.na(final_qpcr_merge$HbtubCT2))) # 0
length(which(is.na(final_qpcr_merge$pfr364CT1))) # 0
length(which(is.na(final_qpcr_merge$pfr364CT2))) # 0
# then change the values
final_qpcr_merge$HbtubCT1[final_qpcr_merge$HbtubCT1 == "Undetermined" | final_qpcr_merge$HbtubCT1 == "0" | final_qpcr_merge$HbtubCT1 == "0.000"] = NA
final_qpcr_merge$HbtubCT2[final_qpcr_merge$HbtubCT2 == "Undetermined" | final_qpcr_merge$HbtubCT2 == "0" | final_qpcr_merge$HbtubCT2 == "0.000"] = NA
final_qpcr_merge$pfr364CT1[final_qpcr_merge$pfr364CT1 == "Undetermined" | final_qpcr_merge$pfr364CT1 == "0" | final_qpcr_merge$pfr364CT1 == "0.000"] = NA
final_qpcr_merge$pfr364CT2[final_qpcr_merge$pfr364CT2 == "Undetermined" | final_qpcr_merge$pfr364CT2 == "0" | final_qpcr_merge$pfr364CT2 == "0.000"] = NA
# then check how many are now missing
length(which(final_qpcr_merge$HbtubCT1 == "Undetermined" | final_qpcr_merge$HbtubCT1 == "0" | final_qpcr_merge$HbtubCT1 == "0.000")) # 0
length(which(final_qpcr_merge$HbtubCT2 == "Undetermined" | final_qpcr_merge$HbtubCT2 == "0" | final_qpcr_merge$HbtubCT2 == "0.000")) # 0
length(which(final_qpcr_merge$pfr364CT1 == "Undetermined" | final_qpcr_merge$pfr364CT1 == "0" | final_qpcr_merge$pfr364CT1 == "0.000")) # 0
length(which(final_qpcr_merge$pfr364CT2 == "Undetermined" | final_qpcr_merge$pfr364CT2 == "0" | final_qpcr_merge$pfr364CT2 == "0.000")) # 0
length(which(is.na(final_qpcr_merge$HbtubCT1))) # 31
length(which(is.na(final_qpcr_merge$HbtubCT2))) # 26
length(which(is.na(final_qpcr_merge$pfr364CT1))) # 1927
length(which(is.na(final_qpcr_merge$pfr364CT2))) # 1917

# make sure the values for the standardization formula are numeric
final_qpcr_merge$pfr364CT1 = as.numeric(final_qpcr_merge$pfr364CT1)
final_qpcr_merge$pfr364CT2 = as.numeric(final_qpcr_merge$pfr364CT2)
final_qpcr_merge$HbtubCT1 = as.numeric(final_qpcr_merge$HbtubCT1)
final_qpcr_merge$HbtubCT2 = as.numeric(final_qpcr_merge$HbtubCT2)
final_qpcr_merge$intercept_std = as.numeric(final_qpcr_merge$intercept_std)
final_qpcr_merge$slope_std = as.numeric(final_qpcr_merge$slope_std)

# look at summaries of all the ct values
summary(final_qpcr_merge$HbtubCT1)
summary(final_qpcr_merge$HbtubCT2)
summary(final_qpcr_merge$pfr364CT1)
summary(final_qpcr_merge$pfr364CT2)

# create two new columns for each of the replicates that are restandardized to the new slope and intercept for standards 1-2000 p/uL
# will use the columns pfr364CT1 and pfr364CT2 as the observed y-values
# plug in those to y=mx+b equation solved for x -> x = (y-b)/m
pfr364Q1_std = rep(NA,nrow(final_qpcr_merge))
pfr364Q2_std = rep(NA,nrow(final_qpcr_merge))
for (i in 1:nrow(final_qpcr_merge)){
  if (is.na(final_qpcr_merge$pfr364CT1[i])){
    pfr364Q1_std[i] = NA
  } else {
    pfr364Q1_std[i] = 10^((final_qpcr_merge$pfr364CT1[i] - final_qpcr_merge$intercept_std[i])/final_qpcr_merge$slope_std[i])
  }
}
for (i in 1:nrow(final_qpcr_merge)){
  if (is.na(final_qpcr_merge$pfr364CT2[i])){
    pfr364Q2_std[i] = NA
  } else {
    pfr364Q2_std[i] = 10^((final_qpcr_merge$pfr364CT2[i] - final_qpcr_merge$intercept_std[i])/final_qpcr_merge$slope_std[i])
  }
}
final_qpcr_merge$pfr364Q1_std = pfr364Q1_std
final_qpcr_merge$pfr364Q2_std = pfr364Q2_std

# compare the output
summary(final_qpcr_merge$pfr364Q1_std)
summary(final_qpcr_merge$pfr364Q2_std)
summary(final_qpcr_merge$slope_std)
summary(final_qpcr_merge$intercept_std)
summary(final_qpcr_merge$pfr364CT1)
summary(final_qpcr_merge$pfr364CT2)
summary(final_qpcr_merge$pfr364Q1)
summary(final_qpcr_merge$pfr364Q2)

# export the data set
write_csv(final_qpcr_merge,"Desktop/phase2_spat21_final_qpcr_merge_human_dbs.csv")

# change all the original Q1 and Q2 values that are 0 to missing
final_qpcr_merge$pfr364Q1[final_qpcr_merge$pfr364Q1 == 0] = NA
final_qpcr_merge$pfr364Q2[final_qpcr_merge$pfr364Q2 == 0] = NA

# test the formula for Q1
pfr364Q1_test = rep(NA,nrow(final_qpcr_merge))
for (i in 1:nrow(final_qpcr_merge)){
  if (is.na(final_qpcr_merge$pfr364CT1[i])){
    pfr364Q1_test[i] = NA
  } else {
    pfr364Q1_test[i] = 10^((final_qpcr_merge$pfr364CT1[i] - final_qpcr_merge$`pfr364Y-Intercept`[i])/final_qpcr_merge$pfr364Slope[i])
  }
}
summary(pfr364Q1_test)
summary(final_qpcr_merge$pfr364Q1)

# add the test variable to the final_qpcr_merge data set
final_qpcr_merge$pfr364Q1_test = pfr364Q1_test

# pull out where there's an extra missing value in pfr364Q1 compared ot the new value
testdata = final_qpcr_merge[which(is.na(final_qpcr_merge$pfr364Q1) & !(is.na(final_qpcr_merge$pfr364Q1_test))),]
testdata$pfr364Q1_test
# looks like there aren't any extra missing values
# look to see if any high Pf CT1 value
length(which(final_qpcr_merge$pfr364CT1 > 38)) # 24 with Pf CT1 value greater than 38
# reread in the original merged_data set (before you edited it with censoring criteria to look at original CT and Q values)
highpfct = qpcr_data[which(as.numeric(qpcr_data$pfr364CT1) > 38 & qpcr_data$pfr364CT1 != "undetermined"),]
# difference is not due to high Pf CT1 value but I think instead in the machine saying that the sample had 0 parasitemia for both duplicates beacause both duplicates had high CT values >38
# therefore, coding looks good

# looking at where differences occurred
chck_val <- data.frame(newval = pfr364Q1_test,oldval = final_qpcr_merge$pfr364Q1)
chck_val %>% apply(1,function(x){identical(x[1],x[2])})
diff = chck_val %>% mutate(diff = newval - oldval)
chck_val %>% ggplot(aes(x=newval,y=oldval)) + geom_point()
# looks like the differences occur because of rounding of the values and the formula is working correctly

# look at how Q1 was calculate when there were "undetermineds"
table(final_qpcr_merge$pfr364CT1,useNA = "always")
length(which(final_qpcr_merge$pfr364CT1=="Undetermined"))
undeter = final_qpcr_merge[which(final_qpcr_merge$pfr364CT1=="Undetermined"),]
undeter = undeter[,c("pfr364CT1","pfr364CT2","pfr364Q1","pfr364Q2","pfr364Y-Intercept","pfr364Slope")]

# test the formula for Q2
pfr364Q2_test = rep(NA,nrow(final_qpcr_merge))
for (i in 1:nrow(final_qpcr_merge)){
  if (is.na(final_qpcr_merge$pfr364CT2[i])){
    pfr364Q2_test[i] = NA
  } else {
    pfr364Q2_test[i] = 10^((final_qpcr_merge$pfr364CT2[i] - final_qpcr_merge$`pfr364Y-Intercept`[i])/final_qpcr_merge$pfr364Slope[i])
  }
}
summary(pfr364Q2_test)
summary(final_qpcr_merge$pfr364Q2)

# add the test variable to the final_qpcr_merge data set
final_qpcr_merge$pfr364Q2_test = pfr364Q2_test

# pull out where there's an extra missing value in pfr364Q2 compared ot the new value
testdata = final_qpcr_merge[which(is.na(final_qpcr_merge$pfr364Q2) & !(is.na(final_qpcr_merge$pfr364Q2_test))),]
testdata$pfr364Q2_test
# looks like there aren't really any differences
# difference could be due to high Pf CT2 value
length(which(final_qpcr_merge$pfr364CT2 > 38)) # 27 with Pf CT2 value greater than 38

# looking at where differences occurred
chck_val <- data.frame(newval = pfr364Q2_test,oldval = final_qpcr_merge$pfr364Q2)
chck_val %>% apply(1,function(x){identical(x[1],x[2])})
diffcheck = chck_val %>% mutate(diff = newval - oldval)
chck_val %>% ggplot(aes(x=newval,y=oldval)) + geom_point()
# looks like the differences occur because of rounding of the values and the formula is working correctly


#### ------------ look over standardized Q1 and Q1 for new standardizations ------------- ####

# read back in the final qpcr data set
final_results = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Cohort data through August July 2019/clean_data/phase2_spat21_final_qpcr_merge_human_dbs.csv")

# look at summaries of the distribution of the old Q1 and Q2 from the 10 standards (0.1-2000 p/uL)
summary(final_results$pfr364Q1)
summary(final_results$pfr364Q2)
boxplot(final_results$pfr364Q1)
boxplot(final_results$pfr364Q2)

# look at summaries of the distribution of the new Q1 and Q2 from the 8 standards (1-2000 p/uL)
summary(final_results$pfr364Q1_std)
summary(final_results$pfr364Q2_std)
boxplot(final_results$pfr364Q1_std)
boxplot(final_results$pfr364Q2_std)


#### ---------------- create a composite parasitemia value (combine Q1 & Q2) ------------------ ####

# read back in the final qpcr data set
final_results = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Cohort data through August July 2019/clean_data/phase2_spat21_final_qpcr_merge_human_dbs.csv")


# make pfr364CT numeric
summary(final_results$pfr364CT1)
summary(final_results$pfr364CT2)
final_results$pfr364CT1 = as.numeric(final_results$pfr364CT1)
final_results$pfr364CT2 = as.numeric(final_results$pfr364CT2)

# determine Sample Name that have human beta tubulin missing and need to be excluded
hbcriteria_1 = final_results[which(is.na(final_results$HbtubCT1) & !(is.na(final_results$r_value_std))),]
# pull out the vector of Sample Name
hbcriteria_1_ids = hbcriteria_1$`Sample Name`
# now the second CT value for Hb
hbcriteria_2 = final_results[which(is.na(final_results$HbtubCT2) & !(is.na(final_results$r_value_std))),]
# pull out the vector of Sample Name
hbcriteria_2_ids = hbcriteria_2$`Sample Name`

# look at original summaries of pfr364Q variables
summary(final_results$pfr364Q1_std) # 1927 missing
summary(final_results$pfr364Q2_std) # 1917 missing

# make a variable that censors for human beta tublin CT values missing
final_results$pfr364Q1_std_censored = ifelse(final_results$`Sample Name` %in% hbcriteria_1_ids,NA,final_results$pfr364Q1_std)
final_results$pfr364Q2_std_censored = ifelse(final_results$`Sample Name` %in% hbcriteria_2_ids,NA,final_results$pfr364Q2_std)
summary(final_results$pfr364Q1_std_censored) # 1928 missing
summary(final_results$pfr364Q2_std_censored) # 1918 missing
hbcriteria_1$pfr364Q1_std
hbcriteria_2$pfr364Q2_std
# added an extra missing value for both

# build off that variable to now make a variable that censors for pf CT values >38 and other replicate missing and rename to pfr364Q_std_censored_v2
final_results$pfr364Q1_std_censored_v2 = ifelse(final_results$pfr364CT1 >= 38 & is.na(final_results$pfr364CT2),NA,final_results$pfr364Q1_std_censored)
final_results$pfr364Q2_std_censored_v2 = ifelse(final_results$pfr364CT2 >= 38 & is.na(final_results$pfr364CT1),NA,final_results$pfr364Q2_std_censored)
summary(final_results$pfr364Q1_std_censored_v2) # 1928 missing
summary(final_results$pfr364Q2_std_censored_v2) # 1919 missing
# check the output one more time
length(which(final_results$pfr364CT1 >= 38 & is.na(final_results$pfr364CT2))) # 0 observations
length(which(final_results$pfr364CT2 >= 38 & is.na(final_results$pfr364CT1))) # 1 observations
# look at the original data sets with this criteria
test1 = final_results[which(final_results$pfr364CT1 >= 38 & is.na(final_results$pfr364CT2)),]
test2 = final_results[which(final_results$pfr364CT2 >= 38 & is.na(final_results$pfr364CT1)),]

# create a variable that indicates whether the sample is positive or negative for Pf malaria infection
# if at least 1 duplicate has parasitemia > 0 after criteria enforced (ie. in pfr364Q_std_censored_v2 variable), then saying sample is positive
# 1 is positive, 0 negative
pf_infection_status = rep(NA,nrow(final_results))
for (i in 1:nrow(final_results)){
  if (!(is.na(final_results$r_value_std[i])) & (is.na(final_results$pfr364Q1_std_censored_v2[i]) & is.na(final_results$pfr364Q2_std_censored_v2[i]))){
    pf_infection_status[i] = 0
  } else if ((final_results$pfr364Q1_std_censored_v2[i] > 0 | final_results$pfr364Q2_std_censored_v2[i] > 0) & !(is.na(final_results$r_value_std[i])) & (!(is.na(final_results$pfr364Q1_std_censored_v2[i])) | !(is.na(final_results$pfr364Q2_std_censored_v2[i])))){
    pf_infection_status[i] = 1
  } else {
    pf_infection_status[i] = NA
  }
}
table(pf_infection_status,useNA = "always")
# check the output
length(which(final_results$pfr364Q1_std_censored_v2 > 0 | final_results$pfr364Q2_std_censored_v2 > 0))
length(which(!(is.na(final_results$r_value_std)))) - length(which(final_results$pfr364Q1_std_censored_v2 > 0 | final_results$pfr364Q2_std_censored_v2 > 0))
length(which((is.na(final_results$r_value_std))))
# make a factor
final_results$pf_pcr_infection_status = factor(pf_infection_status,levels = c(0,1), labels = c("negative", "positive"))
# look at the output
table(final_results$pf_pcr_infection_status,useNA = "always")

# create a combined standardized new variable (combining Q1 and Q2) that was standardized from the 8 standards (1-2000 p/uL)
# if both positive parasitemia for both, then is an average of the two parasitemia for the duplicates.
# if only 1 positive parasitemia then is the value of the positive parasitemia
pfr364Q_std_combined = rep(NA,nrow(final_results))
for (k in 1:nrow(final_results)){
  if (final_results$pf_pcr_infection_status[k] == "positive" & !(is.na(final_results$pf_pcr_infection_status[k]))){
    pfr364Q_std_combined[k] = (sum(final_results$pfr364Q1_std_censored_v2[k],final_results$pfr364Q2_std_censored_v2[k],na.rm = T))/(2-(is.na(final_results$pfr364Q1_std_censored_v2[k])+is.na(final_results$pfr364Q2_std_censored_v2[k])))
  } else
    pfr364Q_std_combined[k] = NA
}
summary(pfr364Q_std_combined,useNA = "always")
# add to the data set
final_results$pfr364Q_std_combined = pfr364Q_std_combined
# check the output
summary(final_results$pfr364Q1_std)
summary(final_results$pfr364Q2_std)
summary(final_results$pfr364Q_std_combined)
checkdata = final_results[which(!(is.na(final_results$pfr364Q_std_combined))),]
head(checkdata$HbtubCT1)
head(checkdata$HbtubCT2)
head(checkdata$pfr364CT1)
head(checkdata$pfr364CT2)
head(checkdata$pfr364Q1_std_censored_v2)
head(checkdata$pfr364Q2_std_censored_v2)
head(checkdata$pf_pcr_infection_status)
head(checkdata$pfr364Q_std_combined)

# final check through all the data processing for the qPCR data
summary(checkdata$HbtubCT1)
summary(checkdata$HbtubCT2)
summary(checkdata$pfr364CT1)
summary(checkdata$pfr364CT2)
summary(final_results$pfr364Q_std_combined)
table(final_results$pf_pcr_infection_status, useNA = "always")

# final check for samples to change to missing
orig_zeroes_1 = qpcr_data[which(qpcr_data$pfr364CT1 == 0 | qpcr_data$HbtubCT1 == 0),]
orig_zeroes_2 = qpcr_data[which(qpcr_data$pfr364CT2 == 0 | qpcr_data$HbtubCT2 == 0),]
# pull out the labid_new for the samples with CT values of 0 for Pf
orig_zeroes_1_labid = orig_zeroes_1$`Sample Name`
orig_zeroes_2_labid = orig_zeroes_2$`Sample Name`
# subset the standardized parasitemia data set to look at these labid values
subset1 = final_results[which(final_results$`Sample Name` %in% orig_zeroes_1_labid),]
subset2 = final_results[which(final_results$`Sample Name` %in% orig_zeroes_2_labid),]
# none had zeroes for all Hb CT values



#### ------- clean the qpcr data set ---------- ####

# now rename the final_results data set back to qpcr_data and clean it
qpcr_data = final_results

# look at all the column names
colnames(qpcr_data)

# Sample Name
table(qpcr_data$`Sample Name`, useNA = "always")
str(qpcr_data$`Sample Name`)
# check for duplicate sample names
length(unique(qpcr_data$`Sample Name`)) # 2733 unique 
length(which(is.na(qpcr_data$`Sample Name`) == T)) # 0 missing
count_table = table(qpcr_data$`Sample Name`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 8 duplicates
# pull out those that are duplicates
# look where these duplicates occurred
dup_data = qpcr_data[which(qpcr_data$`Sample Name` %in% names(dups_table)),]
# only go with the earlier plate number result
# K01-070219-7 - both negative so delete second entry
dup_data$pf_pcr_infection_status[which(dup_data$`Sample Name`=="K01-070219-7")]
qpcr_data = qpcr_data[-which(qpcr_data$`Experiment Name` == "MozP2 DBS B21-B22 Taqman duplex 11-8-19" & qpcr_data$`Sample Name` == "K01-070219-7"),]
# M01-140219-7
dup_data$pf_pcr_infection_status[which(dup_data$`Sample Name`=="M01-140219-7")]
qpcr_data = qpcr_data[-which(qpcr_data$`Well Position` == "K1" & qpcr_data$`Sample Name` == "M01-140219-7"),]
# M03-110419-2
dup_data$pf_pcr_infection_status[which(dup_data$`Sample Name`=="M03-110419-2")]
qpcr_data = qpcr_data[-which(qpcr_data$`Well Position` == "C5" & qpcr_data$`Sample Name` == "M03-110419-2"),]
# M13-090818-12
dup_data$pf_pcr_infection_status[which(dup_data$`Sample Name`=="M13-090818-12")]
qpcr_data = qpcr_data[-which(qpcr_data$`Well Position` == "M5" & qpcr_data$`Sample Name` == "M13-090818-12"),]
# M13-110419-1
dup_data$pf_pcr_infection_status[which(dup_data$`Sample Name`=="M13-110419-1")]
qpcr_data = qpcr_data[-which(qpcr_data$`Well Position` == "B15" & qpcr_data$`Sample Name` == "M13-110419-1"),]
# M13-111018-2
dup_data$pf_pcr_infection_status[which(dup_data$`Sample Name`=="M13-111018-2")]
qpcr_data = qpcr_data[-which(qpcr_data$`Well Position` == "G21" & qpcr_data$`Sample Name` == "M13-111018-2"),]
# S04-210319-1
dup_data$pf_pcr_infection_status[which(dup_data$`Sample Name`=="S04-210319-1")]
qpcr_data = qpcr_data[-which(qpcr_data$`Well Position` == "L13" & qpcr_data$`Sample Name` == "S04-210319-1"),]
# S09-240119-4
dup_data$pf_pcr_infection_status[which(dup_data$`Sample Name`=="S09-240119-4")]
qpcr_data = qpcr_data[-which(qpcr_data$`Well Position` == "F9" & qpcr_data$`Sample Name` == "S09-240119-4"),]
# recheck for duplicate sample names
length(unique(qpcr_data$`Sample Name`)) # 2733 unique 
length(which(is.na(qpcr_data$`Sample Name`) == T)) # 0 missing
count_table = table(qpcr_data$`Sample Name`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates left

# also need to change values that have a hb CT value NA from negative to missing in pf_pcr_infection_status
# do this for labid_new observations that have both Hb CT values as NA
hbctbothmissing = qpcr_data[which(is.na(qpcr_data$HbtubCT1) & is.na(qpcr_data$HbtubCT2) & !(is.na(qpcr_data$r_value_std))),]
length(which(is.na(qpcr_data$HbtubCT1) & is.na(qpcr_data$HbtubCT2))) # we had 7 where both the Hbtub values were missing
# looks like we had 7 with all 0 CT values missing for Hb after duplicates removed
# change these to have a missing pf infection status
table(qpcr_data$pf_pcr_infection_status, useNA = 'always')
qpcr_data$pf_pcr_infection_status[which(qpcr_data$`Sample Name` %in% hbctbothmissing$`Sample Name`)] = NA
table(qpcr_data$pf_pcr_infection_status, useNA = 'always')
# change these to ahve a missing parasite density measure to
summary(qpcr_data$pfr364Q_std_combined)
qpcr_data$pfr364Q_std_combined[which(qpcr_data$`Sample Name` %in% hbctbothmissing$`Sample Name`)] = NA
summary(qpcr_data$pfr364Q_std_combined)

# now clean up the lab id for Sample Name
table(qpcr_data$`Sample Name`, useNA = "always")
table(nchar(qpcr_data$`Sample Name`), useNA = "always")
length(which(is.na(qpcr_data$`Sample Name`))) # 0 missing IDs
# most sample names are 12 characters long but range from 11 to 15
# clean up the Sample Names
clean_sample_id = rep(NA,nrow(qpcr_data))
for (i in 1:nrow(qpcr_data)){
  if (nchar(qpcr_data$`Sample Name`[i]) == 11 & !(is.na(qpcr_data$`Sample Name`[i]))){
    parts = strsplit(qpcr_data$`Sample Name`[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[1],"-","0",parts[2],"-",parts[3]))
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 12 & !(is.na(qpcr_data$`Sample Name`[i]))){
    clean_sample_id[i] = toupper(qpcr_data$`Sample Name`[i])
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 13 & str_count(qpcr_data$`Sample Name`[i], "-") == 2 & !(is.na(qpcr_data$`Sample Name`[i])) & !(str_detect(qpcr_data$`Sample Name`[i],"2019"))){
    clean_sample_id[i] = toupper(qpcr_data$`Sample Name`[i])
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 13 & str_count(qpcr_data$`Sample Name`[i], "-") == 2 & !(is.na(qpcr_data$`Sample Name`[i])) & (str_detect(qpcr_data$`Sample Name`[i],"2019"))){
    parts = strsplit(qpcr_data$`Sample Name`[i],"-")[[1]]
    middle1 = strsplit(parts[2],"")[[1]]
    middle = paste0("0",middle1[1],middle1[2],middle1[3],middle1[6],middle1[7])
    clean_sample_id[i] = toupper(paste0(parts[1],"-",middle,"-",parts[3]))
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 13 & str_count(qpcr_data$`Sample Name`[i], "-") == 3 & !(is.na(qpcr_data$`Sample Name`[i])) & !(str_detect(qpcr_data$`Sample Name`[i],"2019"))){
    parts = strsplit(qpcr_data$`Sample Name`[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[3],"-","0",parts[2],"-",parts[4],"-",parts[1]))
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 14 & str_count(qpcr_data$`Sample Name`[i], "-") == 2 & !(is.na(qpcr_data$`Sample Name`[i])) & (str_detect(qpcr_data$`Sample Name`[i],"2019"))){
    parts = strsplit(qpcr_data$`Sample Name`[i],"-")[[1]]
    if (nchar(parts[3])==1){
      middle1 = strsplit(parts[2],"")[[1]]
      middle = paste0(middle1[1],middle1[2],middle1[3],middle1[4],middle1[7],middle1[8])
      clean_sample_id[i] = toupper(paste0(parts[1],"-",middle,"-",parts[3]))
    }
    if (nchar(parts[3])==2){
      middle1 = strsplit(parts[2],"")[[1]]
      middle = paste0("0",middle1[1],middle1[2],middle1[3],middle1[6],middle1[7])
      clean_sample_id[i] = toupper(paste0(parts[1],"-",middle,"-",parts[3]))
    }
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 14 & !(is.na(qpcr_data$`Sample Name`[i])) & str_count(qpcr_data$`Sample Name`[i],"-") == 3){
    parts = strsplit(qpcr_data$`Sample Name`[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[3],"-",parts[2],"-",parts[4],"-",parts[1]))
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 14 & !(is.na(qpcr_data$`Sample Name`[i])) & str_count(qpcr_data$`Sample Name`[i],"-") == 2){
    clean_sample_id[i] = toupper(clean_sample_id[i])
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 15 & !(is.na(qpcr_data$`Sample Name`[i])) & str_count(qpcr_data$`Sample Name`[i],"-") == 3){
    parts = strsplit(qpcr_data$`Sample Name`[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[3],"-",parts[2],"-",parts[4],"-",parts[1]))
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 15 & !(is.na(qpcr_data$`Sample Name`[i])) & str_count(qpcr_data$`Sample Name`[i],"-") == 2){
    parts = strsplit(qpcr_data$`Sample Name`[i],"-")[[1]]
    middle1 = strsplit(parts[2],"")[[1]]
    middle = paste0(middle1[1],middle1[2],middle1[3],middle1[4],middle1[7],middle1[8])
    clean_sample_id[i] = toupper(paste0(parts[1],"-",middle,"-",parts[3]))
  }
}
# check the recode
table(clean_sample_id, useNA = "always")
length(which(is.na(clean_sample_id))) # 0 missing
summary(nchar(clean_sample_id))
# add the clean Sample Name to the data set
qpcr_data$clean_sample_id = clean_sample_id # check 	M01-15001-1
qpcr_data %>%
  select(`Sample Name`,clean_sample_id) %>%
  View()
qpcr_data$`Sample Name` = clean_sample_id
qpcr_data$clean_sample_id <- NULL


# Experiment Name
table(qpcr_data$`Experiment Name`, useNA = "always")
str(qpcr_data$`Experiment Name`)
# looks good, clean

# Well Position
table(qpcr_data$`Well Position`, useNA = "always")
str(qpcr_data$`Well Position`)
# looks good, clean

# HbtubCT1 & HbtubCT2
table(qpcr_data$HbtubCT1, useNA = "always")
table(qpcr_data$HbtubCT2, useNA = "always")
# some changes need to be made, making below with the Pf CT values

# Pfr364CT1 & Pfr364CT2
table(qpcr_data$pfr364CT1, useNA = "always")
table(qpcr_data$pfr364CT2, useNA = "always")
# some changes need to be made, making below with the Hb CT values

# make sure the qpcr values are numeric and change "Undetermined" to NA to represent missing because undetectable
# make sure all the qpcr values with CT values ==0  are also changed to NA
# first check how many are 0, undetermined, and missing
length(which(qpcr_data$HbtubCT1 == "Undetermined" | qpcr_data$HbtubCT1 == "0" | qpcr_data$HbtubCT1 == "0.000")) # 0
length(which(qpcr_data$HbtubCT2 == "Undetermined" | qpcr_data$HbtubCT2 == "0" | qpcr_data$HbtubCT2 == "0.000")) # 0
length(which(qpcr_data$pfr364CT1 == "Undetermined" | qpcr_data$pfr364CT1 == "0" | qpcr_data$pfr364CT1 == "0.000")) # 0
length(which(qpcr_data$pfr364CT2 == "Undetermined" | qpcr_data$pfr364CT2 == "0" | qpcr_data$pfr364CT2 == "0.000")) # 0
length(which(is.na(qpcr_data$HbtubCT1))) # 28
length(which(is.na(qpcr_data$HbtubCT2))) # 23
length(which(is.na(qpcr_data$pfr364CT1))) # 1920
length(which(is.na(qpcr_data$pfr364CT2))) # 1909
# still looks good

# make sure all the CT variables are numeric
qpcr_data$HbtubCT1 = as.numeric(qpcr_data$HbtubCT1)
qpcr_data$HbtubCT2 = as.numeric(qpcr_data$HbtubCT2)
qpcr_data$pfr364CT1 = as.numeric(qpcr_data$pfr364CT1)
qpcr_data$pfr364CT2 = as.numeric(qpcr_data$pfr364CT2)

# look at final summaries of the CT variables
summary(qpcr_data$HbtubCT1)
summary(qpcr_data$HbtubCT2)
summary(qpcr_data$pfr364CT1)
summary(qpcr_data$pfr364CT2)

# pfr364Y-Intercept
summary(qpcr_data$`pfr364Y-Intercept`)
str(qpcr_data$`pfr364Y-Intercept`)
# looks good, clean
# no longer need because have standardized one
qpcr_data$`pfr364Y-Intercept` <- NULL

# pfr364R_
summary(qpcr_data$pfr364R_)
str(qpcr_data$pfr364R_)
# look good, clean
# no longer need because have standardized one
qpcr_data$pfr364R_ <- NULL

# pfr364Slope
summary(qpcr_data$pfr364Slope)
str(qpcr_data$pfr364Slope)
# looks good, clean
# no longer need because have standardized one
qpcr_data$pfr364Slope <- NULL

# Hb standards
summary(qpcr_data$HbtubStd1a)
summary(qpcr_data$HbtubStd1b)
summary(qpcr_data$HbtubStd2a)
summary(qpcr_data$HbtubStd2b)
summary(qpcr_data$HbtubStd3a)
summary(qpcr_data$HbtubStd3b)
summary(qpcr_data$HbtubStd4a)
summary(qpcr_data$HbtubStd4b)
summary(qpcr_data$HbtubStd5a)
summary(qpcr_data$HbtubStd5b)
summary(qpcr_data$HbtubStd6a)
summary(qpcr_data$HbtubStd6b)
summary(qpcr_data$HbtubStd7a)
summary(qpcr_data$HbtubStd7b)
summary(qpcr_data$HbtubStd8a)
summary(qpcr_data$HbtubStd8b)
summary(qpcr_data$HbtubStd9a)
summary(qpcr_data$HbtubStd9b)
summary(qpcr_data$HbtubStd10a)
summary(qpcr_data$HbtubStd10b)

# Pf standards
summary(qpcr_data$pfr364Std1a)
summary(qpcr_data$pfr364Std1b)
summary(qpcr_data$pfr364Std2a)
summary(qpcr_data$pfr364Std2b)
summary(qpcr_data$pfr364Std3a)
summary(qpcr_data$pfr364Std3b)
summary(qpcr_data$pfr364Std4a)
summary(qpcr_data$pfr364Std4b)
summary(qpcr_data$pfr364Std5a)
summary(qpcr_data$pfr364Std5b)
summary(qpcr_data$pfr364Std6a)
summary(qpcr_data$pfr364Std6b)
summary(qpcr_data$pfr364Std7a)
summary(qpcr_data$pfr364Std7b)
summary(as.numeric(qpcr_data$pfr364Std8a))
summary(as.numeric(qpcr_data$pfr364Std8b))
summary(qpcr_data$pfr364Std9a)
summary(qpcr_data$pfr364Std9b)
summary(qpcr_data$pfr364Std10a)
summary(as.numeric(qpcr_data$pfr364Std10b))

# model_name
table(qpcr_data$model_name, useNA = "always")
str(qpcr_data$model_name)
# don't need anymore, remove
qpcr_data$model_name <- NULL

# r_value_std
summary(qpcr_data$r_value_std)
str(qpcr_data$r_value_std)
# looks good, high R2 values now

# intercept_std
summary(qpcr_data$intercept_std)
str(qpcr_data$intercept_std)
# looks good

# slope_std
summary(qpcr_data$slope_std)
str(qpcr_data$slope_std)
# looks good

# pfr364Q1_std
summary(qpcr_data$pfr364Q1_std)
str(qpcr_data$pfr364Q1_std)
# don't need anymore, remove
qpcr_data$pfr364Q1_std <- NULL

# pfr364Q2_std
summary(qpcr_data$pfr364Q2_std)
str(qpcr_data$pfr364Q2_std)
# don't need anymore, remove
qpcr_data$pfr364Q2_std <- NULL

# pfr364Q1_std_censored
summary(qpcr_data$pfr364Q1_std_censored)
str(qpcr_data$pfr364Q1_std_censored)
# don't need anymore, remove
qpcr_data$pfr364Q1_std_censored <- NULL

# pfr364Q2_std_censored
summary(qpcr_data$pfr364Q2_std_censored)
str(qpcr_data$pfr364Q2_std_censored)
# don't need anymore, remove
qpcr_data$pfr364Q2_std_censored <- NULL

# pfr364Q1_std_censored_v2
summary(qpcr_data$pfr364Q1_std_censored_v2)
str(qpcr_data$pfr364Q1_std_censored_v2)
# rename the variable to pfr364Q1_std
colnames(qpcr_data)[colnames(qpcr_data) == 'pfr364Q1_std_censored_v2'] <- 'pfr364Q1_std'

# pfr364Q2_std_censored_v2
summary(qpcr_data$pfr364Q2_std_censored_v2)
str(qpcr_data$pfr364Q2_std_censored_v2)
# rename the variable to pfr364Q2_std
colnames(qpcr_data)[colnames(qpcr_data) == 'pfr364Q2_std_censored_v2'] <- 'pfr364Q2_std'

# pf_pcr_infection_status
table(qpcr_data$pf_pcr_infection_status, useNA = "always")
str(qpcr_data$pf_pcr_infection_status)
# looks good, clean

# pfr364Q_std_combined
summary(qpcr_data$pfr364Q_std_combined)
str(qpcr_data$pfr364Q_std_combined)
# looks good, clean


# export the data set as CSV and RDS files
write_csv(qpcr_data, "Desktop/phase2_spat21_qpcr_data_clean_human_dbs_19NOV2019.csv")
write_rds(qpcr_data, "Desktop/phase2_spat21_qpcr_data_clean_human_dbs_19NOV2019.RDS")
