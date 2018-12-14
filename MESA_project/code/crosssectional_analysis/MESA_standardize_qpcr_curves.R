# ----------------------------------------- #
#       MESA Standardize qPCR curves        #
#            November 7, 2018               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(broom)
library(tidyr)
library(ggplot2)


#### ------ standardize the qpcr curves ------ ####

# read in the merged data set
merged_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/clean_files_for_lab/mesa_merged_final.csv")

# look at the column names
names(merged_data)

# pull out a table of each of the qpcr plates and their correspond Pf standards
# note: only pulled out standards 1-8 because don't want to use 1-10
qpcr_plates_table = merged_data %>%
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

# recode everything labeled "undetermined" as NA
qpcr_plates_table[qpcr_plates_table == "Undetermined"] = NA

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
model17 = lm(as.numeric(unlist(qpcr_plates_table[17,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[17,18:33]))), data=qpcr_plates_table)
model18 = lm(as.numeric(unlist(qpcr_plates_table[18,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[18,18:33]))), data=qpcr_plates_table)
model19 = lm(as.numeric(unlist(qpcr_plates_table[19,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[19,18:33]))), data=qpcr_plates_table)
model20 = lm(as.numeric(unlist(qpcr_plates_table[20,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[20,18:33]))), data=qpcr_plates_table)
model21 = lm(as.numeric(unlist(qpcr_plates_table[21,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[21,18:33]))), data=qpcr_plates_table)
model22 = lm(as.numeric(unlist(qpcr_plates_table[22,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[22,18:33]))), data=qpcr_plates_table)
model23 = lm(as.numeric(unlist(qpcr_plates_table[23,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[23,18:33]))), data=qpcr_plates_table)
model24 = lm(as.numeric(unlist(qpcr_plates_table[24,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[24,18:33]))), data=qpcr_plates_table)
model25 = lm(as.numeric(unlist(qpcr_plates_table[25,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[25,18:33]))), data=qpcr_plates_table)
model26 = lm(as.numeric(unlist(qpcr_plates_table[26,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[26,18:33]))), data=qpcr_plates_table)

# combine the models in a data frame
m1_df = bind_cols(tidy(model1), confint_tidy(model1)) %>% mutate(model_name="M1: plate27-28") %>% mutate(r_value = summary(model1)$r.squared)
m2_df = bind_cols(tidy(model2), confint_tidy(model2)) %>% mutate(model_name="M2: plate49-50") %>% mutate(r_value = summary(model2)$r.squared)
m3_df = bind_cols(tidy(model3), confint_tidy(model3)) %>% mutate(model_name="M3: plate23-24") %>% mutate(r_value = summary(model3)$r.squared)
m4_df = bind_cols(tidy(model4), confint_tidy(model4)) %>% mutate(model_name="M4: plate25-26") %>% mutate(r_value = summary(model4)$r.squared)
m5_df = bind_cols(tidy(model5), confint_tidy(model5)) %>% mutate(model_name="M5: plate47-48") %>% mutate(r_value = summary(model5)$r.squared)
m6_df = bind_cols(tidy(model6), confint_tidy(model6)) %>% mutate(model_name="M6: plate7-8") %>% mutate(r_value = summary(model6)$r.squared)
m7_df = bind_cols(tidy(model7), confint_tidy(model7)) %>% mutate(model_name="M7: plate3-4") %>% mutate(r_value = summary(model7)$r.squared)
m8_df = bind_cols(tidy(model8), confint_tidy(model8)) %>% mutate(model_name="M8: plate45-46") %>% mutate(r_value = summary(model8)$r.squared)
m9_df = bind_cols(tidy(model9), confint_tidy(model9)) %>% mutate(model_name="M9: plate11-12") %>% mutate(r_value = summary(model9)$r.squared)
m10_df = bind_cols(tidy(model10), confint_tidy(model10)) %>% mutate(model_name="M10: plate43-44") %>% mutate(r_value = summary(model10)$r.squared)
m11_df = bind_cols(tidy(model11), confint_tidy(model11)) %>% mutate(model_name="M11: plate15-16") %>% mutate(r_value = summary(model11)$r.squared)
m12_df = bind_cols(tidy(model12), confint_tidy(model12)) %>% mutate(model_name="M12: plate17-18") %>% mutate(r_value = summary(model12)$r.squared)
m13_df = bind_cols(tidy(model13), confint_tidy(model13)) %>% mutate(model_name="M13: plate5-6") %>% mutate(r_value = summary(model13)$r.squared)
m14_df = bind_cols(tidy(model14), confint_tidy(model14)) %>% mutate(model_name="M14: plate1-2") %>% mutate(r_value = summary(model14)$r.squared)
m15_df = bind_cols(tidy(model15), confint_tidy(model15)) %>% mutate(model_name="M15: plate21-22") %>% mutate(r_value = summary(model15)$r.squared)
m16_df = bind_cols(tidy(model16), confint_tidy(model16)) %>% mutate(model_name="M16: plate19-20") %>% mutate(r_value = summary(model16)$r.squared)
m17_df = bind_cols(tidy(model17), confint_tidy(model17)) %>% mutate(model_name="M17: plate29-30") %>% mutate(r_value = summary(model17)$r.squared)
m18_df = bind_cols(tidy(model18), confint_tidy(model18)) %>% mutate(model_name="M18: plate31-32") %>% mutate(r_value = summary(model18)$r.squared)
m19_df = bind_cols(tidy(model19), confint_tidy(model19)) %>% mutate(model_name="M19: plate33-34") %>% mutate(r_value = summary(model19)$r.squared)
m20_df = bind_cols(tidy(model20), confint_tidy(model20)) %>% mutate(model_name="M20: plate39-40") %>% mutate(r_value = summary(model20)$r.squared)
m21_df = bind_cols(tidy(model21), confint_tidy(model21)) %>% mutate(model_name="M21: plate35-36") %>% mutate(r_value = summary(model21)$r.squared)
m22_df = bind_cols(tidy(model22), confint_tidy(model22)) %>% mutate(model_name="M22: plate41-42") %>% mutate(r_value = summary(model22)$r.squared)
m23_df = bind_cols(tidy(model23), confint_tidy(model23)) %>% mutate(model_name="M23: plate37-38") %>% mutate(r_value = summary(model23)$r.squared)
m24_df = bind_cols(tidy(model24), confint_tidy(model24)) %>% mutate(model_name="M24: plate9-10") %>% mutate(r_value = summary(model24)$r.squared)
m25_df = bind_cols(tidy(model25), confint_tidy(model25)) %>% mutate(model_name="M25: plate13-14") %>% mutate(r_value = summary(model25)$r.squared)
m26_df = bind_cols(tidy(model26), confint_tidy(model26)) %>% mutate(model_name="M26: plate51") %>% mutate(r_value = summary(model26)$r.squared)
model_results_df = bind_rows(m1_df,m2_df,m3_df,m4_df,m5_df,m6_df,m7_df,m8_df,m9_df,m10_df,m11_df,m12_df,m13_df,
                             m14_df,m15_df,m16_df,m17_df,m18_df,m19_df,m20_df,m21_df,m22_df,m23_df,m24_df,m25_df,
                             m26_df)

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
model_results_df[34,1] <- "slope"
model_results_df[36,1] <- "slope"
model_results_df[38,1] <- "slope"
model_results_df[40,1] <- "slope"
model_results_df[42,1] <- "slope"
model_results_df[44,1] <- "slope"
model_results_df[46,1] <- "slope"
model_results_df[48,1] <- "slope"
model_results_df[50,1] <- "slope"
model_results_df[52,1] <- "slope"

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
write_csv(model_results_simplified,"model_results_simplified.csv")


#### ------- check the model output --------- ####

# pull out a table of each of the qpcr plates and their correspond Pf standards
# note: only pulled out standards 1-8 because don't want to use 1-10
qpcr_plates_table = merged_data %>%
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

# recode everything labeled "undetermined" as NA
qpcr_plates_table[qpcr_plates_table == "Undetermined"] = NA

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
qpcr_original = merged_data %>%
  filter(!(is.na(`Experiment Name`))) %>%
  group_by(`Experiment Name`) %>%
  select(`Experiment Name`,`pfr364Y-Intercept`,pfr364R_,pfr364Slope)
qpcr_original = unique(qpcr_original)
# look at a plot of the x and y correlation
plot(as.numeric(unlist(qpcr_plates_table[1,22:41])),as.numeric(unlist(qpcr_plates_table[1,2:21])))


#### -------- standardize the qpcr values for each plate --------- #####

# first clear the working directory
# read in the merged data set
merged_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/clean_files_for_lab/mesa_merged_final.csv")
# read in the new model results table for the standards 1-2000 p/uL (with experiment name manually added in in Excel)
model_results = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/qPCR_results/model_results_simplified.csv")

# remove 6, 7 columns from model_results because empty
model_results$X6 <- NULL
model_results$X7 <- NULL

# change the column names for model_results to represent the new standardization
colnames(model_results)[colnames(model_results) == 'r_value'] <- 'r_value_std'
colnames(model_results)[colnames(model_results) == 'intercept'] <- 'intercept_std'
colnames(model_results)[colnames(model_results) == 'slope'] <- 'slope_std'

# now merge in the model_results with the merged data set for MESA
final_qpcr_merge = left_join(merged_data,model_results,by="Experiment Name")

# check the merge
table(final_qpcr_merge$r_value_std, useNA = "always")
table(final_qpcr_merge$pfr364R_, useNA = "always")
table(final_qpcr_merge$intercept_std, useNA = "always")
table(final_qpcr_merge$`pfr364Y-Intercept`,useNA = "always")

# make sure the qpcr values are numeric and change "Undetermined" to 99 to represent undetermined in numeric format
final_qpcr_merge$HbtubCT1[final_qpcr_merge$HbtubCT1 == "Undetermined"] = 99
final_qpcr_merge$HbtubCT2[final_qpcr_merge$HbtubCT2 == "Undetermined"] = 99
final_qpcr_merge$pfr364CT1[final_qpcr_merge$pfr364CT1 == "Undetermined"] = 99
final_qpcr_merge$pfr364CT2[final_qpcr_merge$pfr364CT2 == "Undetermined"] = 99

# make sure the values for the standardization formula are numeric
final_qpcr_merge$pfr364CT1 = as.numeric(final_qpcr_merge$pfr364CT1)
final_qpcr_merge$pfr364CT2 = as.numeric(final_qpcr_merge$pfr364CT2)
final_qpcr_merge$intercept_std = as.numeric(final_qpcr_merge$intercept_std)
final_qpcr_merge$slope_std = as.numeric(final_qpcr_merge$slope_std)
                                            
# create two new columns for each of the replicates that are restandardized to the new slope and intercept for standards 1-2000 p/uL
# will use the columns pfr364CT1 and pfr364CT2 as the observed y-values
# plug in those to y=mx+b equation solved for x -> x = (y-b)/m
pfr364Q1_std = rep(NA,nrow(final_qpcr_merge))
pfr364Q2_std = rep(NA,nrow(final_qpcr_merge))
for (i in 1:nrow(final_qpcr_merge)){
  if (final_qpcr_merge$pfr364CT1[i] == 99 & !(is.na(final_qpcr_merge$pfr364CT1[i]))){
    pfr364Q1_std[i] = 0
  } else {
    pfr364Q1_std[i] = 10^((final_qpcr_merge$pfr364CT1[i] - final_qpcr_merge$intercept_std[i])/final_qpcr_merge$slope_std[i])
  }
}
for (i in 1:nrow(final_qpcr_merge)){
  if (final_qpcr_merge$pfr364CT2[i] == 99 & !(is.na(final_qpcr_merge$pfr364CT2[i]))){
    pfr364Q2_std[i] = 0
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
write_csv(final_qpcr_merge,"final_qpcr_merge.csv")

# test the formula
pfr364Q1_test = rep(NA,nrow(final_qpcr_merge))
for (i in 1:nrow(final_qpcr_merge)){
  if (final_qpcr_merge$pfr364CT1[i] == 99 & !(is.na(final_qpcr_merge$pfr364CT1[i]))){
    pfr364Q1_test[i] = 0
  } else {
    pfr364Q1_test[i] = 10^((final_qpcr_merge$pfr364CT1[i] - final_qpcr_merge$`pfr364Y-Intercept`[i])/final_qpcr_merge$pfr364Slope[i])
  }
}
summary(pfr364Q1_test)
summary(final_qpcr_merge$pfr364Q1)

# looking at where differences occurred
chck_val <- data.frame(newval = pfr364Q1_test,oldval = final_qpcr_merge$pfr364Q1)
chck_val %>% apply(1,function(x){identical(x[1],x[2])})
chck_val %<>% mutate(diff = newval - oldval)
chck_val %>% ggplot(aes(x=newval,y=oldval)) + geom_point()
# looks like the differences occur because of rounding of the values and the formula is working correctly

# look at how Q1 was calculate when there were "undetermineds"
table(final_qpcr_merge$pfr364CT1,useNA = "always")
length(which(final_qpcr_merge$pfr364CT1=="Undetermined"))
undeter = final_qpcr_merge[which(final_qpcr_merge$pfr364CT1=="Undetermined"),]
undeter = undeter[,c("pfr364CT1","pfr364CT2","pfr364Q1","pfr364Q2","pfr364Y-Intercept","pfr364Slope")]


#### ------------ look over standardized Q1 and Q1 for new standardizations ------------- ####

# read back in the final qpcr data set
final_results = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/clean_files_for_lab/qPCR_results/final_qpcr_merge.csv")

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
final_results = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/clean_files_for_lab/qPCR_results/final_qpcr_merge.csv")

# change the 99 pfr364CT values to NA and make numeric
final_results$pfr364CT1 = as.numeric(final_results$pfr364CT1)
final_results$pfr364CT2 = as.numeric(final_results$pfr364CT2)
final_results$pfr364CT1[final_results$pfr364CT1 == 99] = NA
final_results$pfr364CT2[final_results$pfr364CT2 == 99] = NA
summary(final_results$pfr364CT1)
summary(final_results$pfr364CT2)

# change all the 0 pfr364Q parasitemia values to NA as well
final_results$pfr364Q1_std[final_results$pfr364Q1_std == 0] = NA
final_results$pfr364Q2_std[final_results$pfr364Q2_std == 0] = NA
summary(final_results$pfr364Q1_std)
summary(final_results$pfr364Q2_std)

# determine labid_new that have human beta tubulin CT values >= 35 or missing and need to be excluded
hbcriteria_1 = final_results[which((as.numeric(final_results$HbtubCT1) >= 35 | is.na(final_results$HbtubCT1)) 
                                        & !(is.na(final_results$r_value_std))),]
# 9236_2, 2349_4, 0015_C, 9037_1
hbcriteria_2 = final_results[which((as.numeric(final_results$HbtubCT2) >= 35 | is.na(final_results$HbtubCT2)) 
                                        & !(is.na(final_results$r_value_std))),]
# 9236_2, 9349_4, 0015_C, 9484_D

# make a variable that censors for human beta tublin CT values >= 35 or missing
final_results$pfr364Q1_std_censored = ifelse(final_results$labid_new == "9236_2" | final_results$labid_new == "9349_4" | final_results$labid_new == "0015_C" | final_results$labid_new == "9037_1",NA,final_results$pfr364Q1_std)
final_results$pfr364Q2_std_censored = ifelse(final_results$labid_new == "9236_2" | final_results$labid_new == "9349_4" | final_results$labid_new == "0015_C" | final_results$labid_new == "9484_D",NA,final_results$pfr364Q2_std)
summary(final_results$pfr364Q1_std_censored)
summary(final_results$pfr364Q2_std_censored)

# build off that variable to now make a variable that censors for pf CT values >= 38 or missing and rename to pfr364Q_std_censored_v2
final_results$pfr364Q1_std_censored_v2 = ifelse(final_results$pfr364CT1 >= 38,NA,final_results$pfr364Q1_std_censored)
final_results$pfr364Q2_std_censored_v2 = ifelse(final_results$pfr364CT2 >= 38,NA,final_results$pfr364Q2_std_censored)
summary(final_results$pfr364Q1_std_censored_v2)
summary(final_results$pfr364Q2_std_censored_v2)
# check the output
length(which(final_results$pfr364CT1 >= 38))
length(which(final_results$pfr364CT2 >= 38))

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
checkdata = final_results[which(!(is.na(final_results$pfr364Q_std_combined))),]
head(checkdata$HbtubCT1)
head(checkdata$HbtubCT2)
head(checkdata$pfr364CT1)
head(checkdata$pfr364CT2)
head(checkdata$pfr364Q1_std_censored_v2)
head(checkdata$pfr364Q2_std_censored_v2)
head(checkdata$pf_pcr_infection_status)
head(checkdata$pfr364Q_std_combined)
anothercheck = checkdata[,c(135:145)]

# final check through all the data processing for the qPCR data
summary(checkdata$HbtubCT1)
summary(checkdata$HbtubCT2)
summary(checkdata$pfr364CT1)
summary(checkdata$pfr364CT2)

# export the data set as a CSV file
write_csv(final_results,"final_results_13DEC2018.csv")
