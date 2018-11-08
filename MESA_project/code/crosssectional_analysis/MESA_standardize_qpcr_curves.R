# ----------------------------------------- #
#       MESA Standardize qPCR curves        #
#            November 7, 2018               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)


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
model1 = glm(as.numeric(unlist(qpcr_plates_table[1,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[1,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model2 = glm(as.numeric(unlist(qpcr_plates_table[2,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[2,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model3 = glm(as.numeric(unlist(qpcr_plates_table[3,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[3,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model4 = glm(as.numeric(unlist(qpcr_plates_table[4,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[4,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model5 = glm(as.numeric(unlist(qpcr_plates_table[5,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[5,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model6 = glm(as.numeric(unlist(qpcr_plates_table[6,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[6,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model7 = glm(as.numeric(unlist(qpcr_plates_table[7,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[7,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model8 = glm(as.numeric(unlist(qpcr_plates_table[8,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[8,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model9 = glm(as.numeric(unlist(qpcr_plates_table[9,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[9,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model10 = glm(as.numeric(unlist(qpcr_plates_table[10,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[10,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model11 = glm(as.numeric(unlist(qpcr_plates_table[11,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[11,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model12 = glm(as.numeric(unlist(qpcr_plates_table[12,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[12,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model13 = glm(as.numeric(unlist(qpcr_plates_table[13,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[13,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model14 = glm(as.numeric(unlist(qpcr_plates_table[14,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[14,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model15 = glm(as.numeric(unlist(qpcr_plates_table[15,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[15,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model16 = glm(as.numeric(unlist(qpcr_plates_table[16,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[16,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model17 = glm(as.numeric(unlist(qpcr_plates_table[17,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[17,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model18 = glm(as.numeric(unlist(qpcr_plates_table[18,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[18,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model19 = glm(as.numeric(unlist(qpcr_plates_table[19,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[19,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model20 = glm(as.numeric(unlist(qpcr_plates_table[20,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[20,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model21 = glm(as.numeric(unlist(qpcr_plates_table[21,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[21,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model22 = glm(as.numeric(unlist(qpcr_plates_table[22,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[22,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model23 = glm(as.numeric(unlist(qpcr_plates_table[23,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[23,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model24 = glm(as.numeric(unlist(qpcr_plates_table[24,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[24,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model25 = glm(as.numeric(unlist(qpcr_plates_table[25,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[25,18:33]))), data=qpcr_plates_table, gaussian("identity"))
model26 = glm(as.numeric(unlist(qpcr_plates_table[26,2:17])) ~ log(as.numeric(unlist(qpcr_plates_table[26,18:33]))), data=qpcr_plates_table, gaussian("identity"))




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
qpcr_plates_table$Std9a_x = 0.02
qpcr_plates_table$Std9b_x = 0.02
qpcr_plates_table$Std10a_x = 0.01
qpcr_plates_table$Std10b_x = 0.01

# recode everything labeled "undetermined" as NA
qpcr_plates_table[qpcr_plates_table == "Undetermined"] = NA

# run a linear regression model with the concentrations as the x values and ct values as the y value for the standards
# run this model for each plate
model1 = glm(as.numeric(unlist(qpcr_plates_table[1,2:21])) ~ log(as.numeric(unlist(qpcr_plates_table[1,22:41]))), data=qpcr_plates_table, gaussian("identity"))
summary(model1)
plot(log(as.numeric(unlist(qpcr_plates_table[1,22:41]))),as.numeric(unlist(qpcr_plates_table[1,2:21])))
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



