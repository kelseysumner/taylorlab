# ----------------------------------------- #
#            MESA Visualizations            #
#            Haplotypes Over Time           #
#             November 6, 2018              #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(ggplot2)
library(readr)
library(dplyr)
library(gridExtra) 
library(ggthemes)
library(wesanderson)
library(tidyr)
library(lubridate)


#### ----- creating the figure of haplotype presence over time ----- ####

## for CSP

# read in the csp haplotype merged data set you made for Cody
csp_data = haplotype_merge_CSP

# read in the merged data set
merged_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/clean_files_for_lab/mesa_merged_final.csv")

# merge in the interview dates from the merged_data set
csp_merged = left_join(csp_data,merged_data,by="labid_old_labinventory")

# only keep the first 95 columns
csp_merged = csp_merged[,1:95]

# create a new variable that is just the month
str(csp_merged$interview_date)
csp_merged$interview_date = dmy(csp_merged$interview_date)
str(csp_merged$interview_date)
csp_merged$month = paste0(month(csp_merged$interview_date),"-",year(csp_merged$interview_date))
table(csp_merged$month, useNA = "always")

# only keep the first 88 columns, location, and month
csp_merged = csp_merged[,c(1:88,93,96)]

# create a data frame summarizing each haplotype and the months it is present
# trying gathering the code to long format
long_csp_merged = gather(data=csp_merged, H, starts_with("H"), -month, -location.x)

# rename columns in csp_merged long file
names(long_csp_merged)[names(long_csp_merged) == 'starts_with(\"H\")'] <- 'reads_present'
names(long_csp_merged)[names(long_csp_merged) == 'H'] <- 'haplotypes'
names(long_csp_merged)[names(long_csp_merged) == 'location.x'] <- 'location'

# remove all rows with reads_present equal to 0
long_csp_merged = long_csp_merged[-which(long_csp_merged$reads_present == 0),]

# summarize the new data set by month
month_summary = long_csp_merged %>% 
  group_by(month,haplotypes) %>%
  summarize(n_samples=n())

# summarize the new data set by location
location_summary = long_csp_merged %>%
  group_by(month,haplotypes,location) %>%
  summarise(n_1 = n_distinct(location)) %>%
  select(month,haplotypes,n_1) %>%
  summarise(n_villages=sum(n_1,na.rm=T))

# check the output
length(which(csp_merged$month == "1-2014" & csp_merged$H1 > 0))
length(which(csp_merged$month == "1-2014" & csp_merged$H10 > 0))
length(which(csp_merged$month == "1-2014" & csp_merged$H11 > 0))
unique(csp_merged[which(csp_merged$month == "1-2014" & csp_merged$H1 > 0),c("location.x")])
unique(csp_merged[which(csp_merged$month == "1-2014" & csp_merged$H10 > 0),c("location.x")])
unique(csp_merged[which(csp_merged$month == "1-2014" & csp_merged$H3 > 0),c("location.x")])

# merge the month and location summaries
merged_summary = left_join(month_summary,location_summary,by=c("month","haplotypes"))

# set order for x-axis for months
table(merged_summary$month, useNA = "always")
month_order = c("4-2013","5-2013","6-2013","7-2013","8-2013","9-2013","10-2013","11-2013","12-2013","1-2014","2-2014","3-2014",
                "4-2014","5-2014","6-2014")
merged_summary <- within(merged_summary, month <- factor(month, levels=month_order))

# set order for y-axis based on how many months each haplotype is present
months_hap_present_summary = long_csp_merged %>%
  group_by(haplotypes,month) %>%
  summarise(n_present_1 = n_distinct(month)) %>%
  select(haplotypes,n_present_1) %>%
  summarise(n_present = sum(n_present_1,na.rm=T))
haplotype_order = months_hap_present_summary[order(months_hap_present_summary$n_present),]
merged_summary <- within(merged_summary, haplotypes <- factor(haplotypes, levels=haplotype_order$haplotypes))

# create a plot of the presence and abundance of each haplotype over time
csp_month_plot = ggplot(merged_summary, aes(x=month, y=haplotypes, size=n_samples, color=n_villages)) +
  geom_point() +
  labs(title = "Pfcsp haplotype presence and abundance over time across MESA samples (1/1/18 sequencing run)",
       x = "Month and year",y="Haplotype", color = "Number of villages", size = "Number of samples")
csp_month_plot

ggsave(csp_month_plot, filename="/Users/kelseysumner/Desktop/csp_month_plot.png", device="png",
       height=14, width=11.2, units="in", dpi=500)


# ---------- #

## for AMA

# read in the csp haplotype merged data set you made for Cody
ama_data = haplotype_merge_AMA

# read in the merged data set
merged_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/clean_files_for_lab/mesa_merged_final.csv")

# merge in the interview dates from the merged_data set
ama_merged = left_join(ama_data,merged_data,by="labid_old_labinventory")

# only keep the first 121 columns
ama_merged = ama_merged[,1:121]

# create a new variable that is just the month
str(ama_merged$interview_date)
ama_merged$interview_date = dmy(ama_merged$interview_date)
str(ama_merged$interview_date)
ama_merged$month = paste0(month(ama_merged$interview_date),"-",year(ama_merged$interview_date))
table(ama_merged$month, useNA = "always")

# only keep the first 88 columns, location, and month
ama_merged = ama_merged[,c(1:114,119,122)]

# create a data frame summarizing each haplotype and the months it is present
# trying gathering the code to long format
long_ama_merged = gather(data=ama_merged, H, starts_with("H"), -month, -location.x)

# rename columns in ama_merged long file
names(long_ama_merged)[names(long_ama_merged) == 'starts_with(\"H\")'] <- 'reads_present'
names(long_ama_merged)[names(long_ama_merged) == 'H'] <- 'haplotypes'
names(long_ama_merged)[names(long_ama_merged) == 'location.x'] <- 'location'

# remove all rows with reads_present equal to 0
long_ama_merged = long_ama_merged[-which(long_ama_merged$reads_present == 0),]

# summarize the new data set by month
month_summary = long_ama_merged %>% 
  group_by(month,haplotypes) %>%
  summarize(n_samples=n())

# summarize the new data set by location
location_summary = long_ama_merged %>%
  group_by(month,haplotypes,location) %>%
  summarise(n_1 = n_distinct(location)) %>%
  select(month,haplotypes,n_1) %>%
  summarise(n_villages=sum(n_1,na.rm=T))

# check the output
length(which(ama_merged$month == "1-2014" & ama_merged$H1 > 0))
length(which(ama_merged$month == "1-2014" & ama_merged$H10 > 0))
length(which(ama_merged$month == "1-2014" & ama_merged$H11 > 0))
unique(ama_merged[which(ama_merged$month == "1-2014" & ama_merged$H1 > 0),c("location.x")])
unique(ama_merged[which(ama_merged$month == "1-2014" & ama_merged$H10 > 0),c("location.x")])
unique(ama_merged[which(ama_merged$month == "1-2014" & ama_merged$H3 > 0),c("location.x")])

# merge the month and location summaries
merged_summary = left_join(month_summary,location_summary,by=c("month","haplotypes"))

# set order for x-axis for months
table(merged_summary$month, useNA = "always")
month_order = c("4-2013","5-2013","6-2013","7-2013","8-2013","9-2013","10-2013","11-2013","12-2013","1-2014","2-2014","3-2014",
                "4-2014","5-2014","6-2014")
merged_summary <- within(merged_summary, month <- factor(month, levels=month_order))

# set order for y-axis based on how many months each haplotype is present
months_hap_present_summary = long_ama_merged %>%
  group_by(haplotypes,month) %>%
  summarise(n_present_1 = n_distinct(month)) %>%
  select(haplotypes,n_present_1) %>%
  summarise(n_present = sum(n_present_1,na.rm=T))
haplotype_order = months_hap_present_summary[order(months_hap_present_summary$n_present),]
merged_summary <- within(merged_summary, haplotypes <- factor(haplotypes, levels=haplotype_order$haplotypes))

# create a plot of the presence and abundance of each haplotype over time
ama_month_plot = ggplot(merged_summary, aes(x=month, y=haplotypes, size=n_samples, color=n_villages)) +
  geom_point() +
  scale_colour_gradient(low = "black", high = "green") +
  labs(title = "Pfama1 haplotype presence and abundance over time across MESA samples (1/1/18 sequencing run)",
       x = "Month and year",y="Haplotype", color = "Number of villages", size = "Number of samples")
ama_month_plot

ggsave(ama_month_plot, filename="/Users/kelseysumner/Desktop/ama_month_plot.png", device="png",
       height=20, width=11.2, units="in", dpi=500)





