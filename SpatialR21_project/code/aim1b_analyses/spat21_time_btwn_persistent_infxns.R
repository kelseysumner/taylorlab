
# --------------------------------------- #
# Look at time between persistent infxns  #
#             Mozzie phase 1              #
#                 Aim 1B                  #
#            July 21, 2020                #
#               K. Sumner                 #
# --------------------------------------- #

# will call repeated infections: "reinfection"

#### ------- load libraries -------- ####
library(tidyverse)



#### ------ read in the data sets ------- ####

# read in the ama data set with the first infection
ama_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/with first infection/ama_data_aim1b_11JUN2020.rds")

# read in the csp data set with the first infection
csp_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/with first infection/csp_data_aim1b_11JUN2020.rds")

#### ------ figure out time between persistent infections ------- ####

# calculate number of days between persistent infections
ama_data = arrange(ama_data,unq_memID,sample_id_date)
csp_data = arrange(csp_data,unq_memID,sample_id_date)


# calculate the time between each infection for each person
# for ama
unq_memID_start_date = ama_data[match(unique(ama_data$unq_memID), ama_data$unq_memID),]
days_btwn_infxns = rep(NA,nrow(ama_data))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(ama_data)){
    if (unq_memID_start_date$unq_memID[i] == ama_data$unq_memID[j]){
      if (ama_data$sample_id_date[j] == unq_memID_start_date$sample_id_date[i]){
        days_btwn_infxns[j] = 0
      } else {
        days_btwn_infxns[j] = ama_data$sample_id_date[j] - ama_data$sample_id_date[j-1]
      }
    }
  }
}
summary(days_btwn_infxns)  
ama_data$days_btwn_infxns = days_btwn_infxns
# for csp
unq_memID_start_date = csp_data[match(unique(csp_data$unq_memID), csp_data$unq_memID),]
days_btwn_infxns = rep(NA,nrow(csp_data))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(csp_data)){
    if (unq_memID_start_date$unq_memID[i] == csp_data$unq_memID[j]){
      if (csp_data$sample_id_date[j] == unq_memID_start_date$sample_id_date[i]){
        days_btwn_infxns[j] = 0
      } else {
        days_btwn_infxns[j] = csp_data$sample_id_date[j] - csp_data$sample_id_date[j-1]
      }
    }
  }
}
summary(days_btwn_infxns)  
csp_data$days_btwn_infxns = days_btwn_infxns


# subset to just infections with persistent haplotypes
ama_persistent_data = ama_data %>% filter(str_detect(haplotype_category,"persistent"))
csp_persistent_data = csp_data %>% filter(str_detect(haplotype_category,"persistent"))


# now make plots of interval in days between infections with persistent haplotypes
# ama
any_persistent_plot = ggplot(data=ama_persistent_data,aes(x=days_btwn_infxns)) +
  geom_density(fill="light blue") +
  theme_bw() +
  xlab("Number of days since previous infection") +
  ggtitle("pfama1: all persistent categories")
any_persistent_plot
ggsave(any_persistent_plot, filename="/Users/kelseysumner/Desktop/ama_any_persistent_plot.png", device="png",
       height=4, width=4, units="in", dpi=400)
# csp
any_persistent_plot = ggplot(data=csp_persistent_data,aes(x=days_btwn_infxns)) +
  geom_density(fill="light grey") +
  theme_bw() +
  xlab("Number of days since previous infection") +
  ggtitle("pcfcsp: all persistent categories")
any_persistent_plot
ggsave(any_persistent_plot, filename="/Users/kelseysumner/Desktop/csp_any_persistent_plot.png", device="png",
       height=4, width=4, units="in", dpi=400)


# now subset to the infections that are only "all persistent"
ama_all_persistent_data = ama_data %>% filter(haplotype_category=="all persistent")
csp_all_persistent_data = csp_data %>% filter(haplotype_category=="all persistent")


# now make plots of interval in days between infections with persistent haplotypes
# ama
all_persistent_plot = ggplot(data=ama_all_persistent_data,aes(x=days_btwn_infxns)) +
  geom_density(fill="light blue") +
  theme_bw() +
  xlab("Number of days since previous infection") +
  ggtitle("pfama1: all persistent category")
all_persistent_plot
ggsave(all_persistent_plot, filename="/Users/kelseysumner/Desktop/ama_all_persistent_plot.png", device="png",
       height=4, width=4, units="in", dpi=400)
# csp
all_persistent_plot = ggplot(data=csp_all_persistent_data,aes(x=days_btwn_infxns)) +
  geom_density(fill="light grey") +
  theme_bw() +
  xlab("Number of days since previous infection") +
  ggtitle("pcfcsp: all persistent category")
all_persistent_plot
ggsave(all_persistent_plot, filename="/Users/kelseysumner/Desktop/csp_all_persistent_plot.png", device="png",
       height=4, width=4, units="in", dpi=400)


# now subset to the infections that are only "all persistent"
ama_all_persistent_data = ama_data %>% filter(haplotype_category=="new and persistent")
csp_all_persistent_data = csp_data %>% filter(haplotype_category=="new and persistent")


# now make plots of interval in days between infections with persistent haplotypes
# ama
all_persistent_plot = ggplot(data=ama_all_persistent_data,aes(x=days_btwn_infxns)) +
  geom_density(fill="light blue") +
  theme_bw() +
  xlab("Number of days since previous infection") +
  ggtitle("pfama1: new and persistent category")
all_persistent_plot
ggsave(all_persistent_plot, filename="/Users/kelseysumner/Desktop/ama_new_and_persistent_plot.png", device="png",
       height=4, width=4, units="in", dpi=400)
# csp
all_persistent_plot = ggplot(data=csp_all_persistent_data,aes(x=days_btwn_infxns)) +
  geom_density(fill="light grey") +
  theme_bw() +
  xlab("Number of days since previous infection") +
  ggtitle("pcfcsp: new and persistent category")
all_persistent_plot
ggsave(all_persistent_plot, filename="/Users/kelseysumner/Desktop/csp_new_and_persistent_plot.png", device="png",
       height=4, width=4, units="in", dpi=400)



# now subset to the infections that are only "all persistent"
ama_all_persistent_data = ama_data %>% filter(haplotype_category=="recurrent and persistent")
csp_all_persistent_data = csp_data %>% filter(haplotype_category=="recurrent and persistent")


# now make plots of interval in days between infections with persistent haplotypes
# ama
all_persistent_plot = ggplot(data=ama_all_persistent_data,aes(x=days_btwn_infxns)) +
  geom_density(fill="light blue") +
  theme_bw() +
  xlab("Number of days since previous infection") +
  ggtitle("pfama1: recurrent and persistent category")
all_persistent_plot
ggsave(all_persistent_plot, filename="/Users/kelseysumner/Desktop/ama_recurrent_and_persistent_plot.png", device="png",
       height=4, width=4, units="in", dpi=400)
# csp
all_persistent_plot = ggplot(data=csp_all_persistent_data,aes(x=days_btwn_infxns)) +
  geom_density(fill="light grey") +
  theme_bw() +
  xlab("Number of days since previous infection") +
  ggtitle("pcfcsp: recurrent and persistent category")
all_persistent_plot
ggsave(all_persistent_plot, filename="/Users/kelseysumner/Desktop/csp_recurrent_and_persistent_plot.png", device="png",
       height=4, width=4, units="in", dpi=400)


# now subset to the infections that are only "all persistent"
ama_all_persistent_data = ama_data %>% filter(haplotype_category=="new, recurrent, and persistent")
csp_all_persistent_data = csp_data %>% filter(haplotype_category=="new, recurrent, and persistent")


# now make plots of interval in days between infections with persistent haplotypes
# ama
all_persistent_plot = ggplot(data=ama_all_persistent_data,aes(x=days_btwn_infxns)) +
  geom_density(fill="light blue") +
  theme_bw() +
  xlab("Number of days since previous infection") +
  ggtitle("pfama1: new, recurrent, and persistent category")
all_persistent_plot
ggsave(all_persistent_plot, filename="/Users/kelseysumner/Desktop/ama_new_recurrent_and_persistent_plot.png", device="png",
       height=4, width=4, units="in", dpi=400)
# csp
all_persistent_plot = ggplot(data=csp_all_persistent_data,aes(x=days_btwn_infxns)) +
  geom_density(fill="light grey") +
  theme_bw() +
  xlab("Number of days since previous infection") +
  ggtitle("pcfcsp: new, recurrent, and persistent category")
all_persistent_plot
ggsave(all_persistent_plot, filename="/Users/kelseysumner/Desktop/csp_new_recurrent_and_persistent_plot.png", device="png",
       height=4, width=4, units="in", dpi=400)


