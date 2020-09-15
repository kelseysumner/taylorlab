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
library(car)
library(ggbeeswarm)



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


#### ------ look at the days before infections for symptomatic infections ------- ####

# this is a way to look at pre-symptomatic infections

# first order the data set by date
symptomatic_csp_data = dplyr::arrange(csp_data,unq_memID,sample_id_date)
symptomatic_ama_data = dplyr::arrange(ama_data,unq_memID,sample_id_date)

# look at how many infections each participant had
num_infections_before = symptomatic_csp_data %>%
  group_by(unq_memID) %>%
  summarize (n= n())
num_infections_before = symptomatic_ama_data %>%
  group_by(unq_memID) %>%
  summarize (n= n())

# looks like this worked correctly so apply to everything
symptomatic_csp_data = slice(group_by(symptomatic_csp_data, unq_memID), -1)
symptomatic_ama_data = slice(group_by(symptomatic_ama_data, unq_memID), -1)

# now look at the difference in time between haplotype categories stratified by symptomatic status
csp_boxplot = ggplot(data=symptomatic_csp_data,aes(x=haplotype_category,y=days_btwn_infxns)) + facet_wrap(~symptomatic_status) + geom_boxplot() + theme_bw()
ama_boxplot = ggplot(data=symptomatic_ama_data,aes(x=haplotype_category,y=days_btwn_infxns)) + facet_wrap(~symptomatic_status) + geom_boxplot() + theme_bw()

# first subset the data set to just asymptomatic infections
asymptomatic_csp_data = symptomatic_csp_data %>% filter(symptomatic_status == "asymptomatic infection")
asymptomatic_ama_data = symptomatic_ama_data %>% filter(symptomatic_status == "asymptomatic infection")
# then subset to just symptomatic infections
symptomatic_csp_data = symptomatic_csp_data %>% filter(symptomatic_status == "symptomatic infection")
symptomatic_ama_data = symptomatic_ama_data %>% filter(symptomatic_status == "symptomatic infection")

# make a variable for having persistent haplotypes for the symptomatic infections
symptomatic_csp_data$has_persistent = ifelse(str_detect(symptomatic_csp_data$haplotype_category,"persistent"),"Infections with persistent haplotypes","Infections without persistent haplotypes")
symptomatic_ama_data$has_persistent = ifelse(str_detect(symptomatic_ama_data$haplotype_category,"persistent"),"Infections with persistent haplotypes","Infections without persistent haplotypes")
table(symptomatic_csp_data$has_persistent,symptomatic_csp_data$haplotype_category, useNA = "always")
table(symptomatic_ama_data$has_persistent,symptomatic_ama_data$haplotype_category, useNA = "always")
table(symptomatic_csp_data$has_persistent)
table(symptomatic_ama_data$has_persistent)

# now compare the time between infections with persistent haplotypes compared to those without
# first make a box plot
csp_boxplot = ggplot(data=symptomatic_csp_data,aes(x=has_persistent,y=days_btwn_infxns)) + geom_boxplot(aes(fill=has_persistent)) + theme_bw()
ama_boxplot = ggplot(data=symptomatic_csp_data,aes(x=has_persistent,y=days_btwn_infxns)) + geom_boxplot(aes(fill=has_persistent)) + theme_bw()
# note: observations not really independent so this assumption violated
# check normalcy
leveneTest(symptomatic_csp_data$days_btwn_infxns,symptomatic_csp_data$has_persistent) # not normal
leveneTest(symptomatic_ama_data$days_btwn_infxns,symptomatic_ama_data$has_persistent) # not normal
# do kruskal-wallis test instead
kruskal.test(days_btwn_infxns ~ has_persistent, data=symptomatic_csp_data)
kruskal.test(days_btwn_infxns ~ has_persistent, data=symptomatic_ama_data)
symptomatic_csp_data %>%
  group_by(has_persistent) %>%
  summarize(min=min(days_btwn_infxns),max=max(days_btwn_infxns),mean=mean(days_btwn_infxns),median(days_btwn_infxns))
symptomatic_ama_data %>%
  group_by(has_persistent) %>%
  summarize(min=min(days_btwn_infxns),max=max(days_btwn_infxns),mean=mean(days_btwn_infxns),median(days_btwn_infxns))

# now make a figure
# make a beeswarm plot of the days between infections for persistent categories
csp_pre_symp = ggplot(data=symptomatic_csp_data,aes(x=has_persistent,y=days_btwn_infxns)) + 
  geom_boxplot() +
  geom_quasirandom(aes(color=has_persistent)) + 
  theme_bw() +
  xlab("") +
  ylab("Number of days since previous infection") +
  scale_color_manual(values = c("#0571b0","#ca0020")) +
  coord_flip() +
  theme(legend.position = "none")
csp_pre_symp
ama_pre_symp = ggplot(data=symptomatic_ama_data,aes(x=has_persistent,y=days_btwn_infxns)) + 
  geom_boxplot() +
  geom_quasirandom(aes(color=has_persistent)) + 
  theme_bw() +
  xlab("") +
  ylab("Number of days since previous infection") +
  scale_color_manual(values = c("#0571b0","#ca0020")) +
  coord_flip() +
  theme(legend.position = "none")
ama_pre_symp
ggsave(csp_pre_symp, filename="/Users/kelseysumner/Desktop/csp_pre_symptomatic_plot.png", device="png",
       height=4, width=6.5, units="in", dpi=400)
ggsave(ama_pre_symp, filename="/Users/kelseysumner/Desktop/ama_pre_symptomatic_plot.png", device="png",
       height=4, width=6.5, units="in", dpi=400)



#### ----- subset to just the persistent haplotypes ------ ####

# subset to just infections with persistent haplotypes
ama_persistent_data = ama_data %>% filter(str_detect(haplotype_category,"persistent"))
csp_persistent_data = csp_data %>% filter(str_detect(haplotype_category,"persistent"))


# run an anova on days between infections for each of the persistent categories
# first make a box plot
csp_boxplot = ggplot(data=csp_persistent_data,aes(x=haplotype_category,y=days_btwn_infxns)) + geom_boxplot(aes(fill=haplotype_category)) + theme_bw()
ama_boxplot = ggplot(data=ama_persistent_data,aes(x=haplotype_category,y=days_btwn_infxns)) + geom_boxplot(aes(fill=haplotype_category)) + theme_bw()
ggsave(csp_boxplot, filename="/Users/kelseysumner/Desktop/csp_boxplot.png", device="png",
       height=4, width=9, units="in", dpi=400)
ggsave(ama_boxplot, filename="/Users/kelseysumner/Desktop/ama_boxplot.png", device="png",
       height=4, width=9, units="in", dpi=400)
# note: observations not really independent so this assumption violated
# check normalcy
leveneTest(csp_persistent_data$days_btwn_infxns,csp_persistent_data$haplotype_category) # not normal
leveneTest(ama_persistent_data$days_btwn_infxns,ama_persistent_data$haplotype_category) # not normal
# do kruskal-wallis test instead
kruskal.test(days_btwn_infxns ~ haplotype_category, data=csp_persistent_data)
kruskal.test(days_btwn_infxns ~ haplotype_category, data=ama_persistent_data)

# relabel the categories
# for csp
csp_persistent_data$haplotype_category[which(csp_persistent_data$haplotype_category == "recurrent and persistent")] = "Recurrent and persistent"
csp_persistent_data$haplotype_category[which(csp_persistent_data$haplotype_category == "new, recurrent, and persistent")] = "New, recurrent, and persistent"
csp_persistent_data$haplotype_category[which(csp_persistent_data$haplotype_category == "new and persistent")] = "New and persistent"
csp_persistent_data$haplotype_category[which(csp_persistent_data$haplotype_category == "all persistent")] = "Only persistent"
csp_persistent_data$haplotype_category = factor(csp_persistent_data$haplotype_category,levels=c("Only persistent","New and persistent","Recurrent and persistent","New, recurrent, and persistent"))
# for ama
ama_persistent_data$haplotype_category[which(ama_persistent_data$haplotype_category == "recurrent and persistent")] = "Recurrent and persistent"
ama_persistent_data$haplotype_category[which(ama_persistent_data$haplotype_category == "new, recurrent, and persistent")] = "New, recurrent, and persistent"
ama_persistent_data$haplotype_category[which(ama_persistent_data$haplotype_category == "new and persistent")] = "New and persistent"
ama_persistent_data$haplotype_category[which(ama_persistent_data$haplotype_category == "all persistent")] = "Only persistent"
ama_persistent_data$haplotype_category = factor(ama_persistent_data$haplotype_category,levels=c("Only persistent","New and persistent","Recurrent and persistent","New, recurrent, and persistent"))

# make a beeswarm plot of the days between infections for persistent categories
csp_bees = ggplot(data=csp_persistent_data,aes(x=haplotype_category,y=days_btwn_infxns)) + 
  geom_boxplot() +
  geom_quasirandom(aes(color=haplotype_category)) + 
  theme_bw() +
  xlab("") +
  ylab("Number of days since previous infection") +
  scale_color_manual(values = c("#ca0020","#f4a582","#92c5de","#0571b0")) +
  coord_flip() +
  theme(legend.position = "none")
csp_bees
ama_bees = ggplot(data=ama_persistent_data,aes(x=haplotype_category,y=days_btwn_infxns)) + 
  geom_boxplot() +
  geom_quasirandom(aes(color=haplotype_category)) + 
  theme_bw() +
  xlab("") +
  ylab("Number of days since previous infection") +
  scale_color_manual(values = c("#ca0020","#f4a582","#92c5de","#0571b0")) +
  coord_flip() +
  theme(legend.position = "none")
ama_bees
ggsave(csp_bees, filename="/Users/kelseysumner/Desktop/csp_beeswarm_plot.png", device="png",
       height=4, width=6.5, units="in", dpi=400)
ggsave(ama_bees, filename="/Users/kelseysumner/Desktop/ama_beeswarm_plot.png", device="png",
       height=4, width=6.5, units="in", dpi=400)


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


