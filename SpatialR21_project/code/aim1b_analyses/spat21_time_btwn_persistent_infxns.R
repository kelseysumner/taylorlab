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
library(lme4)
library(glmmTMB)



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



#### ----- split up into asymptomatic and symptomatic infections ----- ####

# now look at the difference in time between haplotype categories stratified by symptomatic status
csp_boxplot = ggplot(data=symptomatic_csp_data,aes(x=haplotype_category,y=days_btwn_infxns)) + facet_wrap(~symptomatic_status) + geom_boxplot() + theme_bw()
ama_boxplot = ggplot(data=symptomatic_ama_data,aes(x=haplotype_category,y=days_btwn_infxns)) + facet_wrap(~symptomatic_status) + geom_boxplot() + theme_bw()

# first subset the data set to just asymptomatic infections
asymptomatic_csp_data = symptomatic_csp_data %>% filter(symptomatic_status == "asymptomatic infection")
asymptomatic_ama_data = symptomatic_ama_data %>% filter(symptomatic_status == "asymptomatic infection")
# then subset to just symptomatic infections
symptomatic_csp_data = symptomatic_csp_data %>% filter(symptomatic_status == "symptomatic infection")
symptomatic_ama_data = symptomatic_ama_data %>% filter(symptomatic_status == "symptomatic infection")




#### ---- look at persistent infections ------- ####


# make a variable for having persistent haplotypes for the symptomatic infections
symptomatic_csp_data$has_persistent = ifelse(str_detect(symptomatic_csp_data$haplotype_category,"persistent"),"Symptomatic infections \n with persistent haplotypes","Symptomatic infections \n without persistent haplotypes")
symptomatic_ama_data$has_persistent = ifelse(str_detect(symptomatic_ama_data$haplotype_category,"persistent"),"Symptomatic infections \n with persistent haplotypes","Symptomatic infections \n without persistent haplotypes")
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
  geom_quasirandom(aes(fill=has_persistent),alpha=0.8,pch=21,color="#000000") + 
  theme_bw() +
  xlab("") +
  ylab("Number of days since previous infection") +
  scale_fill_manual(values = c("#969696","#252525")) +
  coord_flip() +
  theme(legend.position = "none")
csp_pre_symp
ama_pre_symp = ggplot(data=symptomatic_ama_data,aes(x=has_persistent,y=days_btwn_infxns)) + 
  geom_boxplot() +
  geom_quasirandom(aes(fill=has_persistent),alpha=0.8,pch=21,color="#000000") + 
  theme_bw() +
  xlab("") +
  ylab("Number of days since previous infection") +
  scale_fill_manual(values = c("#969696","#252525")) +
  coord_flip() +
  theme(legend.position = "none")
ama_pre_symp
ggsave(csp_pre_symp, filename="/Users/kelseysumner/Desktop/csp_pre_symptomatic_plot.png", device="png",
       height=3, width=6, units="in", dpi=400)
ggsave(ama_pre_symp, filename="/Users/kelseysumner/Desktop/ama_pre_symptomatic_plot.png", device="png",
       height=3, width=6, units="in", dpi=400)


# also create some descriptive covariates for the number of pre-symptomatic infections
table(symptomatic_csp_data$has_persistent,symptomatic_csp_data$symptomatic_status,useNA = "always")
table(symptomatic_ama_data$has_persistent,symptomatic_ama_data$symptomatic_status,useNA = "always")




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
  geom_quasirandom(aes(fill=haplotype_category,shape=symptomatic_status),alpha=0.8,size=1.75,color="#000000") + 
  theme_bw() +
  xlab("") +
  ylab("Number of days since previous infection") +
  scale_fill_manual(values = c("#cccccc","#969696","#636363","#252525")) +
  scale_shape_manual(values = c(21, 24)) +
  coord_flip() +
  labs(shape = "Symptomatic status") +
  theme(legend.position = "top") +
  guides(fill = FALSE)
csp_bees
ama_bees = ggplot(data=ama_persistent_data,aes(x=haplotype_category,y=days_btwn_infxns)) + 
  geom_boxplot() +
  geom_quasirandom(aes(fill=haplotype_category,shape=symptomatic_status),alpha=0.8,size=1.75,color="#000000") + 
  theme_bw() +
  xlab("") +
  ylab("Number of days since previous infection") +
  scale_fill_manual(values = c("#cccccc","#969696","#636363","#252525")) +
  scale_shape_manual(values = c(21, 24)) +
  coord_flip() +
  labs(shape = "Symptomatic status") +
  theme(legend.position = "top") +
  guides(fill = FALSE)
ama_bees
ggsave(csp_bees, filename="/Users/kelseysumner/Desktop/csp_beeswarm_plot.png", device="png",
       height=4, width=6.5, units="in", dpi=400)
ggsave(ama_bees, filename="/Users/kelseysumner/Desktop/ama_beeswarm_plot.png", device="png",
       height=4, width=6.5, units="in", dpi=400)


# compare the proportion of observations that are symptomatic across categories
table(csp_persistent_data$symptomatic_status,csp_persistent_data$haplotype_category,useNA = "always")
table(ama_persistent_data$symptomatic_status,ama_persistent_data$haplotype_category,useNA = "always")

# compare the proportion of observations that are symptomatic within 14 days compared to greater than 14 days
length(which(csp_persistent_data$symptomatic_status=="symptomatic infection" & csp_persistent_data$days_btwn_infxns <= 14)) # 20/37 = 54.1%
length(which(ama_persistent_data$symptomatic_status=="symptomatic infection" & ama_persistent_data$days_btwn_infxns <= 14)) # 18/31 = 58.1%


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


#### ------- run a regression model for persistent infections removing those with infections < 15 days prior ------ ####

# first subset the data set to infections that occurred >= 15 days apart
# for csp
csp_infxn_time_data = symptomatic_csp_data %>% filter(days_btwn_infxns >= 15)
summary(csp_infxn_time_data$days_btwn_infxns)
# for ama
ama_infxn_time_data = symptomatic_ama_data %>% filter(days_btwn_infxns >= 15)
summary(ama_infxn_time_data$days_btwn_infxns)

# for csp
# take out the infections with recurrent haplotypes
no_recurrent_data_csp = csp_infxn_time_data[which(!(str_detect(csp_infxn_time_data$haplotype_category,"recurrent"))),]
table(no_recurrent_data_csp$haplotype_category, useNA = "always")
no_recurrent_data_csp$haplotype_category = as.character(no_recurrent_data_csp$haplotype_category)
no_recurrent_data_csp$haplotype_category = as.factor(no_recurrent_data_csp$haplotype_category)
levels(no_recurrent_data_csp$haplotype_category)
no_recurrent_data_csp$haplotype_category = relevel(no_recurrent_data_csp$haplotype_category,ref="all persistent")
# merge in covariates
csp_cov_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/without first infection/aim1b_csp_final_model_data_21JUL2020.rds")
csp_cov_data = csp_cov_data %>% select(sample_name_dbs,add_cat_number_prior_infections,mosquito_week_count_cat_add,moi_cat)
no_recurrent_data_csp = left_join(no_recurrent_data_csp,csp_cov_data,by="sample_name_dbs")
no_recurrent_data_csp = rename(no_recurrent_data_csp,unq_memID = unq_memID.x)
no_recurrent_data_csp$unq_memID.y <- NULL
length(which(is.na(no_recurrent_data_csp$moi_cat)))
no_recurrent_data_csp$symptomatic_status = as.factor(no_recurrent_data_csp$symptomatic_status)
levels(no_recurrent_data_csp$symptomatic_status)
# now rerun the model
csp_model_1 <- glmmTMB(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), 
                       data = no_recurrent_data_csp)
summary(csp_model_1)
performance::icc(csp_model_1)
exp(confint(csp_model_1,method="Wald"))
# all new: OR 1.55 (95% CI 0.58 to 4.09)
# new and persistent: OR 1.36 (95% CI 0.37 to 4.93)
# make a forest plot of results
table1 = exp(confint(csp_model_1,method="Wald"))
summary(csp_model_1)
estimates = c(table1[2,3],table1[3,3],NA,table1[5,3],table1[4,3],NA,table1[6,3],NA,table1[7,3],NA,table1[8,3])
lower_ci = c(table1[2,1],table1[3,1],NA,table1[5,1],table1[4,1],NA,table1[6,1],NA,table1[7,1],NA,table1[8,1])
upper_ci = c(table1[2,2],table1[3,2],NA,table1[5,2],table1[4,2],NA,table1[6,2],NA,table1[7,2],NA,table1[8,2])
names = c("Only new vs. only persistent haplotypes","New and persistent vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Only new vs. only persistent haplotypes","New and persistent vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Only new vs. only persistent haplotypes","New and persistent vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
# create a forest plot
library(forcats)
library(ggplot2)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,3,1,1,1,1,1,1,1,1,1),colour=c("#2166ac","#67a9cf","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(breaks=c(0,1,2,3,4,5),trans="log10") +
  theme_bw()
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/csp_aim1b_model_all_3_categories_no_recurrent_15infxns.png", device="png",
       height=4.5, width=7, units="in", dpi=400)


# for ama
# take out the infections with recurrent haplotypes
no_recurrent_data_ama = ama_infxn_time_data[which(!(str_detect(ama_infxn_time_data$haplotype_category,"recurrent"))),]
table(no_recurrent_data_ama$haplotype_category, useNA = "always")
no_recurrent_data_ama$haplotype_category = as.character(no_recurrent_data_ama$haplotype_category)
no_recurrent_data_ama$haplotype_category = as.factor(no_recurrent_data_ama$haplotype_category)
levels(no_recurrent_data_ama$haplotype_category)
no_recurrent_data_ama$haplotype_category = relevel(no_recurrent_data_ama$haplotype_category,ref="all persistent")
# merge in covariates
ama_cov_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/without first infection/aim1b_ama_final_model_data_21JUL2020.rds")
ama_cov_data = ama_cov_data %>% select(sample_name_dbs,add_cat_number_prior_infections,mosquito_week_count_cat_add,moi_cat)
no_recurrent_data_ama = left_join(no_recurrent_data_ama,ama_cov_data,by="sample_name_dbs")
no_recurrent_data_ama = rename(no_recurrent_data_ama,unq_memID = unq_memID.x)
no_recurrent_data_ama$unq_memID.y <- NULL
length(which(is.na(no_recurrent_data_ama$moi_cat)))
no_recurrent_data_ama$symptomatic_status = as.factor(no_recurrent_data_ama$symptomatic_status)
levels(no_recurrent_data_ama$symptomatic_status)
# now rerun the model
ama_model_1 <- glmmTMB(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), 
                       data = no_recurrent_data_ama)
summary(ama_model_1)
performance::icc(ama_model_1)
exp(confint(ama_model_1,method="Wald"))
# all new: OR 0.52 (95% CI 0.12 to 2.26)
# new and persistent: OR 0.55 (95% CI 0.10 to 2.96)
# make a forest plot of results
table1 = exp(confint(ama_model_1,method="Wald"))
summary(ama_model_1)
estimates = c(table1[2,3],table1[3,3],NA,table1[5,3],table1[4,3],NA,table1[6,3],NA,table1[7,3],NA,table1[8,3])
lower_ci = c(table1[2,1],table1[3,1],NA,table1[5,1],table1[4,1],NA,table1[6,1],NA,table1[7,1],NA,table1[8,1])
upper_ci = c(table1[2,2],table1[3,2],NA,table1[5,2],table1[4,2],NA,table1[6,2],NA,table1[7,2],NA,table1[8,2])
names = c("Only new vs. only persistent haplotypes","New and persistent vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Only new vs. only persistent haplotypes","New and persistent vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Only new vs. only persistent haplotypes","New and persistent vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
# create a forest plot
library(forcats)
library(ggplot2)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,3,1,1,1,1,1,1,1,1,1),colour=c("#2166ac","#67a9cf","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(breaks=c(0,1,2,4,6,8,10,12),trans="log10") +
  theme_bw()
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/ama_aim1b_model_all_3_categories_no_recurrent_15infxns.png", device="png",
       height=4.5, width=8, units="in", dpi=400)


#### ------ subset infections to those with consecutive infections within 30 days --------- ####

# first subset to infections within 30 days
csp_30days = symptomatic_csp_data %>% filter(days_btwn_infxns <= 30)
ama_30days = symptomatic_ama_data%>% filter(days_btwn_infxns <= 30)
summary(csp_30days$days_btwn_infxns)
summary(ama_30days$days_btwn_infxns)

# for csp
# take out the infections with recurrent haplotypes
no_recurrent_data_csp = csp_30days[which(!(str_detect(csp_30days$haplotype_category,"recurrent"))),]
table(no_recurrent_data_csp$haplotype_category, useNA = "always")
no_recurrent_data_csp$haplotype_category = as.character(no_recurrent_data_csp$haplotype_category)
no_recurrent_data_csp$haplotype_category = as.factor(no_recurrent_data_csp$haplotype_category)
levels(no_recurrent_data_csp$haplotype_category)
no_recurrent_data_csp$haplotype_category = relevel(no_recurrent_data_csp$haplotype_category,ref="all persistent")
# merge in covariates
csp_cov_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/without first infection/aim1b_csp_final_model_data_21JUL2020.rds")
csp_cov_data = csp_cov_data %>% select(sample_name_dbs,add_cat_number_prior_infections,mosquito_week_count_cat_add,moi_cat)
no_recurrent_data_csp = left_join(no_recurrent_data_csp,csp_cov_data,by="sample_name_dbs")
no_recurrent_data_csp = rename(no_recurrent_data_csp,unq_memID = unq_memID.x)
no_recurrent_data_csp$unq_memID.y <- NULL
length(which(is.na(no_recurrent_data_csp$moi_cat)))
no_recurrent_data_csp$symptomatic_status = as.factor(no_recurrent_data_csp$symptomatic_status)
levels(no_recurrent_data_csp$symptomatic_status)
# now rerun the model
csp_model_1 <- glmmTMB(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), 
                       data = no_recurrent_data_csp)
summary(csp_model_1)
performance::icc(csp_model_1)
exp(confint(csp_model_1,method="Wald"))
# all new: OR 0.69 (95% CI 0.27 to 1.77)
# new and persistent: OR 0.75 (95% CI 0.22 to 2.58)
# make a forest plot of results
table1 = exp(confint(csp_model_1,method="Wald"))
summary(csp_model_1)
estimates = c(table1[2,3],table1[3,3],NA,table1[5,3],table1[4,3],NA,table1[6,3],NA,table1[7,3],NA,table1[8,3])
lower_ci = c(table1[2,1],table1[3,1],NA,table1[5,1],table1[4,1],NA,table1[6,1],NA,table1[7,1],NA,table1[8,1])
upper_ci = c(table1[2,2],table1[3,2],NA,table1[5,2],table1[4,2],NA,table1[6,2],NA,table1[7,2],NA,table1[8,2])
names = c("Only new vs. only persistent haplotypes","New and persistent vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Only new vs. only persistent haplotypes","New and persistent vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Only new vs. only persistent haplotypes","New and persistent vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
# create a forest plot
library(forcats)
library(ggplot2)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,3,1,1,1,1,1,1,1,1,1),colour=c("#2166ac","#67a9cf","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(breaks=c(0,1,2,3,4,5),trans="log10") +
  theme_bw()
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/csp_aim1b_model_all_3_categories_no_recurrent_subsetinfxns.png", device="png",
       height=4.5, width=7, units="in", dpi=400)

# for ama
# take out the infections with recurrent haplotypes
no_recurrent_data_ama = ama_30days[which(!(str_detect(ama_30days$haplotype_category,"recurrent"))),]
table(no_recurrent_data_ama$haplotype_category, useNA = "always")
no_recurrent_data_ama$haplotype_category = as.character(no_recurrent_data_ama$haplotype_category)
no_recurrent_data_ama$haplotype_category = as.factor(no_recurrent_data_ama$haplotype_category)
levels(no_recurrent_data_ama$haplotype_category)
no_recurrent_data_ama$haplotype_category = relevel(no_recurrent_data_ama$haplotype_category,ref="all persistent")
# merge in covariates
ama_cov_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/without first infection/aim1b_ama_final_model_data_21JUL2020.rds")
ama_cov_data = ama_cov_data %>% select(sample_name_dbs,add_cat_number_prior_infections,mosquito_week_count_cat_add,moi_cat)
no_recurrent_data_ama = left_join(no_recurrent_data_ama,ama_cov_data,by="sample_name_dbs")
no_recurrent_data_ama = rename(no_recurrent_data_ama,unq_memID = unq_memID.x)
no_recurrent_data_ama$unq_memID.y <- NULL
length(which(is.na(no_recurrent_data_ama$moi_cat)))
no_recurrent_data_ama$symptomatic_status = as.factor(no_recurrent_data_ama$symptomatic_status)
levels(no_recurrent_data_ama$symptomatic_status)
# now rerun the model
ama_model_1 <- glmmTMB(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), 
                       data = no_recurrent_data_ama)
summary(ama_model_1)
performance::icc(ama_model_1)
exp(confint(ama_model_1,method="Wald"))
# all new: OR 0.10 (95% CI 0.02 to 0.46)
# new and persistent: OR 0.14 (95% CI 0.03 to 0.70)
# make a forest plot of results
table1 = exp(confint(ama_model_1,method="Wald"))
summary(ama_model_1)
estimates = c(table1[2,3],table1[3,3],NA,table1[5,3],table1[4,3],NA,table1[6,3],NA,table1[7,3],NA,table1[8,3])
lower_ci = c(table1[2,1],table1[3,1],NA,table1[5,1],table1[4,1],NA,table1[6,1],NA,table1[7,1],NA,table1[8,1])
upper_ci = c(table1[2,2],table1[3,2],NA,table1[5,2],table1[4,2],NA,table1[6,2],NA,table1[7,2],NA,table1[8,2])
names = c("Only new vs. only persistent haplotypes","New and persistent vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Only new vs. only persistent haplotypes","New and persistent vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Only new vs. only persistent haplotypes","New and persistent vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
# create a forest plot
library(forcats)
library(ggplot2)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,3,1,1,1,1,1,1,1,1,1),colour=c("#2166ac","#67a9cf","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(breaks=c(0,1,2,4,6,8,10,12),trans="log10") +
  theme_bw()
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/ama_aim1b_model_all_3_categories_no_recurrent_subsetinfxns.png", device="png",
       height=4.5, width=8, units="in", dpi=400)



#### ------ subset infections to those with consecutive infections within 30 days and with only persistent haplotypes --------- ####

# first subset to infections within 30 days
csp_30days = symptomatic_csp_data %>% filter(days_btwn_infxns <= 30)
ama_30days = symptomatic_ama_data %>% filter(days_btwn_infxns <= 30)
summary(csp_30days$days_btwn_infxns)
summary(ama_30days$days_btwn_infxns)

# take out possible recrudescent infections
# 3 infections for csp
csp_30days = csp_30days[-which(csp_30days$unq_memID == "K01_5" & csp_30days$sample_id_date == "2017-07-06"),]
csp_30days = csp_30days[-which(csp_30days$unq_memID == "K01_7" & csp_30days$sample_id_date == "2017-08-03"),]
csp_30days = csp_30days[-which(csp_30days$unq_memID == "M14_2" & csp_30days$sample_id_date == "2017-08-17"),]
# 1 infection for ama
ama_30days = ama_30days[-which(ama_30days$unq_memID == "K01_7" & ama_30days$sample_id_date == "2017-08-03"),]

# for csp
# take out the infections with recurrent haplotypes
all_persistent_data_csp = csp_30days[which((str_detect(csp_30days$haplotype_category,"persistent"))),]
table(all_persistent_data_csp$haplotype_category, useNA = "always")
all_persistent_data_csp$haplotype_category = as.character(all_persistent_data_csp$haplotype_category)
all_persistent_data_csp$haplotype_category = as.factor(all_persistent_data_csp$haplotype_category)
levels(all_persistent_data_csp$haplotype_category)
all_persistent_data_csp$haplotype_category = relevel(all_persistent_data_csp$haplotype_category,ref="all persistent")
# merge in covariates
csp_cov_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/without first infection/aim1b_csp_final_model_data_21JUL2020.rds")
csp_cov_data = csp_cov_data %>% select(sample_name_dbs,add_cat_number_prior_infections,mosquito_week_count_cat_add,moi_cat)
all_persistent_data_csp = left_join(all_persistent_data_csp,csp_cov_data,by="sample_name_dbs")
all_persistent_data_csp = rename(all_persistent_data_csp,unq_memID = unq_memID.x)
all_persistent_data_csp$unq_memID.y <- NULL
length(which(is.na(all_persistent_data_csp$moi_cat)))
all_persistent_data_csp$symptomatic_status = as.factor(all_persistent_data_csp$symptomatic_status)
levels(all_persistent_data_csp$symptomatic_status)
# create a new category comparing infections with only persistent haplotypes to those with other types of haplotypes in addition to persistent ones
all_persistent_data_csp$persistent_category = ifelse(all_persistent_data_csp$haplotype_category=="all persistent","Only persistent haplotypes","Mix of persistent and other haplotypes")
table(all_persistent_data_csp$persistent_category,all_persistent_data_csp$haplotype_category,useNA = "always")
table(all_persistent_data_csp$persistent_category,all_persistent_data_csp$symptomatic_status,useNA = "always") # good positivity
all_persistent_data_csp$persistent_category = as.factor(all_persistent_data_csp$persistent_category)
all_persistent_data_csp$persistent_category = relevel(all_persistent_data_csp$persistent_category,ref="Only persistent haplotypes")
# now rerun the model
csp_model_1 <- glmmTMB(symptomatic_status ~ persistent_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), 
                       data = all_persistent_data_csp)
summary(csp_model_1)
performance::icc(csp_model_1)
exp(confint(csp_model_1,method="Wald"))
# mix persistent compared to only persistent: OR 0.78 (95% CI 0.22 to 2.74)
# make a forest plot of results
table1 = exp(confint(csp_model_1,method="Wald"))
summary(csp_model_1)
estimates = c(table1[2,3],NA,table1[4,3],table1[3,3],NA,table1[5,3],NA,table1[6,3],NA,table1[7,3])
lower_ci = c(table1[2,1],NA,table1[4,1],table1[3,1],NA,table1[5,1],NA,table1[6,1],NA,table1[7,1])
upper_ci = c(table1[2,2],NA,table1[4,2],table1[3,2],NA,table1[5,2],NA,table1[6,2],NA,table1[7,2])
names = c("Mixed types of haplotypes vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Mixed types of haplotypes vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Mixed types of haplotypes vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
# create a forest plot
library(forcats)
library(ggplot2)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,1,1,1,1,1,1,1,1,1),colour=c("#2166ac","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10", breaks = c(0,1,10,100)) +
  theme_bw()
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/csp_aim1b_model_all_3_categories_all_persistent_subsetinfxns.png", device="png",
       height=4.5, width=7, units="in", dpi=400)

# for ama
# take out the infections with recurrent haplotypes
all_persistent_data_ama = ama_30days[which((str_detect(ama_30days$haplotype_category,"persistent"))),]
table(all_persistent_data_ama$haplotype_category, useNA = "always")
all_persistent_data_ama$haplotype_category = as.character(all_persistent_data_ama$haplotype_category)
all_persistent_data_ama$haplotype_category = as.factor(all_persistent_data_ama$haplotype_category)
levels(all_persistent_data_ama$haplotype_category)
all_persistent_data_ama$haplotype_category = relevel(all_persistent_data_ama$haplotype_category,ref="all persistent")
# merge in covariates
ama_cov_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/without first infection/aim1b_ama_final_model_data_21JUL2020.rds")
ama_cov_data = ama_cov_data %>% select(sample_name_dbs,add_cat_number_prior_infections,mosquito_week_count_cat_add,moi_cat)
all_persistent_data_ama = left_join(all_persistent_data_ama,ama_cov_data,by="sample_name_dbs")
all_persistent_data_ama = rename(all_persistent_data_ama,unq_memID = unq_memID.x)
all_persistent_data_ama$unq_memID.y <- NULL
length(which(is.na(all_persistent_data_ama$moi_cat)))
all_persistent_data_ama$symptomatic_status = as.factor(all_persistent_data_ama$symptomatic_status)
levels(all_persistent_data_ama$symptomatic_status)
# create a new category comparing infections with only persistent haplotypes to those with other types of haplotypes in addition to persistent ones
all_persistent_data_ama$persistent_category = ifelse(all_persistent_data_ama$haplotype_category=="all persistent","Only persistent haplotypes","Mix of persistent and other haplotypes")
table(all_persistent_data_ama$persistent_category,all_persistent_data_ama$haplotype_category,useNA = "always")
table(all_persistent_data_ama$persistent_category,all_persistent_data_ama$symptomatic_status,useNA = "always") # good positivity
all_persistent_data_ama$persistent_category = as.factor(all_persistent_data_ama$persistent_category)
all_persistent_data_ama$persistent_category = relevel(all_persistent_data_ama$persistent_category,ref="Only persistent haplotypes")
# now rerun the model
ama_model_1 <- glmmTMB(symptomatic_status ~ persistent_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), 
                       data = all_persistent_data_ama)
summary(ama_model_1)
performance::icc(ama_model_1)
exp(confint(ama_model_1,method="Wald"))
# mix persistent compared to only persistent: OR 0.78 (95% CI 0.22 to 2.74)
# make a forest plot of results
table1 = exp(confint(ama_model_1,method="Wald"))
summary(ama_model_1)
estimates = c(table1[2,3],NA,table1[4,3],table1[3,3],NA,table1[5,3],NA,table1[6,3],NA,table1[7,3])
lower_ci = c(table1[2,1],NA,table1[4,1],table1[3,1],NA,table1[5,1],NA,table1[6,1],NA,table1[7,1])
upper_ci = c(table1[2,2],NA,table1[4,2],table1[3,2],NA,table1[5,2],NA,table1[6,2],NA,table1[7,2])
names = c("Mixed types of haplotypes vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Mixed types of haplotypes vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Mixed types of haplotypes vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
# create a forest plot
library(forcats)
library(ggplot2)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,1,1,1,1,1,1,1,1,1),colour=c("#2166ac","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10", breaks = c(0,1,10,100)) +
  theme_bw()
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/ama_aim1b_model_all_3_categories_all_persistent_subsetinfxns.png", device="png",
       height=4.5, width=7, units="in", dpi=400)






