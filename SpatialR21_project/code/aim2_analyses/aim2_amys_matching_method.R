# -------------------------------------- #
#           Spat21/Mozzie Study          #
#  Use Amy's asymp/symp matching method  #
#                 Aim 2                  #
#            Mozzie Phase 1              #
#               K. Sumner                #
#            April 7, 2020               #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(tidyverse)
library(BSDA)
library(glmmTMB)



#### ----- read in the data sets ----- ####

# read in the combined ama and csp data set for mosquito abdomens
model_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/AMA_and_CSP/final/model data/final_model_data/spat21_aim2_merged_data_with_weights_5MAR2020.rds")

# subset the data set to samples that passed pfcsp sequencing only
csp_data = model_data %>%
  filter(!(is.na(csp_haps_shared)))

# subset the data set to samples that passed pfcsp sequencing only
ama_data = model_data %>%
  filter(!(is.na(ama_haps_shared)))

# read in the full human demographic data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/final_recoded_data_set/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1MAR2020.rds")
final_data = final_data %>%
  select(c(unq_memID,age_cat_baseline))



#### ------ now try coding amy's method -------- ####

# first subset to only the individuals who have at least 1 symptomatic and 1 asymptomatic infection
# for csp - end up with 65 unique participants and 1565 obs
csp_infxns_to_include_p1 = csp_data %>%
  group_by(unq_memID,aim2_exposure) %>%
  summarize(n=n())
csp_infxns_to_include_p2 = csp_infxns_to_include_p1 %>%
  group_by(unq_memID) %>%
  summarize(n=n())
csp_infxns_to_include_p3 = csp_infxns_to_include_p2 %>%
  filter(n>1)
csp_subset_data = csp_data %>%
  filter(csp_data$unq_memID %in% csp_infxns_to_include_p3$unq_memID)
# for ama - end up with 56 unique participants and 1197 obs
ama_infxns_to_include_p1 = ama_data %>%
  group_by(unq_memID,aim2_exposure) %>%
  summarize(n=n())
ama_infxns_to_include_p2 = ama_infxns_to_include_p1 %>%
  group_by(unq_memID) %>%
  summarize(n=n())
ama_infxns_to_include_p3 = ama_infxns_to_include_p2 %>%
  filter(n>1)
ama_subset_data = ama_data %>%
  filter(ama_data$unq_memID %in% ama_infxns_to_include_p3$unq_memID)

# now calculate the average p_te_all within asymptomatic and symptomatic infections
# for csp
csp_average_data = csp_subset_data %>%
  group_by(village_name,unq_memID,aim2_exposure) %>%
  summarize(average_p_te_all_csp=mean(p_te_all_csp,na.rm=T), median_p_te_all_csp = median(p_te_all_csp,na.rm = T))
# for ama
ama_average_data = ama_subset_data %>%
  group_by(village_name,unq_memID,aim2_exposure) %>%
  summarize(average_p_te_all_ama=mean(p_te_all_ama,na.rm=T), median_p_te_all_ama = median(p_te_all_ama,na.rm = T))  

# now split up the data sets so have separate columns for asymptomatic and symptomatic infections
# for csp
csp_asymp = csp_average_data %>%
  filter(aim2_exposure=="asymptomatic infection") %>%
  select(-c(aim2_exposure)) %>%
  rename("asymp_average_p_te_all_csp" = average_p_te_all_csp,"asymp_median_p_te_all_csp" = median_p_te_all_csp)
csp_symp = csp_average_data %>%
  filter(aim2_exposure == "symptomatic infection") %>%
  select(-c(village_name,aim2_exposure)) %>%
  rename("symp_average_p_te_all_csp" = average_p_te_all_csp,"symp_median_p_te_all_csp" = median_p_te_all_csp)
csp_all = left_join(csp_asymp,csp_symp,by=c("unq_memID"))
csp_all$village_name.y <- NULL
csp_all = rename(csp_all,village_name = village_name.x)
# ama
ama_asymp = ama_average_data %>%
  filter(aim2_exposure=="asymptomatic infection") %>%
  select(-c(aim2_exposure)) %>%
  rename("asymp_average_p_te_all_ama" = average_p_te_all_ama,"asymp_median_p_te_all_ama" = median_p_te_all_ama)
ama_symp = ama_average_data %>%
  filter(aim2_exposure == "symptomatic infection") %>%
  select(-c(village_name,aim2_exposure)) %>%
  rename("symp_average_p_te_all_ama" = average_p_te_all_ama,"asymp_median_p_te_all_ama" = median_p_te_all_ama)
ama_all = left_join(ama_asymp,ama_symp,by=c("unq_memID"))
ama_all$village_name.y <- NULL
ama_all = rename(ama_all,village_name = village_name.x)

# now make a plot of the average probability of p_te_all by symptomatic status
# for csp
csp_plot = ggplot(csp_all, aes(x=symp_average_p_te_all_csp, y=asymp_average_p_te_all_csp)) +
  geom_point(aes(color=village_name),size=2.5,pch=21) + 
  theme_bw() +
  geom_abline(intercept = 0, slope = 1,linetype="dashed") +
  xlab("Average probability of transmission for symptomatic infections") +
  ylab("Average probability of transmission for asymptomatic infections") +
  labs(color="Village name") +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(limits=c(0,1)) +
  scale_color_manual(values = c("#377eb8","#66bd63","#fb9a99"))
ggsave(csp_plot, filename="/Users/kelseysumner/Desktop/csp_matched_prob_plot.png", device="png",
    height=5, width=6, units="in", dpi=500)
# for ama
ama_plot = ggplot(ama_all, aes(x=symp_average_p_te_all_ama, y=asymp_average_p_te_all_ama)) +
  geom_point(aes(color=village_name),size=2.5,pch=21) + 
  theme_bw() +
  geom_abline(intercept = 0, slope = 1,linetype="dashed") +
  xlab("Average probability of transmission for symptomatic infections") +
  ylab("Average probability of transmission for asymptomatic infections") +
  labs(color="Village name") +
  scale_x_continuous(limits=c(0,0.4)) +
  scale_y_continuous(limits=c(0,0.4)) +
  scale_color_manual(values = c("#377eb8","#66bd63","#fb9a99"))
ggsave(ama_plot, filename="/Users/kelseysumner/Desktop/ama_matched_prob_plot.png", device="png",
       height=5, width=6, units="in", dpi=500)

# check the mean of means by symptomatic status
# for csp
mean(csp_all$asymp_average_p_te_all_csp)
mean(csp_all$symp_average_p_te_all_csp)
# for ama
mean(ama_all$asymp_average_p_te_all_ama)
mean(ama_all$symp_average_p_te_all_ama)

# check the median of means by symptomatic status
# for csp
median(csp_all$asymp_average_p_te_all_csp)
median(csp_all$symp_average_p_te_all_csp)
# for ama
median(ama_all$asymp_average_p_te_all_ama)
median(ama_all$symp_average_p_te_all_ama)

# check normality (but N>30 so not a large issue)
# for csp
d <- csp_all$asymp_average_p_te_all_csp-csp_all$symp_average_p_te_all_csp
shapiro.test(d) # normality not an issue
hist(d)
# for ama
d <- ama_all$asymp_average_p_te_all_ama-ama_all$symp_average_p_te_all_ama
shapiro.test(d) # normality could be an issue but N>30 so central limit theorem applies
hist(d)

# paired t-test
t.test(csp_all$asymp_average_p_te_all_csp, csp_all$symp_average_p_te_all_csp, paired = TRUE, alternative = "greater")
t.test(ama_all$asymp_average_p_te_all_ama, ama_all$symp_average_p_te_all_ama, paired = TRUE, alternative = "greater")

# sign test just to be careful
SIGN.test(csp_all$asymp_average_p_te_all_csp, csp_all$symp_average_p_te_all_csp,md=0,alternative = "greater")
SIGN.test(ama_all$asymp_average_p_te_all_ama, ama_all$symp_average_p_te_all_ama,md=0,alternative = "greater")



#### ----- try alternative plot 1: median value for symptomatic status ------ ####

# only do for csp right now

# now make a plot of the average probability of p_te_all by symptomatic status
csp_plot = ggplot(csp_all, aes(x=symp_median_p_te_all_csp, y=asymp_median_p_te_all_csp)) +
  geom_point(aes(color=village_name),size=2.5,pch=21) + 
  theme_bw() +
  geom_abline(intercept = 0, slope = 1,linetype="dashed") +
  xlab("Median probability of transmission for symptomatic infections") +
  ylab("Median probability of transmission for asymptomatic infections") +
  labs(color="Village name") +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(limits=c(0,1)) +
  scale_color_manual(values = c("#377eb8","#66bd63","#fb9a99"))
ggsave(csp_plot, filename="/Users/kelseysumner/Desktop/csp_matched_prob_plot_alt1.png", device="png",
       height=5, width=6, units="in", dpi=500)

# check the median of medians by symptomatic status
# for csp
median(csp_all$asymp_median_p_te_all_csp)
median(csp_all$symp_median_p_te_all_csp)

# sign test
SIGN.test(csp_all$asymp_median_p_te_all_csp, csp_all$symp_median_p_te_all_csp,md=0,alternative = "greater")



#### ----- try alternative plot 2: choose highest probability of transmission value for each person's infection ------ ####

# what this does: choose highest probability of transmission value for each person's infection then take the median
# across symptomatic status

# only do for csp right now

# first write some code to choose the highest probability values
csp_high_data = csp_subset_data %>%
  group_by(village_name,unq_memID,aim2_exposure,sample_id_human) %>%
  summarize(max_p_te_all_csp = max(p_te_all_csp)) %>%
  group_by(village_name,unq_memID,aim2_exposure) %>%
  summarize(median_max_p_te_all_csp = median(max_p_te_all_csp,na.rm=T))

# now split up the data sets so have separate columns for asymptomatic and symptomatic infections
# for csp
csp_asymp = csp_high_data %>%
  filter(aim2_exposure=="asymptomatic infection") %>%
  select(-c(aim2_exposure)) %>%
  rename("asymp_median_max_p_te_all_csp" = median_max_p_te_all_csp,"asymp_median_max_p_te_all_csp" = median_max_p_te_all_csp)
csp_symp = csp_high_data %>%
  filter(aim2_exposure=="symptomatic infection") %>%
  select(-c(aim2_exposure)) %>%
  rename("symp_median_max_p_te_all_csp" = median_max_p_te_all_csp,"symp_median_max_p_te_all_csp" = median_max_p_te_all_csp)
csp_all = left_join(csp_asymp,csp_symp,by=c("unq_memID"))
csp_all$village_name.y <- NULL
csp_all = rename(csp_all,village_name = village_name.x)
  
# now make a plot of the average probability of p_te_all by symptomatic status
csp_plot = ggplot(csp_all, aes(x=symp_median_max_p_te_all_csp, y=asymp_median_max_p_te_all_csp)) +
  geom_point(aes(color=village_name),size=2.5,pch=21) + 
  theme_bw() +
  geom_abline(intercept = 0, slope = 1,linetype="dashed") +
  xlab("Median max probability of transmission for symptomatic infections") +
  ylab("Median max probability of transmission for asymptomatic infections") +
  labs(color="Village name") +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(limits=c(0,1)) +
  scale_color_manual(values = c("#377eb8","#66bd63","#fb9a99"))
ggsave(csp_plot, filename="/Users/kelseysumner/Desktop/csp_matched_prob_plot_alt2.png", device="png",
       height=5, width=6, units="in", dpi=500)

# check the median of medians by symptomatic status
# for csp
median(csp_all$asymp_median_max_p_te_all_csp)
median(csp_all$symp_median_max_p_te_all_csp)

# sign test
SIGN.test(csp_all$asymp_median_max_p_te_all_csp, csp_all$symp_median_max_p_te_all_csp,md=0,alternative = "greater")



#### -------- try alternative plot 3: compute proportion nonzero pairings with a mosquito ------ ####

# what this does: for each person's infection, compute the proportion of nonzero pairings with mosquito
# then take the median of that proportion across symptomatic status

# only for csp right now

# create a variable that says whether or not p_te_all_csp is non-zero or not
csp_subset_data$nonzero = ifelse(csp_subset_data$p_te_all_csp > 0,"nonzero","zero")
table(csp_subset_data$nonzero,useNA = "always")
length(which(csp_subset_data$p_te_all_csp == 0))

# for each person's infection, count the number of nonzero pairings
csp_part1_data = csp_subset_data %>%
  filter(nonzero == "nonzero") %>%
  group_by(village_name,unq_memID,aim2_exposure,sample_id_human) %>%
  summarize(numerator=n())
csp_nonzero_data = csp_subset_data %>%
  group_by(village_name,unq_memID,aim2_exposure,sample_id_human) %>%
  summarize(denominator = n())

# join the data frames
csp_all_nonzero_data = left_join(csp_nonzero_data,csp_part1_data,by=c("sample_id_human"))

# clean up the data frames
csp_all_nonzero_data$village_name.y <- NULL
csp_all_nonzero_data$aim2_exposure.y <- NULL
csp_all_nonzero_data$unq_memID.y <- NULL
csp_all_nonzero_data = csp_all_nonzero_data %>% rename(village_name = village_name.x,unq_memID=unq_memID.x,aim2_exposure=aim2_exposure.x)

# change the numerators that are NA to 0
csp_all_nonzero_data$numerator[which(is.na(csp_all_nonzero_data$numerator))] = 0

# calculate the proportion of infections that are nonzero
csp_all_nonzero_data$prop_nonzero = csp_all_nonzero_data$numerator/csp_all_nonzero_data$denominator

# create a variable for household ID
csp_all_nonzero_data$HH_ID_human = rep(NA,nrow(csp_all_nonzero_data))
for (i in 1:nrow(csp_all_nonzero_data)) {
  csp_all_nonzero_data$HH_ID_human[i] = str_split(csp_all_nonzero_data$unq_memID[i],"_")[[1]][1]
}
table(csp_all_nonzero_data$HH_ID_human, useNA = "always")

# add covarites to the data set for the regression
colnames(csp_data)
csp_to_merge_data = csp_data %>% select(sample_id_human,pfr364Q_std_combined_rescaled,age_cat_baseline,mosquito_week_count_cat)
csp_all_nonzero_data = left_join(csp_all_nonzero_data,csp_to_merge_data,by="sample_id_human")
csp_all_nonzero_data = unique(csp_all_nonzero_data)

# check the covariates
str(csp_all_nonzero_data$sample_id_human)
str(csp_all_nonzero_data$HH_ID_human)
str(csp_all_nonzero_data$unq_memID)
str(csp_all_nonzero_data$age_cat_baseline)
str(csp_all_nonzero_data$village_name)
csp_all_nonzero_data$village_name = relevel(csp_all_nonzero_data$village_name,ref = "Maruti")
str(csp_all_nonzero_data$mosquito_week_count_cat)
str(csp_all_nonzero_data$pfr364Q_std_combined_rescaled)
str(csp_all_nonzero_data$aim2_exposure)

# do a multi-level logistic regression across symptomatic status
csp_model = glmmTMB(prop_nonzero ~ aim2_exposure + pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = csp_all_nonzero_data)
summary(csp_model)
exp(confint(csp_model,method="Wald"))
# OR: 2.60 (95% CI: 1.38 to 4.91)

table1 = exp(confint(csp_model,method="Wald"))
estimates = c(table1[2,3],NA,table1[3,3],NA,table1[4,3],table1[5,3],NA,table1[6,3],NA,table1[7,3],table1[8,3])
lower_ci = c(table1[2,1],NA,table1[3,1],NA,table1[4,1],table1[5,1],NA,table1[6,1],NA,table1[7,1],table1[8,1])
upper_ci = c(table1[2,2],NA,table1[3,2],NA,table1[4,2],table1[5,2],NA,table1[6,2],NA,table1[7,2],table1[8,2])
names = c("Asymptomatic infection","","Participant asexual parasite density"," ","Participant age >15 years","Participant age 5-15 years","  ","75-147 mosquitoes","   ","Kinesamo village","Sitabicha village")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Asymptomatic infection","","Participant asexual parasite density"," ","Participant age >15 years","Participant age 5-15 years","  ","75-147 mosquitoes","   ","Kinesamo village","Sitabicha village"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Asymptomatic infection","","Participant asexual parasite density"," ","Participant age >15 years","Participant age 5-15 years","  ","75-147 mosquitoes","   ","Kinesamo village","Sitabicha village"))

# create a forest plot
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,1,1,1,1,1,1,1,1,1,1),colour=c("#006d2c","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds ratio (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw() +
  theme(text = element_text(size=25)) 
fp

# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/forest_plot_aim2_model_continuous_outcome_figure3coding.png", device="png",
       height=9, width=12.5, units="in", dpi=400)


# now take the median of that proportion by symptomatic status
csp_final_data = csp_all_nonzero_data %>%
  group_by(village_name,unq_memID,aim2_exposure) %>%
  summarize(median_prop_nonzero = median(prop_nonzero,na.rm = T),num_infections = n())

# now split up the data sets so have separate columns for asymptomatic and symptomatic infections
# for csp
csp_asymp = csp_final_data %>%
  filter(aim2_exposure=="asymptomatic infection") %>%
  select(-c(aim2_exposure)) %>%
  rename("asymp_median_prop_nonzero" = median_prop_nonzero)
csp_symp = csp_final_data %>%
  filter(aim2_exposure=="symptomatic infection") %>%
  select(-c(aim2_exposure)) %>%
  rename("symp_median_prop_nonzero" = median_prop_nonzero)
csp_all = left_join(csp_asymp,csp_symp,by=c("unq_memID"))
csp_all$village_name.y <- NULL
csp_all = rename(csp_all,village_name = village_name.x)
csp_all = data.frame(csp_all)
csp_all$total_infections = csp_all$num_infections.x + csp_all$num_infections.y
csp_all$num_infections.x <- NULL
csp_all$num_infections.y <- NULL

# now add a variable for the age categories
final_data = final_data %>% group_by(unq_memID,age_cat_baseline) 
final_data = data.frame(final_data)
csp_all = left_join(csp_all,final_data,by="unq_memID")
csp_all = unique(csp_all)

# now make a plot of the average probability of p_te_all by symptomatic status
csp_plot = ggplot(csp_all, aes(x=symp_median_prop_nonzero, y=asymp_median_prop_nonzero)) +
  geom_point(aes(color=age_cat_baseline,fill=age_cat_baseline,size=total_infections),pch=21,alpha=0.5) + 
  theme_bw() +
  geom_abline(intercept = 0, slope = 1,linetype="dashed") +
  xlab("Median likelihood transmission for symptomatic infections") +
  ylab("Median likelihood transmission for asymptomatic infections") +
  labs(color="Participant age category",fill="Participant age category",size="Number of malaria infections") +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(limits=c(0,1)) +
  scale_color_manual(values = c("#006d2c","#08519c","#cc4c02")) +
  scale_fill_manual(values = c("#006d2c","#08519c","#cc4c02")) +
  theme(text = element_text(size=12)) 
ggsave(csp_plot, filename="/Users/kelseysumner/Desktop/csp_matched_prob_plot_alt3.png", device="png",
       height=5, width=14, units="in", dpi=500)

# check the median of medians by symptomatic status
# for csp
median(csp_all$asymp_median_prop_nonzero)
median(csp_all$symp_median_prop_nonzero)
IQR(csp_all$asymp_median_prop_nonzero)
IQR(csp_all$symp_median_prop_nonzero)

# sign test
SIGN.test(csp_all$asymp_median_prop_nonzero, csp_all$symp_median_prop_nonzero,md=0,alternative = "greater")

# create a test data set
csp_all_nonzero_data$count = rep(1,nrow(csp_all_nonzero_data))
symptomatic = csp_all_nonzero_data %>% 
  filter(aim2_exposure == "symptomatic infection") %>%
  group_by(unq_memID) %>%
  mutate(num_infections = cumsum(count)) %>%
  select(-c(village_name,aim2_exposure,sample_id_human,denominator,numerator,count))
asymptomatic = csp_all_nonzero_data %>% 
  filter(aim2_exposure == "asymptomatic infection") %>%
  group_by(unq_memID) %>%
  mutate(num_infections = cumsum(count)) %>%
  select(-c(village_name,aim2_exposure,sample_id_human,denominator,numerator,count))
asymptomatic = data.frame(asymptomatic)
symptomatic = data.frame(symptomatic)

# make a matrix where column is the person
asymptomatic_matrix = asymptomatic %>%
  spread(key=unq_memID,value=prop_nonzero)
symptomatic_matrix = symptomatic %>%
  spread(key=unq_memID,value=prop_nonzero)

# now do a test comparing each symptomatic infection to the vector of values of asymptomatic infections
# data_1 should be the vector of values that you want to know the
# the percentile location of in the distribution. 
# you are looking for the location of data_1[i] in column i of the matrix data_2
get_percentile_locations<- function(data_1, data_2){
  percentile_location_vector<- matrix(NA,nrow = nrow(data_1),ncol=ncol(data_1))
  for(i in 1:ncol(data_2)){
    percentile<- ecdf(data_2[,i])
    percentile_location_vector[,i]<- percentile(data_1[,i])
  }
  return(percentile_location_vector)
}
# test out the function
get_percentile_locations(symptomatic_matrix,asymptomatic_matrix)


# now do various cutoffs for what you consider to be a transmission event and rerun models
# make a binary variable for 0 or >0
csp_all_nonzero_data$outcome_binary_lessthan0 = ifelse(csp_all_nonzero_data$prop_nonzero > 0,"greater than 0.00","equal to 0.00")
table(csp_all_nonzero_data$outcome_binary_lessthan0,csp_all_nonzero_data$prop_nonzero,useNA = "always")
table(csp_all_nonzero_data$outcome_binary_lessthan0, useNA = "always")
csp_all_nonzero_data$outcome_binary_lessthan0 = factor(csp_all_nonzero_data$outcome_binary_lessthan0)
levels(csp_all_nonzero_data$outcome_binary_lessthan0)
csp_all_nonzero_data$outcome_binary_lessthan0 = relevel(csp_all_nonzero_data$outcome_binary_lessthan0,ref = "equal to 0.00")

# make a binary variable for <0.05 or >= 0.05
csp_all_nonzero_data$outcome_binary_lessthan0.05 = ifelse(csp_all_nonzero_data$prop_nonzero < 0.05,"less than 0.05","greater than 0.05")
table(csp_all_nonzero_data$outcome_binary_lessthan0.05,csp_all_nonzero_data$prop_nonzero,useNA = "always")
table(csp_all_nonzero_data$outcome_binary_lessthan0.05, useNA = "always")
csp_all_nonzero_data$outcome_binary_lessthan0.05 = factor(csp_all_nonzero_data$outcome_binary_lessthan0.05)
levels(csp_all_nonzero_data$outcome_binary_lessthan0.05)
csp_all_nonzero_data$outcome_binary_lessthan0.05 = relevel(csp_all_nonzero_data$outcome_binary_lessthan0.05,ref = "less than 0.05")

# make a binary variable for <0.1 or >= 0.1
csp_all_nonzero_data$outcome_binary_lessthan0.1 = ifelse(csp_all_nonzero_data$prop_nonzero < 0.1,"less than 0.1","greater than 0.1")
table(csp_all_nonzero_data$outcome_binary_lessthan0.1,csp_all_nonzero_data$prop_nonzero,useNA = "always")
table(csp_all_nonzero_data$outcome_binary_lessthan0.1, useNA = "always")
csp_all_nonzero_data$outcome_binary_lessthan0.1 = factor(csp_all_nonzero_data$outcome_binary_lessthan0.1)
levels(csp_all_nonzero_data$outcome_binary_lessthan0.1)
csp_all_nonzero_data$outcome_binary_lessthan0.1 = relevel(csp_all_nonzero_data$outcome_binary_lessthan0.1,ref = "less than 0.1")

# make a binary variable for <0.15 or >= 0.15
csp_all_nonzero_data$outcome_binary_lessthan0.15 = ifelse(csp_all_nonzero_data$prop_nonzero < 0.15,"less than 0.15","greater than 0.15")
table(csp_all_nonzero_data$outcome_binary_lessthan0.15,csp_all_nonzero_data$prop_nonzero,useNA = "always")
table(csp_all_nonzero_data$outcome_binary_lessthan0.15, useNA = "always")
csp_all_nonzero_data$outcome_binary_lessthan0.15 = factor(csp_all_nonzero_data$outcome_binary_lessthan0.15)
levels(csp_all_nonzero_data$outcome_binary_lessthan0.15)
csp_all_nonzero_data$outcome_binary_lessthan0.15 = relevel(csp_all_nonzero_data$outcome_binary_lessthan0.15,ref = "less than 0.15")

# make a binary variable for <0.2 or >= 0.2
csp_all_nonzero_data$outcome_binary_lessthan0.2 = ifelse(csp_all_nonzero_data$prop_nonzero < 0.2,"less than 0.2","greater than 0.2")
table(csp_all_nonzero_data$outcome_binary_lessthan0.2,csp_all_nonzero_data$prop_nonzero,useNA = "always")
table(csp_all_nonzero_data$outcome_binary_lessthan0.2, useNA = "always")
csp_all_nonzero_data$outcome_binary_lessthan0.2 = factor(csp_all_nonzero_data$outcome_binary_lessthan0.2)
levels(csp_all_nonzero_data$outcome_binary_lessthan0.2)
csp_all_nonzero_data$outcome_binary_lessthan0.2 = relevel(csp_all_nonzero_data$outcome_binary_lessthan0.2,ref = "less than 0.2")

# make a binary variable for <0.25 or >= 0.25
csp_all_nonzero_data$outcome_binary_lessthan0.25 = ifelse(csp_all_nonzero_data$prop_nonzero < 0.25,"less than 0.25","greater than 0.25")
table(csp_all_nonzero_data$outcome_binary_lessthan0.25,csp_all_nonzero_data$prop_nonzero,useNA = "always")
table(csp_all_nonzero_data$outcome_binary_lessthan0.25, useNA = "always")
csp_all_nonzero_data$outcome_binary_lessthan0.25 = factor(csp_all_nonzero_data$outcome_binary_lessthan0.25)
levels(csp_all_nonzero_data$outcome_binary_lessthan0.25)
csp_all_nonzero_data$outcome_binary_lessthan0.25 = relevel(csp_all_nonzero_data$outcome_binary_lessthan0.25,ref = "less than 0.25")

# make a binary variable for <0.3 or >= 0.3
csp_all_nonzero_data$outcome_binary_lessthan0.3 = ifelse(csp_all_nonzero_data$prop_nonzero < 0.3,"less than 0.3","greater than 0.3")
table(csp_all_nonzero_data$outcome_binary_lessthan0.3,csp_all_nonzero_data$prop_nonzero,useNA = "always")
table(csp_all_nonzero_data$outcome_binary_lessthan0.3, useNA = "always")
csp_all_nonzero_data$outcome_binary_lessthan0.3 = factor(csp_all_nonzero_data$outcome_binary_lessthan0.3)
levels(csp_all_nonzero_data$outcome_binary_lessthan0.3)
csp_all_nonzero_data$outcome_binary_lessthan0.3 = relevel(csp_all_nonzero_data$outcome_binary_lessthan0.3,ref = "less than 0.3")

# make a binary variable for <0.35 or >= 0.35
csp_all_nonzero_data$outcome_binary_lessthan0.35 = ifelse(csp_all_nonzero_data$prop_nonzero < 0.35,"less than 0.35","greater than 0.35")
table(csp_all_nonzero_data$outcome_binary_lessthan0.35,csp_all_nonzero_data$prop_nonzero,useNA = "always")
table(csp_all_nonzero_data$outcome_binary_lessthan0.35, useNA = "always")
csp_all_nonzero_data$outcome_binary_lessthan0.35 = factor(csp_all_nonzero_data$outcome_binary_lessthan0.35)
levels(csp_all_nonzero_data$outcome_binary_lessthan0.35)
csp_all_nonzero_data$outcome_binary_lessthan0.35 = relevel(csp_all_nonzero_data$outcome_binary_lessthan0.35,ref = "less than 0.35")

# make a binary variable for <0.4 or >= 0.4
csp_all_nonzero_data$outcome_binary_lessthan0.4 = ifelse(csp_all_nonzero_data$prop_nonzero < 0.4,"less than 0.4","greater than 0.4")
table(csp_all_nonzero_data$outcome_binary_lessthan0.4,csp_all_nonzero_data$prop_nonzero,useNA = "always")
table(csp_all_nonzero_data$outcome_binary_lessthan0.4, useNA = "always")
csp_all_nonzero_data$outcome_binary_lessthan0.4 = factor(csp_all_nonzero_data$outcome_binary_lessthan0.4)
levels(csp_all_nonzero_data$outcome_binary_lessthan0.4)
csp_all_nonzero_data$outcome_binary_lessthan0.4 = relevel(csp_all_nonzero_data$outcome_binary_lessthan0.4,ref = "less than 0.4")

# make a binary variable for <0.45 or >= 0.45
csp_all_nonzero_data$outcome_binary_lessthan0.45 = ifelse(csp_all_nonzero_data$prop_nonzero < 0.45,"less than 0.45","greater than 0.45")
table(csp_all_nonzero_data$outcome_binary_lessthan0.45,csp_all_nonzero_data$prop_nonzero,useNA = "always")
table(csp_all_nonzero_data$outcome_binary_lessthan0.45, useNA = "always")
csp_all_nonzero_data$outcome_binary_lessthan0.45 = factor(csp_all_nonzero_data$outcome_binary_lessthan0.45)
levels(csp_all_nonzero_data$outcome_binary_lessthan0.45)
csp_all_nonzero_data$outcome_binary_lessthan0.45 = relevel(csp_all_nonzero_data$outcome_binary_lessthan0.45,ref = "less than 0.45")

# make a binary variable for <0.5 or >= 0.5
csp_all_nonzero_data$outcome_binary_lessthan0.5 = ifelse(csp_all_nonzero_data$prop_nonzero < 0.5,"less than 0.5","greater than 0.5")
table(csp_all_nonzero_data$outcome_binary_lessthan0.5,csp_all_nonzero_data$prop_nonzero,useNA = "always")
table(csp_all_nonzero_data$outcome_binary_lessthan0.5, useNA = "always")
csp_all_nonzero_data$outcome_binary_lessthan0.5 = factor(csp_all_nonzero_data$outcome_binary_lessthan0.5)
levels(csp_all_nonzero_data$outcome_binary_lessthan0.5)
csp_all_nonzero_data$outcome_binary_lessthan0.5 = relevel(csp_all_nonzero_data$outcome_binary_lessthan0.5,ref = "less than 0.5")

# make a binary variable for <0.55 or >= 0.55
csp_all_nonzero_data$outcome_binary_lessthan0.55 = ifelse(csp_all_nonzero_data$prop_nonzero < 0.55,"less than 0.55","greater than 0.55")
table(csp_all_nonzero_data$outcome_binary_lessthan0.55,csp_all_nonzero_data$prop_nonzero,useNA = "always")
table(csp_all_nonzero_data$outcome_binary_lessthan0.55, useNA = "always")
csp_all_nonzero_data$outcome_binary_lessthan0.55 = factor(csp_all_nonzero_data$outcome_binary_lessthan0.55)
levels(csp_all_nonzero_data$outcome_binary_lessthan0.55)
csp_all_nonzero_data$outcome_binary_lessthan0.55 = relevel(csp_all_nonzero_data$outcome_binary_lessthan0.55,ref = "less than 0.55")

# binary outcome 0 with a logistic model - this is basically a hurdle model
model0 <- glmmTMB(outcome_binary_lessthan0~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = csp_all_nonzero_data)
summary(model0)
exp(confint(model0,method="Wald"))

# binary outcome <0.05 with a logistic model
model.05 <- glmmTMB(outcome_binary_lessthan0.05~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat +village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = csp_all_nonzero_data)
summary(model.05)
exp(confint(model.05,method="Wald"))
# converged

# binary outcome <0.1 with a logistic model
model.1 <- glmmTMB(outcome_binary_lessthan0.1~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat +village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = csp_all_nonzero_data)
summary(model.1)
exp(confint(model.1,method="Wald"))
# converged

# binary outcome <0.15 with a logistic model
model.15 <- glmmTMB(outcome_binary_lessthan0.15~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat +village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = csp_all_nonzero_data)
summary(model.15)
exp(confint(model.15,method="Wald"))
# converged

# binary outcome <0.2 with a logistic model
model.2 <- glmmTMB(outcome_binary_lessthan0.2~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = csp_all_nonzero_data)
summary(model.2)
exp(confint(model.2, method="Wald"))
# converged

# binary outcome <0.25 with a logistic model
model.25 <- glmmTMB(outcome_binary_lessthan0.25~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = csp_all_nonzero_data)
summary(model.25)
exp(confint(model.25, method="Wald"))
# converged

# binary outcome <0.3 with a logistic model
model.3 <-  glmmTMB(outcome_binary_lessthan0.3~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = csp_all_nonzero_data)
summary(model.3)
exp(confint(model.3, method="Wald"))
# converged

# binary outcome <0.35 with a logistic model
model.35 <- glmmTMB(outcome_binary_lessthan0.35~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = csp_all_nonzero_data)
summary(model.35)
exp(confint(model.35, method="Wald"))
# converged

# binary outcome <0.4 with a logistic model
model.4 <- glmmTMB(outcome_binary_lessthan0.4~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = csp_all_nonzero_data)
summary(model.4)
exp(confint(model.4, method="Wald")) 
# converged

# binary outcome <0.45 with a logistic model
model.45 <- glmmTMB(outcome_binary_lessthan0.45~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = csp_all_nonzero_data)
summary(model.45)
exp(confint(model.45, method="Wald")) 
# converged

# binary outcome <0.5 with a logistic model
model.5 <- glmmTMB(outcome_binary_lessthan0.5~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat +village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = csp_all_nonzero_data)
summary(model.5)
exp(confint(model.5, method="Wald")) 
# converged

# binary outcome <0.55 with a logistic model
model.55 <- glmmTMB(outcome_binary_lessthan0.55~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat +village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = csp_all_nonzero_data)
summary(model.55)
exp(confint(model.55, method="Wald")) 
# converged

# read in the model results
model_results = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/computational_model_materials/aim2_binary_outcome_final_for_figure3coding.csv")

# try another way to make this plot
model_plot = ggplot(data=model_results,aes(x=binary_outcome,y=estimate,group=1),cex=1.5,col="#006d2c") +
  geom_line(data=model_results,aes(x=binary_outcome,y=estimate,group=1),cex=1.5,col="#006d2c") +
  geom_ribbon(data=model_results,aes(x=1:length(binary_outcome),ymin = lower_ci, ymax = upper_ci),alpha=0.2,fill="#006d2c") +
  theme_bw() +
  xlab("Cutoff for what is a transmission event") + 
  ylab("Odds ratio (95% CI)") + 
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8),trans="log10") +
  geom_hline(yintercept=1,linetype="dashed") + 
  coord_flip() +
  theme(text = element_text(size=18)) 
model_plot

ggsave(model_plot, filename="/Users/kelseysumner/Desktop/binary_coding_model_plot_for_figure3.png", device="png",
       height=7, width=8, units="in", dpi=500)



#### -------- now do plot 3 for ama: compute proportion nonzero pairings with a mosquito ------ ####

# what this does: for each person's infection, compute the proportion of nonzero pairings with mosquito
# then take the median of that proportion across symptomatic status

# only for ama right now

# create a variable that says whether or not p_te_all_csp is non-zero or not
ama_subset_data$nonzero = ifelse(ama_subset_data$p_te_all_ama > 0,"nonzero","zero")
table(ama_subset_data$nonzero,useNA = "always")
length(which(ama_subset_data$p_te_all_ama == 0))

# for each person's infection, count the number of nonzero pairings
ama_part1_data = ama_subset_data %>%
  filter(nonzero == "nonzero") %>%
  group_by(village_name,unq_memID,aim2_exposure,sample_id_human) %>%
  summarize(numerator=n())
ama_nonzero_data = ama_subset_data %>%
  group_by(village_name,unq_memID,aim2_exposure,sample_id_human) %>%
  summarize(denominator = n())

# join the data frames
ama_all_nonzero_data = left_join(ama_nonzero_data,ama_part1_data,by=c("sample_id_human"))

# clean up the data frames
ama_all_nonzero_data$village_name.y <- NULL
ama_all_nonzero_data$aim2_exposure.y <- NULL
ama_all_nonzero_data$unq_memID.y <- NULL
ama_all_nonzero_data = ama_all_nonzero_data %>% rename(village_name = village_name.x,unq_memID=unq_memID.x,aim2_exposure=aim2_exposure.x)

# change the numerators that are NA to 0
ama_all_nonzero_data$numerator[which(is.na(ama_all_nonzero_data$numerator))] = 0

# calculate the proportion of infections that are nonzero
ama_all_nonzero_data$prop_nonzero = ama_all_nonzero_data$numerator/ama_all_nonzero_data$denominator

# now take the median of that proportion by symptomatic status
ama_final_data = ama_all_nonzero_data %>%
  group_by(village_name,unq_memID,aim2_exposure) %>%
  summarize(median_prop_nonzero = median(prop_nonzero,na.rm = T),num_infections = n())

# now split up the data sets so have separate columns for asymptomatic and symptomatic infections
# for ama
ama_asymp = ama_final_data %>%
  filter(aim2_exposure=="asymptomatic infection") %>%
  select(-c(aim2_exposure)) %>%
  rename("asymp_median_prop_nonzero" = median_prop_nonzero)
ama_symp = ama_final_data %>%
  filter(aim2_exposure=="symptomatic infection") %>%
  select(-c(aim2_exposure)) %>%
  rename("symp_median_prop_nonzero" = median_prop_nonzero)
ama_all = left_join(ama_asymp,ama_symp,by=c("unq_memID"))
ama_all$village_name.y <- NULL
ama_all = rename(ama_all,village_name = village_name.x)
ama_all = data.frame(ama_all)
ama_all$total_infections = ama_all$num_infections.x + ama_all$num_infections.y
ama_all$num_infections.x <- NULL
ama_all$num_infections.y <- NULL

# now add a variable for the age categories
ama_all = left_join(ama_all,final_data,by="unq_memID")
ama_all = unique(ama_all)

# now make a plot of the average probability of p_te_all by symptomatic status
ama_plot = ggplot(ama_all, aes(x=symp_median_prop_nonzero, y=asymp_median_prop_nonzero)) +
  geom_point(aes(color=age_cat_baseline,fill=age_cat_baseline,size=total_infections),pch=21,alpha=0.5) + 
  theme_bw() +
  geom_abline(intercept = 0, slope = 1,linetype="dashed") +
  xlab("Median likelihood transmission for symptomatic infections") +
  ylab("Median likelihood transmission for asymptomatic infections") +
  labs(color="Participant age category",fill="Participant age category",size="Number of malaria infections") +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(limits=c(0,1)) +
  scale_color_manual(values = c("#006d2c","#08519c","#cc4c02")) +
  scale_fill_manual(values = c("#006d2c","#08519c","#cc4c02"))
ggsave(ama_plot, filename="/Users/kelseysumner/Desktop/ama_matched_prob_plot_alt3.png", device="png",
       height=5, width=8, units="in", dpi=500)

# check the median of medians by symptomatic status
# for ama
median(ama_all$asymp_median_prop_nonzero)
median(ama_all$symp_median_prop_nonzero)
IQR(ama_all$asymp_median_prop_nonzero)
IQR(ama_all$symp_median_prop_nonzero)

# sign test
SIGN.test(ama_all$asymp_median_prop_nonzero, ama_all$symp_median_prop_nonzero,md=0,alternative = "greater")

# add covariates to the data set for the regression
colnames(ama_data)
ama_to_merge_data = ama_data %>% select(sample_id_human,pfr364Q_std_combined_rescaled,age_cat_baseline,mosquito_week_count_cat)
ama_all_nonzero_data = left_join(ama_all_nonzero_data,ama_to_merge_data,by="sample_id_human")
ama_all_nonzero_data = unique(ama_all_nonzero_data)

# create a variable for household ID
ama_all_nonzero_data$HH_ID_human = rep(NA,nrow(ama_all_nonzero_data))
for (i in 1:nrow(ama_all_nonzero_data)) {
  ama_all_nonzero_data$HH_ID_human[i] = str_split(ama_all_nonzero_data$unq_memID[i],"_")[[1]][1]
}
table(ama_all_nonzero_data$HH_ID_human, useNA = "always")

# check the covariates
str(ama_all_nonzero_data$sample_id_human)
str(ama_all_nonzero_data$HH_ID_human)
str(ama_all_nonzero_data$unq_memID)
str(ama_all_nonzero_data$age_cat_baseline)
str(ama_all_nonzero_data$village_name)
ama_all_nonzero_data$village_name = relevel(ama_all_nonzero_data$village_name,ref = "Maruti")
str(ama_all_nonzero_data$mosquito_week_count_cat)
str(ama_all_nonzero_data$pfr364Q_std_combined_rescaled)
str(ama_all_nonzero_data$aim2_exposure)

# do a multi-level logistic regression across symptomatic status
ama_model = glmmTMB(prop_nonzero ~ aim2_exposure + pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = ama_all_nonzero_data)
summary(ama_model)
exp(confint(ama_model,method="Wald"))
# OR: 1.26 (95% CI: 0.61 to 2.62)

table1 = exp(confint(ama_model,method="Wald"))
estimates = c(table1[2,3],NA,table1[3,3],NA,table1[4,3],table1[5,3],NA,table1[6,3],NA,table1[7,3],table1[8,3])
lower_ci = c(table1[2,1],NA,table1[3,1],NA,table1[4,1],table1[5,1],NA,table1[6,1],NA,table1[7,1],table1[8,1])
upper_ci = c(table1[2,2],NA,table1[3,2],NA,table1[4,2],table1[5,2],NA,table1[6,2],NA,table1[7,2],table1[8,2])
names = c("Asymptomatic infection","","Participant asexual parasite density"," ","Participant age >15 years","Participant age 5-15 years","  ","75-147 mosquitoes","   ","Kinesamo village","Sitabicha village")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Asymptomatic infection","","Participant asexual parasite density"," ","Participant age >15 years","Participant age 5-15 years","  ","75-147 mosquitoes","   ","Kinesamo village","Sitabicha village"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Asymptomatic infection","","Participant asexual parasite density"," ","Participant age >15 years","Participant age 5-15 years","  ","75-147 mosquitoes","   ","Kinesamo village","Sitabicha village"))

# create a forest plot
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,1,1,1,1,1,1,1,1,1,1),colour=c("#006d2c","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds ratio (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw() +
  theme(text = element_text(size=25)) 
fp

# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/forest_plot_aim2_model_continuous_outcome_figure3coding_ama_supplement.png", device="png",
       height=9, width=12.5, units="in", dpi=400)
