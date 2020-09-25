# -------------------------------- #
#   Population genetic analysis    #
#        TriCEM project            #
#       September 24, 2020         #
#           K. Sumner              #
# -------------------------------- #

#### ------- load libraries -------- ####
library(tidyverse)
library(lme4)
library(apex)
library(adegenet)
library(pegas)
library(mmod)
library(poppr)
library(schoolmath)
library(Biostrings)


#### ------- read in the data set ------- ####

# read in the kinesamo data for the first high transmission season (06-17 to 10-17)
kinesamo_data = read_rds("Desktop/Dissertation Materials/TriCEM work/data/tricem_model_data_kinesamo_first_season_17SEP2020.rds")
# read in the maruti data for the first high transmission season (06-17 to 10-17)
maruti_data = read_rds("Desktop/Dissertation Materials/TriCEM work/data/tricem_model_data_maruti_first_season_17SEP2020.rds")

# read in the human demographic data
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/final_recoded_data_set/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1MAR2020.rds")

# read in the merged anopheles mosquito data set
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in the haplotype sequence fasta file
haplotype_sequences = read_tsv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Sequencing Information/Haplotype Results/CSP/final censored haplotype output/spat21_CSP_uniqueSeqs_final_censored.fasta")

# also read in the haplotype sequences in fasta format (same as above just in a slightly different format in R)
haplotype_fasta = read.multiFASTA(c("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Sequencing Information/Haplotype Results/CSP/final censored haplotype output/spat21_CSP_uniqueSeqs_final_censored.fasta","Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Sequencing Information/Haplotype Results/AMA/final censored haplotype output/spat21_AMA_uniqueSeqs_final_censored.fasta"))



#### ---- reorganize the haplotype sequence file ------- ####

# reorganize the haplotype sequence fasta file to be in a better format
sequence_names = rep(NA,298) # number is number of haplotypes in fasta file of unique haplotype sequences
sequences = rep(NA,298)
sequence_names[1] = ">Seq1"
for (i in 1:nrow(haplotype_sequences)) {
  if (is.even(i)) {
    sequence_names[i+1] = haplotype_sequences$`>Seq1`[i]
  }
  if (is.odd(i)){
    sequences[i] = haplotype_sequences$`>Seq1`[i]
  }
}
new_haplotype_sequences = data.frame(sequence_names,sequences)
new_haplotype_sequences = na.omit(new_haplotype_sequences)
new_haplotype_sequences$sequences = as.character(new_haplotype_sequences$sequences)
new_haplotype_sequences$sequence_names = as.character(new_haplotype_sequences$sequence_names)

# change the sequence names 
ids = rep(NA,nrow(new_haplotype_sequences))
for (i in 1:nrow(new_haplotype_sequences)){
  ids[i] = paste0("H",substring(new_haplotype_sequences$sequence_names[i], 5))
}
new_haplotype_sequences$ids = ids


#### -------- calculate fst for each population and genetic structure ------ ####

# create a large for loop where each row in the data frame has the haplotypes pulled out and fst calculated

## -- first do for kinesamo

# first create empty vectors to populate
avg_snp_diff_human = rep(NA,nrow(kinesamo_data))
avg_snp_diff_abdomen = rep(NA,nrow(kinesamo_data))
avg_snp_diff_head = rep(NA,nrow(kinesamo_data))
avg_snp_tot_human = rep(NA,nrow(kinesamo_data))
avg_snp_tot_abdomen = rep(NA,nrow(kinesamo_data))
avg_snp_tot_head = rep(NA,nrow(kinesamo_data))

# then create the for loop
for (i in 1:nrow(kinesamo_data)){
  
  # pull out the sample size for each time window
  human_size = kinesamo_data$human_infections[i]
  abdomen_size = kinesamo_data$abdomen_infections[i]
  head_size = kinesamo_data$head_infections[i]
  
  # pull out the haplotypes for the time window
  human_haplotypes = str_split(kinesamo_data$human_haplotypes[i],",")[[1]]
  abdomen_haplotypes = str_split(kinesamo_data$abdomen_haplotypes[i],",")[[1]]
  head_haplotypes = str_split(kinesamo_data$head_haplotypes[i],",")[[1]]
  population_haplotypes = unique(c(human_haplotypes,abdomen_haplotypes,head_haplotypes))
  
  # create haplotype data frames for the time window
  human_df = new_haplotype_sequences %>% filter(ids %in% human_haplotypes)
  abdomen_df = new_haplotype_sequences %>% filter(ids %in% abdomen_haplotypes)
  head_df = new_haplotype_sequences %>% filter(ids %in% head_haplotypes)
  population_df = new_haplotype_sequences %>% filter(ids %in% population_haplotypes)
  
  # make the haplotype data frames dna string sets
  # for humans
  human_df = DNAStringSet(human_df$sequences)
  human_snp_output = stringDist(human_df, method="hamming")
  human_snp_output = as.matrix(human_snp_output)
  # for mosquito abdomens
  abdomen_df = DNAStringSet(abdomen_df$sequences)
  abdomen_snp_output = stringDist(abdomen_df, method="hamming")
  abdomen_snp_output = as.matrix(abdomen_snp_output)
  # for mosquito heads
  head_df = DNAStringSet(head_df$sequences)
  head_snp_output = stringDist(head_df, method="hamming")
  head_snp_output = as.matrix(head_snp_output)
  # for population
  population_df = DNAStringSet(population_df$sequences)
  population_snp_output = stringDist(population_df, method="hamming")
  population_snp_output = as.matrix(population_snp_output)
  
  # calculate the average snp difference value
  avg_snp_diff_human[i] = (abs(mean(population_snp_output) - mean(human_snp_output)))/mean(population_snp_output)
  avg_snp_diff_abdomen[i] = (abs(mean(population_snp_output) - mean(abdomen_snp_output)))/mean(population_snp_output)
  avg_snp_diff_head[i] = (abs(mean(population_snp_output) - mean(head_snp_output)))/mean(population_snp_output)
  
  # calculate the average snp total value
  avg_snp_tot_human[i] = mean(human_snp_output)
  avg_snp_tot_abdomen[i] = mean(abdomen_snp_output)
  avg_snp_tot_head[i] = mean(head_snp_output)

}

# add the average snp difference values to the data set
kinesamo_data$avg_snp_diff_human = avg_snp_diff_human
kinesamo_data$avg_snp_diff_abdomen = avg_snp_diff_abdomen
kinesamo_data$avg_snp_diff_head = avg_snp_diff_head
summary(kinesamo_data$avg_snp_diff_human)
summary(kinesamo_data$avg_snp_diff_abdomen)
summary(kinesamo_data$avg_snp_diff_head)

# add the total snp values to the data set
kinesamo_data$avg_snp_tot_human = avg_snp_tot_human
kinesamo_data$avg_snp_tot_abdomen = avg_snp_tot_abdomen
kinesamo_data$avg_snp_tot_head = avg_snp_tot_head
summary(kinesamo_data$avg_snp_tot_human)
summary(kinesamo_data$avg_snp_tot_abdomen)
summary(kinesamo_data$avg_snp_tot_head)


## ---- then do for maruti

# first create empty vectors to populate
avg_snp_diff_human = rep(NA,nrow(maruti_data))
avg_snp_diff_abdomen = rep(NA,nrow(maruti_data))
avg_snp_diff_head = rep(NA,nrow(maruti_data))
avg_snp_tot_human = rep(NA,nrow(maruti_data))
avg_snp_tot_abdomen = rep(NA,nrow(maruti_data))
avg_snp_tot_head = rep(NA,nrow(maruti_data))

# then create the for loop
for (i in 1:nrow(maruti_data)){
  
  # pull out the sample size for each time window
  human_size = maruti_data$human_infections[i]
  abdomen_size = maruti_data$abdomen_infections[i]
  head_size = maruti_data$head_infections[i]
  
  # pull out the haplotypes for the time window
  human_haplotypes = str_split(maruti_data$human_haplotypes[i],",")[[1]]
  abdomen_haplotypes = str_split(maruti_data$abdomen_haplotypes[i],",")[[1]]
  head_haplotypes = str_split(maruti_data$head_haplotypes[i],",")[[1]]
  population_haplotypes = unique(c(human_haplotypes,abdomen_haplotypes,head_haplotypes))
  
  # create haplotype data frames for the time window
  human_df = new_haplotype_sequences %>% filter(ids %in% human_haplotypes)
  abdomen_df = new_haplotype_sequences %>% filter(ids %in% abdomen_haplotypes)
  head_df = new_haplotype_sequences %>% filter(ids %in% head_haplotypes)
  population_df = new_haplotype_sequences %>% filter(ids %in% population_haplotypes)
  
  # make the haplotype data frames dna string sets
  # for humans
  human_df = DNAStringSet(human_df$sequences)
  human_snp_output = stringDist(human_df, method="hamming")
  human_snp_output = as.matrix(human_snp_output)
  # for mosquito abdomens
  abdomen_df = DNAStringSet(abdomen_df$sequences)
  abdomen_snp_output = stringDist(abdomen_df, method="hamming")
  abdomen_snp_output = as.matrix(abdomen_snp_output)
  # for mosquito heads
  head_df = DNAStringSet(head_df$sequences)
  head_snp_output = stringDist(head_df, method="hamming")
  head_snp_output = as.matrix(head_snp_output)
  # for population
  population_df = DNAStringSet(population_df$sequences)
  population_snp_output = stringDist(population_df, method="hamming")
  population_snp_output = as.matrix(population_snp_output)
  
  # calculate the average snp difference value
  avg_snp_diff_human[i] = (abs(mean(population_snp_output) - mean(human_snp_output)))/mean(population_snp_output)
  avg_snp_diff_abdomen[i] = (abs(mean(population_snp_output) - mean(abdomen_snp_output)))/mean(population_snp_output)
  avg_snp_diff_head[i] = (abs(mean(population_snp_output) - mean(head_snp_output)))/mean(population_snp_output)
  
  # calculate the average snp total value
  avg_snp_tot_human[i] = mean(human_snp_output)
  avg_snp_tot_abdomen[i] = mean(abdomen_snp_output)
  avg_snp_tot_head[i] = mean(head_snp_output)
  
}

# add the average snp difference values to the data set
maruti_data$avg_snp_diff_human = avg_snp_diff_human
maruti_data$avg_snp_diff_abdomen = avg_snp_diff_abdomen
maruti_data$avg_snp_diff_head = avg_snp_diff_head
summary(maruti_data$avg_snp_diff_human)
summary(maruti_data$avg_snp_diff_abdomen)
summary(maruti_data$avg_snp_diff_head)

# add the total snp values to the data set
maruti_data$avg_snp_tot_human = avg_snp_tot_human
maruti_data$avg_snp_tot_abdomen = avg_snp_tot_abdomen
maruti_data$avg_snp_tot_head = avg_snp_tot_head
summary(maruti_data$avg_snp_tot_human)
summary(maruti_data$avg_snp_tot_abdomen)
summary(maruti_data$avg_snp_tot_head)



#### ----- change the data sets from wide to long format ------ ####

## -- for kinesamo data

# cut down the data set to each sample type
colnames(kinesamo_data)
# for humans
k_human = kinesamo_data %>% 
  select(starting_date,ending_date,human_haplotypes,human_num_unique,human_id_list,human_infections,avg_snp_diff_human,avg_snp_tot_human) %>%
  rename(haplotype_list = human_haplotypes,num_haplotypes = human_num_unique,id_list = human_id_list,num_infections = human_infections,avg_snp_diff = avg_snp_diff_human,avg_snp_tot = avg_snp_tot_human) %>%
  mutate(sample_type = rep("Human",nrow(kinesamo_data)))
# for mosquito heads
k_head = kinesamo_data %>% 
  select(starting_date,ending_date,head_haplotypes,head_num_unique,head_id_list,head_infections,avg_snp_diff_head,avg_snp_tot_head) %>%
  rename(haplotype_list = head_haplotypes,num_haplotypes = head_num_unique,id_list = head_id_list,num_infections = head_infections,avg_snp_diff = avg_snp_diff_head,avg_snp_tot = avg_snp_tot_head) %>%
  mutate(sample_type = rep("Mosquito Head",nrow(kinesamo_data)))
# for mosquito abdomens
k_abdomen = kinesamo_data %>% 
  select(starting_date,ending_date,abdomen_haplotypes,abdomen_num_unique,abdomen_id_list,abdomen_infections,avg_snp_diff_abdomen,avg_snp_tot_abdomen) %>%
  rename(haplotype_list = abdomen_haplotypes,num_haplotypes = abdomen_num_unique,id_list = abdomen_id_list,num_infections = abdomen_infections,avg_snp_diff = avg_snp_diff_abdomen,avg_snp_tot = avg_snp_tot_abdomen) %>%
  mutate(sample_type = rep("Mosquito Abdomen",nrow(kinesamo_data)))

# now combine the sample types
kinesamo_data_long = rbind(k_human,k_head,k_abdomen)


## -- for maruti data

# cut down the data set to each sample type
colnames(maruti_data)
# for humans
m_human = maruti_data %>% 
  select(starting_date,ending_date,human_haplotypes,human_num_unique,human_id_list,human_infections,avg_snp_diff_human,avg_snp_tot_human) %>%
  rename(haplotype_list = human_haplotypes,num_haplotypes = human_num_unique,id_list = human_id_list,num_infections = human_infections,avg_snp_diff = avg_snp_diff_human,avg_snp_tot = avg_snp_tot_human) %>%
  mutate(sample_type = rep("Human",nrow(maruti_data)))
# for mosquito heads
m_head = maruti_data %>% 
  select(starting_date,ending_date,head_haplotypes,head_num_unique,head_id_list,head_infections,avg_snp_diff_head,avg_snp_tot_head) %>%
  rename(haplotype_list = head_haplotypes,num_haplotypes = head_num_unique,id_list = head_id_list,num_infections = head_infections,avg_snp_diff = avg_snp_diff_head,avg_snp_tot = avg_snp_tot_head) %>%
  mutate(sample_type = rep("Mosquito Head",nrow(maruti_data)))
# for mosquito abdomens
m_abdomen = maruti_data %>% 
  select(starting_date,ending_date,abdomen_haplotypes,abdomen_num_unique,abdomen_id_list,abdomen_infections,avg_snp_diff_abdomen,avg_snp_tot_abdomen) %>%
  rename(haplotype_list = abdomen_haplotypes,num_haplotypes = abdomen_num_unique,id_list = abdomen_id_list,num_infections = abdomen_infections,avg_snp_diff = avg_snp_diff_abdomen,avg_snp_tot = avg_snp_tot_abdomen) %>%
  mutate(sample_type = rep("Mosquito Abdomen",nrow(maruti_data)))

# now combine the sample types
maruti_data_long = rbind(m_human,m_head,m_abdomen)





#### ------ run a multilevel logisitic regression model for the population structure over time ------ ####

## ---- make a model for Kinesamo

## --- now do for Maruti

# set up a linear regression
# with a random intercept for the date
model_1 = lmer(avg_snp_tot ~ as.factor(sample_type) + (1|starting_date),data=kinesamo_data_long)
summary(model_1)
performance::icc(model_1)
confint(model_1,method="Wald")

# lets create ipw weights for the number of infections
exposureModel <- glmer(factor(sample_type) ~ num_infections + (1|starting_date), data = kinesamo_data_long, family = binomial(link="logit"))
pA = predict(exposureModel, type = "response")
IPW = 1/pA
summary(exposureModel)
kinesamo_data_long$pA = pA
kinesamo_data_long$IPW = IPW
summary(kinesamo_data_long$pA)
summary(kinesamo_data_long$IPW)

# set up a linear regression
# with a random intercept for the date
# and IPW weights for the number of infections
model_1 = lmer(avg_snp_tot ~ as.factor(sample_type) + (1|starting_date), weights = IPW, data=kinesamo_data_long)
summary(model_1)
performance::icc(model_1)
confint(model_1,method="Wald")

# now make a forest plot of the model results
table1 = confint(model_1,method="Wald")
estimates = c(0.37464,0.24501)
lower_ci = c(table1[4,1],table1[5,1])
upper_ci = c(table1[4,2],table1[5,2])
names = c("Mosquito abdomen vs. human","Mosquito head vs. human")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Mosquito abdomen vs. human","Mosquito head vs. human"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Mosquito abdomen vs. human","Mosquito head vs. human"))
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(1,1),colour = c("#1f78b4","#b2df8a")) + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=0 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Risk difference (95% CI)") +
  scale_y_continuous(limits=c(-0.2,1)) +
  theme_bw()
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/forest_plot_kinesamo_population_diversity.png", device="png",
       height=3, width=5, units="in", dpi=400)


## --- now do for Maruti

# set up a linear regression
# with a random intercept for the date
model_1 = lmer(avg_snp_tot ~ as.factor(sample_type) + (1|starting_date),data=maruti_data_long)
summary(model_1)
performance::icc(model_1)
confint(model_1,method="Wald")

# lets create ipw weights for the number of infections
exposureModel <- glmer(factor(sample_type) ~ num_infections + (1|starting_date), data = maruti_data_long, family = binomial(link="logit"))
pA = predict(exposureModel, type = "response")
IPW = 1/pA
summary(exposureModel)
maruti_data_long$pA = pA
maruti_data_long$IPW = IPW
summary(maruti_data_long$pA)
summary(maruti_data_long$IPW)

# set up a linear regression
# with a random intercept for the date
# and IPW weights for the number of infections
model_1 = lmer(avg_snp_tot ~ as.factor(sample_type) + (1|starting_date), weights = IPW, data=maruti_data_long)
summary(model_1)
performance::icc(model_1)
confint(model_1,method="Wald")

# now make a forest plot of the model results
table1 = confint(model_1,method="Wald")
estimates = c(0.62838,0.30314)
lower_ci = c(table1[4,1],table1[5,1])
upper_ci = c(table1[4,2],table1[5,2])
names = c("Mosquito abdomen vs. human","Mosquito head vs. human")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Mosquito abdomen vs. human","Mosquito head vs. human"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Mosquito abdomen vs. human","Mosquito head vs. human"))
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(1,1),colour = c("#1f78b4","#b2df8a")) + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=0 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Risk difference (95% CI)") +
  scale_y_continuous(limits=c(-0.2,1)) +
  theme_bw()
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/forest_plot_maruti_population_diversity.png", device="png",
       height=3, width=5, units="in", dpi=400)



#### ----- make a plot for the average number of snps over time ------- ####

# for kinesamo
kinesamo_maximum_amounts = kinesamo_data_long %>%
  group_by(starting_date) %>%
  summarize(max_val = max(avg_snp_tot), min_val = min(avg_snp_tot))
# make the plot
plot_6 = ggplot() +
  geom_point(data=kinesamo_data_long,aes(x=starting_date,y=avg_snp_tot,group=sample_type,shape=sample_type,colour=sample_type),alpha=0.7) +
  geom_linerange(data=kinesamo_maximum_amounts,aes(x = starting_date, ymax = max_val, ymin = min_val),colour="black") +
  theme_bw() +
  ylab("Average number of SNPs within each time window") +
  labs(shape="Sample type",colour="Sample type") +
  xlab("Starting date for each 30-day time window") +
  theme(legend.position = c(0.2,0.2),legend.box.background = element_rect(colour = "black"),axis.text.x = element_text(angle = 90)) +
  scale_x_date(date_breaks = "1 week",)
plot_6
ggsave(plot_6, filename="/Users/kelseysumner/Desktop/kinesamo_number_snps_fancy.png", device="png",
       height=5, width=8, units="in", dpi=400)

# for maruti
maruti_maximum_amounts = maruti_data_long %>%
  group_by(starting_date) %>%
  summarize(max_val = max(avg_snp_tot), min_val = min(avg_snp_tot))
# make the plot
plot_6 = ggplot() +
  geom_point(data=maruti_data_long,aes(x=starting_date,y=avg_snp_tot,group=sample_type,shape=sample_type,colour=sample_type),alpha=0.7) +
  geom_linerange(data=maruti_maximum_amounts,aes(x = starting_date, ymax = max_val, ymin = min_val),colour="black") +
  theme_bw() +
  ylab("Average number of SNPs within each time window") +
  labs(shape="Sample type",colour="Sample type") +
  xlab("Starting date for each 30-day time window") +
  theme(legend.position = c(0.2,0.2),legend.box.background = element_rect(colour = "black"),axis.text.x = element_text(angle = 90)) +
  scale_x_date(date_breaks = "1 week",)
plot_6
ggsave(plot_6, filename="/Users/kelseysumner/Desktop/maruti_number_snps_fancy.png", device="png",
       height=5, width=8, units="in", dpi=400)




#### ------- vignette for population structure from sequencing data ------- ####

## the pipeline is set up for two gene targets so doing the workflow for ama and csp

# first make a plot of the haplotype fasta file
plot(haplotype_fasta,cex=.2)

# now check the file name format - looks good
getLocusNames(haplotype_fasta)

# creating genind object by multilocus sequence types
haplotype_fasta.gid <- multidna2genind(haplotype_fasta, mlst = TRUE)
haplotype_fasta.gid

# set the population strata
# not sure how to set this up well




