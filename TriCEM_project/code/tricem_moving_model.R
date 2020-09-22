# -------------------------------- #
#     Start coding moving model    #
#        TriCEM project            #
#       September 18, 2020         #
#           K. Sumner              #
# -------------------------------- #

#### ------- load libraries -------- ####
library(tidyverse)
library(lme4)



#### ------- read in the data set ------- ####

# read in the kinesamo data for the first high transmission season (06-17 to 10-17)
kinesamo_data = read_rds("Desktop/Dissertation Materials/TriCEM work/data/tricem_model_data_kinesamo_first_season_17SEP2020.rds")
# read in the maruti data for the first high transmission season (06-17 to 10-17)
maruti_data = read_rds("Desktop/Dissertation Materials/TriCEM work/data/tricem_model_data_maruti_first_season_17SEP2020.rds")


#### ---- make long versions of both data sets ----- ####

## -- for kinesamo data

# cut down the data set to each sample type
colnames(kinesamo_data)
# for humans
k_human = kinesamo_data %>% 
  select(starting_date,ending_date,human_haplotypes,human_num_unique,human_id_list,human_infections) %>%
  rename(haplotype_list = human_haplotypes,num_haplotypes = human_num_unique,id_list = human_id_list,num_infections = human_infections) %>%
  mutate(sample_type = rep("Human",nrow(kinesamo_data)))
# for mosquito heads
k_head = kinesamo_data %>% 
  select(starting_date,ending_date,head_haplotypes,head_num_unique,head_id_list,head_infections) %>%
  rename(haplotype_list = head_haplotypes,num_haplotypes = head_num_unique,id_list = head_id_list,num_infections = head_infections) %>%
  mutate(sample_type = rep("Mosquito Head",nrow(kinesamo_data)))
# for mosquito abdomens
k_abdomen = kinesamo_data %>% 
  select(starting_date,ending_date,abdomen_haplotypes,abdomen_num_unique,abdomen_id_list,abdomen_infections) %>%
  rename(haplotype_list = abdomen_haplotypes,num_haplotypes = abdomen_num_unique,id_list = abdomen_id_list,num_infections = abdomen_infections) %>%
  mutate(sample_type = rep("Mosquito Abdomen",nrow(kinesamo_data)))

# now combine the sample types
kinesamo_data_long = rbind(k_human,k_head,k_abdomen)


## -- for maruti data

# cut down the data set to each sample type
colnames(maruti_data)
# for humans
m_human = maruti_data %>% 
  select(starting_date,ending_date,human_haplotypes,human_num_unique,human_id_list,human_infections) %>%
  rename(haplotype_list = human_haplotypes,num_haplotypes = human_num_unique,id_list = human_id_list,num_infections = human_infections) %>%
  mutate(sample_type = rep("Human",nrow(maruti_data)))
# for mosquito heads
m_head = maruti_data %>% 
  select(starting_date,ending_date,head_haplotypes,head_num_unique,head_id_list,head_infections) %>%
  rename(haplotype_list = head_haplotypes,num_haplotypes = head_num_unique,id_list = head_id_list,num_infections = head_infections) %>%
  mutate(sample_type = rep("Mosquito Head",nrow(maruti_data)))
# for mosquito abdomens
m_abdomen = maruti_data %>% 
  select(starting_date,ending_date,abdomen_haplotypes,abdomen_num_unique,abdomen_id_list,abdomen_infections) %>%
  rename(haplotype_list = abdomen_haplotypes,num_haplotypes = abdomen_num_unique,id_list = abdomen_id_list,num_infections = abdomen_infections) %>%
  mutate(sample_type = rep("Mosquito Abdomen",nrow(maruti_data)))

# now combine the sample types
maruti_data_long = rbind(m_human,m_head,m_abdomen)



#### -------- make the exploratory plots ------- ####

## ---- first for kinesamo

# look at the colnames for the long data sets
colnames(kinesamo_data_long)

# plot 1: look at number of infections across each time window subset by sample type
plot_1 = ggplot(data=kinesamo_data_long,aes(x=num_infections,group=sample_type,fill=sample_type)) +
  geom_density(alpha=0.5) +
  theme_bw() +
  ylab("Density") + 
  xlab("Number of infections within each time window") +
  labs(fill="Sample type") +
  theme(legend.position = c(0.8,0.8),legend.box.background = element_rect(colour = "black"))
plot_1
ggsave(plot_1, filename="/Users/kelseysumner/Desktop/plot_1_number_infections_density.png", device="png",
       height=4, width=6, units="in", dpi=400)

# plot 2: make a boxplot of the number of infections
plot_2 = ggplot(data=kinesamo_data_long,aes(x=num_infections,y=sample_type,group=sample_type,fill=sample_type)) +
  geom_boxplot(alpha=0.5) +
  theme_bw() +
  xlab("Number of infections within each time window") +
  labs(fill="Sample type") +
  ylab("")+
  theme(legend.position = c(0.8,0.8),legend.box.background = element_rect(colour = "black"))
plot_2
ggsave(plot_2, filename="/Users/kelseysumner/Desktop/plot_2_number_infections_boxplot.png", device="png",
       height=4, width=6, units="in", dpi=400)

# plot 3: look at number of unique haplotypes across each time window subset by sample type
plot_3 = ggplot(data=kinesamo_data_long,aes(x=num_haplotypes,group=sample_type,fill=sample_type)) +
  geom_density(alpha=0.5) +
  theme_bw() +
  ylab("Density") + 
  xlab("Number of unique haplotypes within each time window") +
  labs(fill="Sample type") +
  theme(legend.position = "none")
plot_3
ggsave(plot_3, filename="/Users/kelseysumner/Desktop/plot_3_number_haplotypes_density.png", device="png",
       height=4, width=6, units="in", dpi=400)

# plot 4: make a boxplot of the number of unique haplotypes
plot_4 = ggplot(data=kinesamo_data_long,aes(x=num_haplotypes,y=sample_type,group=sample_type,fill=sample_type)) +
  geom_boxplot(alpha=0.5) +
  theme_bw() +
  xlab("Number of haplotypes within each time window") +
  labs(fill="Sample type") +
  ylab("")+
  theme(legend.box.background = element_rect(colour = "black"))
plot_4
ggsave(plot_4, filename="/Users/kelseysumner/Desktop/plot_4_number_haplotypes_boxplot.png", device="png",
       height=4, width=6, units="in", dpi=400)

# plot 5: make a plot of the number of infections over time
# figure out the maximum and minimum value within each time window
kinesamo_maximum_amounts = kinesamo_data_long %>%
  group_by(starting_date) %>%
  summarize(max_val = max(num_infections), min_val = min(num_infections))
# make the plot
plot_5 = ggplot() +
  geom_point(data=kinesamo_data_long,aes(x=starting_date,y=num_infections,group=sample_type,shape=sample_type,colour=sample_type),alpha=0.7) +
  geom_linerange(data=kinesamo_maximum_amounts,aes(x = starting_date, ymax = max_val, ymin = min_val),colour="black") +
  theme_bw() +
  ylab("Number of infections within each time window") +
  labs(shape="Sample type",colour="Sample type") +
  xlab("Starting date for each 30-day time window") +
  theme(legend.position = c(0.85,0.8),legend.box.background = element_rect(colour = "black"),axis.text.x = element_text(angle = 90)) +
  scale_x_date(date_breaks = "1 week",)
plot_5
ggsave(plot_5, filename="/Users/kelseysumner/Desktop/plot_5_number_infections_fancy.png", device="png",
       height=5, width=8, units="in", dpi=400)

# plot 6: make a plot of the number of unique haplotypes over time
# figure out the maximum and minimum value within each time window
kinesamo_maximum_amounts = kinesamo_data_long %>%
  group_by(starting_date) %>%
  summarize(max_val = max(num_haplotypes), min_val = min(num_haplotypes))
# make the plot
plot_6 = ggplot() +
  geom_point(data=kinesamo_data_long,aes(x=starting_date,y=num_haplotypes,group=sample_type,shape=sample_type,colour=sample_type),alpha=0.7) +
  geom_linerange(data=kinesamo_maximum_amounts,aes(x = starting_date, ymax = max_val, ymin = min_val),colour="black") +
  theme_bw() +
  ylab("Number of haplotypes within each time window") +
  labs(shape="Sample type",colour="Sample type") +
  xlab("Starting date for each 30-day time window") +
  theme(legend.position = c(0.85,0.8),legend.box.background = element_rect(colour = "black"),axis.text.x = element_text(angle = 90)) +
  scale_x_date(date_breaks = "1 week",)
plot_6
ggsave(plot_6, filename="/Users/kelseysumner/Desktop/plot_6_number_haplotypes_fancy.png", device="png",
       height=5, width=8, units="in", dpi=400)


## ---- then for maruti

# look at the colnames for the long data sets
colnames(maruti_data_long)

# plot 1: look at number of infections across each time window subset by sample type
plot_1 = ggplot(data=maruti_data_long,aes(x=num_infections,group=sample_type,fill=sample_type)) +
  geom_density(alpha=0.5) +
  theme_bw() +
  ylab("Density") + 
  xlab("Number of infections within each time window") +
  labs(fill="Sample type") +
  theme(legend.position = c(0.8,0.8),legend.box.background = element_rect(colour = "black"))
plot_1
ggsave(plot_1, filename="/Users/kelseysumner/Desktop/plot_1_number_infections_density_maruti.png", device="png",
       height=4, width=6, units="in", dpi=400)

# plot 2: make a boxplot of the number of infections
plot_2 = ggplot(data=maruti_data_long,aes(x=num_infections,y=sample_type,group=sample_type,fill=sample_type)) +
  geom_boxplot(alpha=0.5) +
  theme_bw() +
  xlab("Number of infections within each time window") +
  labs(fill="Sample type") +
  ylab("")+
  theme(legend.position = c(0.8,0.8),legend.box.background = element_rect(colour = "black"))
plot_2
ggsave(plot_2, filename="/Users/kelseysumner/Desktop/plot_2_number_infections_boxplot_maruit.png", device="png",
       height=4, width=6, units="in", dpi=400)

# plot 3: look at number of unique haplotypes across each time window subset by sample type
plot_3 = ggplot(data=maruti_data_long,aes(x=num_haplotypes,group=sample_type,fill=sample_type)) +
  geom_density(alpha=0.5) +
  theme_bw() +
  ylab("Density") + 
  xlab("Number of unique haplotypes within each time window") +
  labs(fill="Sample type") +
  theme(legend.position = "none")
plot_3
ggsave(plot_3, filename="/Users/kelseysumner/Desktop/plot_3_number_haplotypes_density_maruti.png", device="png",
       height=4, width=6, units="in", dpi=400)

# plot 4: make a boxplot of the number of unique haplotypes
plot_4 = ggplot(data=maruti_data_long,aes(x=num_haplotypes,y=sample_type,group=sample_type,fill=sample_type)) +
  geom_boxplot(alpha=0.5) +
  theme_bw() +
  xlab("Number of haplotypes within each time window") +
  labs(fill="Sample type") +
  ylab("")+
  theme(legend.box.background = element_rect(colour = "black"))
plot_4
ggsave(plot_4, filename="/Users/kelseysumner/Desktop/plot_4_number_haplotypes_boxplot_maruti.png", device="png",
       height=4, width=6, units="in", dpi=400)

# plot 5: make a plot of the number of infections over time
# figure out the maximum and minimum value within each time window
maruti_maximum_amounts = maruti_data_long %>%
  group_by(starting_date) %>%
  summarize(max_val = max(num_infections), min_val = min(num_infections))
# make the plot
plot_5 = ggplot() +
  geom_point(data=maruti_data_long,aes(x=starting_date,y=num_infections,group=sample_type,shape=sample_type,colour=sample_type),alpha=0.7) +
  geom_linerange(data=maruti_maximum_amounts,aes(x = starting_date, ymax = max_val, ymin = min_val),colour="black") +
  theme_bw() +
  ylab("Number of infections within each time window") +
  labs(shape="Sample type",colour="Sample type") +
  xlab("Starting date for each 30-day time window") +
  theme(legend.position = c(0.5,0.8),legend.box.background = element_rect(colour = "black"),axis.text.x = element_text(angle = 90)) +
  scale_x_date(date_breaks = "1 week",)
plot_5
ggsave(plot_5, filename="/Users/kelseysumner/Desktop/plot_5_number_infections_fancy_maruti.png", device="png",
       height=5, width=8, units="in", dpi=400)

# plot 6: make a plot of the number of unique haplotypes over time
# figure out the maximum and minimum value within each time window
maruti_maximum_amounts = maruti_data_long %>%
  group_by(starting_date) %>%
  summarize(max_val = max(num_haplotypes), min_val = min(num_haplotypes))
# make the plot
plot_6 = ggplot() +
  geom_point(data=maruti_data_long,aes(x=starting_date,y=num_haplotypes,shape=sample_type,color=sample_type),alpha=0.7) +
  geom_linerange(data=maruti_maximum_amounts,aes(x = starting_date, ymax = max_val, ymin = min_val),colour="black") +
  theme_bw() +
  ylab("Number of haplotypes within each time window") +
  labs(shape="Sample type",colour="Sample type") +
  xlab("Starting date for each 30-day time window") +
  scale_color_manual(values = c("#238b45","#1f78b4","#fb9a99")) +
  theme(legend.position = c(0.88,0.82),legend.box.background = element_rect(colour = "black"),axis.text.x = element_text(angle = 90)) +
  scale_x_date(date_breaks = "1 week",)
plot_6
ggsave(plot_6, filename="/Users/kelseysumner/Desktop/plot_6_number_haplotypes_fancy_maruti.png", device="png",
       height=5, width=10, units="in", dpi=400)




#### --------- now run a regression model comparing the number of unique haplotypes over time ------- ####

## --- first do for Kinesamo

# set up a poisson regression
# with a random intercept for the date
model_1 = glmer(num_haplotypes ~ as.factor(sample_type) + (1|starting_date),family=poisson(link="log"),data=kinesamo_data_long)
summary(model_1)
performance::icc(model_1)

# lets create ipw weights for the number of infections
exposureModel <- glmer(factor(sample_type) ~ num_infections + (1|starting_date), data = kinesamo_data_long, family = binomial(link="logit"))
pA = predict(exposureModel, type = "response")
IPW = 1/pA
summary(exposureModel)
kinesamo_data_long$pA = pA
kinesamo_data_long$IPW = IPW
summary(kinesamo_data_long$pA)
summary(kinesamo_data_long$IPW)

# set up a poisson regression
# with a random intercept for the date
# and IPW weights for the number of infections
model_1 = glmer(num_haplotypes ~ as.factor(sample_type) + (1|starting_date), weights = IPW, family=poisson(link="log"),data=kinesamo_data_long)
summary(model_1)
performance::icc(model_1)

# now make a forest plot of the model results
table1 = exp(confint(model_1,method="Wald"))
estimates = c(exp(0.11643),exp(0.02104))
lower_ci = c(table1[3,1],table1[4,1])
upper_ci = c(table1[3,2],table1[4,2])
names = c("Mosquito abdomen vs. human","Mosquito head vs. human")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Mosquito abdomen vs. human","Mosquito head vs. human"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Mosquito abdomen vs. human","Mosquito head vs. human"))
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(2,2),colour = c("#1f78b4","#fb9a99")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Rate ratio (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw()
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/forest_plot_kinesamo.png", device="png",
       height=3, width=5, units="in", dpi=400)


## --- now do for Maruti

# set up a poisson regression
# with a random intercept for the date
model_1 = glmer(num_haplotypes ~ as.factor(sample_type) + (1|starting_date),family=poisson(link="log"),data=maruti_data_long)
summary(model_1)
performance::icc(model_1)

# lets create ipw weights for the number of infections
exposureModel <- glmer(factor(sample_type) ~ num_infections + (1|starting_date), data = maruti_data_long, family = binomial(link="logit"))
pA = predict(exposureModel, type = "response")
IPW = 1/pA
summary(exposureModel)
maruti_data_long$pA = pA
maruti_data_long$IPW = IPW
summary(maruti_data_long$pA)
summary(maruti_data_long$IPW)

# set up a poisson regression
# with a random intercept for the date
# and IPW weights for the number of infections
model_1 = glmer(num_haplotypes ~ as.factor(sample_type) + (1|starting_date), weights = IPW, family=poisson(link="log"),data=maruti_data_long)
summary(model_1)
performance::icc(model_1)

# now make a forest plot of the model results
table1 = exp(confint(model_1,method="Wald"))
estimates = c(exp(0.33698),exp(0.11558))
lower_ci = c(table1[3,1],table1[4,1])
upper_ci = c(table1[3,2],table1[4,2])
names = c("Mosquito abdomen vs. human","Mosquito head vs. human")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Mosquito abdomen vs. human","Mosquito head vs. human"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Mosquito abdomen vs. human","Mosquito head vs. human"))
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(2,2),colour = c("#1f78b4","#fb9a99")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Rate ratio (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw()
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/forest_plot_maruti.png", device="png",
       height=3, width=5, units="in", dpi=400)




