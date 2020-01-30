# ------------------------------------------- #
#  Add in variable parameters for comp model  #
#               Mozzie Phase 1                #
#                Clean version                #
#                   Aim 2                     #
#             CSP and AMA data                #
#            December 20, 2019                #
#                K. Sumner                    #
# ------------------------------------------- #

#### --------- load packages ----------------- ####
library(tidyverse)
library(msm)
require(pracma)


#### ---------- read in the data sets ---------- ####

# read in the combined ama and csp data set for mosquito abdomens
edgelist_data = read_rds("Desktop/clean_ids_haplotype_results/AMA_and_CSP/final/spat21_aim2_merged_data_with_weights_14JAN2020.rds")

# read in the clean ama haplotype data
ama_haplotypes <- read_rds("Desktop/clean_ids_haplotype_results/AMA/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_15OCT2019.rds")

# read in the clean csp haplotype data
csp_haplotypes <- read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_CSP_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")



#### -------- calculate the prevalence of each haplotype in the general population -------- ####

## for csp

# first subset the haplotype table
csp_hap_prevalence_data = csp_haplotypes 
csp_hap_prevalence_data = csp_hap_prevalence_data[,c(4:301)]

# summarize the number of samples within each haplotype for the asymp human samples
csp_haplotype_names = rep(1:ncol(csp_hap_prevalence_data))
csp_haplotypes_in_samples = rep(NA,ncol(csp_hap_prevalence_data))
csp_total_reads_in_samples = rep(NA,ncol(csp_hap_prevalence_data))
for (k in 1:ncol(csp_hap_prevalence_data)){
  csp_haplotypes_in_samples[k] = length(which(csp_hap_prevalence_data[,k] > 0))
  csp_total_reads_in_samples[k] = sum(csp_hap_prevalence_data[,k],na.rm=T)
}
csp_hap_summary = data.frame("csp_haplotype_ids" = csp_haplotype_names, "csp_haplotypes_across_samples" = csp_haplotypes_in_samples, "csp_total_reads_across_samples" = csp_total_reads_in_samples)

# add a column that is the haplotype prevalence
csp_hap_summary$csp_hap_prevalence = csp_hap_summary$csp_haplotypes_across_samples/nrow(csp_haplotypes)

# add an H to every haplotype id
csp_hap_summary$csp_haplotype_ids = paste0("H",csp_hap_summary$csp_haplotype_ids)


## for ama

# first subset the haplotype table
ama_hap_prevalence_data = ama_haplotypes 
ama_hap_prevalence_data = ama_hap_prevalence_data[,c(4:459)]

# summarize the number of samples within each haplotype for the asymp human samples
ama_haplotype_names = rep(1:ncol(ama_hap_prevalence_data))
ama_haplotypes_in_samples = rep(NA,ncol(ama_hap_prevalence_data))
ama_total_reads_in_samples = rep(NA,ncol(ama_hap_prevalence_data))
for (k in 1:ncol(ama_hap_prevalence_data)){
  ama_haplotypes_in_samples[k] = length(which(ama_hap_prevalence_data[,k] > 0))
  ama_total_reads_in_samples[k] = sum(ama_hap_prevalence_data[,k],na.rm=T)
}
ama_hap_summary = data.frame("ama_haplotype_ids" = ama_haplotype_names, "ama_haplotypes_across_samples" = ama_haplotypes_in_samples, "ama_total_reads_across_samples" = ama_total_reads_in_samples)

# add a column that is the haplotype prevalence
ama_hap_summary$ama_hap_prevalence = ama_hap_summary$ama_haplotypes_across_samples/nrow(ama_haplotypes)

# add an H to every haplotype id
ama_hap_summary$ama_haplotype_ids = paste0("H",ama_hap_summary$ama_haplotype_ids)




#### ------- calculate haplotype probability values for ama and csp -------- ####


# calculate the P(TE) for csp based on the number and prevalence of haplotypes
p_te_c = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$csp_haps_shared[i] > 0 & !(is.na(merged_data$csp_haps_shared[i]))){
    cum_prop = 1
    split_list = str_split(merged_data$csp_list_haps_shared[i],",")[[1]]
    for (j in 1:merged_data$csp_haps_shared[i]) {
      for (k in 1:nrow(csp_hap_summary)){
        if (split_list[j] == csp_hap_summary$csp_haplotype_ids[k]){
          cum_prop = cum_prop*(1-csp_hap_summary$csp_hap_prevalence[k])
          }
        }
      }
      p_te_c[i] = 1 - cum_prop
  } else {
    p_te_c[i] = 0
  }
}
# add the new variable to the data set
merged_data$p_te_c = p_te_c




# calculate the P(TE) for ama based on the number and prevalence of haplotypes
p_te_a = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$ama_haps_shared[i] > 0 & !(is.na(merged_data$ama_haps_shared[i]))){
    cum_prop = 1
    split_list = str_split(merged_data$ama_list_hap_shared[i],",")[[1]]
    for (j in 1:merged_data$ama_haps_shared[i]) {
      for (k in 1:nrow(ama_hap_summary)){
        if (split_list[j] == ama_hap_summary$ama_haplotype_ids[k]){
          cum_prop = cum_prop*(1-ama_hap_summary$ama_hap_prevalence[k])
        }
      }
    }
    p_te_a[i] = 1 - cum_prop
  } else {
    p_te_a[i] = 0
  }
}
# add the new variable to the data set
merged_data$p_te_a = p_te_a



#### -------- make the probability TE curve for the time between samples ------- ####

# read back in the data set
merged_data = read_rds("Desktop/clean_ids_haplotype_results/AMA_and_CSP/interim/spat21_merged_data_interim.rds")


# create a formula for the P(TE) across time
# our actual equation is y=e^(-3x)

# set up the variables
summary(merged_data$date_difference)
merged_data$date_difference = as.numeric(merged_data$date_difference)
merged_data$date_difference_flipped = merged_data$date_difference*-1
summary(merged_data$date_difference_flipped)



# calculate p(TE) using the logistic function
# equation: Y = 1/(1+0.6e^(-x-16)))
p_te_t = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$date_difference_flipped[i] >= -18 & merged_data$date_difference_flipped[i] <= 0){
    p_te_t[i] = 1/(1+0.6*exp(-merged_data$date_difference_flipped[i]-16))
  } else {
    p_te_t[i] = 0
  }
}
summary(p_te_t)
hist(p_te_t)
p_te_t_no_zeroes = p_te_t[which(p_te_t != 0)]
summary(p_te_t_no_zeroes)
hist(p_te_t_no_zeroes)
merged_data$p_te_t = p_te_t
p_te_t_df = merged_data %>%
  filter(p_te_t != 0)
plot(p_te_t_df$date_difference_flipped,p_te_t_df$p_te_t,xlab = "Days between human infection and mosquito collection",ylab="P(TE)")
abline(v=-14,col="dark red",lwd=1.5)







#### ------- make the probably TE curve for the distance between samples ------- ####

# create a formula for the P(TE) across distance 
# f(x) = λ {e}^{- λ x}

# look at the start values
summary(merged_data$distance)
str(merged_data$distance) # this is in m

# make distance in km
merged_data$distance_km = merged_data$distance/1000
summary(merged_data$distance_km)

# calculate using the exponential decay formula but over distance (what we use) - this is testing out the equation
# our actual equation is y=e^(-3x)
p_te_d = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$distance_km[i] >= 0 & merged_data$distance_km[i] <= 3){
    p_te_d[i] = exp(-merged_data$distance_km[i]*3)
  } else {
    p_te_d[i] = 0
  }
}
summary(p_te_d)
hist(p_te_d)
p_te_d_no_zeroes = p_te_d[which(p_te_d != 0)]
summary(p_te_d_no_zeroes)
hist(p_te_d_no_zeroes)
merged_data$p_te_d = p_te_d
p_te_d_df = merged_data %>%
  filter(p_te_d != 0)
plot(p_te_d_df$distance_km,p_te_d_df$p_te_d)

# look at a summary of p_te_d_df
summary(p_te_d_df$p_te_d)





#### ------- make a final variable of combined P(TEall) ------- ####

# first combine the pfcsp and pfama1 haplotypes
p_te_a_c_combo = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$p_te_a[i] != 0 & merged_data$p_te_c[i] != 0){
    p_te_a_c_combo[i] = 1-(1-merged_data$p_te_a[i])*(1-merged_data$p_te_c[i])
  } else if (merged_data$p_te_a[i] != 0 & merged_data$p_te_c[i] == 0){
    p_te_a_c_combo[i] = 1-(1-merged_data$p_te_a[i])
  } else if (merged_data$p_te_a[i] == 0 & merged_data$p_te_c[i] != 0){
    p_te_a_c_combo[i] = 1-(1-merged_data$p_te_c[i])
  } else{
    p_te_a_c_combo[i] = 0
  }
}
summary(p_te_a_c_combo)
merged_data$p_te_a_c_combo = p_te_a_c_combo
length(which(p_te_a_c_combo == 0)) # 60797
length(which(merged_data$p_te_a == 0 & merged_data$p_te_c == 0)) # 60797


# rescale the variables to all have to be between 0 and 1
summary(merged_data$p_te_a_c_combo)
summary(merged_data$p_te_d) 
summary(merged_data$p_te_t)
# rescale for p_te_a_c_combo
merged_data$rescaled_p_te_a_c_combo = (merged_data$p_te_a_c_combo-min(merged_data$p_te_a_c_combo))/(max(merged_data$p_te_a_c_combo)-min(merged_data$p_te_a_c_combo))
hist(merged_data$p_te_a_c_combo)
hist(merged_data$rescaled_p_te_a_c_combo)
summary(merged_data$rescaled_p_te_a_c_combo)
# rescale for p_te_d
merged_data$rescaled_p_te_d = (merged_data$p_te_d-min(merged_data$p_te_d))/(max(merged_data$p_te_d)-min(merged_data$p_te_d))
hist(merged_data$p_te_d)
hist(merged_data$rescaled_p_te_d)
summary(merged_data$rescaled_p_te_d) # stays same
# rescale for p_te_t
merged_data$rescaled_p_te_t = (merged_data$p_te_t-min(merged_data$p_te_t))/(max(merged_data$p_te_t)-min(merged_data$p_te_t))
hist(merged_data$p_te_t)
hist(merged_data$rescaled_p_te_t)
summary(merged_data$rescaled_p_te_t) # stays same


# make a final variable that is P(TEall)
# have that variable conditioned so you only calculate the probability of transmission if p_te is non-zero for all 4 variables
p_te_all = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$rescaled_p_te_t[i] != 0 & merged_data$rescaled_p_te_d[i] != 0 & merged_data$rescaled_p_te_a_c_combo[i] != 0){
    p_te_all[i] = merged_data$rescaled_p_te_t[i]*merged_data$rescaled_p_te_d[i]*merged_data$rescaled_p_te_a_c_combo[i]
  } else {
    p_te_all[i] = 0
  }
}
summary(p_te_all)
merged_data$p_te_all = p_te_all
length(which(p_te_all == 0)) # 168392
length(which(p_te_all > 0)) # 2262
length(which(merged_data$rescaled_p_te_t != 0 & merged_data$rescaled_p_te_d != 0 & merged_data$rescaled_p_te_a_c_combo != 0)) # 2262


# export the data set 
# write_csv(merged_data,"Desktop/spat21_aim2_merged_data_with_weights_14JAN2020.csv")
# write_rds(merged_data,"Desktop/spat21_aim2_merged_data_with_weights_14JAN2020.rds")



#### ------- make some plots of the output -------- ####


# symptomatic (blue): #3B9AB2
# asymptomatic (yellow): #E1AF00
# mosquitoes (red): #F21A00
# no infection (light grey): #D3DDDC


# make a plot of p_te_t
# time plot
merged_data_time_plot = merged_data %>%
  filter(date_difference_flipped <= 0)
p_te_t_density_plot_x = ggplot(data=merged_data_time_plot,aes(x=date_difference_flipped)) +
  geom_density(alpha=0.6,fill=c("#c2a5cf")) + 
  theme_bw() + 
  xlab("Days between human infection and mosquito collection") +
  ylab("Density") +
  ggtitle("Density of observations over time")
p_te_t_density_plot_x
dpb <- ggplot_build(p_te_t_density_plot_x)
x1 <- min(which(dpb$data[[1]]$x >=-18))
x2 <- max(which(dpb$data[[1]]$x <=0))
p_te_t_density_plot_x = p_te_t_density_plot_x +
  geom_area(data=data.frame(x=dpb$data[[1]]$x[x1:x2],
                            y=dpb$data[[1]]$y[x1:x2]),
            aes(x=x, y=y), fill="#762a83", colour = "black") +
  scale_x_continuous(breaks=c(0,-18,-100,-200,-300,-400)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
ggsave(p_te_t_density_plot_x, filename="/Users/kelseysumner/Desktop/p_te_t_density_plot_x.png", device="png",
       height=8, width=14, units="in", dpi=500)



# make a plot of p_te_d
p_te_d_density_plot_x = ggplot(data=merged_data,aes(x=distance_km)) +
  geom_density(alpha=0.6,fill=c("#fdae6b")) + 
  theme_bw() + 
  xlab("Distance (Km) between human infection and mosquito collection") +
  ylab("Density") +
  ggtitle("Density of observations over distance")
p_te_d_density_plot_x
dpb <- ggplot_build(p_te_d_density_plot_x)
x1 <- min(which(dpb$data[[1]]$x >=0))
x2 <- max(which(dpb$data[[1]]$x <=3))
p_te_d_density_plot_x = p_te_d_density_plot_x +
  geom_area(data=data.frame(x=dpb$data[[1]]$x[x1:x2],
                            y=dpb$data[[1]]$y[x1:x2]),
            aes(x=x, y=y), fill="#f16913", colour = "black") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25)) +
  scale_x_continuous(breaks=c(0,3,6,9,12),limits=c(0,12))
ggsave(p_te_d_density_plot_x, filename="/Users/kelseysumner/Desktop/p_te_d_density_plot_x.png", device="png",
       height=8, width=14, units="in", dpi=500)




# make a plot of the ama and csp haplotype probability values
# set up the data frame
p_te_a_df = merged_data %>%
  filter(p_te_a > 0) %>%
  select(p_te_a,ama_haps_shared)
p_te_a_df$type = rep("pfama1",nrow(p_te_a_df))
p_te_c_df = merged_data %>%
  filter(p_te_c > 0) %>%
  select(p_te_c,csp_haps_shared)
p_te_c_df$type = rep("pfcsp",nrow(p_te_c_df))
p_te_a_df$x_axis = as.factor(p_te_a_df$ama_haps_shared)
p_te_c_df$x_axis = as.factor(p_te_c_df$csp_haps_shared)
p_te_a_df$ama_haps_shared <- NULL
p_te_c_df$csp_haps_shared <- NULL
colnames(p_te_a_df)
colnames(p_te_c_df)
p_te_a_df = rename(p_te_a_df,"probability"=p_te_a)
p_te_c_df = rename(p_te_c_df,"probability"=p_te_c)
p_te_a_c_combo_df = rbind(p_te_a_df,p_te_c_df)
colnames(p_te_a_c_combo_df)
# make the plot
p_te_a_c_combo = ggplot() +
  geom_boxplot(data=p_te_a_c_combo_df,aes(x=factor(x_axis),y=probability,fill=type),alpha=0.8) + 
  theme_bw() + 
  ylab("Probability of transmission") +
  xlab("Number of haplotypes shared") +
  scale_fill_manual(values=c("#081d58","#fb9a99")) +
  labs(fill = "Sequencing target") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25), legend.position = c(0.86,0.12), legend.box.background = element_rect(colour = "black"))
p_te_a_c_combo
ggsave(p_te_a_c_combo, filename="/Users/kelseysumner/Desktop/p_te_a_density_plot_x.png", device="png",
       height=8, width=14, units="in", dpi=500)


