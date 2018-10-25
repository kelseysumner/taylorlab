# ----------------------------------------- #
#            MESA Visualizations            #
#              for ASTMH 2018               #
#             October 4, 2018               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(ggplot2)
library(readr)
library(dplyr)
library(gridExtra) 
library(ggthemes)
library(wesanderson)
library(migest)
library(circlize)
library(tidyr)


#### ----- figure 1: histogram of MOI across 3 targets ----- ####

# read in the merged data set
merged_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/clean_files_for_lab/mesa_merged_final.csv")

## AMA
# create a summarized data frame of the number of people with each MOI
ama_moi_df <- merged_data %>% 
  filter(AMA_number_of_haplotypes != "Removed" & !(is.na(AMA_number_of_haplotypes))) %>%
  group_by(AMA_number_of_haplotypes) %>%
  summarise(n=n())
# make the number of haplotypes column numeric
ama_moi_df$AMA_number_of_haplotypes = as.numeric(ama_moi_df$AMA_number_of_haplotypes)

## CSP
# create a summarized data frame of the number of people with each MOI
csp_moi_df <- merged_data %>% 
  filter(CSP_number_of_haplotypes != "Removed" & !(is.na(CSP_number_of_haplotypes))) %>%
  group_by(CSP_number_of_haplotypes) %>%
  summarise(n=n())
# make the number of haplotypes column numeric
csp_moi_df$CSP_number_of_haplotypes = as.numeric(csp_moi_df$CSP_number_of_haplotypes)

## HistB
# create a summarized data frame of the number of people with each MOI
histb_moi_df <- merged_data %>% 
  filter(HistB_number_of_haplotypes != "Removed" & !(is.na(HistB_number_of_haplotypes))) %>%
  group_by(HistB_number_of_haplotypes) %>%
  summarise(n=n())
# make the number of haplotypes column numeric
histb_moi_df$HistB_number_of_haplotypes = as.numeric(histb_moi_df$HistB_number_of_haplotypes)

# plot each of the target data frames
## AMA
ama_title <- expression(paste(italic("Pfama1"), " target"))
plot_ama <- ggplot() +
  geom_bar(data=ama_moi_df, aes(x=AMA_number_of_haplotypes, y=n), alpha=0.8, stat = "identity", fill = wes_palette("GrandBudapest1", type = "discrete", n = 1)) + 
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") + 
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14), limits=c(0,15)) +
  scale_y_continuous(limits=c(0,200)) +
  labs(x="Multiplicity of infection", y="Number of participants", title= ama_title, pch=19) +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5), text = element_text(size=25))

## CSP
csp_title <- expression(paste(italic("Pfcsp"), " target"))
plot_csp <- ggplot() +
  geom_bar(data=csp_moi_df, aes(x=CSP_number_of_haplotypes, y=n), alpha=0.8, stat = "identity", fill = "#D67236") + 
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") + 
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14), limits=c(0,15)) +
  scale_y_continuous(limits=c(0,200)) +
  labs(x="Multiplicity of infection", y="", title= csp_title, pch=19)  +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5), text = element_text(size=25))

## HistB
histb_title <- expression(paste(italic("PfhistB"), " target"))
plot_histb <- ggplot() +
  geom_bar(data=histb_moi_df, aes(x=HistB_number_of_haplotypes, y=n), alpha=0.8, stat = "identity", fill = "#33a02c") + 
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") + 
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14), limits=c(0,15)) +
  scale_y_continuous(limits=c(0,200)) +
  labs(x="Multiplicity of infection", y="", title= histb_title)  +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5), text = element_text(size=25))
                                  
# arrange all of the plots to be in 1 panel horizontally next to each other
# leave out HistB for now because results looks funky
figure1 = gridExtra::grid.arrange(plot_ama, plot_csp, ncol=2)

ggsave(figure1, filename="/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/ASTMH/Poster/Figures/figure1.png", device="png",
       height=10.5, width=11.2, units="in", dpi=500)


#### ----- figure 2: histograms of haplotype sharing (number of people infected with each haplotype) ----- ####
## AMA
# read in the clean table
ama_fig2 = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/cut_zeros_haplotype_num_summary_censored.csv")

## CSP
# read in the clean table
csp_fig2 = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/cut_zeros_haplotype_num_summary_censored.csv")

## HistB
# read in the clean table
histb_fig2 = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/23AUG2018 HistB MESA Update/cut_zeros_haplotype_num_summary_censored.csv")

# plot each of the target data frames
## AMA
ama_title <- expression(paste(italic("Pfama1"), " target"))
plot_ama <- ggplot() +
  geom_bar(data=ama_fig2, aes(x=haplotype_ID, y=haplotypes_across_samples), alpha=0.9, stat = "identity", fill = "#FD6467", width = 0.8) + 
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") + 
  scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120)) +
  labs(x="Haplotype ID", y="Number of participants", title= ama_title) +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5), text = element_text(size=25)) 

## CSP
csp_title <- expression(paste(italic("Pfcsp"), " target"))
plot_csp <- ggplot() +
  geom_bar(data=csp_fig2, aes(x=haplotype_ID, y=haplotypes_across_samples), alpha=0.9, stat = "identity", fill = "#5B1A18", width = 0.8) + 
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") + 
  scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) +
  scale_x_continuous(limits = c(0,120), breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120)) +
  labs(x="Haplotype ID", y="Number of participants", title= csp_title)  +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5), text = element_text(size=25)) 

## HistB
histb_title <- expression(paste(italic("PfhistB"), " target"))
plot_histb <- ggplot() +
  geom_bar(data=histb_fig2, aes(x=haplotype_ID, y=haplotypes_across_samples), alpha=0.9, stat = "identity", fill = "#33a02c") + 
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") + 
  labs(x="Haplotype ID", y="", title= histb_title)  +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5), text = element_text(size=25)) 

# arrange all of the plots to be in 1 panel horizontally next to each other
# leave out HistB for now because don't trust results
figure2=gridExtra::grid.arrange(plot_ama, plot_csp, nrow=2)

ggsave(figure2, filename="/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/ASTMH/Poster/Figures/figure2.png", device="png",
       height=10.5, width=11.2, units="in", dpi=500)


#### ------ Figure 3: Histograms of MOI distribution conditioned on sex ------ ####

# read in the merged MESA data set
merged_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/clean_files_for_lab/mesa_merged_final.csv")

# look at how many RDT+ in data set
table(merged_data$mem_rdt_results, useNA = "always")

# look at number of ama and csp haplotypes
length(which(is.na(merged_data$AMA_number_of_haplotypes) == F))
length(which(is.na(merged_data$CSP_number_of_haplotypes) == F))

## AMA
# create a summarized data frame of the number of people with each MOI
ama_sex_df <- merged_data %>% 
  filter(AMA_number_of_haplotypes != "Removed" & !(is.na(AMA_number_of_haplotypes)) & !(is.na(sex))) %>%
  group_by(sex, AMA_number_of_haplotypes) %>%
  summarise(n=n())
# check variable structure
str(ama_sex_df$sex)
str(ama_sex_df$AMA_number_of_haplotypes)
str(ama_sex_df$n)
ama_sex_df$sex = as.factor(ama_sex_df$sex)

## CSP
# create a summarized data frame of the number of people with each MOI
csp_sex_df <- merged_data %>% 
  filter(CSP_number_of_haplotypes != "Removed" & !(is.na(CSP_number_of_haplotypes)) & !(is.na(sex))) %>%
  group_by(sex, CSP_number_of_haplotypes) %>%
  summarise(n=n())
# check variable structure
str(csp_sex_df$sex)
str(csp_sex_df$CSP_number_of_haplotypes)
str(csp_sex_df$n)
csp_sex_df$sex = as.factor(csp_sex_df$sex)

## HistB
# create a summarized data frame of the number of people with each MOI
histb_sex_df <- merged_data %>% 
  filter(HistB_number_of_haplotypes != "Removed" & !(is.na(HistB_number_of_haplotypes)) & !(is.na(sex))) %>%
  group_by(sex, HistB_number_of_haplotypes) %>%
  summarise(n=n())
# check variable structure
str(histb_sex_df$sex)
str(histb_sex_df$HistB_number_of_haplotypes)
str(histb_sex_df$n)
histb_sex_df$sex = as.factor(histb_sex_df$sex)

# make histograms of MOI distribution conditioned on sex
## for AMA
ama_title <- expression(paste(italic("Pfama1"), " target"))
plot_ama <- ggplot() +
  geom_bar(data=ama_sex_df, aes(x=AMA_number_of_haplotypes, y=n, group=sex, fill=sex), alpha=0.8, stat = "identity") + 
  theme(legend.position = "none") + 
  scale_fill_manual(values = c("#F1BB7B","#FD6467")) +
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14), limits=c(0,15)) +
  scale_y_continuous(limits=c(0,90)) +
  labs(x="Multiplicity of infection", y="Number of participants", title= ama_title) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  facet_grid(~sex)
plot_ama

## for CSP
csp_title <- expression(paste(italic("Pfcsp"), " target"))
plot_csp <- ggplot() +
  geom_bar(data=csp_sex_df, aes(x=CSP_number_of_haplotypes, y=n, group=sex, fill=sex), alpha=0.8, stat = "identity") + 
  theme(legend.position = "none") + 
  scale_fill_manual(values = c("#5B1A18","#D67236")) +
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14), limits=c(0,15)) +
  scale_y_continuous(limits=c(0,90)) +
  labs(x="Multiplicity of infection", y="Number of participants", title= csp_title) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  facet_grid(~sex)
plot_csp

# calculate median MOI for each sex cateogry
# for ama
males = merged_data[which(merged_data$sex == "male"),]
median(males$AMA_number_of_haplotypes,na.rm = T) # 1.500
females = merged_data[which(merged_data$sex == "female"),]
median(females$AMA_number_of_haplotypes,na.rm = T) # 2.000
# for csp
median(males$CSP_number_of_haplotypes,na.rm = T) # 2.000
median(females$CSP_number_of_haplotypes,na.rm = T) # 2.000

# double check median MOI for each sex category using DPLYR
# for ama
ama_sex_median_df <- merged_data %>% 
  filter(AMA_number_of_haplotypes != "Removed" & !(is.na(AMA_number_of_haplotypes)) & !(is.na(sex))) %>%
  group_by(sex) %>%
  summarise(n=n(), median=median(AMA_number_of_haplotypes, na.rm=T))
# for csp
csp_sex_median_df <- merged_data %>% 
  filter(CSP_number_of_haplotypes != "Removed" & !(is.na(CSP_number_of_haplotypes)) & !(is.na(sex))) %>%
  group_by(sex) %>%
  summarise(n=n(), median=median(CSP_number_of_haplotypes, na.rm=T))


#### ------ Figure 4: Histograms of MOI distribution conditioned on seasons ------ ####

## AMA
# create a summarized data frame of the number of people with each MOI
ama_season_df <- merged_data %>% 
  filter(AMA_number_of_haplotypes != "Removed" & !(is.na(AMA_number_of_haplotypes)) & !(is.na(dry_season))) %>%
  group_by(dry_season, AMA_number_of_haplotypes) %>%
  summarise(n=n())
# check variable structure
str(ama_season_df$dry_season)
str(ama_season_df$AMA_number_of_haplotypes)
str(ama_season_df$n)
ama_season_df$dry_season = as.factor(ama_season_df$dry_season)
ama_season_df$wet_season = as.factor(ifelse(ama_season_df$dry_season == "yes", "no", "yes"))

## CSP
# create a summarized data frame of the number of people with each MOI
csp_season_df <- merged_data %>% 
  filter(CSP_number_of_haplotypes != "Removed" & !(is.na(CSP_number_of_haplotypes)) & !(is.na(dry_season))) %>%
  group_by(dry_season, CSP_number_of_haplotypes) %>%
  summarise(n=n())
# check variable structure
str(csp_season_df$dry_season)
str(csp_season_df$CSP_number_of_haplotypes)
str(csp_season_df$n)
csp_season_df$dry_season = as.factor(csp_season_df$dry_season)
csp_season_df$wet_season = as.factor(ifelse(csp_season_df$dry_season == "yes", "no", "yes"))

## HistB
# create a summarized data frame of the number of people with each MOI
histb_season_df <- merged_data %>% 
  filter(HistB_number_of_haplotypes != "Removed" & !(is.na(HistB_number_of_haplotypes)) & !(is.na(dry_season))) %>%
  group_by(dry_season, HistB_number_of_haplotypes) %>%
  summarise(n=n())
# check variable structure
str(histb_season_df$dry_season)
str(histb_season_df$HistB_number_of_haplotypes)
str(histb_season_df$n)
histb_season_df$dry_season = as.factor(histb_season_df$dry_season)
histb_season_df$wet_season = as.factor(ifelse(histb_season_df$dry_season == "yes", "no", "yes"))

# make histograms of MOI distribution conditioned on season
## for AMA
ama_title <- expression(paste(italic("Pfama1"), " target"))
plot_ama <- ggplot() +
  geom_bar(data=ama_season_df, aes(x=AMA_number_of_haplotypes, y=n, group=wet_season, fill=wet_season), alpha=0.8, stat = "identity") + 
  theme(legend.position = "none") + 
  scale_fill_manual(values = c("#F1BB7B","#D67236")) +
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14), limits=c(0,15)) +
  scale_y_continuous(limits=c(0,60)) +
  labs(x="Multiplicity of infection", y="Number of participants", title= ama_title) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  facet_grid(~wet_season)
plot_ama

## for CSP
csp_title <- expression(paste(italic("Pfcsp"), " target"))
plot_csp <- ggplot() +
  geom_bar(data=csp_season_df, aes(x=CSP_number_of_haplotypes, y=n, group=wet_season, fill=wet_season), alpha=0.8, stat = "identity") + 
  theme(legend.position = "none") + 
  scale_fill_manual(values = c("#5B1A18","#FD6467")) +
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14), limits=c(0,15)) +
  scale_y_continuous(limits=c(0,90)) +
  labs(x="Multiplicity of infection", y="Number of participants", title= csp_title) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  facet_grid(~wet_season)
plot_csp

# calculate mean MOI for each season cateogry
# for ama
dry = merged_data[which(merged_data$dry_season == "yes"),]
median(dry$AMA_number_of_haplotypes,na.rm = T) # 2.000
wet = merged_data[which(merged_data$dry_season == "no"),]
median(wet$AMA_number_of_haplotypes,na.rm = T) # 1.000
# for csp
median(dry$CSP_number_of_haplotypes,na.rm = T) # 1.500
median(wet$CSP_number_of_haplotypes,na.rm = T) # 2.000

# double check median MOI for each sex category using DPLYR
# for ama
ama_season_median_df <- merged_data %>% 
  filter(AMA_number_of_haplotypes != "Removed" & !(is.na(AMA_number_of_haplotypes)) & !(is.na(dry_season))) %>%
  group_by(dry_season) %>%
  summarise(n=n(), median=median(AMA_number_of_haplotypes, na.rm=T))
# for csp
csp_season_median_df <- merged_data %>% 
  filter(CSP_number_of_haplotypes != "Removed" & !(is.na(CSP_number_of_haplotypes)) & !(is.na(dry_season))) %>%
  group_by(dry_season) %>%
  summarise(n=n(), median=median(CSP_number_of_haplotypes, na.rm=T))



#### ------ Figure 5: Boxplots of MOI distribution conditioned on age categories ------ ####

# make a new variable that is mem_age categorized into 5-year categories
age_cat = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if(is.na(merged_data$mem_age[i])){
    age_cat[i] = NA
  } else if(merged_data$mem_age[i] >= 0 & merged_data$mem_age[i] < 5){
    age_cat[i] = "Less than 5"
  } else if(merged_data$mem_age[i] >= 5 & merged_data$mem_age[i] < 10){
    age_cat[i] = "5-9"
  } else if(merged_data$mem_age[i] >= 10 & merged_data$mem_age[i] < 15){
    age_cat[i] = "10-14"
  } else if(merged_data$mem_age[i] >= 15 & merged_data$mem_age[i] < 20){
    age_cat[i] = "15-19"
  } else if(merged_data$mem_age[i] >= 20 & merged_data$mem_age[i] < 25){
    age_cat[i] = "20-24"
  } else if(merged_data$mem_age[i] >= 25){
    age_cat[i] = "25 or greater"
  }
}
merged_data$mem_age_cat = as.factor(age_cat)

# set the agecat order
agecat_order = c("Less than 5","5-9","10-14","15-19", "20-24","25 or greater")

# set the agecat levels
merged_data <- within(merged_data, mem_age_cat <- factor(mem_age_cat, levels=agecat_order))

# now make boxplots of the MOI across age categories
## for AMA
ama_title <- expression(paste(italic("Pfama1"), " target"))
plot_ama <- ggplot(data=subset(merged_data, !is.na(mem_age_cat)), aes(x=mem_age_cat,y=AMA_number_of_haplotypes,fill=mem_age_cat)) +
  geom_boxplot() + 
  scale_fill_manual(values=wes_palette("GrandBudapest1",type="continuous",n=13)) +
  theme(legend.position = "none") + 
  labs(x="Age category (years)", y="Multiplicity of infection", title= ama_title) +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5), text = element_text(size=25)) 
plot_ama

## for CSP
csp_title <- expression(paste(italic("Pfcsp"), " target"))
plot_csp <- ggplot(data=subset(merged_data, !is.na(mem_age_cat)), aes(x=mem_age_cat,y=CSP_number_of_haplotypes,fill=mem_age_cat)) +
  geom_boxplot() + 
  scale_fill_manual(values=wes_palette("GrandBudapest1",type="continuous",n=13)) +
  theme(legend.position = "none") + 
  labs(x="Age category (years)", y="Multiplicity of infection", title= csp_title) +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5), text = element_text(size=25)) 
plot_csp

# arrange all of the plots to be in 1 panel horizontally next to each other
# leave out HistB for now because results looks funky
figure5=gridExtra::grid.arrange(plot_ama, plot_csp, nrow=2)

ggsave(figure5, filename="/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/ASTMH/Poster/Figures/figure5.png", device="png",
       height=10.5, width=11.2, units="in", dpi=500)

# summarize the medians of the agecat results
# for ama
ama_age_df <- merged_data %>% 
  filter(AMA_number_of_haplotypes != "Removed" & !(is.na(AMA_number_of_haplotypes)) & !(is.na(mem_age_cat))) %>%
  group_by(mem_age_cat) %>%
  summarise(n=n(), median=median(AMA_number_of_haplotypes, na.rm = T))
# for csp
csp_age_df <- merged_data %>% 
  filter(CSP_number_of_haplotypes != "Removed" & !(is.na(CSP_number_of_haplotypes)) & !(is.na(mem_age_cat))) %>%
  group_by(mem_age_cat) %>%
  summarise(n=n(), median=median(CSP_number_of_haplotypes, na.rm = T))
# write out these df and paste into summary table
write_csv(ama_age_df, "ama_age_df.csv")
write_csv(csp_age_df, "csp_age_df.csv")


#### ------ Figure 6: Histograms of MOI distribution conditioned on symptoms ------ ####

# read in the merged MESA data set
merged_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/clean_files_for_lab/mesa_merged_final.csv")

## AMA
# create a summarized data frame of the number of people with each MOI
ama_symptoms_df <- merged_data %>% 
  filter(AMA_number_of_haplotypes != "Removed" & !(is.na(AMA_number_of_haplotypes)) & !(is.na(mem_has_fever))) %>%
  group_by(mem_has_fever, AMA_number_of_haplotypes) %>%
  summarise(n=n())
# check variable structure
str(ama_symptoms_df$mem_has_fever)
str(ama_symptoms_df$AMA_number_of_haplotypes)
str(ama_symptoms_df$n)
ama_symptoms_df$mem_has_fever = as.factor(ama_symptoms_df$mem_has_fever)

## CSP
# create a summarized data frame of the number of people with each MOI
csp_symptoms_df <- merged_data %>% 
  filter(CSP_number_of_haplotypes != "Removed" & !(is.na(CSP_number_of_haplotypes)) & !(is.na(mem_has_fever))) %>%
  group_by(mem_has_fever, CSP_number_of_haplotypes) %>%
  summarise(n=n())
# check variable structure
str(csp_symptoms_df$mem_has_fever)
str(csp_symptoms_df$CSP_number_of_haplotypes)
str(csp_symptoms_df$n)
csp_symptoms_df$mem_has_fever = as.factor(csp_symptoms_df$mem_has_fever)

## HistB
# create a summarized data frame of the number of people with each MOI
histb_symptoms_df <- merged_data %>% 
  filter(HistB_number_of_haplotypes != "Removed" & !(is.na(HistB_number_of_haplotypes)) & !(is.na(mem_has_fever))) %>%
  group_by(mem_has_fever, HistB_number_of_haplotypes) %>%
  summarise(n=n())
# check variable structure
str(histb_symptoms_df$mem_has_fever)
str(histb_symptoms_df$HistB_number_of_haplotypes)
str(histb_symptoms_df$n)
histb_symptoms_df$mem_has_fever = as.factor(histb_symptoms_df$mem_has_fever)

# make histograms of MOI distribution conditioned on season
## for AMA
ama_title <- expression(paste(italic("Pfama1"), " target"))
plot_ama <- ggplot() +
  geom_bar(data=ama_symptoms_df, aes(x=AMA_number_of_haplotypes, y=n, group=mem_has_fever, fill=mem_has_fever), alpha=0.8, stat = "identity") + 
  theme(legend.position = "none") + 
  scale_fill_manual(values = c("#F1BB7B","#D67236")) +
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14), limits=c(0,15)) +
  scale_y_continuous(limits=c(0,50)) +
  labs(x="Multiplicity of infection", y="Number of participants", title= ama_title) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  facet_grid(~mem_has_fever)
plot_ama

## for CSP
csp_title <- expression(paste(italic("Pfcsp"), " target"))
plot_csp <- ggplot() +
  geom_bar(data=csp_symptoms_df, aes(x=CSP_number_of_haplotypes, y=n, group=mem_has_fever, fill=mem_has_fever), alpha=0.8, stat = "identity") + 
  theme(legend.position = "none") + 
  scale_fill_manual(values = c("#5B1A18","#FD6467")) +
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14), limits=c(0,15)) +
  scale_y_continuous(limits=c(0,50)) +
  labs(x="Multiplicity of infection", y="Number of participants", title= csp_title) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  facet_grid(~mem_has_fever)
plot_csp

# calculate mean MOI for each symptom category
# for ama
yes = merged_data[which(merged_data$mem_has_fever == "yes"),]
median(yes$AMA_number_of_haplotypes,na.rm = T) # 1
no = merged_data[which(merged_data$mem_has_fever == "no"),]
median(no$AMA_number_of_haplotypes,na.rm = T) # 2
# for csp
median(yes$CSP_number_of_haplotypes,na.rm = T) # 1
median(no$CSP_number_of_haplotypes,na.rm = T) # 2

# double check median MOI for each symptom category using DPLYR
# for ama
ama_symptom_median_df <- merged_data %>% 
  filter(AMA_number_of_haplotypes != "Removed" & !(is.na(AMA_number_of_haplotypes)) & !(is.na(mem_has_fever))) %>%
  group_by(mem_has_fever) %>%
  summarise(n=n(), median=median(AMA_number_of_haplotypes, na.rm=T))
# for csp
csp_symptom_median_df <- merged_data %>% 
  filter(CSP_number_of_haplotypes != "Removed" & !(is.na(CSP_number_of_haplotypes)) & !(is.na(mem_has_fever))) %>%
  group_by(mem_has_fever) %>%
  summarise(n=n(), median=median(CSP_number_of_haplotypes, na.rm=T))


#### ------ Figure 7: Haplotype sharing across locations ------ ####

# circular migration plot representing haplotype sharing

# source
# Abel, G.J. (2016) Estimates of global bilateral migration glows by gender between 1960 and 2015. 
# Vienna Institute of Demography Working Papers 2/2016.

# read in the data set
df0 <- read.csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/ASTMH/Poster/Haplotype Sharing Output/CSP_haplotypes_edgelist_by_location.csv", stringsAsFactors=FALSE)
df1 <- read.csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/ASTMH/Poster/Haplotype Sharing Output/df1_by_location.csv", stringsAsFactors=FALSE)

# create the default chord diagram
chordDiagram(x = df0)

# set up plot parameters
circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))
# increasing the gaps between sectors, start at 12 o'clock, ensure no gap between the chord and the sector at the begining
# subdue warning messages and have no margins in the plot

# create the default chord diagram
figure7=chordDiagram(x = df0, grid.col = df1$col, transparency = 0.01,
             order = df1$region)

save(filename="/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/ASTMH/Poster/Figures/figure7.png", plot=figure7, device="png",
       height=10.5, width=10.5, units="in", dpi=500)


