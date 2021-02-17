# -------------------------------------- #
#           Spat21/Mozzie Study          #
# Make extra figure of asymp/symp infxns #
#                 Aim 1A                 #
#               Human Data               #
#            Mozzie Phase 3              #
#                K. Sumner               #
#            February 16, 2021           #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(tidyverse)


#### ------- load data sets ---------- #####

# read in a data set of the phase 3 infections prior to imputation and putting it in survival format
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/De-identified Phase II_v13/final_merged_data/final_data_set/phase3_spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_18AUG2020.rds")





#### --------- make a figure for asymptomatic and symptomatic sample collection ----------- ####

# make a data set of just symptomatic infections
symptomatic_df = final_data %>%
  filter(visit_type == "monthly and sick visit" | visit_type== "sick visit") %>%
  select(c(unq_memID,sample_id_date,sample_name_final,HH_ID,village_name,main_outcome_primary_case_def)) %>%
  mutate(symp_infection = ifelse(!(is.na(main_outcome_primary_case_def)),"symptomatic infection","no infection"))
table(symptomatic_df$symp_infection, useNA = "always")

# create a new variable that is just the month
symptomatic_df$month = paste0(lubridate::month(symptomatic_df$sample_id_date),"-",lubridate::year(symptomatic_df$sample_id_date))
table(symptomatic_df$month, useNA = "always")

# make a data set of just asymptomatic infections
asymptomatic_df = final_data %>%
  filter(!(is.na(main_exposure_primary_case_def))) %>%
  select(c(unq_memID,sample_id_date,sample_name_final,HH_ID,village_name,main_exposure_primary_case_def))
table(asymptomatic_df$main_exposure_primary_case_def, useNA = "always")

# create a new variable that is just the month
asymptomatic_df$month = paste0(lubridate::month(asymptomatic_df$sample_id_date),"-",lubridate::year(asymptomatic_df$sample_id_date))
table(asymptomatic_df$month, useNA = "always")

# create a combined data frame of all the data frames
colnames(asymptomatic_df)
asymptomatic_df = asymptomatic_df %>%
  select(main_exposure_primary_case_def,sample_id_date) %>%
  mutate(type = "Asymptomatic visit") %>%
  rename(infection_status = main_exposure_primary_case_def,date = sample_id_date)
colnames(symptomatic_df)
symptomatic_df = symptomatic_df %>%
  select(main_outcome_primary_case_def,sample_id_date) %>%
  mutate(type = "Symptomatic visit") %>%
  rename(infection_status = main_outcome_primary_case_def,date=sample_id_date)
colnames(symptomatic_df)
colnames(asymptomatic_df)
all_df = rbind(symptomatic_df,asymptomatic_df)
all_df$infection_status = as.character(all_df$infection_status)
all_df$infection_status[which(all_df$infection_status == "symptomatic infection")] = "Positive"
all_df$infection_status[which(all_df$infection_status == "asymptomatic infection")] = "Positive"
all_df$infection_status[which(all_df$infection_status == "no infection")] = "Negative"
all_df$infection_status[which(is.na(all_df$infection_status))] = "Negative"
table(all_df$infection_status, useNA = "always")
all_df$infection_status = as.character(all_df$infection_status)
all_df$type_status = paste(all_df$type,all_df$infection_status)
table(all_df$type_status,useNA = "always")

# try a facet plot with bars
all_df$type = as.factor(all_df$type)
all_df$type = relevel(all_df$type,ref="Symptomatic visit")
all_df_neg = all_df %>% filter(infection_status=="Negative")
all_df_pos = all_df %>% filter(infection_status=="Positive")
all_df_neg = data.frame(all_df_neg)
all_df_pos = data.frame(all_df_pos)
small_all_df = all_df %>%
  mutate(new_date = floor_date(date,"week")) %>%
  group_by(new_date,type,infection_status) %>%
  summarise(n=n())
# symptomatic (red): #e41a1c
# asymptomatic (orange): #ff7f00
# no infection (light grey): #D3DDDC
small_all_df$color = rep(NA,nrow(small_all_df))
small_all_df$color[which(small_all_df$type=="Symptomatic visit" & small_all_df$infection_status=="Positive")] = "#e41a1c"
small_all_df$color[which(small_all_df$type=="Symptomatic visit" & small_all_df$infection_status=="Negative")] = "#D3DDDC"
small_all_df$color[which(small_all_df$type=="Asymptomatic visit" & small_all_df$infection_status=="Positive")] = "#ff7f00"
small_all_df$color[which(small_all_df$type=="Asymptomatic visit" & small_all_df$infection_status=="Negative")] = "#D3DDDC"
color_order = c("#D3DDDC","#e41a1c","#ff7f00")
small_all_df <- within(small_all_df,color <- factor(color,levels=color_order))
# make the plot
density_all_plot = ggplot(data=small_all_df,aes(x=new_date,fill=color,y=n)) + 
  facet_grid(type ~ .,switch = "y") +
  geom_histogram(stat="identity",color="black") +
  xlab("") +
  ylab("Number of samples collected") +
  scale_fill_identity() +
  theme_bw() +
  scale_x_date(date_breaks="1 month",limits = as.Date(c("2017-06-01","2019-12-01"))) + 
  scale_y_continuous(limits = c(0,100),breaks=c(0,20,40,60,80,100),position = "right") + 
  theme(text = element_text(size=30),axis.text.x = element_text(angle = 90),legend.position=c(0.8,0.8),legend.background = element_rect(color = "black"),legend.title = element_text(size=10)) +
  labs(fill = "Symptomatic status")
density_all_plot
ggsave(density_all_plot, filename="/Users/kelseysumner/Desktop/sampling_all_plot_fig1_aim1b.png", device="png",
       height=12, width=20, units="in", dpi=500)

