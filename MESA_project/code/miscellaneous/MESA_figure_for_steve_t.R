# ----------------------------------------- #
#       MESA Figure for Steve T             #
#              April 10, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)


#### --------- load in the data set ------------ ####

# read in clean mesa data set
mesa_data = read_csv("Desktop/mesa_figure_for_stevet/MESA cRDT sensitivity.csv")

# look at the data set
summary(mesa_data)
colnames(mesa_data)


#### -------- make the new figure ---------- ####

## step 1: compute by category (A), prob of detection (D/B) with upper and lower 95% CIs
# create a new variable (E) that is prob of detection (D/B)
mesa_data$prob_detection = mesa_data$crdtpos/mesa_data$npcrpos
# make the plot
plot_step1 = ggplot(data=mesa_data) +
  geom_point(aes(x=denscat88,y=prob_detection)) +
  geom_smooth(aes(x=denscat88,y=prob_detection)) +
  ylim(0,1)
plot_step1

## step 2: plot counts by category (A) of B and D, with D overlaying B
#  create a function for italicizing words
my_x_title <- expression(paste("Log of ", italic("P. falciparum"), " parasite density"))
my_y_title <- expression(paste("Number of ", italic("P. falciparum"), " positive samples"))
# make the plot
plot_step2 = ggplot(data=mesa_data) +
  geom_bar(aes(x=denscat88,y=npcrpos), stat = "identity", fill = "light blue", colour = "black") +
  geom_bar(aes(x=denscat88,y=crdtpos), stat = "identity", fill= "dark blue", colour = "black") +
  labs(x=my_x_title,y=my_y_title)
plot_step2


## step 3: plot by category (A) sensitivity and CIs from step 1. make continuous if possible.
# make the titles
my_x_title <- expression(paste("Log of parasite density"))
my_y_title <- expression(paste("Number of parasite positive samples"))
# make the plot
plot_step3 = ggplot(data=mesa_data) +
  geom_bar(aes(x=denscat88,y=npcrpos), stat = "identity", fill = "light blue", colour = "black") +
  geom_bar(aes(x=denscat88,y=crdtpos), stat = "identity", fill= "dark blue", colour = "black") +
  geom_point(aes(x=denscat88,y=prob_detection*100),color = "dark grey") +
  geom_smooth(aes(x=denscat88,y=prob_detection*100),color = "pink",method = "loess") +
  theme_minimal() +
  scale_y_continuous(my_y_title, sec.axis=sec_axis(~ . * .01, name = "Probability parasite detection")) +
  labs(x=my_x_title,y=my_y_title)
plot_step3 
# make the plot with prettier colors 
plot_step3 = ggplot(data=mesa_data) +
  geom_bar(aes(x=denscat88,y=npcrpos), stat = "identity", fill = "#9ecae1", colour = "black") +
  geom_bar(aes(x=denscat88,y=crdtpos), stat = "identity", fill= "#045a8d", colour = "black") +
  geom_point(aes(x=denscat88,y=prob_detection*100),color = "#fec44f") +
  geom_smooth(aes(x=denscat88,y=prob_detection*100),color = "#cc4c02",method = "loess") +
  theme_minimal() +
  scale_y_continuous(my_y_title, sec.axis=sec_axis(~ . * .01, name = "Probability parasite detection")) +
  labs(x=my_x_title,y=my_y_title)
plot_step3 
# export figure
ggsave(plot_step3, filename="/Users/kelseysumner/Desktop/Meshnick Lab/mesa_figure_for_steve.png", device="png",
       height=4, width=5, units="in", dpi=500)
