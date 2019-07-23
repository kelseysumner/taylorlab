# ----------------------------------------- #
#       Spat21 Haplotype Output Cleaning    #
#       Expected vs. Observed Proportions   #
#         Figure for Control Haplotypes     #     
#              July 23, 2019                #
#                K. Sumner                  #
# ----------------------------------------- #


#### --------- load packages ----------------- ####
library(tidyverse)
library(ggrepel)


#### -------- create an expected vs observed proportions figure ------------ ####

# read in the data set of the control haplotype information
control_data = read_csv("Desktop/control_haplotype_proportions.csv")

# create a plot of the observed and expected
control_plot_1 = ggplot(data=control_data,aes(x=expected,y=observed,color=target)) +
  geom_point() +
  labs(x="Expected Proportion", y="Observed Proportion", color="Target") +
  geom_abline(intercept = 0, slope = 1,lty="dashed")
control_plot_1

# export the plot
ggsave(control_plot_1, filename="/Users/kelseysumner/Desktop/control_plot_1.png", device="png",
       height=10.5, width=11.2, units="in", dpi=400)

# now make a version that adds labels to the dots
control_plot_2 = ggplot(data=control_data,aes(x=expected,y=observed,color=target)) +
  geom_point() +
  labs(x="Expected Proportion", y="Observed Proportion", color="Target") +
  geom_abline(intercept = 0, slope = 1,lty="dashed") +
  geom_label_repel(aes(label = control),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',size=2)
control_plot_2

# export the plot
ggsave(control_plot_2, filename="/Users/kelseysumner/Desktop/control_plot_2.png", device="png",
       height=10.5, width=11.2, units="in", dpi=400)




