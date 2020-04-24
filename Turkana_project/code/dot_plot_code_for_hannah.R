# create a data frame summarizing each haplotype and the months it is present
# gather the code to long format

# summarize the new data set by month
month_summary = long_csp_merged %>% 
  group_by(month,haplotype) %>%
  summarize(n_samples=n())

# summarize the new data set by location
location_summary = long_csp_merged %>%
  group_by(month,haplotype,HH_ID) %>%
  summarise(n_1 = n_distinct(HH_ID)) %>%
  select(month,haplotype,n_1) %>%
  summarise(n_households=sum(n_1,na.rm=T))

# merge the month and location summaries
merged_summary = left_join(month_summary,location_summary,by=c("month","haplotype"))

# make a figure of the csp haplotypes present over time across all samples (regardless if human or mosquito)
csp_month_plot = ggplot(merged_summary, aes(x=month, y=haplotype, size=n_samples, color=n_households)) +
  geom_point() +
  scale_colour_gradient(low = "#c2a5cf", high = "#3f007d") +
  labs(x = "Month and year",y="Haplotype", color = "Number of households", size = "Number of samples") + 
  theme_bw()
csp_month_plot

