
bps_final_Wythe<- readRDS(file = "bps_final_Wythe.rds")
# Plot
bps_final_Wythe %>%
  ggplot( aes(x=year, y=tot_units)) +
  geom_line( color="navy") +
  geom_point(shape=21, color="red", fill="#0066ff", size=2) +
  theme(legend.title = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_blank(), plot.subtitle = element_text(color="black", size=9, hjust = 0),
        panel.background = element_blank()) +
  ggtitle("Trend of Building Unit Permits by Year") + 
  labs(title = "Building Permits in Wythe County", x = "Year", y = "Total Housing Units") + 
  scale_x_continuous(breaks = seq(1990, 2017, by = 5))

f<- readRDS(file = "housing_ages.rds")
housing_age <- ggplot(f, aes(x=`Median Housing Stock Age`, y=`Proportion of Housing`, fill=as.factor(`Median Housing Stock Age`))) + 
  geom_bar(stat = "identity") +
  coord_flip() + theme(legend.title = element_blank(), plot.title = element_blank(), plot.subtitle = element_text(color="black", size=9, hjust = 0), axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.background = element_blank()) + 
  scale_fill_viridis_d(name = "Age of Housing", labels = c("0-6 years", "7-10 years", "11-20 years", "21-30 years","30+ years")) 
