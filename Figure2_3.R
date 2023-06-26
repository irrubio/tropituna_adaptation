#### Description: this script produces manuscript Figures 2 and 3
#### INPUT: "data.xlsx" 
#### OUTPUT: manuscript Figures 2 and 3
#### Date: 18/02/2020
#### Author: Iratxe Rubio
#######################################################################

library(readxl) #read_xlsx function
library(tidyverse)

#Open data
change1 <- read_xlsx("data/data.xlsx", sheet = 5)
group <- read_xlsx("data/data.xlsx", sheet = 1)
change <- left_join(change1, group, by ="ID")

change$change_value[change$change_value == "NO"] <- "No change" 
change$change_value[change$change_value == "DN"] <- "Don't know" 
change$change_value[change$change_value == "variability"] <- "Natural variability" 
change$change_value[change$change_value == "increase"] <- "Increase" 
change$change_value[change$change_value == "decrease"] <- "Decrease" 
change$change_value[change$change_value == "N/S"] <- "North/South change"

change$change_component[change$change_component == "weather_risks"] <- "Fishery\nrisks\n(n = 13)"
change$change_component[change$change_component == "stock_distribution"] <- "Stock\ndistribution\n(n = 13)"
change$change_component[change$change_component == "stock_abundance"] <- "Stock\nabundance\n(n = 13)"

####CC perception
change_CC_vars <- c("Don't know", #0
                    "Not at all", #1
                    "Slightly", #2
                    "Moderately", #3
                    "Very", #4
                    "Extremely") #5

#by groups
df_group <- change %>% 
              group_by(change_component, change_value, group) %>% 
              summarise(number = n())

df_group$change_value <- factor(df_group$change_value, levels = c("Increase",
                                                                  "Decrease",
                                                                  "North/South change",
                                                                  "Natural variability",
                                                                  "No change" ,
                                                                  "Don't know"))

df_group$proportion <- round((df_group$number/13)*100, 1)

jpeg("Figure2.jpg", 
    width = 11, height = 4, units = 'in', res = 300)

ggplot(df_group, aes(x = group,
               y = proportion,
               fill = change_value)) + 
  scale_fill_manual(values = c("#018571","#80CDC1","#CC79A7","#F0E442","black","#999999")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Percentage of respondents (%)") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.key.height = unit(0.7, "cm"),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16)) +
  #scale_fill_brewer(palette = 'Paired') +
  facet_wrap(~change_component) #+
  
dev.off()

#Change
df2_group <- change %>% 
              group_by(change_component, change_CC, group) %>% 
              summarise(number = n())

df2_group$change_CC[df2_group$change_CC == 6] <- 0
df2_group$change_CC <- as.factor(df2_group$change_CC)

levels(df2_group$change_CC) <- change_CC_vars

df2_group$proportion <- round((df2_group$number/13)*100, 1)


jpeg("Figure3.jpg", 
    width = 11, height = 4, units = 'in', res = 300)

ggplot(df2_group, aes(x = group, fill = change_CC)) + 
  geom_bar(data = df2_group,
           aes(y = proportion), 
           position = position_stack(reverse = T), 
           stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Percentage of respondents (%)") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.key.height = unit(0.7, "cm"),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16)) +
  scale_fill_manual(values = c("Extremely" = "#018571",
                               "Very" = "#80CDC1",
                               "Moderately" = "#F0E442",
                               "Slightly" = "#DFC27D",
                               "Not at all" = "#A6611A",
                               "Don't know" = "#999999"),
                    breaks = as.character(change_CC_vars),
                    guide = guide_legend(reverse = T)) +
  scale_y_continuous(labels = abs) + #, limits = c(-6,6)
  facet_wrap(~change_component) 
 
dev.off()
