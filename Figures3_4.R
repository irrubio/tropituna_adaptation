#### Description: this script produces manuscript Figures 3 and 4
#### INPUT: "data.xlsx" 
#### OUTPUT: manuscript Figures 3 and 4
#### Date: 18/02/2020
#### Author: Iratxe Rubio
#######################################################################

library(readxl) #read_xlsx function
library(tidyverse)

#Open data
change1 <- read_xlsx("data/DATA.xlsx", sheet = 5)
group <- read_xlsx("data/DATA.xlsx", sheet = 1)
change <- left_join(change1, group, by ="ID")

change$change_value[change$change_value == "NO"] <- "No change" 
change$change_value[change$change_value == "DN"] <- "Don't know" 
change$change_value[change$change_value == "variability"] <- "Variability" 
change$change_value[change$change_value == "increase"] <- "Increase" 
change$change_value[change$change_value == "decrease"] <- "Decrease" 
change$change_value[change$change_value == "N/S"] <- "North/South change"

change$change_component[change$change_component == "weather_risks"] <- "Fishery\nrisks"
change$change_component[change$change_component == "stock_distribution"] <- "Stock\ndistribution"
change$change_component[change$change_component == "stock_abundance"] <- "Stock\nabundance"

df <- change %>% 
        group_by(change_component, change_value) %>% 
        summarise(number = n())

df$change_value <- as.factor(df$change_value)

df$change_value <- factor(df$change_value,
                          levels(df$change_value)[c(1,3,5,6,4,2)])

####CC perception
df2 <- change %>% 
  group_by(change_component, change_CC) %>% 
  summarise(number = n())

df2$change_CC[df2$change_CC == 6] <- 0
df2$change_CC <- as.factor(df2$change_CC)

change_CC_vars <- c("Don't know", #0
                    "Not at all", #1
                    "Slightly", #2
                    "Moderately", #3
                    "Very", #4
                    "Extremely") #5

levels(df2$change_CC) <- change_CC_vars

# Grouped barplot
group1 <- c("Extremely","Very","Moderately")
group2 <- c("Moderately","Slightly","Not at all","Don't know")

#by groups
df_group <- change %>% 
              group_by(change_component, change_value, group) %>% 
              summarise(number = n())

df_group$change_value <- as.factor(df_group$change_value)

jpeg("Figure3.jpg", 
    width = 11, height = 4, units = 'in', res = 300)

ggplot(df_group, aes(x = group,
               y = number,
               fill = change_value)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Number of responses") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.key.height = unit(0.7, "cm"),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16)) +
  scale_fill_brewer(palette = 'Paired') +
  facet_wrap(~change_component) #+
  
dev.off()

#Change
df2_group <- change %>% 
              group_by(change_component, change_CC, group) %>% 
              summarise(number = n())

df2_group$change_CC[df2_group$change_CC == 6] <- 0
df2_group$change_CC <- as.factor(df2_group$change_CC)

levels(df2_group$change_CC) <- change_CC_vars

df2_group$new <- df2_group$number
df2_group$new <- ifelse(df2_group$change_CC == "Moderately", 
                       df2_group$number/2, 
                       df2_group$new)

jpeg("Figure4.jpg", 
    width = 11, height = 4, units = 'in', res = 300)

ggplot(df2_group, aes(x = group, fill = change_CC)) + 
  geom_bar(data = subset(df2_group, change_CC %in% group1),
           aes(y = new), 
           position = position_stack(reverse = T), 
           stat = "identity") +
  geom_bar(data = subset(df2_group, change_CC %in% group2), 
           aes(y = -new),
           stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Number of responses") +
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
                               "Moderately" = "gray87",
                               "Slightly" = "#DFC27D",
                               "Not at all" = "#A6611A",
                               "Don't know" = "black"),
                    breaks = as.character(change_CC_vars),
                    guide = guide_legend(reverse = T)) +
  scale_y_continuous(labels = abs) + #, limits = c(-6,6)
  facet_wrap(~change_component) 
 
dev.off()
