#### Description: this script produces manuscript Figure2
#### INPUT: "data.xlsx" 
#### OUTPUT: manuscript Figure2
#### Date: 18/02/2020
#### Author: Iratxe Rubio
#######################################################################

library(readxl) #read_xlsx function
library(RColorBrewer) #for colors 
library(tidyverse) 

#Open data
CV <- read_xlsx("data/data.xlsx", sheet = 4)
group <- read_xlsx("data/data.xlsx", sheet = 1)
data <- left_join(CV, group, by ="ID")

df <- data %>% 
        group_by(CV_component, CV_value) %>% 
        summarise(number = n())

df$CV_component <- as.factor(df$CV_component)
df$CV_value <- as.factor(df$CV_value)

CV_vars <- c("Recreational \n activities", #1
             "Private \n business", #2
             "Inspiration culture/\ndesign", #3
             "Traditional knowledge \n customs", #4
             "Belonging/ \n identity", #5
             "Job \n positions", #6
             "Tourism" #7
)

levels(df$CV_component) <- CV_vars

df$CV_component <- factor(df$CV_component,
                          levels(df$CV_component)[c(2,6,5:3,7,1)])

generated <- c("Not at all", #1
                "Slightly", #2
                "Moderately", #3
                "Very", #4
                "Extremely") #5

levels(df$CV_value) <- generated

group1 <- c("Extremely","Very","Moderately")
group2 <- c("Not at all","Slightly","Moderately")

#by group
df_group <- data %>% 
              group_by(CV_component, CV_value, group) %>% 
              summarise(number = n())

df_group$CV_component <- as.factor(df_group$CV_component)
df_group$CV_value <- as.factor(df_group$CV_value)

levels(df_group$CV_component) <- CV_vars

df_group$CV_component <- factor(df_group$CV_component,
                          levels(df_group$CV_component)[c(2,6,5:3,7,1)])

levels(df_group$CV_value) <- generated

df_group <- df_group %>%  
              mutate(perc = (number*100)/13)

df_group$new <- df_group$number
df_group$new <- ifelse(df_group$CV_value == "Moderately", 
                       df_group$number/2,
                       df_group$new)

jpeg("Figure2.jpg", 
    width = 12.5, height = 6.5, units = 'in', res = 300)

ggplot(df_group, aes(x = group, fill = CV_value)) + 
  geom_bar(data = subset(df_group, CV_value %in% group1),
           aes(y = new), 
           position = position_stack(reverse = T), 
           stat = "identity") +
  geom_bar(data = subset(df_group, CV_value %in% group2), 
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
        plot.title = element_text(size = 16),
        legend.justification = "top") +
  scale_fill_manual(values = c("Extremely" = "#018571", #brewer.pal(n = 5, name = 'BrBG')
                               "Very" = "#80CDC1",
                               "Moderately" = "gray87",
                               "Slightly" = "#DFC27D",
                               "Not at all" = "#A6611A"),
                    breaks = as.character(df_group$CV_value),
                    guide = guide_legend(reverse = T)) +
  scale_y_continuous(labels = abs) +
  facet_wrap(~CV_component, ncol = 4) 

dev.off()
