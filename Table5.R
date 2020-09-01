#### Description: this script produces manuscript Table4 
#### INPUT: "data.xlsx" 
#### OUTPUT: manuscript Table5
#### Date: 18/02/2020
#### Author: Iratxe Rubio
#######################################################################

library(readxl) #read_xlsx function
library(tidyverse)

#Open data
AC <- read_xlsx("data/data.xlsx", sheet = 2)

AC$ID_action <- as.factor(AC$ID_action)
AC$action_importance <- as.factor(AC$action_importance)
levels(AC$action_importance) <- c("first most important", "second most important",
                                  "third most important")

group <- read_xlsx("data/data.xlsx", sheet = 1)

d2 <- left_join(AC, group, by ="ID")
d <- d2 %>% distinct()
res <- d %>%
          group_by(group, action_importance, ID_action) %>%
          summarise(total = n())

res2 <- d %>%
          group_by(group, ID_action) %>%
          summarise(total = n())

important_actions <- d %>%
                        group_by(ID_action) %>%
                        summarise(group = paste0(unique(group), collapse = "; "),
                                  total = n())

important_actions <- important_actions[order(important_actions$total, decreasing = T),]

actions <- read_xlsx("data/data.xlsx", sheet = 3)
actions$ID_action <- as.factor(actions$ID_action)

data <- left_join(important_actions, actions, by ="ID_action")
data <- cbind(data[,4], data[,2], data[,3])

write.csv(data, row.names = F, "data/Table5.csv")
