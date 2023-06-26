#### Description: this script produces manuscript Figure5
#### INPUT: "data.xlsx" 
#### OUTPUT: manuscript Table 4 and 5
#### Date: 10/06/2023
#### Author: Iratxe Rubio
#######################################################################

library(readxl) #read_xlsx function
library(tidyverse)

#Open data
pres <- read_xlsx("data/data.xlsx", sheet = 6)
colnames(pres)[1] <- "code_5_2"
pres$sum <- pres$`Fishing industry` + pres$Research + pres$`NGOs others`+ pres$Governments

pres_short <- pres %>%
              filter(sum != 0)

past <- read_xlsx("data/data.xlsx", sheet = 7)
colnames(past)[1] <- "code_5_3"
past$sum <- past$`Fishing industry` + past$Research + past$`NGOs others`+ past$Governments

past_short <- past %>%
  filter(sum != 0)

actions <- read_xlsx("data/data.xlsx", sheet = 2)
actions$ID_action <- as.factor(actions$ID_action)

pres <- left_join(pres_short, actions, by ="code_5_2")
past <- left_join(past_short, actions, by ="code_5_3")

pres <- pres[,-c(1,6,7,9,10)]
past <- past[,-c(1,6,7,9,10)]

pres$Fishing_indus_prop <- round((pres$`Fishing industry`/6)*100, 0)
pres$Research_prop <- round((pres$`Research`/2)*100, 0)
pres$NGOs_prop <- round((pres$`NGOs others`/3)*100, 0)
pres$Gov_prop <- round((pres$`Governments`/2)*100, 0)
pres <- pres[,-c(1:4)]

past$Fishing_indus_prop <- round((past$`Fishing industry`/6)*100, 0)
past$Research_prop <- round((past$`Research`/2)*100, 0)
past$NGOs_prop <- round((past$`NGOs others`/3)*100, 0)
past$Gov_prop <- round((past$`Governments`/2)*100, 0)
past <- past[,-c(1:4)]

write.csv(pres, row.names = F, "Table4.csv")
write.csv(past, row.names = F, "Table5.csv")