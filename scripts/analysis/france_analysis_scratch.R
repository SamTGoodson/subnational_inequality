library(tidyverse)
library(tidymodels)
library(plm)

france_manifesto <- read.csv('C:/Users/samtg/github/subnational_inequality/data/cleaned/national/france_manifesto.csv')

#Get vote % for each party 
france_manifesto <- france_manifesto%>%
  mutate(perc_vote = (partyvote/totalvote)*100)
#Make a new df with only top % per region per year 
highest_vote <- france_manifesto %>%
  group_by(regionname, year) %>%
  filter(perc_vote == max(perc_vote)) %>%
  ungroup()



panel_data <- pdata.frame(highest_vote,index = c('regionname','year'))

fixed_model <- plm(rile ~ avg_gini, data = panel_data, model = "within")
summary(fixed_model)

test_model <- lm(rile ~ change_gini + year,data = highest_vote)
summary(test_model)
