install.packages('lme4')
install.packages('emmeans')
install.packages('lmerTest')

library(tidyverse)
library(tidymodels)
library(plm)
library(languageserver)
library(lubridate)
library(janitor)
library(lme4)
library(emmeans)
library(lmerTest)

france_manifesto <- read.csv('C:/Users/samtg/github/subnational_inequality/data/cleaned/national/france_manifesto.csv')
multi_country <- read.csv('C:/Users/samtg/github/subnational_inequality/data/cleaned/national/joined_electoral_lissy.csv')
mp <- read.csv('data/raw/parties/manifesto_full.csv')
crosswalk<-read.csv('data/raw/crosswalks/manif_cross.csv')
ches <- read.csv('data/raw/parties/ches.csv')

# Get Crosswalk
file_name <- "partyfacts-mapping.csv"
if( ! file_name %in% list.files()) {
  url <- "https://partyfacts.herokuapp.com/download/external-parties-csv/"
  download.file(url, file_name)
}
partyfacts_raw <- read_csv(file_name, guess_max = 50000)
partyfacts <- partyfacts_raw |> filter(! is.na(partyfacts_id))

dataset_1 <- partyfacts |> filter(dataset_key == "manifesto")

#Check mp colnames 
colnames(mp)
head(mp)
head(multi_country)
colnames(ches)

#Create mp$year col from year in mp$edate, formated mm/dd/yyy
mp$year <- year(dmy(mp$edate))

mp%>%
select(year,edate)%>%
head()

#Drop Belgium
multi_country <- multi_country %>% 
  filter(country_name != "Belgium")

# Inspect cols to join on
multi_country%>%
tabyl(country_name,partyfacts_id)

mp%>%
filter(countryname == "France")%>%
select(countryname,party,year)%>%
head()

multi_country%>% drop_na(country_name,partyfacts_id,,dataset_party_id,year) %>%
select(country_name,partyfacts_id,dataset_party_id,year)%>%
head()

head(mp)
head(crosswalk)
# Calculate rate of NAs in partyfacts_id for each country
multi_country%>%
  group_by(country_name)%>%
  summarize(perc_na = mean(is.na(partyfacts_id)))

# Join crosswalk to multi_country on partyfacts_id
multi_country <- multi_country %>% 
  left_join(crosswalk, by = c("partyfacts_id" = "partyfacts_id"))

#Join mp to multi_country on year, country, and partyname
multi_country <- multi_country %>%
  left_join(mp, by = c("country_name" = "countryname","year" = "year","dataset_party_id" = "party"))

tail(multi_country)

# Calculate percent NA in rile for each country
multi_country%>%
  group_by(country_name)%>%
  summarize(perc_na = mean(is.na(dataset_party_id)))

#Drop NA rows from multi_country
multi_country <- multi_country %>% 
  drop_na(rile,interp_gini)

#Get vote % for each party 
france_manifesto <- france_manifesto%>%
  mutate(perc_vote = (partyvote/totalvote)*100)

multi_country <- multi_country %>%
  mutate(perc_vote = (partyvote/totalvote)*100)
#Make a new df with only top % per region per year 
highest_vote <- france_manifesto %>%
  group_by(regionname, year) %>%
  filter(perc_vote == max(perc_vote)) %>%
  ungroup()

highest_vote_m <- multi_country%>%
  group_by(cleaned_region_x, year) %>%
  filter(perc_vote == max(perc_vote)) %>%
  ungroup()


panel_data <- pdata.frame(highest_vote_m,index = c('country_name','year'))

fixed_model <- plm(rile ~ avg_gini, data = panel_data, model = "within")
summary(fixed_model)

test_model <- lm(rile ~ avg_gini + year,data = highest_vote_m)
summary(test_model)

# Run a multilevel model using lme4 with random intercepts for country
random_model <- lmer(rile ~ interp_gini + interp_ed + interp_im + (1|country_name), data = highest_vote_m)
summary(random_model)

# Run a multilevel model using lme4 with random intercepts for country and random slopes for interp_gini
random_model2 <- lmer(rile ~ interp_gini + interp_ed + interp_im + (interp_gini|country_name), data = highest_vote_m)
summary(random_model2)

random_model2 <- lmer(rile ~ interp_gini + interp_ed + interp_im + (1|country_name) + (interp_gini|country_name), data = highest_vote_m)


emm_interp_gini <- emmeans(random_model2, specs = ~interp_gini|country_name)

# run a simple ols model with fixed year and country effects 
fixed_model2 <- lm(welfare ~ interp_gini + factor(country_name) + factor(year), data = highest_vote_m)
summary(fixed_model2)

# use lme4 to run a model with fixed country and year effects
random_model3 <- lmer(markeco ~ delta_gini + (1|country_name) + (1|year), data = highest_vote_m)
summary(random_model3)

colnames(highest_vote_m)

# Create a new var that is the sum of per403 per404 per406 per409 per412 per415 per504 and per701, treating NAs as zeros
highest_vote_m$socdem_sum <- rowSums(highest_vote_m[,c('per403','per404','per406','per409','per412','per415','per504','per701')], na.rm = TRUE)
highest_vote_m%>%
tabyl(socdem_sum)

random_model3 <- lmer(socdem_sum ~ interp_gini * interp_ed + (1|country_name) + (1|year), data = highest_vote_m)
summary(random_model3)
