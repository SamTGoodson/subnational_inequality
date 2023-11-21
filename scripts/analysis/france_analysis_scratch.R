library(tidyverse)
library(tidymodels)
library(plm)
library(languageserver)
library(lubridate)
library(janitor)
library(lme4)
library(emmeans)
library(lmerTest)
library(lmtest)
library(stargazer)
library(ggtext)
library(broom)
library(ggrepel)        
library(MetBrewer) 
library(gt)
library(sf)
library(webshot2)

install.packages('webshot2')

france_manifesto <- read.csv('C:/Users/samtg/github/subnational_inequality/data/cleaned/national/france_manifesto.csv')
multi_country <- read.csv('C:/Users/samtg/github/subnational_inequality/data/cleaned/national/joined_electoral_lissy.csv')
mp <- read.csv('data/raw/parties/manifesto_full.csv')
crosswalk<-read.csv('data/raw/crosswalks/manif_cross.csv')
ches <- read.csv('data/raw/parties/ches.csv')
multi_country <- read.csv("C:/Users/samtg/github/subnational_inequality/data/cleaned/national/updated_multi_country.csv")
getwd()
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


dim(multi_country)
str(multi_country)
summary(multi_country)
head(multi_country)
sum(is.na(multi_country))


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
colnames(multi_country)
colnames(highest_vote_m)
# Same thing with tiebreaker and an averaging for years with multiple elections (eg. Spain 2019)
highest_vote_m <- multi_country %>%
  group_by(cleaned_region_x, year, party_english) %>%
  mutate(avg_perc_vote = mean(perc_vote, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(cleaned_region_x, year) %>%
  filter(avg_perc_vote == max(avg_perc_vote)) %>%
  slice_head(n = 1) %>%
  ungroup()


panel_data <- pdata.frame(highest_vote_m,index = c('cleaned_region_x','year'))

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


head(multi_country)

# Explore immigration codes 
highest_vote_m %>%
drop_na(per608,per608_1,per608_2,per608_3)%>%
select(per608,per608_1,per608_2,per608_3)%>%
head()

# Create a new var that is the sum of anti_immigrant categories, treating NAs as zeros
highest_vote_m$imm_sum <- rowSums(highest_vote_m[,c('per608','per601')], na.rm = TRUE)

#create the same var in multi_country
multi_country$imm_sum <- rowSums(multi_country[,c('per608','per601')], na.rm = TRUE)
#create new var perc_im that multiplies im_sum by perc_vote
multi_country$perc_im <- multi_country$imm_sum * multi_country$perc_vote
#rollup multi_year by region and year with the sum of perc_im
multi_year <- multi_country %>%
  group_by(country_name,cleaned_region_x, year) %>%
  summarize(perc_im = sum(perc_im, na.rm = TRUE),
  interp_gini = mean(interp_gini, na.rm = TRUE),
  interp_im = mean(interp_im, na.rm = TRUE)) %>%
  ungroup()

# calculate z-score for perc_im
multi_year$z_perc_im <- scale(multi_year$perc_im)  

# Create a new var that is the sum of per403 per404 per406 per409 per412 per415 per504 and per701, treating NAs as zeros
highest_vote_m$socdem_sum <- rowSums(highest_vote_m[,c('per403','per404','per406','per409','per412','per415','per504','per701')], na.rm = TRUE)
#create teh same var in multi_country
multi_country$socdem_sum <- rowSums(multi_country[,c('per403','per404','per406','per409','per412','per415','per504','per701')], na.rm = TRUE)
#create new var perc_socdem that multiplies socdem_sum by perc_vote
multi_country$perc_socdem <- multi_country$socdem_sum * multi_country$perc_vote
#rollup multi_year by region and year with the sum of perc_socdem
multi_year_soc <- multi_country %>%
  group_by(country_name,cleaned_region_x, year) %>%
  summarize(perc_socdem = sum(perc_socdem, na.rm = TRUE),
  interp_gini = mean(interp_gini, na.rm = TRUE),
  interp_im = mean(interp_im, na.rm = TRUE)) %>%
  ungroup()

# calculate z-score for perc_socdem
multi_year_soc$z_perc_socdem <- scale(multi_year_soc$perc_socdem)

# Run a simple ols model with fixed year and country effects with multi_year with perc_im as the DV and interp_gini and interp_im as the IVs
fixed_model3 <- lm(z_perc_im ~ interp_gini + interp_im + factor(country_name) + factor(year), data = multi_year)
summary(fixed_model3)

# Run a simple ols model with fixed year and country effects with multi_year_soc with perc_socdem as the DV and interp_gini and interp_im as the IVs
fixed_model4 <- lm(z_perc_socdem ~ interp_gini + interp_im + factor(country_name) + factor(year), data = multi_year_soc)
summary(fixed_model4)


# Trying to make bar chart
pdata <- pdata.frame(multi_year, index = c("cleaned_region_x", "year"))
panel_model <- plm(z_perc_im ~ interp_gini + interp_im + factor(year) + 
                   country_name:interp_gini, 
                   data = pdata, 
                   model = "random")


# Make a model with fixed country and year effects
random_im_model <- lmer(imm_sum ~ interp_gini + delta_gini + (1|country_name) + (1|year), data = highest_vote_m)
summary(random_im_model)
# Now with random slopes for interp_gini
random_im_model2 <- lmer(imm_sum ~ interp_gini + delta_gini + (interp_gini|country_name) + (1|year), data = highest_vote_m)
summary(random_im_model2)


highest_vote_m_p <- pdata.frame(highest_vote_m, index = c("cleaned_region_x", "year"))

fixed_im_model <- plm(imm_sum ~ interp_gini + delta_gini, data = highest_vote_m_p, model = "within")
summary(fixed_im_model)

random_im_model_plm <- plm(imm_sum ~ interp_gini + delta_gini, data = highest_vote_m_p, model = "random")
summary(random_im_model_plm)

phtest(fixed_im_model, random_im_model_plm)


fixed_im_model <- plm(imm_sum ~ interp_gini + delta_gini + imm_sum * factor(country_name), data = highest_vote_m_p, model = "within")
summary(fixed_im_model)

fixed_im_model <- plm(imm_sum ~ interp_gini + delta_gini + imm_sum:factor(country_name), data = highest_vote_m_p, model = "within")
summary(fixed_im_model)

ols_model <- lm(imm_sum ~ interp_gini + delta_gini + imm_sum * factor(country_name), data = highest_vote_m)
summary(ols_model)

# Plot the interactions
highest_vote_m%>%
ggplot(aes(x = delta_gini, y = imm_sum, color = country_name)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~country_name, ncol = 2)+
  theme_minimal()


highest_vote_m %>%
  ggplot(aes(x = interp_im, y = imm_sum, color = country_name, 
             label = paste(cleaned_region_x, year, party_english, sep = "-"))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(nudge_y = 0.5, check_overlap = TRUE) +
  facet_wrap(~country_name, ncol = 2) +
  theme_minimal()


colnames(multi_country)
multi_country%>%
  tabyl(parfam)

# Make new vars for analysis
change_df <- highest_vote_m %>%

  arrange(cleaned_region_x, year) %>%
  group_by(cleaned_region_x, year) %>%
  mutate(change_in_parfam = if_else(lag(parfam) != parfam & !is.na(lag(parfam)), 1, 0, missing = 0))

change_df %>%
tabyl(change_in_parfam)

highest_vote_m %>%
  group_by(cleaned_region_x,year,parfam) %>%
  mutate(lag_parfam = lag(parfam, order_by = year)) %>%
  ungroup() %>%
  select(cleaned_region_x, year, parfam, lag_parfam)

party_family <- highest_vote_m %>%
  group_by(cleaned_region_x,year,parfam) %>%
  pivot_wider(names_from = parfam, values_from = perc_vote) %>%
  rename(nationalist = `70`, socialist = `20`)

parfam_ols <- lm(socialist ~ interp_im + interp_gini * factor(country_name) + delta_gini, data = party_family)
summary(parfam_ols)


party_family%>%
ggplot(aes(x = interp_gini, y = nationalist, color = country_name)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~country_name, ncol = 2)+
  theme_minimal()

# Try and get a var for change in parfam by region and year 
highest_vote_m %>%
  mutate(lag_parfam = lag(parfam, order_by = c(cleaned_region_x,year)))%>%
  select(cleaned_region_x, year, parfam, lag_parfam)

highest_vote_m %>%
select(country,cleaned_region_x,year,partyvote,parfam)%>%
head()

highest_vote_m %>%
  select(country, cleaned_region_x, year, partyvote, parfam) %>%
  group_by(cleaned_region_x) %>%
  arrange(year) %>%
  mutate(vote_change = ifelse(parfam != lag(parfam, default = parfam[1]), 1, 0)) %>%
  ungroup() %>%
  filter(cleaned_region_x == 'abruzzo') %>%
  head()

colnames(multi_country)
write.csv(multi_country, 'data/cleaned/national/multi_country.csv')

theme_mfx <- function() {
  theme_minimal(base_family = "IBM Plex Sans Condensed") +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.title = element_text(face = "bold"))
}

# Make labels use IBM Plex Sans by default
update_geom_defaults("label", 
                     list(family = "IBM Plex Sans Condensed"))
update_geom_defaults(ggtext::GeomRichText, 
                     list(family = "IBM Plex Sans Condensed"))
update_geom_defaults("label_repel", 
                     list(family = "IBM Plex Sans Condensed"))

# Use the Johnson color palette
clrs <- met.brewer("Johnson")

nice_number <- label_number(style_negative = "minus", accuracy = 0.01)
nice_p <- label_pvalue(prefix = c("p < ", "p = ", "p > "))


model_1 <- plm(z_perc_socdem ~ z_perc_socdem_lag + interp_gini + 
               unemployment + immig_ratio, 
               data = soc_panel, 
               model = "within")

model_2 <- plm(z_perc_socdem ~ z_perc_socdem_lag + interp_ed  +
               unemployment + immig_ratio, 
               data = soc_panel, 
               model = "within")
model_3 <- plm(z_perc_socdem ~ z_perc_socdem_lag + interp_im  +
               unemployment + immig_ratio, 
               data = soc_panel, 
               model = "within")
model_4 <- plm(z_perc_socdem ~ z_perc_socdem_lag + wage_ratio + 
               unemployment + immig_ratio, 
               data = soc_panel, 
               model = "within")  
model_5 <- plm(z_perc_socdem ~ z_perc_socdem_lag + interp_gini + interp_ed + interp_im + wage_ratio +  
               unemployment + immig_ratio,
               data = soc_panel,
               model = "within")              

stargazer(model_1,model_2,model_3,model_4,model_5,type='html',out='data/output/z_perc_socdem.html')

model_1 <- plm(z_perc_im ~ z_perc_im_lag + interp_gini + 
                 unemployment + immig_ratio, 
               data = im_panel, 
               model = "within")

model_2 <- plm(z_perc_im ~ z_perc_im_lag + interp_ed  +
                 unemployment + immig_ratio, 
               data = im_panel, 
               model = "within")
model_3 <- plm(z_perc_im ~ z_perc_im_lag + interp_im  +
                 unemployment + immig_ratio, 
               data = im_panel, 
               model = "within")
model_4 <- plm(z_perc_im ~ z_perc_im_lag + wage_ratio + 
                 unemployment + immig_ratio, 
               data = im_panel, 
               model = "within")  
model_5 <- plm(z_perc_im ~ z_perc_im_lag + interp_gini + interp_ed + interp_im + wage_ratio +  
                 unemployment + immig_ratio,
               data = im_panel,
               model = "within")              

stargazer(model_1,model_2,model_3,model_4,model_5,type='html',out='data/output/z_perc_im.html')


model_1 <- plm(rile ~ rile_lag + interp_avg_gini + 
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")

model_2 <- plm(rile ~ rile_lag + interp_ed_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")
model_3 <- plm(rile ~ rile_lag + interp_im_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")
model_4 <- plm(rile ~ rile_lag + interp_wage_ratio + 
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")  
model_5 <- plm(rile ~ rile_lag + interp_avg_gini + interp_ed_ratio + interp_im_ratio + interp_wage_ratio +  
                 unemployment_ratio + interp_immig_ratio,
               data = hv_panel,
               model = "within")              

stargazer(model_1,model_2,model_3,model_4,model_5,type='html',out='data/output/rile.html',
          dep.var.labels = 'Rile', covariate.labels = c('Rile Lag','Gini','Education Ratio',
                                                                        'Immigration Ratio','Manufacturing Wage',
                                                                        'Unemployment','Immigrants per 100k'),
          omit.stat=c("LL","ser","f"), no.space=TRUE)



model_1 <- plm(z_socdem_sum ~ z_socdem_sum_lag + interp_avg_gini + 
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")

model_2 <- plm(z_socdem_sum ~ z_socdem_sum_lag + interp_ed_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")
model_3 <- plm(z_socdem_sum ~ z_socdem_sum_lag + interp_im_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")
model_4 <- plm(z_socdem_sum ~ z_socdem_sum_lag + interp_wage_ratio + 
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")  
model_5 <- plm(z_socdem_sum ~ z_socdem_sum_lag + interp_avg_gini + interp_ed_ratio + interp_im_ratio + interp_wage_ratio +  
                 unemployment_ratio + interp_immig_ratio,
               data = hv_panel,
               model = "within")              

stargazer(model_1,model_2,model_3,model_4,model_5,type='html',out='data/output/z_socdem_sum.html')

model_1 <- plm(z_socdem_sum ~ z_socdem_sum_lag + interp_avg_gini + 
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")

model_2 <- plm(z_socdem_sum ~ z_socdem_sum_lag + interp_ed_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")
model_3 <- plm(z_socdem_sum ~ z_socdem_sum_lag + interp_im_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")
model_4 <- plm(z_socdem_sum ~ z_socdem_sum_lag + interp_wage_ratio + 
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")  
model_5 <- plm(z_socdem_sum ~ z_socdem_sum_lag + interp_avg_gini + interp_ed_ratio + interp_im_ratio + interp_wage_ratio +  
                 unemployment_ratio + interp_immig_ratio,
               data = hv_panel,
               model = "within")              

stargazer(model_1,model_2,model_3,model_4,model_5,type='html',out='data/output/z_socdem_sum.html')

model_1 <- plm(welfare ~ welfare_lag + interp_avg_gini + 
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")

model_2 <- plm(welfare ~ welfare_lag + interp_ed_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")
model_3 <- plm(welfare ~ welfare_lag + interp_im_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")
model_4 <- plm(welfare ~ welfare_lag + interp_wage_ratio + 
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")  
model_5 <- plm(welfare ~ welfare_lag + interp_avg_gini + interp_ed_ratio + interp_im_ratio + interp_wage_ratio +  
                 unemployment_ratio + interp_immig_ratio,
               data = hv_panel,
               model = "within")              

stargazer(model_1,model_2,model_3,model_4,model_5,type='html',out='data/output/welfare.html')

model_1 <- plm(markeco ~ markeco_lag + interp_avg_gini + 
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")

model_2 <- plm(markeco ~ markeco_lag + interp_ed_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")
model_3 <- plm(markeco ~ markeco_lag + interp_im_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")
model_4 <- plm(markeco ~ markeco_lag + interp_wage_ratio + 
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")  
model_5 <- plm(markeco ~ markeco_lag + interp_avg_gini + interp_ed_ratio + interp_im_ratio + interp_wage_ratio +  
                 unemployment_ratio + interp_immig_ratio,
               data = hv_panel,
               model = "within")              

stargazer(model_1,model_2,model_3,model_4,model_5,type='html',out='data/output/markeco.html',
          dep.var.labels = 'Market Economy Score', covariate.labels = c('Markeco Lag','Gini','Education Ratio',
                                                                        'Immigration Ratio','Manufacturing Wage',
                                                                        'Unemployment','Immigrants per 100k'),
          omit.stat=c("LL","ser","f"), no.space=TRUE)



model_1 <- plm(z_welplan ~ z_welplan_lag + interp_avg_gini + 
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")

model_2 <- plm(z_welplan ~ z_welplan_lag + interp_ed_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")
model_3 <- plm(z_welplan ~ z_welplan_lag + interp_im_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")
model_4 <- plm(z_welplan ~ z_welplan_lag + interp_wage_ratio + 
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")  
model_5 <- plm(z_welplan ~ z_welplan_lag + interp_avg_gini + interp_ed_ratio + interp_im_ratio + interp_wage_ratio +  
                 unemployment_ratio + interp_immig_ratio,
               data = hv_panel,
               model = "within")              

stargazer(model_1,model_2,model_3,model_4,model_5,type='html',out='data/output/hv_welpan.html')


model_1 <- plm(z_perc_welpan ~ z_perc_welpan_lag + interp_gini + 
                 unemployment + immig_ratio, 
               data = welpan, 
               model = "within")

model_2 <- plm(z_perc_welpan ~ z_perc_welpan_lag + interp_ed  +
                 unemployment + immig_ratio, 
               data = welpan, 
               model = "within")
model_3 <- plm(z_perc_welpan ~ z_perc_welpan_lag + interp_im  +
                 unemployment + immig_ratio, 
               data = welpan, 
               model = "within")
model_4 <- plm(z_perc_welpan ~ z_perc_welpan_lag + wage_ratio + 
                 unemployment + immig_ratio, 
               data = welpan, 
               model = "within")  
model_5 <- plm(z_perc_welpan ~ z_perc_welpan_lag + interp_gini + interp_ed + interp_im + wage_ratio +  
                 unemployment + immig_ratio,
               data = welpan,
               model = "within")              

stargazer(model_1,model_2,model_3,model_4,model_5,type='html',out='data/output/z_welplan.html')

model_1 <- plm(z_perc_markeco ~ z_perc_markeco_lag + interp_gini + 
                 unemployment + immig_ratio, 
               data = markpan, 
               model = "within")

model_2 <- plm(z_perc_markeco ~ z_perc_markeco_lag + interp_ed  +
                 unemployment + immig_ratio, 
               data = markpan, 
               model = "within")
model_3 <- plm(z_perc_markeco ~ z_perc_markeco_lag + interp_im  +
                 unemployment + immig_ratio, 
               data = markpan, 
               model = "within")
model_4 <- plm(z_perc_markeco ~ z_perc_markeco_lag + wage_ratio + 
                 unemployment + immig_ratio, 
               data = markpan, 
               model = "within")  
model_5 <- plm(z_perc_markeco ~ z_perc_markeco_lag + interp_gini + interp_ed + interp_im + wage_ratio +  
                 unemployment + immig_ratio,
               data = markpan,
               model = "within")              

stargazer(model_1,model_2,model_3,model_4,model_5,type='html',out='data/output/z_markpan.html')


model_1 <- plm(z_im_sum ~ z_im_sum_lag + interp_avg_gini + 
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")

model_2 <- plm(z_im_sum ~ z_im_sum_lag + interp_ed_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")
model_3 <- plm(z_im_sum ~ z_im_sum_lag + interp_im_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")
model_4 <- plm(z_im_sum ~ z_im_sum_lag + interp_wage_ratio + 
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")  
model_5 <- plm(z_im_sum ~ z_im_sum_lag + interp_avg_gini + interp_ed_ratio + interp_im_ratio + interp_wage_ratio +  
                 unemployment_ratio + interp_immig_ratio,
               data = hv_panel,
               model = "within")              

stargazer(model_1,model_2,model_3,model_4,model_5,type='html',out='data/output/hv_z_im_sum.html')

model_1 <- plm(z_perc_cl ~ z_perc_cl_lag + interp_gini +   
                 unemployment + immig_ratio, 
               data = cl_pan, 
               model = "within")

model_2 <- plm(z_perc_cl ~ z_perc_cl_lag + interp_ed  +   
                 unemployment + immig_ratio, 
               data = cl_pan, 
               model = "within")
model_3 <- plm(z_perc_cl ~ z_perc_cl_lag + interp_im +   
                 unemployment + immig_ratio,  
               data = cl_pan, 
               model = "within")
model_4 <- plm(z_perc_cl ~ z_perc_cl_lag +  wage_ratio +  
                 unemployment + immig_ratio, 
               data = cl_pan, 
               model = "within")  
model_5 <- plm(z_perc_cl ~ z_perc_cl_lag + interp_gini + interp_ed + interp_im + wage_ratio +  
                 unemployment + immig_ratio, 
               data = cl_pan,
               model = "within")              

stargazer(model_1,model_2,model_3,model_4,model_5,type='html',out='data/output/z_perc_cl_lag.html')

model_1 <- plm(z_cl ~ z_cl_lag + interp_avg_gini + 
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")

model_2 <- plm(z_cl ~ z_cl_lag + interp_ed_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")
model_3 <- plm(z_cl ~ z_cl_lag + interp_im_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")
model_4 <- plm(z_cl ~ z_cl_lag + interp_wage_ratio + 
                 unemployment_ratio + interp_immig_ratio, 
               data = hv_panel, 
               model = "within")  
model_5 <- plm(z_cl ~ z_cl_lag + interp_avg_gini + interp_ed_ratio + interp_im_ratio + interp_wage_ratio +  
                 unemployment_ratio + interp_immig_ratio,
               data = hv_panel,
               model = "within")              

stargazer(model_1,model_2,model_3,model_4,model_5,type='html',out='data/output/hv_cl.html')



#Try to visualize 
#Markeco
tidied<-model_5 %>%
  tidy(conf.int = TRUE, conf.level = 0.9)%>%
  mutate(nice_slope = nice_number(estimate))

name_mapping <- c(
  "markeco_lag" = "Lagged Markeco",
  "interp_avg_gini" = "Average Gini",
  "interp_ed_ratio" = "Education Ratio",
  "interp_im_ratio" = "Immigrant Wage Ratio",
  "interp_wage_ratio" = "Manufacturing Wage Ratio",
  "unemployment_ratio" = "Unemployment",
  "interp_immig_ratio" = "Immigrats per 100k"
)

tidied <- tidied %>%
  mutate(nice_term = name_mapping[as.character(term)])



ggplot(tidied, aes(x = estimate, y = nice_term, color = term)) +
  geom_vline(xintercept = 0, linewidth = 0.5, linetype = "24", color = clrs[1]) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_label(aes(label = nice_slope), nudge_y = 0.3) +
  labs(title = "Winning Party's Market Economy Score", subtitle = '90 Percent Confidence Intervals',x = NULL, y = NULL) +
  scale_color_manual(values = c(clrs[4], clrs[1], clrs[2],clrs[3],clrs[5],clrs[4],clrs[1]), guide = "none") +
  theme_mfx()


# Rile

tidied_rile<-model_5 %>%
  tidy(conf.int = TRUE, conf.level = 0.9)%>%
  mutate(nice_slope = nice_number(estimate))



name_mapping <- c(
  "rile_lag" = "Lagged Rile",
  "interp_avg_gini" = "Average Gini",
  "interp_ed_ratio" = "Education Ratio",
  "interp_im_ratio" = "Immigrant Wage Ratio",
  "interp_wage_ratio" = "Manufacturing Wage Ratio",
  "unemployment_ratio" = "Unemployment",
  "interp_immig_ratio" = "Immigrats per 100k"
)

tidied_rile <- tidied_rile %>%
  mutate(nice_term = name_mapping[as.character(term)])



ggplot(tidied_rile, aes(x = estimate, y = nice_term, color = term)) +
  geom_vline(xintercept = 0, linewidth = 0.5, linetype = "24", color = clrs[1]) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_label(aes(label = nice_slope), nudge_y = 0.3) +
  labs(title = "Winning Party's Rile Score", subtitle = '90 Percent Confidence Intervals',x = NULL, y = NULL) +
  scale_color_manual(values = c(clrs[4], clrs[1], clrs[2],clrs[3],clrs[5],clrs[4],clrs[1]), guide = "none") +
  theme_mfx()


#z_perc_socdem
tidied_socdem<-model_5 %>%
  tidy(conf.int = TRUE, conf.level = 0.9)%>%
  mutate(nice_slope = nice_number(estimate))


name_mapping <- c(
  "z_perc_socdem_lag" = "Lagged Soc",
  "interp_avg_gini" = "Average Gini",
  "interp_ed_ratio" = "Education Ratio",
  "interp_im_ratio" = "Immigrant Wage Ratio",
  "interp_wage_ratio" = "Manufacturing Wage Ratio",
  "unemployment_ratio" = "Unemployment",
  "interp_immig_ratio" = "Immigrats per 100k"
)

tidied_socdem <- tidied_socdem %>%
  mutate(nice_term = name_mapping[as.character(term)])



ggplot(tidied_rile, aes(x = estimate, y = nice_term, color = term)) +
  geom_vline(xintercept = 0, linewidth = 0.5, linetype = "24", color = clrs[1]) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_label(aes(label = nice_slope), nudge_y = 0.3) +
  labs(title = "Winning Party's Socialist Score", subtitle = '90 Percent Confidence Intervals',x = NULL, y = NULL) +
  scale_color_manual(values = c(clrs[4], clrs[1], clrs[2],clrs[3],clrs[5],clrs[4],clrs[1]), guide = "none") +
  theme_mfx()



pdwtest(model_5)

summary(hv_panel$rile)
hv_panel%>%
  select(interp_immig,rile,rile_lag)%>%
  view()


highest_vote_m%>%
  ggplot(aes(x=interp_im_ratio,y=markeco,color = factor(country_name)))+
  geom_smooth(method='lm')+
  theme_minimal()

highest_vote_m%>%
  group_by(country_name,year)%>%
  summarise(n = n())%>%
  gt()


eu_map <- read_sf('C:/Users/samtg/github/subnational_inequality/data/raw/shaperfiles')
head(eu_map)


coverage_summary <- multi_country %>%
  group_by(country_name) %>%
  summarise(unique_regions = n_distinct(region),
            years_list = toString(sort(unique(year))),
            total_distinct_parties = n_distinct(party_abbreviation)) %>%
  ungroup()

coverage_table<-coverage_summary %>%
  gt() %>%
  tab_header(
    title = "Dataset Coverage"
  ) %>%
  cols_label(
    country_name = "Country",
    unique_regions = "Regions",
    years_list = "Years",
    total_distinct_parties = "Count of Total Political Parties"
  )

gtsave(coverage_table,'C:/Users/samtg/github/subnational_inequality/data/output/table.png')

multi_country %>%
  filter(country_name == "Italy") %>%
  tabyl(party_abbreviation)