ches_crosswalk<-ches_crosswalk%>%
  select(partyfacts_id, 'party_id' = dataset_party_id.y)

multi_country <- multi_country %>% 
  filter(country_name != "Belgium")
# Join crosswalk to multi_country on partyfacts_id
multi_country <- multi_country %>% 
  left_join(ches_crosswalk, by = c("partyfacts_id" = "partyfacts_id"))

#Join mp to multi_country on year, country, and partyname
multi_country_ches <- multi_country %>%
  left_join(ches, by = c("year" = "year","party_id.y" = "party_id"))

multi_country_ches <- multi_country_ches %>% 
  drop_na(family,interp_avg_gini)

multi_country_ches <- multi_country_ches %>%
  mutate(perc_vote = (partyvote/totalvote)*100)

highest_vote_ches <- multi_country_ches %>%
  group_by(region, year, party_english) %>%
  mutate(avg_perc_vote = mean(perc_vote, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(region, year) %>%
  filter(avg_perc_vote == max(avg_perc_vote)) %>%
  slice_head(n = 1) %>%
  ungroup()
highest_vote_ches <- var_lag(highest_vote_ches,'lrecon')
highest_vote_ches <- var_lag(highest_vote_ches,'redistribution')
highest_vote_ches <- var_lag(highest_vote_ches,'immigrate_policy')

highest_vote_ches$interp_immig_ratio <- highest_vote_ches$interp_immig * 100000
highest_vote_ches$unemployment_ratio <- highest_vote_ches$interp_unemployment * 100

ch_pan <- pdata.frame(highest_vote_ches,index = c('region','year'))

model_1 <- plm(lrecon ~ lrecon_lag + interp_avg_gini + 
                 unemployment_ratio + interp_immig_ratio, 
               data = ch_pan, 
               model = "within")

model_2 <- plm(lrecon ~ lrecon_lag + interp_ed_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = ch_pan, 
               model = "within")
model_3 <- plm(lrecon ~ lrecon_lag + interp_im_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = ch_pan, 
               model = "within")
model_4 <- plm(lrecon ~ lrecon_lag + interp_wage_ratio + 
                 unemployment_ratio + interp_immig_ratio, 
               data = ch_pan, 
               model = "within")  
model_5 <- plm(lrecon ~  interp_avg_gini + interp_ed_ratio + interp_im_ratio + interp_wage_ratio +  
                 unemployment_ratio + interp_immig_ratio,
               data = ch_pan,
               model = "within")              

stargazer(model_1,model_2,model_3,model_4,model_5,type='html',out='data/output/ch_lerecon.html')


model_1 <- plm(redistribution ~ redistribution_lag + interp_avg_gini + 
                 unemployment_ratio + interp_immig_ratio, 
               data = ch_pan, 
               model = "within")

model_2 <- plm(redistribution ~ redistribution_lag + interp_ed_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = ch_pan, 
               model = "within")
model_3 <- plm(redistribution ~ redistribution_lag + interp_im_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = ch_pan, 
               model = "within")
model_4 <- plm(redistribution ~ redistribution_lag + interp_wage_ratio + 
                 unemployment_ratio + interp_immig_ratio, 
               data = ch_pan, 
               model = "within")  
model_5 <- plm(redistribution ~  interp_avg_gini + interp_ed_ratio + interp_im_ratio + interp_wage_ratio +  
                 unemployment_ratio + interp_immig_ratio,
               data = ch_pan,
               model = "within")              

stargazer(model_1,model_2,model_3,model_4,model_5,type='html',out='data/output/ch_redistribution.html')

model_1 <- plm(immigrate_policy ~ immigrate_policy_lag + interp_avg_gini + 
                 unemployment_ratio + interp_immig_ratio, 
               data = ch_pan, 
               model = "within")

model_2 <- plm(immigrate_policy ~ immigrate_policy_lag + interp_ed_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = ch_pan, 
               model = "within")
model_3 <- plm(immigrate_policy ~ immigrate_policy_lag + interp_im_ratio  +
                 unemployment_ratio + interp_immig_ratio, 
               data = ch_pan, 
               model = "within")
model_4 <- plm(immigrate_policy ~ immigrate_policy_lag + interp_wage_ratio + 
                 unemployment_ratio + interp_immig_ratio, 
               data = ch_pan, 
               model = "within")  
model_5 <- plm(immigrate_policy ~  interp_avg_gini + interp_ed_ratio + interp_im_ratio + interp_wage_ratio +  
                 unemployment_ratio + interp_immig_ratio,
               data = ch_pan,
               model = "within")              

stargazer(model_1,model_2,model_3,model_4,model_5,type='html',out='data/output/ch_immigrate_policy.html')


