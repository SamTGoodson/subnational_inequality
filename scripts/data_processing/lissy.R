library(tidyverse)
library(janitor)
library(dineq)
library(purrr)
library(ineq)
#household
h_var_list <- c("hid","year", "dhi", "nhhmem", "hwgt","rural","region_c","own")
names_files_h <- c("fr18h","fr17h","fr16h", "fr15h","fr14h","fr13h","fr12h","fr11h","fr10h","fr09h","fr08h","fr07h","fr06h","fr05h",
"fr84h","fr90h","fr96h","fr97h","fr98h","fr99h","fr00h","fr01h","fr02h","fr03h","fr04h")
files_h <- purrr::map(.x = names_files_h, 
                       ~read.LIS(.x, vars = h_var_list))
names(files_h) <- names_files_h
#personal 
p_var_list<- c("hid",'educ',"educ_c","pid", "pi11", "age", "relation", "pwgt","immigr","inda1",'occ1_c')
names_files_p <- c("fr18p","fr17p","fr16p", "fr15p","fr14p","fr13p","fr12p","fr11p","fr10p","fr09p","fr08p","fr07p","fr06p","fr05p",
"fr84p","fr90p","fr96p","fr97p","fr98p","fr99p","fr00p","fr01p","fr02p","fr03p","fr04p")
files_p <- purrr::map(.x = names_files_p, 
                       ~read.LIS(.x, vars = p_var_list))
names(files_p) <- names_files_p
#merge
merged_files <- purrr::map2(.x = files_p,
                            .y = files_h,
                            .f = ~left_join(.x, .y, by = "hid"))
names(merged_files) <- c("fr18h","fr17h","fr16h", "fr15h","fr14h","fr13h","fr12h","fr11h","fr10h","fr09h","fr08h","fr07h","fr06h","fr05h")
merged_files <- lapply(merged_files, function(x) {x$educ_c <- as.factor(x$educ_c); x})
merged_files <- lapply(merged_files, function(x) {x$region_c <- as.character(x$region_c); x})

df <- merged_files %>%
  bind_rows(.id = "file")

## Income
df %>%
  group_by(region_c, year) %>%
  summarize(avg_dhi = mean(dhi, na.rm = TRUE),
            avg_pi11 = mean(pi11, na.rm = TRUE))
## Gini
df %>%
  group_by(region_c, year) %>%
  summarize(avg_gini = ineq(pi11, type = "Gini"))
df %>%
  group_by(region_c, year) %>%
  summarize(avg_gini = ineq(dhi, type = "Gini"))

#Immigrants
df1 <- df %>%
  group_by(region_c, year,immigr) %>%
  summarize(avg_inc = weighted.mean(dhi,hwgt)) %>%
  ungroup()

df1 <- df1 %>%
  pivot_wider(values_from = avg_inc,names_from = immigr)%>%
  mutate(ratio = (`[0]not immigrant` / `[1]immigrant`)) 

#Employment Groups
df2 <- df %>%
  group_by(region_c, year,inda1) %>%
  summarize(avg_inc = weighted.mean(dhi,hwgt)) %>%
  ungroup()

#Occupations
df2 <- df %>%
  group_by(region_c, year,educ) %>%
  summarize(avg_inc = weighted.mean(dhi,hwgt)) %>%
  ungroup()

df2 <- df2 %>%
  pivot_wider(values_from = avg_inc,names_from = educ)%>%
  mutate(ratio = (`[1]low` / `[3]high`)) 

print(tbl_df(df2), n=502)