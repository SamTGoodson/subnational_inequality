library(tidyverse)
library(janitor)
library(dineq)
library(purrr)
library(ineq)

#household
h_var_list <- c("hid","year", "dhi", "nhhmem", "hwgt","rural","region_c","own")
names_files_h <- c(
#France
"fr18h","fr17h","fr16h", "fr15h","fr14h","fr13h","fr12h","fr11h","fr10h","fr09h","fr08h","fr07h","fr06h","fr05h",
"fr84h","fr90h","fr96h","fr97h","fr98h","fr99h","fr00h","fr01h","fr02h","fr03h","fr04h",
#Italy
"it20h","it16h","it14h","it10h","it08h","it04h","it00h","it98h","it95h","it93h","it91h","it89h",
#Austria
"at20h","at19h","at18h","at17h","at16h","at15h","at14h","at13h","at12h","at11h","at10h","at09h","at08h","at07h",
"at06h","at05h","at04h","at03h","at00h","at99h","at98h","at97h","at96h","at95h","at94h",
#Belgium
"be17h","be16h","be15h","be14h","be13h","be12h","be11h","be10h","be09h","be08h","be07h",
"be06h","be05h","be04h","be03h","be00h","be97h","be95h","be92h","be88h",
#Germany
"de19h","de18h","de17h","de16h","de15h","de14h","de13h","de12h","de11h","de10h","de09h","de08h","de07h","de06h","de05h","de04h",
"de03h","de02h","de01h","de00h","de99h","de98h","de97h","de96h","de95h","de94h","de93h","de92h","de91h","de90h","de89h",
#Spain
"es19h","es18h","es17h","es16h","es15h","es14h","es13h","es12h","es11h","es10h","es09h","es08h","es07h","es06h","es05h","es04h","es00h",
"es99h","es98h","es97h","es96h","es95h","es94h","es93h","es90h","es85h"
)

files_h <- purrr::map(.x = names_files_h, 
                       ~read.LIS(.x, vars = h_var_list))
names(files_h) <- names_files_h

#personal 
p_var_list<- c("hid",'educ',"educ_c","pid", "pi11", "age", "relation", "pwgt","immigr","inda1",'occ1_c')
names_files_p <- c(
#France
"fr18p","fr17p","fr16p", "fr15p","fr14p","fr13p","fr12p","fr11p","fr10p","fr09p","fr08p","fr07p","fr06p","fr05p",
"fr84p","fr90p","fr96p","fr97p","fr98p","fr99p","fr00p","fr01p","fr02p","fr03p","fr04p",
#Italy
"it20p","it16p","it14p","it10p","it08p","it04p","it00p","it98p","it95p","it93p","it91p","it89p",
#Austria
"at20p","at19p","at18p","at17p","at16p","at15p","at14p","at13p","at12p","at11p","at10p","at09p","at08p","at07p",
"at06p","at05p","at04p","at03p","at00p","at99p","at98p","at97p","at96p","at95p","at94p",
#Belgium
"be17p","be16p","be15p","be14p","be13p","be12p","be11p","be10p","be09p","be08p","be07p",
"be06p","be05p","be04p","be03p","be00p","be97p","be95p","be92p","be88p",
#Germany
"de19p","de18p","de17p","de16p","de15p","de14p","de13p","de12p","de11p","de10p","de09p","de08p","de07p","de06p","de05p","de04p",
"de03p","de02p","de01p","de00p","de99p","de98p","de97p","de96p","de95p","de94p","de93p","de92p","de91p","de90p","de89p",
#Spain
"es19p","es18p","es17p","es16p","es15p","es14p","es13p","es12p","es11p","es10p","es09p","es08p","es07p","es06p","es05p","es04p","es00p",
"es99p","es98p","es97p","es96p","es95p","es94p","es93p","es90p","es85p"
)

files_p <- purrr::map(.x = names_files_p, 
                       ~read.LIS(.x, vars = p_var_list))
names(files_p) <- names_files_p

#merge
merged_files <- purrr::map2(.x = files_p,
                            .y = files_h,
                            .f = ~left_join(.x, .y, by = "hid"))
names(merged_files) <- c(names_files_h)
merged_files <- lapply(merged_files, function(x) {x$educ_c <- as.factor(x$educ_c); x})
merged_files <- lapply(merged_files, function(x) {x$region_c <- as.character(x$region_c); x})

df <- merged_files %>%
  bind_rows(.id = "file")

botline <- 0
topline <- 10 * wNtile(df$dhi, df$hpopwgt, 0.5)
df$dhi <- topBottom(df$dhi, botline, topline)
df$edhi <- df$dhi / df$nhhmem^0.5
df$cdhi <- df$dhi / df$nhhmem

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

#Education
df2 <- df %>%
  group_by(region_c, year,educ) %>%
  summarize(avg_inc = weighted.mean(dhi,hwgt)) %>%
  ungroup()

df2 <- df2 %>%
  pivot_wider(values_from = avg_inc,names_from = educ)%>%
  mutate(ratio = (`[1]low` / `[3]high`)) %>%
  select(region_c,year,ratio)

merged_df <- merge(df1,df2,by = c("region_c","year"))

print(tbl_df(df2), n=502)

