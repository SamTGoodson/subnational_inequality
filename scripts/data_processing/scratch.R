library(tidyverse)
library(rio)
library(janitor)
library(manifestoR)
library(here)

nuts_national_path<-here('data','raw','national_election','eu_ned_joint_nuts2.csv')
nuts_national<-read.csv(nuts_national_path)
ches<-crosswalk<-read.csv('C:/Users/samtg/github/subnational_inequality/data/raw/crosswalks/ches_pfid_manifesto.csv')
