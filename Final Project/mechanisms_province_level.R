# Mechanisms

# Loading librarries ---------------
library(tidyverse)
library(broom)
library(haven)
library(modelsummary)
library(gt)
library(dplyr)
library('fastDummies')
library(panelView)
library(fixest)
library(glue)
#library(flextable)
#library(scales)

# Loading data ---------------------------------------
subway_clean_prov <- read_dta("data/Subway_clean_prov_use.dta")

# filtering data -------
subway_clean_prov <- subway_clean_prov |>
  filter(Year>=2003 & Year<=2017)


# control variables for Province level -------
base_c <- c("lgdp", "lpop", "lrev", "gdp_gr")  
pps_c <-  c("pps_age", "pps_edu", "pps_gender") 
gov_c <-  c("gov_age", "gov_edu", "gov_gender") 

# Province Level Analysis----------------
# Column one
# xtreg pps_promotion3y pps_plan  i.Year, fe cluster(Provincial_Code)
column_one <-
  feols(
    pps_promotion3y ~ pps_plan | Provincial_Code + Year ,
    data = subway_clean_prov,
    cluster = "Provincial_Code"
  )


# Column two
coltwo_form <-
  glue(
    "pps_promotion3y ~ pps_plan + 
    {str_c(pps_c, collapse = ' + ')} | Provincial_Code + Year"
  )
column_two <- feols(as.formula(coltwo_form), 
                    subway_clean_prov,
                    cluster = "Provincial_Code")

# Column three
colthree_form <-
  glue(
    "pps_promotion3y ~ pps_plan + 
    {str_c(pps_c, collapse = ' + ')} +
    {str_c(base_c, collapse = ' + ')} | 
    Provincial_Code + Year"
  )
column_three <- feols(as.formula(colthree_form), 
                      subway_clean_prov,
                      cluster = "Provincial_Code")

