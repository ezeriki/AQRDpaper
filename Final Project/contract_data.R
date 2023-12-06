# Loading Packages ---------------------------------------
library(tidyverse)
library(broom)
library(haven)
#library(modelsummary)
#library(gt)
library(dplyr)
library('fastDummies')
#library(panelView)
library(fixest)
library(glue)
#library(flextable)
#library(scales)


# Loading data on types of infrastructure from 2010 to 2017
# contract <-
load("~/Yale Fall 23/Quantitative Research Methods/Final Project/dataverse_files/ppp_panel.RData")
load("~/Yale Fall 23/Quantitative Research Methods/Final Project/dataverse_files/ppp_contract.RData")

?arrange
contract_st <- contract |>
  mutate(City_Code = distCity/100) |>
  select("year", "City_Code", starts_with("invest"), starts_with("mayor"), "type",
         "lgdp_pc", "lpopul", "lfiscal") |>
  filter(type == "Science & Technology") |>
  group_by(year, City_Code, type) |>
  arrange(year, City_Code) |>
  summarise(sum_invest = sum(invest),
            sum_invest1 = sum(invest_l),
            sum_gpd = sum(lgdp_pc),
            mean_pop = mean(lpopul),
            sum_fiscal = sum(lfiscal)) 

# filter out subway for only 2014 - 2016
subway_2014 <- subway_analysis_use |>
  filter(Year >= 2014)



# merge contract data with subway data and only filter on sciente and tech
merged_sub <- subway_2014 |>
  left_join(y = contract_st, by = c("City_Code" = "City_Code", 
                                   "Year" = "year")) |>
  select( "City_Code", "Year", "type", "Mayor_promotion3y", 
          starts_with("Mayor"), starts_with("GRP"), starts_with("Budget_income"),
          "provinceyear", "lpop", "lgdp", "lrev", "inv1_per", "fsj2",
          "with_subway", "rev", "gdp") |>
  arrange(City_Code, Year) |>
  mutate(type_binary = ifelse(is.na(type), 0, 1)) |>
  filter(Budget_income_2 > 1000000 & GRP_2 > 10000000) |>
  filter(type == "Science & Technology")
  distinct()

  
  # let's eee
  
  feols(
    Mayor_promotion3y ~ Mayor_plan:type_binary |
      City_Code + Year ,
    data = merged_sub,
    cluster = "City_Code"
  )

ppp <- ppp |>
  select("year", "distCity", starts_with("invest"), starts_with("mayor"))

# saving as dta file to view bettet
require(foreign)
write.dta(contract, "contract.dta")

# Updating my dist city column to resemble subway analysis
contract <- contract |>
  mutate(City_Code = distCity/100)

# filter out subway for only 2014 - 2016
subway_2014 <- subway_analysis_use |>
  filter(Year >= 2014)




# Getting unique lengths of city codes
city_code <- unique(contract$City_Code)
length(city_code)

sub_prov <- unique(subway_analysis_use$City_Code)
length(sub_prov)

# checking which cities are in contract dataset that are not in subway
ans = setdiff(city_code, sub_prov) 
print(ans)
length(ans)

# merging panel and contract to get infrastructure tpes
# making dummy variables for each type of infrastructure
# making city code match subway
# drop data that is after 2016

merged_con <- ppp |>
  left_join(y = contract, by = c("distCity", "year")) |>
  mutate(City_Code = distCity/100,
         type = replace_na(type, "None")) |>
  dummy_cols(select_columns = 'type') |>
  rename(
    "type_CityManagement" = "type_City Management",
    "type_WaterConservancy" = "type_Water Conservancy",
    "type_PublicTransportation" = "type_Public Transportation",
    "type_SocialSecruity" = "type_Social Secruity",
    "type_ScienceTechnology" = "type_Science & Technology"
  ) |>
  filter(year < 2017)

# joinging to subway data
unique(merged_con$type)



merged_sub <- subway_2010 |>
  left_join(y = merged_con, by = c("City_Code" = "City_Code", 
                                "Year" = "year")) |>
  select( "City_Code", "Year", "Mayor_promotion3y", 
          starts_with("mayor"), starts_with("type")) |>
  distinct()




# test run
# mayor characteristics
mayor_cont <- c("gender2", "race6", "Mayor_age", "Mayor_c_edu", "Mayor_c_central_exp",
                "Mayor_c_prov_exp", "Mayor_c_county_exp", "Mayor_c_soe_exp", "Mayor_c_univ_exp",
                "Mayor_c_league", "Mayor_connection_work")

# city level one year lagged control variables
base_cont <- c("lpop_1", "lgdp_1", "lrev_1", "GRP_growth_1")

# Column one
column_one <-
  feols(
    Mayor_promotion3y ~ type_ScienceTechnology | City_Code + year ,
    data = merged_sub,
    cluster = "City_Code"
  )

# dropping if science and technology is na
merged_con <- merged_con |>
  filter(!is.na(`type_Science & Technology`))

print(min(merged_con$invest_dummy))

