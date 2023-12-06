# AQRD Final Paper

# Loading Packages ---------------------------------------
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

# POSSIBLE EXTENSIONS


# Loading data ---------------------------------------
subway_analysis_use <- read_dta("data/subway_analysis_use.dta")

mean(subway_analysis_use$Mayor_age, na.rm = TRUE)
median(subway_analysis_use$Mayor_age, na.rm = TRUE)

# Replication 1 DID ---------------------------------
# First dropping vice province level cities
# recoding gender to be female for binary
sub_novice <- subway_analysis_use |>
  filter(fsj2 == 0) 


# string formulas I will need
mayor_cont <- c("gender2", "race6", "Mayor_age", "Mayor_c_edu", "Mayor_c_central_exp",
                "Mayor_c_prov_exp", "Mayor_c_county_exp", "Mayor_c_soe_exp", "Mayor_c_univ_exp",
                "Mayor_c_league", "Mayor_connection_work")
mayor_cont2 <- c("gender2", "race6", "Mayor_age", "Mayor_c_edu",
              "Mayor_c_central_exp", "Mayor_c_prov_exp", "Mayor_c_county_exp",
              "Mayor_c_soe_exp", "Mayor_c_univ_exp", "Mayor_c_league")

# city level one year lagged control variables and provice leader 
base_cont <- c("lpop_1", "lgdp_1", "lrev_1", "GRP_growth_1")
PS_cont <- c("PS_age", "PS_gender2", "PS_race8", "PS_connection_work",
             "PS_c_2currentsec2", "PS_c_prov_exp", "PS_c_central_exp", "PS_c_edu",
             "PS_c_soe_exp", "PS_c_univ_exp", "PS_c_league") 

# Running DID
# Column one
# Coding city code to be character and therefore categorical
sub_novice$City_Code_1 <- as.character(sub_novice$City_Code)


# Column one
column_one <-
  feols(
    Mayor_promotion3y ~ Mayor_plan | City_Code + Year ,
    data = sub_novice,
    cluster = "City_Code"
  )


# Column two
coltwo_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + 
    {str_c(mayor_cont, collapse = ' + ')} | City_Code_1 + Year"
  )
column_two <- feols(as.formula(coltwo_form), 
                    sub_novice,
                    cluster = "City_Code_1")

# Column three
colthree_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + 
    {str_c(mayor_cont, collapse = ' + ')} +
    {str_c(base_cont, collapse = ' + ')} | 
    City_Code_1 + Year"
  )
column_three <- feols(as.formula(colthree_form), 
                    sub_novice,
                    cluster = "City_Code_1")

# Column Four
colfour_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + 
    {str_c(mayor_cont, collapse = ' + ')} +
    {str_c(base_cont, collapse = ' + ')} | 
    City_Code_1 + Year:pro_code"
  )
column_four <- feols(as.formula(colfour_form), 
                      sub_novice,
                      cluster = "City_Code_1")


# Parallel Trends Assumption --------------------
#creating my five term lags and 4 leads
#eststo: qui xtreg Mayor_promotion3y mpprior5 mpprior4 mpprior3 
  #mpprior2 mpconn1-mpconn5 mppost1-mppost4 i.Year if fsj2 == 0, fe cluster(City_Code)
#eststo: qui xtreg Mayor_promotion3y mpprior5 mpprior4 mpprior3 
  #mpprior2 mpconn1-mpconn5 mppost1-mppost4 $mayor_cont 
    #$base_cont i.provinceyear if fsj2 == 0, fe cluster(City_Code)

# removing the lead of 1 year before the approval
subway_lag_leads <- subway_analysis_use |>
  arrange(Year) |>
  mutate(
    mayorplanlead_5 = lead(Mayor_plan, 5),
    mayorplanlead_4 = lead(Mayor_plan, 4),
    mayorplanlead_3 = lead(Mayor_plan, 3),
    mayorplanlead_2 = lead(Mayor_plan, 2),
    mayorplanlead_1 = lead(Mayor_plan, 1),
    mayorplanlag_1 = lag(Mayor_plan, 1),
    mayorplanlag_2 = lag(Mayor_plan, 2),
    mayorplanlag_3 = lag(Mayor_plan, 3),
    mayorplanlag_4 = lag(Mayor_plan, 4)
  ) 

# getting the lag and leads column to use in formula
lag_leads <-
  str_subset(colnames(subway_lag_leads), "mayorplan")

# Removing the lead of 1 year before the approval
lag_leads = lag_leads[lag_leads != "mayorplanlead_1"]


# creating Formula 1
# lag_leads_form <-
#   glue(
#     "Mayor_promotion3y ~ Mayor_plan +
#     {str_c(lag_leads, collapse = ' + ')}
#     {str_c(base_cont, collapse = ' + ')} | 
#     City_Code_1 + Year"
#   )


lag_leads_form2 <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + {str_c(lag_leads, collapse = ' + ')} + as.factor(Year)|
    City_Code"
  )


lag_leads_form2 <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + {str_c(lag_leads, collapse = ' + ')} | 
    City_Code + Year"
  )


fit <- feols(
  as.formula(lag_leads_form2),
  data = subway_lag_leads |>
    filter(fsj2 == 0),
  cluster = "City_Code"
)


#plotting
fit |>
  tidy() |>
  filter(str_detect(term, 'plan')) |>
  mutate(
    term = recode_factor(
      term,
      mayorplanlead_5 = -5,
      mayorplanlead_4 = -4,
      mayorplanlead_3 = -3,
      mayorplanlead_2 = -2,
      Mayor_plan = 0,
      mayorplanlag_1 = 1,
      mayorplanlag_2 = 2,
      mayorplanlag_3 = 3,
      mayorplanlag_4 = 4
    )
  ) |>
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 2 * std.error, ymax = estimate + 2 *
                      std.error),
                width = 0) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(y = "Effects of Approval on Mayoral Promotion in 3 years")


# EXTENSION -----------------------------------
# Column one Extension -------------
column_one_ex <-
  feols(
    Mayor_promotion3y ~ Mayor_plan | City_Code + Year ,
    data = sub_novice |>
      filter(Mayor_age > 50),
    cluster = "City_Code"
  )

column_one_ex1 <-
  feols(
    Mayor_promotion3y ~ Mayor_plan | City_Code + Year ,
    data = sub_novice |>
      filter(Mayor_age <= 50),
    cluster = "City_Code"
  )

column_one_ex15 <-
  feols(
    Mayor_promotion3y ~ Mayor_plan + Mayor_age + Mayor_plan:Mayor_age + Mayor_age:Mayor_c_prov_exp |
      City_Code + Year ,
    data = sub_novice |>
      filter(Mayor_age <= 45),
    cluster = "City_Code"
  )


# tenure is promissing
column_one_ex17 <-
  feols(
    Mayor_promotion3y ~ Mayor_plan + Mayor_c_tenure + Mayor_plan:Mayor_c_tenure |
      City_Code + Year ,
    data = sub_novice,
    cluster = "City_Code"
  )

# very promising
column_one_ex18 <-
  feols(
    Mayor_promotion3y ~ Mayor_plan + Mayor_c_tenure + Mayor_plan:Mayor_c_tenure |
      City_Code + Year ,
    data = sub_novice |>
      filter(Mayor_age <= 50),
    cluster = "City_Code"
  )

column_one_ex19 <-
  feols(
    Mayor_promotion3y ~ Mayor_plan + Mayor_c_tenure + Mayor_plan:Mayor_c_tenure |
      City_Code + Year ,
    data = sub_novice |>
      filter(Mayor_age > 50),
    cluster = "City_Code"
  )


column_one_ex20 <-
  feols(
    Mayor_promotion3y ~ Mayor_plan + Mayor_leaderindex + Mayor_plan:Mayor_leaderindex |
      City_Code + Year ,
    data = sub_novice |>
      filter(Mayor_age > 45),
    cluster = "City_Code"
  )


#no female mayor secured an approval
test <- sub_novice |>
  filter(gender_fem == 1)


column_one_ex2 <- feols(
  Mayor_promotion3y ~ Mayor_plan:inv1_per | City_Code + Year ,
  data = sub_novice |>
    filter(gender_fem == 1),
  cluster = "City_Code"
)

sub_novice$inv1_per
column_one_ex3 <- feols(
  Mayor_Finalpromotion ~ gender_fem | City_Code + Year ,
  data = sub_novice,
  cluster = "City_Code"
)

# Column TWO Extension -------------
# age alone
coltwo_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + 
    {str_c(mayor_cont, collapse = ' + ')} | City_Code + Year"
  )
column_twoex1 <- feols(as.formula(coltwo_form), 
                    sub_novice |>
                      filter(Mayor_age > 50),
                    cluster = "City_Code")
column_twoex2 <- feols(as.formula(coltwo_form), 
                    sub_novice |>
                      filter(Mayor_age <= 50),
                    cluster = "City_Code")

# including effect of major tenure
coltwo_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + Mayor_c_tenure + Mayor_plan:Mayor_c_tenure +
    {str_c(mayor_cont, collapse = ' + ')} | City_Code + Year"
  )
column_twoex3 <- feols(as.formula(coltwo_form), 
                       sub_novice |>
                         filter(Mayor_age > 50),
                       cluster = "City_Code")
column_twoex4 <- feols(as.formula(coltwo_form), 
                       sub_novice |>
                         filter(Mayor_age <= 50),
                       cluster = "City_Code")

# including effect of major leader index
coltwo_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + Mayor_leaderindex + Mayor_plan:Mayor_leaderindex +
    {str_c(mayor_cont, collapse = ' + ')} | City_Code + Year"
  )
column_twoex4 <- feols(as.formula(coltwo_form), 
                       sub_novice |>
                         filter(Mayor_age > 45),
                       cluster = "City_Code")
column_twoex5 <- feols(as.formula(coltwo_form), 
                       sub_novice |>
                         filter(Mayor_age <= 45),
                       cluster = "City_Code")

# Column three Extension -------------
# age alone
colthree_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + 
    {str_c(mayor_cont, collapse = ' + ')} +
    {str_c(base_cont, collapse = ' + ')} | City_Code + Year"
  )
column_threeex1 <- feols(as.formula(colthree_form), 
                       sub_novice |>
                         filter(Mayor_age > 50),
                       cluster = "City_Code")
column_threeex2 <- feols(as.formula(colthree_form), 
                       sub_novice |>
                         filter(Mayor_age <= 50),
                       cluster = "City_Code")

# including effect of major tenure
colthree_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + Mayor_c_tenure + Mayor_plan:Mayor_c_tenure +
    {str_c(mayor_cont, collapse = ' + ')} +
    {str_c(base_cont, collapse = ' + ')} | City_Code + Year"
  )
column_threex3 <- feols(as.formula(colthree_form), 
                       sub_novice |>
                         filter(Mayor_age > 50),
                       cluster = "City_Code")
column_threex4 <- feols(as.formula(colthree_form), 
                       sub_novice |>
                         filter(Mayor_age <= 50),
                       cluster = "City_Code")

# including effect of major leader index: probably won't use it bcus don't know what it is
colthree_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + Mayor_leaderindex + Mayor_plan:Mayor_leaderindex +
    {str_c(mayor_cont, collapse = ' + ')} +
    {str_c(base_cont, collapse = ' + ')} | City_Code + Year"
  )
column_threex4 <- feols(as.formula(colthree_form), 
                       sub_novice |>
                         filter(Mayor_age > 45),
                       cluster = "City_Code")
column_threex5 <- feols(as.formula(colthree_form), 
                       sub_novice |>
                         filter(Mayor_age <= 45),
                       cluster = "City_Code")

# Column four Extension -------------
# age alone
colfour_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + 
    {str_c(mayor_cont, collapse = ' + ')} +
    {str_c(base_cont, collapse = ' + ')} | City_Code + Year:pro_code"
  )
column_fourex1 <- feols(as.formula(colfour_form), 
                         sub_novice |>
                           filter(Mayor_age > 50),
                         cluster = "City_Code")
column_fourex2 <- feols(as.formula(colfour_form), 
                         sub_novice |>
                           filter(Mayor_age <= 50),
                         cluster = "City_Code")

# including effect of major tenure
colfour_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + Mayor_c_tenure + Mayor_plan:Mayor_c_tenure +
    {str_c(mayor_cont, collapse = ' + ')} +
    {str_c(base_cont, collapse = ' + ')} | City_Code + Year:pro_code"
  )
column_fourex3 <- feols(as.formula(colfour_form), 
                        sub_novice |>
                          filter(Mayor_age > 50),
                        cluster = "City_Code")
column_fourex4 <- feols(as.formula(colfour_form), 
                        sub_novice |>
                          filter(Mayor_age <= 50),
                        cluster = "City_Code")




