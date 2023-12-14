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


# Replication 1 DID ---------------------------------
# First dropping vice province level cities
# recoding gender to be female for binary
sub_novice <- subway_analysis_use |>
  filter(fsj2 == 0) 


# control variables -------------
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
    {str_c(mayor_cont, collapse = ' + ')} | City_Code + Year"
  )
column_two <- feols(as.formula(coltwo_form), 
                    sub_novice,
                    cluster = "City_Code")

# Column three
colthree_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + 
    {str_c(mayor_cont, collapse = ' + ')} +
    {str_c(base_cont, collapse = ' + ')} | 
    City_Code + Year"
  )
column_three <- feols(as.formula(colthree_form), 
                      sub_novice,
                      cluster = "City_Code")

# Column Four
colfour_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + 
    {str_c(mayor_cont, collapse = ' + ')} +
    {str_c(base_cont, collapse = ' + ')} | 
    City_Code + Year:pro_code"
  )
column_four <- feols(as.formula(colfour_form), 
                     sub_novice,
                     cluster = "City_Code")


# Regression Table---------------------
# models
reg_cols <- list("Model1" = column_one, 
                 "Model2" = column_two, 
                 "Model3" = column_three, 
                 "Model4" = column_four)

#variable renaming
coefficient_names <- list(
  'Mayor_plan' = 'Subway Approval'
)

summary_row <- tribble(
  ~ term,
  ~ Model1,
  ~ Model2,
  ~ Model3,
  ~ Model4,
  "City FE",
  " ",
  " ",
  " ",
  " ",
  "Year FE",
  " ",
  " ",
  " ",
  " ")

#combining it all to make model summary table
# options("modelsummary_factory_default" = "gt")
model_table <-
  modelsummary(reg_cols,
               output = "gt",
               gof_map = "nobs",
               coef_map = coefficient_names,
               add_rows = summary_row)

# extracting the data to get into gt format
gt_table <- model_table$`_data`

# creating checkmarks
checkmark <- "<span style=\"color:black\">&check;</span>"
#sadface <- "<span style=\"color:red\">&#128546;</span>"
#<h3>&check;
gt_table |>
  gt(rowname_col = "row") |>
  text_transform(
    locations = cells_body(
      columns = c("Model1", "Model2",
                  "Model3", "Model4"),
      rows = Model1 == " "
    ),
    fn = function(x) paste(x, checkmark)
  )


gt_table |>
  gt() |>
  tab_spanner(
    label = ""
  )

?rows_add

#typeof(model_table)
gt_table |>
  gt() |>
  tab_header(
    title = "S&P 500")
  rows_add(
    "Subway Approval" = "City FE",
    "(1)" = "D",
    "(2)" = "Z",
    "(3)" = "",
    "(4)" = ""
  )


# Parallel Trends Assumption --------------------
#creating my five term lags and 4 leads
#eststo: qui xtreg Mayor_promotion3y mpprior5 mpprior4 mpprior3 
#mpprior2 mpconn1-mpconn5 mppost1-mppost4 i.Year if fsj2 == 0, fe cluster(City_Code)
#eststo: qui xtreg Mayor_promotion3y mpprior5 mpprior4 mpprior3 
#mpprior2 mpconn1-mpconn5 mppost1-mppost4 $mayor_cont 
#$base_cont i.provinceyear if fsj2 == 0, fe cluster(City_Code)


# Creating Leads and Lags from paper -----------
subway_lag_leads <- subway_analysis_use |>
  group_by(City_Code) |>
  mutate(
    mpprior1 = (Mayor_plan == 0 & lead(Mayor_plan, 1) == 1),
    mpprior2 = (
      Mayor_plan == 0 &
        lead(Mayor_plan, 1) == 0 &
        lead(Mayor_plan, 2) == 1
    ),
    mpprior3 = (
      Mayor_plan == 0 &
        lead(Mayor_plan, 1) == 0 &
        lead(Mayor_plan, 2) == 0 &
        lead(Mayor_plan, 3) == 1
    ),
    mpprior4 = (
      Mayor_plan == 0 &
        lead(Mayor_plan, 1) == 0 &
        lead(Mayor_plan, 2) == 0 &
        lead(Mayor_plan, 3) == 0 &
        lead(Mayor_plan, 4) == 1
    ),
    mpprior5 = (
      Mayor_plan == 0 &
        lead(Mayor_plan, 1) == 0 &
        lead(Mayor_plan, 2) == 0 &
        lead(Mayor_plan, 3) == 0 &
        lead(Mayor_plan, 4) == 0 &
        lead(Mayor_plan, 5) == 1
    ),
    mpconn1 = (Mayor_plan == 1 & lag(Mayor_plan, 1) == 0),
    mpconn2 = (
      Mayor_plan == 1 &
        lag(Mayor_plan, 1) == 1 &
        lag(Mayor_plan, 2) == 0
    ),
    mpconn3 = (
      Mayor_plan == 1 &
        lag(Mayor_plan, 1) == 1 &
        lag(Mayor_plan, 2) == 1 &
        lag(Mayor_plan, 3) == 0
    ),
    mpconn4 = (
      Mayor_plan == 1 &
        lag(Mayor_plan, 1) == 1 &
        lag(Mayor_plan, 2) == 1 &
        lag(Mayor_plan, 3) == 1 &
        lag(Mayor_plan, 4) == 0
    ),
    mpconn5 = (Mayor_plan == 1 &
                 mpconn1 == 0 &
                 mpconn2 == 0 & mpconn3 == 0 & mpconn4 == 0)
    
  )


# getting the lag and leads column to use in formula
lag_leads <- subway_lag_leads |>
  select(starts_with("mpprior"), starts_with("mpconn")) |>
  colnames()

# Removing the lead of 1 year before the approval and City_code
lag_leads = lag_leads[- which(lag_leads %in% c("City_Code", "mpprior1"))]


# xtreg Mayor_promotion3y mpprior5 mpprior4 mpprior3 
#mpprior2 mpconn1-mpconn5 i.Year if fsj2 == 0, fe cluster(City_Code)

lag_leads_form <-
  glue(
    "Mayor_promotion3y ~ {str_c(lag_leads, collapse = ' + ')} | 
    City_Code + Year"
  )


fit <- feols(
  as.formula(lag_leads_form),
  data = subway_lag_leads |>
    filter(fsj2 == 0),
  cluster = "City_Code"
)


#plotting
fit |>
  tidy() |>
  mutate(
    term = recode_factor(
      term,
      mpprior5TRUE = -5,
      mpprior4TRUE = -4,
      mpprior3TRUE = -3,
      mpprior2TRUE = -2,
      mpconn1TRUE = 0,
      mpconn2TRUE = 1,
      mpconn3TRUE = 2,
      mpconn4TRUE = 3,
      mpconn5TRUE = 4
    )
  ) |>
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 2 * std.error, ymax = estimate + 2 *
                      std.error),
                width = 0) +
  geom_vline(xintercept = -1, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(y = "Effects of Approval on Mayoral Promotion in 3 years", x = "") +
  theme_classic() 