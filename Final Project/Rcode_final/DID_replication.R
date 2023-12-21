# Loading Packages ---------------------------------------
library(tidyverse)
library(broom)
library(haven)
library(modelsummary)
library(gt)
library(dplyr)
#library('fastDummies')
library(panelView)
library(fixest)
library(glue)
#library(flextable)
#library(scales)




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



# Summary Statistics Table-----------------------
myfn <- function(x) {c(N = sum(!is.na(x)), 
                       Mean = mean(x, na.rm = TRUE),
                       Max = max(x, na.rm = TRUE) , 
                       Min = min(x, na.rm = TRUE),
                       std_dev = sd(x, na.rm = TRUE))}

# columns to create summ stat of
rel_columns <- c("Mayor_promotion3y", "Mayor_connection_work", "Mayor_c_tenure",
                 "Mayor_age", "Per_pop", "gdp", "rev", "GRP_growth", 
                 "Mayor_plan", "inv1_per", "GRP_per", "land_per", 
                 "rev_per")
row_names <- c("Mayor promoted within three years",
               "Mayor connection", 
               "Mayor tenure",
               "Mayor age", "City population",
               "City GDP (billion ¥)",
               "Cityfiscal revenue (billion ¥)",
               "City GDP growth rate (%)",
               "Mayor obtaining subway approval",
               "City investment in infrastructure per capita (¥)",
               "City GDP per capita (¥)",
               "City land sales revenue per capita (¥)",
               "City fiscal revenue per capita (¥)")

summ_stat <- subway_analysis_use |>
  summarise_at(rel_columns, myfn) |>
  t() |>
  as_tibble() |>
  rename("N" = V1,
         "Mean" = V2,
         "Max" = V3,
         "Min" = V4,
         "Std.Dev" = V5) |>
  mutate(row_names = row_names, .before = N)


#?opt_table_font
# making into gt table
summ_gt <- summ_stat |>
  gt() |>
  opt_table_font(
    font = "serif",
    weight = "nomal"
  ) |>
  fmt_number(
    columns = c("Mean", "Max", "Min", "Std.Dev"),
    rows = everything(),
    decimals = 2
  ) |>
  cols_label(
    row_names = "Variable"
  ) |>
  tab_options(data_row.padding = px(1),
              table.font.size = 12)

summ_gt |>
  gtsave("template/tables/final_summ_stat.png")

#?table.width



# Running DID--------------------------
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
  ~ term, ~ Model1, ~ Model2, ~ Model3, ~ Model4,
  "City FE", "Yes", "Yes", "Yes", "Yes",
  "Year FE", "Yes", "Yes", "Yes", "Yes",
  "Mayor Controls", "No", "Yes", "Yes", "Yes",
  "City Controls", "No", "No", "Yes", "Yes",
  "Province-year FE", "No", "No", "No", "Yes"
  )

#combining it all to make model summary table
# options("modelsummary_factory_default" = "gt")
# Model Summary Table ------------
ms_notes <- "* p < 0.05, ** p < 0.01, *** p < 0.001
<br>Standard Errors clustered at the city level in parentheses.
<br>Mayor Controls include gender, ethnicity, past political connections, and univeristy experience.
<br>City controls include GDP size and growth, fiscal revenue, and population.
<br>FE: Fixed Effects"

# putting into model summary
model_table <-
  modelsummary(reg_cols,
               output = "gt",
               gof_map = "nobs",
               coef_map = coefficient_names,
               add_rows = summary_row,
               stars = c(
                 '*' = .05,
                 '**' = .01,
                 '***' = .001
               ))

# extracting the data to get into gt format
# class(gt_table)
gt_table <- model_table$`_data`
# moving number of observations to bottom row
gt_table <- gt_table |>
  arrange(replace(row_number(), 3, n() + 1))

# creating checkmarks -------------------------
checkmark <- "<span style=\"color:black\">&check;</span>"
#sadface <- "<span style=\"color:red\">&#128546;</span>"
#<h3>&check;
replication_tbl <- gt_table |>
  gt(rowname_col = "row") |>
  opt_table_font(stack = "old-style") |>
  tab_options(column_labels.font.weight = "bolder",
              table.font.size = 12,
              table.font.weight = "bold",
              data_row.padding = px(1)) |>
  tab_header(
    title = md("Subway Approval and Mayoral Promotion")) |>
  tab_spanner(
    label = "Mayor Promoted within Three Years",
    columns = c(Model1, Model2, Model3, Model4)
  ) |>
  cols_label(
    Model1 = "(1)",
    Model2 = "(2)",
    Model3 = "(3)",
    Model4 = "(4)") |>
  tab_footnote(
    footnote = html(ms_notes)) |>
  text_transform(
    locations = cells_body(
      columns = c("Model1", "Model2",
                  "Model3", "Model4"),
      rows = Model1 == "Yes" &  Model2 == "Yes" & Model3 == "Yes" & Model4 == "Yes"
    ),
    fn = function(x) checkmark
  ) |>
  text_transform(
    locations = cells_body(
      columns = c("Model2", "Model3", "Model4"),
      rows = Model1 == "No" & Model2 == "Yes" & Model3 == "Yes" & Model4 == "Yes"
    ),
    fn = function(x) checkmark) |>
  text_transform(
    locations = cells_body(
      columns = c("Model3", "Model4"),
      rows = Model1 == "No" & Model2 == "No" & Model3 == "Yes" & Model4 == "Yes"
    ),
    fn = function(x) checkmark) |>
  text_transform(
    locations = cells_body(
      columns = c("Model4"),
      rows = Model1 == "No" & Model2 == "No" & Model3 == "No" & Model4 == "Yes"
    ),
    fn = function(x) checkmark) |>
  text_transform(
    locations = cells_body(
      columns = c("Model1"),
      rows = Model1 == "No" 
    ),
    fn = function(x) " ") |>
  text_transform(
    locations = cells_body(
      columns = c("Model2"),
      rows = Model2 == "No" 
    ),
    fn = function(x) " ") |>
  text_transform(
    locations = cells_body(
      columns = c("Model3"),
      rows = Model3 == "No" 
    ),
    fn = function(x) " ") |>
  tab_options(table_body.hlines.color = "transparent")  |>
  tab_style(style = cell_borders(sides = c("bottom"),  weight = px(1)),
            locations = cells_body(rows = c(7)))

# saving table
replication_tbl |>
  gtsave("template/tables/did_replication.png")
# ------------End of GT Table --------------------



# Parallel Trends Assumption --------------------
#creating my five term lags and 4 leads
#eststo: qui xtreg Mayor_promotion3y mpprior5 mpprior4 mpprior3 
#mpprior2 mpconn1-mpconn5 mppost1-mppost4 i.Year if fsj2 == 0, fe cluster(City_Code)
#eststo: qui xtreg Mayor_promotion3y mpprior5 mpprior4 mpprior3 
#mpprior2 mpconn1-mpconn5 mppost1-mppost4 $mayor_cont 
#$base_cont i.provinceyear if fsj2 == 0, fe cluster(City_Code)

# Leads and Lags my way ------------------
subway_lag_leads <- subway_analysis_use |>
  arrange(City_Code) |>
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


# getting lead and lags column names
lag_leads <- subway_lag_leads |>
  select(starts_with("mayorplanlead"), starts_with("mayorplanlag")) |>
  colnames()

# Removing the lead of 1 year before the approval and City_code
lag_leads = lag_leads[- which(lag_leads %in% c("City_Code", "mayorplanlead_1"))]

# formula
lag_leads_form2 <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + {str_c(lag_leads, collapse = ' + ')} | 
    City_Code + Year"
  )

# creating model
fit <- feols(
  as.formula(lag_leads_form2),
  data = subway_lag_leads |>
    filter(fsj2 == 0),
  cluster = "City_Code"
)

# plotting
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
  geom_vline(xintercept = -1, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(y = "Effects of Approval on Mayoral Promotion in 3 years", x = "") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12, family = "serif"),  # Adjust axis text font size and style
    axis.title = element_text(size = 12, family = "serif"),  # Adjust axis title font size and style
    plot.title = element_text(size = 15, family = "serif",
                              face = "bold", hjust = 0.5)
  )

# end of leads and lags my way------------------


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
  theme_classic() +
  theme(
    axis.text = element_text(size = 12, family = "serif"),  # Adjust axis text font size and style
    axis.title = element_text(size = 12, family = "serif"),  # Adjust axis title font size and style
    plot.title = element_text(size = 15, family = "serif",
                              face = "bold", hjust = 0.5)
  )