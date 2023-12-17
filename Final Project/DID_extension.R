# Loading Packages ---------------------------------------
library(tidyverse)
library(broom)
library(haven)
library(modelsummary)
library(gt)
library(dplyr)
# library('fastDummies')
library(panelView)
library(fixest)
library(glue)
library(rlang)
library(ggplot2)
#library(stringr)
#library(extrafont)
library(marginaleffects)  #plot_slopes function

# Loading data ---------------------------------------
subway_analysis_use <- read_dta("data/subway_analysis_use.dta")


# cleaning up age data --------------
subway_analysis_use <- subway_analysis_use |> 
  mutate(age_br = case_when(Mayor_age %in% 31:35 ~ "30-35",
                            Mayor_age %in% 36:40 ~ "35-40",
                            Mayor_age %in% 41:45 ~ "40-45",
                            Mayor_age %in% 46:50 ~ "45-50",
                            Mayor_age %in% 51:55 ~ "50-55",
                            Mayor_age %in% 56:60 ~ "55-60",
                            Mayor_age %in% 61:65 ~ "60-65"))


# First dropping vice province level cities
sub_novice <- subway_analysis_use |>
  filter(fsj2 == 0)

# releveling age brackets so that estimates are compared to 
# age bracket 50 - 55
sub_novice <- sub_novice |>
  mutate(age_br = age_br |>
           fct_relevel("50-55"))


# control variables -------------
mayor_cont <- c("gender2", "race6", "Mayor_age", "Mayor_c_edu", "Mayor_c_central_exp",
                "Mayor_c_prov_exp", "Mayor_c_county_exp", "Mayor_c_soe_exp", "Mayor_c_univ_exp",
                "Mayor_c_league", "Mayor_connection_work")

# city level one year lagged control variables and provice leader 
base_cont <- c("lpop_1", "lgdp_1", "lrev_1", "GRP_growth_1")

# wfact = cut(employee$age, 3, labels=c('Young', 'Medium', 'Aged'))
# table(wfact)

# Column one Extension -------------
# age control
column_onex1 <-
  feols(
    Mayor_promotion3y ~ Mayor_plan + Mayor_plan*age_br | City_Code + Year ,
    data = sub_novice,
    cluster = "City_Code"
  )

summary(column_onex1)

# Tenure and age interaction
column_onex2 <-
  feols(
    Mayor_promotion3y ~ Mayor_plan + Mayor_plan*age_br + Mayor_c_tenure |
      City_Code + Year ,
    data = sub_novice,
    cluster = "City_Code"
  )
#summary(column_onex1)

# Column TWO Extension -------------
# age alone
coltwo_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + Mayor_plan*age_br +
    {str_c(mayor_cont, collapse = ' + ')} | City_Code + Year"
  )
column_twoex1 <- feols(as.formula(coltwo_form), 
                       sub_novice,
                       cluster = "City_Code")

# including effect of  age and major tenure
coltwo_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + Mayor_plan*age_br + Mayor_c_tenure +
    {str_c(mayor_cont, collapse = ' + ')} | City_Code + Year"
  )
column_twoex2 <- feols(as.formula(coltwo_form), 
                       sub_novice,
                       cluster = "City_Code")
#summary(column_twoex1)

# Column three Extension -------------
# age alone
colthree_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + Mayor_plan*age_br +
    {str_c(mayor_cont, collapse = ' + ')} +
    {str_c(base_cont, collapse = ' + ')} | City_Code + Year"
  )
column_threex1 <- feols(as.formula(colthree_form), 
                         sub_novice,
                         cluster = "City_Code")


# effect of age and tenure
colthree_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + Mayor_plan*age_br + Mayor_c_tenure +
    {str_c(mayor_cont, collapse = ' + ')} +
    {str_c(base_cont, collapse = ' + ')} | City_Code + Year"
  )
column_threex2 <- feols(as.formula(colthree_form), 
                        sub_novice,
                        cluster = "City_Code")
#summary(column_threex3)


# Column four Extension -------------
# age alone
colfour_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + Mayor_plan*age_br +
    {str_c(mayor_cont, collapse = ' + ')} +
    {str_c(base_cont, collapse = ' + ')} | 
    City_Code + Year:pro_code"
  )
column_fourex1 <- feols(as.formula(colfour_form), 
                        sub_novice,
                        cluster = "City_Code")
summary(column_fourex1)


# including effect of age and major tenure
colfour_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + Mayor_plan*age_br + Mayor_c_tenure +
    {str_c(mayor_cont, collapse = ' + ')} +
    {str_c(base_cont, collapse = ' + ')} | 
    City_Code + Year:pro_code"
  )
column_fourex2 <- feols(as.formula(colfour_form), 
                        sub_novice,
                        cluster = "City_Code")


# Regression Tables -------------------

# relabeling coefficient names
coefficient_names <- list(
  'Mayor_plan' = 'Subway Approval',
  'Mayor_plan:age_br40-45' = 'Subway Approval + Age bracket (40-45)',
  'Mayor_plan:age_br45-50' = 'Subway Approval + Age bracket (45-50)',
  'Mayor_plan:age_br55-60' = 'Subway Approval + Age bracket (55-60)',
  'Mayor_c_tenure' = 'Mayor Tenure'
)
# Long Format ---------------------

# Model renaming ----------
# age isolated models
reg_cols_age <- list("Model1" = column_one_ex1, 
                 "Model2" = column_twoex1, 
                 "Model3" = column_threeex1, 
                 "Model4" = column_fourex1)
# tenure isolated models
reg_cols_tenure <- list("Model1" = column_one_ex2, 
                     "Model2" = column_twoex2, 
                     "Model3" = column_threex2, 
                     "Model4" = column_fourex2)
# tenure & age models
reg_cols_tenure_age <- list("Model1" = column_one_ex3, 
                     "Model2" = column_twoex3, 
                     "Model3" = column_threex3, 
                     "Model4" = column_fourex3)
# end of model renaming --------------

# a list of lists for all my models
models <- list(
  "A" = reg_cols_age,
  "B" = reg_cols_tenure,
  "C" = reg_cols_tenure_age)

# models[[1]]$Model1$nobs
# models[[1]]$

# function to group my models (stack overflow)
tidy_model <- function(model_list) {
  # tidy estimates
  tidy_model1 <- broom::tidy(model_list[[1]])
  tidy_model2 <- broom::tidy(model_list[[2]])
  tidy_model3 <- broom::tidy(model_list[[3]])
  tidy_model4 <- broom::tidy(model_list[[4]])
  # create a "group" column
  tidy_model1$group <- "Model1"
  tidy_model2$group <- "Model2"
  tidy_model3$group <- "Model3"
  tidy_model4$group <- "Model4"
  ti <- bind_rows(tidy_model1, tidy_model2, 
                  tidy_model3, tidy_model4)
  # output
  out <- list(tidy = ti, glance = data.frame("nobs models" = length(model_list[[1]]$fitted.values)))
  class(out) <- "modelsummary_list"
  return(out)
}

#using lapply to apply this function to the model
# of model list
models <- lapply(models, tidy_model)

# summary row of controls and fixed effects
summary_row <- tribble(
  ~ group, ~ term, ~ Model1, ~ Model2, ~ Model3, ~ Model4,
  " ", "City FE", "Yes", "Yes", "Yes", "Yes",
  " ", "Year FE", "Yes", "Yes", "Yes", "Yes",
  " ", "Mayor Controls", "No", "Yes", "Yes", "Yes",
  " ", "City Controls", "No", "No", "Yes", "Yes",
  " ", "Province-year FE", "No", "No", "No", "Yes"
)

# putting grouped models into model summary
model_table <- modelsummary(models, 
                            group = model + term ~ group, 
                            coef_map = coefficient_names,
                            output = "gt",
                            gof_map = "nobs",
                            add_rows = summary_row,
                            stars = c('*' = .05, 
                                      '**' = .01, 
                                      '***' = .001))
# ------------ End of Long Format -------------------------


# Wide Format ------------------
reg_cols <- list(
  "Model1" = column_onex1,
  "Model2" = column_twoex1,
  "Model3" = column_threex1,
  "Model4" = column_fourex1,
  "Model5" = column_onex2,
  "Model6" = column_twoex2,
  "Model7" = column_threex2,
  "Model8" = column_fourex2
)

# summary rows
summary_row <- tribble(
  ~ term, ~ Model1, ~ Model2, ~ Model3, ~ Model4, 
  ~ Model5, ~ Model6, ~ Model7, ~ Model8,
  "City FE", "Yes", "Yes", "Yes", "Yes",
  "Yes", "Yes", "Yes", "Yes",
  "Year FE", "Yes", "Yes", "Yes", "Yes",
  "Yes", "Yes", "Yes", "Yes",
  "Mayor Controls", " ", "Yes", "Yes", "Yes",
  " ", "Yes", "Yes", "Yes",
  "City Controls", " ", " ", "Yes", "Yes",
  " ", " ", "Yes", "Yes",
  "Province-year FE", " ", " ", " ", "Yes",
  " ", " ", " ", "Yes"
)


model_table <- modelsummary(reg_cols,  
                            coef_map = coefficient_names,
                            output = "gt",
                            gof_map = "nobs",
                            add_rows = summary_row,
                            stars = c('*' = .05, 
                                      '**' = .01, 
                                      '***' = .001))

# Model Summary Table ------------
ms_notes <- "* p < 0.05, ** p < 0.01, *** p < 0.001
<br>Standard Errors clustered at the city level in parentheses.
<br>Mayor Controls include gender, ethnicity, past political connections, and univeristy experience.
<br>City controls include GDP size and growth, fiscal revenue, and population.
<br>FE: Fixed Effects"


# extracting the data to get into gt format
# class(gt_table)
gt_table <- model_table$`_data`
# moving number of observations to bottom row
gt_table <- gt_table |>
  arrange(replace(row_number(), 11, n() + 1))

# making gt table
extension_tbl <- gt_table |>
  gt(rowname_col = "row") |>
  opt_table_font(stack = "old-style") |>
  tab_options(column_labels.font.weight = "bolder",
              table.font.size = 12,
              table.font.weight = "bold",
              data_row.padding = px(2)) |>
  tab_header(
    title = md("Heterogenous Age Effects inclusive of Mayor Tenure Control")) |>
  tab_spanner(
    label = "Mayor Promoted within Three Years",
    columns = c(Model1, Model2, Model3, Model4, 
                Model5, Model6, Model7, Model8)
  ) |>
  tab_spanner(
    label = "Without Tenure Control",
    columns = c(Model1, Model2, Model3, Model4)
  ) |>
  tab_spanner(
    label = "With Tenure Control",
    columns = c(Model5, Model6, Model7, Model8)
  ) |>
  cols_label(
    Model1 = "(1)",
    Model2 = "(2)",
    Model3 = "(3)",
    Model4 = "(4)",
    Model5 = "(5)",
    Model6 = "(6)",
    Model7 = "(7)",
    Model8 = "(8)") |>
  tab_footnote(
    footnote = html(ms_notes)) |>
  tab_options(table_body.hlines.color = "transparent") 

# adding in check marks --------------
checkmark <- "<span style=\"color:black\">&check;</span>"


extension_tbl <- extension_tbl |>
  text_transform(
    locations = cells_body(
      columns = c("Model1"),
      rows = Model1 == "Yes"),
    fn = function(x) checkmark
  ) |>
  text_transform(
    locations = cells_body(
      columns = c("Model2"),
      rows = Model2 == "Yes"),
    fn = function(x) checkmark
  ) |>
  text_transform(
    locations = cells_body(
      columns = c("Model3"),
      rows = Model3 == "Yes"),
    fn = function(x) checkmark
  ) |>
  text_transform(
    locations = cells_body(
      columns = c("Model4"),
      rows = Model4 == "Yes"),
    fn = function(x) checkmark
  ) |> 
  text_transform(
    locations = cells_body(
      columns = c("Model5"),
      rows = Model5 == "Yes"),
    fn = function(x) checkmark
  ) |> 
  text_transform(
    locations = cells_body(
      columns = c("Model6"),
      rows = Model6 == "Yes"),
    fn = function(x) checkmark
  ) |> 
  text_transform(
    locations = cells_body(
      columns = c("Model7"),
      rows = Model7 == "Yes"),
    fn = function(x) checkmark
  ) |>
  text_transform(
    locations = cells_body(
      columns = c("Model8"),
      rows = Model8 == "Yes"),
    fn = function(x) checkmark
  )
    

# saving table
extension_tbl |> 
  gtsave("template/tables/did_extension.png")

# Age distributions ------------------
# Density plots
sub_dist <- sub_novice |>
  select("Mayor_age", "PS_age")

sub_dist_long <- sub_dist |>
  pivot_longer(cols = c("Mayor_age", "PS_age") , names_to = "Age_group") 


# plotting age density distribution
# if you want to fill with color, use fill

ggplot(sub_dist_long, aes(x = value, fill = Age_group)) + 
  geom_density(alpha = 0.5) +
  labs(y = "Density", x = "Age") +
  scale_color_manual(name="",
                     values=c("black","blue"),
                     labels=c("Mayor Age","PPS Age")) +
theme_bw()



# Margins----------------
library(sjPlot)
library(ggplot2)
theme_set(theme_sjplot())
library(sjmisc)
library(rlang)
library(stringr)
library(extrafont)


modelplot(models = reg_cols, 
          coef_map = coefficient_names
)

library(marginaleffects)
library(ggplot2)

#mod <- feols(hp ~ vs * am | carb, data = mtcars)
#Mayor_plan + Mayor_plan*age_br

# extract raw data for plotting
dat <- plot_slopes(column_onex1, 
                   variables = "Mayor_plan", 
                   condition = "age_br")

# plotting the effects
plot_slopes(column_onex1, 
            variables = "Mayor_plan", 
            condition = "age_br")

# OR

plot_predictions(column_onex1, condition = c("age_br", "Mayor_plan"))

# OR
# -----------------
# convert categorical (binary/factor) variable to numeric
# dat$age_br <- as.numeric(as.character(dat$age_br))
# 
# ggplot(dat, aes(x = age_br, y = estimate, ymin = conf.low, ymax = conf.high)) +
#   geom_line() +
#   geom_ribbon(alpha = .2) +
#   theme_minimal() +
#   labs(y = "Slopes")
# 
# 
# ggplot(dat, aes(x = age_br, y = estimate)) +
#   geom_line() 
# ------------------------


# Checking for outliers ------------------
sub_40_45 <-sub_novice |>
  filter(age_br == "40-45")

sub_40_45$Mayor_plan

mean(sub_40_45$Mayor_plan)
mean(sub_40_45$Mayor_promotion3y)
mean(sub_40_45$Mayor_c_tenure, na.rm = TRUE)

sub_45_50 <-sub_novice |>
  filter(age_br == "45-50")
mean(sub_45_50$Mayor_plan)
mean(sub_45_50$Mayor_promotion3y)
mean(sub_45_50$Mayor_c_tenure, na.rm = TRUE)

sub_50_55 <-sub_novice |>
  filter(age_br == "50-55")
mean(sub_50_55$Mayor_plan)
mean(sub_50_55$Mayor_promotion3y)
mean(sub_50_55$Mayor_c_tenure, na.rm = TRUE)

sub_55_60 <-sub_novice |>
  filter(age_br == "55-60")
mean(sub_55_60$Mayor_plan)
mean(sub_55_60$Mayor_promotion3y)
mean(sub_55_60$Mayor_c_tenure, na.rm = TRUE)

sub_30_35 <-sub_novice |>
  filter(age_br == "30-35")
sub_30_35$Mayor_plan
mean(sub_30_35$Mayor_plan)
mean(sub_30_35$Mayor_promotion3y)
mean(sub_30_35$Mayor_c_tenure, na.rm = TRUE)

sub_35_40 <-sub_novice |>
  filter(age_br == "35-40")
sub_35_40$Mayor_plan
mean(sub_35_40$Mayor_plan)
mean(sub_35_40$Mayor_promotion3y)
mean(sub_35_40$Mayor_c_tenure, na.rm = TRUE)



