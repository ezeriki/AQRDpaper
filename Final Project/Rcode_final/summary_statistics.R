library(tidyverse)
library(scales)
library(broom)
library(lme4)
library(rstanarm)
library(gt)
#library(haven)

# Loading data ---------------------------------
subway_analysis_use <- read_dta("data/subway_analysis_use.dta")

# creating summary statistics
myfn <- function(x) {c(N = sum(!is.na(x)), 
                       Mean = mean(x, na.rm = TRUE),
                       Max = max(x, na.rm = TRUE) , 
                       Min = min(x, na.rm = TRUE),
                       std_dev = sd(x, na.rm = TRUE))}

# columns to create summ stat of
rel_columns <- c("Mayor_promotion3y", "Mayor_connection_work",
                 "Mayor_age", "Per_pop", "gdp", "rev", "GRP_growth", 
                 "Mayor_plan", "inv1_per", "GRP_per", "land_per", 
                 "rev_per")
row_names <- c("Mayor promoted within three years",
               "Mayor connection", 
               "Mayor age", "City population",
               "City GDP (billion ¥)",
               "Cityfiscal revenue (billion ¥)",
               "City GDP growth rate (%)",
               "Mayor obtaining subway approval",
               "City investment in infrastructure per capita (¥)",
               "City GDP per capita (¥)",
               "City land sales revenue per capita (¥)",
               "Cityfiscal revenue per capita (¥)")

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

# making into gt table
summ_gt <- summ_stat |>
  gt() |>
  opt_table_font(stack = "old-style") |>
  tab_options(column_labels.font.weight = "bolder",
              table.font.size = 12,
              table.font.weight = "bold",
              data_row.padding = px(1)) |>
  fmt_number(
    columns = c("Mean", "Max", "Min", "Std.Dev"),
    rows = everything(),
    decimals = 2
  ) |>
  cols_label(
    row_names = "Variable"
  ) |>
  tab_caption("Table 1: Summary Statistics") |>
  tab_options(table.font.size = 10) |> 
  gtsave("final_summ_stat.png", expand = 10)

summ_gt |>
  gtsave("final_summ_stat.png")