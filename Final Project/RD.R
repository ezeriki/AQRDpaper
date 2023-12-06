# Loading libraries -----------------
library(tidyverse)
library(haven)
library(fixest)
library(modelsummary)
library(patchwork)
library(rdd)
library(broom)
library(rdrobust)
library(glue)
library(gt)
library(AER)


# Loading data -----------------
subway_analysis_use <- read_dta("data/subway_analysis_use.dta")

# Replication 1 DID ---------------------------------
# First dropping vice province level cities
sub_novice <- subway_analysis_use |>
  filter(fsj2 == 0)

# Coding city code to be character and therefore categorical
sub_novice$City_Code_1 <- as.character(sub_novice$City_Code)

# Fuzzy RD in which running variable is population
# with a cutoff of 3 million people
# Cleaning up population data
# using population, lagged by 2 years
rd_pop <- subway_analysis_use |>
  mutate(pop_centered = (Per_pop_2 - 300)/100,
         iv1 = as.numeric(pop_centered >= 0),
         iv1_int = iv1*pop_centered,
         iv1_int2 = iv1 * pop_centered^2,
         iv1_int3 = iv1 * pop_centered^3,
         iv1_int4 = iv1 * pop_centered^4
         ) 

# RD
#ivreg2 Mayor_promotion3y : dependent variable
#(Mayor_plan =iv1) : endogenous variable with iv1 being dummy instrument variable
# indication whether city has up to or more than 3 milli
# Per_pop_2: other control variable with all the city pops
#iv1_int: only contains city pops with up to over 3 milli
#i.provinceyear: control for political connections, such as who was provincial leader that year
# i.City_Code: contro, for city code
# if abs(Per_pop_2) <= 1.058 : the bandwidth & 
# fsj2 == 0, 
# cluster(City_Code) first

# mayor characteristics contols
# Mayor_age gender2 race6 Mayor_c_edu Mayor_c_central_exp 
#Mayor_c_prov_exp Mayor_c_county_exp Mayor_c_soe_exp 
#Mayor_c_univ_exp Mayor_c_league Mayor_connection_work

mayor_cont3 <- c("Mayor_age", "gender2", "race6", "Mayor_c_edu",
                 "Mayor_c_central_exp", "Mayor_c_prov_exp", "Mayor_c_county_exp",
                 "Mayor_c_soe_exp", "Mayor_c_univ_exp", "Mayor_c_league",
                 "Mayor_connection_work"
)

base_cont <- c("lgdp_per_1", "lrev_per_1", "GRP_growth_1")


# getting bandwidth --------------------

# Fit an RDD model with different bandwidths

rd_bw <- rdbwselect(rd_pop$Mayor_plan, rd_pop$pop_centered, c = 0)
summary(rd_bw)
rd_bw$bws[1]


# filtering out data with observations of interest
rd_pop_filtered <- rd_pop |>
  filter(fsj2 == 0,
         Budget_income_2 > 1000000 & GRP_2 > 10000000)

# REGRESSIONS -------------------
# only city FE and province year FE
column_one <- feols(
  Mayor_promotion3y ~ pop_centered + iv1_int + as.factor(City_Code) + as.factor(provinceyear) |
    Mayor_plan ~ iv1,
  data = rd_pop_filtered |>
    filter(abs(pop_centered) <= rd_bw$bws[1]
    ),
  cluster = "City_Code"
)

column_one_FS <- feols(
  Mayor_plan ~ iv1 + pop_centered + iv1_int | City_Code + provinceyear,
  data = rd_pop_filtered |>
    filter(abs(pop_centered) <= rd_bw$bws[1]
    ),
  cluster = "City_Code"
)

# Alternative method

iv_model <-
  ivreg(
    Mayor_promotion3y ~ Mayor_plan + pop_centered + iv1_int + factor(provinceyear) + factor(City_Code) |
      iv1 + pop_centered + iv1_int + factor(provinceyear) + factor(City_Code),
    data = rd_pop |>
      filter(pop_centered >= -rd_bw$bws[1], pop_centered <= rd_bw$bws[1]),
    cluster = "City_Code_1"
  )


# only city FE and province year FE and mayor charac
coltwo_form <-
  glue(
    "Mayor_promotion3y ~ pop_centered + iv1_int + {str_c(mayor_cont3, collapse = ' + ')} | City_Code + provinceyear |
    Mayor_plan ~ iv1")


column_two <- feols(as.formula(coltwo_form),
  data = rd_pop_filtered |>
    filter(abs(pop_centered) <= rd_bw$bws[1]
    ),
  cluster = "City_Code", type = "HC1"
)

coltwo_form_FS <-
  glue(
    "Mayor_plan ~ iv1 + pop_centered + iv1_int + {str_c(mayor_cont3, collapse = ' + ')} | 
    City_Code + provinceyear"
  )

column_two_FS <- feols(as.formula(coltwo_form_FS),
  data = rd_pop_filtered |>
    filter(abs(pop_centered) <= rd_bw$bws[1]
    ),
  cluster = "City_Code"
)



# only city FE and province year FE and mayor and city charac
colthree_form <-
  glue(
    "Mayor_promotion3y ~ pop_centered + iv1_int + 
    {str_c(mayor_cont3, collapse = ' + ')} + lgdp_per_1 + lrev_per_1 + GRP_growth_1 |
    City_Code + provinceyear |
    Mayor_plan ~ iv1")

column_three <- feols(as.formula(colthree_form),
                    data = rd_pop_filtered |>
                      filter(abs(pop_centered) <= rd_bw$bws[1]
                      ),
                    cluster = "City_Code"
)

coltwo_form_FS <-
  glue(
    "Mayor_plan ~ iv1 + pop_centered + iv1_int + {str_c(mayor_cont3, collapse = ' + ')} + 
    factor(City_Code) + factor(provinceyear)"
  )

colthree_form_FS <-
  glue(
    "Mayor_plan ~ iv1 + pop_centered + iv1_int + lgdp_per_1 + lrev_per_1 + GRP_growth_1 +
    {str_c(mayor_cont3, collapse = ' + ')} | 
    City_Code + provinceyear"
  )


# Mayor_promotion3y (Mayor_plan =iv1) Per_pop_2 iv1_int $mayor_cont3 
# lgdp_per_1 lrev_per_1 GRP_growth_1 i.provinceyear i.City_Code if abs(Per_pop_2) <= 1.058  & fsj2 == 0, cluster(City_Code) first
column_three_FS <- feols(as.formula(colthree_form_FS),
                    data = rd_pop_filtered |>
                      filter(abs(pop_centered) <= rd_bw$bws[1]),
                    cluster = "City_Code"
)


# EXTENSION -----------------
rd_extend <- rd_pop_filtered |>
  filter(abs(pop_centered) <= rd_bw$bws[1])

column_one_ex <- feols(
  Mayor_promotion3y ~ pop_centered + iv1_int + as.factor(City_Code) + as.factor(provinceyear) |
    Mayor_plan ~ iv1,
  data = rd_extend |>
    filter(Mayor_age > 50
    ),
  cluster = "City_Code"
)


column_one_ex1 <- feols(
  Mayor_promotion3y ~ pop_centered + iv1_int + as.factor(City_Code) + as.factor(provinceyear) |
    Mayor_plan ~ iv1,
  data = rd_extend |>
    filter(Mayor_age <= 50
    ),
  cluster = "City_Code"
)

column_one_FS <- feols(
  Mayor_plan ~ iv1 + pop_centered + iv1_int | City_Code + provinceyear,
  data = rd_pop_filtered |>
    filter(abs(pop_centered) <= rd_bw$bws[1] &
             Mayor_age <= 45
    ),
  cluster = "City_Code"
)
