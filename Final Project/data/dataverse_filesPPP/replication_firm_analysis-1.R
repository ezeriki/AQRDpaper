# Replication files for Li Li Zhang 2023####
# R Program Environment is R 4.2.2; R studio version 2022.12.0+35 ###
# Operation system is Mac OS 12.0.1 ###
rm(list = ls())
library(Hmisc)
library(tidyverse)
library(knitr)
library(lfe)
library(interflex)
library(haven)
library(tidyverse)
library(foreign)
library(interflex)
library(stargazer)
Sys.setlocale("LC_CTYPE", "UTF-8")
# load panel data 
load( "data/ppp_panel.RData")
#load("C:/Users/HP/Desktop/replication_files/ppp_panel.RData")

# set theme as theme_minimal
theme_set(theme_minimal())

# set variables for regression
dv_list = c("invest_l","invest_dummy") # dependent variables
ind_tvar_controls = c("mayor_age","I(mayor_age^2)", "gender_fem.y","ethnicity.y","edu_onjob.y","first_con_mayor") # individual level covariates
pref_controls = c("lgdp_pc","lpopul", "lfiscal") # city social economic controls
full_baseline =c(ind_tvar_controls,pref_controls)

# set fixed effects model###
fe <- function(dv, iv, controls,fe, data) {
  iv_set = c(iv, controls)
  # run a linear model with html arguments for dv and ivs
  iv_list <- paste(iv_set , collapse =" + ")
  regression_for = paste(dv, iv_list, sep=" ~ ")
  regression_for = paste(regression_for, fe, 0, "distCity",sep = " | " )
  print(regression_for)
  felm( as.formula(regression_for) ,data) 
} 

## Figure B.2: Spatial distribution of PPP investment ####
# See in contract-level analysis

## Figure B.4: Tenure of mayors (2010–2017) ####
label = seq(1,12)

mayor = ggplot(ppp, aes(mayor_term)) +
  geom_histogram(binwidth = .5) +
  scale_x_continuous(breaks = label,
                    label = label) +
  xlab("Years in Office") +
  ylab("Number of Observations")
mayor
ggsave("~/Dropbox/Apps/Overleaf/PPP_2021/figures/yr_in_office.pdf",width = 9, height = 6)

##Figure B5: Flexible Estimate ####
full_add_ps = c(full_baseline,"ps_term1")
m3 = fe("invest_l", "factor(mayor_term1)" , full_add_ps ,"distCity+year", ppp  )
summary(m3)

flex <- data.frame(estimate = m3$coefficients[1:9], se = m3$cse[1:9], term = 2:10)
ggplot(flex)  +
  geom_pointrange(aes(y =estimate, ymin=estimate - 1.96 * se, ymax=estimate + 1.96* se ,x = term ))  +
  geom_pointrange(aes(y =estimate, ymin=estimate - 1.65 * se, ymax=estimate + 1.65* se ,x = term ), lwd=1.2, width=1)  +
  scale_x_continuous(breaks = c(2:12)) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  ylab("") +
  xlab("Mayor Tenure")
flex
ggsave("~/Dropbox/Apps/Overleaf/PPP_2021/figures/flex.pdf",width = 9, height = 6)

## Figure B.6: SOEs, local, and local SOE shares ####
# See in contract-level analysis

## Figure B.7: Marginal effect of competition####
library(interflex)
df_vis = ppp %>% 
  select(invest_l,mayor_term,n_city, mayor_age,mayor_age_2, officerID.y,
         distCity,year,lgdp_pc,lpopul,lfiscal,promotion_mayor) %>% 
  filter(!is.na(invest_l),!is.na(mayor_term),!is.na(n_city)) %>% 
  as.data.frame()

full_baseline_inter = c("mayor_age","mayor_age_2", "lgdp_pc","lpopul","lfiscal")

out <- interflex(Y = "invest_l", D = "mayor_term", X = "n_city", Z = full_baseline_inter,
                 data= df_vis,FE = c("distCity","year"),
                 Xlabel =  "Number of Cities",
                 na.rm = TRUE,
                 Dlabel = "Mayor's Tenure",
                 estimator = "binning",
                 Ylabel = "Investment")

plot(out)
ggsave("~/Dropbox/Apps/Overleaf/PPP_2021/figures/marginal_city.png",height = 6, width = 9)


## Table 1 summary statistics####
library(stargazer)
ppp %>% 
  ungroup() %>% 
  select(invest_l, mayor_term, "mayor_age", "gender_fem.y","ethnicity.y",
         "edu_onjob.y", "first_con_mayor", "mayor_local", "lfiscal1", "lexp", "lgdp_pc","lpopul") %>%  
  as.data.frame() %>% 
  stargazer(., type = "text",style = "QJE",
            label = "summary_stat",
            title = "Summary Statistics",
            digits = 2,
            covariate.labels = c("Value of PPP investment (logged)", 
                                 "Tenure", "Age",  "Female", "Ethnicity", "Education", "Patronage connection","Local experience","Revenue",
                                "Expenditure",  "GDP per capita (logged)", "Population (logged)"))


##  Table 2 baseline ####
m1 = fe("invest_l", "mayor_term" , "0" ,"distCity+year", ppp  ) 
m2 = fe("invest_l", "mayor_term" , pref_controls ,"distCity+year", ppp  ) 
m3 = fe("invest_l", "mayor_term" , c(pref_controls,  "mayor_age", "I(mayor_age^2)") ,"distCity+year", ppp  ) 
m4 = fe("invest_l", "mayor_term", full_baseline, "distCity + year", ppp  ) 

stargazer(m1 ,m2,m3,m4,
          type = "text",
          style = "QJE",
          omit.stat = c("ser","rsq"),
          no.space = T,
          title =  "Effect of Mayor's Tenure on PPP Investment",
          label = "baseline",
          covariate.labels = c("Tenure", "GDP per capita", "Population","Revenue",
         "Age", "Age squared","Female",  "Ethnic minority", "Education","Patronage Connections"),
          dep.var.labels = c("Logged Amount of PPP Investment"),
          
          add.lines = list(
                             c("City FE", "Y", "Y","Y","Y","Y", "Y","Y","Y"),
                             c("Year FE", "Y", "Y","Y","Y","Y", "Y","Y","Y"))
          )

rm(m1,m2,m3,m4)


##Table 3 local vs non-local#####
m1 = fe("invest_l", "mayor_term*mayor_local" , "0" ,"distCity+year", ppp ) 
m2 = fe("invest_l", "mayor_term*mayor_local" , full_baseline ,"distCity+year", ppp ) 
m3 = fe("invest_dummy", "mayor_term*mayor_local" , "0" ,"distCity+year", ppp ) 
m4 = fe("invest_dummy", "mayor_term*mayor_local" , full_baseline ,"distCity+year", ppp ) 

stargazer(m1,m2,m3,m4,
          style = "QJE",
          title = "Local vs non-local mayors",
          no.space=TRUE,
          omit.stat = c("ser","rsq"),
          covariate.labels = c("Mayor's tenure","Local experience", "Mayor's tenure*local experience"),
          omit = full_baseline,
          label = "table:local",
          type = "text",
          column.separate = c(2,2),
          column.labels =c("Investment Value", "Investment Dummy" ),
          add.lines = list(c("City FE", "Y", "Y","Y","Y"),
                           c("Year FE", "Y", "Y","Y","Y"),
                           c("Baseline Controls", "Y", "N" ,"Y", "N")))

rm(m1,m2,m3,m4)


##Table 4 contract Level Analysis #####
# See in contract-level analysis

## Table 5: Effect of PPP investment on corruption dismissal ####
m1 = fe("purge_mayor", "invest_l" , full_baseline ,"distCity+year", ppp  ) 
m2 = fe("purge_mayor", "local_size_l", full_baseline, "distCity + year", ppp  ) 
m3 = fe("purge_mayor", "soe_size_l", full_baseline, "distCity + year", ppp  ) 
m4 = fe("purge_mayor", "local_soe_size_l", full_baseline, "distCity + year", ppp  ) 
m5 = fe("purge_mayor", "invest_l*mayor_term " , full_baseline ,"distCity+year", ppp  ) 
m6 = fe("purge_mayor", "local_size_l*mayor_term" , full_baseline ,"distCity+year", ppp  ) 
m7 = fe("purge_mayor", "soe_size_l*mayor_term" , full_baseline ,"distCity+year", ppp  ) 
m8 = fe("purge_mayor", "local_soe_size_l*mayor_term" , full_baseline ,"distCity+year", ppp  ) 

stargazer(m1,m2,m3,m4,m5,m6,m7,m8,
          #type = "text",
          title = "Effect of PPP investment on corruption dismissal",
          label = "table:purge",
          style = "QJE",
          omit.stat = c("ser","rsq"), 
          digits = 4,
          no.space=TRUE,
          omit = c(full_baseline),
          covariate.labels = c("Gross Investment","Local Investment","SOE Investment", "Local SOE Investment",
                               "Term","Gross Investment*Term", "Local Investment*Term",
                               "SOE Investment*Term", 
                               "Local SOE Investment*Term"),
          dep.var.labels = "Dismissal",
          add.lines = list(
            c("Baseline controls", "N","Y","Y","Y","Y","Y","Y","Y"),
            c("City and Year FE", "Y", "Y","Y","Y","Y","Y","Y","Y"))
)

rm(m1,m2,m3,m4,m5,m6,m7,m8)


## Table 6 Effect of PPP investment on promotion ####
m1 = fe("promotion_mayor", "invest_l" , full_baseline ,"distCity+year", ppp  ) 
m2 = fe("promotion_mayor", "local_size_l", full_baseline, "distCity + year", ppp  ) 
m3 = fe("promotion_mayor", "soe_size_l", full_baseline, "distCity + year", ppp  ) 
m4 = fe("promotion_mayor", "local_soe_size_l", full_baseline, "distCity + year", ppp  ) 
m5 = fe("promotion_mayor", "invest_l*mayor_term " , full_baseline ,"distCity+year", ppp  ) 
m6 = fe("promotion_mayor", "local_size_l*mayor_term" , full_baseline ,"distCity+year", ppp  ) 
m7 = fe("promotion_mayor", "soe_size_l*mayor_term" , full_baseline ,"distCity+year", ppp  ) 
m8 = fe("promotion_mayor", "local_soe_size_l*mayor_term" , full_baseline ,"distCity+year", ppp  ) 

stargazer(m1,m2,m3,m4,m5,m6,m7,m8,
          type = "text",
          title = "Effect of PPP investment on promotion",
          label = "table:promotion",
          style = "QJE",
          omit.stat = c("ser","rsq"), 
          digits = 4,
          no.space=TRUE,
          omit = c(full_baseline),
          covariate.labels = c("Gross Investment","Local Investment","SOE Investment", "Local SOE Investment",
                               "Term","Gross Investment*Term", "Local Investment*Term",
                               "SOE Investment*Term",  "Local SOE Investment*Term"),
          dep.var.labels = "Promotion",
          add.lines = list(
            c("Baseline controls", "N","Y","Y","Y","Y","Y","Y","Y"),
            c("City and Year FE", "Y", "Y","Y","Y","Y","Y","Y","Y"))
)

rm(m1,m2,m3,m4,m5,m6,m7,m8)

##Table 7: Indirect channel for promotion incentives ####
m0 = fe( "lfiscal", "invest_l  + rev_exp_ratio",ind_tvar_controls, "distCity + year", ppp  ) 
m1 = fe( "lfiscal", "local_size_l + rev_exp_ratio",ind_tvar_controls, "distCity + year", ppp  )  
m2 = fe( "lfiscal", "soe_size_l + rev_exp_ratio",ind_tvar_controls, "distCity + year", ppp  )  
m3 = fe( "lfiscal", "local_soe_size_l + rev_exp_ratio",ind_tvar_controls, "distCity + year", ppp  )  
m4 = fe( "promotion_mayor", "lfiscal ", ind_tvar_controls, "distCity + year", ppp  ) 

stargazer(m0,m1 ,m2,m3,m4,
          #type = "text",
          style = "QJE",
          omit.stat = c("ser","rsq"),
          omit = ind_tvar_controls,
          no.space = T,
          column.separate = c(4,3),
          column.labels = c("Revenue", "Promotion"),
          title =  "Indirect Channel for Promotion Incentives",
          label = "alter_promotion",
          dep.var.labels = c(""),
          covariate.labels= c("Gross Investment" ,"Local Investment", "SOE Investment","Local SOE Investment",
                              "Revenue-Expenditure Ratio","Revenue"),
          add.lines = list(
            c("City FE", "Y", "Y","Y","Y","Y", "Y","Y","Y"),
            c("Year FE", "Y", "Y","Y","Y","Y", "Y","Y","Y"),
            c("Individual Controls","Y", "Y","Y","Y","Y", "Y","Y","Y"))
)

rm(m0,m1,m2,m3,m4)

# Robustness Checks####=

## Table C1 Fixed Asset Investment####
m1 = fe("log(fixed_invest)", "mayor_term" , "0" ,"distCity+year", ppp  ) 
m2 = fe("log(fixed_invest)", "mayor_term" , pref_controls ,"distCity+year", ppp  ) 
m3 = fe("log(fixed_invest)", "mayor_term" , c(pref_controls, "mayor_age", "I(mayor_age^2)") ,"distCity+year", ppp  ) 
m4 = fe("log(fixed_invest)", "mayor_term", full_baseline, "distCity + year", ppp  ) 

stargazer(m1 ,m2,m3,m4,
          #type = "text",
          style = "QJE",
          omit.stat = c("ser","rsq"),
          no.space = T,
          title =  "Effect of Mayor's Tenure on Fixed Asset Investment",
          label = "baseline",
          covariate.labels = c("Tenure", "GDP per capita", "Population","Revenue",
                               "Age", "Age squared","Female",  "Ethnic minority", "Education","Patronage Connections"),
          dep.var.labels = c("Amount of Fixed Asset Investment"),
          add.lines = list(
            c("City FE", "Y", "Y","Y","Y","Y", "Y","Y","Y"),
            c("Year FE", "Y", "Y","Y","Y","Y", "Y","Y","Y"))
)

rm(m1,m2,m3,m4)

## Table C2 Ability of Mayor####
m1 = fe("invest_l", "mayor_term" , "0" ,"officerID.y+year", ppp  ) 
m2 = fe("invest_l", "mayor_term" , ind_tvar_controls ,"officerID.y+year", ppp  )
m3 = fe("invest_l", "mayor_term" , full_baseline ,"officerID.y+year", ppp  ) 

stargazer(m1 ,m2,m3,
          #type = "text",
          title = "Accounting for Individual Fixed Effects",
          label = "table:ind_fe",
          style = "QJE",
          covariate.labels = c("Mayor's Tenure"),
          omit.stat = c("ser","rsq"),
          no.space = T,
          omit = full_baseline,
          dep.var.labels = c("Investment Value"),
          add.lines = list(
            c("Individual FE", "Y", "Y","Y"),
            c("Year FE", "Y", "Y","Y"),
            c("City Controls", "N", "N","Y"),
            c("Individual Controls", "N", "Y","Y")
       )
)

rm(m1,m2,m3)

## Table C3 Truncated Measure#### 
m1 = fe("invest_l", "mayor_term1" , "0" ,"distCity+year", ppp  ) 
m2 = fe("invest_l", "mayor_term1" , ind_tvar_controls ,"distCity+year", ppp  ) 
m3 = fe("invest_l", "mayor_term1" , pref_controls ,"distCity+year", ppp  ) 
m4 = fe("invest_l", "mayor_term1" , full_baseline ,"distCity+year", ppp  ) 

stargazer(m1 ,m2,m3,m4,
          type = "text",
          style = "QJE",
          omit.stat = c("ser","rsq"),
          no.space = T,
          covariate.labels = c("Mayor's Tenure"),
          omit = c(full_baseline, "ps_term1"),
          dep.var.labels = c("Amount of investment (logged)"),
          add.lines = list(
                            c("Individual controls", "N", "Y","N","Y","N", "Y","N","Y"),
                            c("City controls", "N", "N","Y","Y","N", "N","Y","Y"),
                            c("City FE", "Y", "Y","Y","Y","Y", "Y","Y","Y"),
                            c("Year FE", "Y", "Y","Y","Y","Y", "Y","Y","Y")))
       
rm(m1,m2,m3,m4)

## Table C.4 Party Secretary#### 
sec_tvar_controls = c("sec_age","sec_age_2", "ps_term1") 
full_add_ps = c(full_baseline,sec_tvar_controls) 

m1 = fe("invest_l", "mayor_term + ps_term1"  ,full_baseline,"distCity+year", ppp  ) 
m2 = fe("invest_l", "mayor_term  + sec_age + sec_age_2" , full_baseline ,"distCity+year", ppp  ) 
m3 = fe("invest_l", "mayor_term" ,full_add_ps ,"distCity+year", ppp  )

stargazer(m1 ,m2,m3,
          #type = "text",
          style = "QJE",
          omit.stat = c("ser","rsq"),
          no.space = T,
          omit = c(full_baseline),
          dep.var.labels = c(" Investment(logged)"),
          covariate.labels = c("Mayor's Tenure", "Party secretary's tenure","Party secretary's age", "Party secretary's age,squared"),
          add.lines = list(
                            c("Baseline controls", "Y", "Y","Y"),
                             c("City FE", "Y", "Y","Y"),
                             c("Year FE", "Y", "Y","Y"))
          )

rm(m1,m2,m3)

##Table C.5 Quadratic Relationship table####

m1 = fe("invest_l", "mayor_term + I(mayor_term^2)" , "0" ,"distCity+year", ppp  ) 
m2 = fe("invest_l", "mayor_term +I(mayor_term^2)" , pref_controls  ,"distCity+year", ppp  )
m3 = fe("invest_l", "mayor_term + I(mayor_term^2)" , ind_tvar_controls ,"distCity+year", ppp  ) 
m4 = fe("invest_l", "mayor_term + I(mayor_term^2)" , full_baseline ,"distCity+year", ppp  ) 

stargazer(m1 ,m2,m3,m4,
          #type = "text",
          style = "QJE",
          omit.stat = c("ser","rsq"),
          no.space = T,
          title =  "Quadratic specification",
          label = "table:quadratic",
          dep.var.labels = "Amount of Investment",
          covariate.labels = c("Mayor's tenure", "Mayor's tenure squared","GDP per capita", "Population",
                               "Revenue", "Age", "Age squared","Female",  "Ethnic minority", "Education",
                               "Patronage connections"),
          add.lines = list(
            c("City FE", "Y", "Y","Y","Y","Y", "Y","Y","Y"),
            c("Year FE", "Y", "Y","Y","Y","Y", "Y","Y","Y"))
)

rm(m1,m2,m3,m4)

##Table C.6: Binary Outcome Variable####

m1 = fe("invest_dummy", "mayor_term" , "0" ,"distCity+year", ppp  ) 
m2 = fe("invest_dummy", "mayor_term" , pref_controls ,"distCity+year", ppp  ) 
m3 = fe("invest_dummy", "mayor_term" , c(pref_controls,  "mayor_age", "I(mayor_age^2)") ,"distCity+year", ppp  ) 
m4 = fe("invest_dummy", "mayor_term", full_baseline, "distCity + year", ppp  ) 

stargazer(m1 ,m2,m3,m4,
          #type = "text",
          style = "QJE",
          omit.stat = c("ser","rsq"),
          no.space = T,
          title =  "Effect of Mayor's Tenure on PPP Investment",
          label = "table:binary",
          covariate.labels = c("Mayor's tenure", "GDP per capita", "Population","Revenue",
                               "Age", "Age squared","Female",  "Ethnic minority", "Education", "Patronage connections"),
          dep.var.labels = c("Investment dummy"),
          add.lines = list(
            c("City FE", "Y", "Y","Y","Y","Y", "Y","Y","Y"),
            c("Year FE", "Y", "Y","Y","Y","Y", "Y","Y","Y"))
)

rm(m1,m2,m3,m4)

## table C.7 Vice-Province City A6####
m1 = fe("invest_l", "mayor_term" , full_add_ps ,"distCity+year", ppp  %>% filter(vice_province==0)) 
m2 = fe("invest_l", "mayor_term" , full_add_ps ,"officerID.y+year", ppp  %>% filter(vice_province==0)) 
m3 = fe("invest_dummy", "mayor_term" , full_add_ps ,"distCity+year", ppp  %>% filter(vice_province==0)) 
m4 = fe("invest_dummy", "mayor_term" , full_add_ps ,"officerID.y+year", ppp  %>% filter(vice_province==0)) 

stargazer(m1,m2,m3,m4,
          type = "text",
          style = "QJE",
          omit.stat = c("ser","rsq"), #m3,m4,
          omit = c(full_add_ps),
          no.space = T,
          covariate.labels = c("Mayor's tenure"),
          column.separate = c(2,2),
          column.labels = c("Amount of Investment", "Investment Dummy"),
          dep.var.labels = c(""),
           add.lines = list(c("Baseline and party scretary controls", "Y","Y", "Y", "Y" ),
                            c("City FE", "Y", "N","Y","N"),
                            c("Individual FE", "N", "Y","N","Y"),
                            c("Year FE", "Y", "Y","Y","Y")))


rm(m1,m2,m3,m4)

##Table C.8 Xi administration####
full_add_ps = c(full_baseline,"ps_term1")

m1= fe("invest_l", "mayor_term" , full_add_ps ,"distCity+year", ppp   %>% filter(year >=2012)) 
m2 = fe("invest_dummy", "mayor_term" , full_add_ps ,"distCity+year", ppp   %>% filter(year >=2012)) 

stargazer(m1 ,m2,
          style = "QJE",
         # type = "text",
          omit.stat = c("ser","rsq"),
          no.space = T,
          title = "Post2012 Analysis",
          covariate.labels = c("Mayor's Tenure"),
          omit = c(full_baseline,"ps_term1"),
          column.labels = c("Amount of Investment", "Investment Dummy"),
          add.lines = list(
                            c("Party secretary controls", "Y", "Y"),
                            c("Baseline controls", "Y", "Y","Y","Y","N", "N","Y","Y"),
                             c("City FE", "Y", "Y","Y","Y","Y", "Y","Y","Y"),
                             c("Year FE", "Y", "Y","Y","Y","Y", "Y","Y","Y"))
          )

rm(m1,m2)


## Table C.9 Volume or Average ####
m1 = fe("log(invest_n+1)", "mayor_term" , full_baseline,"distCity+year", ppp  ) 
m2 = fe("log(invest_mean+1)", "mayor_term", full_baseline, "distCity + year", ppp  ) 

stargazer(m1 ,m2,
          #Wtype = "text",
          style = "QJE",
          omit.stat = c("ser","rsq"),
          no.space = T,
          title =  "Effect of Mayor's Tenure on PPP Investment",
          label = "volume_value",
          dep.var.labels =  "",
          covariate.labels = c("Tenure", "GDP per capita", "Population","Revenue",
                               "Age", "Age squared","Female",  "Ethnic minority", "Education","Patronage Connections"),         
          column.labels = c("Number of Contract", "Averge Contract Value"),
          add.lines = list(
            c("City FE", "Y", "Y"),
            c("Year FE", "Y", "Y"))
)

rm(m1,m2)


##Table C.10: Effect of PPP investment on employment####
m1 = fe("log(employment)", "invest_l" , full_baseline ,"distCity+year", ppp  ) 
m2 = fe("log(employment)", "local_size_l" , full_baseline ,"distCity+year", ppp  ) 
m3 = fe("log(employment)", "soe_size_l" , full_baseline ,"distCity+year", ppp  ) 
m4 = fe("log(employment)", "local_soe_size_l" , full_baseline ,"distCity+year", ppp  ) 

stargazer(m1,m2,m3,m4,
          #type = "text",
          title = "Effect of PPP investment on employment",
          label = "table:employment",
          style = "QJE",
          omit.stat = c("ser","rsq"), 
          digits = 4,
          no.space=TRUE,
          omit = c(full_baseline),
          covariate.labels = c("Gross Investment","Local Investment","SOE Investment", "Local SOE Investment"),
          dep.var.labels = "Employment",
          add.lines = list(
            c("Baseline controls", "Y","Y","Y","Y"),
            c("City and Year FE", "Y", "Y","Y","Y"))
)


rm(m1,m2,m3,m4)


##Table C11: Interactive Effect with Purge#####
m1 = fe("invest_l", "purge_n_l1" , "0" ,"distCity+year", ppp  %>% filter(year >=2012)) 
m2 = fe("invest_l", "purge_n_l1" , full_baseline ,"distCity+year",  ppp  %>% filter(year >=2012)) 
m3 = fe("invest_l", "mayor_term*purge_n_l1" , full_baseline ,"distCity+year",  ppp  %>% filter(year >=2012)) 
m4 = fe("invest_dummy", "purge_n_l1" , "0" ,"distCity+year", ppp  %>% filter(year >=2012)) 
m5 = fe("invest_dummy", "purge_n_l1" , full_baseline ,"distCity+year",  ppp  %>% filter(year >=2012)) 
m6 = fe("invest_dummy", "mayor_term*purge_n_l1" , full_baseline ,"distCity+year",  ppp  %>% filter(year >=2012)) 

stargazer(m1,m2,m3,m4,m5,m6,
          style = "QJE",
          #type = "text",
          omit.stat = c("ser","rsq"),
          title = "Marginal Effect of Corruption Crackdowns in 2013--2017",
          label = "corruption",
          no.space=TRUE,
          column.separate = c(3,3),
          dep.var.labels = "",
          column.labels  = c("Amount of Investment", "Investument Dummy"),
          covariate.labels = c("Mayor's tenure", "Purge,t-1", "Mayor's tenure*purge,t-1" ),
          omit = full_baseline,
          add.lines = list(
            c("Controls", "N", "Y" ,"Y", "N","Y", "Y"),
            c("City FE", "Y", "Y","Y","Y","Y","Y"),
            c("Year FE", "Y", "Y","Y","Y","Y","Y")))


rm(m1,m2,m3,m4,m5,m6)

####################################################
##Table 4 Connected vs Non-connected ###
# m1 = fe("invest_l", "mayor_term*first_con_mayor" , "0" ,"distCity+year", ppp ) 
# m2 = fe("invest_l", "mayor_term*first_con_mayor" , full_baseline ,"distCity+year", ppp ) 
# m3 = fe("invest_dummy", "mayor_term*first_con_mayor" , "0" ,"distCity+year", ppp ) 
# m4 = fe("invest_dummy", "mayor_term*first_con_mayor" , full_baseline ,"distCity+year", ppp ) 
# 
# stargazer(m1,m2,m3,m4,
#           style = "QJE",
#           title = "Connected vs non-unconnected mayors",
#           no.space=TRUE,
#           omit.stat = c("ser","rsq"),
#           covariate.labels = c("Mayor's tenure","Patronage connections", "Mayor's tenur*Patronage connections"),
#           keep = c("mayor_term", "first_con_mayor", "mayor_term:first_con_mayor"),
#           label = "table:local",
#           #type = "text",
#           column.separate = c(2,2),
#           column.labels =c("Investment Value", "Investment Dummy" ),
#           add.lines = list(c("City FE", "Y", "Y","Y","Y"),
#                            c("Year FE", "Y", "Y","Y","Y"),
#                            c("Baseline Controls", "Y", "N" ,"Y", "N")))


# full_baseline_inter = c("mayor_age","mayor_age_2", "lgdp_pc","lpopul","lfiscal")
# 
# out <- interflex(Y = "promotion_mayor1", D = "invest_l", X = "mayor_term", Z = full_baseline_inter,
#                  data= df_vis,FE = c("distCity","year"),
#                  Xlabel =  "Number of Cities",
#                  na.rm = TRUE,
#                  Dlabel = "Mayor's Tenure",
#                  estimator = "binning",
#                  Ylabel = "Investment")
# 
# plot(out)


#write.dta(ppp,"/Users/zerenli1992/Nutstore Files/ppp/data/ppp_panel.dta")

# Summary statistics and visualization
## Mayor and Party Secretary's Tenure
# questionr::freq(ppp$mayor_term, cum = TRUE, sort = "dec", total = TRUE) %>% as.data.frame()  %>% dplyr::select("n" ,"%") %>%
#   kable(caption = "Mayor")

# questionr::freq(ppp$ps_term, cum = TRUE, sort = "dec", total = TRUE) %>%  as.data.frame()  %>% dplyr::select("n" ,"%") %>%
#   kable(caption = "Party Secretary",format = "html")
