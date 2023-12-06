rm(list = ls())
# load packages
library(Hmisc)
library(tidyverse)
library(knitr)
library(lfe)
library(interflex)
theme_set(theme_minimal())
library(haven)
library(tidyverse)
library(stargazer)
library(ggpubr)
library(sf)
library(viridis)
# load contract data
load("/Users/zerenli1992/Dropbox/ppp/data/ppp_contract.RData")

###set variables###
dv_list = c("invest_l","invest_dummy") # dependent variables
#exp_var_list = c("mayor_age_d") 
ind_tvar_controls = c("mayor_age","I(mayor_age^2)", "gender_fem.y","ethnicity.y","edu_onjob.y","first_con_mayor") # individual level covariates
#sec_tvar_controls = c("sec_age","sec_age_2", "ps_term1") 
pref_controls = c("lgdp_pc","lpopul", "lfiscal") # city social economic controls
#full_sec =c(sec_tvar_controls,pref_controls) # all controls
full_baseline =c(ind_tvar_controls,pref_controls)

# control variable set
#full_add_ps = c(ind_tvar_controls,pref_controls,sec_tvar_controls) 
###set fixed effects model###
#这个是使用一个叫felm的model来估计two-way fixed effect model
# 这里写了一个小的function
fe <- function(dv, iv, controls,fe, data) {
  iv_set = c(iv, controls)
  # run a linear model with html arguments for dv and ivs
  iv_list <- paste(iv_set , collapse =" + ")
  regression_for = paste(dv, iv_list, sep=" ~ ")
  regression_for = paste(regression_for, fe, 0, "distCity",sep = " | " )
  print(regression_for)
  felm( as.formula(regression_for) ,data) 
} 

## Figure 2 Time trend and type of PPP Investment ####
cate = contract %>% 
  filter(!is.na(distCity)) %>%
  group_by(type) %>% 
  tally() %>% 
  arrange(desc(n)) 

level_cate = as.vector( cate$type )
cate$type <- ordered(cate$type, levels = level_cate)

# types of PPP contracts
cate_fig = ggplot(cate, aes(x = type , y = n, fill= type)) +
  geom_bar(stat = "identity", width=0.5)  +
  coord_flip() +
  theme_minimal(15) +
  xlab("number of projects") +
  theme(legend.position = "none")


# Time Trend
t_trend = contract %>% group_by(year) %>%
  summarise(total = sum(invest)/10^9) %>% 
  #tally()  %>% 
  filter(year <2018) %>% 
  mutate(rate = (total - lag(total))/lag(total) )


t_trend_figure = ggplot(t_trend,aes(x = year, y = total)) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  theme_minimal(15) +
  ylab("Size of PPP Investment (Billion RMB)")


ggpubr::ggarrange(t_trend_figure,cate_fig)
ggsave("~/Dropbox/Apps/Overleaf/PPP_2021/figures/time_trend.png",width = 14, height = 6)


## Figure B2 Spatial distribution of PPP investment####
theme_map <- function(...) {
  theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      ...
    )}

path = "./replication_files/dijishi_2004.shp"
pref_map = contract %>% 
  group_by(distCity) %>% 
  summarise(invest = sum(invest,na.rm = T)/10^9) %>% # unit is 10,000 yuan
  ungroup() %>% 
  mutate(distCity = as.numeric(distCity))

china_map <- st_read(path,options = "ENCODING=GBK") %>% 
  #read_rds("~/Dropbox/ppp/data/gadm36_CHN_2_sf.rds") %>% 
  st_sf() %>%
  left_join(pref_map, by = c("AD2004" = "distCity")) %>% 
  filter(!str_detect(str_sub(AD2004,1,1), "9|8|7")   )

# break
my_breaks = c(1,5,10,20,30)
ggplot() +
  geom_sf(data= china_map, aes(fill = invest) , alpha = .8, color = NA) +
  ylim(17, NA) +
  theme_map()+
  scale_fill_viridis(option = "magma",
                     trans ="log",
                     alpha = 0.8,
                     breaks = my_breaks,
                     label = my_breaks,
                     direction = -1,
                     guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ,
                     name = "PPP Investment (Billion RMB)")

ggsave( "~/Dropbox/Apps/Overleaf/PPP_2021/figures/map.png", width = 5.5,height = 4)

# Figure B.6 SOEs and local shares in PPP projects
contract1 = contract %>%
  filter(!is.na(soe_share) | is.na(local_share)) 

f1  =ggplot(contract1) +
  geom_histogram(aes(x = soe_share), color = "black", alpha =.7) +
  theme_classic() +
  xlab("SOE Share %" ) +
  ylab("Number of Contracts")


f2  =ggplot(contract1) +
  geom_histogram(aes(x = local_share), color = "black", alpha =.7) +
  theme_classic() +
  xlab("Local Firm Share %" ) +
  ylab("Number of Contracts")

f3  =ggplot(contract1) +
  geom_histogram(aes(x = local_soe_share), color = "black", alpha =.7) +
  theme_classic() +
  xlab("Local SOE Firm Share %" ) +
  ylab("Number of Contracts")

ggarrange(f1,f2,f3,ncol = 3)
ggsave("~/Dropbox/Apps/Overleaf/PPP_2021/figures/firm_share_contract.png",width = 9, height = 3)

# Table 4: Contract-level analysis ####
m1 = fe("invest_l" , "local_size_l",full_baseline ,"distCity+ year", contract) 
m2 = fe("invest_l" , "soe_size_l",  full_baseline ,"distCity+ year", contract) 
m3 = fe("invest_l" ,"local_size_l*soe_size_l" ,  full_baseline  ,"distCity+ year", contract)
m4 = fe("invest_l" ,"local_soe_size_l" ,  full_baseline  ,"distCity+ year", contract)
m5 = fe("invest_l" ,"local_nonsoe_size_l" ,  full_baseline  ,"distCity+ year", contract)

spec = c( "Local investment", "SOE investment",
         "Local SOE investment","Local Non SOE investment","Local investment*SOE investment" ) 

stargazer(m1,m2,m3,m4,m5,
          type = "text",
          style = "QJE",
          title = "Contract-level Analysis",
          no.space=TRUE,
          omit.stat = c("ser","rsq"),
          covariate.labels = spec,
          omit = c(full_baseline,"mayor_term"),
          label = "table:contract",
          dep.var.labels =c( "Contract Value"),
          add.lines = list(c("City FE", "Y", "Y","Y","Y","Y","Y"),
                           c("Year FE", "Y", "Y","Y","Y","Y","Y"),
                           c("Controls", "Y", "Y" ,"Y", "Y","Y","Y")))

