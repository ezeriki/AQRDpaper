# Loading Packages ---------------------------------------
library(tidyverse)
library(broom)
library(haven)
library(modelsummary)
library(gt)
library(dplyr)
library(panelView)
library(fixest)
library(glue)
#library(rlang)
library(ggplot2)
library(marginaleffects) # plot slopes
library(gridExtra) # plotting on one figure
library(grid) # also plotting on one figure
library(cowplot) # also for plotting on one grid

# Loading data ---------------------------------------
subway_analysis_use <- read_dta("data/subway_analysis_use.dta")


# First dropping vice province level cities
sub_novice <- subway_analysis_use |>
  filter(fsj2 == 0)


# control variables -------------
mayor_cont <- c("gender2", "race6", "Mayor_age", "Mayor_c_edu", "Mayor_c_central_exp",
                "Mayor_c_prov_exp", "Mayor_c_county_exp", "Mayor_c_soe_exp", "Mayor_c_univ_exp",
                "Mayor_c_league", "Mayor_connection_work")

# city level one year lagged control variables and provice leader 
base_cont <- c("lpop_1", "lgdp_1", "lrev_1", "GRP_growth_1")


# Column one Extension -------------
# tenure mayor plan interaction
column_onex1 <-
  feols(
    Mayor_promotion3y ~ Mayor_plan + Mayor_plan*Mayor_c_tenure | City_Code + Year ,
    data = sub_novice,
    cluster = "City_Code"
  )

#summary(column_onex1)

# Column TWO Extension -------------
# age alone
coltwo_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + Mayor_plan*Mayor_c_tenure +
    {str_c(mayor_cont, collapse = ' + ')} | City_Code + Year"
  )
column_twoex1 <- feols(as.formula(coltwo_form), 
                       sub_novice,
                       cluster = "City_Code")

# summary(column_twoex1)


# Column three Extension -------------
# age alone
colthree_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + Mayor_plan*Mayor_c_tenure +
    {str_c(mayor_cont, collapse = ' + ')} +
    {str_c(base_cont, collapse = ' + ')} | City_Code + Year"
  )
column_threex1 <- feols(as.formula(colthree_form), 
                        sub_novice,
                        cluster = "City_Code")
#summary(column_threex1)

# Column four Extension -------------
# age alone
colfour_form <-
  glue(
    "Mayor_promotion3y ~ Mayor_plan + Mayor_plan*Mayor_c_tenure +
    {str_c(mayor_cont, collapse = ' + ')} +
    {str_c(base_cont, collapse = ' + ')} | 
    City_Code + Year:pro_code"
  )
column_fourex1 <- feols(as.formula(colfour_form), 
                        sub_novice,
                        cluster = "City_Code")
#summary(column_fourex1)


# Regression tables ----
# Regression Table---------------------
# models
reg_cols <- list("Model1" = column_onex1, 
                 "Model2" = column_twoex1, 
                 "Model3" = column_threex1, 
                 "Model4" = column_fourex1)

#variable renaming
coefficient_names <- list(
  'Mayor_plan' = 'Subway Approval',
  'Mayor_c_tenure' = 'Mayor Tenure',
  'Mayor_plan:Mayor_c_tenure' = 'Subway Approval * Mayor Tenure'
)

summary_row <- tribble(
  ~ term, ~ Model1, ~ Model2, ~ Model3, ~ Model4,
  "City FE", "Yes", "Yes", "Yes", "Yes",
  "Year FE", "Yes", "Yes", "Yes", "Yes",
  "Mayor Controls", " ", "Yes", "Yes", "Yes",
  "City Controls", " ", " ", "Yes", "Yes",
  "Province-year FE", " ", " ", " ", "Yes"
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
  arrange(replace(row_number(), 7, n() + 1))

# creating checkmarks -------------------------
checkmark <- "<span style=\"color:black\">&check;</span>"
#sadface <- "<span style=\"color:red\">&#128546;</span>"
#<h3>&check;
extension_tenure <- gt_table |>
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
  tab_options(table_body.hlines.color = "transparent") 

# save gttable
extension_tenure |> 
  gtsave("template/tables/extension_tenure.png")

# end of regression table ------------


# first iteration ----------
# model 1 plot
model1 <- plot_slopes(column_onex1,
                      variables = "Mayor_plan",
                      condition = "Mayor_plan:Mayor_c_tenure") +
  xlab("Interaction between Mayor plan and tenure") +
  ylab("Mayor Promotion in 3 years") +
  theme(axis.title = element_text(size = 10, face = 3))

#model 2 plot
model2 <- plot_slopes(column_twoex1,
                      variables = "Mayor_plan",
                      condition = "Mayor_c_tenure") +
  xlab("Interaction between Mayor plan and tenure") +
  ylab("Mayor Promotion in 3 years") +
  theme(axis.title = element_text(size = 10, face = 3))

#model 3 plot
model3 <- plot_slopes(column_threex1,
                      variables = "Mayor_plan",
                      condition = "Mayor_c_tenure") +
  xlab("Interaction between Mayor plan and tenure") +
  ylab("Mayor Promotion in 3 years") +
  theme(axis.title = element_text(size = 10, face = 3))

# #model 4 plot, not plotting
# model4 <- plot_slopes(column_fourex1,
#                       variables = "Mayor_plan",
#                       condition = "Mayor_c_tenure") +
#   xlab("Interaction between Mayor plan and tenure") +
#   ylab("Mayor Promotion in 3 years") +
#   theme(axis.title = element_text(size = 10, face = 3))


# Plotting in oen figure
# Arrange the plots in a 3x1 grid
grid.arrange(model1, model2, model3, ncol = 1)
# ----------- end of first iteration----------

# Plotting marginal effects
# model 1 plot
model1 <- plot_slopes(column_onex1,
                      variables = "Mayor_plan",
                      condition = "Mayor_c_tenure") +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())


#model 2 plot
model2 <- plot_slopes(column_twoex1,
                      variables = "Mayor_plan",
                      condition = "Mayor_c_tenure") +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

?plot_slopes
#model 3 plot
model3 <- plot_slopes(column_threex1,
                      variables = "Mayor_plan",
                      condition = "Mayor_c_tenure") +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())


# model4 <- plot_slopes(column_fourex1,
#                       variables = "Mayor_plan",
#                       condition = "Mayor_c_tenure") +
#   theme_cowplot() +
#   theme(axis.title.y = element_blank(),
#         axis.title.x = element_blank())

# ---------------- old grid arrange-------------
#?theme
#?plot_slopes
#?scale_y_continuous
# #gp = gpar(fontfamily = "HersheySerif" , fontsize = 9)
# grid.arrange(arrangeGrob(model1 + theme(legend.position="none"), 
#                          model2 + theme(legend.position="none"),
#                          model3 + theme(legend.position="none"), 
#                          ncol = 1,
#                          top = textGrob("Marginal Effects Based on Mayor Tenure", 
#                                         vjust = 1, gp = gpar(fontfamily = "serif", cex = 1)),
#                          left = textGrob("Mayor Promoted in 3 years", 
#                                          rot = 90, vjust = 1,
#                                          gp = gpar(fontfamily = "serif")),
#                          bottom = textGrob("Interaction between Subway Approval and Mayor Tenure", 
#                                            vjust = 1,
#                                            gp = gpar(fontfamily = "serif"))))
# 


#?textGrob
#?gpar
# ---------------- end of old grid arrange-------------


# LAbels and titles
main_title = textGrob("Marginal Effects Based on Mayor Tenure",
                      gp = gpar(fontfamily = "serif", 
                                fontface = "bold",
                                cex = 1))
y.grob <- textGrob("Effect of Approval on Mayor's future Promotion", 
                   gp=gpar(fontfamily = "serif", fontsize=13), rot=90)
x.grob <- textGrob("Mayor Tenure", 
                   gp=gpar(fontfamily="serif", fontsize=13))

# using cowplot plot grid
plot_tenure <- plot_grid(
  model1, model2, model3,
  labels = c('A', 'B', 'C'),
  align="v",
  label_size = 12,
  label_fontfamily = "serif",
  label_x = -0.025,
  label_y = 1.0325
)

#add to plot to label x and y axes
grid.arrange(arrangeGrob(plot_tenure, top = main_title, 
                         left = y.grob, bottom = x.grob))


# Tenure Histogram chart --------------
ggplot(data = sub_novice, aes(x = Mayor_c_tenure)) +
  geom_histogram(binwidth = 1, fill = "blue", 
                 color = "black", alpha = 0.7) +
  labs(x = "Years of Tenure", y = "Frequency",
       title = "Distribution of Mayor's Tenure") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12, family = "serif"),  # Adjust axis text font size and style
    axis.title = element_text(size = 12, family = "serif"),  # Adjust axis title font size and style
    plot.title = element_text(size = 15, family = "serif",
                              face = "bold", hjust = 0.5)  # Adjust plot title font size, style, and center it
  )


#using egg
# install.packages("egg")
# library(egg)
# 
# 
# figure <- egg::ggarrange(model1, model2, 
#                          model3, 
#                          nrow = 2,
#                          labels=c("A", "B", "C"),
#                          label.args = list(gp=gpar(font=2), x=unit(.5,"line")))
# figure

# Outliers -------------
sub_novice_6 <- sub_novice |>
  filter(tenure_group == "6")


sub_novice_6$Mayor_plan
mean(sub_novice_6$Mayor_plan)

------
sub_novice_5 <- sub_novice |>
  filter(tenure_group == "5")


sub_novice_5$Mayor_plan
mean(sub_novice_5$Mayor_plan)

#----------
sub_novice_7 <- sub_novice |>
  filter(tenure_group == "7")


sub_novice_7$Mayor_plan
mean(sub_novice_7$Mayor_plan)

#-----------
sub_novice_3 <- sub_novice |>
  filter(tenure_group == "3")


sub_novice_3$Mayor_plan
mean(sub_novice_3$Mayor_plan)

#-----------
sub_novice_2 <- sub_novice |>
  filter(tenure_group == "2")


sub_novice_2$Mayor_plan
mean(sub_novice_2$Mayor_plan)


#---------------
sub_novice_1 <- sub_novice |>
  filter(tenure_group == "1")


sub_novice_1$Mayor_plan
mean(sub_novice_1$Mayor_plan)


#---------------
sub_novice_0 <- sub_novice |>
  filter(tenure_group == "0")


sub_novice_0$Mayor_plan
mean(sub_novice_0$Mayor_plan)