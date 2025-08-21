# -----------------------------------------------------------------------
# Title: Did Inflation Affect Households Differently? 
# A Look at the Postpandemic Inflation and Wage Growth Dynamics
# Purpose: Replication code for graphing and analysis
# File: Script for extending previous graphing and analysis with addition of 
# purchasing power and utilization of equivalized household earnings
# Authors: Andre Victor D. Luduvice, Anaya M. Truss-Williams, and
# Christopher J. Walker
# Cleveland, August 2025
# -----------------------------------------------------------------------

#==============================================================================#
### DATA CLEANING
# import the data: 
data <- read.csv("Data/eq_indwages_hh_data.csv")

# create it as a data.table and convert all series to a 12 month moving average: 
data <- as.data.table(data)

ma12 <- function(old, new){
  new <- rollmean(old, k=12, na.pad=T, align="right")
}

# edit the date variable: 
data$date <- as.POSIXct(as.Date(data$date, format = "%m/%d/%Y"))

data = data[, wg_agg_12:= ma12(mwagegrowth_agg, wg_agg_12), ]
data = data[, wg_q1_12:= ma12(mwagegrowth_agg_q1, wg_q1_12), ]
data = data[, wg_q2_12:= ma12(mwagegrowth_agg_q2, wg_q2_12), ]
data = data[, wg_q3_12:= ma12(mwagegrowth_agg_q3, wg_q3_12), ]
data = data[, wg_q4_12:= ma12(mwagegrowth_agg_q4, wg_q4_12), ]
data = data[, wg_q5_12:= ma12(mwagegrowth_agg_q5, wg_q5_12), ]
data = data[, wg_b40_12:= ma12(mwagegrowth_agg_b40, wg_b40_12), ]
data = data[, wg_m40_12:= ma12(mwagegrowth_agg_m40, wg_m40_12), ]

# save the relevant variables: 
data <- data %>% select(c(date, wg_agg_12, wg_q1_12, wg_q2_12, wg_q3_12, 
                          wg_q4_12, wg_q5_12, wg_b40_12, wg_m40_12))

# save it as a new file: 
write.csv(data, "data/eq_indwages_hh_annualized.csv")
rm(data)

#==============================================================================#
# CHARTS
#==============================================================================#
# pull in data tables: 
wag <- as.data.table(read.csv("data/eq_indwages_hh_annualized.csv"))
inf <- as.data.table(read.csv("data/cpi_disagg_yoy.csv"))

# edit the data tables dates so that the graph can read them: 
inf$date <- as.Date(inf$date)
wag$date <- as.Date(wag$date, format = "%y-%m-%d")

# edit the dates for the graph:
inf = inf[date >= "2019-01-01" & date <= "2024-12-01", ]
wag = wag[date >= "2018-12-01" & date <= "2024-12-01", ]

#add an inflation/disinflation line for June 2022
disinf_date <- as.Date("2022-06-01")

## Graphing ###################################################################
# updating output path 
output <- "Output/Equivalized"

## Wages ######################################################################
# Graph 7 <- Equivalized Wages by Division
graph7 <- ggplot(wag) + 
  add_rec_shade(min(wag$date),max(wag$date)) +
  annotate("segment", x = disinf_date, xend = disinf_date, y = -Inf, yend = +Inf, 
           linetype = "dashed", color = 'darkgray', size = 1.5) +
  geom_line(size = 2, aes(y=wg_agg_12, x = date, color = "Median")) +
  geom_line(size = 1, aes(y=wg_b40_12, x=date, color = "Bottom 40%")) +
  geom_line(size = 1, aes(y=wg_m40_12, x=date, color = "Middle 40%")) +
  geom_line(size = 1, aes(y=wg_q5_12, x=date, color = "Top 20%")) +
  geom_hline(yintercept = seq(2,8, by = 2), size = 1, linetype = 1)+
  labs(x = " ", y = "Percent", title = " ") +
  theme_classic() +
  theme_house + 
  theme(legend.position=c(0.1,0.8)) + #override legend position
  scale_color_manual(values = c("Median" = 'black', 
                                "Bottom 40%" = color1,
                                "Middle 40%" = color2, 
                                "Top 20%" = color3)) + 
  scale_x_date(date_break = "8 months", date_labels = "%b-%Y")

#ggsave("equiv_wag_div.jpeg", graph7, path = output, width = 14, height = 8, unit = "in")

# Graph 8 <- Equivalized Wages by Quintiles
graph8 <- ggplot(wag)+
  add_rec_shade(min(wag$date),max(wag$date)) +
  annotate("segment", x = disinf_date, xend = disinf_date, y = -Inf, yend = +Inf, 
           linetype = "dashed", color = 'darkgray', size = 1.5) +
  geom_line(size = 2, aes(y=wg_agg_12, x = date, color = "Median")) +
  geom_line(size = 1, aes(y=wg_q1_12, x=date, color = "1st Quintile")) +
  geom_line(size = 1, aes(y=wg_q2_12, x=date, color = "2nd Quintile")) +
  geom_line(size = 1, aes(y=wg_q3_12, x=date, color = "3rd Quintile")) +
  geom_line(size = 1, aes(y=wg_q4_12, x=date, color = "4th Quintile")) +
  geom_line(size = 1, aes(y=wg_q5_12, x=date, color = "5th Quintile")) +
  geom_hline(yintercept = seq(2,8, by = 2), size = 1, linetype = 1)+
  labs(x = " ", y = "Percent", title = " ") +
  theme_classic() +
  theme_house + 
  theme(legend.position=c(0.1,0.8)) + #override legend position
  theme(plot.caption = element_text(hjust=(0), size=11)) +
  scale_color_manual(values = c("Median" = 'black', 
                                "1st Quintile" = color1, 
                                "2nd Quintile" = color5,
                                "3rd Quintile" = color2, 
                                "4th Quintile" = color4, 
                                "5th Quintile" = color3)) + 
  scale_x_date(date_break = "8 months", date_labels = "%b-%Y")

#==============================================================================#
# PURCHASING POWER
# first, select the variables we want: 
pp_inf <- inf %>% select(date, cpi_b40_12, cpi_m40_12, cpi_q5_12, cpi_12, 
                         cpi_q1_12, cpi_q2_12, cpi_q3_12, cpi_q4_12)
pp_wag <- wag %>% select(date, wg_b40_12, wg_m40_12, wg_q5_12, wg_agg_12, 
                         wg_q1_12, wg_q2_12, wg_q3_12, wg_q4_12)

# combine the tables together: 
pp_inf$date <- as.character(pp_inf$date)
pp_wag$date <- as.character(pp_wag$date)
pp <- merge(pp_inf, pp_wag, by = "date")

pp$date <- as.Date(pp$date)

# create new variables and de-annuualize the data: 
pp[, d_wg_b40:= (1+(wg_b40_12/100)) ^ (1/12), ]
pp[, d_wg_m40:= (1+(wg_m40_12/100)) ^ (1/12), ]
pp[, d_wg_q5:= (1+(wg_q5_12/100)) ^ (1/12), ]
pp[, d_wg_ov:= (1+(wg_agg_12/100)) ^ (1/12), ]
pp[, d_cpi_b40:= (1+(cpi_b40_12/100)) ^ (1/12), ]
pp[, d_cpi_m40:= (1+(cpi_m40_12/100)) ^ (1/12), ]
pp[, d_cpi_q5:= (1+(cpi_q5_12/100)) ^ (1/12), ]
pp[, d_cpi_ov:= (1+(cpi_12/100)) ^ (1/12), ]

# pp[, pp_b40:= d_wg_b40-d_cpi_b40, ]
# pp[, pp_m40:= d_wg_m40-d_cpi_m40, ]
# pp[, pp_t20:= d_wg_q5-d_cpi_q5, ]
# pp[, pp_ov:= d_wg_ov-d_cpi_ov, ]

pp[, pp_b40:= d_wg_b40/d_cpi_b40-1, ]
pp[, pp_m40:= d_wg_m40/d_cpi_m40-1, ]
pp[, pp_t20:= d_wg_q5/d_cpi_q5-1, ]
pp[, pp_ov:= d_wg_ov/d_cpi_ov-1, ]

pp[, cs_b40:= cumsum(pp_b40)*100, ]
pp[, cs_m40:= cumsum(pp_m40)*100, ]
pp[, cs_t20:= cumsum(pp_t20)*100, ]
pp[, cs_ov:= cumsum(pp_ov)*100, ]

pp[, d_wg_q5:= (1+(wg_q5_12/100)) ^ (1/12), ]
pp[, d_wg_q4:= (1+(wg_q4_12/100)) ^ (1/12), ]
pp[, d_wg_q3:= (1+(wg_q3_12/100)) ^ (1/12), ]
pp[, d_wg_q2:= (1+(wg_q2_12/100)) ^ (1/12), ]
pp[, d_wg_q1:= (1+(wg_q1_12/100)) ^ (1/12), ]

pp[, d_cpi_q5:= (1+(cpi_q5_12/100)) ^ (1/12), ]
pp[, d_cpi_q4:= (1+(cpi_q4_12/100)) ^ (1/12), ]
pp[, d_cpi_q3:= (1+(cpi_q3_12/100)) ^ (1/12), ]
pp[, d_cpi_q2:= (1+(cpi_q2_12/100)) ^ (1/12), ]
pp[, d_cpi_q1:= (1+(cpi_q1_12/100)) ^ (1/12), ]

pp[, pp_q5:= d_wg_q5-d_cpi_q5, ]
pp[, pp_q4:= d_wg_q4-d_cpi_q4, ]
pp[, pp_q3:= d_wg_q3-d_cpi_q3, ]
pp[, pp_q2:= d_wg_q2-d_cpi_q2, ]
pp[, pp_q1:= d_wg_q1-d_cpi_q1, ]

pp[, cs_q5:= cumsum(pp_q5)*100, ]
pp[, cs_q4:= cumsum(pp_q4)*100, ]
pp[, cs_q3:= cumsum(pp_q3)*100, ]
pp[, cs_q2:= cumsum(pp_q2)*100, ]
pp[, cs_q1:= cumsum(pp_q1)*100, ]


## Graphing ###################################################################

# Graph 9 <- Cumulative Purchasing Power by Division

graph9 <- ggplot(pp) +
  add_rec_shade(min(pp$date),max(pp$date)) +
  annotate("segment", x = disinf_date, xend = disinf_date, y = -Inf, yend = +Inf, 
           linetype = "dashed", color = 'darkgray', size = 1.5) +
  geom_line(size = 1, aes(y=cs_b40, x=date, color = "Bottom 40%")) +
  geom_line(size = 1, aes(y=cs_m40, x=date, color = "Middle 40%")) +
  geom_line(size = 1, aes(y=cs_t20, x=date, color = "Top 20%")) +
  geom_hline(yintercept = seq(0,6, by = 2), size = 1, linetype = 1)+
  geom_hline(yintercept = 0, size = 2, linetype = 1)+ 
  labs(x = " ", y = "Cumulative Percentage Points", title = " ") +
  theme_classic() +
  theme_house + 
  theme(legend.position=c(0.1,0.8)) + #override legend position
  scale_color_manual(values = c("Bottom 40%" = color1, "Overall" = "black",
                                "Middle 40%" = color2, "Top 20%" = color3)) + 
  scale_x_date(date_break = "8 months", date_labels = "%b-%Y")

ggsave("purch_power_div.jpeg", graph9, path = output, width = 14, height = 8, unit = "in")

# # writes the data represented at each quartile bin in the graph
# ggplot_build(graph9)$data

# Graph 10 <- Cumulative Purchasing Power by Division

graph10 <- ggplot(pp) +
  add_rec_shade(min(pp$date),max(pp$date)) +
  annotate("segment", x = disinf_date, xend = disinf_date, y = -Inf, yend = +Inf, 
           linetype = "dashed", color = 'darkgray', size = 1.5) +
  geom_line(size = 1, aes(y=cs_q1, x=date, color = "1st Quintile")) +
  geom_line(size = 1, aes(y=cs_q2, x=date, color = "2nd Quintile")) +
  geom_line(size = 1, aes(y=cs_q3, x=date, color = "3rd Quintile")) +
  geom_line(size = 1, aes(y=cs_q4, x=date, color = "4th Quintile")) +
  geom_line(size = 1, aes(y=cs_q5, x=date, color = "5th Quintile")) +
  geom_hline(yintercept = seq(0,6, by = 2), size = 1, linetype = 1)+
  geom_hline(yintercept = seq(0,6, by = 2), size = 1, linetype = 1)+
  geom_hline(yintercept = 0, size = 2, linetype = 1)+ 
  labs(x = " ", y = "Cumulative Percentage Points", 
       title = " ") +
  theme_classic() +
  theme_house + 
  theme(legend.position=c(0.1,0.8)) + #override legend location
  theme(plot.caption = element_text(hjust=(0), size=11)) +
  scale_color_manual(values = c("Overall" = 'black', 
                                "1st Quintile" = color1, 
                                "2nd Quintile" = color5,
                                "3rd Quintile" = color2, 
                                "4th Quintile" = color4, 
                                "5th Quintile" = color3)) + 
  scale_x_date(date_break = "8 months", date_labels = "%b-%Y") 

#ggsave("purch_power_quintiles.jpeg", graph10, path = output, width = 14, height = 8, unit = "in")

#===============================================================================#
