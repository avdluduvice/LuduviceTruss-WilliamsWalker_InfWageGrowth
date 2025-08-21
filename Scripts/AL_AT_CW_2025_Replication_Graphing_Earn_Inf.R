# -----------------------------------------------------------------------
# Title: Did Inflation Affect Households Differently? 
# A Look at the Postpandemic Inflation and Wage Growth Dynamics
# File: Script for graphing breakdown of earnings growth and
# inflation facing the disaggregated groups
# Authors: Andre Victor D. Luduvice, Anaya M. Truss-Williams, and
# Christopher J. Walker
# Cleveland, August 2025
# -----------------------------------------------------------------------

# pull in data tables: 
wag <- as.data.table(read.csv("data/wage_quintile_annualized.csv"))
inf <- as.data.table(read.csv("data/cpi_disagg_yoy.csv"))

# edit the data tables dates so that the graph can read them: 
inf$date <- as.Date(inf$date)
wag$date <- as.Date(wag$date, format = "%y-%m-%d")

inf = inf[date >= "2019-01-01" & date < "2024-12-01", ]
wag = wag[date >= "2019-01-01" & date < "2024-12-01", ]

#add an inflation/disinflation line for June 2022
disinf_date <- as.Date("2022-06-01")

#==============================================================================#
# CHARTS
#==============================================================================#

## Inflation ###################################################################

# updating output path 
output <- "Output/Inflation"

### Graph 1 - 12 month inflation pct change, headline and divisions, percent change of averages
graph1 <- ggplot(inf) + 
  add_rec_shade(min(inf$date),max(inf$date)) +
  annotate("segment", x = disinf_date, xend = disinf_date, y = -Inf, yend = +Inf, 
           linetype = "dashed", color = 'darkgray', size = 1.5) +
  geom_line(size = 1.25, aes(y=cpi_12, x = date, color = "Headline")) +
  geom_line(size = 1, aes(y=cpi_b40_12, x=date, color = "Bottom 40%")) +
  geom_line(size = 1, aes(y=cpi_m40_12, x=date, color = "Middle 40%")) +
  geom_line(size = 1, aes(y=cpi_q5_12, x=date, color = "Top 20%")) +
  geom_hline(yintercept = seq(0,10, by = 2.5), size = 1, linetype = 1)+
  labs(x = " ", y = "12 Month Percent Change", title = " ") +
  theme_classic() +
  theme(legend.position = c(0.1,0.85)) + #override legend location 
  scale_color_manual(values = c("Headline" = 'black', 
                                "Bottom 40%" = color1,
                                "Middle 40%" =color2, 
                                "Top 20%" = color3)) + 
  scale_x_date(date_break = "8 months", date_labels = "%b-%Y") + theme_house

ggsave("cpi_inf_division.jpeg", graph1, path = output, width = 14, height = 8, unit = "in")

### Graph 2 - 12 month inflation gap between the division group and the headline, percent change of averages
graph2 <- ggplot(inf) +
  add_rec_shade(min(inf$date),max(inf$date)) +
  annotate("segment", x = disinf_date, xend = disinf_date, y = -Inf, yend = +Inf, 
           linetype = "dashed", color = 'darkgray', size = 1.5) + 
  geom_line(size = 1, aes(y=(cpi_b40_12-cpi_12), x=date, color = "Bottom 40%")) +
  geom_line(size = 1, aes(y=(cpi_m40_12-cpi_12), x=date, color = "Middle 40%")) +
  geom_line(size = 1, aes(y=(cpi_q5_12-cpi_12), x=date, color = "Top 20%")) +
  geom_hline(yintercept = seq(-0.5,0.75, by = 0.25), size = 1, linetype = 1)+
  geom_hline(yintercept = 0, size = 2, linetype = 1)+
  labs(x = " ", y = "Group - Headline, Percentage Points", title = " ") + theme_classic() +
  theme_house +
  theme(legend.position = c(0.1,0.85)) + #override legend location
  scale_color_manual(values = c("Bottom 40%" = color1,
                                "Middle 40%" = color2, 
                                "Top 20%" = color3)) + 
  scale_x_date(date_break = "8 months", date_labels = "%b-%Y") 

ggsave("cpi_inf_diff_division.jpeg", graph2, path = output, width = 14, height = 8, unit = "in")

### Graph 3 - 12 month inflation pct change, headline and quintiles, percent change of averages
graph3 <- ggplot(inf)+
  add_rec_shade(min(inf$date),max(inf$date)) +
  annotate("segment", x = disinf_date, xend = disinf_date, y = -Inf, yend = +Inf, 
           linetype = "dashed", color = 'darkgray', size = 1.5) +
  geom_line(size = 1.25, aes(y=cpi_12, x = date, color = "Headline")) +
  geom_line(size = 1, aes(y=cpi_q1_12, x=date, color = "1st Quintile")) +
  geom_line(size = 1, aes(y=cpi_q2_12, x=date, color = "2nd Quintile")) +
  geom_line(size = 1, aes(y=cpi_q3_12, x=date, color = "3rd Quintile")) +
  geom_line(size = 1, aes(y=cpi_q4_12, x=date, color = "4th Quintile")) +
  geom_line(size = 1, aes(y=cpi_q5_12, x=date, color = "5th Quintile")) +
  geom_hline(yintercept = seq(0,10, by = 2.5), size = 1, linetype = 1) +
  labs(x = " ", y = "12 Month Percent Change", 
       title = " ") + 
  theme_classic() +
  theme_house + 
  theme(legend.position=c(0.1,0.8)) + #override legend position
  theme(plot.caption = element_text(hjust=(0), size=11, color="black")) +
  scale_color_manual(values = c("Headline" = 'black', 
                                "1st Quintile" = color1, 
                                "2nd Quintile" = color5,
                                "3rd Quintile" = color2, 
                                "4th Quintile" = color4, 
                                "5th Quintile" = color3)) + 
  scale_x_date(date_break = "8 months", date_labels = "%b-%Y")

#ggsave("cpi_inf_quintiles.jpeg", graph3, path = output, width = 14, height = 8, unit = "in")

### Graph 4 - 12 month inflation gap between the quintile group and the overall, percent change of averages
graph4 <- ggplot(inf) +
  add_rec_shade(min(inf$date),max(inf$date)) +
  annotate("segment", x = disinf_date, xend = disinf_date, y = -Inf, yend = +Inf, 
           linetype = "dashed", color = 'darkgray', size = 1.5) +
  geom_line(size = 1, aes(y=(cpi_q1_12-cpi_12), x=date, color = "1st Quintile")) +
  geom_line(size = 1, aes(y=(cpi_q2_12-cpi_12), x=date, color = "2nd Quintile")) +
  geom_line(size = 1, aes(y=(cpi_q3_12-cpi_12), x=date, color = "3rd Quintile")) +
  geom_line(size = 1, aes(y=(cpi_q4_12-cpi_12), x=date, color = "4th Quintile")) +
  geom_line(size = 1, aes(y=(cpi_q5_12-cpi_12), x=date, color = "5th Quintile")) +
  geom_hline(yintercept = seq(-0.5,0.75, by = 0.25), size = 1, linetype = 1)+
  geom_hline(yintercept = 0, size = 2, linetype = 1)+
  labs(x = " ", y = "Group - Headline, Percentage Points", 
       title = " ") +
  theme_classic() +
  theme_house +
  theme(legend.position=c(0.1,0.8)) + #override legend location
  theme(plot.caption = element_text(hjust=(0), size=11)) +
  scale_color_manual(values = c("1st Quintile" = color1, 
                                "2nd Quintile" = color5,
                                "3rd Quintile" = color2, 
                                "4th Quintile" = color4, 
                                "5th Quintile" = color3)) + 
  scale_x_date(date_break = "8 months", date_labels = "%b-%Y")

#ggsave("cpi_inf_diff_quintiles.jpeg", graph4, path = output, width = 14, height = 8, unit = "in")

## Wages ######################################################################

# updating output path 
output <- "Output/Wages"

### Graph 5 - 12mo wage growth, median and by division, percent change of averages
graph5 <- ggplot(wag) + 
  add_rec_shade(min(wag$date),max(wag$date)) +
  annotate("segment", x = disinf_date, xend = disinf_date, y = -Inf, yend = +Inf, 
           linetype = "dashed", color = 'darkgray', size = 1.5) +
  geom_line(size = 2, aes(y=wg_agg_12, x = date, color = "Median")) +
  geom_line(size = 1, aes(y=wg_b40_12, x=date, color = "Bottom 40%")) +
  geom_line(size = 1, aes(y=wg_m40_12, x=date, color = "Middle 40%")) +
  geom_line(size = 1, aes(y=wg_q5_12, x=date, color = "Top 20%")) +
  geom_hline(yintercept = seq(2,8, by = 2), size = 1, linetype = 1)+
  labs(x = " ", y = "Percent", title = " ") + theme_classic() +
  theme_house +
  theme(legend.position=c(0.1,0.8)) + #override legend position
  scale_color_manual(values = c("Median" = 'black', 
                                "Bottom 40%" = color1,
                                "Middle 40%" = color2, 
                                "Top 20%" = color3)) + 
  scale_x_date(date_break = "8 months", date_labels = "%b-%Y")

ggsave("wage_division.jpeg", graph5, path = output, width = 14, height = 8, unit = "in")

### Graph 6 - 12mo wage growth, median and by quintile, percent change of averages
graph6 <- ggplot(wag) + 
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
  theme(legend.position=c(0.1,0.8),
        legend.justification = c(0,1)) + #override legend position
  theme(plot.caption = element_text(hjust=(0), size=11)) +
  scale_color_manual(values = c("Median" = 'black', 
                                "1st Quintile" = color1, 
                                "2nd Quintile" = color5,
                                "3rd Quintile" = color2, 
                                "4th Quintile" = color4, 
                                "5th Quintile" = color3)) + 
  scale_x_date(date_break = "8 months", date_labels = "%b-%Y")

#ggsave("wage_quintiles.jpeg", graph6, path = output, width = 14, height = 8, unit = "in")

#==============================================================================#

