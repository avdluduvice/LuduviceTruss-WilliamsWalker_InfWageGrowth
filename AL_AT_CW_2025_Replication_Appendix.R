# -----------------------------------------------------------------------
# Title: Did Inflation Affect Households Differently? 
# A Look at the Postpandemic Inflation and Wage Growth Dynamics
# File: Script pulls and saves the previous charts made for the appendix
# Authors: Andre Victor D. Luduvice, Anaya M. Truss-Williams, and
# Christopher J. Walker
# Cleveland, August 2025
# -----------------------------------------------------------------------

# Make sure to run the code in order as shown in the master, as this script will
# not run without that first. 

# pull the graph numbers that are needed from the appendix: 
# Graph 3: 12 month inflation pct change, headline and quintiles, percent change of averages
# Graph 4: 12 month inflation gap between the quintile group and the headline, percent change of averages
# Graph 6: 12mo wage growth, median and by quintile, percent change of averages
# Graph 7: Equivalized Wages by division
# Graph 8: Equivalized Wages by quintiles
# Graph 10: Cumulative Purchasing Power by Division
# Graph 11: Cumulated inflation measured by the R-CPI-I and R-C-CPI-I by income quintiles
#------------------------------------------------------------------------------#
# updating output path 
output <- "Output/Appendix"

# save the files: 
ggsave("cpi_inf_quintiles.jpeg", graph3, path = output, width = 14, height = 8, unit = "in")
ggsave("cpi_inf_diff_quintiles.jpeg", graph4, path = output, width = 14, height = 8, unit = "in")
ggsave("wage_quintiles.jpeg", graph6, path = output, width = 14, height = 8, unit = "in")
ggsave("equiv_wag_div.jpeg", graph7, path = output, width = 14, height = 8, unit = "in")
ggsave("equiv_wag_quintiles.jpeg", graph8, path = output, width = 14, height = 8, unit = "in")
ggsave("purch_power_quintiles.jpeg", graph10, path = output, width = 14, height = 8, unit = "in")
ggsave("r-c-cpi_comp_quintiles.jpeg", graph11, path = output, width = 14, height = 8, unit = "in")

#------------------------------------------------------------------------------#
