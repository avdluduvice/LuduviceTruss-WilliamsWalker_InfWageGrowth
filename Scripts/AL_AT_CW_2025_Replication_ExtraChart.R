# -----------------------------------------------------------------------
# Title: Did Inflation Affect Households Differently? 
# A Look at the Postpandemic Inflation and Wage Growth Dynamics
# File: Script for generating the extra chart in the appendix comparing
# R-CPI-I with R-C-CPI-I (Horwich, 2024)
# Authors: Andre Victor D. Luduvice, Anaya M. Truss-Williams, and
# Christopher J. Walker
# Cleveland, August 2025
# -----------------------------------------------------------------------

#==============================================================================#
### DATA CLEANING
# import the data: 
cpi_i <- as.data.table(read_excel("Data/r-cpi-i-data.xlsx", skip = 1))

# pull the data from the CPI website (R-C-CPI-I December 2005 - December 2023)
# https://www.bls.gov/cpi/research-series/r-cpi-i.htm
c_cpi_i <- as.data.table(read_excel("Data/r-c-cpi-i-data.xlsx", skip = 1))

# create a date variable for both data frames: 
cpi_i$date <- as.POSIXct(paste(cpi_i$Year, cpi_i$Month, "01", sep = "-"), format = "%Y-%m-%d")
c_cpi_i$date <- as.POSIXct(paste(c_cpi_i$Year, c_cpi_i$Month, "01", sep = "-"), format = "%Y-%m-%d")

# remove excess columns from each one: 
cpi_i <- cpi_i %>% select(-c(Year, Month))
c_cpi_i <- c_cpi_i %>% select(-c(Year, Month))

# a function to create a percentage change:
pctx <- function(old, new){
  new <- ((old-100)/100)
}

# using this for within the data tables: 
cpi_i[, Q1:=pctx(`R-CPI-I1`, Q1), ]
cpi_i[, Q2:=pctx(`R-CPI-I2`, Q2), ]
cpi_i[, Q3:=pctx(`R-CPI-I3`, Q3), ]
cpi_i[, Q4:=pctx(`R-CPI-I4`, Q4), ]
cpi_i[, Q5:=pctx(`R-CPI-I5`, Q5), ]

c_cpi_i[, Q1:= pctx(`R-C-CPI-I1`, Q1), ]
c_cpi_i[, Q2:= pctx(`R-C-CPI-I2`, Q2), ]
c_cpi_i[, Q3:= pctx(`R-C-CPI-I3`, Q3), ]
c_cpi_i[, Q4:= pctx(`R-C-CPI-I4`, Q4), ]
c_cpi_i[, Q5:= pctx(`R-C-CPI-I5`, Q5), ]

# only select the date for December 2023: 
cpi_i = cpi_i[date == "2023-12-01", ]
c_cpi_i = c_cpi_i[date == "2023-12-01", ]

# select the variables and possibly pivot? 
cpi_i <- cpi_i %>% select(date, Q1, Q2, Q3, Q4, Q5)
c_cpi_i <- c_cpi_i %>% select(date, Q1, Q2, Q3, Q4, Q5)

cpi_i <- cpi_i %>% pivot_longer(cols=c(Q1, Q2, Q3, Q4, Q5), names_to = "var", values_to = "values")
cpi_i <- as.data.table(cpi_i)
cpi_i[, CPI:= "R-CPI-I", ]

c_cpi_i <- c_cpi_i %>% pivot_longer(cols = c(Q1, Q2, Q3, Q4, Q5), 
                                    names_to = "var", 
                                    values_to = "values")
c_cpi_i <- as.data.table(c_cpi_i)
c_cpi_i[, CPI:= "R-C-CPI-I", ]

# combine the data together into one: 
data <- rbind(cpi_i, c_cpi_i)

#==============================================================================#
# CHART
#==============================================================================#

# Graph 11 <-Cumulated inflation measured by the R-CPI-I and R-C-CPI-I by income quintiles 

graph11 <- ggplot(data, aes(fill = CPI, y = values, x = var)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  theme_classic() +  
  labs(x = "Income Quintiles", y = "Cumulative Inflation (%)") +
  theme_house + 
  theme(legend.position=c(0.1,0.9)) + #override legend position
  theme(plot.caption = element_text(hjust=(0), size=11, color="black")) +
  scale_y_continuous(limits=c(0,.80), breaks = seq(0,0.80, by=.10)) +
  scale_fill_manual('CPI', values = c(color5, color4))

# writes the data represented at each quartile bin in the graph
ggplot_build(graph11)$data[[1]]
