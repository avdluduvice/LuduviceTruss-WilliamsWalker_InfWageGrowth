# -------------------------------------------------------------------------
# Title: Did Inflation Affect Households Differently? 
# A Look at the Postpandemic Inflation and Wage Growth Dynamics
# File: Script for cleaning data and preparing for analysis
# Authors: Andre Victor D. Luduvice, Anaya M. Truss-Williams, and
# Christopher J. Walker
# Cleveland, August 2024
# -------------------------------------------------------------------------

# import the data: 
data <- read.csv(paste0(getwd(),"/Data/wage_data_worker.csv"))

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

# save relevant variables: 
data <- data %>% select(c(date, wg_agg_12, wg_q1_12, wg_q2_12, wg_q3_12, 
                          wg_q4_12, wg_q5_12, wg_b40_12, wg_m40_12))

# transformations to percent change: 
pct12 <- function(old, new){
  lag <- lag(old, 12)
  new <- 100*log(old/lag)
}

# save as new file: 
write.csv(data, "Data/wage_quintile_annualized.csv")
rm(data)

################################################################################
# pull the data from the CPI website for the quintiles for inflation 
# https://www.bls.gov/cpi/research-series/r-cpi-i.htm
data <- as.data.table(read_excel("Data/r-cpi-i-data.xlsx", skip = 1))

# create the pct changes: 
data[, cpi_q1_12:= pct12(`R-CPI-I1`, cpi_q1_12), ]
data[, cpi_q2_12:= pct12(`R-CPI-I2`, cpi_q2_12), ]
data[, cpi_q3_12:= pct12(`R-CPI-I3`, cpi_q3_12), ]
data[, cpi_q4_12:= pct12(`R-CPI-I4`, cpi_q4_12), ]
data[, cpi_q5_12:= pct12(`R-CPI-I5`, cpi_q5_12), ]

# create a bottom 40% and a middle 40%
data[, cpi_b40:= (`R-CPI-I1`+`R-CPI-I2`)/2, ]
data[, cpi_m40:= (`R-CPI-I3`+`R-CPI-I4`)/2, ]

# create the percentage changes for 12 month and 1 month
data[, cpi_b40_12:= pct12(cpi_b40, cpi_b40_12), ]
data[, cpi_m40_12:= pct12(cpi_m40, cpi_m40_12), ]

# create the date variable as one variable that is of POSIXct class
data$f_month <- sprintf("%02d", data$Month)
data$date <- paste(data$Year, data$f_month, "01", sep="-")
data$date <- as.POSIXct(ymd(data$date))

# fix the table
data <- data %>% select(-c(f_month, Year, Month))

# pull in the other data files from FRED
cpi<-as.data.table(fredr(series_id = "CPIAUCSL"))

# modify the date variable
cpi$date <- as.POSIXct(as.Date(as.yearmon(cpi$date, format = "%Y-%b"), format = "%b %Y")) 
cpi <- cpi %>% select(-c(realtime_start,realtime_end,series_id))

# rebase everything to January 2005
spec_date <- as.POSIXct("2005-12-01", tz = "UTC")
key <- cpi[date == spec_date, value]
cpi[, cpi:= (value/key)*100, ]

# edit the data
cpi = cpi[date >= "2005-11-01" & date <= "2024-12-01", ]
cpi <- cpi %>% select(-c(value))

# create a new variable in the cpi data
cpi[, cpi_12:= pct12(cpi, cpi_12), ]

# merge the data together 
data <- merge(data, cpi, by = "date")

# save the data
write.csv(data, "data/cpi_disagg_yoy.csv")
rm(data, cpi, key)

#------------------------------------------------------------------------------#
