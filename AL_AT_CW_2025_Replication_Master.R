# -----------------------------------------------------------------------
# Title: Did Inflation Affect Households Differently? 
# A Look at the Postpandemic Inflation and Wage Growth Dynamics
# File: Master script for running replication package
# Authors: Andre Victor D. Luduvice, Anaya M. Truss-Williams, and
# Christopher J. Walker
# Cleveland, August 2024
# -----------------------------------------------------------------------

# Load necessary packages
library(data.table)
library(tidyverse)
library(lubridate)
library(zoo)
library(readxl)
library(fredr)

# Change the filepath to your directory
setwd("filepath/AL_AT_CW_2025_Replication/")
output <- paste0(getwd(), "/Output/")

# Request an API key from the link below to retrieve data from FRED
# https://fred.stlouisfed.org/docs/api/api_key.html
fredr_set_key("yourAPIkey") #type your FRED API key here

# Recession shading set up - Update line 32 with your API key 
add_rec_shade <- function(st_date,ed_date,shade_color="gray")
{
  library(fredr)
  library(ecm)
  library(ggplot2)
  fredr_set_key("yourAPIkey") #type your FRED API key here
  
  st_date <- as.Date("2018-01-01")
  ed_date <- as.Date(Sys.Date())
  
  recession <- fredr(series_id = "USRECD",observation_start = as.Date(st_date),
                   observation_end = as.Date(ed_date))
  
  recession$diff <- recession$value-lagpad(recession$value,k=1)
  recession <- recession[!is.na(recession$diff),]
  recession.start <- recession[recession$diff==1,]$date
  recession.end <- recession[recession$diff==(-1),]$date
  
  if(length(recession.start)>length(recession.end))
  {recession.end <- c(recession.end,Sys.Date())}
  if(length(recession.end)>length(recession.start))
  {recession.start <- c(min(recession$date),recession.start)}
  
  recs<-as.data.frame(cbind(recession.start,recession.end))
  recs$recession.start <- as.Date(as.numeric(recs$recession.start),
                                origin=as.Date("1970-01-01"))
  recs$recession.end <- as.Date(recs$recession.end,origin=as.Date("1970-01-01"))
  if(nrow(recs)>0)
  {
    rec_shade<-geom_rect(data=recs, inherit.aes=F, 
                         aes(xmin=recession.start, xmax=recession.end, 
                             ymin=-Inf, ymax=+Inf), fill=shade_color, alpha=0.4)
    return(rec_shade)
  }
}

# Setting colors 
color1 = "#f22a2a" #red
color2 = '#62bb50' #green
color3 = '#9467bd' #purple
color4 = '#2875a8' #blue
color5 = '#e67a17' #orange

# THEME HOUSE
theme_house <- theme(axis.title = element_text(size=20, color="black"),
                     axis.text.y=element_text(size=20, color="black"),
                     axis.text.x=element_text(size=12, color="black"),
                     strip.background = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black", 
                                                     linewidth = 1),
                     plot.title=element_text(size=18, color="black"),
                     legend.key.size = unit(.69, 'cm'),
                     legend.text=element_text( size=15),
                     legend.text.align = 1,
                     legend.title=element_blank(),
                     legend.key = element_rect(colour = "transparent", 
                                               fill="transparent"),
                     legend.background = element_rect(linetype = 1, 
                                                      size = 0.5, colour = 1),
                     legend.justification = c(0,1))

# Cleaning wage data ----------------------------------------------------
source(paste0(getwd(),"/Code/AL_AT_CW_2025_Replication_Wage_Cleaning.R"), local = TRUE)

# Graphing --------------------------------------------------------------
source(paste0(getwd(),"/Code/AL_AT_CW_2025_Replication_Graphing_Earn_Inf.R"), local = TRUE)
source(paste0(getwd(),"/Code/AL_AT_CW_2025_Replication_Graphing_Eq_Earn_PP.R"), local = TRUE)
source(paste0(getwd(),"/Code/AL_AT_CW_2025_Replication_ExtraChart.R"), local = TRUE)

# Appendix --------------------------------------------------------------
source(paste0(getwd(),"/Code/AL_AT_CW_2025_Replication_Appendix.R"), local = TRUE)

#------------------------------------------------------------------------------#
