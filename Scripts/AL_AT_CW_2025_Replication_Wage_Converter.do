/*******************************************************************************
Title: Did Inflation Affect Households Differently? A Look at the Postpandemic
Inflation and Wage growth Dynamics
File: Script for creating wage growth data shown in commentary images for wage 
growth and purchasing power
Main source: CPS Longitudinal Data from the Federal Reserve Bank of
Kansas City's - CADRE
Authors: Andre Victor D. Luduvice, Anaya M. Truss-Williams, and Christopher J. 
Walker
Cleveland, August 2025
********************************************************************************/

* Set data path for where the CPS_harmonized_variable_longitudinally_matched_age16plus.dta 

clear 
global data "filepath\Data"

use "$data\CPS_harmonized_variable_longitudinally_matched_age16plus.dta"

* now, we want to create the wages that comes from Daly, Hobijn and Wiles (2012)

/* With the Longitudinal Data we have, we can now go ahead and begin to re-create the
wages that FRB Atlanta has. But we also want to have the following in terms of both 
the mean and median, the qunitile, the bottom and middle 40% as well as the raw values: i
 - Workers Hourly
 */
 
* clean the data
drop if date < mdy(12, 31, 1996)
drop if wagegrowth83 ==.
drop if wagegrowthtracker83 ==. 

ssc install egenmore

********************************************************************************
******************************* WORKER HOURLY **********************************
********************************************************************************

* AGGREGATE
* we first need to create the median/overall: 
gen wagerate_agg = 100*ln(wageperhr82/wageperhr82_tm12)
egen wagegrowth_agg = mean(wagerate_agg), by(date)
egen mwagegrowth_agg = median(wagerate_agg), by(date)

* QUNITILES
* now, we will create the quintile wages: 
egen wage_agg_q = xtile(wageperhr82), nq(5) by (date)
egen wage_agg_q_tm12 = xtile(wageperhr82_tm12), nq(5) by (date)

forval i=1/5{
	gen wagerate_agg_q`i' = 100*ln(wageperhr82/wageperhr82_tm12) if (wage_agg_q == `i' & wage_agg_q_tm12 == `i')
	
	egen wagegrowth_agg_q`i' = mean(wagerate_agg_q`i'), by (date)
	egen mwagegrowth_agg_q`i' = median(wagerate_agg_q`i'), by (date)
}

* NYFed EHIs bins
* here, we will combine the bottom 40% and the middle 40%
gen wage_agg_40 = 1 if (wage_agg_q == 1 | wage_agg_q == 2)
replace wage_agg_40 = 2 if (wage_agg_q == 3 | wage_agg_q == 4)
gen wage_agg_40_tm12 = 1 if (wage_agg_q_tm12 == 1 | wage_agg_q_tm12 == 2)
replace wage_agg_40_tm12 = 2 if (wage_agg_q_tm12 == 3 | wage_agg_q_tm12 == 4)

gen wagerate_agg_b40 = 100*ln(wageperhr82/wageperhr82_tm12) if (wage_agg_40 == 1 & wage_agg_40_tm12 == 1)
gen wagerate_agg_m40 = 100*ln(wageperhr82/wageperhr82_tm12) if (wage_agg_40 == 2 & wage_agg_40_tm12 == 2)

egen wagegrowth_agg_b40 = mean(wagerate_agg_b40), by (date)
egen wagegrowth_agg_m40 = mean(wagerate_agg_m40), by (date)
egen mwagegrowth_agg_b40 = median(wagerate_agg_b40), by (date)
egen mwagegrowth_agg_m40 = median(wagerate_agg_m40), by (date)

********************************* DATA FILE CREATION ********************************

* now, let's only keep the variables that we want: 
keep date wagegrowth_agg wagegrowth_agg_q1 wagegrowth_agg_q2 wagegrowth_agg_q3 wagegrowth_agg_q4 wagegrowth_agg_q5 wagegrowth_agg_b40 wagegrowth_agg_m40 mwagegrowth_agg mwagegrowth_agg_q1 mwagegrowth_agg_q2 mwagegrowth_agg_q3 mwagegrowth_agg_q4 mwagegrowth_agg_q5 mwagegrowth_agg_b40 mwagegrowth_agg_m40 

* now, we should have this for only each date: 
duplicates drop date, force

* now, save this as a csv file: 
export delimited "$data\wage_data_worker.csv", delimiter(",") replace

/*******************************************************************************
******************************* EARNINGS **********************************
*******************************************************************************/

* now, we do the same thing, but this time for the equivalized household earnings 
* turned back into individual worker wages: 

clear 
use "$data\CPS_harmonized_variable_longitudinally_matched_age16plus.dta"

/* With the Longitudinal Data we have, we can now go ahead and begin to re-create the
wages that FRB Atlanta has. But we also want to have the following in terms of both 
the mean and median, the qunitile, the bottom and middle 40% as well as the raw values: 
 - Equivalized Household Earnings
We will start with the workers hourly after cleaning:
 */

* clean the data
drop if date < mdy(12, 31, 1996)

* first, we want the full sample instead of only samples in ATL Wage Tracker
drop if wagegrowthtracker83 == . 
* CHECK: drop if mlr76 != 1 |mlr76_tm12 != 1

* first, we count the number of people in the household: 
bysort householdid date: gen adults = _N

* then, let's calculate the number of children per household: 
bysort householdid date: gen hh_size = (numkids82 + adults)

* now, create the household earnings: 
* first, we start with the individual. Since we have wages per week, we will use 50 weeks
* as our way to create a yearly income:
gen inc = wageperhr82*hours82 
gen inc_tm12 = wageperhr82_tm12*hours82_tm12

* now, we sum earnings for the household: 
egen inc_hh = sum(inc), by (date householdid)
egen inc_hh_tm12 = sum(inc_tm12), by (date householdid)

* Now, we calculate number of hours worked by each household
egen hours = sum(hours82), by (householdid date)
egen hours_tm12 = sum(hours82_tm12), by (householdid date)

* now, we equivalize the data: 
gen income82 = inc_hh/sqrt(hh_size)
gen income82_tm12 = inc_hh_tm12/sqrt(hh_size)

*** QUNITILES 
egen wage_agg_q = xtile(income82), nq(5) by (date) 
egen wage_agg_q_tm12 = xtile(income82_tm12), nq(5) by (date) 


********************************************************************************
******************** RE-CALCULATED HOUSEHOLD WAGES *****************************
********************************************************************************
*** AGGREGATE
gen wagerate_agg = 100*ln(wageperhr82/wageperhr82_tm12)
egen wagegrowth_agg = mean(wagerate_agg), by(date)
egen mwagegrowth_agg = median(wagerate_agg), by(date)


*** QUNITILES 
forval i=1/5{
	gen wagerate_agg_q`i' = 100*ln(wageperhr82/wageperhr82_tm12) if (wage_agg_q == `i' & wage_agg_q_tm12 == `i')
	
	egen wagegrowth_agg_q`i' = mean(wagerate_agg_q`i'), by (date)
	egen mwagegrowth_agg_q`i' = median(wagerate_agg_q`i'), by (date)
}

*** NY FED EHIs bins
gen wage_agg_40 = 1 if (wage_agg_q == 1 | wage_agg_q == 2)
replace wage_agg_40 = 2 if (wage_agg_q == 3 | wage_agg_q == 4)
gen wage_agg_40_tm12 = 1 if (wage_agg_q_tm12 == 1 | wage_agg_q_tm12 == 2)
replace wage_agg_40_tm12 = 2 if (wage_agg_q_tm12 == 3 | wage_agg_q_tm12 == 4)

gen wagerate_agg_b40 = 100*ln(wageperhr82/wageperhr82_tm12) if (wage_agg_40 == 1 & wage_agg_40_tm12 == 1)
gen wagerate_agg_m40 = 100*ln(wageperhr82/wageperhr82_tm12) if (wage_agg_40 == 2 & wage_agg_40_tm12 == 2)

egen wagegrowth_agg_b40 = mean(wagerate_agg_b40), by (date)
egen wagegrowth_agg_m40 = mean(wagerate_agg_m40), by (date)
egen mwagegrowth_agg_b40 = median(wagerate_agg_b40), by (date)
egen mwagegrowth_agg_m40 = median(wagerate_agg_m40), by (date)

********************************* FILE CREATION ********************************
keep date wagegrowth_agg wagegrowth_agg_q1 wagegrowth_agg_q2 wagegrowth_agg_q3 wagegrowth_agg_q4 wagegrowth_agg_q5 wagegrowth_agg_b40 wagegrowth_agg_m40 mwagegrowth_agg mwagegrowth_agg_q1 mwagegrowth_agg_q2 mwagegrowth_agg_q3 mwagegrowth_agg_q4 mwagegrowth_agg_q5 mwagegrowth_agg_b40 mwagegrowth_agg_m40 

duplicates drop date, force

export delimited "$data\eq_indwages_hh_data.csv", delimiter(",") replace
