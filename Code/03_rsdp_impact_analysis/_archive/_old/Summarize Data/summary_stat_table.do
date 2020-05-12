* Impact of Roads on NTL and Land Cover Change

set more off 

* Rob Personal Computer
global filepath "/Users/robmarty/Dropbox/Ethiopia IE"

* texdoc do "/Users/robmarty/Dropbox/Ethiopia IE/Code/Analysis/Summarize Data/summary_stat_table.do"

* ------------------------------------------------------------------------------
*                    Load Data and Prep Variables
* ------------------------------------------------------------------------------

use "$filepath/Data/Fishnets/Fishnets STATA Datasets/panel_annual/fishnet.5x5km.extraction_panelannual.dta", clear
tempfile data_all
save `data_all'

* ------------------------------------------------------------------------------
*                         Summary Stat Table
* ------------------------------------------------------------------------------

texdoc init "$filepath/Results/summary_stat_full.tex", replace

tex \begin{tabular}{lcccccc}
tex \hline
tex Variable & Mean & SD & Min Year & Max Year & Resolution & Source \\
tex \hline

* Nighttime Lights -------------------------------------------------------------
local var dmspols_mean 
	tex Nighttime Lights (DMSP-OLS), Intercalibrated &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex $\sim$1 km &
	tex NOAA \\	
	
local var viirs_mean 
	tex Nighttime Lights (VIIRS), Average &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex $\sim$750 m &
	tex NOAA \\
	
local var viirs_sum 
	tex Nighttime Lights (VIIRS), Sum &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex $\sim$750 m &
	tex NOAA \\
	
* ESA-Globcover ----------------------------------------------------------------	
local var gc_cropland_mean 
	tex \% Cropland &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex 300 m &
	tex ESA-Globcover \\	
	
local var gc_urban_mean 
	tex \% Urban &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex 300 m &
	tex ESA-Globcover \\
	
local var gc_vegetation_mean 
	tex \% Vegetation &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex 300 m &
	tex ESA-Globcover \\		
	
* Market Access ----------------------------------------------------------------	
local var ma_access_travel_time
	tex Market Access (Travel Time) &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex 5 km &
	tex Roads, Population, Author's Calculation \\		
	
local var ma_euc_dist 
	tex Market Access (Euclidean Distance)  &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex 5 km &
	tex Population, Author's Calculation \\		
	
* Distance Roads ---------------------------------------------------------------	
	
local var distrd_all
	tex Distance Any Road  &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex 5 km &
	tex ERA \\		
	
local var distrd_type_trunk
	tex Distance Trunk Road  &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex 5 km &
	tex ERA \\	
	 
local var distrd_rdclss_urrap
	tex Distance URRAP Road  &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex 5 km &
	tex ERA \\
	
local var distrd_addis_adama
	tex Distance Addis-Adama Expressway  &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex 5 km &
	tex ERA \\	
	
* Soil Moisture ----------------------------------------------------------------
  
local var soilmoist 
	tex Soil Moisture &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex $\sim$25km &
	tex ESA \\	 	
 
* Precipitation ----------------------------------------------------------------	
local var mean_precip 
	tex Average Monthly Precipitation &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex $\sim$50 km &
	tex NOAA \\	 
	
local var min_precip 
	tex Min Monthly Precipitation &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex $\sim$50 km &
	tex NOAA \\		
	
local var max_precip 
	tex Max Monthly Precipitation &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex $\sim$50 km &
	tex NOAA \\	
 
* Temperature ------------------------------------------------------------------
local var mean_temp_maxavg 
	tex Average of Monthly Average Maximum Temperature &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex $\sim$50 km &
	tex NOAA \\	

local var min_temp_maxavg 
	tex Min of Monthly Average Maximum Temperature &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex $\sim$50 km &
	tex NOAA \\	
	
local var max_temp_maxavg 
	tex Max of Monthly Average Maximum Temperature &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex $\sim$50 km &
	tex NOAA \\	
	
local var mean_temp_minavg 
	tex Average of Monthly Average Minimum Temperature &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex $\sim$50 km &
	tex NOAA \\	

local var min_temp_minavg 
	tex Min of Monthly Average Minimum Temperature &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex $\sim$50 km &
	tex NOAA \\	
	
local var max_temp_minavg 
	tex Max of Monthly Average Minimum Temperature &
	sum `var'
	tex `=string(`=round(`r(mean)', .001)',"%4.3f")' &
	tex `=string(`=round(`r(sd)', .001)',"%4.3f")' &
	sum year if `var' != .
	tex `=string(`=round(`r(min)', 1)',"%4.0f")' &
	tex `=string(`=round(`r(max)', 1)',"%4.0f")' &
	tex $\sim$50 km &
	tex NOAA \\		
 
tex \hline
tex \end{tabular} 
texdoc close

* ------------------------------------------------------------------------------
*                         Sources & Website Table
* ------------------------------------------------------------------------------

/*
texdoc init "$filepath/Results/data_sources.tex", replace

tex \begin{tabular}{lcccccc}
tex \hline
tex Variable & Mean & SD & Min Year & Max Year & Resolution & Source \\
tex \hline

DMSP-OLS & https://urban.yale.edu/data	\\
VIIRS & https://ngdc.noaa.gov/eog/download.html	\\
ESA-Globcover & http://due.esrin.esa.int/page_globcover.php	\\
Roads & Ethiopian Roads Authority (ERA)  \\
Population & http://www.citypopulation.de/Ethiopia.html 	\\
Soil Moisture & http://www.esa-soilmoisture-cci.org/node/227 	\\
Precipitation & https://www.esrl.noaa.gov/psd/data/gridded/data.cpc.globalprecip.html 	\\
Temperature & https://www.esrl.noaa.gov/psd/data/gridded/data.cpc.globaltemp.html 	\\

tex \hline
tex \end{tabular} 
texdoc close

* Sources:



