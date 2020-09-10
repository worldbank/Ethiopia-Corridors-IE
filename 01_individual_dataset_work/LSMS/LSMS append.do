clear all

/*
* Installing necessary Stata packages
	ssc install shp2dta
*/
	dis "`c(username)'" // The text that shows up is the username of your computer (say XXX), and insert that into the code below

	*change to working directory

	* Alice
	if c(username)=="wb495814" {
			global onedrive ""
            }
			*

	* Kaustubh
	if c(username)=="wb554990" {
			global onedrive "C:/Users/wb554990/OneDrive - WBG/Ethiopia IE"
			}
			
	if c(username)=="kc" {
			global onedrive ""
			}

* ---------------------------------------------------------------------------- *
*  									GLOBALS
* ---------------------------------------------------------------------------- *


	global lsms_rawdata				"$onedrive/Data/RawData/LSMS"
	
	global lsms_2011				"$lsms_rawdata/2011"

	global lsms_2013				"$lsms_rawdata/2013"

	global lsms_2015				"$lsms_rawdata/2015"

	
	global lsms_intermed_data		"$onedrive/Data/IntermediateData/LSMS"
	
	global lsms_finaldata			"$onedrive/Data/FinalData/LSMS"
	

	
	
*-------------------------------------------------------------------------------
*...............................................................................
* 								LSMS 2011 Survey
*...............................................................................
*-------------------------------------------------------------------------------

	use "$lsms_2011/Pub_ETH_HouseholdGeovariables_Y1.dta", clear
	
	rename *, lower

	keep household_id ea_id lat_dd_mod lon_dd_mod
	
	save "$lsms_intermed_data/lsms_2011_geo.dta", replace
	
	
	
	use "$lsms_2011/sect9_hh_w1.dta", clear
	
	keep household_id ea_id saq01 saq02 saq03 saq04 saq05 saq06 saq07 saq08 pw rural hh_s9q19 hh_s9q20
	
	merge 1:m household_id using "$lsms_intermed_data/lsms_2011_geo.dta"

*!!!!!!!!!!!!!!!!!!
*	52 household ids from the household dataset did not match the geo dataset!
*!!!!!!!!!!!!!!!!!!
	
	br household_id saq01 saq02 saq03 saq04 saq05 saq06 saq07 saq08 if _merge==1

	save "$lsms_intermed_data/lsms_2011_geomerged.dta", replace


*-------------------------------------------------------------------------------
*	Exploring LSMS 2011 Electricity variables
*-------------------------------------------------------------------------------
	
	use "$lsms_intermed_data/lsms_2011_geomerged.dta", clear
	
	label list hh_s9q19
	label list hh_s9q20
	
	table hh_s9q19 [pweight=pw], missing row format(%11.2gc)
	table hh_s9q19 if hh_s9q20==. [pweight=pw], missing row format(%11.2gc)
	table hh_s9q19 if hh_s9q20==1 [pweight=pw], missing row format(%11.2gc)
	
	
*-------------------------------------------------------------------------------
*...............................................................................
* 								LSMS 2013 Survey
*...............................................................................
*-------------------------------------------------------------------------------

	use "$lsms_2013/Pub_ETH_HouseholdGeovars_Y2.dta", clear
	
	codebook household_id household_id2
	
	rename *, lower

	keep household_id household_id2 ea_id ea_id2 lat_dd_mod lon_dd_mod
	
	save "$lsms_intermed_data/lsms_2013_geo.dta", replace
	
	
	
	use "$lsms_2013/sect9_hh_w2.dta", clear
	
	codebook household_id household_id2	
	
	keep household_id household_id2 ea_id ea_id2 saq01 saq02 saq03 saq04 saq05 saq06 saq07 saq08 ///
			pw rural hh_s9q19_a hh_s9q19_b hh_s9q19_c hh_s9q20_a hh_s9q20_b
	
	merge m:1 household_id2 using "$lsms_intermed_data/lsms_2013_geo.dta"

*!!!!!!!!!!!!!!!!!!
*	25 household ids from the geo dataset did not match the household dataset!
*!!!!!!!!!!!!!!!!!!

	save "$lsms_intermed_data/lsms_2013_geomerged.dta", replace

*-------------------------------------------------------------------------------
*	Exploring LSMS 2013 Electricity variables
*-------------------------------------------------------------------------------
	
	use "$lsms_intermed_data/lsms_2013_geomerged.dta", clear
	
	codebook hh_s9q19_a hh_s9q19_b hh_s9q19_c
	
	label list hh_s9q19_a
	label list hh_s9q19_b	
	label list hh_s9q19_c
	
	label list hh_s9q20
	
	table hh_s9q19 [pweight=pw], missing row format(%11.2gc)
	table hh_s9q19 if hh_s9q20==. [pweight=pw], missing row format(%11.2gc)
	table hh_s9q19 if hh_s9q20==1 [pweight=pw], missing row format(%11.2gc)
	
	
	

	
*-------------------------------------------------------------------------------
*...............................................................................
* 								LSMS 2015 Survey
*...............................................................................
*-------------------------------------------------------------------------------

	use "$lsms_2015/ETH_HouseholdGeovars_y3.dta", clear

	codebook household_id2

	rename *, lower
	
	keep household_id2 ea_id ea_id2 lat_dd_mod lon_dd_mod

	save "$lsms_intermed_data/lsms_2015_geo.dta", replace
	
	
	
	use "$lsms_2015/sect9_hh_w3.dta", clear
	
	codebook household_id household_id2
	
	keep household_id household_id2 ea_id ea_id2 saq01 saq02 saq03 saq04 saq05 saq06 saq07 saq08 ///
			pw rural hh_s9q19_a hh_s9q19_c hh_s9q20_a hh_s9q20_b
	
	merge 1:1 household_id2 using "$lsms_intermed_data/lsms_2015_geo.dta"
	
	save "$lsms_intermed_data/lsms_2015_geomerged.dta", replace

	

	
*-------------------------------------------------------------------------------
*...............................................................................
* 					LSMS 2013 Survey Household ID correction
*...............................................................................
*-------------------------------------------------------------------------------

	use "$lsms_intermed_data/lsms_2013_geomerged.dta", clear
	
	count if household_id=="" & household_id2!=""
	* 1,486 missing w1 HSIDs with nonmissing w2 HSIDs


	use "$lsms_intermed_data/lsms_2015_geomerged.dta", clear
	
	keep household_id household_id2
	
	save "$lsms_intermed_data/household_id_w1_w2.dta", replace
	

	merge 1:1 household_id2 using "$lsms_intermed_data/lsms_2013_geomerged.dta", gen(id2merge)
	
	drop if id2merge==1
	
	count if household_id=="" & household_id2!=""
	* Now we are only down to 238 missing w1 HSIDs with nonmissing w2 HSIDs
	
	save "$lsms_intermed_data/lsms_2013_geomerged2.dta", replace



*-------------------------------------------------------------------------------
*...............................................................................
* 						Creating a Panel LSMS dataset
*...............................................................................
*-------------------------------------------------------------------------------

	use "$lsms_intermed_data/lsms_2011_geomerged.dta", clear
	
	append using "$lsms_intermed_data/lsms_2013_geomerged2.dta"
	append using "$lsms_intermed_data/lsms_2015_geomerged.dta"
	
	save "$lsms_finaldata/lsms_panel_2011_2015.dta", replace
	
/*
	NOTE: 	The different waves have inconsistencies in questions 19 and 20 in
			existence of options.
			Also, not all household IDs were able to be matches with geo coordinates.
*/
