*****************************************************************************
*			Panel Fixed Effects
*****************************************************************************

* Ariana Computer
global filepath "/Users/ariana/Dropbox/Ethiopia IE"
cd "$filepath/Data/Fishnets/Fishnets STATA Datasets/panel_annual"

* Rob Computer
*global filepath "/Users/robmarty/Dropbox/Ethiopia IE"
*cd "$filepath/Data/Fishnets/Fishnets STATA Datasets/panel_annual"

use fishnet.5x5km.extraction_panelannual.dta, clear

set more off, permanently

*label variables
foreach v in ma_access_travel_time ma_euc_dist distance_addis distrd_all distrd_type_trunk distrd_rdclss_urrap soilmoist {
	gen ln_`v'=ln(`v')
}
label var ln_ma_access_travel_time "log MA cost-dist"
label var ln_ma_euc_dist "log MA euclidian dist"
label var ln_distance_addis "log dist to Addis Ababa"
label var ln_distrd_all "log dist to any road"
label var ln_distrd_type_trunk "log dist to trunk road"
label var ln_distrd_rdclss_urrap "log dist to URRAP road"
label var ln_soilmoist "log soil moisture"	

label var soilmoist "soil moisture"
label var mean_temp_maxavg "max avg temp"						  

*assign land cover types							  
gen gc_landcover_dummy =.
replace gc_landcover_dummy = 0 if (gc_vegetation_mean > gc_cropland_mean) & ///
								  (gc_vegetation_mean > gc_urban_mean)								  
replace gc_landcover_dummy = 1 if (gc_cropland_mean > gc_vegetation_mean) & ///
								  (gc_cropland_mean > gc_urban_mean)									  
replace gc_landcover_dummy = 2 if (gc_urban_mean > gc_vegetation_mean) & ///
								  (gc_urban_mean > gc_cropland_mean)
								  
replace gc_landcover_dummy = . if year == 2016 | year == 2017 // For some reason years > 2015 were getting values.
label var gc_landcover_dummy "landcover class"

label values gc_landcover_dummy gc
label define gc 0 "vegetation" ///
	1 "cropland" ///
	2 "urban", modify

gen gc_cropland_dummy = 0
replace gc_cropland_dummy = 1 if gc_landcover_dummy == 1
label var gc_cropland_dummy "majority cropland"

gen gc_urban_dummy = 0
replace gc_urban_dummy = 1 if gc_landcover_dummy == 2
label var gc_urban_dummy "majority urban"

cd "$filepath//Code/Analysis"
saveold data_panel, replace

*****ANALYSIS
cd "$filepath/Code/Analysis"
use data_panel, clear

global Z "mean_temp_maxavg soilmoist"
global V "gc_cropland_mean gc_urban_mean gc_landcover_dummy ln_distrd_all ln_distrd_trunk ln_ma_euc_time ln_ma_access_travel_time"
*ssc install estout, replace
tab gc_landcover_dummy
xtset id year
rename ln_ma_access_travel_time ln_ma_time

*ols
quietly eststo ols1_crops: xtreg gc_cropland_mean ln_distrd_all $Z, fe robust
quietly eststo ols2_crops: xtreg gc_cropland_mean ln_distrd_type_trunk $Z, fe robust
quietly eststo ols3_crops: xtreg gc_cropland_mean ln_ma_time $Z, fe robust
*quietly eststo ols4_crops: xtreg gc_cropland_mean ln_ma_euc_dist $Z, fe robust

quietly eststo ols1_urban: xtreg gc_urban_mean ln_distrd_all $Z, fe robust
quietly eststo ols2_urban: xtreg gc_urban_mean ln_distrd_type_trunk $Z, fe robust
quietly eststo ols3_urban: xtreg gc_urban_mean ln_ma_time $Z, fe robust
*quietly eststo ols4_urban: xtreg gc_urban_mean ln_ma_euc_dist $Z, fe robust

*logit
quietly eststo logit1_crops: xtlogit gc_cropland_dummy ln_distrd_all $Z, fe
quietly eststo logit2_crops: xtlogit gc_cropland_dummy ln_distrd_type_trunk $Z, fe
quietly eststo logit3_crops: xtlogit gc_cropland_dummy ln_ma_time $Z, fe
*quietly eststo logit4_crops: xtlogit gc_cropland_dummy ln_ma_euc_dist $Z, fe

*quietly eststo logit1_urban: xtlogit gc_urban_dummy ln_distrd_all $Z, fe --> redundant or inconsistent constraint
quietly eststo logit1_urban: xtlogit gc_urban_dummy ln_distrd_type_trunk $Z, fe
quietly eststo logit2_urban: xtlogit gc_urban_dummy ln_ma_euc_dist $Z, fe
quietly eststo logit3_urban: xtlogit gc_urban_dummy ln_ma_time $Z, fe

*multinomial (no fixed effects)
quietly eststo mlogit1: mlogit gc_landcover_dummy ln_distrd_all $Z, robust cluster(id)
quietly eststo mlogit2: mlogit gc_landcover_dummy ln_distrd_type_trunk $Z, robust cluster(id)
quietly eststo mlogit3: mlogit gc_landcover_dummy ln_ma_time $Z, robust cluster(id)
*quietly eststo mlogit4: mlogit gc_landcover_dummy ln_ma_euc_dist $Z, robust cluster(id)

*save tables
cd "$filepath/Code/Analysis"
esttab ols1_crops ols2_crops ols3_crops using panel_ols_cropland.tex, se ar2 label drop($Z) ///
	title(Impact of Roads on Agricultural Area, OLS Model) ///
	addnotes(Dates are for 1992 through 2017. OLS model with fixed effects and robust standard errors. The dependent variable is the percent of land devoted to growing crops (in 10km by 10km units). All regressions control for average daily maximum temperature and soil moisture.) replace
esttab ols1_urban ols2_urban ols3_urban using panel_ols_urban.tex, se ar2 label drop($Z) ///
	title(Impact of Roads on Urban Development, OLS Model) ///
	addnotes(Dates are for 1992 through 2017. OLS model with fixed effects and robust standard errors. The dependent variable is the percent of urban land (in 10km by 10km units). All regressions control for average daily maximum temperature and soil moisture.)  replace
esttab logit1_crops logit2_crops logit3_crops using panel_logit_cropland.tex, se ar2 label drop($Z) ///
	title(Impact of Roads on Agricultural Area, Logit Model) ///
	addnotes(Dates are for 1992 through 2017. Logit model with fixed effects and robust standard errors. The dependent variable is the percent of land devoted to growing crops (in 10km by 10km units). All regressions control for average daily maximum temperature and soil moisture.)  replace
esttab /*logit1_urban*/ logit2_urban logit3_urban using panel_logit_urban.tex, se ar2 label drop($Z) ///
	title(Impact of Roads on Urban Development, Logit Model) ///
	addnotes(Dates are for 1992 through 2017. Logit model with fixed effects and robust standard errors. The dependent variable is the percent of urban land (in 10km by 10km units). All regressions control for average daily maximum temperature and soil moisture.)  replace
esttab mlogit1 mlogit2 mlogit3 using panel_mlogit.tex, se ar2 label drop($Z) nobaselevels ///
	title(Impact of Roads on Land Use, Multinomial Logit Model) ///
	addnotes(Dates are for 1992 through 2017. Multinomial logit model with standard errors clustered at the parcel/unit level. The dependent variable is the land cover category, aggregated to urban, cropland or vegetation based on majority share in 10km by 10km units. All regressions control for average daily maximum temperature and soil moisture.)  replace

*excel
est table ols1_crops ols2_crops ols3_crops, eform star(.01 .05 .10) stats(N ll df_m chi2) b(%9.2f)
xml_tab ols1_crops ols2_crops ols3_crops, ///
	    constant(Constant) stats(F Fp N r2 r2_o) below format((S2110) (N2204)) cnames("") sheet("OLS Crops") ///
	    lines(SCOL_NAMES 4 COL_NAMES 4 LAST_ROW 4) replace save("estimation_results.xls") ///
		title(Impact of Roads on Agricultural Area, OLS Model)
		
est table ols1_urban ols2_urban ols3_urban, eform star(.01 .05 .10) stats(N ll df_m chi2) b(%9.2f)
xml_tab ols1_urban ols2_urban ols3_urban, ///
	    constant(Constant) stats(F Fp N r2 r2_o) below format((S2110) (N2204)) cnames("") sheet("OLS Urban") ///
	    lines(SCOL_NAMES 4 COL_NAMES 4 LAST_ROW 4) append save("estimation_results.xls") ///
		title(Impact of Roads on Urban Development, OLS Model)
		
est table logit1_crops logit2_crops logit3_crops, eform star(.01 .05 .10) stats(N ll df_m chi2) b(%9.2f)
xml_tab logit1_crops logit2_crops logit3_crops, ///
	    constant(Constant) stats(F Fp N r2 r2_o) below format((S2110) (N2204)) cnames("") sheet("Logit Crops") ///
	    lines(SCOL_NAMES 4 COL_NAMES 4 LAST_ROW 4) append save("estimation_results.xls") ///
		title(Impact of Roads on Agricultural Area, Logit Model)
		
est table /*logit1_urban*/ logit2_urban logit3_urban, eform star(.01 .05 .10) stats(N ll df_m chi2) b(%9.2f) 
xml_tab /*logit1_urban*/ logit2_urban logit3_urban, ///
	    constant(Constant) stats(F Fp N r2 r2_o) below format((S2110) (N2204)) cnames("") sheet("Logit Urban") ///
	    lines(SCOL_NAMES 4 COL_NAMES 4 LAST_ROW 4) append save("estimation_results.xls") ///
		title(Impact of Roads on Urban Development, Logit Model)
		
est table mlogit1 mlogit2 mlogit3, eform star(.01 .05 .10) stats(N ll df_m chi2) b(%9.2f)
xml_tab mlogit1 mlogit2 mlogit3, ///
	    constant(Constant) stats(F Fp N r2 r2_o) below format((S2110) (N2204)) cnames("") sheet("Mlogit") ///
	    lines(SCOL_NAMES 4 COL_NAMES 4 LAST_ROW 4) append save("estimation_results.xls") ///
		title(Impact of Roads on Land Use, Multinomial Logit Model)

*****************************************************************************
*			Expressway Difference in Difference
*****************************************************************************

cd "$filepath/Code/Analysis"
use data_panel, clear

global M "distrd_addis_adama gc_cropland_mean gc_urban_mean gc_vegetation_mean gc_landcover_dummy"
keep year id $M

reshape wide $M, i(id) j(year)
keep if distrd_addis_adama2015 <= 60
gen treated1 = (distrd_addis_adama2015 < 40) & !missing(distrd_addis_adama2015)
gen treated2 = (distrd_addis_adama2015 < 20) & !missing(distrd_addis_adama2015)

reshape long $M, i(id) j(year)
keep if year == 2009 | 2015

gen time = (year>2014) & !missing(year)
gen did1 = time*treated1
gen did2 = time*treated2

label var time "Post"
label var did1 "DiD"
label var did2 "DiD"
label var treated1 "Treated"
label var treated2 "Treated"

cd "$filepath/Code/Analysis"
save data_did, replace

****ANALYSIS
cd "$filepath/Code/Analysis"
use data_did, clear

*ssc install estout
quietly eststo did1_crops: reg gc_cropland_mean time treated1 did1, robust
quietly eststo did1_urban: reg gc_urban_mean time treated1 did1, robust

keep if distrd_addis_adama <= 40
quietly eststo did2_crops: reg gc_cropland_mean time treated2 did2, robust  
quietly eststo did2_urban: reg gc_urban_mean time treated2 did2, robust

esttab did1_crops did1_urban using did_buffer60km.tex, se ar2 label ///
	title(Impact of Roads on Land Use, 60km Expressway Buffer) ///
	addnotes(Data is for 2009 and 2015 (the Addis Ababa-Adama expressway was built between 2010 and 2014). Post is defined as 2015, the treatment group are land parcels within an inner (20km buffer) of the expressway and the control group are land parcels within an outer (20-40km) buffer of the expressway. The dependent variable is the percentage of land devoted to growing crops (in 10km by 10km units).) replace
esttab did2_crops did2_urban using did_buffer40km.tex, se ar2 label ///
	title(Impact of Roads on Land Use, 40km Expressway Buffer) ///
	addnotes(Data is for 2009 and 2015 (the Addis Ababa-Adama expressway was built between 2010 and 2014). Post is defined as 2015, the treatment group are land parcels within an inner (40km buffer) of the expressway and the control group are land parcels within an outer (40-60km) buffer of the expressway. The dependent variable is the percentage of land devoted to growing crops (in 10km by 10km units).) replace
