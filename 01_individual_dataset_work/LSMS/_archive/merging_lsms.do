* Ethiopia IE
* Merging LSMS

* ------------------------------------------------------------------------------
*global folder_path "~/Dropbox/Ethiopia IE"
global folder_path "C:/Users/wb521633/Documents/Ethiopia"

* ------------------------------------------------------------------------------
* 2011
use "$folder_path/Data/LSMS/2011/Pub_ETH_HouseholdGeovariables_Y1.dta", clear
merge 1:1 household_id ea_id using "$folder_path/Data/LSMS/2011/cons_agg_w1.dta", nogen

keep household_id ea_id pw LAT_DD_MOD LON_DD_MOD food_cons_ann nonfood_cons_ann educ_cons_ann total_cons_ann nom_totcons_aeq cons_quint hh_size saq01
gen wave = 1

rename LAT_DD_MOD latitude
rename LON_DD_MOD longitude
rename pw prob_weight

tempfile data_2011
save `data_2011'

* ------------------------------------------------------------------------------
* 2013
use "$folder_path/Data/LSMS/2013/Pub_ETH_HouseholdGeovars_Y2.dta", clear
merge 1:1 household_id2 ea_id2 using "$folder_path/Data/LSMS/2013/cons_agg_w2.dta", nogen

keep household_id household_id2 ea_id ea_id2 pw2 lat_dd_mod lon_dd_mod food_cons_ann nonfood_cons_ann educ_cons_ann total_cons_ann nom_totcons_aeq cons_quint hh_size saq01
gen wave = 2

rename lat_dd_mod latitude
rename lon_dd_mod longitude
rename pw2 prob_weight

tempfile data_2013
save `data_2013'

* ------------------------------------------------------------------------------
* 2015
use "$folder_path/Data/LSMS/2015/Geovariables/ETH_HouseholdGeovars_y3.dta", clear
merge 1:1 household_id2 ea_id2 using "$folder_path/Data/LSMS/2015/Consumption Aggregate/cons_agg_w3.dta", nogen

keep household_id household_id2 ea_id ea_id2 pw_w3 lat_dd_mod lon_dd_mod food_cons_ann nonfood_cons_ann educ_cons_ann total_cons_ann nom_totcons_aeq cons_quint hh_size saq01
gen wave = 3

rename lat_dd_mod latitude
rename lon_dd_mod longitude
rename pw_w3 prob_weight

tempfile data_2015
save `data_2015'

* ------------------------------------------------------------------------------
* Households in All Datasets
use "$folder_path/Data/LSMS/2011/cons_agg_w1.dta", clear
keep household_id ea_id 
gen wave1 = 1
tempfile data_2011_commonHH
save `data_2011_commonHH'

use "$folder_path/Data/LSMS/2013/cons_agg_w2.dta", clear
keep household_id ea_id
gen wave2 = 1
drop if household_id == ""
tempfile data_2013_commonHH
save `data_2013_commonHH'

use "$folder_path/Data/LSMS/2015/Consumption Aggregate/cons_agg_w3.dta", clear
keep  household_id ea_id
gen wave3 = 1
drop if household_id == ""
duplicates drop household_id ea_id, force
tempfile data_2015_commonHH
save `data_2015_commonHH'


use `data_2011_commonHH', clear
merge 1:1 household_id ea_id using `data_2013_commonHH', nogen
merge 1:1 household_id ea_id using `data_2015_commonHH', nogen

keep if wave1 == 1 & wave2 == 1 & wave3 == 1
drop wave1 wave2 wave3
gen HH_in_all_waves = 1

tempfile common_HH
save `common_HH'

* ------------------------------------------------------------------------------
* Append Datasets
use `data_2011', clear
append using `data_2013'
append using `data_2015'

duplicates drop household_id ea_id wave, force
drop if household_id == ""

merge m:1 household_id ea_id using `common_HH', nogen
replace HH_in_all_waves = 0 if HH_in_all_waves == .

* ------------------------------------------------------------------------------
* Saving Dataset

replace household_id = "hh" + household_id if household_id != ""
replace ea_id = "ea" + ea_id if ea_id != ""

replace household_id2 = "hh" + household_id2 if household_id2 != ""
replace ea_id2 = "ea" + ea_id2 if ea_id2 != ""

save "$folder_path/Data/LSMS/Merged Data/lsms_merged.dta", replace





