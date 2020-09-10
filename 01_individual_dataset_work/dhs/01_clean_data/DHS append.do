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
	
	global dhs_rawdata			"$onedrive/Data/RawData/DHS"
	
	global dhs_2000				"$dhs_rawdata/Ethiopia 2000"
	
	global dhs_2005				"$dhs_rawdata/Ethiopia 2005"
	
	global dhs_2011				"$dhs_rawdata/Ethiopia 2011"
	
	global dhs_2016				"$dhs_rawdata/Ethiopia 2016"
	
	global dhs_intermediate		"$onedrive/Data/IntermediateData/DHS"
	
	global dhs_finaldata		"$onedrive/Data/FinalData/DHS"

	
	
	
	
	
*-------------------------------------------------------------------------------
*...............................................................................
* 									DHS 2000 Survey
*...............................................................................
*-------------------------------------------------------------------------------


*---------- Preparing Geographic Data to merge with Individual Recode data
	cd "$dhs_intermediate"
	shp2dta using "$dhs_rawdata/Ethiopia 2000/ETGE42FL/ETGE42FL.shp" ///
					, database("etdhs2000_db") coordinates("etdhs2000_coord")
	
	use "$dhs_intermediate/etdhs2000_db.dta", clear
	
	rename *, lower
	
	gen v001 = dhsclust
	
	label variable dhsid "DHS ID Code"
	label variable dhscc "DHS Country Code"
	label variable dhsyear "Year of Data Collection"
	label variable dhsclust "Cluster ID Number"
	label variable ccfips "FIPS Country Code"
	label variable adm1fips "FIPS Division Code"
	label variable adm1fipsna "FIPS Division Name"
	label variable adm1salbna "SALB Name"
	label variable adm1salbco "SALB Code"
	label variable adm1dhs "Admin Region Code"
	label variable adm1name "Admin Region Name"
	label variable dhsregco "DHS Sampling Region Code"
	label variable dhsregna "DHS Sampling Region Name"
	label variable source "Source of Coordinates"
	label variable urban_rura "Urban or Rural, DHS"
	label variable latnum "Lattitude"
	label variable longnum "Longitude"
	label variable alt_gps "Cluster Elevation from GPS Reciever (in meters)"
	label variable alt_dem "Cluster Elevation from SRTM DEM (in meters)"
	label variable datum "CRS & Geo Datum"

	save "$dhs_intermediate/etdhs2000_db.dta", replace
	

*----------  Individual Recode data	
	use "$dhs_rawdata/Ethiopia 2000/ETIR41DT/ETIR41FL.DTA", clear

	keep caseid v000 v001 v002 v005 v006 v007 v008 v025 v026 v102 v140 v141
	

*----------  Merging Individual Recode data with Geographic data	
	merge m:m v001 using "$dhs_intermediate/etdhs2000_db.dta"
	
	save "$dhs_intermediate/etdhs2000_geomerged.dta", replace

	
	
	
	
	
	
*-------------------------------------------------------------------------------
*...............................................................................
* 									DHS 2005 Survey
*...............................................................................
*-------------------------------------------------------------------------------

*---------- Preparing Geographic Data to merge with Individual Recode data
	cd "$dhs_intermediate"
	shp2dta using "$dhs_rawdata/Ethiopia 2005/ETGE52FL/ETGE52FL.shp" ///
					, database("etdhs2005_db") coordinates("etdhs2005_coord")
	
	
	use "$dhs_intermediate/etdhs2005_db.dta", clear
	
	rename *, lower
	
	gen v001 = dhsclust
	
	label variable dhsid "DHS ID Code"
	label variable dhscc "DHS Country Code"
	label variable dhsyear "Year of Data Collection"
	label variable dhsclust "Cluster ID Number"
	label variable ccfips "FIPS Country Code"
	label variable adm1fips "FIPS Division Code"
	label variable adm1fipsna "FIPS Division Name"
	label variable adm1salbna "SALB Name"
	label variable adm1salbco "SALB Code"
	label variable adm1dhs "Admin Region Code"
	label variable adm1name "Admin Region Name"
	label variable dhsregco "DHS Sampling Region Code"
	label variable dhsregna "DHS Sampling Region Name"
	label variable source "Source of Coordinates"
	label variable urban_rura "Urban or Rural, DHS"
	label variable latnum "Lattitude"
	label variable longnum "Longitude"
	label variable alt_gps "Cluster Elevation from GPS Reciever (in meters)"
	label variable alt_dem "Cluster Elevation from SRTM DEM (in meters)"
	label variable datum "CRS & Geo Datum"

	save "$dhs_intermediate/etdhs2005_db.dta", replace	

	
*----------  Individual Recode data	
	use "$dhs_rawdata/Ethiopia 2005/ETIR51DT/ETIR51FL.DTA", clear

	keep caseid v000 v001 v002 v005 v006 v007 v008 v025 v026 v102 v140 v141
	

*----------  Merging Individual Recode data with Geographic data	
	merge m:m v001 using "$dhs_intermediate/etdhs2005_db.dta"
	
	save "$dhs_intermediate/etdhs2005_geomerged.dta", replace	


	
	
	
	
	
*-------------------------------------------------------------------------------
*...............................................................................
* 									DHS 2011 Survey
*...............................................................................
*-------------------------------------------------------------------------------

*---------- Preparing Geographic Data to merge with Individual Recode data
	cd "$dhs_intermediate"
	shp2dta using "$dhs_rawdata/Ethiopia 2011/ETGE61FL/ETGE61FL.shp" ///
					, database("etdhs2011_db") coordinates("etdhs2011_coord")
	
	use "$dhs_intermediate/etdhs2011_db.dta", clear
	
	rename *, lower
	
	gen v001 = dhsclust
	
	label variable dhsid "DHS ID Code"
	label variable dhscc "DHS Country Code"
	label variable dhsyear "Year of Data Collection"
	label variable dhsclust "Cluster ID Number"
	label variable ccfips "FIPS Country Code"
	label variable adm1fips "FIPS Division Code"
	label variable adm1fipsna "FIPS Division Name"
	label variable adm1salbna "SALB Name"
	label variable adm1salbco "SALB Code"
	label variable adm1dhs "Admin Region Code"
	label variable adm1name "Admin Region Name"
	label variable dhsregco "DHS Sampling Region Code"
	label variable dhsregna "DHS Sampling Region Name"
	label variable source "Source of Coordinates"
	label variable urban_rura "Urban or Rural, DHS"
	label variable latnum "Lattitude"
	label variable longnum "Longitude"
	label variable alt_gps "Cluster Elevation from GPS Reciever (in meters)"
	label variable alt_dem "Cluster Elevation from SRTM DEM (in meters)"
	label variable datum "CRS & Geo Datum"

	save "$dhs_intermediate/etdhs2011_db.dta", replace


*----------  Individual Recode data	
	use "$dhs_rawdata/Ethiopia 2011/ETIR61DT/ETIR61FL.DTA", clear

	/* 	variables different here as compared to surveys from 2000-2005:
		v005
		v026
		v141
*/	

	keep caseid v000 v001 v002 v005 v006 v007 v008 v025 v026 v102 v140 v141
	

*----------  Merging Individual Recode data with Geographic data	
	merge m:m v001 using "$dhs_intermediate/etdhs2011_db.dta"
	
	save "$dhs_intermediate/etdhs2011_geomerged.dta", replace

	
	


	
	

*-------------------------------------------------------------------------------
*...............................................................................
* 									DHS 2016 Survey
*...............................................................................
*-------------------------------------------------------------------------------


*---------- Preparing Geographic Data to merge with Individual Recode data
	cd "$dhs_intermediate"
	shp2dta using "$dhs_rawdata/Ethiopia 2016/ETGE71FL/ETGE71FL.shp" ///
					, database("etdhs2016_db") coordinates("etdhs2016_coord")
	
	use "$dhs_intermediate/etdhs2016_db.dta", clear
	
	rename *, lower
	
	gen v001 = dhsclust
	
	label variable dhsid "DHS ID Code"
	label variable dhscc "DHS Country Code"
	label variable dhsyear "Year of Data Collection"
	label variable dhsclust "Cluster ID Number"
	label variable ccfips "FIPS Country Code"
	label variable adm1fips "FIPS Division Code"
	label variable adm1fipsna "FIPS Division Name"
	label variable adm1salbna "SALB Name"
	label variable adm1salbco "SALB Code"
	label variable adm1dhs "Admin Region Code"
	label variable adm1name "Admin Region Name"
	label variable dhsregco "DHS Sampling Region Code"
	label variable dhsregna "DHS Sampling Region Name"
	label variable source "Source of Coordinates"
	label variable urban_rura "Urban or Rural, DHS"
	label variable latnum "Lattitude"
	label variable longnum "Longitude"
	label variable alt_gps "Cluster Elevation from GPS Reciever (in meters)"
	label variable alt_dem "Cluster Elevation from SRTM DEM (in meters)"
	label variable datum "CRS & Geo Datum"

	save "$dhs_intermediate/etdhs2016_db.dta", replace
	

	
*----------  Individual Recode data	
	clear
	set maxvar 120000
	use "$dhs_rawdata/Ethiopia 2016/ETIR71DT/ETIR71FL.DTA", clear

	
/* variables different here as compared to surveys from 2000-2005:
	v005
	v026
	v141
*/	
	
	
	keep caseid v000 v001 v002 v005 v006 v007 v008 v025 v026 v102 v140 v141
	

*----------  Merging Individual Recode data with Geographic data	
	merge m:m v001 using "$dhs_intermediate/etdhs2016_db.dta"
	
	save "$dhs_intermediate/etdhs2016_geomerged.dta", replace

	
	
	
	
*-------------------------------------------------------------------------------
*...............................................................................
* 							Creating a Panel DHS dataset
*...............................................................................
*-------------------------------------------------------------------------------

	use "$dhs_intermediate/etdhs2000_geomerged.dta", clear
	
	append using "$dhs_intermediate/etdhs2005_geomerged.dta"
	append using "$dhs_intermediate/etdhs2011_geomerged.dta"
	append using "$dhs_intermediate/etdhs2016_geomerged.dta"
	
	save "$dhs_finaldata/dhs_panel_2000_2016.dta", replace
