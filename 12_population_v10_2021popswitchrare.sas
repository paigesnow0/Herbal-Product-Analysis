*Population for Tableau;
*Paige Snow paige.snow@dhsoha.state.or.us;
*OHA CRRU;
*6/3/2022;

%let year = 2021;
%let lastyear = 2020;
%let PSU_year = 2019;
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*PSU rarest race */
proc import datafile = "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Population\PSU\Results_RarestRace_2020_5year_2022-07-29.xlsx"
	out= PSU_rare_total_imp 
	dbms=xlsx replace; 
	sheet = "total_ombrr";
	getnames=yes; 
	*guessingrows = 5000;
run;

data PSU_rare_total_imp;
	keep county AIAN_new Asian Black Hispanic NHPI White Other Total;
	retain county AIAN_new Asian Black Hispanic NHPI White Other Total;
	set PSU_rare_total_imp;
	county = put(fips, county.);
	AIAN_new = input(AIAN, comma15.);
	drop AIAN;
	rename AIAN_new = AIAN;
	format AIAN_new comma15.;
	attrib _all_ label='';
run;

proc contents data = PSU_rare_total_imp order = varnum;
run;


proc import datafile = "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Population\PSU\Results_RarestRace_2020_5year_2022-07-29.xlsx"
	out= PSU_rare_adult_imp 
	dbms=xlsx replace; 
	sheet = "vap_ombrr";
	getnames=yes; 
	*guessingrows = 5000;
run;

data PSU_rare_adult_imp;
	keep county AIAN_new Asian Black Hispanic NHPI White Other Total;
	retain county AIAN_new Asian Black Hispanic NHPI White Other Total;
	set PSU_rare_adult_imp;
	county = put(fips, county.);
	AIAN_new = input(AIAN, comma15.);
	drop AIAN;
	rename AIAN_new = AIAN;
	format AIAN_new comma15.;
	attrib _all_ label='';
run;

proc contents data = PSU_rare_adult_imp order = varnum;
run;

proc import datafile = "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Population\PSU\Results_RarestRace_2020_5year_2022-07-29.xlsx"
	out= PSU_rare_age1 
	dbms=xlsx replace; 
	sheet = "age_system1_ombrr";
	getnames=yes; 
	*guessingrows = 5000;
run;

data PSU_rare_age1;
	keep county rarest_race _0_4_new _5_11 _12_17 _18_19 _20_49 _50_64 _65_ Total;
	retain county rarest_race _0_4_new _5_11 _12_17 _18_19 _20_49 _50_64 _65_ Total;
	set PSU_rare_age1;
	county = put(fips, county.);
	rarest_race = ombrr;
	_0_4_new = input(_0_4, comma15.);
	drop _0_4;
	rename _0_4_new = _0_4;
	format _0_4_new comma15.;
	attrib _all_ label='';
	if county = '' then delete;
run;

proc contents data = PSU_rare_age1 order = varnum;
run;

proc import datafile = "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Population\PSU\Results_RarestRace_2020_5year_2022-07-29.xlsx"
	out= PSU_rare_age2 
	dbms=xlsx replace; 
	sheet = "age_system2_ombrr";
	getnames=yes; 
	*guessingrows = 5000;
run;

data PSU_rare_age2;
	keep county rarest_race _0_9_new _10_19 _20_29 _30_39 _40_49 _50_59 _60_69 _70_79 _80_ Total;
	retain county rarest_race _0_9_new _10_19 _20_29 _30_39 _40_49 _50_59 _60_69 _70_79 _80_ Total;
	set PSU_rare_age2;
	county = put(fips, county.);
	rarest_race = ombrr;
	_0_9_new = input(_0_9, comma15.);
	drop _0_9;
	rename _0_9_new = _0_9;
	format _0_9_new comma15.;
	attrib _all_ label='';
	if county = '' then delete;
run;

proc contents data = PSU_rare_age2 order = varnum;
run;


/*PSU sex & age */
proc import datafile = "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Population\age_county_pop_&year..csv"
	out= pop_import 
	dbms=csv replace; 
	getnames=yes; 
	guessingrows = 5000;
run;

/* Rarest race from Marjorie */
/*
proc import datafile = "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Population\rarest_race_&PSU_year..csv"
	out= rarest_race 
	dbms=csv replace; 
	getnames=yes; 
	guessingrows = 5000;
run;
*/

proc import datafile = "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Population\sex_pop_&year..csv"
	out= sex_pop 
	dbms=csv replace; 
	getnames=yes; 
	guessingrows = 5000;
run;

proc import datafile = "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Population\regions.csv"
	out= region
	dbms=csv replace; 
	getnames=yes; 
	guessingrows = 5000;
run;

/* From Census DP05 total population for ZCTAs codes*/
proc import datafile = "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Population\Zip Code\OregonZipCodes &lastyear..csv"
	out= oregonzips_imp
	dbms=csv replace; 
	getnames=yes; 
	guessingrows = 5000;
run;


/* From https://udsmapper.org/zip-code-to-zcta-crosswalk/ */
proc import datafile = "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Population\Zip Code\Zip_to_zcta_crosswalk_&lastyear..csv"
	out= zip_zcta_imp
	dbms=csv replace; 
	getnames=yes; 
	guessingrows = 5000;
run;

/* From data.census.gov, for diverse rank of zip code */
proc import datafile = "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Population\Zip Code\ZIP explore_nonwhite &lastyear..csv"
	out= zip_nonwhite_imp
	dbms=csv replace; 
	getnames=yes; 
	guessingrows = 5000;
run;

/* From data.census.gov, for OMB demographic estimates */
proc import datafile = "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Population\CDC_census_RaceEth\ACSDP5Y&lastyear..DP05_4SAS.csv"
	out= OMB_demos_imp
	dbms=csv replace; 
	getnames=yes; 
	guessingrows = 5000;
run;

/*From Marjorie*/
/*
data re_PUMA;
	set pop.SAS_201519ORWA_Indiv4v1Final;
	if ST = 41 then output;
run;

proc contents data = re_PUMA order = varnum;
run;
*/

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/* 
Process county estimates for rarest group from PSU to get statewide estimates.
Takes data from wide format to long format.
Transpose, create DemographicCategories to match vax and case data, then aggregate.
First do adults, then total population.
*/

proc sort data = PSU_rare_adult_imp;
by county;
run;

proc transpose data=PSU_rare_adult_imp out=PSU_rare_adult_long;
  by county;
run;


data PSU_rare_adult_long;
	set PSU_rare_adult_long;
	format DemographicType DemographicCategory $40. county $20.;
	attrib _all_ label='';
	if col1 = . then delete;
  	population18 = Col1;
	DemographicType = 'Race';
	if _NAME_ = 'AIAN' then DemographicCategory = 'AI/AN';
	if _NAME_ = 'Total' then DemographicCategory = 'All Oregonians';
	if _NAME_ = 'Asian' then DemographicCategory = 'Asian';
	if _NAME_ = 'Black' then DemographicCategory = 'Black';
	if _NAME_ = 'Hispanic' then DemographicCategory = 'Hispanic';
	if _NAME_ = 'NHPI' then DemographicCategory = 'NH/PI';
	if _NAME_ = 'Other' then DemographicCategory = 'Other Race';
	if _NAME_ = 'White' then DemographicCategory = 'White';
	drop _NAME_ Col1 _label_;
run;

********************;
proc sort data = PSU_rare_total_imp;
by county;
run;

proc transpose data=PSU_rare_total_imp out=PSU_rare_total_long;
  by county;
run;

data PSU_rare_total_long;
	set PSU_rare_total_long;
	format DemographicType DemographicCategory $40. county $20.;
  	population = Col1;
	attrib _all_ label='';
	if col1 = . then delete;
	DemographicType = 'Race';
	if _NAME_ = 'AIAN' then DemographicCategory = 'AI/AN';
	if _NAME_ = 'Total' then DemographicCategory = 'All Oregonians';
	if _NAME_ = 'Asian' then DemographicCategory = 'Asian';
	if _NAME_ = 'Black' then DemographicCategory = 'Black';
	if _NAME_ = 'Hispanic' then DemographicCategory = 'Hispanic';
	if _NAME_ = 'NHPI' then DemographicCategory = 'NH/PI';
	if _NAME_ = 'Other' then DemographicCategory = 'Other Race';
	if _NAME_ = 'White' then DemographicCategory = 'White';
	drop _NAME_ Col1 _label_;
run;

*********************;

proc sort data = PSU_rare_adult_long;
by county DemographicType DemographicCategory;
run;

proc sort data = PSU_rare_total_long;
by county DemographicType DemographicCategory;
run;

data PSU_rare_county;
	merge PSU_rare_total_long PSU_rare_adult_long;
	by county DemographicType DemographicCategory;
	if county = 'Oregon' then delete;
	if DemographicCategory = 'All Oregonians' then delete;
run;
data PSU_rare_state;
	merge PSU_rare_total_long PSU_rare_adult_long;
	by county DemographicType DemographicCategory;
	if county ^= 'Oregon' then delete;
	if DemographicCategory = 'All Oregonians' then delete;
run;

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/* 
Use PSU rarest race by age data
*/
proc sort data = Psu_rare_age1;
by county rarest_race;
run;

proc transpose data=Psu_rare_age1 out=Psu_rare_age1_long;
  by county rarest_race;
run;

data Psu_rare_age1_long;
	keep DemographicType DemographicCategory age_group county population;
	retain DemographicType DemographicCategory age_group county population;
	set Psu_rare_age1_long;
	format DemographicType DemographicCategory $40. county age_group $20.;
  	population = Col1;
	attrib _all_ label='';
	if col1 = . then delete;
	DemographicType = 'Race';
	if rarest_race = 'AIAN' then DemographicCategory = 'AI/AN';
	if rarest_race = 'Total' then DemographicCategory = 'All Oregonians';
	if rarest_race = 'Asian' then DemographicCategory = 'Asian';
	if rarest_race = 'Black' then DemographicCategory = 'Black';
	if rarest_race = 'Hispanic' then DemographicCategory = 'Hispanic';
	if rarest_race = 'NHPI' then DemographicCategory = 'NH/PI';
	if rarest_race = 'Other' then DemographicCategory = 'Other Race';
	if rarest_race = 'White' then DemographicCategory = 'White';

	if _NAME_ = '_0_4' then Age_Group = '0 to 4';
	if _NAME_ = '_12_17' then Age_Group = '12 to 17';
	if _NAME_ = '_18_19' then Age_Group = '18 to 19';
	if _NAME_ = '_20_49' then Age_Group = '20 to 49';
	if _NAME_ = '_50_64' then Age_Group = '50 to 64';
	if _NAME_ = '_5_11' then Age_Group = '5 to 11';
	if _NAME_ = '_65_' then Age_Group = '65+';
	if _NAME_ = 'Total' then Age_Group = 'All Ages';

run;


/***/
proc sort data = Psu_rare_age2;
by county rarest_race;
run;

proc transpose data=Psu_rare_age2 out=Psu_rare_age2_long;
  by county rarest_race;
run;

data Psu_rare_age2_long;
	keep DemographicType DemographicCategory age_group county population;
	retain DemographicType DemographicCategory age_group county population;
	set Psu_rare_age2_long;
	format DemographicType DemographicCategory $40. county age_group $20.;
  	population = Col1;
	attrib _all_ label='';
	if col1 = . then delete;
	DemographicType = 'Race';
	if rarest_race = 'AIAN' then DemographicCategory = 'AI/AN';
	if rarest_race = 'Total' then DemographicCategory = 'All Oregonians';
	if rarest_race = 'Asian' then DemographicCategory = 'Asian';
	if rarest_race = 'Black' then DemographicCategory = 'Black';
	if rarest_race = 'Hispanic' then DemographicCategory = 'Hispanic';
	if rarest_race = 'NHPI' then DemographicCategory = 'NH/PI';
	if rarest_race = 'Other' then DemographicCategory = 'Other Race';
	if rarest_race = 'White' then DemographicCategory = 'White';

	if _NAME_ = '_0_9' then Age_Group = '0 to 9';
	if _NAME_ = '_10_19' then Age_Group = '10 to 19';
	if _NAME_ = '_20_29' then Age_Group = '20 to 29';
	if _NAME_ = '_30_39' then Age_Group = '30 to 39';
	if _NAME_ = '_40_49' then Age_Group = '40 to 49';
	if _NAME_ = '_50_59' then Age_Group = '50 to 59';
	if _NAME_ = '_60_69' then Age_Group = '60 to 69';
	if _NAME_ = '_70_79' then Age_Group = '70 to 79';
	if _NAME_ = '_80_' then Age_Group = '80+';
	if _NAME_ = 'Total' then Age_Group = 'All Ages';
run;

data race_age_pop;
	set PSU_rare_age1_long PSU_rare_age2_long;
run;

proc sort data = race_age_pop;
	by DemographicType Age_Group DemographicCategory County;
run;

data pop.race_age_pop_&year;
	set race_age_pop;
run;
 
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/* Create a ZCTA file, where ZCTAs haven't been aggregated yet. */

/*
The format from the OregonZips and zip_nonwhite import are a little weird, I did it that way because it would be easier to delete rows in SAS
No longer have a reliable count of zips per ZCTA. This was from Vivian's calculations, and I can't find the script to replicate it.
Leaving the variable in, where it == 1 for all ZCTAs, because it is used as a filter for a merge later.
*/
data OregonZips;
	keep ZCTA ZCTA_pop ZCTA_numb_zips ;
	set OregonZips_imp;
	if find(label, 'Estimate') then delete;
	if Total_population = . then delete;
	ZCTA = input(strip(tranwrd(label, 'ZCTA5 ', '')), 8.);
	ZCTA_pop = total_population;
	ZCTA_numb_zips = 1;
run;

data zip_nonwhite_imp;
	set zip_nonwhite_imp;
	if ZCTA = . then delete;
run;

/* Process ZCTA, ZIP crosswalk */
data zip_zcta;
	keep ZCTA ZIP_CODE zip_type;
	retain ZCTA ZIP_CODE zip_type;
	set zip_zcta_imp;
	ZIP_city = po_name;
	if state = 'OR' then output;
run;

/*Sort and merge ZCTA files */

proc sort data = Oregonzips;
	by ZCTA; 
run;

proc sort data = zip_zcta;
	by ZCTA; 
run;

proc sort data = zip_nonwhite_imp;
	by ZCTA; 
run;

data zip_zcta_oregon;
	merge oregonzips zip_zcta zip_nonwhite_imp;
	by ZCTA;
	if zip_code = . then delete;
run;

proc print data = zip_zcta_oregon;
	where ZCTA_pop = .;
run;

/* Store in files */
data pop.zip_ZCTA_Oregon_&year;
	set zip_zcta_oregon;
	pop_nonwhite = ZCTA_pop*(1-percent_white/100);
	drop percent_white;
run;

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/* Get different populations by sex & age (5+, 12+, 18+, etc) */

data sex_pop;
	set sex_pop;
	format DemographicType DemographicCategory $40.;
	DemographicType = "Sex";
	DemographicCategory = sex;
	population5 = population - _0_to_4;
	population12 = population - _0_to_4 - _5_to_9 - 2/5*_10_to_14;
	population16 = population - _0_to_4 - _5_to_9 - _10_to_14 - 1/3*_15_to_17;
	population18 = population - _0_to_4 - _5_to_9 - _10_to_14 - _15_to_17;
	drop sex _0_to_4 _5_to_9 _10_to_14 _15_to_17;
run;

data sex_pop_state;
	set sex_pop;
	if county = 'OREGON' then output;
run;

data sex_pop_county;
	set sex_pop;
	if county ^= 'OREGON' then output;
run;

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/* Take the PSU age population estimates, create different populations by age (5+, 12+, 18+, etc) and make the dataset long */

/*Create macro to process one column at a time. Some columns that are split up will be run twice (like 12+)*/
%macro age_long (name = ,numb = , var = );
data pop_&name;
	keep county age_cat &var;
	set pop_import;
	rename &var = population;
	age_cat = &numb;
run;
%mend;

%age_long (name = 1_, numb = -1, var = _0_to_4);
%age_long (name = 1a, numb = 1, var = _5_to_9);
%age_long (name = 1b, numb = 1, var = _10_to_14);
%age_long (name = 2a, numb = 2, var = _10_to_14);
%age_long (name = 2b, numb = 2, var = _15_to_17);
%age_long (name = 3a, numb = 3, var = _18_to_19);
%age_long (name = 4a, numb = 4, var = _20_to_24);
%age_long (name = 4b, numb = 4, var = _25_to_29);
%age_long (name = 5a, numb = 5, var = _30_to_34);
%age_long (name = 5b, numb = 5, var = _35_to_39);
%age_long (name = 6a, numb = 6, var = _40_to_44);
%age_long (name = 6b, numb = 6, var = _45_to_49);
%age_long (name = 7a, numb = 7, var = _50_to_54);
%age_long (name = 7b, numb = 7, var = _55_to_59);
%age_long (name = 8a, numb = 8, var = _60_to_64);
%age_long (name = 9a, numb = 9, var = _65_to_69);
%age_long (name = 10a, numb = 10, var = _70_to_74);
%age_long (name = 10b, numb = 10, var = _75_to_79);
%age_long (name = 11a, numb = 11, var = _80_to_84);
%age_long (name = 11b, numb = 11, var = _85_);
%age_long (name = 12a, numb = 12, var = Total);

/********************/
*Dis-aggregated age;
*Split 10 to 14 into 10 & 11 & 12 & 13 & 14;
data pop_1b;
	set pop_1b;
		population_new = population*2/5;
	drop population;
	rename population_new = population;
run;

data pop_2a;
	set pop_2a;
		population_new = population*3/5;
	drop population;
	rename population_new = population;
run;

/********************/
/*Stack each table to make the data long */

data pop_long;
	set	pop_1_
		pop_1a pop_1b 
		pop_2a pop_2b pop_3a 
		pop_4a pop_4b pop_5a pop_5b
		pop_6a pop_6b pop_7a pop_7b
		pop_8a pop_9a
		pop_10a pop_10b pop_11a pop_11b
		pop_12a;
run;

/*Aggregate by age_cat and county */
proc sql;
	create table pop_long_agg as
		select county,
				age_cat, 
				sum(population) as population
		from pop_long
		group by county, age_cat;
quit;

/*Apply the age format to appear in the table view */
data pop_long_agg;
	set pop_long_agg;
	format age_cat age_dis.;
run;

/*Create Demographic variables for merging */
data pop_long_by_age;
	set pop_long_agg;
	format DemographicType DemographicCategory $40.;
	DemographicType = "Age Groups";
	DemographicCategory = put(age_cat, age_dis.);
	if age_cat ^= 12 then output;
run;

/********************/
/*
Stack and aggregate some tables to make extra age categories that are more aggregated
This way we have both 10 year age bands, and larger age bands 
*/

data pop_long2549;
	set pop_4a pop_4b pop_5a pop_5b
		pop_6a pop_6b;
		age_cat2 = '20 to 49';
run;

data pop_long5064;
	set pop_7a pop_7b
		pop_8a;
		age_cat2 = '50 to 64';
run;

data pop_long65a;
	set pop_1_
		pop_1a pop_1b 
		pop_2a pop_2b pop_3a 
		pop_4a pop_4b pop_5a pop_5b
		pop_6a pop_6b pop_7a pop_7b
		pop_8a;
		age_cat2 = 'Under 65';
run;

data pop_long65b;
	set pop_9a
		pop_10a pop_10b pop_11a pop_11b;
		age_cat2 = '65+';
run;

data pop_long65;
	set pop_long2549 pop_long5064 pop_long65a pop_long65b;

run;

proc sql;
	create table pop_long_agg65 as
		select county,
				age_cat2, 
				sum(population) as population
		from pop_long65
		group by county, age_cat2;
quit;

proc print data = pop_long65;
	where county = 'BAKER';
run;


data pop_long_by_age65;
	set pop_long_agg65;
	format DemographicType DemographicCategory $40.;
	DemographicType = "Age Groups";
	DemographicCategory = age_cat2;
run;

/*******************/
/*Separate State and County Estimates */
data pop_by_age_state;
	set pop_long_by_age pop_long_by_age65;
	drop age_cat age_cat2;
	if county = 'OREGON' then output;
run;

data pop_by_age_county;
	set pop_long_by_age pop_long_by_age65;
	drop age_cat age_cat2;
	if county ^= 'OREGON' then output;
run;

/**********************************************************************************************************/
/**********************************************************************************************************/
/*Create columns for population of all ages, population5+, population12+, and population18+ */

data pop_long_all;
set pop_long_agg;
	DemographicType = "All Oregonians";
	if age_cat = 12 then output;
run;

/**********/

data pop_long5;
	set	pop_1a pop_1b 
		pop_2a pop_2b pop_3a 
		pop_4a pop_4b pop_5a pop_5b
		pop_6a pop_6b pop_7a pop_7b
		pop_8a pop_9a
		pop_10a pop_10b pop_11a pop_11b;
run;

proc sql;
	create table pop_long_agg5 as
		select county,
				sum(population) as population5
		from pop_long5
		group by county;
quit;

/**********/

data pop_long18;
	set pop_3a 
		pop_4a pop_4b pop_5a pop_5b
		pop_6a pop_6b pop_7a pop_7b
		pop_8a pop_9a
		pop_10a pop_10b pop_11a pop_11b;
run;

proc sql;
	create table pop_long_agg18 as
		select county,
				sum(population) as population18
		from pop_long18
		group by county;
quit;

/**********/

data pop_long12;
	set pop_2a pop_2b pop_3a 
		pop_4a pop_4b pop_5a pop_5b
		pop_6a pop_6b pop_7a pop_7b
		pop_8a pop_9a
		pop_10a pop_10b pop_11a pop_11b;
run;

proc sql;
	create table pop_long_agg12 as
		select county,
				sum(population) as population12
		from pop_long12
		group by county;
quit;

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/


data oregon;
	merge pop_long_all pop_long_agg18 pop_long_agg12 pop_long_agg5;
	by county;
	drop age_cat;
	if county = "OREGON" then output;
run;

data oregon_county;
	merge pop_long_all pop_long_agg18 pop_long_agg12 pop_long_agg5;
	by county;
	drop age_cat;
	if county ^= "OREGON" then output;
run;



/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*OMB demographics */
proc contents data = omb_demos_imp order = varnum;
run;

proc print data = omb_demos_imp;
	var label;
run;

data omb_demos;
	keep Label DemographicType DemographicCategory
		Baker_County__Oregon__Estimate
		Benton_County__Oregon__Estimate
		Clackamas_County__Oregon__Estim
		Clatsop_County__Oregon__Estimat
		Columbia_County__Oregon__Estima
		Coos_County__Oregon__Estimate
		Crook_County__Oregon__Estimate
		Curry_County__Oregon__Estimate
		Deschutes_County__Oregon__Estim
		Douglas_County__Oregon__Estimat
		Gilliam_County__Oregon__Estimat
		Grant_County__Oregon__Estimate
		Harney_County__Oregon__Estimate
		Hood_River_County__Oregon__Esti
		Jackson_County__Oregon__Estimat
		Jefferson_County__Oregon__Estim
		Josephine_County__Oregon__Estim
		Klamath_County__Oregon__Estimat
		Lake_County__Oregon__Estimate
		Lane_County__Oregon__Estimate
		Lincoln_County__Oregon__Estimat
		Linn_County__Oregon__Estimate
		Malheur_County__Oregon__Estimat
		Marion_County__Oregon__Estimate
		Morrow_County__Oregon__Estimate
		Multnomah_County__Oregon__Estim
		Polk_County__Oregon__Estimate
		Sherman_County__Oregon__Estimat
		Tillamook_County__Oregon__Estim
		Umatilla_County__Oregon__Estima
		Union_County__Oregon__Estimate
		Wallowa_County__Oregon__Estimat
		Wasco_County__Oregon__Estimate
		Washington_County__Oregon__Esti
		Wheeler_County__Oregon__Estimat
		Yamhill_County__Oregon__Estimat
		Oregon__Estimate
		;
	retain Label DemographicType DemographicCategory
		Baker_County__Oregon__Estimate
		Benton_County__Oregon__Estimate
		Clackamas_County__Oregon__Estim
		Clatsop_County__Oregon__Estimat
		Columbia_County__Oregon__Estima
		Coos_County__Oregon__Estimate
		Crook_County__Oregon__Estimate
		Curry_County__Oregon__Estimate
		Deschutes_County__Oregon__Estim
		Douglas_County__Oregon__Estimat
		Gilliam_County__Oregon__Estimat
		Grant_County__Oregon__Estimate
		Harney_County__Oregon__Estimate
		Hood_River_County__Oregon__Esti
		Jackson_County__Oregon__Estimat
		Jefferson_County__Oregon__Estim
		Josephine_County__Oregon__Estim
		Klamath_County__Oregon__Estimat
		Lake_County__Oregon__Estimate
		Lane_County__Oregon__Estimate
		Lincoln_County__Oregon__Estimat
		Linn_County__Oregon__Estimate
		Malheur_County__Oregon__Estimat
		Marion_County__Oregon__Estimate
		Morrow_County__Oregon__Estimate
		Multnomah_County__Oregon__Estim
		Polk_County__Oregon__Estimate
		Sherman_County__Oregon__Estimat
		Tillamook_County__Oregon__Estim
		Umatilla_County__Oregon__Estima
		Union_County__Oregon__Estimate
		Wallowa_County__Oregon__Estimat
		Wasco_County__Oregon__Estimate
		Washington_County__Oregon__Esti
		Wheeler_County__Oregon__Estimat
		Yamhill_County__Oregon__Estimat
		Oregon__Estimate
		;
	set omb_demos_imp;
		if label in ('SEX AND AGE','        Male','        Female',
		'        Sex ratio (males per 100 females)','        Under 5 years','        5 to 9 years','        10 to 14 years',
		'        15 to 19 years','        20 to 24 years','        25 to 34 years','        35 to 44 years',
		'        45 to 54 years','        55 to 59 years','        60 to 64 years','        65 to 74 years',
		'        75 to 84 years','        85 years and over','        Median age (years)','        Under 18 years',
		'        16 years and over','        18 years and over',
		'        21 years and over','        62 years and over','        65 years and over',
		'        18 years and over','            Male','            Female','            Sex ratio (males per 100 females)',
		'        65 years and over','            Male','            Female','            Sex ratio (males per 100 females)',
		'RACE',
		'        One race','        One race','                Cherokee tribal grouping','                Chippewa tribal grouping',
		'                Navajo tribal grouping','                Sioux tribal grouping','                Asian Indian',
		'                Chinese','                Filipino','                Japanese','                Korean',
		'                Vietnamese','                Other Asian','                Native Hawaiian',
		'                Guamanian or Chamorro','                Samoan','                Other Pacific Islander',
		'            White and Black or African American','            White and American Indian and Alaska Native',
		'            White and Asian','            Black or African American and American Indian and Alaska Native',

		'Race alone or in combination with one or more other races',
		'HISPANIC OR LATINO AND RACE','            Mexican',
		'            Puerto Rican','            Cuban','            Other Hispanic or Latino',
		'                Two races including Some other race',
		'                Two races excluding Some other race, and Three or more races',
		'Total housing units','CITIZEN, VOTING AGE POPULATION','    Citizen, 18 and over population','        Male','        Female'
		) then delete;
		
		format DemographicType $20. DemographicCategory $50.;
				if label = '        Two or more races' then do; 
				DemographicType = 'Race OMB'; 
				DemographicCategory = 'Multiracial'; 
				end;
				if label = '            White' then do; 
				DemographicType = 'Race OMB'; 
				DemographicCategory = 'White'; 
				end;
				if label = '            Black or African American' then do;  
				DemographicType = 'Race OMB'; 
				DemographicCategory = 'Black'; 
				end;
				if label = '            American Indian and Alaska Native' then do; 
				DemographicType = 'Race OMB'; 
				DemographicCategory = 'AI/AN'; 
				end;
				if label = '            Asian' then do; 
				DemographicType = 'Race OMB'; 
				DemographicCategory = 'Asian'; 
				end;
				if label = '            Native Hawaiian and Other Pacific Islander' then do; 
				DemographicType = 'Race OMB'; 
				DemographicCategory = 'NH/PI'; 
				end;
				if label = '            Some other race' then do; 
				DemographicType = 'Race OMB'; 
				DemographicCategory = 'Other Race'; 
				end;
				if label = '        White' then do; 
				DemographicType = 'Race in combo'; 
				DemographicCategory = 'White'; 
				end;
				if label = '        Black or African American' then do; 
				DemographicType = 'Race in combo'; 
				DemographicCategory = 'Black'; 
				end;
				if label = '        American Indian and Alaska Native' then do; 
				DemographicType = 'Race in combo'; 
				DemographicCategory = 'AI/AN'; 
				end;
				if label = '        Asian' then do; 
				DemographicType = 'Race in combo'; 
				DemographicCategory = 'Asian'; 
				end;
				if label = '        Native Hawaiian and Other Pacific Islander' then do; 
				DemographicType = 'Race in combo'; 
				DemographicCategory = 'NH/PI'; 
				end;
				if label = '        Some other race' then do; 
				DemographicType = 'Race in combo'; 
				DemographicCategory = 'Other Race'; 
				end;
				if label = '        Hispanic or Latino (of any race)' then do; 
				DemographicType = 'Ethnicity OMB'; 
				DemographicCategory = 'Hispanic'; 
				end;
				if label = '        Not Hispanic or Latino' then do; 
				DemographicType = 'Ethnicity OMB'; 
				DemographicCategory = 'Non-Hispanic'; 
				end;
				if label = '            White alone' then do; 
				DemographicType = 'Race & Ethnicity OMB'; 
				DemographicCategory = 'Non-Hispanic White'; 
				end;
				if label = '            Black or African American alone' then do; 
				DemographicType = 'Race & Ethnicity OMB'; 
				DemographicCategory = 'Non-Hispanic Black'; 
				end;
				if label = '            American Indian and Alaska Native alone' then do; 
				DemographicType = 'Race & Ethnicity OMB'; 
				DemographicCategory = 'Non-Hispanic AI/AN'; 
				end;
				if label = '            Asian alone' then do; 
				DemographicType = 'Race & Ethnicity OMB'; 
				DemographicCategory = 'Non-Hispanic Asian'; 
				end;
				if label = '            Native Hawaiian and Other Pacific Islander alone' then do; 
				DemographicType = 'Race & Ethnicity OMB'; 
				DemographicCategory = 'Non-Hispanic NH/PI'; 
				end;
				if label = '            Some other race alone' then do; 
				DemographicType = 'Race & Ethnicity OMB'; 
				DemographicCategory = 'Non-Hispanic Other Race'; 
				end;
				if label = '            Two or more races' then do; 
				DemographicType = 'Race & Ethnicity OMB'; 
				DemographicCategory = 'Non-Hispanic Multiracial'; 
				end;
run;

data omb_demos_hisp;
	set omb_demos;
	DemographicType = 'Race & Ethnicity OMB';
	if DemographicCategory = 'Hispanic' then output;
run;

data omb_demos;
	set omb_demos omb_demos_hisp;
	drop label;
run;

proc sort data = omb_demos nodupkey;
	by DemographicType DemographicCategory;
run;

data omb_demos;
	set omb_demos;
	rename 
		Baker_County__Oregon__Estimate = baker
		Benton_County__Oregon__Estimate = benton
		Clackamas_County__Oregon__Estim = clackamas
		Clatsop_County__Oregon__Estimat = clatsop
		Columbia_County__Oregon__Estima = columbia
		Coos_County__Oregon__Estimate = coos
		Crook_County__Oregon__Estimate = crook
		Curry_County__Oregon__Estimate = curry
		Deschutes_County__Oregon__Estim = deschutes
		Douglas_County__Oregon__Estimat = douglas
		Gilliam_County__Oregon__Estimat = gilliam
		Grant_County__Oregon__Estimate = grant
		Harney_County__Oregon__Estimate = harney
		Hood_River_County__Oregon__Esti = hood_river
		Jackson_County__Oregon__Estimat = jackson
		Jefferson_County__Oregon__Estim = jefferson
		Josephine_County__Oregon__Estim = josephine
		Klamath_County__Oregon__Estimat = klamath
		Lake_County__Oregon__Estimate = lake
		Lane_County__Oregon__Estimate = lane
		Lincoln_County__Oregon__Estimat = lincoln
		Linn_County__Oregon__Estimate = linn
		Malheur_County__Oregon__Estimat = malheur
		Marion_County__Oregon__Estimate = marion
		Morrow_County__Oregon__Estimate = morrow
		Multnomah_County__Oregon__Estim = multnomah
		Polk_County__Oregon__Estimate = polk
		Sherman_County__Oregon__Estimat = sherman
		Tillamook_County__Oregon__Estim = tillamook
		Umatilla_County__Oregon__Estima = umatilla
		Union_County__Oregon__Estimate = union
		Wallowa_County__Oregon__Estimat = wallowa
		Wasco_County__Oregon__Estimate = wasco
		Washington_County__Oregon__Esti = washington
		Wheeler_County__Oregon__Estimat = wheeler
		Yamhill_County__Oregon__Estimat = yamhill
		Oregon__Estimate =  oregon;
run;

%let county = oregon;

%macro county (county = );
data omb_demoss_&county._num;
	keep DemographicType DemographicCategory county numerator;
	set omb_demos;
	format county $20.;
	numerator = &county;
	county = "&county.";
	county = upcase(county);
	if county = "HOOD_RIVER" then do; 
		county = "HOOD RIVER";
		end;
	if DemographicType ^= '' then output;
run;

data omb_demoss_&county._denom;
	keep  county denominator;
	set omb_demos;
	format county $20.;
	denominator = &county;
	county = "&county.";
	county = upcase(county);
	if county = "HOOD_RIVER" then do; 
		county = "HOOD RIVER";
		end;
	if DemographicType = '' then output;
run;

data omb_demoss_&county;
	merge omb_demoss_&county._num omb_demoss_&county._denom;
	by county;
	percent = numerator/denominator;
run;
%mend;

%county( county = Baker);
%county( county = Benton);
%county( county = Clackamas);
%county( county = Clatsop);
%county( county = Columbia);
%county( county = Coos);
%county( county = Crook);
%county( county = Curry);
%county( county = Deschutes);
%county( county = Douglas);
%county( county = Gilliam);
%county( county = Grant);
%county( county = Harney);
%county( county = Hood_River);
%county( county = Jackson);
%county( county = Jefferson);
%county( county = Josephine);
%county( county = Klamath);
%county( county = Lake);
%county( county = Lane);
%county( county = Lincoln);
%county( county = Linn);
%county( county = Malheur);
%county( county = Marion);
%county( county = Morrow);
%county( county = Multnomah);
%county( county = Polk);
%county( county = Sherman);
%county( county = Tillamook);
%county( county = Umatilla);
%county( county = Union);
%county( county = Wallowa);
%county( county = Wasco);
%county( county = Washington);
%county( county = Wheeler);
%county( county = Yamhill);
%county( county = Oregon);

data omb_demos_county;
	set omb_demoss_Baker
omb_demoss_Benton
omb_demoss_Clackamas
omb_demoss_Clatsop
omb_demoss_Columbia
omb_demoss_Coos
omb_demoss_Crook
omb_demoss_Curry
omb_demoss_Deschutes
omb_demoss_Douglas
omb_demoss_Gilliam
omb_demoss_Grant
omb_demoss_Harney
omb_demoss_Hood_River
omb_demoss_Jackson
omb_demoss_Jefferson
omb_demoss_Josephine
omb_demoss_Klamath
omb_demoss_Lake
omb_demoss_Lane
omb_demoss_Lincoln
omb_demoss_Linn
omb_demoss_Malheur
omb_demoss_Marion
omb_demoss_Morrow
omb_demoss_Multnomah
omb_demoss_Polk
omb_demoss_Sherman
omb_demoss_Tillamook
omb_demoss_Umatilla
omb_demoss_Union
omb_demoss_Wallowa
omb_demoss_Wasco
omb_demoss_Washington
omb_demoss_Wheeler
omb_demoss_Yamhill;
if DemographicType = 'Race in combo' then delete;
drop numerator denominator;
run;

data omb_demos_state;
	set omb_demoss_Oregon;
if DemographicType = 'Race in combo' then delete;
drop numerator denominator;
run;

/**/
data oregon_omb;
	set oregon;
	drop DemographicType population5 population12 population18;
run;

data oregon_county_omb;
	set oregon_county;
	drop DemographicType population5 population12 population18;
run;

data omb_demos_state2;
	merge omb_demos_state oregon_omb;
	by county;
	percent_new = percent;
	population_new = percent_new*population;

	drop percent;
	rename percent_new = percent 
			population = oregon_pop
			population_new = population ;
run;

data omb_demos_county2;
	merge omb_demos_county oregon_county_omb;
	by county;
	percent_new = percent;
	population_new = percent_new*population;

	drop percent;
	rename percent_new = percent 
			population = oregon_pop
			population_new = population ;
run;

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/

data state_demographics;
	keep DemographicType DemographicCategory oregon_pop oregon_pop12 oregon_pop18 percent population population5 population12 population18;
	retain DemographicType DemographicCategory oregon_pop oregon_pop12 oregon_pop18 percent population population5 population12 population18;
	set omb_demos_state2 oregon pop_by_age_state sex_pop_state PSU_rare_state ;
	if DemographicType = "All Oregonians" then DemographicCategory = "All Oregonians";
run;

data county_demographics;
	set oregon_county pop_by_age_county sex_pop_county PSU_rare_county;
	if DemographicType = "All Oregonians" then DemographicCategory = "All Oregonians";

run;


data county_demographics;
	keep DemographicType DemographicCategory County County_New Region oregon_pop oregon_pop12 oregon_pop18 percent population population12 population18;
	retain DemographicType DemographicCategory County County_New Region oregon_pop oregon_pop12 oregon_pop18 percent population population12 population18;
	set omb_demos_county2 county_demographics ;
	county_new = substr(county,1,1) || lowcase(substr(county, 2));
	if county_new = "Hood river" then do; county_new = "Hood River"; end;
	rename county_new = county;
run;

data county_demographics;
	set county_demographics;
	drop county;
	rename county_new = county;
run;

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/

proc sort data = state_demographics;
	by DemographicType DemographicCategory;
run;

proc sort data = county_demographics;
	by DemographicType DemographicCategory county;
run;

data pop.state_demog_PSU_&year;
	set  state_demographics;
run;

data pop.county_demog_PSU_&year;
	set  county_demographics;
run;
/*
proc export data = pop.state_demog_PSU_&year outfile="\\wpohaappl109\EPI\state_demographics_&year..csv" 
	dbms = csv replace;
run;

proc export data = pop.county_demog_PSU_&year outfile="\\wpohaappl109\EPI\county_demographics_&year..csv" 
	dbms = csv replace;
run;

*/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/

proc datasets library = work nolist;
	delete omb_demoss_:;
quit;
