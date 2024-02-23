*Daily Administrations for Tableau;
*Paige Snow paige.snow@dhsoha.state.or.us;
*CRRU Vax Dashboards;
*6/8/2022;

DM 'clear log; clear output;';
dm 'odsresults; clear';

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/

data admin_imms;
	set data.tableau_imms_boost_utd_&thismonday;
run;

data admin_county_pop;
	keep county population;
	set pop.county_demog_psu_2021;
	if DemographicType = 'All Oregonians' then output;
run;

data zip_ZCTA_Oregon;
	set pop.zip_ZCTA_Oregon_2021;
	*if zip_code = 97635 then delete;
run;

ods select attributes;
proc contents data = zip_ZCTA_Oregon;
run;

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
proc freq data = admin_imms;
	table vaccine*administrations;
	run;

/* Administrations by day */
proc sql;
	create table admin_over_time1 as
		select admin_date as date,
				vaccine as vaccine,
				recip_address_county as recip_county,
				sum(vaccinated) as vaccinated, 
				sum(needs_boost_now) as needs_boost_now,
				sum(people_count) as people_count
	from admin_imms
	group by date, vaccine, recip_county;
quit;

proc sql;
	create table admin_over_time2 as
		select admin_date as date,
				vaccine as vaccine,
				recip_address_county as recip_county,
				sum(administrations) as administrations
	from final_imms1
	group by date, vaccine, recip_county;
quit;

data admin_over_time;
	merge  admin_over_time1 admin_over_time2;
	by date vaccine recip_county;
run;

data admin_over_time;
	set admin_over_time;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "All Oregonians";
	DemographicCategory = "All Oregonians";
	county = put(recip_county, county.);
run;

proc sort data = admin_over_time;
	by county;
run;

proc sort data = admin_county_pop;
	by county;
run;

data admin_over_time_pop;
	merge admin_over_time admin_county_pop;
	by county;
run;

/****************/

ods output CrossTabFreqs = admin_race0; *SAS always outputs the table in a long format;
proc freq data = admin_imms;
	where people_count = 1;
	table recip_address_county*rarest_race / norow nocol nopercent missing;
	format recip_address_county county. rarest_race race.;
run;
ods output close;

data admin_race_denom;
	keep recip_address_county denominator; *denominator: checking if total number of people with one dose is >50 to check suppression criteria.;
	set admin_race0;
	denominator = frequency;
	if recip_address_county = -1 then delete;
	if recip_address_County = . then delete;
	if rarest_race = .  then output; *in the table output by the previos step, remember: . means total in the tables. So we only want to output the totals of each county;
run;

proc sort data = admin_race0;
by recip_address_county;
run;

data admin_race1;
	merge admin_race0 admin_race_Denom;
	by recip_address_county;
	if frequency < 10 then suppress = 1;
	if denominator < 50 then suppress = 1;
run;

proc sql;
	create table admin_race_suppress as
		select recip_address_county,
				sum(suppress) as suppress
		from admin_race1
		group by recip_Address_county;
quit;

data admin_race_suppress;
	set admin_race_suppress;
	if suppress > 0 then action = 'Suppressed';
	if recip_address_county = . then delete;
	if recip_address_county = -1 then delete;
run;

data admin_race2;
	merge admin_race1 admin_race_suppress;
	by recip_Address_county;
run;

data admin_race;
	keep DemographicType DemographicCategory county people_count action;
	retain DemographicType DemographicCategory county people_count action;
	set admin_race2;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Race";
	DemographicCategory = put(rarest_race, race.);
	county = put(recip_address_county, county.);
	people_count = frequency;
	if rarest_race = . then delete;
	if _TYPE_ = 01 then delete;
run;

ods output CrossTabFreqs = admin_age1;
proc freq data = admin_imms;
	where people_count = 1;
	table recip_address_county*age_cat / norow nocol nopercent missing;
	format recip_address_county county. age_cat age_agg.;
run;
ods output close;

data admin_age1;
	keep DemographicType DemographicCategory county people_count;
	set admin_age1;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Age Groups";
	DemographicCategory = put(age_cat, age_agg.);
	county = put(recip_address_county, county.);
	people_count = frequency;
	if age_cat = . then delete;
	if _TYPE_ = 01 then delete;
run;

ods output CrossTabFreqs = admin_age2;
proc freq data = admin_imms;
	where people_count = 1;
	table recip_address_county*age_cat / norow nocol nopercent missing;
	format recip_address_county county. age_cat age_dis.;
run;
ods output close;

data admin_age2;
	keep DemographicType DemographicCategory county people_count;
	set admin_age2;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Age Groups";
	DemographicCategory = put(age_cat, age_dis.);
	county = put(recip_address_county, county.);
	people_count = frequency;
	if age_cat = . then delete;
	if _TYPE_ = 01 then delete;
run;

data admin_age;
	set admin_age1 admin_age2;
run;

proc sort data = admin_age;
	by DemographicType DemographicCategory county;
run;

proc sort data = admin_age nodupkey;
	by DemographicType DemographicCategory county;
run;

ods output CrossTabFreqs = admin_sex;
proc freq data = admin_imms;
	where people_count = 1;
	table recip_address_county*recip_sex / norow nocol nopercent missing;
	format recip_address_county county.;
run;
ods output close;

data admin_sex;
	keep DemographicType DemographicCategory county people_count;
	set admin_sex;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Sex";
	DemographicCategory = recip_sex;
	county = put(recip_address_county, county.);
	people_count = frequency;
	if _TYPE_  in ('01','00', '10') then delete;
run;

data alert_admin;
	set admin_over_time_pop admin_race admin_age admin_sex;
	format reference todays_date mmddyy10. vaccine $17.;
	reference = &today - 4;
	todays_date = &today;
	people_count2 = people_count;
	if action = 'Suppressed' then do;
		people_count2 = .;
	end;
/*
	if vaccine = ' ' then do;
		vaccine = put(cvx, cvx.);
	end;
*/	
	if county = "Baker" AND DemographicType = "Race" then do;
		people_count2 = .;
	end;
	*if date = &today then delete;
	drop recip_county;
run;


data data.alert_admin;
	set alert_admin;
run;

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/

/*ZIP Code*/
/* 
Aggregate measures by zip code.
We're nulling zip codes that aren't in Oregon. 
*/

data admin_zip1;
	set admin_imms;
	if recip_address_county = . then do;
		recip_address_zip = .;
	end;

	if recip_address_zip < 97000 then do;
		recip_address_zip = .;
	end;
	if recip_address_zip > 97999 then do;
		recip_address_zip = .;
	end;
run;

proc sql;
   create table admin_zip_by_day1 as 
     select recip_address_zip,
			sum(people_count) as people_count,
			sum(booster) as booster,
			sum(booster_first) as booster_first,
			sum(booster_second) as booster_second,
			sum(booster_biv) as booster_biv,
			sum(needs_boost_now) as needs_boost_now,
 			sum(administrations) as administrations
           from admin_zip1 group by recip_address_zip;
quit;

proc sort data = admin_zip_by_day1 nodupkey; *might not be by day anymore. Keeping name cause no need to change;
	by recip_address_Zip;
run;

proc sql;
	create table admin_zip_day_check as
			select 
			sum(people_count) as people_count,
			sum(booster) as booster,
			sum(booster_first) as booster_first,
			sum(booster_second) as booster_second,
			sum(booster_biv) as booster_biv,
			sum(needs_boost_now) as needs_boost_now,
 			sum(administrations) as administrations
           from admin_zip_by_day1;
quit; *here we check that the total admins are correct;

/*
Do these numbers match the metrics and trends validation files?
No need to check this everyday, just when you are making changes to the script
*/
proc print data = admin_zip_day_check;
run;

data admin_zip_by_day2;
	set admin_zip_by_day1;
	*create new zip code variable to merge, keep old for checks;
	zip_code = recip_address_Zip;
run;

proc sort data = admin_zip_by_day2;
	by zip_code;
run;

/*
Check which zip codes/ZCTAs need to be aggregated with other zip codes/ZCTAs because the population is below 50.
This will change each year when you update the population estimates. 
*/
proc print data = zip_zcta_oregon;
	where ZCTA_pop < 50;
run;

proc sort data = zip_zcta_oregon;
	by zip_code;
run;

/* 
Look at a map of each county, figure out which ZCTA the zip code/ZCTA from the previous table should be aggregated with
	-should be within the same county and adjacent in geography. 
The zip code stays the same, but the ZCTA changes.
*/

data admin_zip_oregon0;
	merge admin_zip_by_day2 zip_zcta_oregon;
	by zip_code;
	if zcta = . then zip_code = .;
	*combine zip codes/ZCTAs that need to be suppressed with another ZCTA close by so they're suppressed.;
	if zip_code = 97033 then ZCTA = 97029;
	if zip_code = 97057 then ZCTA = 97001;
	if zip_code = 97208 then ZCTA = 97209;
	if zip_code = 97228 then ZCTA = 97209;
	if zip_code = 97711 then ZCTA = 97741;
	if zip_code = 97735 then ZCTA = 97638;
	if zip_code = 97752 then ZCTA = 97751;
	if zip_code = 97819 then ZCTA = 97837;
	if zip_code = 97908 then ZCTA = 97903;
	if zip_code = 97909 then ZCTA = 97903;
	if zip_code = 97920 then ZCTA = 97911;
run;

data admin_zip_oregon0;
	set admin_zip_oregon0;
	count + 1; *counting number of rows;
run;

proc sql;
	create table admin_zip_check as
		select sum(people_count) as people_count,
			sum(administrations) as administrations
		from admin_zip_oregon0;
	quit;

/* Check to make sure totals are still the same */
proc print data = admin_zip_check;
run;

/* 
Check how many rows have no ZCTA data.
Note that original zip codes from admin_imms don't exist per Google. 
*/
proc print data = admin_zip_oregon0;
	where ZCTA = .;
	var recip_address_Zip zip_code ZCTA ZCTA_pop count people_count administrations;
run;

/* 
Import a file from Steve which assigns the zip codes/ZCTAs to the right county.
This assigns the zip code/ZCTA to a county.
NOTE: I switched 97909 from Baker to Malheur. For 2020 data (published in 2021) 97909 was showen in Malheur. 
And when I check Google, 97909 is in Malheur. But for some reason in Steve's file it shows Baker. 
I'm gonna stick with 97909 = Malheur. 
Did the same with 97014 - switched to Hood River County instead of Multnomah County. 
Google says it's in both Hood River and Multnomah county, 
 but they are joined based on the zip to ZCTA crosswalk. And based on the map it makes sense to aggregate them. 
*/

proc import datafile = "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Population\Zip Code\SteveZipList2020.csv"
	out= zip_steve
	dbms=csv replace; 
	getnames=yes; 
	guessingrows = 5000;
run;

/*
Rename zip to zip_code for merge 
Create ZCTA_county variable and put it in right format
*/
data zip_steve;
	set zip_steve;
	rename zip = zip_code;
	drop id;
	ZCTA_county = substr(cnty,1,1) || lowcase(substr(cnty, 2));
	if ZCTA_county = "Hood river" then do; 
		ZCTA_county = "Hood River"; 
	end;
run;

proc sort data = admin_zip_oregon0;
	by zip_code;
run;

proc sort data = zip_steve;
	by zip_code;
run;

data admin_zip_oregon;
	merge admin_zip_oregon0 (in = a) zip_steve (in = b);
	by zip_code;
run;

/*
Remove any unnecessary rows.
*/

proc sort data = admin_zip_oregon nodupkeys;
	by count;
run;

data admin_zip_oregon;
	set admin_zip_oregon;
	if administrations = . and ZCTA_pop = . then delete;
run;

/* 
Set everything to null, in order to combine measures for all zip codes/ZCTAs which don't exist or don't have a county.
*/
data admin_zip_oregon;
	set admin_zip_oregon;
	if cnty in ('Unk', ' ') then do;
	zip_code = .;
	ZCTA = .;
	ZCTA_county = ' ';
	ZCTA_pop = .;
	ZCTA_numb_zips = .;
	zip_type = ' '; 
	end;
run;

/* Check counts again to make sure everything is okay */
proc sql;
	create table admin_zip_check as
		select sum(people_count) as people_count,
			sum(administrations) as administrations
		from admin_zip_oregon;
	quit;

proc print data = admin_zip_check;
run;

/*
Note in admin_zip_oregon, the ZCTA_pop is repeated when the ZCTA has multiple rows or multiple ZIP codes. 
We do NOT want to sum these when we aggregate. We want to mean them. 
*/
proc sort data = admin_zip_oregon;
by ZCTA;
run;

proc print data = admin_zip_oregon;
	where ZCTA = 97006;
	var zip_code ZCTA ZCTA_pop count people_count administrations;
run;

/*
Note in admin_zip_oregon, for the zip codes/ZCTAs we aggregated above for small populations, 
the ZCTA_pop is unique for each row. 
We want to sum these when we aggregate. We do NOT want to mean them.
*/

proc print data = admin_zip_oregon;
	where ZCTA = 97001;
	var zip_code ZCTA ZCTA_pop count people_count administrations;
run;

/*
Are there any ZCTAs where some rows need to be meaned and some rows need to be added?
*/

proc print data = admin_zip_oregon;
	where ZCTA in (97029, 97001, 97209, 97209, 97741, 97638, 97751, 97837, 97903, 97903, 97911);
	var zip_code ZCTA ZCTA_pop count people_count administrations;
run;

/* Yes, make one of the rows for 97911 equal to zero, so that it doesn't add the repeated ZCTA_pop twice. */

data admin_zip_oregon;
	set admin_zip_oregon;
	if ZCTA = 97911 AND zip_code = 97917 then do;
		ZCTA_pop = 0;
	end;
run;

/* Check */

proc print data = admin_zip_oregon;
	where ZCTA =97911;
	var zip_code ZCTA ZCTA_pop count people_count administrations;
run;

/*
So we need to aggregate vaccine measures by ZCTA (so each ZCTA has one row instead of multiple rows), 
but we need to do it different depending if we aggregated the ZCTAs due to small populations or not.
I'm almost certain that we need to have measures aggregated by ZCTA to make the map grouping work in Tableau.
So create two tables. One with the small population ZCTAs and one without.
*/

data admin_zip_oregon_a;
	set admin_zip_oregon;
		if ZCTA in (97029, 97001, 97209, 97209, 97741, 97638, 97751, 97837, 97903, 97903, 97911) then output;
run;


data admin_zip_oregon_b;
	set admin_zip_oregon;
		if ZCTA not in (97029, 97001, 97209, 97209, 97741, 97638, 97751, 97837, 97903, 97903, 97911) then output;
run;

/* Aggregate vaccine measures by ZCTA*/
proc sql;
	create table admin_zip_oregon1a as
		select
				ZCTA,
				ZCTA_county,
				sum(people_count) as people_count,
				sum(administrations) as administrations,
				sum(ZCTA_pop) as ZCTA_pop,
				sum(pop_nonwhite) as pop_nonwhite,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				sum(needs_boost_now) as needs_boost_now
		from admin_zip_oregon_a group by ZCTA, ZCTA_county;
quit;

proc sql;
	create table admin_zip_oregon1b as
		select
				ZCTA,
				ZCTA_county,
				sum(people_count) as people_count,
				sum(administrations) as administrations,
				mean(ZCTA_pop) as ZCTA_pop,
				mean(pop_nonwhite) as pop_nonwhite,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				sum(needs_boost_now) as needs_boost_now
		from admin_zip_oregon_b group by ZCTA, ZCTA_county;
quit;

/* Stack the ZCTAs back together after aggregation */
data admin_zip_oregon1;
	set admin_zip_oregon1a admin_zip_oregon1b;
run;

/* Check counts again to make sure everything is okay */
proc sql;
	create table admin_zip_check as
		select sum(people_count) as people_count,
			sum(administrations) as administrations
		from admin_zip_oregon1;
	quit;

proc print data = admin_zip_check;
run;

/*
Now we need to make the data long again ¯\_(?)_/¯
So, in Tableau we will show avg() measures for ZCTA, 
and the data will show the same # by ZCTA regardless of which zip code they hover over.
*/

/* Make a list of zip code and ZCTA*/

data zip_ZCTA_only;
	keep zip_code ZCTA;
	set admin_zip_oregon;
run;

/* Merge the clean list with the ZCTA-level table*/
proc sort data = zip_ZCTA_only nodupkey;
	by zip_code;
run;

proc sort data = zip_ZCTA_only;
	by ZCTA;
run;

proc sort data = admin_zip_oregon1;
	by ZCTA;
run;

data admin_zip_oregon2;
	merge zip_ZCTA_only admin_Zip_oregon1;
	by ZCTA;
run;

/* Delete any weird rows created by the merge */
proc sort data = admin_zip_oregon2 nodupkeys;
	by ZCTA zip_code ZCTA_county;
run;

/*Should only show row for null zip code, if not - check for errors above. */
proc print data = admin_zip_oregon2;
	where ZCTA_pop < 50;
run;

/*
Create regions by county 
These match the PUMS, rarest group regions
*/
data admin_zip_oregon2;
	set admin_zip_oregon2;
	format region $100.;
	if ZCTA_county = 'Baker' then region = 'Umatilla, Union, Baker & Wallowa';
	if ZCTA_county = 'Benton' then region = 'Linn & Benton ';
	if ZCTA_county = 'Clackamas' then region = 'Clackamas';
	if ZCTA_county = 'Clatsop' then region = 'Columbia, Lincoln, Clatsop & Tillamook ';
	if ZCTA_county = 'Columbia' then region = 'Columbia, Lincoln, Clatsop & Tillamook ';
	if ZCTA_county = 'Coos' then region = 'Josephine, Coos & Curry ';
	if ZCTA_county = 'Crook' then region = 'Crook, Gilliam, Grant, Hood River, Jefferson, Morrow, Sherman, Wasco, & Wheeler ';
	if ZCTA_county = 'Curry' then region = 'Josephine, Coos & Curry ';
	if ZCTA_county = 'Deschutes' then region = 'Deschutes';
	if ZCTA_county = 'Douglas' then region = 'Douglas';
	if ZCTA_county = 'Gilliam' then region = 'Crook, Gilliam, Grant, Hood River, Jefferson, Morrow, Sherman, Wasco, & Wheeler ';
	if ZCTA_county = 'Grant' then region = 'Crook, Gilliam, Grant, Hood River, Jefferson, Morrow, Sherman, Wasco, & Wheeler ';
	if ZCTA_county = 'Harney' then region = 'Klamath, Malheur, Lake & Harney ';
	if ZCTA_county = 'Hood River' then region = 'Crook, Gilliam, Grant, Hood River, Jefferson, Morrow, Sherman, Wasco, & Wheeler ';
	if ZCTA_county = 'Jackson' then region = 'Jackson';
	if ZCTA_county = 'Jefferson' then region = 'Crook, Gilliam, Grant, Hood River, Jefferson, Morrow, Sherman, Wasco, & Wheeler ';
	if ZCTA_county = 'Josephine' then region = 'Josephine, Coos & Curry ';
	if ZCTA_county = 'Klamath' then region = 'Klamath, Malheur, Lake & Harney ';
	if ZCTA_county = 'Lake' then region = 'Klamath, Malheur, Lake & Harney ';
	if ZCTA_county = 'Lane' then region = 'Lane';
	if ZCTA_county = 'Lincoln' then region = 'Columbia, Lincoln, Clatsop & Tillamook ';
	if ZCTA_county = 'Linn' then region = 'Linn & Benton ';
	if ZCTA_county = 'Malheur' then region = 'Klamath, Malheur, Lake & Harney ';
	if ZCTA_county = 'Marion' then region = 'Marion';
	if ZCTA_county = 'Morrow' then region = 'Crook, Gilliam, Grant, Hood River, Jefferson, Morrow, Sherman, Wasco, & Wheeler ';
	if ZCTA_county = 'Multnomah' then region = 'Multnomah';
	if ZCTA_county = 'Polk' then region = 'Yamhill & Polk ';
	if ZCTA_county = 'Sherman' then region = 'Crook, Gilliam, Grant, Hood River, Jefferson, Morrow, Sherman, Wasco, & Wheeler ';
	if ZCTA_county = 'Tillamook' then region = 'Columbia, Lincoln, Clatsop & Tillamook ';
	if ZCTA_county = 'Umatilla' then region = 'Umatilla, Union, Baker & Wallowa ';
	if ZCTA_county = 'Union' then region = 'Umatilla, Union, Baker & Wallowa ';
	if ZCTA_county = 'Wallowa' then region = 'Umatilla, Union, Baker & Wallowa ';
	if ZCTA_county = 'Wasco' then region = 'Crook, Gilliam, Grant, Hood River, Jefferson, Morrow, Sherman, Wasco, & Wheeler ';
	if ZCTA_county = 'Washington' then region = 'Washington';
	if ZCTA_county = 'Wheeler' then region = 'Crook, Gilliam, Grant, Hood River, Jefferson, Morrow, Sherman, Wasco, & Wheeler ';
	if ZCTA_county = 'Yamhill' then region = 'Yamhill & Polk ';
	if ZCTA_county = ' ' then region = ' ';
run;


data data.alert_admin_zip;
	set admin_zip_oregon2;
run;


/***********************************************************************/
/*
These ZCTAs were chosen to display for analysis for the Operations Equity Meeting & for 
Neighborhood based outreach after looking at people remaining and diversity index analyses in Tableau.
So keep these ones, and get more granular data for just these ZCTAs for visualizations. 
Keep certain data from the ZCTA-aggregated file.
*/

data admin_zip_neigh_pop;
	keep zip_code ZCTA ZCTA_county ZCTA_pop pop_nonwhite;
	set admin_zip_oregon2;
	if ZCTA in (97006,
					97007,
					97030,
					97031,
					97058,
					97080,
					97124,
					97128,
					97206,
					97230,
					97233,
					97236,
					97301,
					97303,
					97305,
					97317,
					97401,
					97402,
					97478,
					97501,
					97504,
					97603,
					97701,
					97838,
					97914 ) then output;
run;

/* Merge to full imms list together */
proc sort data = admin_zip_neigh_pop;
	by zip_code;
run;

data admin_zip_neigh;
	set admin_zip1;
	zip_code = recip_address_zip;
run;

proc sort data = admin_zip_neigh;
	by zip_code;
run;

proc contents data = admin_zip_neigh_pop;
run;

proc contents data = admin_zip1;
run;

data admin_zip_neigh_pop_imms;
	merge admin_zip_neigh admin_zip_neigh_pop;
	by zip_code;
	if ZCTA = . then delete;
run;


proc sql;
   create table admin_neigh_by_day as 
     select admin_date as date,
			sum(people_count) as people_count,
			sum(vaccinated) as vaccinated,
			sum(booster) as booster,
			sum(booster_first) as booster_first,
			sum(booster_second) as booster_second,
			sum(booster_biv) as booster_biv,
			sum(needs_boost_now) as needs_boost_now,
			mean(ZCTA) as ZCTA,
			ZCTA_county,
			mean(ZCTA_pop) as ZCTA_pop,
			mean(pop_nonwhite) as pop_nonwhite
            from admin_zip_neigh_pop_imms group by ZCTA, date;
quit;

proc sort data = admin_neigh_by_day nodupkey;
by ZCTA date;
run;

/*Check*/

proc sql;
	create table admin_neigh_check as
		select sum(people_count) as people_count,
				mean(ZCTA_pop) as ZCTA_pop
		from admin_neigh_by_day
	where ZCTA = 97006;
quit;

proc print data = admin_neigh_check;
run;

proc freq data = admin_imms;
	where recip_address_zip in (97003, 97006, 97076);
	table people_count;
run;

proc print data = pop.zip_zcta_oregon_2021;
	where ZCTA = 97006;
run;



data data.admin_neigh_by_day;
	set admin_neigh_by_day;
run;


data pop.zip_zcta_public_crosswalk_2021;
	keep zip_code ZCTA ZCTA_county region ZCTA_pop pop_nonwhite;
	set admin_zip_oregon2;
	if ZCTA = . then delete;
run;

/* 
Delete large datasets from the work library to conserve space on the server. 
*/
proc datasets library = work nolist;
	delete admin_zip_neigh_pop admin_zip_neigh_imms admin_zip_neigh admin_imms admin_zip1 admin_zip2;
quit;
