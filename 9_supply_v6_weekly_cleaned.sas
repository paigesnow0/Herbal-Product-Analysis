*Metrics for Tableau;
*Paige Snow paige.snow@dhsoha.state.or.us;
*OHA CRRU;
*5/2022;
DM 'clear log; clear output;';
dm 'odsresults; clear';

/*
We aggregate here to do extra calculations (the countdown to 80%, % change, equity analyses, etc) and to combine with federal data.
*/

data supply_state;
	set data.alert_state;
	region = county;
	if DemographicType in ('Race2') then delete;
run;

data supply_county;
	set data.alert_county;
	region = county;
run;

/************************************************************************************************************************/
/************************************************************************************************************************/
/************************************************************************************************************************/
/************************************************************************************************************************/
/************************************************************************************************************************/
/************************************************************************************************************************/
/*
Calculate % change for current week and previous week.
Save three time points - most recent = 3 days ago, a week and then two weeks before that
*/

/* Keep only the numbers up to 3 days ago (providers have 3 days to input immunizations)*/
data supply_change_state_new;
	set supply_state;
	format date_new mmddyy10.;
	date_new = &today - 4;
	if date <= (&today - 4) then output;
run;

/* Keep only the numbers up to a week before 3 days ago */
data supply_change_state_old;
	set supply_state;
	format date_old mmddyy10.;
	date_old = &today - 4 - 7;
	if date <= (&today - 4 - 7) then output;
run;

/*Keep only the numbers up to two weeks before 3 days ago */
data supply_change_state_old2;
	set supply_state;
	format date_old mmddyy10.;
	date_old = &today - 4 - 7 - 7;
	if date <= (&today - 4 - 7 - 7) then output;
run;

/*Aggregate data at all three time points */
proc sql;
	create table supply_change_state_old2_agg as
		select DemographicType,
				DemographicCategory,
				mean(date_old) as date_old2, 
				sum(people_count) as people_count_old2,
				sum(people_count16) as people_count16_old2,
				sum(booster) as booster_old2,
				sum(booster16) as booster16_old2,
				sum(vaccinated) as vaccinated_old2
		from supply_change_state_old2
		group by DemographicType, DemographicCategory;
quit;

proc sql;
	create table supply_change_state_old_agg as
		select DemographicType,
				DemographicCategory,
				mean(date_old) as date_old, 
				sum(people_count) as people_count_old,
				sum(people_count16) as people_count16_old,
				sum(booster) as booster_old,
				sum(booster16) as booster16_old,
				sum(vaccinated) as vaccinated_old
		from supply_change_state_old
		group by DemographicType, DemographicCategory;
quit;

proc sql;
	create table supply_change_state_new_agg as
		select DemographicType,
				DemographicCategory, 
				mean(date_new) as date_new, 
				sum(people_count) as people_count_new,
				sum(people_count16) as people_count16_new,
				sum(booster) as booster_new,
				sum(booster16) as booster16_new,
				sum(vaccinated) as vaccinated_new
		from supply_change_state_new
		group by DemographicType, DemographicCategory;
quit;

/*Merge data from the three time points, so you can calculate with different columns */
data supply_change_state_all;
	merge supply_change_state_old2_agg supply_change_state_old_agg supply_change_state_new_agg;
	by DemographicType DemographicCategory;
run;

/*******************/
/*Repeat at the county level */
data supply_change_cty_old2;
	set supply_county;
	format date_old mmddyy10.;
	date_old = (&today -4 -7-7);
	if date <= (&today - 4 - 7-7) then output;
run;

data supply_change_cty_old;
	set supply_county;
	format date_old mmddyy10.;
	date_old = (&today -4 -7);
	if date <= (&today - 4 - 7) then output;
run;

data supply_change_cty_new;
	set supply_county;
	format date_new mmddyy10.;
	date_new = (&today - 4);
	if date <= (&today - 4) then output;
run;

proc sql;
	create table supply_change_cty_old2_agg as
		select DemographicType,
				DemographicCategory, 
				county,
				region,
				mean(date_old) as date_old2, 
				sum(people_count) as people_count_old2,
				sum(people_count16) as people_count16_old2,
				sum(booster) as booster_old2,
				sum(booster16) as booster16_old2,
				sum(vaccinated) as vaccinated_old2
		from supply_change_cty_old2
		group by DemographicType, DemographicCategory, region;
quit;

proc sql;
	create table supply_change_cty_old_agg as
		select DemographicType,
				DemographicCategory, 
				county,
				region,
				mean(date_old) as date_old, 
				sum(people_count) as people_count_old,
				sum(people_count16) as people_count16_old,
				sum(booster) as booster_old,
				sum(booster16) as booster16_old,
				sum(vaccinated) as vaccinated_old
		from supply_change_cty_old
		group by DemographicType, DemographicCategory, region;
quit;


proc sql;
	create table supply_change_cty_new_agg as
		select DemographicType,
				DemographicCategory, 
				county,
				region,
				mean(date_new) as date_new, 
				sum(people_count) as people_count_new,
				sum(people_count16) as people_count16_new,
				sum(booster) as booster_new,
				sum(booster16) as booster16_new,
				sum(vaccinated) as vaccinated_new
		from supply_change_cty_new
		group by DemographicType, DemographicCategory, region;
quit;

proc sort data = supply_change_cty_new_agg nodupkey;
	by DemographicType DemographicCategory county region;
run;

proc sort data = supply_change_cty_old_agg nodupkey;
	by DemographicType DemographicCategory county region;
run;

proc sort data = supply_change_cty_old2_agg nodupkey;
	by DemographicType DemographicCategory county region;
run;

data supply_change_cty_all;
	merge supply_change_cty_old2_agg supply_change_cty_old_agg supply_change_cty_new_agg;
	by DemographicType DemographicCategory county region;
	if county = ' ' and region = ' ' then delete;
run;

/*****************/
/*Stack state and county tables and do the calculations for % change and averages */
data supply_change_all;
	set supply_change_state_all supply_change_cty_all;
	format date_old2 date_old date_new mmddyy10.;
	people_change = (people_count_new - people_count_old)/people_count_old;
	people_change2 = (people_count_old - people_count_old2)/people_count_old2;
	people_average = (people_count_new - people_count_old)/7;
	people_average2 = (people_count_old - people_count_old2)/7;
	
	people16_change = (people_count16_new - people_count16_old)/people_count16_old;
	people16_change2 = (people_count16_old - people_count16_old2)/people_count16_old2;
	people16_average = (people_count16_new - people_count16_old)/7;
	people16_average2 = (people_count16_old - people_count16_old2)/7;

	boost_change = (booster_new - booster_old)/booster_old;
	boost_change2 = (booster_old - booster_old2)/booster_old2;
	boost_average = (booster_new - booster_old)/7;
	boost_average2 = (booster_old - booster_old2)/7;
	
	boost16_change = (booster16_new - booster16_old)/booster16_old;
	boost16_change2 = (booster16_old - booster16_old2)/booster16_old2;
	boost16_average = (booster16_new - booster16_old)/7;
	boost16_average2 = (booster16_old - booster16_old2)/7;

	if DemographicCategory in( "0 to 4", "5 to 11", "12 to 17") then do;
	people16_change = (people_count_new - people_count_old)/people_count_old;
	people16_average = (people_count_new - people_count_old)/7;
	people16_change2 = (people_count_old - people_count_old2)/people_count_old2;
	people16_average2 = (people_count_old - people_count_old2)/7;

	boost16_change = (booster_new - booster_old)/booster_old;
	boost16_average = (booster_new - booster_old)/7;
	boost16_change2 = (booster_old - booster_old2)/booster_old2;
	boost16_average2 = (booster_old - booster_old2)/7;
	end;
	*vax_change = (vaccinated_new - vaccinated_old)/vaccinated_old;
	date_label = strip(put(date_old, mmddyy10.)) || " to " || strip(put(date_new, mmddyy10.)); 
	if county = ' ' and region = ' ' then do;
		county = 'Oregon';
		region = 'Oregon';
	end;
	if county ^= ' ' and region = ' ' then do;
		region = county;
	end;
run;

proc print data = supply_change_all;
	where DemographicCategory = "NH/PI" and county = "Douglas";
run;

data data.supply_percent_change;
	set supply_change_all;
run;

proc export data = data.supply_percent_change outfile='\\wpohaappl109\EPI\supply_percent_change.csv' 
	dbms = csv replace;
run;

/************************************************************************************************************************/
/************************************************************************************************************************/
/************************************************************************************************************************/
/* 
Aggregate data as of today and as of April.
Figure out the state/county % one dose for all residents, total pop and 18+, to be references in the equity analyses.
The Qs: What's the R/E % compared with the % for all residents? What's the difference between April 2021 and today?
*/

proc sql;
	create table supply_state_agg as
		select DemographicType,
				DemographicCategory,
				sum(people_count) as people_count,
				sum(people_count16) as people_count16,
				sum(vaccinated) as vaccinated,
				sum(vaccinated16) as vaccinated16,
				sum(booster) as booster,
				sum(booster16) as booster16,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost_now16) as needs_boost_now16,
				mean(population) as population,
				mean(population18) as population18
	from supply_state
	group by DemographicType, DemographicCategory;
quit;

proc sort data = supply_state_agg nodupkey;
	by DemographicType DemographicCategory;
quit;

data state_reference;
	keep reference reference18 reference_vax reference_vax18 reference_boost reference_boost18;
	set supply_state_agg;
	reference = people_count/population;
	reference18 = people_count16/population18;
	reference_vax = vaccinated/population;
	reference_vax18 = vaccinated16/population18;
	reference_boost = booster/(booster + needs_boost_now);
	reference_boost18 = booster16/(booster16 + needs_boost_now16);
	if DemographicType = 'All Oregonians' then output;
run;

data supply_state_april;
	set data.alert_state;
	if DemographicType in ('Race2') then delete;
	if date > '01APR21'd then delete;
run;

proc sql;
	create table supply_state_agg_april as
		select DemographicType,
				DemographicCategory,
				sum(people_count) as people_count,
				sum(people_count16) as people_count16,
				mean(population) as population,
				mean(population18) as population18
	from supply_state_april
	group by DemographicType, DemographicCategory;
quit;

proc sort data = supply_state_agg_april nodupkey;
	by DemographicType DemographicCategory;
quit;

data state_reference_april;
	keep reference_apr reference18_apr;
	set supply_state_agg_april;
	reference_apr = people_count/population;
	reference18_apr = people_count16/population18;
	if DemographicType = 'All Oregonians' then output;
run;

data state_reference;
	merge state_reference state_reference_april;
	DemographicType = 'Race';
	County = 'Oregon';
run;

/*************/


proc sql;
	create table supply_county_agg as
		select DemographicType,
				DemographicCategory,
				county,
				region,
				sum(people_count) as people_count,
				sum(people_count16) as people_count16,
				sum(vaccinated) as vaccinated,
				sum(vaccinated16) as vaccinated16,
				sum(booster) as booster,
				sum(booster16) as booster16,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost_now16) as needs_boost_now16,
				mean(population) as population,
				mean(population18) as population18
	from supply_county
	group by DemographicType, DemographicCategory, county, region;
quit;

proc sort data = supply_county_agg nodupkey;
	by DemographicType DemographicCategory county region;
quit;

data supply_county_agg;
	set supply_county_agg;
	if county = ' ' and region = ' ' then delete;
run;

/**/

data county_reference;
	keep county reference reference18 reference_vax reference_vax18 reference_boost reference_boost18;
	set supply_county_agg;
	reference = people_count/population;
	reference18 = people_count16/population18;
	reference_vax = vaccinated/population;
	reference_vax18 = vaccinated16/population18;
	reference_boost = booster/(booster + needs_boost_now);
	reference_boost18 = booster16/(booster16 + needs_boost_now16);
	if DemographicType = 'All Oregonians' then output;
run;

data supply_county_april;
	set data.alert_county;
	if DemographicType in ('Race2') then delete;
	if date > '01APR21'd then delete;
run;

proc sql;
	create table supply_county_agg_april as
		select DemographicType,
				DemographicCategory,
				county,
				region,
				sum(people_count) as people_count,
				sum(people_count16) as people_count16,
				mean(population) as population,
				mean(population18) as population18
	from supply_county_april
	group by DemographicType, DemographicCategory, county, region;
quit;

proc sort data = supply_county_agg_april nodupkey;
	by DemographicType DemographicCategory county region;
quit;

data supply_county_agg_april;
	set supply_county_agg_april;
	if county = ' ' and region = ' ' then delete;
run;

data county_reference_april;
	keep county reference_apr reference18_apr;
	set supply_county_agg_april;
	reference_apr = people_count/population;
	reference18_apr = people_count16/population18;
	if DemographicType = 'All Oregonians' then output;
run;

data County_reference;
	merge County_reference county_reference_april;
	by county;
	DemographicType = 'Race';
run;

data equity_references;
	set county_reference state_reference ;
run;

/*Stack the aggregated data (up until current date) for further analysis */

data supply_agg_stack;
	set supply_county_agg supply_state_agg;
run;

/******************/
/*Calculate the people remaining*/

data supply_agg_stack;
	set supply_agg_stack;
	if DemographicType in ('All Oregonians', 'Race','Race OMB', 'Ethnicity OMB', 'Race & Ethnicity OMB', 'Sex') then do;
		difference80 = population18*0.8 - people_count16;
		difference80_all = population*.8 - people_count;
	end;
	if DemographicType  = 'Age Groups' then do;
		difference80 = population*0.8 - people_count;
		difference80_all = population*0.8 - people_count;
	end;
	if DemographicCategory not in ('Other Race', 'Unknown') then do;
		if difference80 < 0 then do;
			difference80 = 0;
		end;
		if difference80_all < 0 then do;
			difference80_all = 0;
		end;
	end;
run;

proc sort data = supply_agg_stack;
by DemographicType county;
run;

data supply_countdown;
	set supply_agg_stack;
	if county = ' ' and region = ' ' then do;
		county = 'Oregon';
		region = 'Oregon';
	end;
	if DemographicType = 'Age Groups' then do;
		population18 = population;
	end;
run;


/***************************/
data supply_change_avg;
	keep DemographicType DemographicCategory County Region people16_average people_average;
	set supply_change_all;
run;

proc sort data = supply_change_avg;
by DemographicType DemographicCategory county region;
run;

proc sort data = supply_countdown;
by DemographicType DemographicCategory county region;
run;

data supply_countdown_avg;
	merge supply_countdown supply_change_avg;
	by DemographicType DemographicCategory county region;
	format predicted_date predicted16_date mmddyy10.;
	predicted_date = &today +(difference80_all/people_average);
	predicted16_date = &today +(difference80/people16_average);
run;

proc sort data = supply_countdown_avg;
	by DemographicType County;
run;

proc sort data = equity_references;
	by DemographicType County;
run;

data supply_countdown_avg;
	merge supply_countdown_avg equity_references;
	by DemographicType county;
run;

proc sort data = supply_countdown_avg;
	by DemographicType DemographicCategory county region;
run;

data data.supply_countdown;
	set supply_countdown_avg;
run;

proc export data = data.supply_countdown outfile='\\wpohaappl109\EPI\supply_countdown.csv' 
	dbms = csv replace;
run;

/************************************************************************************************************************/
/************************************************************************************************************************/
/************************************************************************************************************************/
/************************************************************************************************************************/
/************************************************************************************************************************/
/************************************************************************************************************************/
/************************************************************************************************************************/
/************************************************************************************************************************/
/************************************************************************************************************************/

data supply_state_spark;
	keep date DemographicType DemographicCategory people_count people_count16 population population18 reference;
	set supply_state;
run;

data supply_county_spark;
	keep date DemographicType DemographicCategory county region people_count people_count16 population population18 reference;
	set supply_county;
	if county = ' ' and region = ' ' then delete;
run;

data supply_spark_all0;
	set supply_state_spark supply_county_spark;
	if county = ' ' and region = ' ' then do;
		county = 'Oregon';
		region = 'Oregon';
	end;
	if county ^= ' ' and region = ' ' then do;
		region = county;
	end;
	if DemographicType ^= 'All Oregonians' then delete;
run;

proc sort data = supply_spark_all0;
	 by DemographicType DemographicCategory County Region;
run;

proc sql;
	create table supply_spark_all1 as
		select DemographicType,
				DemographicCategory,
				County,
				Region, 
				sum(people_count) as people_count,
				sum(people_count16) as people_count16,
				mean(population) as population,
				mean(population18) as population18
	from supply_spark_all0
	group by DemographicType, DemographicCategory, County, Region;
quit;


data supply_spark_all1;
	set supply_spark_all1;
	format reference mmddyy10.;
	reference = &today - 4;
	today = put(&today, mmddyy10.);
	source = 'ALERT     ';
run;

data supply_spark_all;
	set supply_spark_all1 federal_people_count;
run;

/*
ods output CrossTabFreqs = supply_people_7day;
proc freq data=first_event_id; 
	where admin_date between (&yesterday -9) and (&yesterday-3); 
	tables admin_date*people_count/nopercent; 
	run;
ods output close;

data supply_people_7day;
	keep frequency;
	set supply_people_7day;
	if people_count = . then delete;
	if admin_date = . then delete;
run;

proc sql;
	create table supply_people_7day1 as
		select mean(frequency) as people_7day
	from supply_people_7day;
quit;

data CDC;
	merge CDC supply_people_7day1;
	days_left = difference/people_7day;
	days_to_July = '30JUN21'd - &today;
	target_met = "     ";
	if days_left < days_to_july then target_met = "will";
	if days_left > days_to_july then target_met = "will not";
	format reopen_date yesterday mmddyy10.;
	reopen_date = &today + days_left;
	yesterday = &yesterday;
	date_label1 = put((&yesterday -9), mmddyy10.);
	date_label2 = put((&yesterday -3), mmddyy10.);
	date_label = date_label1 || " and " || date_label2;
	drop date_label1 date_label2;
run;
	
data supply_spark_all;
	merge supply_spark_all0 CDC;
	by today;
run;
*/

data data.supply_spark_all;
	set supply_spark_all;
run;


/*
proc print data = supply_change_all;
	where DemographicCategory = 'Hispanic';
run;
*/
/******************/

proc datasets library = work nolist;
	delete demogv: final_imms:  tableau_imms: time_to_completion:
			admin_imms admin_zip supply_county_april by_day_: completed_extra:;
quit;
