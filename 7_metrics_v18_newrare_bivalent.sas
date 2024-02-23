*Metrics for Tableau;
*Paige Snow paige.snow@dhsoha.state.or.us;
*OHA VPU;
*3/8/2021;
DM 'clear log; clear output;';
dm 'odsresults; clear';

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

data tableau_imms;
set data.tableau_imms_boost_utd_&thismonday;
run;

data completed_extra;
set data.completed_extra_utd_&thismonday;
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

/*One Dose count table*/
ods output  CrossTabFreqs = race_age_ppl;
proc freq data = tableau_imms;
	where people_count = 1;
	table age_cat*rarest_race /nopercent norow nocol missing;
	format age_cat age_dis_two. rarest_race race.;
run;
ods output close;

data race_age_ppl;
	keep DemographicType DemographicCategory Age_Group people_count;
	retain DemographicType DemographicCategory Age_Group people_count;
	set race_age_ppl;		
	format DemographicType $20. DemographicCategory $50.;
	age_group = put(age_cat, age_dis_two.);
	DemographicType = 'Race';
	DemographicCategory = put(rarest_race, race.);
	people_count = frequency;
	if rarest_race = '.' then delete;
	if age_group = '.' then delete;
	if age_cat = . then delete;
run;

proc sort data = race_age_ppl;
	by DemographicCategory Age_Group;
run;

/*Process race_age_ppl */

data race_age_pop;
	set pop.race_age_pop_2021;
	if county = 'Oregon' then output;
run;

proc sort data = race_age_pop;
	by DemographicCategory Age_Group;
run;

data race_age_ppl_pop;
	merge race_age_pop race_age_ppl;
	by DemographicCategory age_group;
	if people_count = . then delete;
run;

data data.race_age_ppl_pop;
	set race_age_ppl_pop;
run;


/*Vaccinated Count Table*/
ods output  CrossTabFreqs = race_age_vaccinated;
proc freq data = tableau_imms;
	where vaccinated = 1;
	table age_cat*rarest_race /nopercent norow nocol missing;
	format age_cat age_dis_two. rarest_race race.;
run;
ods output close;

data race_age_vaccinated;
	keep DemographicType DemographicCategory Age_Group vaccinated_count;
	retain DemographicType DemographicCategory Age_Group vaccinated_count;
	set race_age_vaccinated;		
	format DemographicType $20. DemographicCategory $50.;
	age_group = put(age_cat, age_dis_two.);
	DemographicType = 'Race';
	DemographicCategory = put(rarest_race, race.);
	vaccinated_count = frequency;
	if rarest_race = '.' then delete;
	if age_group = '.' then delete;
	if age_cat = . then delete;
run;

proc sort data = race_age_vaccinated;
	by DemographicCategory Age_Group;
run;

/*Process race_age_vaccinated */

data race_age_ppl_vaccinated;
	merge race_age_pop race_age_vaccinated;
	by DemographicCategory age_group;
	if vaccinated_count = . then delete;
run;

data data.race_age_ppl_vaccinated;
	set race_age_ppl_vaccinated;
run;

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*ods output  CrossTabFreqs = race_age_ppl_cty1;
proc freq data = tableau_imms;
	where people_count = 1;
	table county*age_cat*rarest_race /nopercent norow nocol missing;
	format age_cat age_dis_two. rarest_race race.;
run;
ods output close;

data race_age_ppl_cty1;
	keep County DemographicType DemographicCategory Age_Group people_count;
	retain County DemographicType DemographicCategory Age_Group people_count;
	set race_age_ppl_cty1;		
	format DemographicType $20. DemographicCategory $50.;
	age_group = put(age_cat, age_dis_two.);
	DemographicType = 'Race';
	DemographicCategory = put(rarest_race, race.);
	people_count = frequency;
	if County = ' ' then delete;
	if rarest_race = '.' then delete;
	if age_group = '.' then delete;
	if age_cat = . then delete;
run;

ods output  CrossTabFreqs = race_age_ppl_cty2;
proc freq data = tableau_imms;
	where people_count = 1;
	table region*age_cat*rarest_race /nopercent norow nocol missing;
	format age_cat age_dis. rarest_race race.;
run;
ods output close;

data race_age_ppl_cty2;
	keep Region DemographicType DemographicCategory Age_Group people_count;
	retain Region DemographicType DemographicCategory Age_Group people_count;
	set race_age_ppl_cty2;		
	format DemographicType $20. DemographicCategory $50.;
	age_group = put(age_cat, age_dis.);
	DemographicType = 'Race';
	DemographicCategory = put(rarest_race, race.);
	people_count = frequency;
	if region = ' ' then delete;
	if rarest_race = '.' then delete;
	if age_group = '.' then delete;
	if age_cat = . then delete;
run;

proc sort data = race_age_ppl_cty1;
	by Region DemographicCategory Age_Group;
run;

proc sort data = race_age_ppl_cty2;
	by Region DemographicCategory Age_Group;
run;

/*Process race_age_ppl */
/*
data Region_re_age1;
	set pop.Region_re_age1;
run;

data Region_re_age2;
	set pop.Region_re_age2;
run;

proc sort data = Region_re_age1;
	by Region DemographicCategory Age_Group;
run;

proc sort data = Region_re_age2;
	by Region DemographicCategory Age_Group;
run;

data race_age_ppl_pop_cty1;
	merge Region_re_age1 race_age_ppl_cty1;
	by Region DemographicCategory age_group;
	if age_group not in ('5 to 11', '12 to 17') then delete;
	if DemographicCategory in ('Other Race', 'Unknown') then delete;
run;

data race_age_ppl_pop_cty2;
	merge Region_re_age2 race_age_ppl_cty2;
	by Region DemographicCategory age_group;
	if age_group not in ('5 to 11', '12 to 17') then delete;
	if DemographicCategory in ('Other Race', 'Unknown') then delete;
run;

proc print data = race_age_ppl_pop_cty1;
	where action ^= 'Report';
run;

proc print data = race_age_ppl_pop_cty2;
	where action ^= 'Report';
run;

data data.race_age_ppl_pop_cty;
	set race_age_ppl_pop_cty1;
run;
*/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/

/* Time to completion - Updated with series complete and extra dose methodology */
data time_to_completion1;
	set completed_extra;
run;

data time_to_completion2;
	set time_to_completion1;
	if prim_series = . then delete;
	if completion_vax in ('Johnson & Johnson', 'Unspecified') then delete;
	daydiff = intck('DAY', initiated_date, completion_date);
	weekdiff = round(daydiff/7, 1);
run;

proc freq data = time_to_completion2;
	table prim_series / missing;
run;

proc freq data = time_to_completion2;
	table prim_series*weekdiff / missing;
run;

proc sort data = time_to_completion2; by cvx; run;

ods output CrossTabFreqs = time_to_completion3;
proc freq data = time_to_completion2;
	where initiated_date < (&today - 56) ;
	table completion_vax*weekdiff*prim_series/ cumcol missing;
run;


%macro time_compl(numb = );
data week&numb;
	set time_to_completion3;
	if weekdiff <= &numb and weekdiff ^=. and prim_series = 1 and completion_vax ^= ' ' then output;
run;

proc sql;
	create table week&numb._num as
		select completion_vax,
				max(weekdiff) as weekdiff,
				sum(frequency) as numerator
		from week&numb
		group by completion_vax;
quit;
%mend;

%time_compl (numb = 0);
%time_compl (numb = 1);
%time_compl (numb = 2);
%time_compl (numb = 3);
%time_compl (numb = 4);
%time_compl (numb = 5);
%time_compl (numb = 6);
%time_compl (numb = 7);
%time_compl (numb = 8);
*%time_compl (numb = 9);
*%time_compl (numb = 10);
*%time_compl (numb = 11);
*%time_compl (numb = 12);


data time_to_completion4;
	keep vaccine weekdiff numerator;
	retain vaccine week numerator;
	set week0_num week1_num week2_num week3_num week4_num week5_num
					week6_num week7_num week8_num;
					*week9_num week10_num
					week11_num week12_num;
	vaccine = completion_vax;
run;


data time_to_completion5;
	set time_to_completion3;
	if weekdiff ^=. and prim_series ^= . and completion_vax ^= ' ' then output;
run;

proc sql;
	create table time_denominator as
		select completion_vax,
				sum(frequency) as denominator
		from time_to_completion5
		group by completion_vax;
quit;

data time_denominator;
	keep vaccine denominator;
	set time_denominator;
	vaccine = completion_vax;
run;

proc sort data = time_to_completion4; by vaccine; run;

data time_to_completion;
	*keep vaccine weekdiff complete_percent;
	merge time_to_completion4 time_denominator;
	by vaccine;
	complete_percent = numerator/denominator;
run;

data data.time_to_completion;
	set time_to_completion;
run;

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/

/* Find % Unknown, % Other Race */
ods output OneWayFreqs = unknown_race;
proc freq data = tableau_imms;
	where people_count = 1;
	table rarest_race / missing;
	format rarest_race race.;
run;

ods output OneWayFreqs = unknown_race2;
proc freq data = tableau_imms;
	where people_count = 1;
	table race / missing;
run;

ods output OneWayFreqs = unknown_race3;
proc freq data = tableau_imms;
	where people_count = 1;
	table ethnicity / missing;
run;

ods output OneWayFreqs = unknown_race4;
proc freq data = tableau_imms;
	where people_count = 1;
	table race_ethnicity / missing;
run;

ods output OneWayFreqs = unknown_sex;
proc freq data = tableau_imms;
	where people_count = 1;
	table recip_sex / missing;
run;

data unknown_race;
	keep DemographicType DemographicCategory unknown_percent;
	set unknown_race;
		format DemographicType $20. DemographicCategory $50.;
		DemographicType = "Race";
		DemographicCategory = put(rarest_race, race.);
		unknown_percent = percent;
		if DemographicCategory not in ('Other Race', 'Unknown') then delete;
run;
	
data unknown_race2;
	keep DemographicType DemographicCategory unknown_percent;
	set unknown_race2;
		format DemographicType $20. DemographicCategory $50.;
		DemographicType = "Race OMB";
		DemographicCategory = race;
		unknown_percent = percent;
		if DemographicCategory not in ('Other Race', 'Unknown') then delete;
run;

data unknown_race3;
	keep DemographicType DemographicCategory unknown_percent;
	set unknown_race3;
		format DemographicType $20. DemographicCategory $50.;
		DemographicType = "Ethnicity OMB";
		DemographicCategory = ethnicity;
		unknown_percent = percent;
		if DemographicCategory not in ('Other Race', 'Unknown') then delete;
run;

data unknown_race4;
	keep DemographicType DemographicCategory unknown_percent;
	set unknown_race4;
		format DemographicType $20. DemographicCategory $50.;
		DemographicType = "Race & Ethnicity OMB";
		DemographicCategory = race_ethnicity;
		unknown_percent = percent;
		if DemographicCategory not in ('Non-Hispanic Other Race', 'Unknown') then delete;
run;
	
data unknown_sex;
	keep DemographicType DemographicCategory unknown_percent;
	set unknown_sex;	
		format DemographicType $20. DemographicCategory $50.;
		DemographicType = "Sex";
		DemographicCategory = recip_sex;
		unknown_percent = percent;
		if DemographicCategory not in ('U') then delete;
run;

data unknown_stack;
	set unknown_Race unknown_race2 unknown_race3 unknown_race4 unknown_sex;
run;


/****/
ods output CrossTabFreqs = unknown_race_cty;
proc freq data = tableau_imms;
	where people_count = 1;
	table rarest_race*county / missing;
	format rarest_race race.;
run;

ods output CrossTabFreqs = unknown_race_cty2;
proc freq data = tableau_imms;
	where people_count = 1;
	table race*county / missing;
run;

ods output CrossTabFreqs = unknown_race_cty3;
proc freq data = tableau_imms;
	where people_count = 1;
	table ethnicity*county / missing;
run;

ods output CrossTabFreqs = unknown_race_cty4;
proc freq data = tableau_imms;
	where people_count = 1;
	table race_ethnicity*county / missing;
run;

ods output CrossTabFreqs = unknown_sex_cty;
proc freq data = tableau_imms;
	where people_count = 1;
	table recip_sex*county / missing;
	format recip_address_county county.;
run;

data unknown_race_cty;
	keep DemographicType DemographicCategory county unknown_percent;
	set unknown_race_cty;
		format DemographicType $20. DemographicCategory $50.;
		DemographicType = "Race";
		DemographicCategory = put(rarest_race, race.);
		unknown_percent = rowpercent;
		if DemographicCategory not in ('Other Race', 'Unknown') then delete;
		if county = '' then delete;
		if rowpercent = . then delete;
run;
	
data unknown_race_cty2;
	keep DemographicType DemographicCategory county unknown_percent;
	set unknown_race_cty2;
		format DemographicType $20. DemographicCategory $50.;
		DemographicType = "Race OMB";
		DemographicCategory = race;
		unknown_percent = rowpercent;
		if DemographicCategory not in ('Other Race', 'Unknown') then delete;
		if county = '' then delete;
		if rowpercent = . then delete;
run;

data unknown_race_cty3;
	keep DemographicType DemographicCategory county unknown_percent;
	set unknown_race_cty3;
		format DemographicType $20. DemographicCategory $50.;
		DemographicType = "Ethnicity OMB";
		DemographicCategory = ethnicity;
		unknown_percent = rowpercent;
		if DemographicCategory not in ('Other Race', 'Unknown') then delete;
		if county = '' then delete;
		if rowpercent = . then delete;
run;

data unknown_race_cty4;
	keep DemographicType DemographicCategory county unknown_percent;
	set unknown_race_cty4;
		format DemographicType $20. DemographicCategory $50.;
		DemographicType = "Race & Ethnicity OMB";
		DemographicCategory = race_ethnicity;
		unknown_percent = rowpercent;
		if DemographicCategory not in ('Non-Hispanic Other Race', 'Unknown') then delete;
		if county = '' then delete;
		if rowpercent = . then delete;
run;

data unknown_sex_cty;
	keep DemographicType DemographicCategory county unknown_percent;
	set unknown_sex_cty;	
		format DemographicType $20. DemographicCategory $50.;
		DemographicType = "Sex";
		DemographicCategory = recip_sex;
		unknown_percent = rowpercent;
		if DemographicCategory not in ('U') then delete;
		if county = '' then delete;
		if rowpercent = . then delete;
run;

data unknown_stack_cty;
	set unknown_Race_cty unknown_Race_cty2 unknown_Race_cty3 unknown_Race_cty4 unknown_sex_cty;
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
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/* Get administrations, people count, in progress and vaccinated by day*/
/* Use a macro to do this for All Oregonians and broken down by demographics */
/* Also does by county */

proc sql;
	create table by_day as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,
				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,

				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms
	group by date, elig_boost1_date, vaccine, vaccine_boost;
quit;

proc sql;
	create table by_day_age as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				age_cat,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms
	group by date, elig_boost1_date, vaccine, vaccine_boost, age_cat;
quit;

proc sql;
	create table by_day_age2 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				age_cat2,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms
	group by date, elig_boost1_date, vaccine, vaccine_boost, age_cat2;
quit;

proc sql;
	create table by_day_age3 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				age_cat3,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms
	group by date, elig_boost1_date, vaccine, vaccine_boost, age_cat3;
quit;

proc sql;
	create table by_day_sex as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				recip_sex,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms
	group by date, elig_boost1_date, vaccine, vaccine_boost, recip_sex;
quit;

proc sql;
	create table by_day_re as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				rarest_race,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms
	group by date, elig_boost1_date, vaccine, vaccine_boost, rarest_race;
quit;

proc sql;
	create table by_day_re2 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				race,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms
	group by date, elig_boost1_date, vaccine, vaccine_boost, race;
quit;

proc sql;
	create table by_day_re3 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				ethnicity,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms
	group by date, elig_boost1_date, vaccine, vaccine_boost, ethnicity;
quit;

proc sql;
	create table by_day_re4 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				race_ethnicity,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms
	group by date, elig_boost1_date, vaccine, vaccine_boost, race_ethnicity;
quit;

*****;
data by_day;
	set by_day;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "All Oregonians";
	DemographicCategory = "All Oregonians";
run;

data by_day_age;
	set by_day_age;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Age Groups";
	DemographicCategory = put(age_cat, age_dis.);
run;

data by_day_age2;
	set by_day_age2;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Age Groups";
	DemographicCategory = age_cat2;
run;

data by_day_age3;
	set by_day_age3;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Age Groups";
	DemographicCategory = age_cat3;
	if DemographicCategory not in ('20 to 49', '50 to 64') then delete;
run;

data by_day_sex;
	set by_day_sex;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Sex";
	DemographicCategory = recip_sex;
run;

data by_day_re;
	set by_day_re;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Race";
	DemographicCategory = put(rarest_race, race.);
run;

data by_day_re2;
	set by_day_re2;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Race OMB";
	DemographicCategory = race;
run;

data by_day_re3;
	set by_day_re3;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Ethnicity OMB";
	DemographicCategory = ethnicity;
run;

data by_day_re4;
	set by_day_re4;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Race & Ethnicity OMB";
	DemographicCategory = race_ethnicity;
run;

*******;
proc sql;
	create table by_day_cty as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,
				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				recip_address_county as recip_county,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms
	group by date, elig_boost1_date, vaccine, vaccine_boost, recip_county;
quit;

proc sql;
	create table by_day_age_cty as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,
				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				age_cat,
				recip_address_county as recip_county,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms
	group by date, elig_boost1_date, vaccine, vaccine_boost, age_cat, recip_county;
quit;

proc sql;
	create table by_day_age_cty2 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,
				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				age_cat2,
				recip_address_county as recip_county,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms
	group by date, elig_boost1_date, vaccine, vaccine_boost, age_cat2, recip_county;
quit;

proc sql;
	create table by_day_age_cty3 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,
				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				age_cat3,
				recip_address_county as recip_county,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms
	group by date, elig_boost1_date, vaccine, vaccine_boost, age_cat3, recip_county;
quit;

proc sql;
	create table by_day_sex_cty as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,
				sum(up_to_date) as up_to_date,
				sum(needs_boost) as needs_boost,
				sum(needs_boost_now) as needs_boost_now,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				recip_sex,
				recip_address_county as recip_county,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms
	group by date, elig_boost1_date, vaccine, vaccine_boost, recip_sex, recip_county;
quit;

proc sql;
	create table by_day_re_cty as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,
				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				rarest_race,
				recip_address_county as recip_county,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms
	group by date, elig_boost1_date, vaccine, vaccine_boost, rarest_race, recip_county;
quit;

proc sql;
	create table by_day_re_cty2 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,
				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				race,
				recip_address_county as recip_county,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms
	group by date, elig_boost1_date, vaccine, vaccine_boost, race, recip_county;
quit;

proc sql;
	create table by_day_re_cty3 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,
				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				ethnicity,
				recip_address_county as recip_county,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms
	group by date, elig_boost1_date, vaccine, vaccine_boost, ethnicity, recip_county;
quit;

proc sql;
	create table by_day_re_cty4 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,
				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				race_ethnicity,
				recip_address_county as recip_county,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms
	group by date, elig_boost1_date, vaccine, vaccine_boost, race_ethnicity, recip_county;
quit;

proc sql;
	create table by_day_re_rgn as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,
				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				rarest_race,
				region as region,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms
	group by date, elig_boost1_date, vaccine, vaccine_boost, rarest_race, region;
quit;

*****;
data by_day_cty;
	set by_day_cty;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "All Oregonians";
	DemographicCategory = "All Oregonians";
	county = put(recip_county, county.);
	geography = 'County';
	*county = recip_county;
run;

data by_day_age_cty;
	set by_day_age_cty;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Age Groups";
	DemographicCategory = put(age_cat, age_dis.);
	county = put(recip_county, county.);
	geography = 'County';
	*county = recip_county;
run;

data by_day_age_cty2;
	set by_day_age_cty2;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Age Groups";
	DemographicCategory = age_cat2;
	county = put(recip_county, county.);
	geography = 'County';
	*county = recip_county;
run;

data by_day_age_cty3;
	set by_day_age_cty3;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Age Groups";
	DemographicCategory = age_cat3;
	county = put(recip_county, county.);
	geography = 'County';
	if DemographicCategory not in ('20 to 49', '50 to 64') then delete;
	*county = recip_county;
run;

data by_day_sex_cty;
	set by_day_sex_cty;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Sex";
	DemographicCategory = recip_sex;
	county = put(recip_county, county.);
	geography = 'County';
	*county = recip_county;
run;

data by_day_re_cty;
	set by_day_re_cty;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Race";
	DemographicCategory = put(rarest_race, race.);
	county = put(recip_county, county.);
	geography = 'County';
	*county = recip_county;
run;

data by_day_re_cty2;
	set by_day_re_cty2;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Race OMB";
	DemographicCategory = race;
	county = put(recip_county, county.);
	geography = 'County';
	*county = recip_county;
run;

data by_day_re_cty3;
	set by_day_re_cty3;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Ethnicity OMB";
	DemographicCategory = ethnicity;
	county = put(recip_county, county.);
	geography = 'County';
	*county = recip_county;
run;

data by_day_re_cty4;
	set by_day_re_cty4;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Race & Ethnicity OMB";
	DemographicCategory = race_ethnicity;
	county = put(recip_county, county.);
	geography = 'County';
	*county = recip_county;
run;

data by_day_re_rgn;
	set by_day_re_rgn;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Race";
	DemographicCategory = put(rarest_race, race.);
	geography = 'County';
	*county = recip_county;
run;


/********************/

data tableau_imms16;
	set tableau_imms;
	if age_cat >= 3 AND age_cat ^= . then output;
run;

proc sql;
	create table by_day16 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,
				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms16
	group by date, elig_boost1_date, vaccine, vaccine_boost;
quit;

proc sql;
	create table by_day_age16 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				age_cat,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms16
	group by date, elig_boost1_date, vaccine, vaccine_boost, age_cat;
quit;

proc sql;
	create table by_day_age216 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				age_cat2,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms16
	group by date, elig_boost1_date, vaccine, vaccine_boost, age_cat2;
quit;

proc sql;
	create table by_day_age316 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				age_cat3,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms16
	group by date, elig_boost1_date, vaccine, vaccine_boost, age_cat3;
quit;

proc sql;
	create table by_day_sex16 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				recip_sex,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms16
	group by date, elig_boost1_date, vaccine, vaccine_boost, recip_sex;
quit;

proc sql;
	create table by_day_re16 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,

				rarest_race,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms16
	group by date, elig_boost1_date, vaccine, vaccine_boost, rarest_race;
quit;

proc sql;
	create table by_day_re216 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,

				race,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms16
	group by date, elig_boost1_date, vaccine, vaccine_boost, race;
quit;

proc sql;
	create table by_day_re316 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,

				ethnicity,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms16
	group by date, elig_boost1_date, vaccine, vaccine_boost, ethnicity;
quit;

proc sql;
	create table by_day_re416 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,

				race_ethnicity,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms16
	group by date, elig_boost1_date, vaccine, vaccine_boost, race_ethnicity;
quit;

*****;
data by_day16;
	set by_day16;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "All Oregonians";
	DemographicCategory = "All Oregonians";
run;

data by_day_age16;
	set by_day_age16;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Age Groups";
	DemographicCategory = put(age_cat, age_dis.);
run;

data by_day_age216;
	set by_day_age216;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Age Groups";
	DemographicCategory = age_cat2;
run;

data by_day_age316;
	set by_day_age316;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Age Groups";
	DemographicCategory = age_cat3;
	if DemographicCategory not in ('20 to 49', '50 to 64') then delete;
run;

data by_day_sex16;
	set by_day_sex16;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Sex";
	DemographicCategory = recip_sex;
run;

data by_day_re16;
	set by_day_re16;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Race";
	DemographicCategory = put(rarest_race, race.);
run;

data by_day_re216;
	set by_day_re216;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Race OMB";
	DemographicCategory = race;
run;

data by_day_re316;
	set by_day_re316;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Ethnicity OMB";
	DemographicCategory = ethnicity;
run;

data by_day_re416;
	set by_day_re416;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Race & Ethnicity OMB";
	DemographicCategory = race_ethnicity;
run;

*******;
proc sql;
	create table by_day_cty16 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,

				recip_address_county as recip_county,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
from tableau_imms16
	group by date, elig_boost1_date, vaccine, vaccine_boost, recip_county;
quit;

proc sql;
	create table by_day_age_cty16 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,

				age_cat,
				recip_address_county as recip_county,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms16
	group by date, elig_boost1_date, vaccine, vaccine_boost, age_cat, recip_county;
quit;

proc sql;
	create table by_day_age_cty216 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,

				age_cat2,
				recip_address_county as recip_county,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms16
	group by date, elig_boost1_date, vaccine, vaccine_boost, age_cat2, recip_county;
quit;

proc sql;
	create table by_day_age_cty316 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,

				age_cat3,
				recip_address_county as recip_county,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms16
	group by date, elig_boost1_date, vaccine, vaccine_boost, age_cat3, recip_county;
quit;

proc sql;
	create table by_day_sex_cty16 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				recip_sex,
				recip_address_county as recip_county,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms16
	group by date, elig_boost1_date, vaccine, vaccine_boost, recip_sex, recip_county;
quit;

proc sql;
	create table by_day_re_cty16 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,

				rarest_race,
				recip_address_county as recip_county,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms16
	group by date, elig_boost1_date, vaccine, vaccine_boost, rarest_race, recip_county;
quit;

proc sql;
	create table by_day_re_cty216 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				race,
				recip_address_county as recip_county,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms16
	group by date, elig_boost1_date, vaccine, vaccine_boost, race, recip_county;
quit;

proc sql;
	create table by_day_re_cty316 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,

				ethnicity,
				recip_address_county as recip_county,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms16
	group by date, elig_boost1_date, vaccine, vaccine_boost, ethnicity, recip_county;
quit;

proc sql;
	create table by_day_re_cty416 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

					sum(up_to_date) as up_to_date,
			sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,

				race_ethnicity,
				recip_address_county as recip_county,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms16
	group by date, elig_boost1_date, vaccine, vaccine_boost, race_ethnicity, recip_county;
quit;

proc sql;
	create table by_day_re_rgn16 as
		select admin_date as date,
				elig_boost1_date as elig_boost1_date,
				vaccine as vaccine,
				vaccine_boost as vaccine_boost,

				sum(up_to_date) as up_to_date,
				sum(needs_boost_now) as needs_boost_now,
				sum(needs_boost) as needs_boost,
				sum(booster) as booster,
				sum(booster_first) as booster_first,
				sum(booster_second) as booster_second,
				sum(booster_biv) as booster_biv,
				rarest_race,
				region as region,
				sum(vaccinated) as vaccinated,
				sum(people_count) as people_count
	from tableau_imms16
	group by date, elig_boost1_date, vaccine, vaccine_boost, rarest_race, region;
quit;

*****;
data by_day_cty16;
	set by_day_cty16;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "All Oregonians";
	DemographicCategory = "All Oregonians";
	county = put(recip_county, county.);
	geography = 'County';
	*county = recip_county;
run;

data by_day_age_cty16;
	set by_day_age_cty16;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Age Groups";
	DemographicCategory = put(age_cat, age_dis.);
	county = put(recip_county, county.);
	geography = 'County';
	*county = recip_county;
run;

data by_day_age_cty216;
	set by_day_age_cty216;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Age Groups";
	DemographicCategory = age_cat2;
	county = put(recip_county, county.);
	geography = 'County';
	*county = recip_county;
run;

data by_day_age_cty316;
	set by_day_age_cty316;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Age Groups";
	DemographicCategory = age_cat3;
	county = put(recip_county, county.);
	geography = 'County';
	if DemographicCategory not in ('20 to 49', '50 to 64') then delete;
	*county = recip_county;
run;

data by_day_sex_cty16;
	set by_day_sex_cty16;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Sex";
	DemographicCategory = recip_sex;
	county = put(recip_county, county.);
	geography = 'County';
	*county = recip_county;
run;

data by_day_re_cty16;
	set by_day_re_cty16;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Race";
	DemographicCategory = put(rarest_race, race.);
	county = put(recip_county, county.);
	geography = 'County';
	*county = recip_county;
run;

data by_day_re_cty216;
	set by_day_re_cty216;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Race OMB";
	DemographicCategory = race;
	county = put(recip_county, county.);
	geography = 'County';
	*county = recip_county;
run;

data by_day_re_cty316;
	set by_day_re_cty316;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Ethnicity OMB";
	DemographicCategory = ethnicity;
	county = put(recip_county, county.);
	geography = 'County';
	*county = recip_county;
run;

data by_day_re_cty416;
	set by_day_re_cty416;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Race & Ethnicity OMB";
	DemographicCategory = race_ethnicity;
	county = put(recip_county, county.);
	geography = 'County';
	*county = recip_county;
run;

data by_day_re_rgn16;
	set by_day_re_rgn16;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = "Race";
	DemographicCategory = put(rarest_race, race.);
	geography = 'County';
	*county = recip_county;
run;

/****************************/


data all;
	set by_day by_day_age by_day_age2 by_day_age3 by_day_sex by_day_re by_day_re2 by_day_re3 by_day_re4;
	drop age_cat age_cat2 age_cat3 recip_sex rarest_race race ethnicity race_ethnicity;
run;

data all_cty;
	set by_day_cty by_day_age_cty by_day_age_cty2 by_day_age_cty3 by_day_sex_cty 
		by_day_re_cty by_day_re_cty2 by_day_re_cty3 by_day_re_cty4;
	drop age_cat age_cat2 age_cat3 recip_sex rarest_race race ethnicity race_ethnicity recip_county;
run;


data all16;
	set by_day16 by_day_age16 by_day_age216 by_day_age316 by_day_sex16 by_day_re16 by_day_re216 by_day_re316 by_day_re416;
	drop age_cat age_cat2 age_cat3 recip_sex rarest_race race ethnicity race_ethnicity;
	rename  vaccinated = vaccinated16 people_count = people_count16 
			needs_boost = needs_boost16 needs_boost_now = needs_boost_now16
			booster = booster16 booster_first = booster_first16 booster_second = booster_second16 eligible = eligible16 up_to_date = up_to_date16 booster_biv = booster_biv16;
run;

data all_cty16;
	set by_day_cty16 by_day_age_cty16 by_day_age_cty216 by_day_age_cty316 by_day_sex_cty16 
		by_day_re_cty16 by_day_re_cty216 by_day_re_cty316 by_day_re_cty416;
	drop age_cat age_cat2 age_cat3 recip_sex rarest_race race ethnicity race_ethnicity recip_county;
	rename  vaccinated = vaccinated16 people_count = people_count16 
			needs_boost = needs_boost16 needs_boost_now = needs_boost_now16
			booster = booster16 booster_first = booster_first16 booster_second = booster_second16 eligible = eligible16 up_to_date = up_to_date16 booster_biv = booster_biv16 ;
run;

/* Merge tables together */
proc sort data = all; by date elig_boost1_date vaccine vaccine_boost DemographicType DemographicCategory ; run;
proc sort data = all16; by date elig_boost1_date vaccine vaccine_boost DemographicType DemographicCategory ; run;

data alert_merge;
	merge all all16;
	 by date elig_boost1_date vaccine vaccine_boost DemographicType DemographicCategory ;
run;


/* Merge county tables together */
proc sort data = all_cty; by date elig_boost1_date vaccine vaccine_boost DemographicType DemographicCategory County ; run;
proc sort data = all_cty16; by date elig_boost1_date vaccine vaccine_boost DemographicType DemographicCategory County ; run;

data alert_merge_cty;
	merge all_cty all_cty16;
	 by date elig_boost1_date vaccine vaccine_boost DemographicType DemographicCategory County ;
run;

/*******************************/
/*******************************/
/*******************************/

ods output CrossTabFreqs = white_percent;
proc freq data = tableau_imms;
	where people_count = 1;
	table county*rarest_race / nocol missing;
	format rarest_race race.;
run;

data white_percent;
	keep DemographicType county white_percent;
	set white_percent;
	format DemographicType $40.;
	DemographicType = 'Race';
	if county = '' then do;
		white_percent = percent/100;
	end;
	if county ^= '' then do;
		white_percent = rowpercent/100;
	end;
	if county = '' and rowpercent ^= . then delete;
	if rarest_race ^= 6 then delete;
run;

ods output CrossTabFreqs = white_percent16;
proc freq data = tableau_imms16;
	where people_count = 1;
	table county*rarest_race / nocol missing;
	format rarest_race race.;
run;

data white_percent16;
	keep DemographicType county white_percent16;
	set white_percent16;
	format DemographicType $40.;
	DemographicType = 'Race';
	if county = '' then do;
		white_percent16 = percent/100;
	end;
	if county ^= '' then do;
		white_percent16 = rowpercent/100;
	end;
	if county = '' and rowpercent ^= . then delete;
	if rarest_race ^= 6 then delete;
run;

proc sort data = white_percent;
by county;
run;

proc sort data = white_percent16;
by county;
run;

data white_percent_state;
	merge white_percent white_percent16;
	by county;
	if county = '' then output;
run;

data white_percent_county;
	merge white_percent white_percent16;
	by county;
	if county ^= '' then output;
run;

/*******************************/
/*******************************/
/*****************************/
proc freq data = alert_merge;
	table DemographicCategory / missing;
run;
/* Merge state data with state population */
proc sort data = alert_merge; by DemographicType DemographicCategory; run;
proc sort data = pop.state_demog_PSU_2021; by DemographicType DemographicCategory; run;

data alert_state;
	merge alert_merge (in = a) pop.state_demog_PSU_2021 (in = b);
	by DemographicType DemographicCategory; 
	if a = 1 then output;
run;

/* Merge unknown percent */
proc sort data = alert_state; by DemographicType DemographicCategory; run;
proc sort data = unknown_stack; by DemographicType DemographicCategory; run;

data alert_state;
	merge alert_state unknown_stack;
	by DemographicType DemographicCategory; 
run;

/*Merge white percent */
proc sort data = alert_state; by DemographicType; run;
proc sort data = white_percent_state; by DemographicType ; run;

data alert_state;
	merge alert_state white_percent_state;
	by DemographicType; 
run;

/******************/
/*******************/
/*****************/

proc sort data = alert_merge_cty; by DemographicType DemographicCategory county; run;
proc sort data = unknown_stack_cty; by DemographicType DemographicCategory county; run;

data alert_county;
	merge alert_merge_cty unknown_stack_cty;
	by DemographicType DemographicCategory county; 
run;

/* Merge county data to population data */
proc sort data = alert_county; by County  DemographicType DemographicCategory; run;
proc sort data = pop.county_demog_PSU_2021; by County  DemographicType DemographicCategory; run;

data alert_county;
	merge alert_county (in = a) pop.county_demog_PSU_2021 (in = b);
	by County  DemographicType DemographicCategory; 
	if a = 1 then output;
run;

/*Merge white percent */
proc sort data = alert_county; by DemographicType county; run;
proc sort data = white_percent_county; by DemographicType county; run;

data alert_county;
	merge alert_county white_percent_county;
	by DemographicType county; 
run;

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*
data alert_st_ref;
	keep date cvx people_ref people_ref18 ref_pop ref_pop18;
	set alert_state;
	people_ref = people_count;
	people_ref18 = people_count16;
	ref_pop = population;
	ref_pop18 = population18;
	if DemographicType = 'All Oregonians' then output;
run;

proc sort data = alert_st_ref;
 by date cvx ;
 run;

proc sort data = alert_state;
 by date cvx ;
 run;

data alert_state;
	merge alert_state alert_St_ref;
 	by date cvx ;
run;
*/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/* Suppress R/E */
proc contents data = alert_county order = varnum;run;

proc sql;
	create table alert_race_cty as
		select 	DemographicType,
				DemographicCategory,
				County,
				sum(people_count) as people_count_old,
				sum(vaccinated) as vaccinated_old,
				sum(booster) as booster_old,
				sum(booster_first) as booster_first_old,
				sum(booster_second) as booster_second_old,
				sum(booster_biv) as booster_biv_old,
				sum(needs_boost_now) as needs_boost_now_old,
				sum(needs_boost) as needs_boost_old,
				sum(people_count16) as people_count16_old,
				sum(vaccinated16) as vaccinated16_old,
				sum(booster16) as booster16_old,
				sum(booster_first16) as booster_first16_old,
				sum(booster_second16) as booster_second16_old,
				sum(booster_biv16) as booster_biv16_old,
				sum(needs_boost_now16) as needs_boost_now16_old,
				sum(needs_boost16) as needs_boost16_old,
				avg(population) as population,
				avg(population18) as population18
			from alert_county
			group by DemographicType, DemographicCategory, County;
quit;

data alert_race_cty;
	set alert_race_cty;
	if DemographicCategory = 'Other Race' then do;
		population = .;
		population18 =.;
	end;
	if DemographicType not in ('Race') then delete;
	difference = 0.8*population - people_count_old;
	difference16 = 0.8*population18 - people_count16_old;
	if difference < 0 then do; 
		difference = 0;
	end;
	if difference16 < 0 then do; 
		difference16 = 0;
	end;
	if county = ' ' and population = . then delete;
run;


data alert_race_cty;
	set alert_race_cty;
	format   people_percent_label
			  people_percent16_label
			  vacc_percent_label
			  vacc_percent16_label
			  boost1_percent_label
			  boost1_percent16_label
			  boost2_percent_label
			  boost2_percent16_label
			  boost_percent_label
			  boost_biv_percent16_label
			  boost_biv_percent_label
			  boost_percent16_label
				boost_eligible_label
				boost_eligible16_label $20.;

	people_count = people_count_old;
	vaccinated = vaccinated_old;
	booster = booster_old;
	booster_first = booster_first_old;
	booster_second = booster_second_old;
	booster_biv = booster_biv_old;
	needs_boost_now = needs_boost_now_old;
	needs_boost = needs_boost_old;

	people_count16 = people_count16_old;
	vaccinated16 = vaccinated16_old;
	booster16 = booster16_old;
	booster_first16 = booster_first16_old;
	booster_second16 = booster_second16_old;
	booster_biv16 = booster_biv16_old;
	needs_boost_now16 = needs_boost_now16_old;
	needs_boost16 = needs_boost16_old;

	people_percent = people_count_old/population;
	vacc_percent = vaccinated_old/population;
	boost1_percent = booster_first_old/population;
	boost2_percent = booster_second_old/population;
	boost_biv_percent = booster_biv_old/population;
	boost_percent = booster_old/population;

	people_percent16 = people_count16_old/ population18;
	vacc_percent16 = vaccinated16_old/ population18;
	boost1_percent16 = booster_first16_old/ population18;
	boost2_percent16 = booster_second16_old/ population18;
	boost_biv_percent16 = booster_biv16_old/ population18;
	boost_percent16 = booster16_old/ population18;

	boost_eligible = booster/(needs_boost_now + booster);
	boost_eligible16 = booster16/(needs_boost_now16 + booster16);
run;

data alert_race_cty;
	set alert_race_cty;
	if DemographicCategory = 'Other Race' then do;
		population = .;
		population18 = .;
	end;
	people_count_label = strip(put(people_count_old, comma32.));
	people_percent_label = strip(put(people_percent, percent10.1));
	people_count16_label = strip(put(people_count16_old, comma32.));
	people_percent16_label = strip(put(people_percent16, percent10.1));

	vaccinated_label = strip(put(vaccinated_old, comma32.));
	vacc_percent_label = strip(put(vacc_percent, percent10.1));
	vaccinated16_label = strip(put(vaccinated16_old, comma32.));
	vacc_percent16_label = strip(put(vacc_percent16, percent10.1));

	booster_label = strip(put(booster_old, comma32.));
	boost_percent_label = strip(put(boost_percent, percent10.1));
	booster16_label = strip(put(booster16_old, comma32.));
	boost_percent16_label = strip(put(boost_percent16, percent10.1));

	booster_first_label = strip(put(booster_first_old, comma32.));
	boost1_percent_label = strip(put(boost1_percent, percent10.1));
	booster_first16_label = strip(put(booster_first16_old, comma32.));
	boost1_percent16_label = strip(put(boost1_percent16, percent10.1));

	booster_second_label = strip(put(booster_second_old, comma32.));
	boost2_percent_label = strip(put(boost2_percent, percent10.1));
	booster_second16_label = strip(put(booster_second16_old, comma32.));
	boost2_percent16_label = strip(put(boost2_percent16, percent10.1));

	booster_biv_label = strip(put(booster_biv_old, comma32.));
	boost_biv_percent_label = strip(put(boost_biv_percent, percent10.1));
	booster_biv16_label = strip(put(booster_biv16_old, comma32.));
	boost_biv_percent16_label = strip(put(boost_biv_percent16, percent10.1));

	needs_boost_label = strip(put(needs_boost_old, comma32.));
	needs_boost16_label = strip(put(needs_boost16_old, comma32.));

	needs_boost_now_label = strip(put(needs_boost_now_old, comma32.));
	needs_boost_now16_label = strip(put(needs_boost_now16_old, comma32.));

	diff_label = strip(put(difference, comma32.));
	diff16_label = strip(put(difference16, comma32.));

	boost_eligible_label = strip(put(boost_eligible, percent10.1));
	boost_eligible16_label = strip(put(boost_eligible16, percent10.1));
	if people_count_old <=10 then do;
		people_count = 10;
		people_count_label = '10 or less';
	end;
	if vaccinated_old <=10 then do;
		vaccinated = 10;
		vaccinated_label = '10 or less';
	end;
	if booster_first_old <=10 then do;
		booster_first = 10;
		booster_first_label = '10 or less';
	end;
	if booster_second_old <=10 then do;
		booster_second = 10;
		booster_second_label = '10 or less';
	end;
	if booster_biv_old <=10 then do;
		booster_biv = 10;
		booster_biv_label = '10 or less';
	end;
	if booster_old <=10 then do;
		booster = 10;
		booster_label = '10 or less';
			if boost_eligible > 0.80 then do;
				boost_eligible = 0.809;
				boost_eligible_label = '80% or more';
			end;
			if boost_eligible <= 0.80 then do;
				boost_eligible = 0;
				boost_eligible_label = 'less than 80%';
			end;
	end;
	if needs_boost_old <=10 then do;
		needs_boost = 10;
		needs_boost_label = '10 or less';
	end;
	if needs_boost_now_old <=10 then do;
		needs_boost_now = 10;
		needs_boost_now_label = '10 or less';
	end;
	if people_count_old <=10 then do;
		diff_label = '10 or less';
		difference = 10;
	end;
	if people_count16_old <=10 then do;
		people_count16 = 10;
		people_count16_label = '10 or less';
	end;
	if vaccinated16_old <=10 then do;
		vaccinated16 = 10;
		vaccinated16_label = '10 or less';
	end;
	if booster_first16_old <=10 then do;
		booster_first16 = 10;
		booster_first16_label = '10 or less';
	end;
	if booster_second16_old <=10 then do;
		booster_second16 = 10;
		booster_second16_label = '10 or less';
	end;
	if booster_biv16_old <=10 then do;
		booster_biv16 = 10;
		booster_biv16_label = '10 or less';
	end;
	if booster16_old <=10 then do;
		booster16 = 10;
		booster16_label = '10 or less';
			if boost_eligible16 > 0.80 then do;
				boost_eligible16 = 0.809;
				boost_eligible16_label = '80% or more';
			end;
			if boost_eligible16 <= 0.80 then do;
				boost_eligible16 = 0;
				boost_eligible16_label = 'less than 80%';
			end;
	end;
	if needs_boost16_old <=10 then do;
		needs_boost16 = 10;
		needs_boost16_label = '10 or less';
	end;
	if needs_boost_now16_old <=10 then do;
		needs_boost_now16 = 10;
		needs_boost_now16_label = '10 or less';
	end;
	if people_count16_old <=10 then do;
		diff16_label = '10 or less';
		difference16 = 10;
	end;
	if DemographicCategory not in ('Other Race', 'Unknown') then do;
		if people_count_old <=10 OR population < 50 then do;
			diff_label = '50 or less';
			if people_percent > 0.80 then do;
				people_percent = 0.809;
				people_percent_label = '80% or more';
				people_count_label = strip(put(people_count_old, suppress.));
			end;
			if people_percent <= 0.80 then do;
				people_percent = 0;
				people_percent_label = 'less than 80%';
				people_count_label = strip(put(people_count_old, suppress.));
			end;
		end;
		if vaccinated_old <=10 OR population < 50 then do;
			if vacc_percent > 0.80 then do;
				vacc_percent = 0.809;
				vacc_percent_label = '80% or more';
				vaccinated_label = strip(put(vaccinated_old, suppress.));
			end;
			if vacc_percent <= 0.80 then do;
				vacc_percent = 0;
				vacc_percent_label = 'less than 80%';
				vaccinated_label = strip(put(vaccinated_old, suppress.));
			end;
		end;
		if booster_first_old <=10 OR population < 50 then do;
			if boost1_percent > 0.80 then do;
				boost1_percent = 0.809;
				boost1_percent_label = '80% or more';
				booster_first_label = strip(put(booster_first_old, suppress.));
			end;
			if boost1_percent <= 0.80 then do;
				boost1_percent = 0;
				boost1_percent_label = 'less than 80%';
				booster_first_label = strip(put(booster_first_old, suppress.));
			end;
		end;
		if booster_second_old <=10 OR population < 50 then do;
			if boost2_percent > 0.80 then do;
				boost2_percent = 0.809;
				boost2_percent_label = '80% or more';
				booster_second_label = strip(put(booster_second_old, suppress.));
			end;
			if boost2_percent <= 0.80 then do;
				boost2_percent = 0;
				boost2_percent_label = 'less than 80%';
				booster_second_label = strip(put(booster_second_old, suppress.));
			end;
		end;
		if booster_biv_old <=10 OR population < 50 then do;
			if boost_biv_percent > 0.80 then do;
				boost_biv_percent = 0.809;
				boost_biv_percent_label = '80% or more';
				booster_biv_label = strip(put(booster_biv_old, suppress.));
			end;
			if boost_biv_percent <= 0.80 then do;
				boost_biv_percent = 0;
				boost_biv_percent_label = 'less than 80%';
				booster_biv_label = strip(put(booster_biv_old, suppress.));
			end;
		end;
		if booster_old <=10 OR population < 50 then do;
			if boost_percent > 0.80 then do;
				boost_percent = 0.809;
				boost_percent_label = '80% or more';
				booster_label = strip(put(booster_old, suppress.));
			end;
			if boost_percent <= 0.80 then do;
				boost_percent = 0;
				boost_percent_label = 'less than 80%';
				booster_label = strip(put(booster_old, suppress.));
			end;
			if boost_eligible > 0.80 then do;
				boost_eligible = 0.809;
				boost_eligible_label = '80% or more';
			end;
			if boost_eligible <= 0.80 then do;
				boost_eligible = 0;
				boost_eligible_label = 'less than 80%';
			end;
		end;
		if needs_boost_old <=10 OR population < 50 then do;
			needs_boost_label = strip(put(needs_boost_old, suppress.));
		end;
		if needs_boost_now_old <=10 OR population < 50 then do;
			needs_boost_now_label = strip(put(needs_boost_now_old, suppress.));
		end;
		if people_count16_old <=10 OR population < 50 then do;
			diff16_label = '50 or less';
			if people_percent16 > 0.80 then do;
				people_percent16 = 0.809;
				people_percent16_label = '80% or more';
				people_count16_label = strip(put(people_count16_old, suppress.));
			end;
			if people_percent16 <= 0.80 then do;
				people_percent16 = 0;
				people_percent16_label = 'less than 80%';
				people_count16_label = strip(put(people_count16_old, suppress.));
			end;
		end;
		if vaccinated16_old <=10 OR population < 50 then do;
			if vacc_percent16 > 0.80 then do;
				vacc_percent16 = 0.809;
				vacc_percent16_label = '80% or more';
				vaccinated16_label = strip(put(vaccinated16_old, suppress.));
			end;
			if vacc_percent16 <= 0.80 then do;
				vacc_percent16 = 0;
				vacc_percent16_label = 'less than 80%';
				vaccinated16_label = strip(put(vaccinated16_old, suppress.));
			end;
		end;
		if booster_first16_old <=10 OR population < 50 then do;
			if boost1_percent16 > 0.80 then do;
				boost1_percent16 = 0.809;
				boost1_percent16_label = '80% or more';
				booster_first16_label = strip(put(booster_first16_old, suppress.));
			end;
			if boost1_percent16 <= 0.80 then do;
				boost1_percent16 = 0;
				boost1_percent16_label = 'less than 80%';
				booster_first16_label = strip(put(booster_first16_old, suppress.));
			end;
		end;
		if booster_second16_old <=10 OR population < 50 then do;
			if boost2_percent16 > 0.80 then do;
				boost2_percent16 = 0.809;
				boost2_percent16_label = '80% or more';
				booster_second16_label = strip(put(booster_second16_old, suppress.));
			end;
			if boost2_percent16 <= 0.80 then do;
				boost2_percent16 = 0;
				boost2_percent16_label = 'less than 80%';
				booster_second16_label = strip(put(booster_second16_old, suppress.));
			end;
		end;
		if booster_biv16_old <=10 OR population < 50 then do;
			if boost_biv_percent16 > 0.80 then do;
				boost_biv_percent16 = 0.809;
				boost_biv_percent16_label = '80% or more';
				booster_biv16_label = strip(put(booster_biv16_old, suppress.));
			end;
			if boost_biv_percent16 <= 0.80 then do;
				boost_biv_percent16 = 0;
				boost_biv_percent16_label = 'less than 80%';
				booster_biv16_label = strip(put(booster_biv16_old, suppress.));
			end;
		end;
		if booster16_old <=10 OR population < 50 then do;
			if boost_percent16 > 0.80 then do;
				boost_percent16 = 0.809;
				boost_percent16_label = '80% or more';
				booster16_label = strip(put(booster16_old, suppress.));
			end;
			if boost_percent16 <= 0.80 then do;
				boost_percent16 = 0;
				boost_percent16_label = 'less than 80%';
				booster16_label = strip(put(booster16_old, suppress.));
			end;
			if boost_eligible16 > 0.80 then do;
				boost_eligible16 = 0.809;
				boost_eligible16_label = '80% or more';
			end;
			if boost_eligible16 <= 0.80 then do;
				boost_eligible16 = 0;
				boost_eligible16_label = 'less than 80%';
			end;
		end;
		if needs_boost16_old <=10 OR population < 50 then do;
			needs_boost16_label = strip(put(needs_boost16_old, suppress.));
		end;
		if needs_boost_now16_old <=10 OR population < 50 then do;
			needs_boost_now16_label = strip(put(needs_boost_now16_old, suppress.));
		end;
		if DemographicType = 'Race' AND DemographicCategory = 'Black' AND County in ('Lake', 'Harney') then do;
			if people_percent > 0.80 then do;
				people_percent = 0.809;
				people_percent_label = '80% or more';
				people_count_label = strip(put(people_count_old, suppress.));
			end;
			if people_percent <= 0.80 then do;
				people_percent = 0;
				people_percent_label = 'less than 80%';
				people_count_label = strip(put(people_count_old, suppress.));
			end;
			if vacc_percent > 0.80 then do;
				vacc_percent = 0.809;
				vacc_percent_label = '80% or more';
				vaccinated_label =  strip(put(vaccinated_old, suppress.));
			end;
			if vacc_percent <= 0.80 then do;
				vacc_percent = 0;
				vacc_percent_label = 'less than 80%';
				vaccinated_label =  strip(put(vaccinated_old, suppress.));
			end;
			if boost_percent > 0.80 then do;
				boost_percent = 0.809;
				boost_percent_label = '80% or more';
				booster_label = strip(put(booster_old, suppress.));
			end;
			if boost_percent <= 0.80 then do;
				boost_percent = 0;
				boost_percent_label = 'less than 80%';
				booster_label = strip(put(booster_old, suppress.));
			end;
			if boost1_percent > 0.80 then do;
				boost1_percent = 0.809;
				boost1_percent_label = '80% or more';
				booster_first_label = strip(put(booster_first_old, suppress.));
			end;
			if boost1_percent <= 0.80 then do;
				boost1_percent = 0;
				boost1_percent_label = 'less than 80%';
				booster_first_label = strip(put(booster_first_old, suppress.));
			end;
			if boost2_percent > 0.80 then do;
				boost2_percent = 0.809;
				boost2_percent_label = '80% or more';
				booster_first_label = strip(put(booster_first_old, suppress.));
			end;
			if boost2_percent <= 0.80 then do;
				boost2_percent = 0;
				boost2_percent_label = 'less than 80%';
				booster_first_label = strip(put(booster_first_old, suppress.));
			end;
			if boost_biv_percent > 0.80 then do;
				boost_biv_percent = 0.809;
				boost_biv_percent_label = '80% or more';
				booster_first_label = strip(put(booster_first_old, suppress.));
			end;
			if boost_biv_percent <= 0.80 then do;
				boost_biv_percent = 0;
				boost_biv_percent_label = 'less than 80%';
				booster_first_label = strip(put(booster_first_old, suppress.));
			end;

			if boost_eligible > 0.80 then do;
				boost_eligible = 0.809;
				boost_eligible_label = '80% or more';
			end;
			if boost_eligible <= 0.80 then do;
				boost_eligible = 0;
				boost_eligible_label = 'less than 80%';
			end;
			if people_percent16 > 0.80 then do;
				people_percent16 = 0.809;
				people_percent16_label = '80% or more';
				people_count16_label = strip(put(people_count16_old, suppress.));
			end;
			if people_percent16 <= 0.80 then do;
				people_percent16 = 0;
				people_percent16_label = 'less than 80%';
				people_count16_label = strip(put(people_count16_old, suppress.));
			end;
			if vacc_percent16 > 0.80 then do;
				vacc_percent16 = 0.809;
				vacc_percent16_label = '80% or more';
				vaccinated16_label = strip(put(vaccinated16_old, suppress.));
			end;
			if vacc_percent16 <= 0.80 then do;
				vacc_percent16 = 0;
				vacc_percent16_label = 'less than 80%';
				vaccinated16_label = strip(put(vaccinated16_old, suppress.));
			end;
			if boost1_percent16 > 0.80 then do;
				boost1_percent16 = 0.809;
				boost1_percent16_label = '80% or more';
				booster_first16_label = strip(put(booster_first16_old, suppress.));
			end;
			if boost1_percent16 <= 0.80 then do;
				boost1_percent16 = 0.;
				boost1_percent16_label = 'less than 80%';
				booster_first16_label = strip(put(booster_first16_old, suppress.));
			end;
			if boost2_percent16 > 0.80 then do;
				boost2_percent16 = 0.809;
				boost2_percent16_label = '80% or more';
				booster_first16_label = strip(put(booster_first16_old, suppress.));
			end;
			if boost2_percent16 <= 0.80 then do;
				boost2_percent16 = 0.;
				boost2_percent16_label = 'less than 80%';
				booster_first16_label = strip(put(booster_first16_old, suppress.));
			end;
			if boost_biv_percent16 > 0.80 then do;
				boost_biv_percent16 = 0.809;
				boost_biv_percent16_label = '80% or more';
				booster_first16_label = strip(put(booster_first16_old, suppress.));
			end;
			if boost_biv_percent16 <= 0.80 then do;
				boost_biv_percent16 = 0.;
				boost_biv_percent16_label = 'less than 80%';
				booster_first16_label = strip(put(booster_first16_old, suppress.));
			end;
			if boost_percent16 > 0.80 then do;
				boost_percent16 = 0.809;
				boost_percent16_label = '80% or more';
				booster16_label = strip(put(booster16_old, suppress.));
			end;
			if boost_percent16 <= 0.80 then do;
				boost_percent16 = 0.;
				boost_percent16_label = 'less than 80%';
				booster16_label = strip(put(booster16_old, suppress.));
			end;
			if boost_eligible16 > 0.80 then do;
				boost_eligible16 = 0.809;
				boost_eligible16_label = '80% or more';
			end;
			if boost_eligible16 <= 0.80 then do;
				boost_eligible16 = 0;
				boost_eligible16_label = 'less than 80%';
			end;
		end;
		/**************/
		if people_percent >= 1 then do;
			people_percent = .999;
			people_percent_label = strip(put(.999, percent10.1));
		end;
		if vacc_percent >=1 then do;
			vacc_percent = .999;
			vacc_percent_label = strip(put(.999, percent10.1));
		end;
		if boost1_percent >=1 then do;
			boost1_percent = .999;
			boost1_percent_label = strip(put(.999, percent10.1));
		end;
		if boost2_percent >=1 then do;
			boost2_percent = .999;
			boost2_percent_label = strip(put(.999, percent10.1));
		end;
		if boost_biv_percent >=1 then do;
			boost_biv_percent = .999;
			boost_biv_percent_label = strip(put(.999, percent10.1));
		end;
		if boost_percent >=1 then do;
			boost_percent = .999;
			boost_percent_label = strip(put(.999, percent10.1));
		end;
		if people_percent16 >=1 then do;
			people_percent16 = .999;
			people_percent16_label = strip(put(.999, percent10.1));
		end;
		if vacc_percent16 >=1 then do;
			vacc_percent16 = .999;
			vacc_percent16_label = strip(put(.999, percent10.1));
		end;
		if boost1_percent16 >=1 then do;
			boost1_percent16 = .999;
			boost1_percent16_label = strip(put(.999, percent10.1));
		end;
		if boost_percent16 >=1 then do;
			boost_percent16 = .999;
			boost_percent16_label = strip(put(.999, percent10.1));
		end;
	end;

	if people_percent_label = '.' then do;
		people_percent_label = 'less than 80%';
	end;
	if vaccinated_label = '.' then do;
		vaccinated_label = '10 or less';
	end;
	if vacc_percent_label = '.' then do;
		vacc_percent_label = 'less than 80%';
	end;
	if booster_label = '.' then do;
		booster_label = '10 or less';
	end;
	if boost_percent_label = '.' then do;
		boost_percent_label = 'less than 80%';
	end;
	if booster_first_label = '.' then do;
		booster_first_label = '10 or less';
	end;
	if boost1_percent_label = '.' then do;
		boost1_percent_label = 'less than 80%';
	end;
	if booster_second_label = '.' then do;
		booster_second_label = '10 or less';
	end;
	if boost2_percent_label = '.' then do;
		boost2_percent_label = 'less than 80%';
	end;
	if booster_biv_label = '.' then do;
		booster_biv_label = '10 or less';
	end;
	if boost_biv_percent_label = '.' then do;
		boost_biv_percent_label = 'less than 80%';
	end;
	if needs_boost_label = '.' then do;
		needs_boost_label = '10 or less';
	end;
	if needs_boost_now_label = '.' then do;
		needs_boost_now_label = '10 or less';
	end;
	if boost_eligible_label = '.' then do;
		boost_eligible_label = 'less than 80%';
	end;
	if people_count16_label = '.' then do;
		people_count16_label = '10 or less';
	end;
	if people_percent16_label = '.' then do;
		people_percent16_label = 'less than 80%';
	end;
	if vaccinated16_label = '.' then do;
		vaccinated16_label = '10 or less';
	end;
	if vacc_percent16_label = '.' then do;
		vacc_percent16_label = 'less than 80%';
	end;
	if booster_first16_label = '.' then do;
		booster_first16_label = '10 or less';
	end;
	if boost1_percent16_label = '.' then do;
		boost1_percent16_label = 'less than 80%';
	end;
	if booster_second16_label = '.' then do;
		booster_second16_label = '10 or less';
	end;
	if boost2_percent16_label = '.' then do;
		boost2_percent16_label = 'less than 80%';
	end;
	if booster_biv16_label = '.' then do;
		booster_biv16_label = '10 or less';
	end;
	if boost_biv_percent16_label = '.' then do;
		boost_biv_percent16_label = 'less than 80%';
	end;
	if booster16_label = '.' then do;
		booster16_label = '10 or less';
	end;
	if boost_percent16_label = '.' then do;
		boost_percent16_label = 'less than 80%';
	end;
	if needs_boost16_label = '.' then do;
		needs_boost16_label = '10 or less';
	end;
	if needs_boost_now16_label = '.' then do;
		needs_boost_now16_label = '10 or less';
	end;
	if boost_eligible16_label = '.' then do;
		boost_eligible16_label = 'less than 80%';
	end;
run;


data data.alert_race_county;
	keep County DemographicType DemographicCategory 
	people_count people_count_label people_percent people_percent_label
	vaccinated vaccinated_label vacc_percent vacc_percent_label
	booster_first booster_first_label boost1_percent boost1_percent_label 
	booster_second booster_second_label boost2_percent boost2_percent_label 
	booster_biv booster_biv_label boost_biv_percent boost_biv_percent_label 
	booster booster_label boost_percent boost_percent_label 
	needs_boost_now needs_boost_now_label
	boost_eligible boost_eligible_label
	difference diff_label
	population
	people_count16 people_count16_label people_percent16 people_percent16_label
	vaccinated16 vaccinated16_label vacc_percent16 vacc_percent16_label
	booster_first16 booster_first16_label boost1_percent16 boost1_percent16_label
	booster_second16 booster_second16_label boost2_percent16 boost2_percent16_label
	booster_biv16 booster_biv16_label boost_biv_percent16 boost_biv_percent16_label
	booster16 booster16_label boost_percent16 boost_percent16_label 
	needs_boost_now16  needs_boost_now16_label
	boost_eligible16 boost_eligible16_label
	difference16 diff16_label
	population18;
	retain County DemographicType DemographicCategory 
	people_count people_count_label people_percent people_percent_label
	vaccinated vaccinated_label vacc_percent vacc_percent_label
	booster_first booster_first_label boost1_percent boost1_percent_label 
	booster_second booster_second_label boost2_percent boost2_percent_label 
	booster_biv booster_biv_label boost_biv_percent boost_biv_percent_label 
	booster booster_label boost_percent boost_percent_label 
	needs_boost_now needs_boost_now_label
	boost_eligible boost_eligible_label
	difference diff_label
	population
	people_count16 people_count16_label people_percent16 people_percent16_label
	vaccinated16 vaccinated16_label vacc_percent16 vacc_percent16_label
	booster_first16 booster_first16_label boost1_percent16 boost1_percent16_label
	booster_second16 booster_second16_label boost2_percent16 boost2_percent16_label
	booster_biv16 booster_biv16_label boost_biv_percent16 boost_biv_percent16_label
	booster16 booster16_label boost_percent16 boost_percent16_label 
	needs_boost_now16  needs_boost_now16_label
	boost_eligible16 boost_eligible16_label
	difference16 diff16_label
	population18;
	set alert_race_cty;
run;

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/* Send to data folder for Tableau */


data data.alert_state;
	set alert_state;
	format reference todays_date fully_vaxd mmddyy10. vaccine $17.;
	reference = &today - 4;
	todays_date = &today;
	fully_vaxd = date + 14;
	if vaccine = ' ' then do;
		vaccine = put(cvx, cvx.);
	end;	
	if DemographicCategory = 'Other Race' then do; 
		percent = .;
		population = .;
		population16 = .;
		population18 = .;
		oregon_pop = .;
		oregon_pop16 = .;
		oregon_pop18 = .;
	end;
	*if date = &today then delete;
	rename elig_boost1_date = eligibility_date;
	*if DemographicType = 'Race2' then delete;
	drop cvx oregon_pop oregon_pop oregon_pop12 oregon_pop16 oregon_pop18 population12 population16;
	county = 'Oregon';
run;

proc freq data = alert_state;
	table vaccine / missing;
run;
/*
proc print data = data.alert_state_booster (obs = 5);
run;
*/
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

data data.alert_county;
	set alert_county;
	format reference todays_date fully_vaxd mmddyy10. vaccine $17.;
	reference = &today - 4;
	todays_date = &today;
		fully_vaxd = date + 14;
	if vaccine = ' ' then do;
		vaccine = put(cvx, cvx.);
	end;
	if DemographicCategory = 'Other Race' then do; 
		percent = .;
		population = .;
		population16 = .;
		population18 = .;
		oregon_pop = .;
		oregon_pop16 = .;
		oregon_pop18 = .;
	end;
	format region $100.;
	if county = 'Baker' then region = 'Umatilla, Union, Baker & Wallowa';
	if county = 'Benton' then region = 'Linn & Benton ';
	if county = 'Clackamas' then region = 'Clackamas';
	if county = 'Clatsop' then region = 'Columbia, Lincoln, Clatsop & Tillamook ';
	if county = 'Columbia' then region = 'Columbia, Lincoln, Clatsop & Tillamook ';
	if county = 'Coos' then region = 'Josephine, Coos & Curry ';
	if county = 'Crook' then region = 'Crook, Gilliam, Grant, Hood River, Jefferson, Morrow, Sherman, Wasco, & Wheeler ';
	if county = 'Curry' then region = 'Josephine, Coos & Curry ';
	if county = 'Deschutes' then region = 'Deschutes';
	if county = 'Douglas' then region = 'Douglas';
	if county = 'Gilliam' then region = 'Crook, Gilliam, Grant, Hood River, Jefferson, Morrow, Sherman, Wasco, & Wheeler ';
	if county = 'Grant' then region = 'Crook, Gilliam, Grant, Hood River, Jefferson, Morrow, Sherman, Wasco, & Wheeler ';
	if county = 'Harney' then region = 'Klamath, Malheur, Lake & Harney ';
	if county = 'Hood River' then region = 'Crook, Gilliam, Grant, Hood River, Jefferson, Morrow, Sherman, Wasco, & Wheeler ';
	if county = 'Jackson' then region = 'Jackson';
	if county = 'Jefferson' then region = 'Crook, Gilliam, Grant, Hood River, Jefferson, Morrow, Sherman, Wasco, & Wheeler ';
	if county = 'Josephine' then region = 'Josephine, Coos & Curry ';
	if county = 'Klamath' then region = 'Klamath, Malheur, Lake & Harney ';
	if county = 'Lake' then region = 'Klamath, Malheur, Lake & Harney ';
	if county = 'Lane' then region = 'Lane';
	if county = 'Lincoln' then region = 'Columbia, Lincoln, Clatsop & Tillamook ';
	if county = 'Linn' then region = 'Linn & Benton ';
	if county = 'Malheur' then region = 'Klamath, Malheur, Lake & Harney ';
	if county = 'Marion' then region = 'Marion';
	if county = 'Morrow' then region = 'Crook, Gilliam, Grant, Hood River, Jefferson, Morrow, Sherman, Wasco, & Wheeler ';
	if county = 'Multnomah' then region = 'Multnomah';
	if county = 'Polk' then region = 'Yamhill & Polk ';
	if county = 'Sherman' then region = 'Crook, Gilliam, Grant, Hood River, Jefferson, Morrow, Sherman, Wasco, & Wheeler ';
	if county = 'Tillamook' then region = 'Columbia, Lincoln, Clatsop & Tillamook ';
	if county = 'Umatilla' then region = 'Umatilla, Union, Baker & Wallowa ';
	if county = 'Union' then region = 'Umatilla, Union, Baker & Wallowa ';
	if county = 'Wallowa' then region = 'Umatilla, Union, Baker & Wallowa ';
	if county = 'Wasco' then region = 'Crook, Gilliam, Grant, Hood River, Jefferson, Morrow, Sherman, Wasco, & Wheeler ';
	if county = 'Washington' then region = 'Washington';
	if county = 'Wheeler' then region = 'Crook, Gilliam, Grant, Hood River, Jefferson, Morrow, Sherman, Wasco, & Wheeler ';
	if county = 'Yamhill' then region = 'Yamhill & Polk ';
	if county = ' ' then region = ' ';
	*if date = &today then delete;
	*if DemographicType = 'Race2' then delete;
	rename elig_boost1_date = eligibility_date;
	drop geography cvx oregon_pop oregon_pop oregon_pop12 oregon_pop16 oregon_pop18 population12 population16;
run;

proc sort data = data.alert_county; by county DemographicType DemographicCategory ; run;

proc export data = data.alert_state outfile='\\wpohaappl109\EPI\alert_state.csv' 
	dbms = csv replace;
run;

proc export data = data.alert_county outfile='\\wpohaappl109\EPI\alert_county.csv' 
	dbms = csv replace;
run;
/*
proc export data = data.alert_state outfile='\\wpohaappl109\EPI\alert_state_break.csv' 
	dbms = csv replace;
run;

proc export data = data.alert_county outfile='\\wpohaappl109\EPI\alert_county_break.csv' 
	dbms = csv replace;
run;
*/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*ods html close; ods html;*/
/**/
/*proc printto log   = log*/
/*             print = print*/
/*             new;*/
/*run;*/

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/* Output data for powerpoints */

ods output CrossTabFreqs = county_18scatter;
proc freq data = tableau_imms;
	where people_count = 1 and age >= 18;
	table county*people_count /norow nocol nopercent missing;
run;

data county_18scatter;
	keep county frequency;
	set county_18scatter;
	if people_count = . then delete;
	if _TYPE_ = 01 then delete;
run;

data county_18scatter_pop;
	keep county population18;
	set pop.county_demog_psu_2021;
	if DemographicType ^= 'All Oregonians' then delete;
run;

proc sort data = county_18scatter;
by county;
run;

proc sort data = county_18scatter_pop;
by county;
run;

data county_18scatter_final;
	merge county_18scatter county_18scatter_pop;
	by county;
	rename frequency = people_count population18 = population;
	rate = frequency/population18;
run;

proc export data = county_18scatter_final outfile='\\wpohaappl109\EPI\vax_by_county 18+.csv' 
	dbms = csv replace;
run;

/*****************************************************************************************************************************************/

data vax_rates_sarah1;
	set data.alert_county;
	if DemographicType = 'All Oregonians' then output;
run;

proc sql;
	create table vax_rates_sarah as
		select sum(people_count) as people_count,
				mean(population) as population,
				county
		from vax_rates_sarah1
		group by county;
quit;

data vax_rates_sarah;
	set vax_rates_sarah;
	percent_one_dose = people_count/population;
run;

proc export data = vax_rates_sarah outfile='\\wpohaappl109\EPI\vax_by_county.csv' 
	dbms = csv replace;
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
/*
proc freq data = tableau_imms;
	where people_count = 1 and rarest_race in (2,3);
	table age_cat*rarest_race /nopercent norow nocol missing;
	format age_Cat age_new. rarest_race race.;
run;

proc freq data = tableau_imms;
	where age_cat >= 3;
	table age*people_count /nopercent norow nocol missing;
	format age age.;
run;

proc print data = supply_change_all;
	where county = 'Polk' and DemographicType = 'All Oregonians';
run;

data vax_rates1;
	set data.alert_county;
	if DemographicType ^= 'All Oregonians' then delete;
run;

proc sql;
	create table vax_rates as
		select demographicType,
				county,
				sum(people_count16) as people_count16,
				population16
		from vax_rates1
		group by county;
quit;

proc sort data = vax_rates nodupkeys;
	by DemographicType county;
	run;

data vax_rates;
	set vax_rates;
	vax_rate16 = people_count16/population16;
run;

proc print data = vax_Rates;
run;


proc contents data = data.final_imms order = varnum;
run;

proc freq data = data.tableau_imms_boost;
	where people_count = 1;
	table ONEAPCICSHSRI*rarest_race /nopercent norow nocol missing;
	format rarest_race race.;
run;

proc freq data = data.final_imms;
	where lastid = 1;
	table ONEAPCICSHSRI*rarest_race /nopercent norow nocol missing;
	format rarest_race race.;
run;

proc freq data = data.final_imms;
	table firstid/nopercent norow nocol missing;
	format rarest_race race.;
run;

data for_apac;
	set data.tableau_imms_boost;
	if people_count = 1 and ONEAPCICSHSRI = '' and rarest_race in (8,10) then output;
run;

data for_apac;
	set data.final_imms;
	if lastid = 1 and ONEAPCICSHSRI = '' and rarest_race in (8,10,11,12) then output;
run;

proc freq data = data.final_imms;
	table rarest_race /nopercent norow nocol missing;
	format rarest_race;
run;
*/
