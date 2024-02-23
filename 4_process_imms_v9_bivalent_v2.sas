/******* 3a Process final_imms to the analytic data sets for CRRU: tableau_imms_boost (dose level) & completed_extra (person level)*/
*Paige Snow paige.snow@dhsoha.state.or.us;
*OHA CRRU;
*5/13/2022;

/*Clear log and and results output to reduce lag in processing */
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

/* Pick up final_imms from Monday from the data library. */
data final_imms1;
	set data.final_imms_&thismonday;
run;

*Remove duplicates;
proc sort data = final_imms1 nodupkey;
	by recip_id vax_event_id;
run;

proc freq data = final_imms1;
	table age_cat / missing; *age_cat2 hasn't been coded yet/;
	format age_cat age_dis.;
run;

proc contents data = final_imms1 order=collate;
	run;


/* Create variables for region, race OMB, ethnicity OMB, and race & ethnicity OMB */
data final_imms1;
	set final_imms1;
	if age < 65 then age_cat2 = 'Under 65';
	if age >=65 then age_cat2 = '65+';
	age_cat3 = put(age_cat, age_agg.);
	age_cat4 = put(age_cat, age_agg_two.);
	if age <= 4 then age_cat5 = 'Under 5';
	if age >= 5 AND age <= 49 then age_cat5 = '5 to 49';
	if age >= 50 then age_cat5 = '50+';

	vaccine = put(cvx, cvx.);
	vaccine_ped = put(cvx, cvx_ped.);
	vaccine_biv = put(cvx, cvx_biv.);

	format region $100.;
	county = put(recip_address_county, county.);
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
	if recip_id = . then delete;

	/*flag unknown ethnicity other race */
	if (recip_ethnicity = 'UNK' AND other = 1 AND AIAN = 0 AND Asian = 0 AND Black = 0 AND NHPI = 0 AND White = 0)then do;
		unk_eth_other = 1;
	end;

	/*ethnicity*/
	format ethnicity $30.;
	if recip_ethnicity = "2135-2" then do; ethnicity = "Hispanic"; end;
	if recip_ethnicity = "2186-5" then do; ethnicity = "Non-Hispanic"; end;
	if recip_ethnicity = "UNK" then do; ethnicity = "Unknown"; end;

	/*race*/
	/*	count missing and subtract from max possible to get number of non-missing*/
	numb_races = aian + asian + black + nhpi + white + other;

	format race $40.;
	if Race_unk = 0 and AIAN = 0 AND asian = 0 and black = 0 and nhpi = 0 and white = 0 and other = 0 then race = "Unknown";
	if Race_unk = 1 and AIAN = 0 AND asian = 0 and black = 0 and nhpi = 0 and white = 0 and other = 0 then race = "Unknown";
	if Race_unk = 0 and AIAN = 1 AND asian = 0 and black = 0 and nhpi = 0 and white = 0 and other = 0 then race = "AI/AN";
	if Race_unk = 0 and AIAN = 0 AND asian = 1 and black = 0 and nhpi = 0 and white = 0 and other = 0 then race = "Asian";
	if Race_unk = 0 and AIAN = 0 AND asian = 0 and black = 1 and nhpi = 0 and white = 0 and other = 0 then race = "Black";
	if Race_unk = 0 and AIAN = 0 AND asian = 0 and black = 0 and nhpi = 1 and white = 0 and other = 0 then race = "NH/PI";
	if Race_unk = 0 and AIAN = 0 AND asian = 0 and black = 0 and nhpi = 0 and white = 1 and other = 0 then race = "White";
	if Race_unk = 0 and AIAN = 0 AND asian = 0 and black = 0 and nhpi = 0 and white = 0 and other = 1 then race = "Other Race";
	if numb_races > 1 then race = "Multiracial";

	/*COMBINED RACE-ETHNICITY VARIABLE;*/
	format race_ethnicity $40.;	
	if race = 'AI/AN' then race_ethnicity = "Non-Hispanic AI/AN";
	if race = 'Asian' then race_ethnicity = "Non-Hispanic Asian";
	if race = 'NH/PI' then race_ethnicity = "Non-Hispanic NH/PI";
	if race = 'Black' then race_ethnicity = "Non-Hispanic Black";
	if race = 'White' then race_ethnicity = "Non-Hispanic White";
	if race = 'Other Race' then race_ethnicity = "Non-Hispanic Other Race";
	if (ethnicity = "Non-Hispanic" AND numb_races > 1) then do; race_ethnicity = "Non-Hispanic Multiracial"; end;
	if ethnicity = "Hispanic" then race_ethnicity = "Hispanic" ;
	if (Race_unk = 1 AND ethnicity = "Non-Hispanic") then race_ethnicity = "Unknown";
	if (race = "Unknown" AND ethnicity = "Non-Hispanic") then race_ethnicity = "Unknown";
	if ethnicity = "Unknown" then race_ethnicity = "Unknown";
run;
/*
*CHECKS for variable creation;
proc freq data = final_imms1; 
	table vaccine vaccine_ped vaccine_biv cvx/ missing;
run;

proc freq data = final_imms1; 
	table county*recip_address_county / missing;
run;

proc freq data = final_imms1; 
	table numb_Races*rarest_race*race / missing;
	format rarest_race race.;
run;

proc freq data = final_imms1; 
	table aian / missing;
run;
proc print data = final_imms1;
	where rarest_race = 4 and race = 'White';
	var recip_id rarest_race race ethnicity race_ethnicity AIAN asian black nhpi white other Race_unk;
run;

proc freq data = final_imms1;
	table recip_ethnicity*ethnicity / missing;
run;

proc freq data = final_imms1;
	table numb_races*aian
			numb_races*asian
			numb_races*black
			numb_races*nhpi
			numb_races*white
			numb_races*other
			numb_races*race_unk
/ missing;
run;

proc freq data = final_imms1;
	table race*aian
			race*asian
			race*black
			race*nhpi
			race*white
			race*other
			race*race_unk
			race*numb_races
/ missing;
run;

proc freq data = final_imms1;
	table rarest_race*race
/ missing;
run;

proc freq data = final_imms1;
	table race*ethnicity
			ethnicity*race_ethnicity
			race*race_ethnicity
/ missing;
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

/*Each vax_event_id/row is a unique dose administered, so give it a column for analysis */
proc sort data = final_imms1;	*always sort by the variable before data step;
	by vax_event_id;
run;

data final_imms1;
	set final_imms1;
	by vax_event_id;
	if first.vax_event_id then administrations = 1;
run;

proc freq data = final_imms1; *checks if there are any weird missing data in administration column;
	table administrations / missing;
run;

/*
*CHECKS;
proc freq data = final_imms1;
	where admin_date > (&today - 7) AND administrations = 1;
	table admin_date*cvx / missing;
	format cvx cvx.;
run;
*/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*
Determine demographics (age, r/e, county) by first and last vaccination event ids 
Age comes from their first dose, other geographies and demographics come from their most recent ID (per Vivian)
*/

/* 
Sort by the Person ID and the date the dose was administered. 
Admin_date is preferred over vax_event_id (newer vax_event_id's can actually be for old data).
*/
proc sort data=final_imms1; 
by recip_id admin_date;
run;

/* Labels the first and last rows for each person, by admin_date */
data demogv;
	set final_imms1;
	by recip_id admin_date;
	firstid= first.recip_id; *auto codes the first recip_id as 1;
	lastid= last.recip_id;
		if firstid=1 and lastid=1 then id=1; *only had one dose of some kind; * ! number doesn't correspond with the administration dose.;
		else if firstid=0 and lastid=1 then id=2; *not the first dose, is the last dose;
		else if firstid=1 and lastid=0 then id=3; *is the first dose, not the last dose;
		else if firstid=0 and lastid=0 then id=4; *not the first or last dose, is a dose in between;
run;

/* Keep last IDs and race, sex, county */
data last_ids;
	keep admin_date vax_event_id cvx recip_id rarest_race race ethnicity race_ethnicity unk_eth_other recip_sex 
			age age_cat age_cat2 age_cat3 age_cat4 age_cat5
			recip_address_zip recip_address_County county region;
	retain admin_date vax_event_id cvx recip_id rarest_race race ethnicity race_ethnicity unk_eth_other recip_sex 
			age age_cat age_cat2 age_cat3 age_cat4 age_cat5
			recip_address_zip recip_address_County county region;
	set demogv; 
	where id IN (1,2); *Only include, practically, the very last dose administered;
run;

/*Purpose: Used later*/
data last_event_id;
	keep admin_date vax_event_id vaccine recip_id;
	set last_ids;	
	vaccine = put(cvx, cvx.);
run;

/*Purpose: *one row per person, effectively creates a mastersheet of demographic variables for each person*/
data last_ids; 
	set last_ids;
	drop cvx admin_date vax_event_id; *recip_id is used to join it back to a truncated version of final imms later. This effectively replaces all administration/rows of a single person/recip_id (regardless of vax_event_id) with the most recent demographic variables;
run;

proc sort data=last_ids; 
by recip_id; 
run;

/* Keep first IDs and age */
data first_ids;
	keep admin_date recip_id vax_event_id;
	set demogv; 
	where id IN (1,3); *now, we take the age from the very first observed administration. This might change...;
run;

/*Purpose: Number of People Vaccinated (atleast one dose)*/
data first_event_id;
	keep admin_date vax_event_id recip_id people_count;
	set first_ids;
	people_count = 1; *people count means how many people completed one dose (got vaccinated). Does not mean fully vaccinated;
run;

/*Purpose: Keep the age at initiation*/
data first_ids;
	set first_ids;
	drop admin_date vax_event_id; *we are merging by recip_id again. This time we keep each person's initial age from first dose all the way through sequential doses;
run;

/* Merge first_ids and last_ids to get all demographics in one table by person ID. Age at initiation + up to date demographics*/
proc sort data=last_ids; 
by recip_id; 
run;
proc sort data=first_ids; 
by recip_id; 
run;

data first_last;
	merge last_ids first_ids;
	by recip_id;
run;

/*Make a person table to merge with the doses later, that only keeps a few necessary columns */
data last_age;
	set first_last;
	keep recip_id age age_cat5 county;
run;

/*
*CHECKS;
proc freq data = first_last;
	table age_cat age_cat2 rarest_race county
			county*age_cat county*rarest_race / norow nocol nopercent missing;
	format age_cat age. rarest_race race.;
run;

proc freq data = first_last;
	table age_cat*age_cat2 age_cat*age_cat3 / nowrow nocol nopercent missing;
	format age_cat age_dis.;
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
/* Determine # one dose (initiated), # primary series complete (1JJ, 2 Moderna, 2 Pfizer), or first/second booster doses*/

data final_imms2;
	set final_imms1;
	
	*attrib _all_ label='';
	
run;

proc contents data = Final_imms2;
	run;
/*
Remove any administrations of the same vaccine on the same date.
Looks like Meg has already removed them, we'll leave in just in case
*/
proc sort data = final_imms2 nodupkey; 
	by recip_id admin_date vaccine_biv; 
run;

/* 
Sort by person and administration date and count the number of doses for each person
I use this A LOT. 
https://stats.oarc.ucla.edu/sas/faq/how-can-i-create-an-enumeration-variable-by-groups/
*/

proc sort data = final_imms2; 
	by recip_id admin_date; 
run;

/* Make the vaccine variables here, not sure why I put it here */
data final_imms2;
	set final_imms2;
	count + 1;
	by recip_id;
		if first.recip_id then count=1;
run;

/*
*CHECKS;
*What's the highest number of doses someone has? Is vaccine assigned for everyone?;
proc freq data = final_imms2; 
	tables count vaccine_biv / missing; 
run;

*Check one recip_id to see that it's counted correctly;
proc print data = final_imms2;
	where recip_id = 12052972;
	var admin_date cvx vax_event_id recip_id count;
run;

proc contents data = final_imms2 varnum; 
run;
*/

/* Transform to a wide data set.*/
/***** Going from one table to 9 tables */
data dose1 dose2 dose3 dose4 dose5 dose6 dose7 dose8;
	set final_imms2;

	keep RECIP_ID VAX_EVENT_ID ADMIN_DATE vaccine_biv dose count;

	if count=1 then output dose1;
	else if count=2 then output dose2;
	else if count=3 then output dose3;
	else if count=4 then output dose4;
	else if count=5 then output dose5;
	else if count=6 then output dose6;
	else if count=7 then output dose7;
	else if count=8 then output dose8;
	*else if count=9 then output dose9;
	*else if count=10 then output dose10;
run;

/*
*CHECKS;
proc contents data = dose1; 
run;

proc freq data = dose1; 
tables dose ; 
run;
*/

/* Use a macro program to transform each dose table */
/* This is Vivian's code, seems overly complicated */
%macro change_names(library=,dataset=,prefix=,suffix=);
%local rename_list;
proc sql noprint;
  select catx('=',name,cats("&prefix",name,"&suffix"))
    into :rename_list separated by ' ' 
  from dictionary.columns
  where libname = %upcase("&library")
    and memname = %upcase("&dataset")
  ;
quit;

%if (&sqlobs) %then %do;
proc datasets library=&library nolist nodetails;
  modify &dataset;
    rename &rename_list;
  run;
quit;
%end;
%else %put WARNING: Did not find any variables for &library..&dataset..;
%mend change_names;

%change_names(library=WORK , dataset=dose1 , suffix=_dose1);
%change_names(library=WORK , dataset=dose2 , suffix=_dose2);
%change_names(library=WORK , dataset=dose3 , suffix=_dose3);
%change_names(library=WORK , dataset=dose4 , suffix=_dose4);
%change_names(library=WORK , dataset=dose5 , suffix=_dose5);
%change_names(library=WORK , dataset=dose6 , suffix=_dose6);
%change_names(library=WORK , dataset=dose7 , suffix=_dose7);
%change_names(library=WORK , dataset=dose8 , suffix=_dose8);
*%change_names(library=WORK , dataset=dose9 , suffix=_dose9);
*%change_names(library=WORK , dataset=dose10 , suffix=_dose10);

/*Rename recip_ids in each data set*/
/*Rename vaccine_biv_dose# with vaccine_dose#, so downstream code won't need to be changed*/
data dose1; set dose1; rename RECIP_ID_dose1=RECIP_ID vaccine_biv_dose1 = vaccine_dose1; run;
data dose2; set dose2; rename RECIP_ID_dose2=RECIP_ID vaccine_biv_dose2 = vaccine_dose2; run;
data dose3; set dose3; rename RECIP_ID_dose3=RECIP_ID vaccine_biv_dose3 = vaccine_dose3; run;
data dose4; set dose4; rename RECIP_ID_dose4=RECIP_ID vaccine_biv_dose4 = vaccine_dose4; run;
data dose5; set dose5; rename RECIP_ID_dose5=RECIP_ID vaccine_biv_dose5 = vaccine_dose5; run;
data dose6; set dose6; rename RECIP_ID_dose6=RECIP_ID vaccine_biv_dose6 = vaccine_dose6; run;
data dose7; set dose7; rename RECIP_ID_dose7=RECIP_ID vaccine_biv_dose7 = vaccine_dose7; run;
data dose8; set dose8; rename RECIP_ID_dose8=RECIP_ID vaccine_biv_dose8 = vaccine_dose8; run;

/*
proc contents data=dose1 varnum; run;
proc contents data=dose2 varnum; run;
proc contents data=dose3 varnum; run;
proc contents data=dose4 varnum; run;
proc contents data=dose5 varnum; run;
proc contents data=dose6 varnum; run;
proc contents data=dose7 varnum; run;
proc contents data=dose8 varnum; run;
*/

/* Sort all the tables to merge by recip_id*/
proc sort data=dose1; by recip_id; run;
proc sort data=dose2; by recip_id; run;
proc sort data=dose3; by recip_id; run;
proc sort data=dose4; by recip_id; run;
proc sort data=dose5; by recip_id; run;
proc sort data=dose6; by recip_id; run;
proc sort data=dose7; by recip_id; run;
proc sort data=dose8; by recip_id; run;
proc sort data=last_age; by recip_id; run;

/* 
Merge tables and remove duplicate doses. 
*/

data dose123456;
	merge dose1 dose2 dose3 dose4 dose5 dose6 dose7 dose8 last_age;
	by recip_id;
	attrib _all_ label='';
run;

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*
Create variables for initiation (first dose), primary series completion, first booster completion, keep dates for all 
*/
data completed_extra;
	set dose123456;
	*Calculate time between all doses;
	time_dose12=ADMIN_DATE_dose2-ADMIN_DATE_dose1; 
	time_dose13=ADMIN_DATE_dose3-ADMIN_DATE_dose1; 
	time_dose14=ADMIN_DATE_dose4-ADMIN_DATE_dose1; 
	time_dose15=ADMIN_DATE_dose5-ADMIN_DATE_dose1; 
	time_dose16=ADMIN_DATE_dose6-ADMIN_DATE_dose1; 
	time_dose17=ADMIN_DATE_dose7-ADMIN_DATE_dose1; 
	time_dose18=ADMIN_DATE_dose8-ADMIN_DATE_dose1; 
	time_dose23=ADMIN_DATE_dose3-ADMIN_DATE_dose2;
	time_dose24=ADMIN_DATE_dose4-ADMIN_DATE_dose2;
	time_dose25=ADMIN_DATE_dose5-ADMIN_DATE_dose2;
	time_dose26=ADMIN_DATE_dose6-ADMIN_DATE_dose2;
	time_dose27=ADMIN_DATE_dose7-ADMIN_DATE_dose2;
	time_dose28=ADMIN_DATE_dose8-ADMIN_DATE_dose2;
	time_dose34=ADMIN_DATE_dose4-ADMIN_DATE_dose3;
	time_dose35=ADMIN_DATE_dose5-ADMIN_DATE_dose3;
	time_dose36=ADMIN_DATE_dose6-ADMIN_DATE_dose3;
	time_dose37=ADMIN_DATE_dose7-ADMIN_DATE_dose3;
	time_dose38=ADMIN_DATE_dose8-ADMIN_DATE_dose3;
	time_dose45=ADMIN_DATE_dose5-ADMIN_DATE_dose4;
	time_dose46=ADMIN_DATE_dose6-ADMIN_DATE_dose4;
	time_dose47=ADMIN_DATE_dose7-ADMIN_DATE_dose4;
	time_dose48=ADMIN_DATE_dose8-ADMIN_DATE_dose4;
	time_dose56=ADMIN_DATE_dose6-ADMIN_DATE_dose5;
	time_dose57=ADMIN_DATE_dose7-ADMIN_DATE_dose5;
	time_dose58=ADMIN_DATE_dose8-ADMIN_DATE_dose5;
	time_dose67=ADMIN_DATE_dose7-ADMIN_DATE_dose6;
	time_dose68=ADMIN_DATE_dose8-ADMIN_DATE_dose6;
	time_dose78=ADMIN_DATE_dose8-ADMIN_DATE_dose7;

	*Primary Series Completion and Third/Boosters based on doses not ALERT forecaster;
	****(time not incorporated);
	**Set all to zero;
	initiated = 1; *same as people count;
	prim_series = 0;
	booster_first = .;
	booster_second = .;
	initiation_time = &today - admin_date_dose1; *time between today and first dose;
	format initiated_date completion_date mmddyy10.;
	******************************;
	*Script completion date in descending dose order for primary series, so that earlier completion dates are coded last;
	******************************;
	*DOSE6 AS LAST PRIMARY DOSE;
	***J&J as first dose;
	if vaccine_dose6 = 'Johnson & Johnson' then do; 
	prim_series = 1;
	initiated_date = admin_date_dose6;
	completion_vax = vaccine_dose6;
	completion_id = vax_event_id_dose6;
	completion_date = admin_date_dose6;
	completion_dose = 'vaccine_dose6';
	completion_time = &today - admin_date_dose6;
	end;
	***Moderna or Pfizer as first dose;
	***Dose5 and dose6;
	if vaccine_dose5 in ('Moderna', 'Pfizer', 'Novavax') then do;
		if vaccine_dose5 = vaccine_dose6 then do;
		prim_series = 1;
		initiated_date = admin_date_dose5;
		completion_vax = vaccine_dose6;
		completion_id = vax_event_id_dose6;
		completion_date = admin_date_dose6;
		completion_dose = 'vaccine_dose6';
		completion_time = &today - admin_date_dose6;
		end;
	end;
	***Dose4 and dose6;
	if vaccine_dose4 in ('Moderna', 'Pfizer', 'Novavax') then do;
		if vaccine_dose4 = vaccine_dose6 then do;
		prim_series = 1;
		initiated_date = admin_date_dose4;
		completion_vax = vaccine_dose6;
		completion_id = vax_event_id_dose6;
		completion_date = admin_date_dose6;
		completion_dose = 'vaccine_dose6';
		completion_time = &today - admin_date_dose6;
		end;
	end;
	***Dose3 and dose6;
	if vaccine_dose3 in ('Moderna', 'Pfizer', 'Novavax') then do;
		if vaccine_dose3 = vaccine_dose6 then do;
		prim_series = 1;
		initiated_date = admin_date_dose3;
		completion_vax = vaccine_dose6;
		completion_id = vax_event_id_dose6;
		completion_date = admin_date_dose6;
		completion_dose = 'vaccine_dose6';
		completion_time = &today - admin_date_dose6;
		end;
	end;
	***Dose2 and dose6;
	if vaccine_dose2 in ('Moderna', 'Pfizer', 'Novavax') then do;
		if vaccine_dose2 = vaccine_dose6 then do;
		prim_series = 1;
		initiated_date = admin_date_dose2;
		completion_vax = vaccine_dose6;
		completion_id = vax_event_id_dose6;
		completion_date = admin_date_dose6;
		completion_dose = 'vaccine_dose6';
		completion_time = &today - admin_date_dose6;
		end;
	end;
	***Dose1 and dose6;
	if vaccine_dose1 in ('Moderna', 'Pfizer', 'Novavax') then do;
		if vaccine_dose1 = vaccine_dose6 then do;
		prim_series = 1;
		initiated_date = admin_date_dose1;
		completion_vax = vaccine_dose6;
		completion_id = vax_event_id_dose6;
		completion_date = admin_date_dose6;
		completion_dose = 'vaccine_dose6';
		completion_time = &today - admin_date_dose6;
		end;
	end;
	******************************;
	*DOSE5 AS LAST PRIMARY DOSE;
	***J&J as first dose;
	if vaccine_dose5 = 'Johnson & Johnson' then do; 
	prim_series = 1;
	initiated_date = admin_date_dose5;
	completion_vax = vaccine_dose5;
	completion_id = vax_event_id_dose5;
	completion_date = admin_date_dose5;
	completion_dose = 'vaccine_dose5';
	completion_time = &today - admin_date_dose5;
	end;
	***Moderna or Pfizer as first dose;
	***Dose4 and dose5;
	if vaccine_dose4 in ('Moderna', 'Pfizer', 'Novavax') then do;
		if vaccine_dose4 = vaccine_dose5 then do;
		prim_series = 1;
		initiated_date = admin_date_dose4;
		completion_vax = vaccine_dose5;
		completion_id = vax_event_id_dose5;
		completion_date = admin_date_dose5;
		completion_dose = 'vaccine_dose5';
		completion_time = &today - admin_date_dose5;
		end;
	end;
	***Dose3 and dose5;
	if vaccine_dose3 in ('Moderna', 'Pfizer', 'Novavax') then do;
		if vaccine_dose3 = vaccine_dose5 then do;
		prim_series = 1;
		initiated_date = admin_date_dose3;
		completion_vax = vaccine_dose5;
		completion_id = vax_event_id_dose5;
		completion_date = admin_date_dose5;
		completion_dose = 'vaccine_dose5';
		completion_time = &today - admin_date_dose5;
		end;
	end;
	***Dose2 and dose5;
	if vaccine_dose2 in ('Moderna', 'Pfizer', 'Novavax') then do;
		if vaccine_dose2 = vaccine_dose5 then do;
		prim_series = 1;
		initiated_date = admin_date_dose2;
		completion_vax = vaccine_dose5;
		completion_id = vax_event_id_dose5;
		completion_date = admin_date_dose5;
		completion_dose = 'vaccine_dose5';
		completion_time = &today - admin_date_dose5;
		end;
	end;
	***Dose1 and dose5;
	if vaccine_dose1 in ('Moderna', 'Pfizer', 'Novavax') then do;
		if vaccine_dose1 = vaccine_dose5 then do;
		prim_series = 1;
		initiated_date = admin_date_dose1;
		completion_vax = vaccine_dose5;
		completion_id = vax_event_id_dose5;
		completion_date = admin_date_dose5;
		completion_dose = 'vaccine_dose5';
		completion_time = &today - admin_date_dose5;
		end;
	end;
	******************************;
	*DOSE4 AS LAST PRIMARY DOSE;
	***J&J as first dose;
	if vaccine_dose4 = 'Johnson & Johnson' then do; 
	prim_series = 1;
	initiated_date = admin_date_dose4;
	completion_vax = vaccine_dose4;
	completion_id = vax_event_id_dose4;
	completion_date = admin_date_dose4;
	completion_dose = 'vaccine_dose4';
	completion_time = &today - admin_date_dose4;
	end;
	***Moderna or Pfizer as first dose;
	***Dose3 and Dose4;
	if vaccine_dose3 in ('Moderna', 'Pfizer', 'Novavax') then do;
		if vaccine_dose3 = vaccine_dose4 then do;
		prim_series = 1;
		initiated_date = admin_date_dose3;
		completion_vax = vaccine_dose4;
		completion_id = vax_event_id_dose4;
		completion_date = admin_date_dose4;
		completion_dose = 'vaccine_dose4';
		completion_time = &today - admin_date_dose4;
		end;
	end;
	***Dose2 and Dose4;
	if vaccine_dose2 in ('Moderna', 'Pfizer', 'Novavax') then do;
		if vaccine_dose2 = vaccine_dose4 then do;
		prim_series = 1;
		initiated_date = admin_date_dose2;
		completion_vax = vaccine_dose4;
		completion_id = vax_event_id_dose4;
		completion_date = admin_date_dose4;
		completion_dose = 'vaccine_dose4';
		completion_time = &today - admin_date_dose4;
		end;
	end;
	***Dose1 and Dose4;
	if vaccine_dose1 in ('Moderna', 'Pfizer', 'Novavax') then do;
		if vaccine_dose1 = vaccine_dose4 then do;
		prim_series = 1;
		initiated_date = admin_date_dose1;
		completion_vax = vaccine_dose4;
		completion_id = vax_event_id_dose4;
		completion_date = admin_date_dose4;
		completion_dose = 'vaccine_dose4';
		completion_time = &today - admin_date_dose4;
		end;
	end;
	******************************;
	*DOSE3 AS LAST PRIMARY DOSE;
	***J&J as first dose;
	if vaccine_dose3 = 'Johnson & Johnson' then do; 
	prim_series = 1;
	initiated_date = admin_date_dose3;
	completion_vax = vaccine_dose3;
	completion_id = vax_event_id_dose3;
	completion_date = admin_date_dose3;
	completion_dose = 'vaccine_dose3';
	completion_time = &today - admin_date_dose3;
	end;
	***Moderna or Pfizer as first dose;
	***Dose2 and Dose3;
	if vaccine_dose2 in ('Moderna', 'Pfizer', 'Novavax') then do;
		if vaccine_dose2 = vaccine_dose3 then do;
		prim_series = 1;
		initiated_date = admin_date_dose2;
		completion_vax = vaccine_dose3;
		completion_id = vax_event_id_dose3;
		completion_date = admin_date_dose3;
		completion_dose = 'vaccine_dose3';
		completion_time = &today - admin_date_dose3;
		end;
	end;
	***Dose1 and Dose3;
	if vaccine_dose1 in ('Moderna', 'Pfizer', 'Novavax') then do;
		if vaccine_dose1 = vaccine_dose3 then do;
		prim_series = 1;
		initiated_date = admin_date_dose1;
		completion_vax = vaccine_dose3;
		completion_id = vax_event_id_dose3;
		completion_date = admin_date_dose3;
		completion_dose = 'vaccine_dose3';
		completion_time = &today - admin_date_dose3;
		end;
	end;
	******************************;
	*DOSE2 AS LAST PRIMARY DOSE;
	***J&J as first dose;
	if vaccine_dose2 = 'Johnson & Johnson' then do; 
	prim_series = 1;
	initiated_date = admin_date_dose2;
	completion_vax = vaccine_dose2;
	completion_id = vax_event_id_dose2;
	completion_date = admin_date_dose2;
	completion_dose = 'vaccine_dose2';
	completion_time = &today - admin_date_dose2;
	end;
	***Moderna or Pfizer as first dose;
	***Dose1 and Dose2;
	if vaccine_dose1 in ('Moderna', 'Pfizer', 'Novavax') then do;
		if vaccine_dose1 = vaccine_dose2 then do;
		prim_series = 1;
		initiated_date = admin_date_dose1;
		completion_vax = vaccine_dose2;
		completion_id = vax_event_id_dose2;
		completion_date = admin_date_dose2;
		completion_dose = 'vaccine_dose2';
		completion_time = &today - admin_date_dose2;
		end;
	end;
	******************************;
	*DOSE1 AS LAST PRIMARY DOSE;
	***J&J as first dose;
	if vaccine_dose1 = 'Johnson & Johnson' then do; *dose 1 at bottom is because it's the first thing that happened, and at the bottom it overwrites the earlier scripts;
	prim_series = 1;
	initiated_date = admin_date_dose1;
	completion_vax = vaccine_dose1;
	completion_id = vax_event_id_dose1;
	completion_date = admin_date_dose1;
	completion_dose = 'vaccine_dose1';
	completion_time = &today - admin_date_dose1;
	end;
run;

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*
Johnson & Johnson
Moderna
Moderna Biv
Novavax
Pfizer
Pfizer Biv 12
Pfizer Biv 5
Unspecified
*/
data completed_extra0;
	set completed_extra;
	format boost_date boost2_date mmddyy10.;
	******************************;
	*DOSE1 AS COMPLETION DOSE;
	***J&J;
	if completion_dose = 'vaccine_dose1' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose2 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose12 > 60 then booster_first = 1;
		if time_dose12 <= 60 then booster_first = 0;
		if time_dose12 = . then booster_first = .;
		boost_time = time_dose12;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose2;
			boost_id = vax_event_id_dose2;
			boost_date = admin_date_dose2;
			boost_check = 'vaccine_dose2';
		end;
	end;
	if completion_dose = 'vaccine_dose1' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose3 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do; *vaccine_dose2 may not have qualified as booster;
		if time_dose13 > 60 then booster_first = 1;
		if time_dose13 <= 60 then booster_first = 0;
		if time_dose13 = . then booster_first = .;
		boost_time = time_dose13;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose3;
			boost_id = vax_event_id_dose3;
			boost_date = admin_date_dose3;
			boost_check = 'vaccine_dose3';
		end;
	end;
	if completion_dose = 'vaccine_dose1' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose4 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose14 > 60 then booster_first = 1;
		if time_dose14 <= 60 then booster_first = 0;
		if time_dose14 = . then booster_first = .;
		boost_time = time_dose14;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose4;
			boost_id = vax_event_id_dose4;
			boost_date = admin_date_dose4;
			boost_check = 'vaccine_dose4';
		end;
	end;
	if completion_dose = 'vaccine_dose1' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose5 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose15 > 60 then booster_first = 1;
		if time_dose15 <= 60 then booster_first = 0;
		if time_dose15 = . then booster_first = .;
		boost_time = time_dose15;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose5;
			boost_id = vax_event_id_dose5;
			boost_date = admin_date_dose5;
			boost_check = 'vaccine_dose5';
		end;
	end;
	if completion_dose = 'vaccine_dose1' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose6 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose16 > 60 then booster_first = 1;
		if time_dose16 <= 60 then booster_first = 0;
		if time_dose16 = . then booster_first = .;
		boost_time = time_dose16;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose6;
			boost_id = vax_event_id_dose6;
			boost_date = admin_date_dose6;
			boost_check = 'vaccine_dose6';
		end;
	end;
	******************************;
	*DOSE2 AS COMPLETION DOSE;
	***J&J;
	if completion_dose = 'vaccine_dose2' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose3 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose23 > 60 then booster_first = 1;
		if time_dose23 <= 60 then booster_first = 0;
		if time_dose23 = . then booster_first = .;
		boost_time = time_dose23;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose3;
			boost_id = vax_event_id_dose3;
			boost_date = admin_date_dose3;
			boost_check = 'vaccine_dose3';
		end;
	end;
	if completion_dose = 'vaccine_dose2' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose4 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose24 > 60 then booster_first = 1;
		if time_dose24 <= 60 then booster_first = 0;
		if time_dose24 = . then booster_first = .;
		boost_time = time_dose24;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose4;
			boost_id = vax_event_id_dose4;
			boost_date = admin_date_dose4;
			boost_check = 'vaccine_dose4';
		end;
	end;
	if completion_dose = 'vaccine_dose2' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose5 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose25 > 60 then booster_first = 1;
		if time_dose25 <= 60 then booster_first = 0;
		if time_dose25 = . then booster_first = .;
		boost_time = time_dose25;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose5;
			boost_id = vax_event_id_dose5;
			boost_date = admin_date_dose5;
			boost_check = 'vaccine_dose5';
		end;
	end;
	if completion_dose = 'vaccine_dose2' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose6 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose26 > 60 then booster_first = 1;
		if time_dose26 <= 60 then booster_first = 0;
		if time_dose26 = . then booster_first = .;
		boost_time = time_dose26;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose6;
			boost_id = vax_event_id_dose6;
			boost_date = admin_date_dose6;
			boost_check = 'vaccine_dose6';
		end;
	end;
	if completion_dose = 'vaccine_dose2' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose7 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose27 > 60 then booster_first = 1;
		if time_dose27 <= 60 then booster_first = 0;
		if time_dose27 = . then booster_first = .;
		boost_time = time_dose27;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose7;
			boost_id = vax_event_id_dose7;
			boost_date = admin_date_dose7;
			boost_check = 'vaccine_dose7';
		end;
	end;
	******************************;
	*DOSE3 AS COMPLETION DOSE;
	***J&J;
	if completion_dose = 'vaccine_dose3' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose4 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose34 > 60 then booster_first = 1;
		if time_dose34 <= 60 then booster_first = 0;
		if time_dose34 = . then booster_first = .;
		boost_time = time_dose34;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose4;
			boost_id = vax_event_id_dose4;
			boost_date = admin_date_dose4;
			boost_check = 'vaccine_dose4';
		end;
	end;
	if completion_dose = 'vaccine_dose3' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose5 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose35 > 60 then booster_first = 1;
		if time_dose35 <= 60 then booster_first = 0;
		if time_dose35 = . then booster_first = .;
		boost_time = time_dose35;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose5;
			boost_id = vax_event_id_dose5;
			boost_date = admin_date_dose5;
			boost_check = 'vaccine_dose5';
		end;
	end;
	if completion_dose = 'vaccine_dose3' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose6 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose36 > 60 then booster_first = 1;
		if time_dose36 <= 60 then booster_first = 0;
		if time_dose36 = . then booster_first = .;
		boost_time = time_dose36;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose6;
			boost_id = vax_event_id_dose6;
			boost_date = admin_date_dose6;
			boost_check = 'vaccine_dose6';
		end;
	end;
	if completion_dose = 'vaccine_dose3' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose7 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose37 > 60 then booster_first = 1;
		if time_dose37 <= 60 then booster_first = 0;
		if time_dose37 = . then booster_first = .;
		boost_time = time_dose37;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose7;
			boost_id = vax_event_id_dose7;
			boost_date = admin_date_dose7;
			boost_check = 'vaccine_dose7';
		end;
	end;
	if completion_dose = 'vaccine_dose3' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose8 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose38 > 60 then booster_first = 1;
		if time_dose38 <= 60 then booster_first = 0;
		if time_dose38 = . then booster_first = .;
		boost_time = time_dose38;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose8;
			boost_id = vax_event_id_dose8;
			boost_date = admin_date_dose8;
			boost_check = 'vaccine_dose8';
		end;
	end;
	******************************;
	*DOSE4 AS COMPLETION DOSE;
	***J&J;
	if completion_dose = 'vaccine_dose4' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose5 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose45 > 60 then booster_first = 1;
		if time_dose45 <= 60 then booster_first = 0;
		if time_dose45 = . then booster_first = .;
		boost_time = time_dose45;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose5;
			boost_id = vax_event_id_dose5;
			boost_date = admin_date_dose5;
			boost_check = 'vaccine_dose5';
		end;
	end;
	if completion_dose = 'vaccine_dose4' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose6 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose46 > 60 then booster_first = 1;
		if time_dose46 <= 60 then booster_first = 0;
		if time_dose46 = . then booster_first = .;
		boost_time = time_dose46;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose6;
			boost_id = vax_event_id_dose6;
			boost_date = admin_date_dose6;
			boost_check = 'vaccine_dose6';
		end;
	end;
	if completion_dose = 'vaccine_dose4' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose7 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose47 > 60 then booster_first = 1;
		if time_dose47 <= 60 then booster_first = 0;
		if time_dose47 = . then booster_first = .;
		boost_time = time_dose47;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose7;
			boost_id = vax_event_id_dose7;
			boost_date = admin_date_dose7;
			boost_check = 'vaccine_dose7';
		end;
	end;
	if completion_dose = 'vaccine_dose4' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose8 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose48 > 60 then booster_first = 1;
		if time_dose48 <= 60 then booster_first = 0;
		if time_dose48 = . then booster_first = .;
		boost_time = time_dose48;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose8;
			boost_id = vax_event_id_dose8;
			boost_date = admin_date_dose8;
			boost_check = 'vaccine_dose8';
		end;
	end;
	******************************;
	*DOSE4 AS COMPLETION DOSE;
	***J&J; *Keep here in case of dose5 coding;
	if completion_dose = 'vaccine_dose4' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose5 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do; 
		if time_dose45 > 60 then booster_first = 1;
		if time_dose45 <= 60 then booster_first = 0;
		if time_dose45 = . then booster_first = .;
		boost_time = time_dose45;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose5;
			boost_id = vax_event_id_dose5;
			boost_date = admin_date_dose5;
			boost_check = 'vaccine_dose5';
		end;
	end;
	if completion_dose = 'vaccine_dose4' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose6 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose46 > 60 then booster_first = 1;
		if time_dose46 <= 60 then booster_first = 0;
		if time_dose46 = . then booster_first = .;
		boost_time = time_dose46;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose6;
			boost_id = vax_event_id_dose6;
			boost_date = admin_date_dose6;
			boost_check = 'vaccine_dose6';
		end;
	end;
	if completion_dose = 'vaccine_dose4' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose7 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose47 > 60 then booster_first = 1;
		if time_dose47 <= 60 then booster_first = 0;
		if time_dose47 = . then booster_first = .;
		boost_time = time_dose47;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose7;
			boost_id = vax_event_id_dose7;
			boost_date = admin_date_dose7;
			boost_check = 'vaccine_dose7';
		end;
	end;
	if completion_dose = 'vaccine_dose4' AND completion_vax = 'Johnson & Johnson' AND vaccine_dose8 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose48 > 60 then booster_first = 1;
		if time_dose48 <= 60 then booster_first = 0;
		if time_dose48 = . then booster_first = .;
		boost_time = time_dose48;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose8;
			boost_id = vax_event_id_dose8;
			boost_date = admin_date_dose8;
			boost_check = 'vaccine_dose8';
		end;
	end;
	******************************;
	******************************;
	******************************;
	******************************;
	******************************;
	******************************;
	******************************;
	******************************;
	*DOSE2 AS COMPLETION DOSE;
	***mRNA;
	if completion_dose = 'vaccine_dose2' AND completion_vax in ('Moderna', 'Pfizer', 'Novavax') AND vaccine_dose3 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose23 > 152 then booster_first = 1;
		if time_dose23 <= 152 then booster_first = 0;
		if time_dose23 = . then booster_first = .;
		boost_time = time_dose23;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose3;
			boost_id = vax_event_id_dose3;
			boost_date = admin_date_dose3;
			boost_check = 'vaccine_dose3';
		end;
	end;
	if completion_dose = 'vaccine_dose2' AND completion_vax in ('Moderna', 'Pfizer', 'Novavax') AND vaccine_dose4 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose24 > 152 then booster_first = 1;
		if time_dose24 <= 152 then booster_first = 0;
		if time_dose24 = . then booster_first = .;
		boost_time = time_dose24;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose4;
			boost_id = vax_event_id_dose4;
			boost_date = admin_date_dose4;
			boost_check = 'vaccine_dose4';
		end;
	end;
	if completion_dose = 'vaccine_dose2' AND completion_vax in ('Moderna', 'Pfizer', 'Novavax') AND vaccine_dose5 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose25 > 152 then booster_first = 1;
		if time_dose25 <= 152 then booster_first = 0;
		if time_dose25 = . then booster_first = .;
		boost_time = time_dose25;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose5;
			boost_id = vax_event_id_dose5;
			boost_date = admin_date_dose5;
			boost_check = 'vaccine_dose5';
		end;
	end;
	if completion_dose = 'vaccine_dose2' AND completion_vax in ('Moderna', 'Pfizer', 'Novavax') AND vaccine_dose6 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose26 > 152 then booster_first = 1;
		if time_dose26 <= 152 then booster_first = 0;
		if time_dose26 = . then booster_first = .;
		boost_time = time_dose26;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose6;
			boost_id = vax_event_id_dose6;
			boost_date = admin_date_dose6;
			boost_check = 'vaccine_dose6';
		end;
	end;
	if completion_dose = 'vaccine_dose2' AND completion_vax in ('Moderna', 'Pfizer', 'Novavax') AND vaccine_dose7 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose27 > 152 then booster_first = 1;
		if time_dose27 <= 152 then booster_first = 0;
		if time_dose27 = . then booster_first = .;
		boost_time = time_dose27;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose7;
			boost_id = vax_event_id_dose7;
			boost_date = admin_date_dose7;
			boost_check = 'vaccine_dose7';
		end;
	end;
	******************************;
	*DOSE3 AS COMPLETION DOSE;
	***mRNA;
	if completion_dose = 'vaccine_dose3' AND completion_vax in ('Moderna', 'Pfizer', 'Novavax') AND vaccine_dose4 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose34 > 152 then booster_first = 1;
		if time_dose34 <= 152 then booster_first = 0;
		if time_dose34 = . then booster_first = .;
		boost_time = time_dose34;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose4;
			boost_id = vax_event_id_dose4;
			boost_date = admin_date_dose4;
			boost_check = 'vaccine_dose4';
		end;
	end;
	if completion_dose = 'vaccine_dose3' AND completion_vax in ('Moderna', 'Pfizer', 'Novavax') AND vaccine_dose5 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose35 > 152 then booster_first = 1;
		if time_dose35 <= 152 then booster_first = 0;
		if time_dose35 = . then booster_first = .;
		boost_time = time_dose35;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose5;
			boost_id = vax_event_id_dose5;
			boost_date = admin_date_dose5;
			boost_check = 'vaccine_dose5';
		end;
	end;
	if completion_dose = 'vaccine_dose3' AND completion_vax in ('Moderna', 'Pfizer', 'Novavax') AND vaccine_dose6 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose36 > 152 then booster_first = 1;
		if time_dose36 <= 152 then booster_first = 0;
		if time_dose36 = . then booster_first = .;
		boost_time = time_dose36;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose6;
			boost_id = vax_event_id_dose6;
			boost_date = admin_date_dose6;
			boost_check = 'vaccine_dose6';
		end;
	end;
	if completion_dose = 'vaccine_dose3' AND completion_vax in ('Moderna', 'Pfizer', 'Novavax') AND vaccine_dose7 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose37 > 152 then booster_first = 1;
		if time_dose37 <= 152 then booster_first = 0;
		if time_dose37 = . then booster_first = .;
		boost_time = time_dose37;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose7;
			boost_id = vax_event_id_dose7;
			boost_date = admin_date_dose7;
			boost_check = 'vaccine_dose7';
		end;
	end;
	if completion_dose = 'vaccine_dose3' AND completion_vax in ('Moderna', 'Pfizer', 'Novavax') AND vaccine_dose8 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose38 > 152 then booster_first = 1;
		if time_dose38 <= 152 then booster_first = 0;
		if time_dose38 = . then booster_first = .;
		boost_time = time_dose38;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose8;
			boost_id = vax_event_id_dose8;
			boost_date = admin_date_dose8;
			boost_check = 'vaccine_dose8';
		end;
	end;
	******************************;
	*DOSE4 AS COMPLETION DOSE;
	***mRNA;
	if completion_dose = 'vaccine_dose4' AND completion_vax in ('Moderna', 'Pfizer', 'Novavax') AND vaccine_dose5 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose45 > 152 then booster_first = 1;
		if time_dose45 <= 152 then booster_first = 0;
		if time_dose45 = . then booster_first = .;
		boost_time = time_dose45;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose5;
			boost_id = vax_event_id_dose5;
			boost_date = admin_date_dose5;
			boost_check = 'vaccine_dose5';
		end;
	end;
	if completion_dose = 'vaccine_dose4' AND completion_vax in ('Moderna', 'Pfizer', 'Novavax') AND vaccine_dose6 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose46 > 152 then booster_first = 1;
		if time_dose46 <= 152 then booster_first = 0;
		if time_dose46 = . then booster_first = .;
		boost_time = time_dose46;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose6;
			boost_id = vax_event_id_dose6;
			boost_date = admin_date_dose6;
			boost_check = 'vaccine_dose6';
		end;
	end;
	if completion_dose = 'vaccine_dose4' AND completion_vax in ('Moderna', 'Pfizer', 'Novavax') AND vaccine_dose7 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose47 > 152 then booster_first = 1;
		if time_dose47 <= 152 then booster_first = 0;
		if time_dose47 = . then booster_first = .;
		boost_time = time_dose47;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose7;
			boost_id = vax_event_id_dose7;
			boost_date = admin_date_dose7;
			boost_check = 'vaccine_dose7';
		end;
	end;
	if completion_dose = 'vaccine_dose4' AND completion_vax in ('Moderna', 'Pfizer', 'Novavax') AND vaccine_dose8 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose48 > 152 then booster_first = 1;
		if time_dose48 <= 152 then booster_first = 0;
		if time_dose48 = . then booster_first = .;
		boost_time = time_dose48;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose8;
			boost_id = vax_event_id_dose8;
			boost_date = admin_date_dose8;
			boost_check = 'vaccine_dose8';
		end;
	end;
	******************************;
	*DOSE4 AS COMPLETION DOSE;
	***mRNA; *Keep here in case of dose5 coding;
	if completion_dose = 'vaccine_dose4' AND completion_vax in ('Moderna', 'Pfizer', 'Novavax') AND vaccine_dose5 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do; 
		if time_dose45 > 152 then booster_first = 1;
		if time_dose45 <= 152 then booster_first = 0;
		if time_dose45 = . then booster_first = .;
		boost_time = time_dose45;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose5;
			boost_id = vax_event_id_dose5;
			boost_date = admin_date_dose5;
			boost_check = 'vaccine_dose5';
		end;
	end;
	if completion_dose = 'vaccine_dose4' AND completion_vax in ('Moderna', 'Pfizer', 'Novavax') AND vaccine_dose6 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose46 > 152 then booster_first = 1;
		if time_dose46 <= 152 then booster_first = 0;
		if time_dose46 = . then booster_first = .;
		boost_time = time_dose46;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose6;
			boost_id = vax_event_id_dose6;
			boost_date = admin_date_dose6;
			boost_check = 'vaccine_dose6';
		end;
	end;
	if completion_dose = 'vaccine_dose4' AND completion_vax in ('Moderna', 'Pfizer', 'Novavax') AND vaccine_dose7 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose47 > 152 then booster_first = 1;
		if time_dose47 <= 152 then booster_first = 0;
		if time_dose47 = . then booster_first = .;
		boost_time = time_dose47;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose7;
			boost_id = vax_event_id_dose7;
			boost_date = admin_date_dose7;
			boost_check = 'vaccine_dose7';
		end;
	end;
	if completion_dose = 'vaccine_dose4' AND completion_vax in ('Moderna', 'Pfizer', 'Novavax') AND vaccine_dose8 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') AND booster_first = 0 then do;
		if time_dose48 > 152 then booster_first = 1;
		if time_dose48 <= 152 then booster_first = 0;
		if time_dose48 = . then booster_first = .;
		boost_time = time_dose48;
		if booster_first = 1 then do;
			boost_vax = vaccine_dose8;
			boost_id = vax_event_id_dose8;
			boost_date = admin_date_dose8;
			boost_check = 'vaccine_dose8';
		end;
	end;
run;

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/

/***********************************************************************/
/* Creat variable for second booster, keep dates for all */
data completed_extra1;
	set completed_extra0;
	/* DOSE2 AS FIRST BOOSTER */
	if boost_check = 'vaccine_dose2' and booster_first = 1 and vaccine_dose3 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose23 > 120 then booster_second = 1;
		if time_dose23 <= 120 then booster_second = 0;
		if time_dose23 = . then booster_second = .;
		boost2_time = time_dose23;
		if booster_second = 1 then do;
			boost2_vax = vaccine_dose3;
			boost2_id = vax_event_id_dose3;
			boost2_date = admin_date_dose3;
			boost2_check = 'vaccine_dose3';
		end;
	end;
	if boost_check = 'vaccine_dose2' and booster_first = 1 and booster_second = 0 and vaccine_dose4 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose24 > 120 then booster_second = 1;
		if time_dose24 <= 120 then booster_second = 0;
		if time_dose24 = . then booster_second = .;
		boost2_time = time_dose24;
		if booster_second = 1 then do;
			boost2_vax = vaccine_dose4;
			boost2_id = vax_event_id_dose4;
			boost2_date = admin_date_dose4;
			boost2_check = 'vaccine_dose4';
		end;
	end;
	if boost_check = 'vaccine_dose2' and booster_first = 1 and booster_second = 0 and vaccine_dose5 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose25 > 120 then booster_second = 1;
		if time_dose25 <= 120 then booster_second = 0;
		if time_dose25 = . then booster_second = .;
		boost2_time = time_dose25;
		if booster_second = 1 then do;
			boost2_vax = vaccine_dose5;
			boost2_id = vax_event_id_dose5;
			boost2_date = admin_date_dose5;
			boost2_check = 'vaccine_dose5';
		end;
	end;
	/* DOSE3 AS FIRST BOOSTER */
	if boost_check = 'vaccine_dose3' and booster_first = 1 and vaccine_dose4 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose34 > 120 then booster_second = 1;
		if time_dose34 <= 120 then booster_second = 0;
		if time_dose34 = . then booster_second = .;
		boost2_time = time_dose34;
		if booster_second = 1 then do;
			boost2_vax = vaccine_dose4;
			boost2_id = vax_event_id_dose4;
			boost2_date = admin_date_dose4;
			boost2_check = 'vaccine_dose4';
		end;
	end;	
	if boost_check = 'vaccine_dose3' and booster_first = 1 and vaccine_dose5 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose35 > 120 then booster_second = 1;
		if time_dose35 <= 120 then booster_second = 0;
		if time_dose35 = . then booster_second = .;
		boost2_time = time_dose35;
		if booster_second = 1 then do;
			boost2_vax = vaccine_dose5;
			boost2_id = vax_event_id_dose5;
			boost2_date = admin_date_dose5;
			boost2_check = 'vaccine_dose5';
		end;
	end;	
	if boost_check = 'vaccine_dose3' and booster_first = 1 and vaccine_dose6 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose36 > 120 then booster_second = 1;
		if time_dose36 <= 120 then booster_second = 0;
		if time_dose36 = . then booster_second = .;
		boost2_time = time_dose36;
		if booster_second = 1 then do;
			boost2_vax = vaccine_dose6;
			boost2_id = vax_event_id_dose6;
			boost2_date = admin_date_dose6;
			boost2_check = 'vaccine_dose6';
		end;
	end;	
	if boost_check = 'vaccine_dose3' and booster_first = 1 and vaccine_dose7 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose37 > 120 then booster_second = 1;
		if time_dose37 <= 120 then booster_second = 0;
		if time_dose37 = . then booster_second = .;
		boost2_time = time_dose37;
		if booster_second = 1 then do;
			boost2_vax = vaccine_dose7;
			boost2_id = vax_event_id_dose7;
			boost2_date = admin_date_dose7;
			boost2_check = 'vaccine_dose7';
		end;
	end;	
	/* DOSE4 AS FIRST BOOSTER */
	if boost_check = 'vaccine_dose4' and booster_first = 1 and vaccine_dose5 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose45 > 120 then booster_second = 1;
		if time_dose45 <= 120 then booster_second = 0;
		if time_dose45 = . then booster_second = .;
		boost2_time = time_dose45;
		if booster_second = 1 then do;
			boost2_vax = vaccine_dose5;
			boost2_id = vax_event_id_dose5;
			boost2_date = admin_date_dose5;
			boost2_check = 'vaccine_dose5';
		end;
	end;
	if boost_check = 'vaccine_dose4' and booster_first = 1 and vaccine_dose6 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose46 > 120 then booster_second = 1;
		if time_dose46 <= 120 then booster_second = 0;
		if time_dose46 = . then booster_second = .;
		boost2_time = time_dose46;
		if booster_second = 1 then do;
			boost2_vax = vaccine_dose6;
			boost2_id = vax_event_id_dose6;
			boost2_date = admin_date_dose6;
			boost2_check = 'vaccine_dose6';
		end;
	end;
	if boost_check = 'vaccine_dose4' and booster_first = 1 and vaccine_dose7 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose47 > 120 then booster_second = 1;
		if time_dose47 <= 120 then booster_second = 0;
		if time_dose47 = . then booster_second = .;
		boost2_time = time_dose47;
		if booster_second = 1 then do;
			boost2_vax = vaccine_dose7;
			boost2_id = vax_event_id_dose7;
			boost2_date = admin_date_dose7;
			boost2_check = 'vaccine_dose7';
		end;
	end;
	/* DOSE5 AS FIRST BOOSTER */
	if boost_check = 'vaccine_dose5' and booster_first = 1 and vaccine_dose6 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose56 > 120 then booster_second = 1;
		if time_dose56 <= 120 then booster_second = 0;
		if time_dose56 = . then booster_second = .;
		boost2_time = time_dose56;
		if booster_second = 1 then do;
			boost2_vax = vaccine_dose6;
			boost2_id = vax_event_id_dose6;
			boost2_date = admin_date_dose6;
			boost2_check = 'vaccine_dose6';
		end;
	end;
	if boost_check = 'vaccine_dose5' and booster_first = 1 and vaccine_dose7 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose57 > 120 then booster_second = 1;
		if time_dose57 <= 120 then booster_second = 0;
		if time_dose57 = . then booster_second = .;
		boost2_time = time_dose57;
		if booster_second = 1 then do;
			boost2_vax = vaccine_dose7;
			boost2_id = vax_event_id_dose7;
			boost2_date = admin_date_dose7;
			boost2_check = 'vaccine_dose7';
		end;
	end;
	if boost_check = 'vaccine_dose5' and booster_first = 1 and vaccine_dose8 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose58 > 120 then booster_second = 1;
		if time_dose58 <= 120 then booster_second = 0;
		if time_dose58 = . then booster_second = .;
		boost2_time = time_dose58;
		if booster_second = 1 then do;
			boost2_vax = vaccine_dose8;
			boost2_id = vax_event_id_dose8;
			boost2_date = admin_date_dose8;
			boost2_check = 'vaccine_dose8';
		end;
	end;
	/* DOSE6 AS FIRST BOOSTER */
	if boost_check = 'vaccine_dose6' and booster_first = 1 and vaccine_dose7 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose67 > 120 then booster_second = 1;
		if time_dose67 <= 120 then booster_second = 0;
		if time_dose67 = . then booster_second = .;
		boost2_time = time_dose67;
		if booster_second = 1 then do;
			boost2_vax = vaccine_dose7;
			boost2_id = vax_event_id_dose7;
			boost2_date = admin_date_dose7;
			boost2_check = 'vaccine_dose7';
		end;
	end;
	if boost_check = 'vaccine_dose6' and booster_first = 1 and vaccine_dose8 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose68 > 120 then booster_second = 1;
		if time_dose68 <= 120 then booster_second = 0;
		if time_dose68 = . then booster_second = .;
		boost2_time = time_dose68;
		if booster_second = 1 then do;
			boost2_vax = vaccine_dose8;
			boost2_id = vax_event_id_dose8;
			boost2_date = admin_date_dose8;
			boost2_check = 'vaccine_dose8';
		end;
	end;
	/* DOSE7 AS FIRST BOOSTER */
	if boost_check = 'vaccine_dose7' and booster_first = 1 and vaccine_dose8 in ('Johnson & Johnson', 'Moderna', 'Novavax', 'Pfizer', 'Unspecified') then do;
		if time_dose78 > 120 then booster_second = 1;
		if time_dose78 <= 120 then booster_second = 0;
		if time_dose78 = . then booster_second = .;
		boost2_time = time_dose78;
		if booster_second = 1 then do;
			boost2_vax = vaccine_dose8;
			boost2_id = vax_event_id_dose8;
			boost2_date = admin_date_dose8;
			boost2_check = 'vaccine_dose8';
		end;
	end;
run;

/***********************************************************************/
/***********************************************************************/
/***********************************************************************/
/* Create flags for who is still eligible for monovalent boosters. 
	Basically, we just need these to determine who is eligible for bivalent boosters, 
		and # 5 to 11 year olds are still getting boosted with monovalent
		and # 5 to 11 year olds who still need a monovalent booster. 
*/

data completed_extra2;
	set completed_extra1;
	format elig_boost1_date elig_boost2_date mmddyy10.;
	/*determine date people are eligible for a booster */
	if completion_vax = 'Johnson & Johnson' then do;
		elig_boost1_date = completion_date + 60;
	end;
	if completion_vax in ('Moderna', 'Pfizer') then do;
		elig_boost1_date = completion_date + 152;
	end;
	/*determine date people are eligible for a second booster*/
	if boost_check ^= '' then do;
		elig_boost2_date = boost_date + 120;
	end;
	/*make a flag to show when people BECOME/BECAME eligible for a booster*/
	if age >= 5 then do; *This is Total Population, which counts age 5 and above. We filter for 18+ populatoin later;
		needs_boost = 1;
		if elig_boost1_date <= &today and booster_first = 1 then needs_boost = 0;	*powering T6 of the Metrics Dash ST. Checks how many people still needs a booster in past months;
		if elig_boost1_date <= &today and booster_first = 0 then needs_boost = 1;
		if elig_boost1_date > &today and booster_first = 1 then needs_boost = 1; *Since elig_boost1_date is in the future (>today), then we just count all of them as needing a booster then.;
		if elig_boost1_date > &today and booster_first = 0 then needs_boost = 1;
		if elig_boost1_date = . then needs_boost = .;
	end;
	/*make a flag for people who ARE eligible for a booster and still need one*/
	if age >= 5 then do;
		needs_boost1_now = 1;
		if elig_boost1_date <= &today and booster_first = 1 then needs_boost1_now = 0;
		if elig_boost1_date <= &today and booster_first = 0 then needs_boost1_now = 1;
		if elig_boost1_date > &today then needs_boost1_now = 0; *Here, because these people are not eligible for a booster yet, they are counted as 0/not needing boost now;
		if elig_boost1_date = . then needs_boost1_now = .;
	end;
	/*make a flag for people who are eligible for a second booster and still need one */
	if age >= 50 then do;
		needs_boost2_now = 1;
		if elig_boost2_date <= &today and booster_second = 1 then needs_boost2_now = 0;
		if elig_boost2_date <= &today and booster_second = 0 then needs_boost2_now = 1;
		if elig_boost2_date > &today then needs_boost2_now = 0;
		if elig_boost2_date = . then needs_boost2_now = .;
	end;
run;

proc contents data = completed_extra2;
run;

proc print data = completed_extra2 (obs = 10);
where prim_series = 0;
var recip_id vaccine_dose1 vaccine_dose2 vaccine_dose3 vaccine_dose4 vaccine_dose5;
run;

proc freq data = completed_extra2;
	table completion_dose boost_check boost2_check / norow nocol nopercent missing;
run;

/*Figure out who is eligible for a bivalent booster */
data completed_extra3;
	set completed_extra2; 
	booster_biv = .;
	needs_boostbiv_now = .;
	boostmono_time = .;
	boostbiv_time = .;
	boostbiv_add = .;
	format last_date mmddyy10.;
	/******************************************************/
	/* Primary series only*/
	if completion_dose = 'vaccine_dose1' AND vaccine_dose2 in ('Moderna Biv', 'Pfizer Biv 12', 'Pfizer Biv 5') then do;
		if time_dose12 > 60 then booster_biv = 1;
		if time_dose12 <= 60 then booster_biv = 0;
		if time_dose12 = . then booster_biv = .;
		boostbiv_time = time_dose12;
		if booster_biv = 1 then do;
			boostbiv_vax = vaccine_dose2;
			boostbiv_id = vax_event_id_dose2;
			boostbiv_date = admin_date_dose2;
			boostbiv_check = 'vaccine_dose2';
		end;
	end;
	if completion_dose = 'vaccine_dose2' AND vaccine_dose3 in ('Moderna Biv', 'Pfizer Biv 12', 'Pfizer Biv 5') then do;
		if time_dose23 > 60 then booster_biv = 1;
		if time_dose23 <= 60 then booster_biv = 0;
		if time_dose23 = . then booster_biv = .;
		boostbiv_time = time_dose23;
		if booster_biv = 1 then do;
			boostbiv_vax = vaccine_dose3;
			boostbiv_id = vax_event_id_dose3;
			boostbiv_date = admin_date_dose3;
			boostbiv_check = 'vaccine_dose3';
		end;
	end;
	if completion_dose = 'vaccine_dose3' AND vaccine_dose4 in ('Moderna Biv', 'Pfizer Biv 12', 'Pfizer Biv 5') then do;
		if time_dose34 > 60 then booster_biv = 1;
		if time_dose34 <= 60 then booster_biv = 0;
		if time_dose34 = . then booster_biv = .;
		boostbiv_time = time_dose34;
		if booster_biv = 1 then do;
			boostbiv_vax = vaccine_dose4;
			boostbiv_id = vax_event_id_dose4;
			boostbiv_date = admin_date_dose4;
			boostbiv_check = 'vaccine_dose4';
		end;
	end;
	if completion_dose = 'vaccine_dose4' AND vaccine_dose5 in ('Moderna Biv', 'Pfizer Biv 12', 'Pfizer Biv 5') then do;
		if time_dose45 > 60 then booster_biv = 1;
		if time_dose45 <= 60 then booster_biv = 0;
		if time_dose45 = . then booster_biv = .;
		boostbiv_time = time_dose45;
		if booster_biv = 1 then do;
			boostbiv_vax = vaccine_dose5;
			boostbiv_id = vax_event_id_dose5;
			boostbiv_date = admin_date_dose5;
			boostbiv_check = 'vaccine_dose5';
		end;
	end;
	if completion_dose = 'vaccine_dose5' AND vaccine_dose6 in ('Moderna Biv', 'Pfizer Biv 12', 'Pfizer Biv 5') then do;
		if time_dose56 > 60 then booster_biv = 1;
		if time_dose56 <= 60 then booster_biv = 0;
		if time_dose56 = . then booster_biv = .;
		boostbiv_time = time_dose56;
		if booster_biv = 1 then do;
			boostbiv_vax = vaccine_dose6;
			boostbiv_id = vax_event_id_dose6;
			boostbiv_date = admin_date_dose6;
			boostbiv_check = 'vaccine_dose6';
		end;
	end;
	if completion_dose = 'vaccine_dose6' AND vaccine_dose7 in ('Moderna Biv', 'Pfizer Biv 12', 'Pfizer Biv 5') then do;
		if time_dose67 > 60 then booster_biv = 1;
		if time_dose67 <= 60 then booster_biv = 0;
		if time_dose67 = . then booster_biv = .;
		boostbiv_time = time_dose67;
		if booster_biv = 1 then do;
			boostbiv_vax = vaccine_dose7;
			boostbiv_id = vax_event_id_dose7;
			boostbiv_date = admin_date_dose7;
			boostbiv_check = 'vaccine_dose7';
		end;
	end;
	/**/
	if prim_series = 1 AND booster_first in (., 0) AND booster_second in (., 0) AND booster_biv in (.,0) then do;
		boostmono_time = &today -  completion_date;
		boostbiv_add = prim_series;
	end;
	if prim_series = 1 AND booster_first in (., 0) AND booster_second in (., 0) AND boostmono_time >= 60 AND booster_biv in (.,0) then do;
		needs_boostbiv_now = 1;
	end;
	if prim_series = 1 AND booster_first in (., 0) AND booster_second in (., 0) AND boostmono_time < 60 AND booster_biv in (.,0) then do;
		needs_boostbiv_now = 0;
	end;

	/* Found this filtering step. It seems to be filtering out the population below age 12 from the needs_boostbiv_now. 
	   From a few proc freq data check, it looks like it filters out all people below age 11 that are only Vaccinated, no boosters.
	   That's why there are still needs_boostbiv_now = 1 for population <= 11, because the later steps do not have this filter/exclusion step.
	   This means that the people who got monovalent 1st booster and 2nd booster were not filtered out in this population.

	if age <= 11 then do;
		boostmono_time = .;
		needs_boostbiv_now = .;
	end;
	*/

	/******************************************************/
	/* Primary series + one boosters */
	if boost_check = 'vaccine_dose2' AND vaccine_dose3 in ('Moderna Biv', 'Pfizer Biv 12', 'Pfizer Biv 5') then do;
		if time_dose23 > 60 then booster_biv = 1;
		if time_dose23 <= 60 then booster_biv = 0;
		if time_dose23 = . then booster_biv = .;
		boostbiv_time = time_dose23;
		if booster_biv = 1 then do;
			boostbiv_vax = vaccine_dose3;
			boostbiv_id = vax_event_id_dose3;
			boostbiv_date = admin_date_dose3;
			boostbiv_check = 'vaccine_dose3';
		end;
	end;
	if boost_check = 'vaccine_dose3' AND vaccine_dose4 in ('Moderna Biv', 'Pfizer Biv 12', 'Pfizer Biv 5') then do;
		if time_dose34 > 60 then booster_biv = 1;
		if time_dose34 <= 60 then booster_biv = 0;
		if time_dose34 = . then booster_biv = .;
		boostbiv_time = time_dose34;
		if booster_biv = 1 then do;
			boostbiv_vax = vaccine_dose4;
			boostbiv_id = vax_event_id_dose4;
			boostbiv_date = admin_date_dose4;
			boostbiv_check = 'vaccine_dose4';
		end;
	end;
	if boost_check = 'vaccine_dose4' AND vaccine_dose5 in ('Moderna Biv', 'Pfizer Biv 12', 'Pfizer Biv 5') then do;
		if time_dose45 > 60 then booster_biv = 1;
		if time_dose45 <= 60 then booster_biv = 0;
		if time_dose45 = . then booster_biv = .;
		boostbiv_time = time_dose45;
		if booster_biv = 1 then do;
			boostbiv_vax = vaccine_dose5;
			boostbiv_id = vax_event_id_dose5;
			boostbiv_date = admin_date_dose5;
			boostbiv_check = 'vaccine_dose5';
		end;
	end;
	if boost_check = 'vaccine_dose5' AND vaccine_dose6 in ('Moderna Biv', 'Pfizer Biv 12', 'Pfizer Biv 5') then do;
		if time_dose56 > 60 then booster_biv = 1;
		if time_dose56 <= 60 then booster_biv = 0;
		if time_dose56 = . then booster_biv = .;
		boostbiv_time = time_dose56;
		if booster_biv = 1 then do;
			boostbiv_vax = vaccine_dose6;
			boostbiv_id = vax_event_id_dose6;
			boostbiv_date = admin_date_dose6;
			boostbiv_check = 'vaccine_dose6';
		end;
	end;
	if boost_check = 'vaccine_dose6' AND vaccine_dose7 in ('Moderna Biv', 'Pfizer Biv 12', 'Pfizer Biv 5') then do;
		if time_dose67 > 60 then booster_biv = 1;
		if time_dose67 <= 60 then booster_biv = 0;
		if time_dose67 = . then booster_biv = .;
		boostbiv_time = time_dose67;
		if booster_biv = 1 then do;
			boostbiv_vax = vaccine_dose7;
			boostbiv_id = vax_event_id_dose7;
			boostbiv_date = admin_date_dose7;
			boostbiv_check = 'vaccine_dose7';
		end;
	end;
	if boost_check = 'vaccine_dose7' AND vaccine_dose8 in ('Moderna Biv', 'Pfizer Biv 12', 'Pfizer Biv 5') then do;
		if time_dose78 > 60 then booster_biv = 1;
		if time_dose78 <= 60 then booster_biv = 0;
		if time_dose78 = . then booster_biv = .;
		boostbiv_time = time_dose78;
		if booster_biv = 1 then do;
			boostbiv_vax = vaccine_dose8;
			boostbiv_id = vax_event_id_dose8;
			boostbiv_date = admin_date_dose8;
			boostbiv_check = 'vaccine_dose8';
		end;
	end;
	/**/
	if prim_series = 1 AND booster_first = 1 AND booster_second in (., 0) AND booster_biv in (.,0) then do;
		boostmono_time = &today -  boost_date;
		boostbiv_add = prim_series + booster;
	end;
	if prim_series = 1 AND booster_first = 1 AND booster_second in (., 0) AND boostmono_time >= 60 AND booster_biv in (.,0) then do;
		needs_boostbiv_now = 1;
	end;
	if prim_series = 1 AND booster_first = 1 AND booster_second in (., 0) AND boostmono_time < 60 AND booster_biv in (.,0) then do;
		needs_boostbiv_now = 0;
	end;
	/******************************************************/
	/* Primary series + two boosters */
	if boost2_check = 'vaccine_dose3' AND vaccine_dose4 in ('Moderna Biv', 'Pfizer Biv 12', 'Pfizer Biv 5') then do;
		if time_dose34 > 60 then booster_biv = 1;
		if time_dose34 <= 60 then booster_biv = 0;
		if time_dose34 = . then booster_biv = .;
		boostbiv_time = time_dose34;
		if booster_biv = 1 then do;
			boostbiv_vax = vaccine_dose4;
			boostbiv_id = vax_event_id_dose4;
			boostbiv_date = admin_date_dose4;
			boostbiv_check = 'vaccine_dose4';
		end;
	end;
	if boost2_check = 'vaccine_dose4' AND vaccine_dose5 in ('Moderna Biv', 'Pfizer Biv 12', 'Pfizer Biv 5') then do;
		if time_dose45 > 60 then booster_biv = 1;
		if time_dose45 <= 60 then booster_biv = 0;
		if time_dose45 = . then booster_biv = .;
		boostbiv_time = time_dose45;
		if booster_biv = 1 then do;
			boostbiv_vax = vaccine_dose5;
			boostbiv_id = vax_event_id_dose5;
			boostbiv_date = admin_date_dose5;
			boostbiv_check = 'vaccine_dose5';
		end;
	end;
	if boost2_check = 'vaccine_dose5' AND vaccine_dose6 in ('Moderna Biv', 'Pfizer Biv 12', 'Pfizer Biv 5') then do;
		if time_dose56 > 60 then booster_biv = 1;
		if time_dose56 <= 60 then booster_biv = 0;
		if time_dose56 = . then booster_biv = .;
		boostbiv_time = time_dose56;
		if booster_biv = 1 then do;
			boostbiv_vax = vaccine_dose6;
			boostbiv_id = vax_event_id_dose6;
			boostbiv_date = admin_date_dose6;
			boostbiv_check = 'vaccine_dose6';
		end;
	end;
	if boost2_check = 'vaccine_dose6' AND vaccine_dose7 in ('Moderna Biv', 'Pfizer Biv 12', 'Pfizer Biv 5') then do;
		if time_dose67 > 60 then booster_biv = 1;
		if time_dose67 <= 60 then booster_biv = 0;
		if time_dose67 = . then booster_biv = .;
		boostbiv_time = time_dose67;
		if booster_biv = 1 then do;
			boostbiv_vax = vaccine_dose7;
			boostbiv_id = vax_event_id_dose7;
			boostbiv_date = admin_date_dose7;
			boostbiv_check = 'vaccine_dose7';
		end;
	end;
	if boost2_check = 'vaccine_dose7' AND vaccine_dose8 in ('Moderna Biv', 'Pfizer Biv 12', 'Pfizer Biv 5') then do;
		if time_dose78 > 60 then booster_biv = 1;
		if time_dose78 <= 60 then booster_biv = 0;
		if time_dose78 = . then booster_biv = .;
		boostbiv_time = time_dose78;
		if booster_biv = 1 then do;
			boostbiv_vax = vaccine_dose8;
			boostbiv_id = vax_event_id_dose8;
			boostbiv_date = admin_date_dose8;
			boostbiv_check = 'vaccine_dose8';
		end;
	end;
	if boost2_check = 'vaccine_dose8' AND vaccine_dose9 in ('Moderna Biv', 'Pfizer Biv 12', 'Pfizer Biv 5') then do;
		if time_dose89 > 60 then booster_biv = 1;
		if time_dose89 <= 60 then booster_biv = 0;
		if time_dose89 = . then booster_biv = .;
		boostbiv_time = time_dose89;
		if booster_biv = 1 then do;
			boostbiv_vax = vaccine_dose9;
			boostbiv_id = vax_event_id_dose9;
			boostbiv_date = admin_date_dose9;
			boostbiv_check = 'vaccine_dose9';
		end;
	end;
	/**/
	if prim_series = 1 AND booster_first = 1 AND booster_second = 1 AND booster_biv in (.,0) then do;
		boostmono_time = &today - boost2_date;
		boostbiv_add = prim_series + booster_first + booster_second;
	end;
	if prim_series = 1 AND booster_first = 1 AND booster_second = 1 AND boostmono_time >= 60 AND booster_biv in (.,0) then do;
		needs_boostbiv_now = 1;
	end;
	if prim_series = 1 AND booster_first = 1 AND booster_second = 1 AND boostmono_time < 60 AND booster_biv in (.,0) then do;
		needs_boostbiv_now = 0;
	end;

	/******************************************************/
	/******************************************************/
	/* Flag last vax event */
	if vaccine_dose8 ^= '' then do;
			last_vax = vaccine_dose8;
			last_id = vax_event_id_dose8;
			last_date = admin_date_dose8;
			last_check = 'vaccine_dose8';
	end;
	if vaccine_dose7 ^= '' and vaccine_dose8 = '' then do;
			last_vax = vaccine_dose7;
			last_id = vax_event_id_dose7;
			last_date = admin_date_dose7;
			last_check = 'vaccine_dose7';
	end;
	if vaccine_dose6 ^= '' and vaccine_dose7 = '' then do;
			last_vax = vaccine_dose6;
			last_id = vax_event_id_dose6;
			last_date = admin_date_dose6;
			last_check = 'vaccine_dose6';
	end;
	if vaccine_dose5 ^= '' and vaccine_dose6 = '' then do;
			last_vax = vaccine_dose5;
			last_id = vax_event_id_dose5;
			last_date = admin_date_dose5;
			last_check = 'vaccine_dose5';
	end;
	if vaccine_dose4 ^= '' and vaccine_dose5 = '' then do;
			last_vax = vaccine_dose4;
			last_id = vax_event_id_dose4;
			last_date = admin_date_dose4;
			last_check = 'vaccine_dose4';
	end;
	if vaccine_dose3 ^= '' and vaccine_dose4 = '' then do;
			last_vax = vaccine_dose3;
			last_id = vax_event_id_dose3;
			last_date = admin_date_dose3;
			last_check = 'vaccine_dose3';
	end;
	if vaccine_dose2 ^= '' and vaccine_dose3 = '' then do;
			last_vax = vaccine_dose2;
			last_id = vax_event_id_dose2;
			last_date = admin_date_dose2;
			last_check = 'vaccine_dose2';
	end;
	if vaccine_dose1 ^= '' and vaccine_dose2 = '' then do;
			last_vax = vaccine_dose1;
			last_id = vax_event_id_dose1;
			last_date = admin_date_dose1;
			last_check = 'vaccine_dose1';
	end;
	if needs_boostbiv_now = 1 and (&today -  last_date) < 60 then do;
		boostmono_time = &today -  last_date;
		needs_boostbiv_now = 0;
	end;
	if booster_biv = 1 then needs_boostbiv_now = 0;
run;

proc print data = completed_extra3 (obs = 10);
	where boostmono_time ^= .;
	var boost_time boost2_time boostmono_time;
run;

proc freq data = completed_extra3;
	where age >= 12;
	table boostbiv_add*needs_boostbiv_now / missing norow nocol nopercent;
run;

proc freq data = completed_extra3;
	table booster_first booster_second booster_biv / missing norow nocol nopercent;
run;

proc freq data = completed_extra3;
	table age_cat*booster_biv / missing norow nocol nopercent;
run;

proc sort data = completed_extra3;
	by booster_biv;
run;

proc means data = completed_extra3;
	by booster_biv;
	var boostbiv_time;
run;


/***********************************************************************/
/***********************************************************************/
/***********************************************************************/
/* 
Create a flag for whether or not someone is up-to-date on their vaccines 
COME BACK AND UPDATE THIS, OUT OF DATE WITH BIVALENT BOOSTERS 9/8/2022
*/
data completed_extra4;
	set completed_extra3;
	/*make a flag for people up to date */
	up_to_date = .;
	format up_to_date_flag $50. up_to_date_date mmddyy10.;
	if age <= 4 then do;
		*series not complete;
		if prim_series = 0 then do;
			up_to_date = 0;
			up_to_date_date = admin_date_dose1;
			up_to_date_vax = vaccine_dose1;
			up_to_date_time = &today - admin_date_dose1;
			up_to_date_id = vax_event_id_dose1;
			up_to_date_flag = 'Not Complete';
		end;
		*series complete;
		if prim_series = 1 then do;
			up_to_date = 1;
			up_to_date_date = completion_date;
			up_to_date_vax = completion_vax;
			up_to_date_time = &today - completion_date;
			up_to_date_id = completion_id;
			up_to_date_flag = 'Series Complete';
		end;
	end;
	if age >= 5 and age <= 11 then do;
		*series not complete;
		if prim_series = 0 then do;
			up_to_date = 0;
			up_to_date_date = admin_date_dose1;
			up_to_date_vax = vaccine_dose1;
			up_to_date_time = &today - admin_date_dose1;
			up_to_date_id = vax_event_id_dose1;
			up_to_date_flag = 'Not Complete';
		end;
		*series complete, eligible for a boost, not boosted;
		if prim_series = 1 and booster_first in (.,0) and elig_boost1_date <= &today then do;
			up_to_date = 0;
			up_to_date_date = completion_date;
			up_to_date_vax = completion_vax;
			up_to_date_time = &today - completion_date;
			up_to_date_id = completion_id;
			up_to_date_flag = 'Needs First Boost';
		end;
		*series complete, boosted;
		if booster_first = 1 then do;
			up_to_date = 1;
			up_to_date_date = boost_date;
			up_to_date_vax = boost_vax;
			up_to_date_time = &today - boost_date;
			up_to_date_id = boost_id;
			up_to_date_flag = 'Booster        ';
		end;
		if booster_second = 1 then do;
			up_to_date = 1;
			up_to_date_date = boost2_date;
			up_to_date_vax = boost2_vax;
			up_to_date_time = &today - boost2_date;
			up_to_date_id = boost2_id;
			up_to_date_flag = 'Second Booster ';
		end;
		*series complete, not eligible for boost;
		if prim_series = 1 and (elig_boost1_date > &today) then do;
			up_to_date = 1;
			up_to_date_date = completion_date;
			up_to_date_vax = completion_vax;
			up_to_date_time = &today - completion_date;
			up_to_date_id = completion_id;
			up_to_date_flag = 'Series Complete';
		end;
	end;
 	if age >= 12 and age ^=. then do;
		*series not complete;
		if prim_series = 0 then do;
			up_to_date = 0; *up_to_date just indicates Yes or No. up_to_date_flag gives more context as to HOW they are not up to date;
			up_to_date_date = admin_date_dose1;
			up_to_date_vax = vaccine_dose1;
			up_to_date_time = &today - admin_date_dose1;
			up_to_date_id = vax_event_id_dose1;
			up_to_date_flag = 'Not Complete';
		end;
		*series complete, eligible for a boost, not boosted;
		if prim_series = 1 and needs_boostbiv_now = 1 then do;
			up_to_date = 0;
			up_to_date_date = last_date;
			up_to_date_vax = last_vax;
			up_to_date_time = &today - last_date;
			up_to_date_id = last_id;
			up_to_date_flag = 'Needs Bivalent Boost';
		end;
		*series complete, not eligible;
		if prim_series = 1 and needs_boostbiv_now = 0 then do;
			up_to_date = 1;
			up_to_date_date = last_date;
			up_to_date_vax = last_vax;
			up_to_date_time = &today - last_date;
			up_to_date_id = last_id;
			up_to_date_flag = 'Series Complete';
		end;
		*series complete, boosted, not eligible;
		if booster_first = 1 and booster_second in (.,0) and needs_boostbiv_now = 0 then do;
			up_to_date = 1;
			up_to_date_date = boost_date;
			up_to_date_vax = boost_vax;
			up_to_date_time = &today - boost_date;
			up_to_date_id = boost_id;
			up_to_date_flag = 'Booster        ';
		end;
		*series complete, boosted twice, not eligible;
		if booster_first = 1 and booster_second = 1 and needs_boostbiv_now = 0 then do;
			up_to_date = 1;
			up_to_date_date = boost2_date;
			up_to_date_vax = boost2_vax;
			up_to_date_time = &today - boost2_date;
			up_to_date_id = boost2_id;
			up_to_date_flag = 'Second Booster ';
		end;
		*has bivalent booster;
		if booster_biv = 1 then do;
			up_to_date = 1;
			up_to_date_date = boostbiv_date;
			up_to_date_vax = boostbiv_vax;
			up_to_date_time = &today - boostbiv_date;
			up_to_date_id = boostbiv_id;
			up_to_date_flag = 'Booster Bivalent';
		end;
	end;

run;

proc freq data = completed_extra4;
	table boost_check boost2_check completion_dose up_to_date_flag up_to_date/ nocol norow nopercent missing;
run;

proc freq data = completed_extra4;
	table needs_boost1_now*up_to_date_flag needs_boost2_now*up_to_date_flag /nocol norow nopercent  missing;
run;

proc freq data = completed_extra4;
	table needs_boost1_now needs_boost2_now up_to_date_flag up_to_date;
run;

proc contents data = completed_extra4;
run;

/* CHECK - output a validation file to check the coding, timing, etc */

proc sql; *shows the total count of each categories of vaccine and its combinations of booster categories;
create table vaccine_combos as
select vaccine_dose1, vaccine_dose2, vaccine_dose3, vaccine_dose4, vaccine_dose5, vaccine_dose6, initiated, 
	prim_series, booster_first, booster_second, count(*) as count,
	completion_dose, boost_check, boost2_check
from completed_extra4
group by vaccine_dose1, vaccine_dose2, vaccine_dose3, vaccine_dose4, vaccine_dose5, vaccine_dose6, completion_dose, boost_check, boost2_check;
quit;

proc sort data = vaccine_combos nodupkey;
by vaccine_dose1 vaccine_dose2 vaccine_dose3 vaccine_dose4 vaccine_dose5 vaccine_dose6 booster_second;
run;

proc sql; *transform each variable from their coded category of 0/1 into a sum total for data check;
create table vaccine_combos_sum as
select vaccine_dose1, vaccine_dose2, vaccine_dose3, vaccine_dose4, vaccine_dose5, vaccine_dose6, count(*) as count, 
		sum(initiated) as initiated, 
		sum(prim_series) as prim_series,
		sum(booster_first) as booster_first,
		sum(booster_second) as booster_second,
		completion_dose,
		boost_check,
		boost2_check
from completed_extra4
group by vaccine_dose1, vaccine_dose2, vaccine_dose3, vaccine_dose4, vaccine_dose5, vaccine_dose6, completion_dose, boost_check, boost2_check;
quit;

proc sort data = vaccine_combos_sum nodupkey; 
by vaccine_dose1 vaccine_dose2 vaccine_dose3 vaccine_dose4 vaccine_dose5 vaccine_dose6 booster_second;
run;

proc sql; *making sure the minimum boost date is at least x days for respective vaccine categories;
create table vaccine_combos_boost as
select vaccine_dose1, vaccine_dose2, vaccine_dose3, vaccine_dose4, vaccine_dose5, vaccine_dose6, count(*) as count, 
		min(boost_time) as boost_time_min,
		max(boost_time) as boost_time_max,
		booster_first,
		booster_second,
		min(boost2_time) as boost2_time_min,
		max(boost2_time) as boost2_time_max,
		completion_dose,
		boost_check,
		boost2_check
from completed_extra4
group by vaccine_dose1, vaccine_dose2, vaccine_dose3, vaccine_dose4, vaccine_dose5, vaccine_dose6, completion_dose, booster_first, boost_check, booster_second, boost2_check;
quit;

proc sort data = vaccine_combos_boost nodupkey; 
by vaccine_dose1 vaccine_dose2 vaccine_dose3 vaccine_dose4 vaccine_dose5 vaccine_dose6 booster_first booster_second;
run;

data vaccine_combos_up_to_date;
	keep recip_id county age age_cat5 
			up_to_date		up_to_date_flag
			initiation_time completion_time completion_dose  boost_time boost_check boost2_time boost2_check 
			boostmono_time boostbiv_time boostbiv_check needs_boostbiv_now last_vax last_date last_check
			admin_date_dose1 vaccine_dose1 admin_date_dose2 vaccine_dose2
			admin_date_dose3 vaccine_dose3 admin_date_dose4 vaccine_dose4
			admin_date_dose5 vaccine_dose5 admin_date_dose6 vaccine_dose6
			admin_date_dose7 vaccine_dose7 admin_date_dose8 vaccine_dose8;
			*initiated initiated_date  
			prim_series completion_date completion_vax 
			elig_boost1_date booster_first boost_vax boost_date needs_boost1_now
			elig_boost2_date booster_second boost2_vax boost2_date needs_boost2_now
			up_to_date_date	up_to_date_vax	up_to_date_time;
	retain recip_id county age age_cat5 
			up_to_date		up_to_date_flag
			initiation_time completion_time completion_dose  boost_time boost_check boost2_time boost2_check 
			boostmono_time boostbiv_time boostbiv_check needs_boostbiv_now last_vax last_date last_check
			admin_date_dose1 vaccine_dose1 admin_date_dose2 vaccine_dose2
			admin_date_dose3 vaccine_dose3 admin_date_dose4 vaccine_dose4
			admin_date_dose5 vaccine_dose5 admin_date_dose6 vaccine_dose6
			admin_date_dose7 vaccine_dose7 admin_date_dose8 vaccine_dose8;
			*initiated initiated_date  
			prim_series completion_date completion_vax
			elig_boost1_date booster_first boost_vax boost_date needs_boost1_now
			elig_boost2_date booster_second boost2_vax boost2_date needs_boost2_now
			up_to_date_date	up_to_date_vax	up_to_date_time;
	set completed_extra4;
	if county not in ('Clackamas', 'Marion', 'Tillamook', 'Douglas','Deschutes', 
		'Lincoln', 'Jackson') then delete; *excel output doesn't take a doc with more than 1 million rows. We remove rows by County to keep it under 1 million. May have to add another county in the future;
run;

data vaccine_combos_bivalent;
	keep recip_id county age age_cat5 
			up_to_date		up_to_date_flag
			initiation_time completion_time completion_dose  boost_time boost_check boost2_time boost2_check 
			boostmono_time boostbiv_time boostbiv_check needs_boostbiv_now last_vax last_date last_check
			admin_date_dose1 vaccine_dose1 admin_date_dose2 vaccine_dose2
			admin_date_dose3 vaccine_dose3 admin_date_dose4 vaccine_dose4
			admin_date_dose5 vaccine_dose5 admin_date_dose6 vaccine_dose6
			admin_date_dose7 vaccine_dose7 admin_date_dose8 vaccine_dose8;
			*initiated initiated_date  
			prim_series completion_date completion_vax 
			elig_boost1_date booster_first boost_vax boost_date needs_boost1_now
			elig_boost2_date booster_second boost2_vax boost2_date needs_boost2_now
			up_to_date_date	up_to_date_vax	up_to_date_time;
	retain recip_id county age age_cat5 
			up_to_date		up_to_date_flag
			initiation_time completion_time completion_dose  boost_time boost_check boost2_time boost2_check 
			boostmono_time boostbiv_time boostbiv_check needs_boostbiv_now last_vax last_date last_check
			admin_date_dose1 vaccine_dose1 admin_date_dose2 vaccine_dose2
			admin_date_dose3 vaccine_dose3 admin_date_dose4 vaccine_dose4
			admin_date_dose5 vaccine_dose5 admin_date_dose6 vaccine_dose6
			admin_date_dose7 vaccine_dose7 admin_date_dose8 vaccine_dose8;
			*initiated initiated_date  
			prim_series completion_date completion_vax
			elig_boost1_date booster_first boost_vax boost_date needs_boost1_now
			elig_boost2_date booster_second boost2_vax boost2_date needs_boost2_now
			up_to_date_date	up_to_date_vax	up_to_date_time;
	set completed_extra4;
	if booster_biv ^= 1 then delete; *excel output doesn't take a doc with more than 1 million rows. We remove rows by County to keep it under 1 million. May have to add another county in the future;
run;

proc export data = vaccine_combos_sum outfile="S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Validation\vaccine_combos4 &today..xlsx" 
	dbms = xlsx replace;
	sheet = "counts";
run;

proc export data = vaccine_combos outfile="S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Validation\vaccine_combos4 &today..xlsx" 
	dbms = xlsx replace;
	sheet = "coding";
run;

proc export data = vaccine_combos_boost outfile="S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Validation\vaccine_combos4 &today..xlsx" 
	dbms = xlsx replace;
	sheet = "boost time check";
run;

proc export data = vaccine_combos_up_to_date outfile="S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Validation\vaccine_combos4 &today..xlsx" 
	dbms = xlsx replace;
	sheet = "up-to-date check";
run;

proc export data = vaccine_combos_bivalent outfile="S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Validation\vaccine_combos4 &today..xlsx" 
	dbms = xlsx replace;
	sheet = "bivalent only";
run;

/*
Place the final person table in the data folder.
Kyle uses this too.
Notice up_to_date is NOT included.
*/

data data.completed_extra_UTD_&thismonday;
	retain 	RECIP_ID
			dose_dose1	ADMIN_DATE_dose1	VAX_EVENT_ID_dose1	vaccine_dose1	count_dose1									
			dose_dose2	ADMIN_DATE_dose2	VAX_EVENT_ID_dose2	vaccine_dose2	count_dose2										
			dose_dose3	ADMIN_DATE_dose3	VAX_EVENT_ID_dose3	vaccine_dose3	count_dose3										
			dose_dose4	ADMIN_DATE_dose4	VAX_EVENT_ID_dose4	vaccine_dose4	count_dose4										
			dose_dose5	ADMIN_DATE_dose5	VAX_EVENT_ID_dose5	vaccine_dose5	count_dose5										
			dose_dose6	ADMIN_DATE_dose6	VAX_EVENT_ID_dose6	vaccine_dose6	count_dose6										
			time_dose12	time_dose13	time_dose23	time_dose24	time_dose34	time_dose45	time_dose56	time_met							
			vaccine	initiated initiated_date	
			prim_series	in_progress	completion_date	completion_vax	completion_id	completion_dose	
			boost_date	boost_vax	boost_id booster_first boost_time	boost_check	
			booster_second boost2_date boost2_vax boost2_id boost2_dose boost2_time boost2_check
			booster_bivalent boostbiv_date boostbiv_vax boostbiv_id boostbiv_dose boostbiv_time boostbiv_check
			flag; *retain only orders, does not exclude variables. up_to_date is not ordered but is at the end of the order since it wasn't mentioned;
	set completed_extra4;
run;

proc freq data = data.completed_extra_utd_&thismonday;
	where age >= 12;
	table up_to_date_flag needs_boostbiv_now needs_boostbiv_now*prim_series needs_boostbiv_now*initiated/ missing norow nocol nopercent; 
run;

proc freq data = data.tableau_imms_boost_utd_&thismonday;
	where age >= 12 AND week >= (&today - 60);
	table week*administrations / missing norow nocol nopercent;
run;

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/* 
Prepare multiple tables to merge with final_imms by recip_id and admin_dates - to flag doses on correct dates 
IE, people_count = 1 is on their first dose, vaccinated = 1 is on 1 J&J or second Moderna/Pfizer, etc
*/
/**************** Keeping the variables people_count, vaccinated, 
and others even though the names don't make sense to preserve downstream Tableau vizzes ***********************/

*We used Final_Imms to create the variables we needed, but Final_Immss is in long format, while we created our variables using wide format.
Now we have to merge back to Final_Imms using Recip_ID admin_date vax_event_id, and we can't use admin_date_doseX or vax_event_id doseX etc,
since Final_Imms don't have those as variables as it was in long format. 
We have to add back in people_count, vaccinated, booster_dose, booster_second_dose, up_to_date.
Since they all use the same field of admin_date, vax_event_id, we have to code each portion separately, and rejoin Final_Imms one at a time.;

/* people_count = initiated */
data people_count;
	keep admin_date recip_id vax_event_id people_count;
	set completed_extra4;
	format admin_date mmddyy10.;
	admin_date = admin_date_dose1;
	vax_event_id = vax_event_id_dose1;
	people_count = 1;
	vaccine = vaccine_dose1;
	if people_count = 0 then delete;
run;

/*vaccinated = 1 J&J, 2 Moderna, 2 Pfizer */
/*create needs_boost and needs_boost1_now */
data vaccinated;
	keep admin_date recip_id vax_event_id vaccinated 
		eligible_date vaccine_boost elig_boost1_date needs_boost needs_boost1_now;
	set completed_extra4;
	format admin_date eligible_date mmddyy10.;
	admin_date = completion_date;
	vax_event_id = completion_id;
	vaccine = completion_vax;
	vaccine_boost = boost_vax;
	vaccinated = prim_series;
	if vaccinated ^= 1 then delete;
run;

/*booster & extra_dose = any extra dose by same manufacturer*/
data booster_dose;
	keep admin_date recip_id vax_event_id booster_first needs_boost2_now;
	set completed_extra4;
	format admin_date mmddyy10.;
	admin_date = boost_date;
	vax_event_id = boost_id;
	vaccine = boost_vax;
	if booster_first ^= 1 then delete;
run;

data booster_second_dose;
	keep admin_date recip_id vax_event_id booster_second;
	set completed_extra4;
	format admin_date mmddyy10.;
	admin_date = boost2_date;
	vax_event_id = boost2_id;
	vaccine = boost2_vax;
	if booster_second ^=1 then delete;
run;

data booster_biv_dose;
	keep admin_date recip_id vax_event_id booster_biv;
	set completed_extra4;
	format admin_date mmddyy10.;
	admin_date = boostbiv_date;
	vax_event_id = boostbiv_id;
	vaccine = boostbiv_vax;
	if booster_biv ^=1 then delete;
run;

data last_dose;
	keep admin_date recip_id vax_event_id needs_boostbiv_now;
	set completed_extra4;
	format admin_date mmddyy10.;
	admin_date = last_date;
	vax_event_id = last_id;
	vaccine = last_vax;
	if last_check = '' then delete;
run;

data up_to_date;
	keep admin_date recip_id vax_event_id up_to_date needs_update up_to_date_flag;
	set completed_extra4;
	format admin_date mmddyy10.;
	admin_date = up_to_date_date;
	vax_event_id = up_to_date_id;
	vaccine = up_to_date_vax;
	if up_to_date = 0 then needs_update = 1;
	if up_to_date = 1 then needs_update = 0;
run;

/*****************************************************************************************************************************************/

/* Check */
/*
proc sort data = up_to_date;
	by recip_id;
run;

proc sort data = vaccinated;
	by recip_id;
run;

proc sort data = extra_dose;
	by recip_id;
run;

proc sort data = people_count;
	by recip_id;
run;

data people_table;
	merge people_count vaccinated extra_dose up_to_date;
	by recip_id;
run;

data completed_vax_up;
	merge completed_extra people_count vaccinated extra_dose up_to_date;
	by recip_id;
run;

proc freq data = completed_vax_up;
	where eligible_date <= &today;
	table needs_boost*up_to_date2 / missing;
run;

proc freq data = completed_vax_up;
	where eligible_date > &today;
	table needs_boost*up_to_date2 / missing;
run;

proc print data = completed_vax_up (obs = 10);
	where up_to_date2 = 0 and needs_boost = 1;
run;
*/
/*****************************************************************************************************************************************/
/*Sort tables to merge with final_imms */
proc sort data = people_count;
	by recip_id admin_date vax_event_id;
run;

proc sort data = vaccinated;
	by recip_id admin_date vax_event_id;
run;

proc sort data = booster_dose;
	by recip_id admin_date vax_event_id;
run;

proc sort data = booster_second_dose;
	by recip_id admin_date vax_event_id;
run;

proc sort data = booster_biv_dose;
	by recip_id admin_date vax_event_id;
run;

proc sort data = last_dose;
	by recip_id admin_date vax_event_id;
run;

proc sort data = up_to_date;
	by recip_id admin_date vax_event_id;
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
/*The variables you want to keep from final_imms*/
data final_imms_merge;
	keep ADMIN_DATE VAX_EVENT_ID RECIP_ID  CVX ADMIN_NAME week filename administrations ONEAPCICSHSRI; 
	retain ADMIN_DATE VAX_EVENT_ID RECIP_ID  CVX ADMIN_NAME week filename administrations ONEAPCICSHSRI; 
	set final_imms1;
run;

proc sort data = final_imms_merge;
	by recip_id admin_date vax_event_id;
run;

/*Merge final_imms with all the other variables we just created by recip_id and vax_Event_id and admin_date*/
/*all rows in final_imms_merge coded temperarily as a = 1 etc. We want to keep all rows from final_imms_merge*/
data tableau_imms0;
	merge final_imms_merge (in = a) people_count (in = b) vaccinated (in = c) booster_dose (in = d) booster_second_dose (in = e)  
			booster_biv_dose (in = f)  last_dose (in = g)  up_to_date (in = h);
	by recip_id admin_date vax_event_id;
	if a = 1 then output;
run;

/*
Merge tableau_imms with the demographic data by recip_id
So each row for each recip_id will have the SAME demographic data
If someone had a different county on different vaccine/dose events, it will now only show the county from the most recent event
*/

proc sort data = tableau_imms0;
	by recip_id;
run;

proc sort data = first_last; *contains the demographic variables of each person;
	by recip_id;
run;

data tableau_imms;
	merge tableau_imms0 first_last;
	by recip_id;
	*zero out some variables for people under 5;
	if age <= 4 then do;
		booster_first = .;
		booster_second = .;
		needs_boost = 0;
		needs_boost1_now = 0;
		needs_boostbiv_now = 0;
	end;
	*combine needs_boost1_now with needs_boostbiv_now;
	needs_boost_now = .;
	/*if age >= 5 AND age <= 11 AND needs_boost1_now = 1 then needs_boost_now = 1;
	if age >= 12 AND age ^= . AND needs_boostbiv_now = 1 then needs_boost_now = 1;*/ 
	if age >= 5 AND age ^= . AND needs_boostbiv_now = 1 then needs_boost_now = 1; *5 - 11 now uses bivalent, so recoding the variable here for Metrics script to use;

	booster_combined = .;
	if age >= 5 AND age <= 11 AND booster_first = 1 then booster = 1;
	if age >= 12 AND age ^= . AND booster_biv = 1 then booster = 1;

	*assign any dose without a flag as unknown;
	unknown_dose = .;
	if administrations = 1 AND people_count ^= 1 AND vaccinated ^= 1 AND booster_first ^= 1 AND booster_second ^= 1 then do;
		unknown_dose = 1;
	end;
	*make sure vaccine and vaccine_ped are not blank;
	format vaccine vaccine_ped vaccine_biv $30.;
		vaccine = put(cvx, cvx.);
		vaccine_ped = put(cvx, cvx_ped.);
		vaccine_biv = put(cvx, cvx_biv.);
	*check = .;
	*if people_count = 1 then check = 1;
	*if vaccinated = 1 then check = 1;
	*if extra_dose = 1 then check = 1;
	*if unknown_dose = 1 then check = 1;
run;

proc freq data = tableau_imms;
	table age_cat*needs_boost1_now age_cat*needs_boostbiv_now age_cat*needs_boost_now / nocol norow nopercent missing;
	format age_cat age_agg.;
run;

proc freq data = tableau_imms;
	where vaccinated = 1;
	table age_cat*needs_boost1_now / nocol norow nopercent missing;
	format age_cat age_agg.;
run;

proc freq data = tableau_imms;
	table rarest_race*needs_update / nocol nopercent  ;
	format rarest_race race.;
run;

proc freq data = tableau_imms;
	table up_to_date;
	run;

proc freq data = up_to_date; *outputs are giving us a smaller total of People_Count compared to Final_Imms. About 11000 people where we somehow didn't code their up_to_date status.;
	table up_to_date;
	run;

proc freq data = tableau_imms;
	table up_to_date_flag*needs_update / nocol norow;
run;

proc freq data = tableau_imms;
	table vaccine vaccine_ped vaccine_biv cvx/ norow nocol nopercent missing;
	format cvx cvx.;
run;

/*
*CHECKS;



proc freq data = tableau_imms;
	where unknown_dose = 1;
	table cvx*vaccine cvx*vaccine_ped/ missing nocol norow nopercent;
run;

proc freq data = tableau_imms;
	table cvx*vaccine cvx*vaccine_ped/ missing nocol norow nopercent;
run;


proc freq data = tableau_imms;
	where rarest_race in (8, 10) and people_count = 1;
	table rarest_race*ONEAPCICSHSRI / missing;
	format rarest_race race.;
run;

proc freq data = tableau_imms;
	where people_count = 1 and ONEAPCICSHSRI not in ('', "NoRE", "noRE");
	table rarest_race*ONEAPCICSHSRI / missing;
	format rarest_race race.;
run;

proc freq data = tableau_imms;
	table vaccine*vaccine_ped / missing;
run;

proc freq data = tableau_imms;
	table age_cat3*booster_first age_cat3*needs_boost age_cat3*needs_boost1_now / missing;
run;

proc freq data = tableau_imms;
	table administrations people_count*vaccinated booster_first unknown_dose / missing;
run;

proc freq data = tableau_imms;
	table unknown_dose*people_count unknown_dose*vaccinated unknown_dose*booster_first / missing;
run;


proc freq data = tableau_imms;
	table administrations*check / missing;
run;

data booster_first_last;
	merge booster_first first_last;
	by recip_id;
	if eligible = . then delete;
run;

data tableau_imms;
	set tableau_imms1 booster_first_last;
run;
*/
/*
*CHECKs;
proc freq data = tableau_imms;
	table people_count vaccinated extra_dose cvx*vaccine county age_cat age_cat2 rarest_race race ethnicity / missing;
	format age_cat age. rarest_race race. cvx cvx.;
run;

proc freq data = tableau_imms;
	where people_count = 1;
	table vaccinated age_cat age_cat2 rarest_race race ethnicity county*age_cat/nocol norow nopercent missing;
	format age_cat age. rarest_race race. cvx cvx.;
run;

proc sort data = tableau_imms;
	by age_cat2;
run;

proc means data = tableau_imms;
	by age_Cat2;
	var age;
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
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/

/*Place in data folder */

data data.tableau_imms_boost_UTD_&thismonday;
set tableau_imms;
run;

proc freq data = data.tableau_imms_boost_UTD_&thismonday;
	table age_cat*booster_first age_cat*booster_biv age_cat*booster/ norow nocol nopercent missing;
run;

proc freq data = data.tableau_imms_boost_UTD_&thismonday;
	table administrations/ norow nocol nopercent missing;
run;

proc freq data = data.tableau_imms_boost_UTD_&thismonday;
	table vaccine vaccine_ped vaccine_biv cvx/ norow nocol nopercent missing;
run;

/*
proc tabulate data=data.tableau_imms_boost_&thismonday;
	format age_cat age_agg.;
  	class age_cat;
	var people_count booster_first booster_second;
   	table age_cat, people_count booster_first booster_second;
run;

proc tabulate data=data.tableau_imms_boost_utd_&thismonday;
	format age_cat age_agg.;
  	class age_cat;
	var people_count booster_first booster_second up_to_date;
   	table age_cat, people_count booster_first booster_second up_to_date;
run;

proc tabulate data=tableau_imms;
	format age_cat age_agg.;
  	class age_cat;
	var people_count booster_first booster_second up_to_date;
   	table age_cat, people_count booster_first booster_second up_to_date;
run;

proc print data = pop.state_demog_psu_2021;
	where DemographicCategory in ('0 to 4', '5 to 11', '12 to 17', '18 to 19', '20 to 49', '50 to 64', '65+');
	var DemographicCategory population;
run;
*/
