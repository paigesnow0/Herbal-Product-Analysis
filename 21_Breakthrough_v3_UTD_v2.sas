DM 'clear log; clear output;';
dm 'odsresults; clear';

proc import datafile = "\\dhs.sdc.pvt\PSOB\ACDP-Outbreaks\2020 Outbreak & Cluster Investigations\Novel Coronavirus 2020\Daily Case Count Tables\Daily Orpheus Case Exports\0_COVID_19_EpiExport_4BT_Tableau.csv"
	out= EpiExport_imp /*Check if SpecDate was imported as Character or Numeric format, then run the EpiExport Data Step with the corresponding code*/
	dbms=csv replace; 
	getnames=yes; 
	guessingrows = 100000;
run;

proc import datafile = "S:\Offices\Portland (800 NE Oregon St)\ACDP-Outbreaks\2020 Outbreak & Cluster Investigations\Novel Coronavirus 2020\Breakthrough Cases\Case Ascertainment\Output Files\0_boosters_current_PS.xlsx"
	out= booster_cases_imp 
	dbms=xlsx replace; 
	getnames=yes; 
run;

proc import datafile = "S:\Offices\Portland (800 NE Oregon St)\ACDP-Outbreaks\2020 Outbreak & Cluster Investigations\Novel Coronavirus 2020\Breakthrough Cases\Case Ascertainment\Output Files\0_onedose_current_PS.xlsx"
	out= onedose_cases_imp 
	dbms=xlsx replace; 
	getnames=yes; 
run;
/*
proc import datafile = "S:\Offices\Portland (800 NE Oregon St)\ACDP-Outbreaks\2020 Outbreak & Cluster Investigations\Novel Coronavirus 2020\Breakthrough Cases\Case Ascertainment\Output Files\0_boosters2_current_PS.xlsx"
	out= booster2_cases_imp 
	dbms=xlsx replace; 
	getnames=yes; 
run;
*/
data opera_alert;
	set data.opera_alert;
	if PersonID = . then delete;
run;

proc sort data = opera_alert nodupkey;
	by PersonID;
run;

data opera_all;
	set data.opera_all;
run;
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/* Merge EpiExport with Booster Case Ascertainment*/

/*data EpiExport_imp1;				Experimental Code to transform SpecDate from Numeric Date to Character then back to Numeric. Failed
keep SpecDate SpecDateChar SpecDate2;
retain SpecDate SpecDateChar SpecDate2;
set EpiExport_imp;
SpecDateChar = put(SpecDate, 100.);
SpecDate2 = input(SpecDateChar, DATETIME.);
run;*/


data EpiExport;
	keep Age BreakThroughCase CaseID CaseStatus County DOD Died Ethnicity Hosp Onset PersonID Race SpecDate2 TrueCaseDate Subtype;
	set EpiExport_imp;
	format SpecDate2 DATETIME.;
	*informat SpecDate2 ANYDTDTM40.;
	*SpecDate2 = input(strip(tranwrd(SpecDate, '#', '')), ANYDTDTM40.); /*The old script here needed to transform the old SpecDate to Numeric Date Format from Character because it wasn't being imported properly.*/
	SpecDate2 = SpecDate; /*Run this code if SpecDate was imported correctly in the Numeric Date Format already*/
	informat SpecDate2 ANYDTDTM40.;
	rename SpecDate = SpecDate_old SpecDate2 = SpecDate;
run;

data booster_cases;
	keep case_id person_id primseries_type vb_type;
	set booster_cases_imp;
	rename case_id = CaseID person_id = PersonID;
run;

data onedose_cases;
	keep person_id one_dose;
	set onedose_cases_imp;
	one_dose = 1;
	rename person_id = PersonID;
run;

data booster2_cases;
	keep case_id person_id booster2;
	set booster_cases_imp;
	booster2 = 1;
	rename case_id = CaseID person_id = PersonID;
	if vb_type = '2ndBooster' then output;
run;

proc sort data = EpiExport;
	by PersonID CaseID;
run;

proc sort data = booster_cases;
	by PersonID CaseID;
run;

proc sort data = booster2_cases;
	by PersonID CaseID;
run;

proc sort data = onedose_cases;
	by PersonID;
run;

data epi_case_boosters;
	merge EpiExport (in = a) booster_cases (in = b) booster2_cases (in = c);
	by PersonID CaseID;
	if a ^= 1 then delete; *delete observations/rows if PersonID CaseID missing from EpiExport;
run;

data epi_case_boosters_unmatched;
	merge EpiExport (in = a) booster_cases (in = b) booster2_cases (in = c);
	by PersonID CaseID;
	if a ^= 1 and b = 1 then output; *double checking;
run;

proc freq data = booster_cases;
	table vb_type / norow nocol nopercent missing;
run;

proc freq data = epi_case_boosters;
	table vb_type / norow nocol nopercent missing; *double checking: should be less than booster_cases since we removed anyone with missing epiexport PersonID CaseID;
run;

data epi_case_boost_one;
	merge epi_case_boosters (in = a) onedose_cases (in = b);
	by PersonID;
	if a ^= 1 then delete;
run;

proc freq data = epi_case_boost_one;
	table one_dose booster2/ norow nocol nopercent;
run;

/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/*Merge cases/boosters with opera report data */

proc sort data = epi_case_boost_one;
	by PersonID;
run;

proc sort data = opera_all;
	by PersonID;
run;

proc sort data = opera_alert;
	by PersonID;
run;

/*
IMPORTANT: work.breakthrough_cases is used for data validation later. 
Keep it to have same # rows as Epi_Export with added columns from the merges. 
*/
data breakthrough_cases;
	merge epi_case_boost_one (in = a) opera_all (in = b) opera_alert (in = c);
	by PersonID;
	if a ^= 1 then delete;
run;

/*Filter by Case Status, County, Person IDs, Case IDs, VaxMaker and Subtype*/
/*I'm not sure why we delete these cases, but it's in the RAs code so I kept it */
data breakthrough_cases2;
	set breakthrough_cases;
	if CaseID in (51021030, 51095224, 51129175, 51144844) then delete;
	if SpecDate = . then delete;
	if CaseStatus not in ('Confirmed', 'Presumptive') then delete;
	if county = 'linn' then do;
		county = 'Linn';
	end;
	if county = 'Out Of State' then delete;
	if Subtype in ('MIS-A', 'MIS-C') then delete;
run;

/*
proc freq data = breakthrough_cases;
	table rarest_race unknown / nocol norow nopercent missing;
run;

proc freq data = breakthrough_cases;
	table vb_type*booster2 / nocol norow nopercent missing;
run;
*/
/***************************************************************************************************************/
/***************************************************************************************************************/
/*Group and Create New Variables*/

data breakthrough_cases2;
	set breakthrough_cases2;

	TCD = datepart(TrueCaseDate);
	vb_type = strip(vb_type);

	Age_new = .;
	if Age >=0 And Age <= 115 then Age_new = Age;
	age_cat = put(Age_new, age_pums.);
	if Age_new = . then age_cat = 'Unknown';
	age_cat2 = put(Age_new, age_pums_new.);
	if Age_new = . then age_cat2 = 'Unknown';
	if Age_new <65 then age_65plus = 'Under 65';
	if Age_new >= 65 then age_65plus = '65+';
	if Age_new = . then age_65plus = 'Unknown';

	Cases = 1;
	Cases18 = 0;
	if (Age_new >= 18 and Age_new ^= .) then Cases18 = 1;

	unknown18 = 0;
	if unknown ^= 0 then do;
		unknown = 1;
	end;
	if (Age_new >= 18 and Age_new ^= .) and unknown = 1 then unknown18 = 1;

	Hospitalized = 0;
	Hospitalized18 = 0;
	Hosp_old = Hosp;
	if hosp_old = 'Y' then Hospitalized = 1;
	if (Age_new >= 18 and Age_new ^= .) and hosp_old = 'Y' then Hospitalized18 = 1;
	
	Died_old = Died;
	Died_new = 0;
	Died18 = 0;
	if died_old = 'Y' then Died_new = 1;
	if (Age_new >= 18 and Age_new ^= .) and died_old = 'Y' then Died18 = 1;

	Breakth = 0;
	Breakth18 = 0;
	if BreakThroughCase = 1 then Breakth = 1;
	if (Age_new >= 18 and Age_new ^= .) and BreakThroughCase = 1 then Breakth18 = 1;

	Breakth_not = 0;
	Breakth_not18 = 0;	
	if BreakThroughCase ^= 1 AND one_dose ^= 1 then Breakth_not = 1;
	if (Age_new >= 18 and Age_new ^= .) and BreakThroughCase ^= 1 AND one_dose ^= 1 then Breakth_not18 = 1;

	UTD_Breakth_Num = 0;
	UTD_Breakth_Num18 = 0;
	if vb_type in ('Primary', '1stBooster', '2ndBooster') AND TCD <= '30OCT21'd  then UTD_Breakth_Num = 1;
	if (Age_new >= 18 and Age_new ^= .) and vb_type in ('Primary', '1stBooster', '2ndBooster') AND TCD <= '30OCT21'd  then UTD_Breakth_Num18 = 1;
	*;
	if vb_type in ('1stBooster', '2ndBooster') AND TCD >= '31OCT21'd AND TCD <= '30APR22'd then UTD_Breakth_Num = 1;
	if (Age_new >= 18 and Age_new ^= .) and vb_type in ('1stBooster', '2ndBooster') AND TCD >= '31OCT21'd AND TCD <= '30APR22'd  then UTD_Breakth_Num18 = 1;
	*;
	if (Age_new >= 18 and Age_new <=49) and vb_type in ('1stBooster', '2ndBooster') AND TCD >= '01MAY22'd then do;
		UTD_Breakth_Num = 1;
		UTD_Breakth_Num18 = 1;
	end;
	if (Age_new >= 50 and Age_new ^= .) and booster2 = 1 AND TCD >= '01MAY22'd then do;
		UTD_Breakth_Num = 1;
		UTD_Breakth_Num18 = 1;
	end;

	Hosp_Breakth = 0;
	Hosp_Breakth18 = 0;
	if Hospitalized = 1 and Breakth = 1 then Hosp_Breakth = 1;
	if Hospitalized18 = 1 and Breakth18 = 1 then Hosp_Breakth18 = 1;

	Hosp_Breakth_not = 0;
	Hosp_Breakth_not18 = 0;
	if Hospitalized = 1 and Breakth_not = 1 then Hosp_Breakth_not = 1;
	if Hospitalized18 = 1 and Breakth_not18 = 1 then Hosp_Breakth_not18 = 1;

	Hosp_UTDB = 0;
	Hosp_UTDB18 = 0;
	if Hospitalized = 1 and UTD_Breakth_Num = 1 then Hosp_UTDB = 1;
	if Hospitalized18 = 1 and UTD_Breakth_Num18 = 1 then Hosp_UTDB18 = 1;
	
	Died_Breakth = 0;
	Died_Breakth18 = 0;
	if Died_new = 1 and Breakth = 1 then Died_Breakth = 1;
	if Died18 = 1 and Breakth18 = 1 then Died_Breakth18 = 1;

	Died_Breakth_not = 0;
	Died_Breakth_not18 = 0;
	if Died_new = 1 and Breakth_not = 1 then Died_Breakth_not = 1;
	if Died18 = 1 and Breakth_not18 = 1 then Died_Breakth_not18 = 1;

	Died_UTDB = 0;
	Died_UTDB18 = 0;
	if Died_new = 1 and UTD_Breakth_Num = 1 then Died_UTDB = 1;
	if Died18 = 1 and UTD_Breakth_Num18 = 1 then Died_UTDB18 = 1;

	format Vaccine $20.;
	if primseries_type = 'JJ' then Vaccine = 'Johnson & Johnson';
	if primseries_type = 'MD' then Vaccine = 'Moderna';
	if primseries_type = 'PF' then Vaccine = 'Pfizer';
	if primseries_type = '' then Vaccine = 'Unknown';

	if race in ('Refused', '[Null]') then do;
		race = 'Unknown';
	end;
	if race in ('Pacific Is.') then do;
		race = 'NH/PI';
	end;
	if race in ('Other') then do;
		race = 'Other Race';
	end;
	if ethnicity = 'Not Hispanic' then do;
		ethnicity = 'Non-Hispanic';
	end;
	if rarest_race = '' then do;
		rarest_race = 'Unknown';
	end;

	/*combined race-ethnicity variable;*/
	format race_ethnicity $40.;	
	if race = 'AI/AN' then race_ethnicity = "Non-Hispanic AI/AN";
	if race = 'Asian' then race_ethnicity = "Non-Hispanic Asian";
	if race = 'NH/PI' then race_ethnicity = "Non-Hispanic NH/PI";
	if race = 'Black' then race_ethnicity = "Non-Hispanic Black";
	if race = 'White' then race_ethnicity = "Non-Hispanic White";
	if race = 'Other Race' then race_ethnicity = "Non-Hispanic Other Race";
	if race = 'Multiracial' then do; race_ethnicity = "Non-Hispanic Multiracial"; end;
	if ethnicity = "Hispanic" then race_ethnicity = "Hispanic" ;
	if (race = 'Unknown' AND ethnicity = "Non-Hispanic") then race_ethnicity = "Unknown";
	if ethnicity = "Unknown" then race_ethnicity = "Unknown";
	
	drop died;
	rename died_new = died;
run;

proc freq data = breakthrough_cases2;
	table   UTD_Breakth_Num18 Hosp_UTDB18 Died_UTDB18 / nocol norow nopercent missing;
run;


/*
proc freq data = breakthrough_cases;
	table age*age_cat age_new*age_cat age_new*age_cat2 age_new*age_65plus / nocol norow nopercent missing;
run;

proc freq data = breakthrough_cases;
	table race ethnicity rarest_race race*race_ethnicity ethnicity*race_ethnicity /nocol norow nopercent missing;
run;

%macro means (var = , var2 = );
title "&var.";
proc means data = breakthrough_cases;
	where &var = 1;
	var age_new ;
run;

proc freq data = breakthrough_cases;
	table &var*&var2 / nocol norow nopercent missing;
run;

title;
%mend;

DM 'clear log; clear output;';
dm 'odsresults; clear';

%means(var = Cases, var2 = Cases);
%means(var = Cases18, var2 = Cases18);
%means(var = unknown, var2 = BreakThroughCase);
%means(var = unknown18, var2 = BreakThroughCase);
%means(var = Hospitalized, var2 = hosp_old);
%means(var = Hospitalized18, var2 = hosp_old);
%means(var = Died, var2 = died_old);
%means(var = Died18, var2 = died_old);
%means(var = Breakth, var2 = BreakThroughCase);
%means(var = Breakth, var2 = vb_type);
%means(var = Breakth18, var2 = BreakThroughCase);
%means(var = Breakth_not, var2 = BreakThroughCase);
%means(var = Breakth_not18, var2 = BreakThroughCase);
%means(var = Booster_Breakth, var2 = vb_type);
%means(var = Booster_Breakth18, var2 = vb_type);
%means(var = Primary_Breakth, var2 = vb_type);
%means(var = Primary_Breakth18, var2 = vb_type);
%means(var = Hosp_Breakth, var2 = vb_type);
%means(var = Hosp_Breakth18, var2 = vb_type);
%means(var = Hosp_Breakth_not, var2 = vb_type);
%means(var = Hosp_Breakth_not18, var2 = vb_type);
%means(var = Hosp_BoostB, var2 = vb_type);
%means(var = Hosp_BoostB18, var2 = vb_type);
%means(var = Hosp_PrimB, var2 = vb_type);
%means(var = Hosp_PrimB18, var2 = vb_type);
%means(var = Died_Breakth, var2 = vb_type);
%means(var = Died_Breakth18, var2 = vb_type);
%means(var = Died_Breakth_not, var2 = vb_type);
%means(var = Died_Breakth_not18, var2 = vb_type);
%means(var = Died_BoostB, var2 = vb_type);
%means(var = Died_BoostB18, var2 = vb_type);
%means(var = Died_PrimB, var2 = vb_type);
%means(var = Died_PrimB18, var2 = vb_type);

%means(var = Hosp_Breakth, var2 = hosp_old);
%means(var = Hosp_Breakth18, var2 = hosp_old);
%means(var = Hosp_Breakth_not, var2 = hosp_old);
%means(var = Hosp_Breakth_not18, var2 = hosp_old);
%means(var = Hosp_BoostB, var2 = hosp_old);
%means(var = Hosp_BoostB18, var2 = hosp_old);
%means(var = Hosp_PrimB, var2 = hosp_old);
%means(var = Hosp_PrimB18, var2 = hosp_old);
%means(var = Died_Breakth, var2 = died_old);
%means(var = Died_Breakth18, var2 = died_old);
%means(var = Died_Breakth_not, var2 = died_old);
%means(var = Died_Breakth_not18, var2 = died_old);
%means(var = Died_BoostB, var2 = died_old);
%means(var = Died_BoostB18, var2 = died_old);
%means(var = Died_PrimB, var2 = died_old);
%means(var = Died_PrimB18, var2 = died_old);
*/

/***************************************************************************************************************/
/***************************************************************************************************************/
/* Create new dates and check formats */
data breakthrough_cases2;
	set breakthrough_cases2;
	format TCD_week TCD_month mmddyy10.;
	TCD = datepart(TrueCaseDate);
	TCD_week = intnx('week',TCD,0,'b');
	TCD_month = intnx('month',TCD,0,'b');
	format TCD mmddyy10.;
run;

proc sort data = breakthrough_cases2;
	by TCD;
run;

proc print data = breakthrough_cases2 (obs = 5);
var SpecDate TrueCaseDate Onset TCD TCD_week TCD_month;
run;

proc sort data = breakthrough_cases2;
	by descending TCD;
run;

proc print data = breakthrough_cases2 (obs = 5);
var SpecDate TrueCaseDate Onset TCD TCD_week TCD_month;
run;

/***************************************************************************************************************/
/***************************************************************************************************************/
/*Delete all cases with no ALERT query
	Drop extra columns
	Delete data prior to February 2021*/

data breakthrough_cases2;
	set breakthrough_cases2;
	drop CaseStatus died_old Hosp_old BreakThroughCase vb_type;
	if TCD < '01FEB21'd then delete;
	if unknown = 1 then delete;
run;

data break.breakthrough_cases_valid;
	set breakthrough_cases2;
	*if TCD < '01JAN21'd then delete;
run;

data break.breakthrough_cases;
	set breakthrough_cases2;
	if TCD < '01FEB21'd then delete;
run;

/***************************************************************************************************************/
/***************************************************************************************************************/
/*Aggregate by Demographics and County */

/* 
proc contents data = breakthrough_cases2 order = varnum;
run;
*/

proc sql;
	create table BT_by_week as
		select TCD_week as date,
				sum(Cases) as Cases,
				sum(Cases18) as Cases18,
				sum(unknown) as unknown,
				sum(unknown18) as unknown18,
				sum(Breakth) as Breakth,
				sum(Breakth18) as Breakth18,
				sum(Breakth_not) as Breakth_not,
				sum(Breakth_not18) as Breakth_not18,
				sum(UTD_Breakth_Num) as UTD_Breakth_Num,
				sum(UTD_Breakth_Num18) as UTD_Breakth_Num18
/*				sum(Booster_Breakth) as Booster_Breakth,
				sum(Booster_Breakth18) as Booster_Breakth18,
				sum(Primary_Breakth) as Primary_Breakth,
				sum(Primary_Breakth18) as Primary_Breakth18
*/		from breakthrough_cases2
		group by date;
quit;

proc sql;
	create table BT_by_month as
		select TCD_month as date,
				sum(Breakth) as Breakth,
				sum(Breakth18) as Breakth18,
				sum(Breakth_not) as Breakth_not,
				sum(Breakth_not18) as Breakth_not18,
				sum(UTD_Breakth_Num) as UTD_Breakth_Num,
				sum(UTD_Breakth_Num18) as UTD_Breakth_Num18,
/*				sum(Booster_Breakth) as Booster_Breakth,
				sum(Booster_Breakth18) as Booster_Breakth18,
				sum(Primary_Breakth) as Primary_Breakth,
				sum(Primary_Breakth18) as Primary_Breakth18,
*/				sum(Hospitalized) as Hospitalized,
				sum(Hospitalized18) as Hospitalized18,
				sum(died) as died,
				sum(Died18) as Died18,
				sum(Hosp_Breakth) as Hosp_Breakth,
				sum(Hosp_Breakth18) as Hosp_Breakth18,
				sum(Hosp_Breakth_not) as Hosp_Breakth_not,
				sum(Hosp_Breakth_not18) as Hosp_Breakth_not18,
				sum(Hosp_UTDB) as Hosp_UTDB,
				sum(Hosp_UTDB18) as Hosp_UTDB18,
/*				sum(Hosp_BoostB) as Hosp_BoostB,
				sum(Hosp_BoostB18) as Hosp_BoostB18,
				sum(Hosp_PrimB) as Hosp_PrimB,
				sum(Hosp_PrimB18) as Hosp_PrimB18,
*/				sum(Died_Breakth) as Died_Breakth,
				sum(Died_Breakth18) as Died_Breakth18,
				sum(Died_Breakth_not) as Died_Breakth_not,
				sum(Died_Breakth_not18) as Died_Breakth_not18,
				sum(Died_UTDB) as Died_UTDB,
				sum(Died_UTDB18) as Died_UTDB18
/*				sum(Died_BoostB) as Died_BoostB,
				sum(Died_BoostB18) as Died_BoostB18,
				sum(Died_PrimB) as Died_PrimB,
				sum(Died_PrimB18) as Died_PrimB18
*/		from breakthrough_cases2
		group by date;
quit;

proc sql;
	create table BT_by_week_cty as
		select TCD_week as date,
				County,
				sum(Cases) as Cases,
				sum(Cases18) as Cases18,
				sum(unknown) as unknown,
				sum(unknown18) as unknown18,
				sum(Breakth) as Breakth,
				sum(Breakth18) as Breakth18,
				sum(Breakth_not) as Breakth_not,
				sum(Breakth_not18) as Breakth_not18,
				sum(UTD_Breakth_Num) as UTD_Breakth_Num,
				sum(UTD_Breakth_Num18) as UTD_Breakth_Num18
/*				sum(Booster_Breakth) as Booster_Breakth,
				sum(Booster_Breakth18) as Booster_Breakth18,
				sum(Primary_Breakth) as Primary_Breakth,
				sum(Primary_Breakth18) as Primary_Breakth18
*/
		from breakthrough_cases2
		group by date, County;
quit;


proc sql;
	create table BT_by_month_cty as
		select TCD_month as date,
				County,
				sum(Breakth) as Breakth,
				sum(Breakth18) as Breakth18,
				sum(Breakth_not) as Breakth_not,
				sum(Breakth_not18) as Breakth_not18,
				sum(UTD_Breakth_Num) as UTD_Breakth_Num,
				sum(UTD_Breakth_Num18) as UTD_Breakth_Num18,
/*				sum(Booster_Breakth) as Booster_Breakth,
				sum(Booster_Breakth18) as Booster_Breakth18,
				sum(Primary_Breakth) as Primary_Breakth,
				sum(Primary_Breakth18) as Primary_Breakth18,
*/				sum(Hospitalized) as Hospitalized,
				sum(Hospitalized18) as Hospitalized18,
				sum(died) as died,
				sum(Died18) as Died18,
				sum(Hosp_Breakth) as Hosp_Breakth,
				sum(Hosp_Breakth18) as Hosp_Breakth18,
				sum(Hosp_Breakth_not) as Hosp_Breakth_not,
				sum(Hosp_Breakth_not18) as Hosp_Breakth_not18,
				sum(Hosp_UTDB) as Hosp_UTDB,
				sum(Hosp_UTDB18) as Hosp_UTDB18,
/*				sum(Hosp_BoostB) as Hosp_BoostB,
				sum(Hosp_BoostB18) as Hosp_BoostB18,
				sum(Hosp_PrimB) as Hosp_PrimB,
				sum(Hosp_PrimB18) as Hosp_PrimB18,
*/				sum(Died_Breakth) as Died_Breakth,
				sum(Died_Breakth18) as Died_Breakth18,
				sum(Died_Breakth_not) as Died_Breakth_not,
				sum(Died_Breakth_not18) as Died_Breakth_not18,
				sum(Died_UTDB) as Died_UTDB,
				sum(Died_UTDB18) as Died_UTDB18
/*				sum(Died_BoostB) as Died_BoostB,
				sum(Died_BoostB18) as Died_BoostB18,
				sum(Died_PrimB) as Died_PrimB,
				sum(Died_PrimB18) as Died_PrimB18
*/
		from breakthrough_cases2
		group by date, County;
quit;

data BT_by_week;
	set BT_by_week;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = 'All Oregonians';
	DemographicCategory = 'All Oregonians';
run;

data BT_by_month;
	set BT_by_month;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = 'All Oregonians';
	DemographicCategory = 'All Oregonians';
run;

data BT_by_week_cty;
	set BT_by_week_cty;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = 'All Oregonians';
	DemographicCategory = 'All Oregonians';
run;

data BT_by_month_cty;
	set BT_by_month_cty;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = 'All Oregonians';
	DemographicCategory = 'All Oregonians';
run;

/*
%let var = age_cat;
%let type = "Age Groups";
*/

%macro aggregate (var = , type = );
proc sql;
	create table BT_by_week_&var as
		select TCD_week as date,
				&var,
				sum(Cases) as Cases,
				sum(Cases18) as Cases18,
				sum(unknown) as unknown,
				sum(unknown18) as unknown18,
				sum(Breakth) as Breakth,
				sum(Breakth18) as Breakth18,
				sum(Breakth_not) as Breakth_not,
				sum(Breakth_not18) as Breakth_not18,
				sum(UTD_Breakth_Num) as UTD_Breakth_Num,
				sum(UTD_Breakth_Num18) as UTD_Breakth_Num18
/*				sum(Booster_Breakth) as Booster_Breakth,
				sum(Booster_Breakth18) as Booster_Breakth18,
				sum(Primary_Breakth) as Primary_Breakth,
				sum(Primary_Breakth18) as Primary_Breakth18
*/		from breakthrough_cases2
		group by date, &var;
quit;

proc sql;
	create table BT_by_month_&var as
		select TCD_month as date,
				&var,
				sum(Breakth) as Breakth,
				sum(Breakth18) as Breakth18,
				sum(Breakth_not) as Breakth_not,
				sum(Breakth_not18) as Breakth_not18,
				sum(UTD_Breakth_Num) as UTD_Breakth_Num,
				sum(UTD_Breakth_Num18) as UTD_Breakth_Num18,
/*				sum(Booster_Breakth) as Booster_Breakth,
				sum(Booster_Breakth18) as Booster_Breakth18,
				sum(Primary_Breakth) as Primary_Breakth,
				sum(Primary_Breakth18) as Primary_Breakth18,
*/				sum(Hospitalized) as Hospitalized,
				sum(Hospitalized18) as Hospitalized18,
				sum(died) as died,
				sum(Died18) as Died18,
				sum(Hosp_Breakth) as Hosp_Breakth,
				sum(Hosp_Breakth18) as Hosp_Breakth18,
				sum(Hosp_Breakth_not) as Hosp_Breakth_not,
				sum(Hosp_Breakth_not18) as Hosp_Breakth_not18,
				sum(Hosp_UTDB) as Hosp_UTDB,
				sum(Hosp_UTDB18) as Hosp_UTDB18,
/*				sum(Hosp_BoostB) as Hosp_BoostB,
				sum(Hosp_BoostB18) as Hosp_BoostB18,
				sum(Hosp_PrimB) as Hosp_PrimB,
				sum(Hosp_PrimB18) as Hosp_PrimB18,
*/				sum(Died_Breakth) as Died_Breakth,
				sum(Died_Breakth18) as Died_Breakth18,
				sum(Died_Breakth_not) as Died_Breakth_not,
				sum(Died_Breakth_not18) as Died_Breakth_not18,
				sum(Died_UTDB) as Died_UTDB,
				sum(Died_UTDB18) as Died_UTDB18
/*				sum(Died_BoostB) as Died_BoostB,
				sum(Died_BoostB18) as Died_BoostB18,
				sum(Died_PrimB) as Died_PrimB,
				sum(Died_PrimB18) as Died_PrimB18
*/
		from breakthrough_cases2
		group by date, &var;
quit;

proc sql;
	create table BT_by_week_&var._cty as
		select TCD_week as date,
				&var,
				County,
				sum(Cases) as Cases,
				sum(Cases18) as Cases18,
				sum(unknown) as unknown,
				sum(unknown18) as unknown18,
				sum(Breakth) as Breakth,
				sum(Breakth18) as Breakth18,
				sum(Breakth_not) as Breakth_not,
				sum(Breakth_not18) as Breakth_not18,
				sum(UTD_Breakth_Num) as UTD_Breakth_Num,
				sum(UTD_Breakth_Num18) as UTD_Breakth_Num18
/*				sum(Booster_Breakth) as Booster_Breakth,
				sum(Booster_Breakth18) as Booster_Breakth18,
				sum(Primary_Breakth) as Primary_Breakth,
				sum(Primary_Breakth18) as Primary_Breakth18
*/
		from breakthrough_cases2
		group by date, &var, County;
quit;


proc sql;
	create table BT_by_month_&var._cty as
		select TCD_month as date,
				&var,
				County,
				sum(Breakth) as Breakth,
				sum(Breakth18) as Breakth18,
				sum(Breakth_not) as Breakth_not,
				sum(Breakth_not18) as Breakth_not18,
				sum(UTD_Breakth_Num) as UTD_Breakth_Num,
				sum(UTD_Breakth_Num18) as UTD_Breakth_Num18,
/*				sum(Booster_Breakth) as Booster_Breakth,
				sum(Booster_Breakth18) as Booster_Breakth18,
				sum(Primary_Breakth) as Primary_Breakth,
				sum(Primary_Breakth18) as Primary_Breakth18,
*/				sum(Hospitalized) as Hospitalized,
				sum(Hospitalized18) as Hospitalized18,
				sum(died) as died,
				sum(Died18) as Died18,
				sum(Hosp_Breakth) as Hosp_Breakth,
				sum(Hosp_Breakth18) as Hosp_Breakth18,
				sum(Hosp_Breakth_not) as Hosp_Breakth_not,
				sum(Hosp_Breakth_not18) as Hosp_Breakth_not18,
				sum(Hosp_UTDB) as Hosp_UTDB,
				sum(Hosp_UTDB18) as Hosp_UTDB18,
/*				sum(Hosp_BoostB) as Hosp_BoostB,
				sum(Hosp_BoostB18) as Hosp_BoostB18,
				sum(Hosp_PrimB) as Hosp_PrimB,
				sum(Hosp_PrimB18) as Hosp_PrimB18,
*/				sum(Died_Breakth) as Died_Breakth,
				sum(Died_Breakth18) as Died_Breakth18,
				sum(Died_Breakth_not) as Died_Breakth_not,
				sum(Died_Breakth_not18) as Died_Breakth_not18,
				sum(Died_UTDB) as Died_UTDB,
				sum(Died_UTDB18) as Died_UTDB18
/*				sum(Died_BoostB) as Died_BoostB,
				sum(Died_BoostB18) as Died_BoostB18,
				sum(Died_PrimB) as Died_PrimB,
				sum(Died_PrimB18) as Died_PrimB18
*/		from breakthrough_cases2
		group by date, &var, County;
quit;

data BT_by_week_&var;
	set BT_by_week_&var;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
run;

data BT_by_month_&var;
	set BT_by_month_&var;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
run;

data BT_by_week_&var._cty;
	set BT_by_week_&var._cty;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
run;

data BT_by_month_&var._cty;
	set BT_by_month_&var._cty;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
run;
%mend;

%aggregate (var = age_cat, type = "Age Groups");
%aggregate (var = age_cat2, type = "Age Groups");
%aggregate (var = age_65plus, type = "Age Groups");
%aggregate (var = race, type = "Race OMB");
%aggregate (var = ethnicity, type = "Ethnicity OMB");
%aggregate (var = race_ethnicity, type = "Race & Ethnicity OMB");
%aggregate (var = rarest_race, type = "Race");


data BT_by_week_age_cat2;
	set BT_by_week_age_cat2;
	if age_cat2 in ('0 to 4', '5 to 11', '12 to 17', '18 to 19', 'Unknown') then delete;
run;

data BT_by_week_age_cat2_cty;
	set BT_by_week_age_cat2_cty;
	if age_cat2 in ('0 to 4', '5 to 11', '12 to 17', '18 to 19', 'Unknown') then delete;
run;

data BT_by_month_age_cat2;
	set BT_by_month_age_cat2;
	if age_cat2 in ('0 to 4', '5 to 11', '12 to 17', '18 to 19', 'Unknown') then delete;
run;

data BT_by_month_age_cat2_cty;
	set BT_by_month_age_cat2_cty;
	if age_cat2 in ('0 to 4', '5 to 11', '12 to 17', '18 to 19', 'Unknown') then delete;
run;

data BT_by_week_age_65plus;
	set BT_by_week_age_65plus;
	if age_65plus in ('65+', 'Unknown') then delete;
run;

data BT_by_week_age_65plus_cty;
	set BT_by_week_age_65plus_cty;
	if age_65plus in ('65+', 'Unknown') then delete;
run;

data BT_by_month_age_65plus;
	set BT_by_month_age_65plus;
	if age_65plus in ('65+', 'Unknown') then delete;
run;

data BT_by_month_age_65plus_cty;
	set BT_by_month_age_65plus_cty;
	if age_65plus in ('65+', 'Unknown') then delete;
run;

data BT_week_all;
	set BT_by_week BT_by_week_age_cat BT_by_week_age_cat2 BT_by_week_age_65plus 
		BT_by_week_rarest_race BT_by_week_race BT_by_week_ethnicity BT_by_week_race_ethnicity;
		County = 'Oregon            ';
		drop age_cat age_cat2 age_65plus rarest_race race ethnicity race_ethnicity;
run;

data BT_month_all;
	set BT_by_month BT_by_month_age_cat BT_by_month_age_cat2 BT_by_month_age_65plus 
		BT_by_month_rarest_race BT_by_month_race BT_by_month_ethnicity BT_by_month_race_ethnicity;
		County = 'Oregon            ';
		drop age_cat age_cat2 age_65plus rarest_race race ethnicity race_ethnicity;
run;

data BT_week_all_cty;
	set BT_by_week_cty BT_by_week_age_cat_cty BT_by_week_age_cat2_cty BT_by_week_age_65plus_cty 
		BT_by_week_rarest_race_cty BT_by_week_race_cty BT_by_week_ethnicity_cty BT_by_week_race_ethnicity_cty;
		drop age_cat age_cat2 age_65plus rarest_race race ethnicity race_ethnicity;
run;

data BT_month_all_cty;
	set BT_by_month_cty BT_by_month_age_cat_cty BT_by_month_age_cat2_cty BT_by_month_age_65plus_cty 
		BT_by_month_rarest_race_cty BT_by_month_race_cty BT_by_month_ethnicity_cty BT_by_month_race_ethnicity_cty;
		drop age_cat age_cat2 age_65plus rarest_race race ethnicity race_ethnicity;
run;


/* Case aggregation is finished*/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/* Move on to processing and aggregating vax data */

data tableau_imms_break;
	keep recip_id admin_date fully_vaxd date_week date_month County age age_cat2 rarest_race race ethnicity race_ethnicity UTD_Population UTD_Population18 people_count people_count18 vaccinated booster_first booster_second vaccinated18 booster18 elig_boost1_date;
	retain recip_id admin_date fully_vaxd date_week date_month County age age_cat2 rarest_race race ethnicity race_ethnicity UTD_Population UTD_Population18 people_count people_count18 vaccinated booster_first booster_second vaccinated18 booster18 elig_boost1_date;
	set data.tableau_imms_boost_utd_&break_date;
	format fully_vaxd date_week date_month mmddyy10.;
	rename rarest_race = rarest_race_old age_cat2 = age_cat2_old;

	fully_vaxd = admin_date + 14;
	date_week = intnx('week',fully_vaxd,0,'b');
	date_month = intnx('month',fully_vaxd,0,'b');
	vaccinated18 = .;
	booster18=.;
	people_count18 = .;
	if (vaccinated = 1 and age >= 18) then do;
		vaccinated18 = 1;
	end;
	if (booster_first = 1 and age >= 18) then do;
		booster18 = 1;
	end;
	if (people_count = 1 and age >= 18) then do;
		people_count18 = 1;
	end;
/*
	UTD_Population = .;
	UTD_Population18 =.;
	if vaccinated = 1 AND fully_vaxd <= '16OCT21'd  then UTD_Population = 1;
	if vaccinated18 = 1 AND fully_vaxd <= '16OCT21'd  then UTD_Population18 = 1;
	if booster = 1 AND fully_vaxd >= '17OCT21'd then UTD_Population = 1;
	if booster18 = 1 AND fully_vaxd >= '17OCT21'd AND fully_vaxd <='30APR22'd then UTD_Population18 = 1;
	if booster = 1 AND fully_vaxd >= '01MAY22'd AND age >= 18 AND age <= 49 then UTD_Population18 = 1;
	if booster_second = 1 AND fully_vaxd >= '01MAY22'd AND age >= 50 AND age ^=. then UTD_Population18 = 1;
*/

	if vaccinated in (0, .) AND booster_first in (0, .) AND UTD_Population in (0,.) AND people_count in (0,.) AND booster_second in (0,.) then delete;
run;

/*
proc freq data = tableau_imms_break;
	table UTD_Population UTD_Population18 vaccinated vaccinated18 booster booster18 / nocol norow nopercent missing;
run;

proc freq data = tableau_imms_break;
	where fully_vaxd <= '14AUG21'd;
	table  vaccinated vaccinated18  / nocol norow nopercent missing;
run;

proc freq data = tableau_imms_break;
	where fully_vaxd >= '15AUG21'd;
	table booster booster18 / nocol norow nopercent missing;
run;
*/
data tableau_imms_break;
	set tableau_imms_break;
	all = "All Oregonians";
	rarest_race = put(rarest_race_old, race.);
	age_cat = put(age, age_pums.);
	age_cat2 = put(age, age_pums_new.);
	age_65plus = age_cat2_old;
run;

data data.tableau_imms_break;
	set tableau_imms_break;
run;

/*
data tableau_imms_break;
	set data.tableau_imms_break;
run;
*/
/*Create a list with all dates by week and month in data set */
ods output OneWayFreqs = week_list;
proc freq data = tableau_imms_break;
	table date_week / norow nocol nopercent missing;
run;
ods output close;

ods output OneWayFreqs = month_list;
proc freq data = tableau_imms_break;
	table date_month / norow nocol nopercent missing;
run;
ods output close;

data week_list;
	keep date_week;
	set week_list;
run;

data month_list;
	keep date_month;
	set month_list;
run;


%let var = rarest_race;
%let type = "Race";
%let labl = rare;

%macro cum_alert (var = , type = , labl = );
dm "odsresults; clear";
proc sort data = tableau_imms_break;
	by &var;
run;

ods output OneWayFreqs = cumw_&labl._1;
proc freq data = tableau_imms_break;
	by &var;
	where vaccinated = 1 and age >=18 and age ^=.;
	table date_week /nocol norow nopercent;
run;
ods output close;

ods output OneWayFreqs = cumw_&labl._2;
proc freq data = tableau_imms_break;
	by &var;
	where booster_first = 1 and age >=18 and age ^=.;
	table date_week /nocol norow nopercent;
run;
ods output close;

ods output OneWayFreqs = cumw_&labl._3;
proc freq data = tableau_imms_break;
	by &var;
	where booster_first = 1 and age >=18 and age <= 49;
	table date_week /nocol norow nopercent;
run;
ods output close;

ods output OneWayFreqs = cumw_&labl._4;
proc freq data = tableau_imms_break;
	by &var;
	where booster_second = 1 and age >=50 and age ^=.;
	table date_week /nocol norow nopercent;
run;
ods output close;

ods output OneWayFreqs = cumw_&labl._5;
proc freq data = tableau_imms_break;
	by &var;
	where people_count = 1;
	table date_week /nocol norow nopercent;
run;
ods output close;

ods output OneWayFreqs = cumw_&labl._6;
proc freq data = tableau_imms_break;
	by &var;
	where people_count18 = 1;
	table date_week /nocol norow nopercent;
run;
ods output close; 


data cumw_&labl._1;
	keep DemographicType DemographicCategory County date_week vaccinated18;
	retain DemographicType DemographicCategory County date_week vaccinated18;
	set cumw_&labl._1;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	County = "Oregon";
	vaccinated18 = CumFrequency;
run;

data cumw_&labl._2;
	keep DemographicType DemographicCategory County date_week boosted18;
	retain DemographicType DemographicCategory County date_week boosted18;
	set cumw_&labl._2;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	County = "Oregon";
	boosted18 = CumFrequency;
run;

data cumw_&labl._3;
	keep DemographicType DemographicCategory County date_week boosted1849;
	retain DemographicType DemographicCategory County date_week boosted1849;
	set cumw_&labl._3;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	County = "Oregon";
	boosted1849 = CumFrequency;
run;

data cumw_&labl._4;
	keep DemographicType DemographicCategory County date_week boosted50;
	retain DemographicType DemographicCategory County date_week boosted50;
	set cumw_&labl._4;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	County = "Oregon";
	boosted50 = CumFrequency;
run;

data cumw_&labl._5;
	keep DemographicType DemographicCategory County date_week people_count;
	retain DemographicType DemographicCategory County date_week people_count;
	set cumw_&labl._5;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	County = "Oregon";
	people_count = CumFrequency;
run;

data cumw_&labl._6;
	keep DemographicType DemographicCategory County date_week people_count18;
	retain DemographicType DemographicCategory County date_week people_count18;
	set cumw_&labl._6;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	County = "Oregon";
	people_count18 = CumFrequency;
run;

data cumw_&labl._merged;
	merge cumw_&labl._1 cumw_&labl._2 cumw_&labl._3 cumw_&labl._4 cumw_&labl._5 cumw_&labl._6;
	by DemographicType DemographicCategory  date_week;

run;

proc sort data = cumw_&labl._merged;
	by DemographicType DemographicCategory County date_week;
run;

data cumw_&labl._merged;
	update cumw_&labl._merged (obs = 0) cumw_&labl._merged;
	by DemographicType DemographicCategory County;
	output;
run;

data cum_week_&labl;
	keep DemographicType DemographicCategory County date_week UTD_population18 people_count people_count18;
	retain DemographicType DemographicCategory County date_week UTD_population18 people_count people_count18;
	set cumw_&labl._merged;
	if people_count = . then do;
		people_count = 0;
	end;
	if people_count18 = . then do;
		people_count18 = 0;
	end;
	if vaccinated18 = . then do;
		vaccinated18 = 0;
	end;
	if boosted18 = . then do;
		boosted18 = 0;
	end;
	if boosted1849 = . then do;
		boosted1849 = 0;
	end;
	if boosted50 = . then do;
		boosted50 = 0;
	end;
	UTD_population18 = .;
	if date_week <= '30OCT21'd then UTD_population18 = vaccinated18;
	if date_week >= '31OCT21'd AND date_week <= '30APR22'd then UTD_population18 = boosted18;
	if date_week >= '01MAY22'd then UTD_population18 = boosted1849 + boosted50;
run;

*;
ods output OneWayFreqs = cumm_&labl._1;
proc freq data = tableau_imms_break;
	by &var;
	where vaccinated = 1 and age >=18 and age ^=.;
	table date_month /nocol norow nopercent;
run;
ods output close;

ods output OneWayFreqs = cumm_&labl._2;
proc freq data = tableau_imms_break;
	by &var;
	where booster_first = 1 and age >=18 and age ^=.;
	table date_month /nocol norow nopercent;
run;
ods output close;

ods output OneWayFreqs = cumm_&labl._3;
proc freq data = tableau_imms_break;
	by &var;
	where booster_first = 1 and age >=18 and age <= 49;
	table date_month /nocol norow nopercent;
run;
ods output close;

ods output OneWayFreqs = cumm_&labl._4;
proc freq data = tableau_imms_break;
	by &var;
	where booster_second = 1 and age >=50 and age ^=.;
	table date_month /nocol norow nopercent;
run;
ods output close;

ods output OneWayFreqs = cumm_&labl._5;
proc freq data = tableau_imms_break;
	by &var;
	where people_count = 1;
	table date_month /nocol norow nopercent;
run;
ods output close;

ods output OneWayFreqs = cumm_&labl._6;
proc freq data = tableau_imms_break;
	by &var;
	where people_count18 = 1;
	table date_month /nocol norow nopercent;
run;
ods output close; 


data cumm_&labl._1;
	keep DemographicType DemographicCategory County date_month vaccinated18;
	retain DemographicType DemographicCategory County date_month vaccinated18;
	set cumm_&labl._1;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	County = "Oregon";
	vaccinated18 = CumFrequency;
run;

data cumm_&labl._2;
	keep DemographicType DemographicCategory County date_month boosted18;
	retain DemographicType DemographicCategory County date_month boosted18;
	set cumm_&labl._2;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	County = "Oregon";
	boosted18 = CumFrequency;
run;

data cumm_&labl._3;
	keep DemographicType DemographicCategory County date_month boosted1849;
	retain DemographicType DemographicCategory County date_month boosted1849;
	set cumm_&labl._3;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	County = "Oregon";
	boosted1849 = CumFrequency;
run;

data cumm_&labl._4;
	keep DemographicType DemographicCategory County date_month boosted50;
	retain DemographicType DemographicCategory County date_month boosted50;
	set cumm_&labl._4;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	County = "Oregon";
	boosted50 = CumFrequency;
run;

data cumm_&labl._5;
	keep DemographicType DemographicCategory County date_month people_count;
	retain DemographicType DemographicCategory County date_month people_count;
	set cumm_&labl._5;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	County = "Oregon";
	people_count = CumFrequency;
run;

data cumm_&labl._6;
	keep DemographicType DemographicCategory County date_month people_count18;
	retain DemographicType DemographicCategory County date_month people_count18;
	set cumm_&labl._6;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	County = "Oregon";
	people_count18 = CumFrequency;
run;

data cumm_&labl._merged;
	merge cumm_&labl._1 cumm_&labl._2 cumm_&labl._3 cumm_&labl._4 cumm_&labl._5 cumm_&labl._6;
	by DemographicType DemographicCategory  date_month;
	UTD_population18 = .;
	if date_month <= '30OCT21'd then UTD_population18 = vaccinated18;
	if date_month >= '31OCT21'd AND date_month <= '30APR22'd then UTD_population18 = boosted18;
	if date_month >= '01MAY22'd then UTD_population18 = boosted1849 + boosted50;
run;

proc sort data = cumm_&labl._merged;
	by DemographicType DemographicCategory County date_month;
run;

data cumm_&labl._merged;
	update cumm_&labl._merged (obs = 0) cumm_&labl._merged;
	by DemographicType DemographicCategory County;
	output;
run;

data cum_month_&labl;
	keep DemographicType DemographicCategory County date_month UTD_population18 people_count people_count18;
	retain DemographicType DemographicCategory County date_month UTD_population18 people_count people_count18;
	set cumm_&labl._merged;
	if people_count = . then do;
		people_count = 0;
	end;
	if people_count18 = . then do;
		people_count18 = 0;
	end;
	if vaccinated18 = . then do;
		vaccinated18 = 0;
	end;
	if boosted18 = . then do;
		boosted18 = 0;
	end;
	if boosted1849 = . then do;
		boosted1849 = 0;
	end;
	if boosted50 = . then do;
		boosted50 = 0;
	end;
	UTD_population18 = .;
	if date_month <= '30OCT21'd then UTD_population18 = vaccinated18;
	if date_month >= '31OCT21'd AND date_month <= '30APR22'd then UTD_population18 = boosted18;
	if date_month >= '01MAY22'd then UTD_population18 = boosted1849 + boosted50;
run;

*;
proc sort data = tableau_imms_break;
	by &var county;
run;

ods output OneWayFreqs = cumw_&labl._1cty;
proc freq data = tableau_imms_break;
	by &var county;
	where vaccinated = 1 and age >=18 and age ^=.;
	table date_week /nocol norow nopercent;
run;
ods output close;

ods output OneWayFreqs = cumw_&labl._2cty;
proc freq data = tableau_imms_break;
	by &var county;
	where booster_first = 1 and age >=18 and age ^=.;
	table date_week /nocol norow nopercent;
run;
ods output close;

ods output OneWayFreqs = cumw_&labl._3cty;
proc freq data = tableau_imms_break;
	by &var county;
	where booster_first = 1 and age >=18 and age <= 49;
	table date_week /nocol norow nopercent;
run;
ods output close;

ods output OneWayFreqs = cumw_&labl._4cty;
proc freq data = tableau_imms_break;
	by &var county;
	where booster_second = 1 and age >=50 and age ^=.;
	table date_week /nocol norow nopercent;
run;
ods output close;

ods output OneWayFreqs = cumw_&labl._5cty;
proc freq data = tableau_imms_break;
	by &var county;
	where people_count = 1;
	table date_week /nocol norow nopercent;
run;
ods output close;

ods output OneWayFreqs = cumw_&labl._6cty;
proc freq data = tableau_imms_break;
	by &var county;
	where people_count18 = 1;
	table date_week /nocol norow nopercent;
run;
ods output close; 


data cumw_&labl._1cty;
	keep DemographicType DemographicCategory County date_week vaccinated18;
	retain DemographicType DemographicCategory County date_week vaccinated18;
	set cumw_&labl._1cty;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	vaccinated18 = CumFrequency;
run;

data cumw_&labl._2cty;
	keep DemographicType DemographicCategory County date_week boosted18;
	retain DemographicType DemographicCategory County date_week boosted18;
	set cumw_&labl._2cty;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	boosted18 = CumFrequency;
run;

data cumw_&labl._3cty;
	keep DemographicType DemographicCategory County date_week boosted1849;
	retain DemographicType DemographicCategory County date_week boosted1849;
	set cumw_&labl._3cty;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	boosted1849 = CumFrequency;
run;

data cumw_&labl._4cty;
	keep DemographicType DemographicCategory County date_week boosted50;
	retain DemographicType DemographicCategory County date_week boosted50;
	set cumw_&labl._4cty;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	boosted50 = CumFrequency;
run;

data cumw_&labl._5cty;
	keep DemographicType DemographicCategory County date_week people_count;
	retain DemographicType DemographicCategory County date_week people_count;
	set cumw_&labl._5cty;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	people_count = CumFrequency;
run;

data cumw_&labl._6cty;
	keep DemographicType DemographicCategory County date_week people_count18;
	retain DemographicType DemographicCategory County date_week people_count18;
	set cumw_&labl._6cty;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	people_count18 = CumFrequency;
run;

data cumw_&labl._merged_cty;
	merge cumw_&labl._1cty cumw_&labl._2cty cumw_&labl._3cty cumw_&labl._4cty cumw_&labl._5cty cumw_&labl._6cty;
	by DemographicType DemographicCategory county date_week;
run;

proc sort data = cumw_&labl._merged_cty;
	by DemographicType DemographicCategory County date_week;
run;

data cumw_&labl._merged_cty;
	update cumw_&labl._merged_cty (obs = 0) cumw_&labl._merged_cty;
	by DemographicType DemographicCategory County;
	output;
run;

data cum_week_&labl._cty;
	keep DemographicType DemographicCategory County date_week UTD_population18 people_count people_count18;
	retain DemographicType DemographicCategory County date_week UTD_population18 people_count people_count18;
	set cumw_&labl._merged_cty;
	if people_count = . then do;
		people_count = 0;
	end;
	if people_count18 = . then do;
		people_count18 = 0;
	end;
	if vaccinated18 = . then do;
		vaccinated18 = 0;
	end;
	if boosted18 = . then do;
		boosted18 = 0;
	end;
	if boosted1849 = . then do;
		boosted1849 = 0;
	end;
	if boosted50 = . then do;
		boosted50 = 0;
	end;
	UTD_population18 = .;
	if date_week <= '30OCT21'd then UTD_population18 = vaccinated18;
	if date_week >= '31OCT21'd AND date_week <= '30APR22'd then UTD_population18 = boosted18;
	if date_week >= '01MAY22'd then UTD_population18 = boosted1849 + boosted50;
run;

*;
ods output OneWayFreqs = cumm_&labl._1cty;
proc freq data = tableau_imms_break;
	by &var county;
	where vaccinated = 1 and age >=18 and age ^=.;
	table date_month /nocol norow nopercent;
run;
ods output close;

ods output OneWayFreqs = cumm_&labl._2cty;
proc freq data = tableau_imms_break;
	by &var county;
	where booster_first = 1 and age >=18 and age ^=.;
	table date_month /nocol norow nopercent;
run;
ods output close;

ods output OneWayFreqs = cumm_&labl._3cty;
proc freq data = tableau_imms_break;
	by &var county;
	where booster_first = 1 and age >=18 and age <= 49;
	table date_month /nocol norow nopercent;
run;
ods output close;

ods output OneWayFreqs = cumm_&labl._4cty;
proc freq data = tableau_imms_break;
	by &var county;
	where booster_second = 1 and age >=50 and age ^=.;
	table date_month /nocol norow nopercent;
run;
ods output close;

ods output OneWayFreqs = cumm_&labl._5cty;
proc freq data = tableau_imms_break;
	by &var county;
	where people_count = 1;
	table date_month /nocol norow nopercent;
run;
ods output close;

ods output OneWayFreqs = cumm_&labl._6cty;
proc freq data = tableau_imms_break;
	by &var county;
	where people_count18 = 1;
	table date_month /nocol norow nopercent;
run;
ods output close; 


data cumm_&labl._1cty;
	keep DemographicType DemographicCategory County date_month vaccinated18;
	retain DemographicType DemographicCategory County date_month vaccinated18;
	set cumm_&labl._1cty;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	vaccinated18 = CumFrequency;
run;

data cumm_&labl._2cty;
	keep DemographicType DemographicCategory County date_month boosted18;
	retain DemographicType DemographicCategory County date_month boosted18;
	set cumm_&labl._2cty;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	boosted18 = CumFrequency;
run;

data cumm_&labl._3cty;
	keep DemographicType DemographicCategory County date_month boosted1849;
	retain DemographicType DemographicCategory County date_month boosted1849;
	set cumm_&labl._3cty;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	boosted1849 = CumFrequency;
run;

data cumm_&labl._4cty;
	keep DemographicType DemographicCategory County date_month boosted50;
	retain DemographicType DemographicCategory County date_month boosted50;
	set cumm_&labl._4cty;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	boosted50 = CumFrequency;
run;

data cumm_&labl._5cty;
	keep DemographicType DemographicCategory County date_month people_count;
	retain DemographicType DemographicCategory County date_month people_count;
	set cumm_&labl._5cty;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	people_count = CumFrequency;
run;

data cumm_&labl._6cty;
	keep DemographicType DemographicCategory County date_month people_count18;
	retain DemographicType DemographicCategory County date_month people_count18;
	set cumm_&labl._6cty;
	format county $10. DemographicType $20. DemographicCategory $50.;
	DemographicType = &type;
	DemographicCategory = &var;
	people_count18 = CumFrequency;
run;

data cumm_&labl._merged_cty;
	merge cumm_&labl._1cty cumm_&labl._2cty cumm_&labl._3cty cumm_&labl._4cty cumm_&labl._5cty cumm_&labl._6cty;
	by DemographicType DemographicCategory county date_month;
run;

proc sort data = cumm_&labl._merged_cty;
	by DemographicType DemographicCategory County date_month;
run;

data cumm_&labl._merged_cty;
	update cumm_&labl._merged_cty (obs = 0) cumm_&labl._merged_cty;
	by DemographicType DemographicCategory County;
	output;
run;

data cum_month_&labl._cty;
	keep DemographicType DemographicCategory County date_month UTD_population18 people_count people_count18;
	retain DemographicType DemographicCategory County date_month UTD_population18 people_count people_count18;
	set cumm_&labl._merged_cty;
	if people_count = . then do;
		people_count = 0;
	end;
	if people_count18 = . then do;
		people_count18 = 0;
	end;
	if vaccinated18 = . then do;
		vaccinated18 = 0;
	end;
	if boosted18 = . then do;
		boosted18 = 0;
	end;
	if boosted1849 = . then do;
		boosted1849 = 0;
	end;
	if boosted50 = . then do;
		boosted50 = 0;
	end;
	UTD_population18 = .;
	if date_month <= '30OCT21'd then UTD_population18 = vaccinated18;
	if date_month >= '31OCT21'd AND date_month <= '30APR22'd then UTD_population18 = boosted18;
	if date_month >= '01MAY22'd then UTD_population18 = boosted1849 + boosted50;
run;


%mend;

%cum_alert (var = all, type = "All Oregonians", labl = all);
%cum_alert (var = rarest_race, type = "Race", labl = rare);
%cum_alert (var = race, type = "Race OMB", labl = race);
%cum_alert (var = ethnicity, type = "Ethnicity OMB", labl = eth);
%cum_alert (var = race_ethnicity, type = "Race & Ethnicity OMB", labl = re);
%cum_alert (var = age_cat, type = "Age Groups", labl = age1);
%cum_alert (var = age_cat2, type = "Age Groups", labl = age2);
%cum_alert (var = age_65plus, type = "Age Groups", labl = age3);


/********************************************************************************************************************/
/********************************************************************************************************************/

data alert_week;
	set cum_week_all cum_week_rare cum_week_race cum_week_eth cum_week_re cum_week_age1 cum_week_age2 cum_week_age3;
run;

proc sort data = alert_week nodupkey;
	by DemographicType DemographicCategory County date_week;
run;

data alert_month;
	set cum_month_all cum_month_rare cum_month_race cum_month_eth cum_month_re cum_month_age1 cum_month_age2 cum_month_age3;
run;

proc sort data = alert_month nodupkey;
	by DemographicType DemographicCategory County date_month;
run;

data alert_week_cty;
	set cum_week_all_cty cum_week_rare_cty cum_week_race_cty cum_week_eth_cty cum_week_re_cty 
		cum_week_age1_cty cum_week_age2_cty cum_week_age3_cty;
run;

proc sort data = alert_week_cty nodupkey;
	by DemographicType DemographicCategory County date_week;
run;

data alert_month_cty;
	set cum_month_all_cty cum_month_rare_cty cum_month_race_cty cum_month_eth_cty cum_month_re_cty 
		cum_month_age1_cty cum_month_age2_cty cum_month_age3_cty;
run;

proc sort data = alert_month_cty nodupkey;
	by DemographicType DemographicCategory County date_month;
run;

data alert_week;
	set alert_week;
	rename date_week = date;
run;

data alert_week_cty;
	set alert_week_cty;
	rename date_week = date;
run;

data alert_month;
	set alert_month;
	rename date_month = date;
run;

data alert_month_cty;
	set alert_month_cty;
	rename date_month = date;
run;

/* Vax data aggregation is finished. Move onto merge case and vax data */
/********************************************************************************************************************/
/********************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/********************************************************************************************************************/
/********************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/********************************************************************************************************************/
/********************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/* Merge Case and Vax Data*/
proc sort data = BT_week_all;
	by DemographicType DemographicCategory date county;
run;

proc sort data = BT_week_all_cty;
	by DemographicType DemographicCategory date county;
run;

proc sort data = BT_month_all;
	by DemographicType DemographicCategory date county;
run;

proc sort data = BT_month_all_cty;
	by DemographicType DemographicCategory date county;
run;

proc sort data = alert_week;
	by DemographicType DemographicCategory date county;
run;

proc sort data = alert_month;
	by DemographicType DemographicCategory date county;
run;

proc sort data = alert_week_cty;
	by DemographicType DemographicCategory date county;
run;

proc sort data = alert_month_cty;
	by DemographicType DemographicCategory date county;
run;

data BT_alert_week;
	merge alert_week BT_week_all ;
	by DemographicType DemographicCategory date county;
run;

data BT_alert_month;
	merge alert_month BT_month_all;
	by DemographicType DemographicCategory date county;
run;

data BT_alert_week_cty;
	merge alert_week_cty BT_week_all_cty;
	by DemographicType DemographicCategory date county;
run;

data BT_alert_month_cty;
	merge alert_month_cty BT_month_all_cty;
	by DemographicType DemographicCategory date county;
run;

/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
data state_pop_break2;
	keep DemographicType DemographicCategory Population Population18 group;
	keep DemographicType DemographicCategory Population Population18 group;
	set pop.state_demog_PSU_2021;
run;

data pop_unknown;
	keep DemographicType DemographicCategory County;
	keep DemographicType DemographicCategory County;
	set pop.state_demog_PSU_2020;
	county = '';
run;

data pop_unknown2;
	keep DemographicType DemographicCategory County;
	keep DemographicType DemographicCategory County;
	set pop.state_demog_PSU_2021;
	county = '';
	DemographicCategory = 'Unknown';
run;

proc sort data = pop_unknown2 nodupkey;
	by DemographicType DemographicCategory County;
run;

data pop_unknown3;
	set pop_unknown2;
	drop county;
run;

data state_pop_break;
	keep DemographicType DemographicCategory Population Population18;
	keep DemographicType DemographicCategory Population Population18;
	set state_pop_break2 pop_unknown3;
	if DemographicType = 'All Oregonians' and DemographicCategory = 'Unknown' then delete;
	if DemographicType = 'Sex' then delete;
run;

proc sort data = state_pop_break;
	by DemographicType DemographicCategory;
run;

data state_pop_break;
	set state_pop_break;
	group + 1;
run;

/**/

data county_pop_break2;
	keep DemographicType DemographicCategory County Population Population18 group;
	keep DemographicType DemographicCategory County Population Population18 group;
	set pop.county_demog_PSU_2021;
run;

data pop_unknown4;
	keep DemographicType DemographicCategory County;
	keep DemographicType DemographicCategory County;
	set pop.county_demog_PSU_2021;
	DemographicCategory = 'Unknown';
run;

proc sort data = pop_unknown4 nodupkey;
	by DemographicType DemographicCategory County;
run;

data county_pop_break;
	keep DemographicType DemographicCategory County Population Population18;
	keep DemographicType DemographicCategory County Population Population18;
	set county_pop_break2 pop_unknown pop_unknown2 pop_unknown4;
	if DemographicType = 'All Oregonians' and DemographicCategory = 'Unknown' then delete;
	if DemographicType = 'Sex' then delete;
run;

proc sort data = county_pop_break;
	by DemographicType DemographicCategory County;
run;

data county_pop_break;
	set county_pop_break;
	group_old + 1;
	group = group_old + 46;
	drop group_old;
run;

/***/

proc sort data = BT_alert_week; 
by DemographicType DemographicCategory; 
run;

proc sort data = BT_alert_week_cty; 
by DemographicType DemographicCategory County; 
run;

proc sort data = BT_alert_month; 
by DemographicType DemographicCategory; 
run;

proc sort data = BT_alert_month_cty; 
by DemographicType DemographicCategory County; 
run;

data BT_alert_week_pop;
	merge BT_alert_week state_pop_break;
	by DemographicType DemographicCategory; 
run;

data BT_alert_week_cty_pop;
	merge BT_alert_week_cty County_pop_break;
	by DemographicType DemographicCategory County; 
run;

data BT_alert_month_pop;
	merge BT_alert_month state_pop_break;
	by DemographicType DemographicCategory; 
run;

data BT_alert_month_cty_pop;
	merge BT_alert_month_cty County_pop_break;
	by DemographicType DemographicCategory County; 
run;

/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/

/*
https://communities.sas.com/t5/SAS-Programming/Filling-up-missing-rows-with-previous-rows-value/td-p/530048
*/

data BT_table_week2;
	set BT_alert_week_pop BT_alert_week_cty_pop;
	if county = '' then do;
		county = 'Unknown';
	end;
	if Cases =  . Then do; Cases = 0; end;
	if Cases18 =  . Then do; Cases18 = 0; end;
	if unknown =  . Then do; unknown = 0; end;
	if unknown18 =  . Then do; unknown18 = 0; end;
	if Breakth =  . Then do; Breakth = 0; end;
	if Breakth18 =  . Then do; Breakth18 = 0; end;
	if Breakth_not =  . Then do; Breakth_not = 0; end;
	if Breakth_not18 =  . Then do; Breakth_not18 = 0; end;
	if UTD_Breakth_Num =  . Then do; UTD_Breakth_Num = 0; end;
	if UTD_Breakth_Num18 =  . Then do; UTD_Breakth_Num18 = 0; end;
run;

proc sort data = BT_table_week2;
	by group Date;
run;

data BT_table_week2;
	set BT_table_week2;
	count + 1; 
	by group;
	if first.group then count = 1;
	/*
	if count = 1 and UTD_Population = . then do;
		UTD_Population = 0;
	end;
	*/
	if count = 1 and UTD_Population18 = . then do;
		UTD_Population18 = 0;
	end;
	if count = 1 and people_count = . then do;
		people_count = 0;
	end;
	if count = 1 and people_count18 = . then do;
		people_count18 = 0;
	end;

run;

data BT_table_week1;
	update BT_table_week2(obs = 0) BT_table_week2;
	by group;
	output;
run;

/**/

data BT_table_month2;
	set BT_alert_month_pop BT_alert_month_cty_pop;
	if county = '' then do;
		county = 'Unknown';
	end;
	if Breakth =  . Then do; Breakth = 0; end;
	if Breakth18 =  . Then do; Breakth18 = 0; end;
	if Breakth_not =  . Then do; Breakth_not = 0; end;
	if Breakth_not18 =  . Then do; Breakth_not18 = 0; end;
	if UTD_Breakth_Num =  . Then do; UTD_Breakth_Num = 0; end;
	if UTD_Breakth_Num18 =  . Then do; UTD_Breakth_Num18 = 0; end;
	if Hospitalized =  . Then do; Hospitalized = 0; end;
	if Hospitalized18 =  . Then do; Hospitalized18 = 0; end;
	if died =  . Then do; died = 0; end;
	if Died18 =  . Then do; Died18 = 0; end;
	if Hosp_Breakth =  . Then do; Hosp_Breakth = 0; end;
	if Hosp_Breakth18 =  . Then do; Hosp_Breakth18 = 0; end;
	if Hosp_Breakth_not =  . Then do; Hosp_Breakth_not = 0; end;
	if Hosp_Breakth_not18 =  . Then do; Hosp_Breakth_not18 = 0; end;
	if Hosp_UTDB =  . Then do; Hosp_UTDB = 0; end;
	if Hosp_UTDB18 =  . Then do; Hosp_UTDB18 = 0; end;
	if Died_Breakth =  . Then do; Died_Breakth = 0; end;
	if Died_Breakth18 =  . Then do; Died_Breakth18 = 0; end;
	if Died_Breakth_not =  . Then do; Died_Breakth_not = 0; end;
	if Died_Breakth_not18 =  . Then do; Died_Breakth_not18 = 0; end;
	if Died_UTDB =  . Then do; Died_UTDB = 0; end;
	if Died_UTDB18 =  . Then do; Died_UTDB18 = 0; end;
run;

proc sort data = BT_table_month2;
	by group Date;
run;

data BT_table_month2;
	set BT_table_month2;
	count + 1; 
	by group;
	if first.group then count = 1;
	/*
	if count = 1 and UTD_Population = . then do;
		UTD_Population = 0;
	end;
	*/
	if count = 1 and UTD_Population18 = . then do;
		UTD_Population18 = 0;
	end;
	if count = 1 and people_count = . then do;
		people_count = 0;
	end;
	if count = 1 and people_count18 = . then do;
		people_count18 = 0;
	end;
run;

data BT_table_month1;
	update BT_table_month2(obs = 0) BT_table_month2;
	by group;
	output;
run;


/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/*
Cases
Cases18
unknown
unknown18
Breakth
Breakth18
Breakth_not
Breakth_not18
Booster_Breakth
Booster_Breakth18
Primary_Breakth
Primary_Breakth18

Breakth
Breakth18
Hospitalized
Hospitalized18
died
Died18
Hosp_Breakth
Hosp_Breakth18
Hosp_Breakth_not
Hosp_Breakth_not18
Hosp_BoostB
Hosp_BoostB18
Hosp_PrimB
Hosp_PrimB18
Died_Breakth
Died_Breakth18
Died_Breakth_not
Died_Breakth_not18
Died_BoostB
Died_BoostB18
Died_PrimB
Died_PrimB18
*/

data BT_table_week0;
	keep DemographicType DemographicCategory New_County Date
				cases breakth_not unvaccinated cases_unvaccinated
				cases18 breakth_not18 unvaccinated18 cases_unvaccinated18
				UTD_Breakth_Num UTD_Population cases_UTD 
				UTD_Breakth_Num18 UTD_Population18 cases_UTD18 cases_UTD18a cases_UTD18b cases_UTD18c
				group count;
	retain DemographicType DemographicCategory New_County Date
				cases breakth_not unvaccinated cases_unvaccinated
				cases18 breakth_not18 unvaccinated18 cases_unvaccinated18
				UTD_Breakth_Num UTD_Population cases_UTD 
				UTD_Breakth_Num18 UTD_Population18 cases_UTD18 cases_UTD18a cases_UTD18b 
				group count;
	set BT_table_week1;
	if DemographicType = 'Age Groups' then do;
		unvaccinated = population - people_count;
		unvaccinated18 = population - people_count;

		cases_unvaccinated = breakth_not/unvaccinated*100000;
		cases_unvaccinated18 = breakth_not/unvaccinated*100000;
		cases_UTD = UTD_Breakth_Num/UTD_Population*100000;
		cases_UTD18 = UTD_Breakth_Num/UTD_Population*100000;
	end;
	if DemographicType ^= 'Age Groups' then do;
		unvaccinated = population - people_count;
		unvaccinated18 = population18 - people_count18;

		cases_unvaccinated = breakth_not/unvaccinated*100000;
		cases_unvaccinated18 = breakth_not18/unvaccinated18*100000;
		cases_UTD = UTD_Breakth_Num/UTD_Population*100000;
		cases_UTD18 = UTD_Breakth_Num18/UTD_Population18*100000;
	end;
	if unvaccinated = . then do;
		unvaccinated = 0;
	end;
	if unvaccinated18 = . then do;
		unvaccinated18 = 0;
	end;
	if cases_unvaccinated = . then do;
		cases_unvaccinated = 0;
	end;
	if cases_unvaccinated18 = . then do;
		cases_unvaccinated18 = 0;
	end;
	if cases_UTD = . then do;
		cases_UTD = 0;
	end;
	if cases_UTD18 = . then do;
		cases_UTD18 = 0;
	end;
	cases_UTD18a = .;
	cases_UTD18b = .;
	cases_UTD18c = .;
	if date <= '31OCT21'd then cases_UTD18a = cases_UTD18;
	if date >= '30OCT21'd and date <='24APR22'd then cases_UTD18b = cases_UTD18;
	if date >= '24APR22'd then cases_UTD18c = cases_UTD18;
	if county ^= 'Oregon' then do; *add suffix to county for interpretations;
		New_county = strip(county) || " County";
	end;
	if county = 'Oregon' then do;
		New_county = strip(county);
	end;
	drop county;
	rename new_county = county; 
run;

data BT_table_month0;
	keep DemographicType DemographicCategory New_County Date
				Breakth Breakth18 
				Breakth_not Breakth_not18 
				UTD_Breakth_Num UTD_Breakth_Num18

 				Hosp_Breakth Hospitalized 
				Hosp_Breakth18 Hospitalized18 
				hosp_breakth_not unvaccinated hosp_unvaccinated
				Hosp_UTDB UTD_Population hosp_UTD
				hosp_breakth_not18 unvaccinated18 hosp_unvaccinated18
				Hosp_UTDB18 UTD_Population18 hosp_UTD18 hosp_UTD18a hosp_UTD18b hosp_UTD18c

				Died_Breakth died
				Died_Breakth18 Died18
				Died_Breakth_not unvaccinated died_unvaccinated
				Died_UTDB UTD_Population died_UTD
				Died_Breakth_not18 unvaccinated18 died_unvaccinated18
				Died_UTDB18 UTD_Population18 died_UTD18 died_UTD18a died_UTD18b died_UTD18c
				group count;
	retain DemographicType DemographicCategory New_County Date
				Breakth Breakth18 
				Breakth_not Breakth_not18 
				UTD_Breakth_Num UTD_Breakth_Num18

 				Hosp_Breakth Hospitalized 
				Hosp_Breakth18 Hospitalized18 
				hosp_breakth_not unvaccinated hosp_unvaccinated
				Hosp_UTDB UTD_Population hosp_UTD
				hosp_breakth_not18 unvaccinated18 hosp_unvaccinated18
				Hosp_UTDB18 UTD_Population18 hosp_UTD18 hosp_UTD18a hosp_UTD18b hosp_UTD18c

				Died_Breakth died
				Died_Breakth18 Died18
				Died_Breakth_not unvaccinated died_unvaccinated
				Died_UTDB UTD_Population died_UTD
				Died_Breakth_not18 unvaccinated18 died_unvaccinated18
				Died_UTDB18 UTD_Population18 died_UTD18 died_UTD18a died_UTD18b died_UTD18c
				group count;
	set BT_table_month1;
	if DemographicType = 'Age Groups' then do;
		unvaccinated = population - people_count;
		unvaccinated18 = population - people_count;
		hosp_unvaccinated = hosp_breakth_not/unvaccinated*100000;
		hosp_unvaccinated18 = hosp_breakth_not/unvaccinated*100000;
		hosp_UTD = Hosp_UTDB/UTD_Population*100000;
		hosp_UTD18 = Hosp_UTDB/UTD_Population*100000;
		died_unvaccinated = Died_Breakth_not/unvaccinated*100000;
		died_unvaccinated18 = Died_Breakth_not/unvaccinated*100000;
		died_UTD = Died_UTDB/UTD_Population*100000;
		died_UTD18 = Died_UTDB/UTD_Population*100000;
	end;
	if DemographicType ^= 'Age Groups' then do;
		unvaccinated = population - people_count;
		unvaccinated18 = population18 - people_count18;
		hosp_unvaccinated = hosp_breakth_not/unvaccinated*100000;
		hosp_unvaccinated18 = hosp_breakth_not18/unvaccinated18*100000;
		hosp_UTD = Hosp_UTDB/UTD_Population*100000;
		hosp_UTD18 = Hosp_UTDB18/UTD_Population18*100000;
		died_unvaccinated = Died_Breakth_not/unvaccinated*100000;
		died_unvaccinated18 = Died_Breakth_not18/unvaccinated18*100000;
		died_UTD = Died_UTDB/UTD_Population*100000;
		died_UTD18 = Died_UTDB18/UTD_Population18*100000;
	end;
	hosp_UTD18a = .;
	hosp_UTD18b = .;
	hosp_UTD18c = .;
	if date <= '01OCT21'd then hosp_UTD18a = hosp_UTD18;
	if date >= '01OCT21'd AND date <= '01MAY22'd then hosp_UTD18b = hosp_UTD18;
	if date >= '01MAY22'd then hosp_UTD18c = hosp_UTD18;

	died_UTD18a = .;
	died_UTD18b = .;
	died_UTD18c = .;
	if date <= '01OCT21'd then died_UTD18a = died_UTD18;
	if date >= '01OCT21'd AND date <= '01MAY22'd then died_UTD18b = died_UTD18;
	if date >= '01MAY22'd then died_UTD18c = died_UTD18;
	if county ^= 'Oregon' then do; *add suffix to county for interpretations;
		New_county = strip(county) || " County";
	end;
	if county = 'Oregon' then do;
		New_county = strip(county);
	end;
	drop county;
	rename new_county = county; 
run;

/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/* Suppress monthly data if there are too few observations - to prevent disclosing vaccination status of individuals */

proc sql;
	create table BT_month_suppress as
		select DemographicType,
				DemographicCategory,
				County,
				sum(died18) as died_suppress
	from BT_table_month0
	group by DemographicType, DemographicCategory, County;
quit;

data BT_month_suppress;
	set BT_month_suppress;
		if died_suppress < 50 then action = 'Suppress';
		else action = 'Report';
run;

proc sort data = BT_table_month0;
by DemographicType DemographicCategory County;
run;

proc sort data = BT_month_suppress;
by DemographicType DemographicCategory County;
run;

data BT_table_month0_suppress;
	merge BT_table_month0 BT_month_suppress;
	by DemographicType DemographicCategory County;
run;

/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/

data breakthrough_week;
	keep DemographicType DemographicCategory County Date
				cases18 breakth_not18 unvaccinated18 cases_unvaccinated18
				UTD_Breakth_Num18 UTD_Population18 cases_UTD18 cases_UTD18a cases_UTD18b cases_UTD18c
				delta_times omicron_times delta_int omicron_int
				todays_date week_end;
	retain DemographicType DemographicCategory County Date
				cases18 breakth_not18 unvaccinated18 cases_unvaccinated18
				UTD_Breakth_Num18 UTD_Population18 cases_UTD18 cases_UTD18a cases_UTD18b cases_UTD18c
				delta_times omicron_times delta_int omicron_int
				todays_date week_end;
	set BT_table_week0;
	if date >= (&break_date2 - 11) then delete;
	if date < '31JAN21'd then delete;
	format todays_date week_end mmddyy10.;
	todays_date = &break_date2;
	week_end = date + 6;
	/*times variables for title*/
	delta_times = cases_unvaccinated18/cases_UTD18;
	if cases_UTD18 = 0 then delta_times = 0;
	if date ^= '29AUG21'd then delta_times = .;
	omicron_times = cases_unvaccinated18/cases_UTD18;
	if cases_UTD18 = 0 then omicron_times = 0;
	if date ^= '16JAN22'd then omicron_times = .;
		if delta_times = 0 then delta_int = 
			"At the peak of the Delta variant surge in August 2021, there were "|| strip(put(cases_unvaccinated18, comma10.0))|| 
			" cases per 100,000 unvaccinated adults while there were "||strip(put(cases_UTD18, comma10.0))||
			" cases per 100,000 adults who were vaccinated as recommended in "|| strip(county)||".";
		if delta_times > 0 AND delta_times ^= . then delta_int = 
			"At the peak of the Delta variant surge in August 2021, cases in unvaccinated adults were approximately "|| strip(round(delta_times,1)) ||
			" times cases in adults who were vaccinated as recommended in "|| strip(county)||".";
		if delta_times = . then delta_int = "";
		if omicron_times = 0 then omicron_int = 
			"At the peak of the Omicron variant surge in January 2022, there were "|| strip(put(cases_unvaccinated18, comma10.0))|| 
			" cases per 100,000 unvaccinated adults while there were "||strip(put(cases_UTD18, comma10.0))||
			" cases per 100,000 adults who were vaccinated as recommended in "|| strip(county)||".";
		if omicron_times > 0 AND omicron_times ^= . then omicron_int = 
			"At the peak of the Omicron variant surge in January 2022, cases in unvaccinated adults were approximately "|| strip(round(omicron_times,1)) ||
			" times cases in adults who were vaccinated as recommended in "|| strip(county)||".";
		if omicron_times = . then omicron_int = "";
run;


/****/
proc sql;
	create table max_date as
		select max(week_end) as data_date
		from breakthrough_week;
quit;

proc sql noprint;
   select data_date
      into :data_date
      from max_date;
quit;

%put &data_date;
/****/

data breakthrough_week;
	set breakthrough_week;
	format data_date mmddyy10.;
	data_date = &data_date;
	if county = 'Unknown County' then delete;
	if DemographicType = 'All Oregonians' then output;
run;

data breakthrough_month;
	keep DemographicType DemographicCategory County Date
				Breakth18 Breakth_not18 UTD_Breakth_Num18

				Hosp_Breakth18 Hospitalized18 
				hosp_breakth_not18 unvaccinated18 hosp_unvaccinated18
				Hosp_UTDB18 UTD_Population18 hosp_UTD18 hosp_UTD18a hosp_UTD18b hosp_UTD18c

				Died_Breakth18 Died18
				Died_Breakth_not18 unvaccinated18 died_unvaccinated18
				Died_UTDB18 UTD_Population18 died_UTD18 died_UTD18a died_UTD18b died_UTD18c
				hosp_delta_times hosp_omicron_times hosp_delta_int hosp_omicron_int
				died_delta_times died_omicron_times died_delta_int died_omicron_int
				todays_date data_date;
	retain DemographicType DemographicCategory County Date
				Breakth18 Breakth_not18 UTD_Breakth_Num18

				Hosp_Breakth18 Hospitalized18 
				hosp_breakth_not18 unvaccinated18 hosp_unvaccinated18
				Hosp_UTDB18 UTD_Population18 hosp_UTD18 hosp_UTD18a hosp_UTD18b hosp_UTD18c

				Died_Breakth18 Died18
				Died_Breakth_not18 unvaccinated18 died_unvaccinated18
				Died_UTDB18 UTD_Population18 died_UTD18 died_UTD18a died_UTD18b died_UTD18c
				hosp_delta_times hosp_omicron_times hosp_delta_int hosp_omicron_int
				died_delta_times died_omicron_times died_delta_int died_omicron_int
				todays_date data_date;
	set BT_table_month0_suppress;
	if (month(date) >= (month(&today)-1)) AND (year(date) = year(&today)) then delete;
	if date < '31JAN21'd then delete;
	format todays_date data_date mmddyy10.;
	todays_date = &break_date2;
	/*times variables for title*/
	hosp_delta_times = hosp_unvaccinated18/hosp_UTD18;
	died_delta_times = died_unvaccinated18/died_UTD18;
	if hosp_UTD18 = 0 then hosp_delta_times = 0;
	if died_UTD18 = 0 then died_delta_times = 0;
	if date ^= '01AUG21'd then hosp_delta_times = .;
	if date ^= '01SEP21'd then died_delta_times = .;
	hosp_omicron_times = hosp_unvaccinated18/hosp_UTD18;
	died_omicron_times = died_unvaccinated18/died_UTD18;
	if hosp_UTD18 = 0 then hosp_omicron_times = 0;
	if died_UTD18 = 0 then died_omicron_times = 0;
	if date ^= '01JAN22'd then hosp_omicron_times = .;
	if date ^= '01JAN22'd then died_omicron_times = .;
		if hosp_delta_times = 0 then hosp_delta_int = 
			"At the peak of the Delta variant surge in August 2021, there were "|| strip(put(hosp_unvaccinated18, comma10.0))|| 
			" hospitalizations per 100,000 unvaccinated adults while there were "||strip(put(hosp_UTD18, comma10.0))||
			" hospitalizations per 100,000 adults who were vaccinated as recommended in "|| strip(county)||".";
		if hosp_delta_times > 0 AND hosp_delta_times ^= . then hosp_delta_int = 
			"At the peak of the Delta variant surge in August 2021, hospitalizations in unvaccinated adults were approximately "|| strip(round(hosp_delta_times,1)) ||
			" times hospitalizations in adults who were vaccinated as recommended in "|| strip(county)||".";
		if hosp_delta_times = . then hosp_delta_int = "";
		if hosp_omicron_times = 0 then hosp_omicron_int = 
			"At the peak of the Omicron variant surge in January 2022, there were "|| strip(put(hosp_unvaccinated18, comma10.0))|| 
			" hospitalizations per 100,000 unvaccinated adults while there were "||strip(put(hosp_UTD18, comma10.0))||
			" hospitalizations per 100,000 adults who were vaccinated as recommended in "|| strip(county)||".";
		if hosp_omicron_times > 0 AND hosp_omicron_times ^= . then hosp_omicron_int = 
			"At the peak of the Omicron variant surge in January 2022, hospitalizations in unvaccinated adults were approximately "|| strip(round(hosp_omicron_times,1)) ||
			" times hospitalizations in adults who were vaccinated as recommended in "|| strip(county)||".";
		if hosp_omicron_times = . then hosp_omicron_int = "";

		if died_delta_times = 0 then died_delta_int = 
			"At the peak of the Delta variant surge in September 2021, there were "|| strip(put(died_unvaccinated18, comma10.0))|| 
			" deaths per 100,000 unvaccinated adults while there were "||strip(put(died_UTD18, comma10.0))||
			" deaths per 100,000 adults who were vaccinated as recommended in "|| strip(county)||".";
		if died_delta_times > 0 AND died_delta_times ^= . then died_delta_int = 
			"At the peak of the Delta variant surge in September 2021, deaths in unvaccinated adults were approximately "|| strip(round(died_delta_times,1)) ||
			" times deaths in adults who were vaccinated as recommended in "|| strip(county)||".";
		if died_delta_times = . then died_delta_int = "";
		if died_omicron_times = 0 then died_omicron_int = 
			"At the peak of the Omicron variant surge in January 2022, there were "|| strip(put(died_unvaccinated18, comma10.0))|| 
			" deaths per 100,000 unvaccinated adults while there were "||strip(put(died_UTD18, comma10.0))||
			" deaths per 100,000 adults who were vaccinated as recommended in "|| strip(county)||".";
		if died_omicron_times > 0 AND died_omicron_times ^= . then died_omicron_int = 
			"At the peak of the Omicron variant surge in January 2022, deaths in unvaccinated adults were approximately "|| strip(round(died_omicron_times,1)) ||
			" times deaths in adults who were vaccinated as recommended in "|| strip(county)||".";
		if died_omicron_times = . then died_omicron_int = "";

	if action = 'Suppress' then delete;
	if county = 'Unknown County' then delete;
	if DemographicType = 'All Oregonians' then output;
run;

/****/
proc sql;
	create table max_date2 as
		select max(date) as max_date
		from breakthrough_month;
quit;

proc sql noprint;
   select max_date
      into :max_date
      from max_date2;
quit;

%put &max_date;

/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/*Make a table for interpretations to join in Tableau.*/
/*Delta and Omicron Peak interpretations */

data delta_case_int;
	keep DemographicType DemographicCategory County delta_int;
	set breakthrough_week;
	if delta_int = '' then delete;
run;

proc sort data = delta_case_int;
by DemographicType DemographicCategory County;
run;

data omicron_case_int;
	keep DemographicType DemographicCategory County omicron_int;
	set breakthrough_week;
	if omicron_int = '' then delete;
run;

proc sort data = omicron_case_int;
by DemographicType DemographicCategory County;
run;

data delta_hosp_int;
	keep DemographicType DemographicCategory County hosp_delta_int;
	set breakthrough_month;
	if hosp_delta_int = '' then delete;
run;

proc sort data = delta_hosp_int;
by DemographicType DemographicCategory County;
run;

data delta_died_int;
	keep DemographicType DemographicCategory County died_delta_int;
	set breakthrough_month;
	if died_delta_int = '' then delete;
run;

proc sort data = delta_died_int;
by DemographicType DemographicCategory County;
run;

data omicron_hosp_int;
	keep DemographicType DemographicCategory County hosp_omicron_int;
	set breakthrough_month;
	if hosp_omicron_int = '' then delete;
run;

proc sort data = omicron_hosp_int;
by DemographicType DemographicCategory County;
run;

data omicron_died_int;
	keep DemographicType DemographicCategory County died_omicron_int;
	set breakthrough_month;
	if died_omicron_int = '' then delete;
run;

proc sort data = omicron_died_int;
by DemographicType DemographicCategory County;
run;

/*Cumulative Case, Hosp, and Death Interpretations*/

ods output table = cumulative_case;
proc tabulate data = breakthrough_week;
	format data_date mmddyy10.;
	var cases18 breakth_not18 UTD_breakth_num18 data_date;
  	class County;
   	table County, cases18*sum breakth_not18*sum UTD_breakth_num18*sum data_date*mean;
run;
ods output close;

data cumulative_case;
	keep DemographicType DemographicCategory County cum_case_int;
	retain DemographicType DemographicCategory County cum_case_int;
	set cumulative_case;
	format DemographicType $20. DemographicCategory $50.;
	format day day. month monname. year year. cases18 comma10.0 vax_adult_total unvax_adult_total percent10.1; 
	DemographicType = 'All Oregonians';
	DemographicCategory = 'All Oregonians';
	day = data_date_mean;
	month = data_date_mean;
	year = data_date_mean;
	cases18 = cases18_sum;
	vax_adult_total = UTD_breakth_num18_sum/cases18_sum;
	unvax_adult_total = breakth_not18_sum/cases18_sum;
	cum_case_int = "Between February 1, 2021 through the week of " || strip(put(month, monname.)) || " " 
	|| strip(put(day, day.)) ||", " || strip(put(year, year.)) || " there were " || strip(put(cases18, comma10.0)) || 
	" total COVID-19 cases in adults. Of these, " || strip(put(unvax_adult_total, percent10.1)) || 
	" were in unvaccinated adults, while " || strip(put(vax_adult_total, percent10.1)) || 
	" were in adults who were vaccinated as recommended.";
run;

proc sort data = cumulative_case;
by DemographicType DemographicCategory County;
run;

ods output table = cumulative_hosp;
proc tabulate data = breakthrough_month;
	format date mmddyy10.;
	var hospitalized18 hosp_breakth_not18 hosp_UTDB18 date;
  	class County;
   	table County, hospitalized18*sum hosp_breakth_not18*sum hosp_UTDB18*sum date*max;
run;
ods output close;

data cumulative_hosp;
	keep DemographicType DemographicCategory County cum_hosp_int;
	retain DemographicType DemographicCategory County cum_hosp_int;
	set cumulative_hosp;
	format DemographicType $20. DemographicCategory $50.;
	format month monname. year year. hospitalized18 comma10.0 hosp_vax_adult_total hosp_unvax_adult_total percent10.1; 
	DemographicType = 'All Oregonians';
	DemographicCategory = 'All Oregonians';
	month = date_max;
	year = date_max;
	hospitalized18 = hospitalized18_sum;
	hosp_vax_adult_total = hosp_UTDB18_sum/hospitalized18_sum;
	hosp_unvax_adult_total = hosp_breakth_not18_sum/hospitalized18_sum;
	cum_hosp_int = "Between February 1, 2021 through the end of " || strip(put(month, monname.)) || 
	" " || strip(put(year, year.)) ||
	" there were " || strip(put(hospitalized18, comma10.0)) || 
	" total COVID-19-associated hospitalizations in adults. Of these, " || strip(put(hosp_unvax_adult_total, percent10.1)) || 
	" were in unvaccinated adults, while " || strip(put(hosp_vax_adult_total, percent10.1)) || 
	" were in adults who were vaccinated as recommended.";
run;

proc sort data = cumulative_hosp;
by DemographicType DemographicCategory County;
run;

ods output table = cumulative_died;
proc tabulate data = breakthrough_month;
	format date mmddyy10.;
	var died18 died_breakth_not18 died_UTDB18 date;
  	class County;
   	table County, died18*sum died_breakth_not18*sum died_UTDB18*sum date*max;
run;
ods output close;

data cumulative_died;
	keep DemographicType DemographicCategory County cum_died_int;
	retain DemographicType DemographicCategory County cum_died_int;
	set cumulative_died;
	format DemographicType $20. DemographicCategory $50.;
	format month monname. year year. died18 comma10.0 died_vax_adult_total died_unvax_adult_total percent10.1; 
	DemographicType = 'All Oregonians';
	DemographicCategory = 'All Oregonians';
	month = date_max;
	year = date_max;
	died18 = died18_sum;
	died_vax_adult_total = died_UTDB18_sum/died18_sum;
	died_unvax_adult_total = died_breakth_not18_sum/died18_sum;
	cum_died_int = "Between February 1, 2021 through the end of " || strip(put(month, monname.)) || 
	" " || strip(put(year, year.)) ||
	" there were " || strip(put(died18, comma10.0)) || 
	" total deaths with COVID-19 in adults. Of these, " || strip(put(died_unvax_adult_total, percent10.1)) || 
	" were in unvaccinated adults, while " || strip(put(died_vax_adult_total, percent10.1)) || 
	" were in adults who were vaccinated as recommended.";
run;

proc sort data = cumulative_died;
by DemographicType DemographicCategory County;
run;
/*Merge everything together into one table*/

data breakthrough_interpretations;
	merge delta_case_int delta_hosp_int delta_died_int omicron_case_int omicron_hosp_int omicron_died_int 
			cumulative_case cumulative_hosp cumulative_died;
	by DemographicType DemographicCategory County;
	rename delta_int = case_delta_int omicron_int = case_omicron_int;
	if hosp_delta_int = " " then do;
		hosp_delta_int = "Data not available due to small numbers in "|| strip(county)||".";
		cum_hosp_int = "Data not available due to small numbers in "|| strip(county)||".";
	end;
	if died_delta_int = " " then do;
		died_delta_int = "Data not available due to small numbers in "|| strip(county)||".";
		cum_died_int = "Data not available due to small numbers in "|| strip(county)||".";
	end;
	if hosp_omicron_int = " " then do;
		hosp_omicron_int = "    ";
	end;
	if died_omicron_int = " " then do;
		died_omicron_int = "    ";
	end;
	format month_max_date mmddyy10.;
	month_max_date = &max_date;
run;

/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
proc sort data = breakthrough_week;
	by DemographicType DemographicCategory County;
run;

proc sort data = breakthrough_month;
	by DemographicType DemographicCategory County;
run;

proc sort data = breakthrough_interpretations;
	by DemographicType DemographicCategory County;
run;

data break.breakthrough_week;
	merge breakthrough_week breakthrough_interpretations;
	by DemographicType DemographicCategory County;
	drop delta_int omicron_int;
run;

data break.breakthrough_month;
	set breakthrough_month;
	drop hosp_delta_int hosp_omicron_int died_delta_int died_omicron_int;
run;
/*
data break.breakthrough_month;
	merge breakthrough_month breakthrough_interpretations;
	by DemographicType DemographicCategory County;
	*drop hosp_delta_int hosp_omicron_int died_delta_int died_omicron_int;
run;
*/
/*
proc sort data = breakthrough_week;
	by DemographicType DemographicCategory County date;
run;

proc sort data = breakthrough_month;
	by DemographicType DemographicCategory County date;
run;

proc sort data = breakthrough_interpretations;
	by DemographicType DemographicCategory County;
run;

data break.breakthrough_all;
	merge breakthrough_week breakthrough_month;
	by DemographicType DemographicCategory County date;
	drop delta_int omicron_int;
	drop hosp_delta_int hosp_omicron_int died_delta_int died_omicron_int;
run;

data break.breakthrough_all;
	merge break.breakthrough_all breakthrough_interpretations;
	by DemographicType DemographicCategory County;
run;
*/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/

/*
dm 'odsresults; clear';
proc print data = break.breakthrough_week;
	where DemographicCategory in ('All Oregonians', 'Black') and county in ('Oregon', 'Clackamas');
	var DemographicCategory County Date 
				booster_breakth18 booster18 cases_boosted18
				booster_breakth booster cases_boosted;
run;
*/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/***************************************************************************************************************/
/*Output a Validation File */

proc print data = break.breakthrough_week;
where county = 'Oregon' and DemographicType = 'All Oregonians';
var date cases18 breakth_not18 UTD_Breakth_Num18 unvaccinated18 utd_population18 cases_unvaccinated18 cases_UTD18;
run;

proc print data = break.breakthrough_month;
where county = 'Oregon' and DemographicType = 'All Oregonians';
var date hospitalized18 hosp_breakth_not18 hosp_UTDB18 unvaccinated18 utd_population18 hosp_unvaccinated18 hosp_UTD18;
run;

proc print data = break.breakthrough_month;
where county = 'Oregon' and DemographicType = 'All Oregonians';
var date died18 died_breakth_not18 died_UTDB18 unvaccinated18 utd_population18 died_unvaccinated18 died_UTD18;
run;



data breakthrough_cases_valid;
	set breakthrough_cases;
	case = 1;
	TCD = datepart(TrueCaseDate);
	date_week = intnx('week',TCD,0,'b');
	date_month = intnx('month',TCD,0,'b');
	format date_week date_month mmddyy10.;
run;

**********;
proc sql;
	create table valid_people_count_jan as
		select max(admin_date) as max_date,
				sum(people_count) as people_count18
	from data.tableau_imms_boost_utd_&break_date
	where admin_date < ('07FEB21'd-14) and age >= 18;
quit;

proc sql;
	create table valid_people_count_now as
		select 	max(admin_date) as max_date,
				sum(people_count) as people_count18
	from data.tableau_imms_boost_utd_&break_date
	where admin_date < (&break_date2 - 8 - 14) and age >= 18;
quit;

data valid_people_count;
	keep date DemographicType people_count18;
	retain date DemographicType people_count18;
	set valid_people_count_jan valid_people_count_now;
	format date mmddyy10.;
	date = max_date + 14 - 6;
	DemographicType = 'All Oregonians';
run;

data valid_pop;
	keep DemographicType population18;
	set state_pop_break2;
	if DemographicType ^= 'All Oregonians' then delete;
run;

data valid_unvax;
	merge valid_people_count valid_pop ;
	by DemographicType;
	unvaccinated_adults = population18 - people_count18;
run;


*******************;
proc sql;
	create table valid_vax1 as
		select max(admin_date) as max_date,
				sum(vaccinated) as vaccinated18
	from data.tableau_imms_boost_utd_&break_date
	where admin_date < ('07FEB21'd-14) and age >= 18;
quit;

proc sql;
	create table valid_vax2 as
		select max(admin_date) as max_date,
				sum(vaccinated) as vaccinated18
	from data.tableau_imms_boost_utd_&break_date
	where admin_date < ('31OCT21'd-14) and age >= 18;
quit;

proc sql;
	create table valid_vax3 as
		select max(admin_date) as max_date,
				sum(booster) as vaccinated18
	from data.tableau_imms_boost_utd_&break_date
	where admin_date < ('07NOV21'd-14) and age >= 18;
quit;

proc sql;
	create table valid_vax4 as
		select max(admin_date) as max_date,
				sum(booster) as vaccinated18
	from data.tableau_imms_boost_utd_&break_date
	where admin_date < ('01MAY22'd-14) and age >= 18;
quit;

proc sql;
	create table valid_vax5 as
		select max(admin_date) as max_date,
				sum(booster) as vaccinated18
	from data.tableau_imms_boost_utd_&break_date
	where admin_date < ('08MAY22'd-14) and age >= 18 AND age <=49;
quit;

proc sql;
	create table valid_vax6 as
		select max(admin_date) as max_date,
				sum(booster_second) as vaccinated18
	from data.tableau_imms_boost_utd_&break_date
	where admin_date < ('08MAY22'd-14) AND age >= 50;
quit;

proc sql;
	create table valid_vax7 as
		select max(admin_date) as max_date,
				sum(booster) as vaccinated18
	from data.tableau_imms_boost_utd_&break_date
	where admin_date < (&break_date2 - 8 - 14) AND age >= 18 and age <= 49;
quit;

proc sql;
	create table valid_vax8 as
		select max(admin_date) as max_date,
				sum(booster_second) as vaccinated18
	from data.tableau_imms_boost_utd_&break_date
	where admin_date < (&break_date2 - 8 - 14) AND age >= 50;
quit;

data valid_vaccinated;
	keep date  vaccinated18;
	retain date  vaccinated18;
	set valid_vax1 valid_vax2 valid_vax3 valid_vax4 valid_vax5 valid_vax6 valid_vax7 valid_vax8;
	format date mmddyy10.;
	date = max_date + 14 - 6;
run;

proc sql;
	create table valid_vaccinated2 as
		select date,
			sum(vaccinated18) as vaccinated18
		from valid_vaccinated
		group by date;
quit;

***********;
proc sql;
	create table valid_people_count_feb as
		select max(admin_date) as max_date,
				sum(people_count) as people_count18
	from data.tableau_imms_boost_utd_&break_date
	where admin_date < ('01MAR21'd-14) and age >= 18;
quit;

proc sql;
	create table valid_people_count_jun as
		select 	max(admin_date) as max_date,
				sum(people_count) as people_count18
	from data.tableau_imms_boost_utd_&break_date
	where admin_date < ('01JUL22'd-14) and age >= 18;
quit;

data valid_people_count2;
	keep date DemographicType people_count18;
	retain date DemographicType people_count18;
	set valid_people_count_feb valid_people_count_jun;
	format date mmddyy10.;
	date = max_date + 14;
	DemographicType = 'All Oregonians';
run;

data valid_unvax2;
	merge valid_people_count2 valid_pop ;
	by DemographicType;
	unvaccinated_adults = population18 - people_count18;
run;

******;
proc sql;
	create table valid_vax12 as
		select max(admin_date) as max_date,
				sum(vaccinated) as vaccinated18
	from data.tableau_imms_boost_utd_&break_date
	where admin_date < ('01MAR21'd-14) and age >= 18;
quit;

proc sql;
	create table valid_vax22 as
		select max(admin_date) as max_date,
				sum(vaccinated) as vaccinated18
	from data.tableau_imms_boost_utd_&break_date
	where admin_date < ('01NOV21'd-14) and age >= 18;
quit;

proc sql;
	create table valid_vax32 as
		select max(admin_date) as max_date,
				sum(booster) as vaccinated18
	from data.tableau_imms_boost_utd_&break_date
	where admin_date < ('01DEC21'd-14) and age >= 18;
quit;

proc sql;
	create table valid_vax42 as
		select max(admin_date) as max_date,
				sum(booster) as vaccinated18
	from data.tableau_imms_boost_utd_&break_date
	where admin_date < ('01MAY22'd-14) and age >= 18;
quit;

proc sql;
	create table valid_vax52 as
		select max(admin_date) as max_date,
				sum(booster) as vaccinated18
	from data.tableau_imms_boost_utd_&break_date
	where admin_date < ('01JUN22'd-14) and age >= 18 AND age <=49;
quit;

proc sql;
	create table valid_vax62 as
		select max(admin_date) as max_date,
				sum(booster_second) as vaccinated18
	from data.tableau_imms_boost_utd_&break_date
	where admin_date < ('01JUN22'd-14) AND age >= 50;
quit;

proc sql;
	create table valid_vax72 as
		select max(admin_date) as max_date,
				sum(booster) as vaccinated18
	from data.tableau_imms_boost_utd_&break_date
	where admin_date < ('01JUL22'd-14) AND age >= 18 and age <= 49;
quit;

proc sql;
	create table valid_vax82 as
		select max(admin_date) as max_date,
				sum(booster_second) as vaccinated18
	from data.tableau_imms_boost_utd_&break_date
	where admin_date < ('01JUL22'd-14) AND age >= 50;
quit;

data valid_vaccinated22;
	keep date  vaccinated18;
	retain date  vaccinated18;
	set valid_vax12 valid_vax22 valid_vax32 valid_vax42 valid_vax52 valid_vax62 valid_vax72 valid_vax82;
	format date mmddyy10.;
	date = max_date + 14 - 6;
run;

proc sql;
	create table valid_vaccinated222 as
		select date,
			sum(vaccinated18) as vaccinated18
		from valid_vaccinated22
		group by date;
quit;

*****;
DM 'clear log; clear output;';
dm 'odsresults; clear';

ods excel file="S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Validation\Vax_Status_&break_date..xlsx" options(embedded_titles="on");
title1 "Total Adult Cases";
proc freq data = breakthrough_cases_valid;
	where unknown = 0 AND age >=18 AND age <=115 
			AND TCD >= '01FEB21'd AND date_week <=(&break_date2 - 9)
			AND subtype ^= 'MIS-A' AND subtype ^= 'MIS-C'
			AND CaseID not in (51021030, 51095224, 51129175, 51144844)
			AND SpecDate ^= .; 
	table date_week*case / norow nocol nopercent missing;
run;

title1 "Total Adult Hospitalizations and Deaths";
proc freq data = breakthrough_cases_valid;
	where unknown = 0 AND age >=18 AND age <=115 
			AND TCD >= '01FEB21'd AND date_month <=(&break_date2 - 60)
			AND subtype ^= 'MIS-A' AND subtype ^= 'MIS-C'
			AND CaseID not in (51021030, 51095224, 51129175, 51144844)
			AND SpecDate ^= .; 
	table date_month*hosp date_month*died/ norow nocol nopercent missing;
run;

*;

title1 "Unvaccinated Adult Cases";
proc freq data = breakthrough_cases_valid;
	where unknown = 0 AND age >=18 AND age <=115 
			AND TCD >= '01FEB21'd AND date_week <=(&break_date2 - 9)
			AND subtype ^= 'MIS-A' AND subtype ^= 'MIS-C'
			AND CaseID not in (51021030, 51095224, 51129175, 51144844)
			AND SpecDate ^= .
			AND vb_type ^= 'Primary' AND vb_type not in ('1stBooster', '2ndBooster')
			AND BreakThroughCase = .
			AND one_dose = .; 
	table date_week*case / norow nocol nopercent missing;
run;

title1 "Unvaccinated Adult Hospitalizations and Deaths";
proc freq data = breakthrough_cases_valid;
	where unknown = 0 AND age >=18 AND age <=115 
			AND TCD >= '01FEB21'd AND date_month <=(&break_date2 - 60)
			AND subtype ^= 'MIS-A' AND subtype ^= 'MIS-C'
			AND CaseID not in (51021030, 51095224, 51129175, 51144844)
			AND SpecDate ^= .
			AND vb_type ^= 'Primary' AND vb_type not in ('1stBooster', '2ndBooster')
			AND BreakThroughCase = .
			AND one_dose = .; 
	table date_month*hosp date_month*died/ norow nocol nopercent missing;
run;

*;
title1 "Vaccinated Adult Cases 2/1/2021 to 10/30/2021";
proc freq data = breakthrough_cases_valid;
	where unknown = 0 AND age >=18 AND age <=115 
			AND TCD >= '01FEB21'd AND date_week <= '30OCT21'd
			AND subtype ^= 'MIS-A' AND subtype ^= 'MIS-C'
			AND CaseID not in (51021030, 51095224, 51129175, 51144844)
			AND SpecDate ^= .
			AND vb_type in ('Primary', '1stBooster', '2ndBooster')
			AND BreakThroughCase = 1
			AND one_dose = .; 
	table date_week*case / norow nocol nopercent missing;
run;

title1 "Vaccinated Adult Cases 10/31/2021 to 04/30/22";
proc freq data = breakthrough_cases_valid;
	where unknown = 0 AND age >=18 AND age <=115 
			AND date_week >= '30OCT21'd AND date_week <= '30APR22'd
			AND subtype ^= 'MIS-A' AND subtype ^= 'MIS-C'
			AND CaseID not in (51021030, 51095224, 51129175, 51144844)
			AND SpecDate ^= .
			AND vb_type in ('1stBooster', '2ndBooster')
			AND BreakThroughCase = 1
			AND one_dose = .; 
	table date_week*case / norow nocol nopercent missing;
run;

title1 "Vaccinated Adults 18 to 49 Cases 5/1/2022 to present";
proc freq data = breakthrough_cases_valid;
	where unknown = 0 AND age >=18 AND age <=49 
			AND date_week >= '30APR22'd AND date_week <=(&break_date2 - 9)
			AND subtype ^= 'MIS-A' AND subtype ^= 'MIS-C'
			AND CaseID not in (51021030, 51095224, 51129175, 51144844)
			AND SpecDate ^= .
			AND vb_type in ('1stBooster', '2ndBooster')
			AND BreakThroughCase = 1
			AND one_dose = .; 
	table date_week*case / norow nocol nopercent missing;
run;

title1 "Vaccinated Adults 50+ Cases 5/1/2022 to present";
proc freq data = breakthrough_cases_valid;
	where unknown = 0 AND age >=50 AND age <=115 
			AND date_week >= '30APR22'd AND date_week <=(&break_date2 - 9)
			AND subtype ^= 'MIS-A' AND subtype ^= 'MIS-C'
			AND CaseID not in (51021030, 51095224, 51129175, 51144844)
			AND SpecDate ^= .
			AND booster2 = 1
			AND BreakThroughCase = 1
			AND one_dose = .; 
	table date_week*case / norow nocol nopercent missing;
run;

*;
title1 "Vaccinated Adult Hospitalizations and Deaths 2/1/2021 to 10/30/2021";

proc freq data = breakthrough_cases_valid;
	where unknown = 0 AND age >=18 AND age <=115 
			AND TCD >= '01FEB21'd AND TCD <= '30OCT21'd
			AND subtype ^= 'MIS-A' AND subtype ^= 'MIS-C'
			AND CaseID not in (51021030, 51095224, 51129175, 51144844)
			AND SpecDate ^= .
			AND vb_type in ('Primary', '1stBooster', '2ndBooster')
			AND BreakThroughCase = 1
			AND one_dose = .; 
	table date_month*hosp date_month*died/ norow nocol nopercent missing;
run;

title1 "Vaccinated Adult Hospitalizations and Deaths 10/31/2021 to 04/30/22";

proc freq data = breakthrough_cases_valid;
	where unknown = 0 AND age >=18 AND age <=115 
			AND TCD >= '31OCT21'd AND TCD <= '30APR22'd
			AND subtype ^= 'MIS-A' AND subtype ^= 'MIS-C'
			AND CaseID not in (51021030, 51095224, 51129175, 51144844)
			AND SpecDate ^= .
			AND vb_type in ('1stBooster', '2ndBooster')
			AND BreakThroughCase = 1
			AND one_dose = .; 
	table date_month*hosp date_month*died / norow nocol nopercent missing;
run;

title1 "Vaccinated Adults 18 to 49 Hospitalizations and Deaths 5/1/2022 to present";

proc freq data = breakthrough_cases_valid;
	where unknown = 0 AND age >=18 AND age <=49 
			AND TCD >= '01MAY22'd AND date_month <=(&break_date2 - 60)
			AND subtype ^= 'MIS-A' AND subtype ^= 'MIS-C'
			AND CaseID not in (51021030, 51095224, 51129175, 51144844)
			AND SpecDate ^= .
			AND vb_type in ('1stBooster', '2ndBooster')
			AND BreakThroughCase = 1
			AND one_dose = .; 
	table date_month*hosp date_month*died / norow nocol nopercent missing;
run;

title1 "Vaccinated Adults 50+ Hospitalizations and Deaths 5/1/2022 to present";

proc freq data = breakthrough_cases_valid;
	where unknown = 0 AND age >=50 AND age <=115 
			AND TCD >= '01MAY22'd AND date_month <=(&break_date2 - 60)
			AND subtype ^= 'MIS-A' AND subtype ^= 'MIS-C'
			AND CaseID not in (51021030, 51095224, 51129175, 51144844)
			AND SpecDate ^= .
			AND booster2 = 1
			AND BreakThroughCase = 1
			AND one_dose = .; 
	table date_month*hosp date_month*died / norow nocol nopercent missing;
run;

*******;
title1 "For Cases: Unvaccinated Adult Population";
proc print data = valid_unvax;
	var date unvaccinated_adults;
run;

***********;
title1 "For Cases: Vaccinated Adult Population";
proc print data = valid_vaccinated2;
run;

***************;
*******;
title1 "For Hospitalizations and Deaths: Unvaccinated Adult Population";
proc print data = valid_unvax2;
	var date unvaccinated_adults;
run;

***********;

title1 "For Hospitalizations and Deaths: Vaccinated Adult Population";
proc print data = valid_vaccinated222;
run;

title1 "Case Incidence Check";
proc print data = break.breakthrough_week;
where county = 'Oregon' and DemographicType = 'All Oregonians';
var date cases_unvaccinated18 cases_UTD18;
run;

title1 "Hospitalization Incidence Check";
proc print data = break.breakthrough_month;
where county = 'Oregon' and DemographicType = 'All Oregonians';
var date hosp_unvaccinated18 hosp_UTD18;
run;

title1 "Death Incidence Check";
proc print data = break.breakthrough_month;
where county = 'Oregon' and DemographicType = 'All Oregonians';
var date  died_unvaccinated18 died_UTD18;
run;
title;
ods excel close;
***************;

proc datasets library = work nolist;
	delete BT_by_w: BT_by_m: BT_by_d:;
quit; 


proc datasets library = work nolist;
	delete cumw: cumm: cum_:;
quit;
