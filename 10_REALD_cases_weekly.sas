/*** Rarest Race (REALD & OMB) for Cases ***/

DM 'clear log; clear output;';
dm 'odsresults; clear';


/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/

proc import datafile = "\\dhs.sdc.pvt\PSOB\ACDP-Outbreaks\2020 Outbreak & Cluster Investigations\Novel Coronavirus 2020\Tableau\Combined Vaccine and Case Dashboard\Race and Ethnicity\OperaDataReport_REALD_&filedate2..xlsx"
	out= opera_reald_imp 
	dbms=xlsx replace; 
	getnames=yes; 
	*guessingrows = 200000;
run;


proc contents data = opera_reald_imp order = varnum;
run;

proc freq data = opera_reald_imp;
	table RaceCount RacePrimary / missing; 
run;

data opera_reald;
	set opera_reald_imp;
run;


/*********************************************************************************************************************************/
/*********************************************************************************************************************************/
/*********************************************************************************************************************************/

data opera_reald2;
	set opera_reald;
		if RaceCount = 1 and RacePrimary = '' then do;
		RacePrimary = RaceDisplay;
		end;
run;

proc freq data = opera_reald2;
	*where RaceCount = .;
	table RacePrimary RaceCount RaceDisplay / missing;
run;

proc freq data = opera_reald2;
	where RaceCount >= 2;
	table RacePrimary / missing;
run;

proc freq data = opera_reald2;
	table RacePrimary/ missing;
run;

proc freq data = opera_reald2;
	where RaceUnk = 1;
	table RaceCount RacePrimary RaceDisplay/ missing;
run;

/*********************************************************************************************************************************/
/*********************************************************************************************************************************/
/*********************************************************************************************************************************/

data opera_reald2;
	set opera_reald2;
	if RaceCount ^=. then do;
		AIAN = 0;
		Asian = 0;
		Black = 0;
		Hispanic = 0;
		White = 0;
		Other = 0;
		NHPI = 0;
		*Tribal = 0;
	end;
	/*AIAN*/
		/*RaceAiAI, American Indian*/	
		if RaceAiAI = 1 then AIAN = 1;
		/*RaceAiAN, Alaska Native*/	
		if RaceAiAN = 1 then AIAN = 1;
		/*RaceAiCI, Canadian Inuit, Metis, or First Nation*/	
		if RaceAiCI = 1 then AIAN = 1;
		/*RaceAiIM, Indigenous Mexican, Central American, or South American*/	
		if RaceAiIM = 1 then AIAN = 1;
	/*Asian*/
		/*RaceAsianAI, Asian Indian*/	
		if RaceAsianAI = 1 then Asian = 1;
		/*RaceAsianCh, Chinese*/	
		if RaceAsianCh = 1 then Asian = 1;
		/*RaceAsianCmb, cambodian*/
		if RaceAsianCmb = 1 then Asian = 1;
		/*RaceAsianF, Filipino/a*/	
		if RaceAsianF = 1 then Asian = 1;
		/*RaceAsianH, Hmong*/	
		if RaceAsianH = 1 then Asian = 1;
		/*RaceAsianJ, Japanese*/	
		if RaceAsianJ = 1 then Asian = 1;
		/*RaceAsianK, Korean*/	
		if RaceAsianK = 1 then Asian = 1;
		/*RaceAsianL, Laotian*/	
		if RaceAsianL = 1 then Asian = 1;
		/*RaceAsianMyn, communities of myanmar*/	
		if RaceAsianMyn = 1 then Asian = 1;
		/*RaceAsianO, Other Asian*/	
		if RaceAsianO = 1 then Asian = 1;
		/*RaceAsianSA, South Asian*/	
		if RaceAsianSA = 1 then Asian = 1;
		/*RaceAsianV, Vietnamese*/	
		if RaceAsianV = 1 then Asian = 1;
	/*Black*/
		/*RaceBlackA, Other African (Black)*/	
		if RaceBlackA = 1 then Black = 1;
		/*RaceBlackAA, African American*/	
		if RaceBlackAA = 1 then Black = 1;
		/*RaceBlackC, Afro-Caribbean*/	
		if RaceBlackC = 1 then Black = 1;
		/*RaceBlackEth, ethiopian*/	
		if RaceBlackEth = 1 then Black = 1;
		/*RaceBlackO, Other Black*/	
		if RaceBlackO = 1 then Black = 1;
		/*RaceBlackSom, somali*/	
		if RaceBlackSom = 1 then Black = 1;
	/*Hispanic*/
		/*RaceHispCA, Central American*/	
		if RaceHispCA = 1 then Hispanic = 1;
		/*RaceHispM, Mexican*/	
		if RaceHispM = 1 then Hispanic = 1;
		/*RaceHispO, Other Hispanic or Latino/a/x*/	
		if RaceHispO = 1 then Hispanic = 1;
		/*RaceHispSA, South American*/	
		if RaceHispSA = 1 then Hispanic = 1;
	/*MENA - White*/
		/*RaceMidEast, Middle Eastern*/	
		if RaceMidEast = 1 then White = 1;
		/*RaceNorthAf, North African*/	
		if RaceNorthAf = 1 then White = 1;
	/*Other*/
		/*RaceOther, Response of "Other" coded as 1. // Are the data for Other: Please specify mapped somewhere else?*/	
		if RaceOther = 1 then Other = 1;
	/*NHPI*/
		/*RacePiCh, Chamoru (Chamorro)*/	
		if RacePiCh = 1 then NHPI = 1;
		/*RacePiGu, Guamanian // deprecated*/	
		if RacePiGu = 1 then NHPI = 1;
		/*RacePiH, Native Hawaiian*/	
		if RacePiH = 1 then NHPI = 1;
		/*RacePiM, Micronesian*/	
		if RacePiM = 1 then NHPI = 1;
		/*RacePiMrsh, marshallese*/	
		if RacePiMrsh = 1 then NHPI = 1;
		/*RacePiO, Other Pacific Islander*/	
		if RacePiO = 1 then NHPI = 1;
		/*RacePiS, Samoan*/	
		if RacePiS = 1 then NHPI = 1;
		/*RacePiT, Tongan // deprecated*/	
		if RacePiT = 1 then NHPI = 1;
	/*Unknown*/
		/*RaceUnk, "Don't know"?*/	
/*		if RaceUnk = 1 then Unknown = 1;*/
		/*RaceDeclined, "Don't want to answer"*/	
/*		if RaceDeclined = 1 then Unknown = 1;*/
	/*White*/
		/*RaceWhiteE, Eastern European*/	
		if RaceWhiteE = 1 then White = 1;
		/*RaceWhiteO, Other White*/	
		if RaceWhiteO = 1 then White = 1;
		/*RaceWhiteS, Slavic*/	
		if RaceWhiteS = 1 then White = 1;
		/*RaceWhiteW, Western European*/	
		if RaceWhiteW = 1 then White = 1;
	/*Tribal*/
		/*TribalAffiliation, Are you a member of a federally recognized tribe?*/	
/*		if TribalAffiliation = 1 then Tribal = 1;*/
		/*TribalEligibility, Are you eligible, as an AI or AN, to receive services from HIS, a Tribal Health Clinic or an Urban Indian Health Program?*/	
/*		if TribalEligibility = 1 then Tribal = 1;*/
		/*TribalMember, Specify which tribe(s)?*/	
/*		if TribalMember = 1 then Tribal = 1;*/
run;

data opera_reald2;
	set opera_reald2;
		format rarest_race $40.;
		If NHPI=1 then rarest_race='NH/PI';
		else if Black=1 then rarest_race='Black';
		else if AIAN=1 then rarest_race='AI/AN';
		else if Asian=1 then rarest_race='Asian';
		else if Hispanic= 1 then rarest_race='Hispanic';
		else if White=1 then rarest_race='White';
		else if Other=1 then rarest_race='Other Race';
		else if NHPI=0 and Black=0 and AIAN=0 and Asian=0 and Hispanic =0 and White=0 and Other=0 then rarest_race='Other Race';
		else rarest_race='Unknown';

		numb_races = aian + asian + black + nhpi + white + other;
		if NHPI=0 and Black=0 and AIAN=0 and Asian=0 and White=0 and Other=0 then Race_unk=1;

		/*ethnicity*/
		format ethnicity $30.;
		if hispanic = 1 then do; ethnicity = "Hispanic"; end;
		if hispanic = 0 then do; ethnicity = "Non-Hispanic"; end;
		if hispanic = . then do; ethnicity = "Unknown"; end;

		/*race*/
		format race $40.;
		if Race_unk = 1  then race = "Unknown";
		if aian = 1 then race = "AI/AN";
		if asian = 1 then race = "Asian";
		if black = 1 then race = "Black";
		if nhpi = 1 then race = "NH/PI";
		if white = 1 then race = "White";
		if other = 1 then race = "Other Race";
		if numb_races > 1 then race = "Multiracial";

		/*COMBINED RACE-ETHNICITY VARIABLE;*/
		format race_ethnicity $40.;	
		if aian = 1 then race_ethnicity = "Non-Hispanic AI/AN";
		if asian = 1 then race_ethnicity = "Non-Hispanic Asian";
		if nhpi = 1 then race_ethnicity = "Non-Hispanic NH/PI";
		if black = 1 then race_ethnicity = "Non-Hispanic Black";
		if white = 1 then race_ethnicity = "Non-Hispanic White";
		if other = 1 then race_ethnicity = "Non-Hispanic Other Race";
		if (ethnicity = "Non-Hispanic" AND numb_races > 1) then do; race_ethnicity = "Non-Hispanic Multiracial"; end;
		if ethnicity = "Hispanic" then race_ethnicity = "Hispanic" ;
		if (Race_unk = 1 AND ethnicity = "Non-Hispanic") then race_ethnicity = "Unknown";
		if (race = "Unknown" AND ethnicity = "Non-Hispanic") then race_ethnicity = "Unknown";
		if ethnicity = "Unknown" then race_ethnicity = "Unknown";
run;

proc freq data = opera_reald2;
	table rarest_race ethnicity race race_ethnicity/ missing;
run;

proc freq data = opera_reald2;
	table RacePrimary*rarest_race / missing;
run;

/****************************/
/****************************/
/****************************/
/*Keep most recent data */

proc sort data = opera_reald2;
	by PersonID descending Modified;
run;

data opera_reald2;
  set opera_reald2;
  count + 1;
  by PersonID;
  if first.PersonID then count = 1;
run;

proc freq data = opera_reald2;
	table count / missing;
run;

proc freq data = opera_reald2;
	table RaceCount*AIAN RaceAIAI*AIAN RaceAIAN*AIAN RaceAICI*AIAN RaceAIIM*AIAN/ missing;
run;

data opera_reald2a;
	keep PersonID rarest_race1;
	set opera_reald2;
	rarest_race1 = rarest_race;
	if count = 1 then output;
run;

data opera_reald2b;
	keep PersonID rarest_race2;
	set opera_reald2;
	rarest_race2 = rarest_race;
	if count = 2 then output;
run;

data opera_reald2c;
	keep PersonID rarest_race3;
	set opera_reald2;
	rarest_race3 = rarest_race;
	if count = 3 then output;
run;

data opera_reald2d;
	keep PersonID rarest_race4;
	set opera_reald2;
	rarest_race4 = rarest_race;
	if count = 4 then output;
run;


data opera_reald3;
	merge opera_reald2a opera_reald2b opera_reald2c opera_reald2d;
	by PersonID;
	rarest_race = rarest_race1;
	if rarest_race1 not in (' ' , 'Unknown') then rarest_race = rarest_race1;
	if rarest_race2 not in (' ' , 'Unknown') then rarest_race = rarest_race2;
	if rarest_race3 not in (' ' , 'Unknown') then rarest_race = rarest_race3;
	if rarest_race4 not in (' ' , 'Unknown') then rarest_race = rarest_race4;
run;

proc print data = opera_reald3;
	where rarest_race3 ^= ' ';
run;

proc freq data = opera_reald3;
	table rarest_race rarest_race1 rarest_race2;
run;


/****************************/
/****************************/
/****************************/

data opera_reald4;
	keep PersonID rarest_Race rarest_raceREALD;
	set opera_reald3;
	rename rarest_Race = rarest_raceREALD;
run;

/*********************************************************************************************************************************/
/*********************************************************************************************************************************/
/*********************************************************************************************************************************/
/*********************************************************************************************************************************/
/*********************************************************************************************************************************/
/*********************************************************************************************************************************/

proc import datafile = "\\dhs.sdc.pvt\PSOB\ACDP-Outbreaks\2020 Outbreak & Cluster Investigations\Novel Coronavirus 2020\Tableau\Combined Vaccine and Case Dashboard\Race and Ethnicity\OperaDataReport_PersonRace_&filedate2..xlsx"
	out= OPERA_race_imp 
	dbms=xlsx replace; 
	getnames=yes; 
	*guessingrows = 200000;
run;

proc contents data = opera_race_imp;
run;


proc sort data = opera_race_imp nodupkey;
	by PersonID z_CreatedTS z_ModifiedTS;
run;


data opera_race_imp1;
	set opera_race_imp;
	nhpi = 0;
	black = 0;
	aian = 0;
	asian = 0;
	hispanic_new = 0;
	white = 0;
	other = 0;
	if hispanic = '' then hispanic_new = .;
	if find(race, 'Pacific Is.') >= 1 then nhpi = 1;
	if find(race, 'PACIFIC IS.') >= 1 then nhpi = 1;
	if find(race, 'Black') >= 1 then black = 1;
	if find(race, 'BLACK') >= 1 then black = 1;
	if find(race, 'AI/AN') >= 1 then aian = 1;
	if find(race, 'Asian') >= 1 then asian = 1;
	if find(race, 'ASIAN') >= 1 then asian = 1;
	if find(hispanic, 'Yes') >=1 then hispanic_new = 1;
	if find(hispanic, 'No') >=1 then hispanic_new = 0;
	if find(hispanic, 'Refused') >=1 then hispanic_new = .;
	if find(hispanic, 'Declined') >=1 then hispanic_new = .;
	if find(hispanic, 'Unknown') >=1 then hispanic_new = .;
	if find(hispanic, 'UNKNOWN') >=1 then hispanic_new = .;
	if find(race, 'White') >= 1 then white = 1;
	if find(race, 'WHITE') >= 1 then white = 1;
	if find(race, 'Other') >= 1 then other = 1;
	if find(race, 'OTHER') >= 1 then other = 1;
	rename hispanic = hispanic_old hispanic_new = hispanic;
	rename race = race_old;
run;
/*
proc freq data = opera_race_imp1;
	where nhpi = 1;
	table race_old*nhpi / missing;
run;

proc freq data = opera_race_imp1;
	where black = 1;
	table race_old*black / missing;
run;

proc freq data = opera_race_imp1;
	where aian = 1;
	table race_old*aian / missing;
run;

proc freq data = opera_race_imp1;
	where asian = 1;
	table race_old*asian / missing;
run;

proc freq data = opera_race_imp1;
	where white = 1;
	table race_old*white / missing;
run;

proc freq data = opera_race_imp1;
	where other = 1;
	table race_old*other / missing;
run;

proc freq data = opera_race_imp1;
	table hispanic_old*hispanic / missing;
run;
*/

data opera_race;
	set opera_race_imp1;
	rarest_race=(.);
	If NHPI=1 then rarest_race=4;
	else if Black=1 then rarest_race=2;
	else if AIAN=1 then rarest_race=0;
	else if Asian=1 then rarest_race=1;
	else if hispanic=1 then rarest_race=3;
	else if White=1 then rarest_race=6;
	else if Other=1 then rarest_race=8;
	else if NHPI=0 and Black=0 and AIAN=0 and Asian=0 and hispanic IN (0, .) and White=0 and Other=0 then rarest_race=10;
	else rarest_race=12;

	/*flag unknown ethnicity other race */
	if (hispanic = . AND other = 1 AND AIAN = 0 AND Asian = 0 AND Black = 0 AND NHPI = 0 AND White = 0)then do;
		unk_eth_other = 1;
	end;

	/*ethnicity*/
	format ethnicity $30.;
	if hispanic = 1 then do; ethnicity = "Hispanic"; end;
	if hispanic = 0 then do; ethnicity = "Non-Hispanic"; end;
	if hispanic = . then do; ethnicity = "Unknown"; end;

	/*race*/
	/*	count missing and subtract from max possible to get number of non-missing*/
	numb_races = aian + asian + black + nhpi + white + other;
	if  aian + asian + black + nhpi + white + other in (0, .) then race_unk = 1;
	if  aian + asian + black + nhpi + white + other in (1, 2, 3, 4, 5, 6) then race_unk = 0;

	format race $40.;
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

proc print data = opera_race;
	where personID >= 51624616 and personID <= 51624630;
run;

/*
proc freq data = opera_race; 
	table rarest_race*race / missing;
	format rarest_race race.;
run;

proc freq data = opera_race; 
	table rarest_race*race / missing;
	format rarest_race race.;
run;

proc freq data = opera_race; 
	where rarest_race = 0;
	table race_old*rarest_race / missing;
	format rarest_race race.;
run;

proc freq data = opera_race; 
	where rarest_race = 1;
	table race_old*rarest_race / missing;
	format rarest_race race.;
run;

proc freq data = opera_race; 
	where rarest_race = 2;
	table race_old*rarest_race / missing;
	format rarest_race race.;
run;

proc freq data = opera_race; 
	where rarest_race = 3;
	table race_old*rarest_race / missing;
	format rarest_race race.;
run;

proc freq data = opera_race; 
	where rarest_race = 4;
	table race_old*rarest_race / missing;
	format rarest_race race.;
run;

proc freq data = opera_race; 
	where rarest_race = 6;
	table race_old*rarest_race / missing;
	format rarest_race race.;
run;

proc freq data = opera_race; 
	where rarest_race = 8;
	table race_old*rarest_race / missing;
	format rarest_race race.;
run;

proc freq data = opera_race; 
	where rarest_race = 12;
	table race_old*rarest_race / missing;
	format rarest_race race.;
run;

proc freq data = opera_race;
	where rarest_Race = .;
	table race_old / missing;
run;

proc freq data = opera_race;
	table race_old*numb_races / missing;
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

proc freq data = final_imms;
	table rarest_race*aian
			rarest_race*asian
			rarest_race*black
			rarest_race*nhpi
			rarest_race*white
			rarest_race*other
			rarest_race*race_unk
/ missing;
format rarest_race race.;
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
	table race*ethnicity
			ethnicity*race_ethnicity
			race*race_ethnicity
/ missing;
run;
*/

/*Keep most recent data */


data opera_race;
	set opera_race;
run;

proc sort data = opera_race;
	by PersonID descending z_ModifiedTS;
run;

data opera_race;
  set opera_race;
  count + 1;
  by PersonID;
  if first.PersonID then count = 1;
run;

proc freq data = opera_race;
	table count / missing;
run;

data opera_race_a;
	keep PersonID rarest_race1;
	set opera_race;
	rarest_race1 = rarest_race;
	if count = 2 then delete;
run;

data opera_race_b;
	keep PersonID rarest_race2;
	set opera_race;
	rarest_race2 = rarest_race;
	if count = 1 then delete;
run;

data opera_race3;
	merge opera_race_a opera_race_b;
	by PersonID;
	rarest_race = rarest_race1;
	if rarest_race1 in (., 10, 11, 12) and rarest_race2 in (0,1,2,3,4,5,6,7,8) then rarest_race = rarest_race2;
run;

proc print data = opera_race3;
where rarest_race2 > 9;
format rarest_race1 rarest_race2 rarest_race race.;
run;

proc print data = opera_race3 (obs = 50);
where rarest_race1 in (., 10, 11, 12) and rarest_race2 in (0,1,2,3,4,5,6,7,8);
run;

proc freq data = opera_race3;
	table rarest_race rarest_race1 rarest_race2;
run;

/****************************/

data opera_race4;
	keep PersonID rarest_race;
	set opera_race3;
run;

data opera_race4;
	set opera_race4;
	rarest_raceOMB = put(rarest_race, race.);
	drop rarest_race;
run;

proc sort data = opera_race4;
by PersonID;
run;

proc sort data = opera_reald4;
by PersonID;
run;

data opera_merge;
	merge opera_reald4 opera_race4;
	by PersonID;
run;

data opera_merge;
	set opera_merge;
		format rarest_race $40.;
		if rarest_raceREALD = 'Unknown' and rarest_raceOMB = 'Unknown' then rarest_race = 'Unknown';
		else if rarest_raceREALD = '' and rarest_raceOMB = '' then rarest_race = 'Unknown';
		else if rarest_raceREALD = 'Unknown' and rarest_raceOMB = '' then rarest_race = 'Unknown';
		else if rarest_raceREALD = '' and rarest_raceOMB = 'Unknown' then rarest_race = 'Unknown';
		else if rarest_raceREALD = 'Unknown' and rarest_raceOMB ^= 'Unknown' then rarest_race = rarest_raceOMB;
		else if rarest_raceREALD ^= 'Unknown' and rarest_raceOMB = 'Unknown' then rarest_race = rarest_raceREALD;
		else if rarest_raceREALD ^= '' and rarest_raceOMB = '' then rarest_race = rarest_raceREALD;
		else if rarest_raceREALD = '' and rarest_raceOMB ^= '' then rarest_race = rarest_raceOMB;
		else rarest_race = rarest_raceREALD;
		count + 1;
		by PersonID;
		if first.PersonID then count=1;
run;

proc freq data = opera_merge;
	table count / missing;
run;

/**********/


proc freq data = opera_merge;
	table rarest_race / missing;
run;

proc print data = opera_merge;
	where personID >= 51624616 and personID <= 51624630;
run;

proc print data = data.opera_all;
	where personID >= 51624616 and personID <= 51624630;
run;


data data.opera_all;
	keep PersonID rarest_race;
	set opera_merge;
run;

proc freq data = opera_merge;
	table rarest_raceOMB*rarest_raceREALD;
run;

proc export data = data.opera_all outfile='\\wpohaappl109\EPI\opera_all.csv' 
	dbms = csv replace;
run;

proc import datafile = "\\dhs.sdc.pvt\PSOB\ACDP-Outbreaks\2020 Outbreak & Cluster Investigations\Novel Coronavirus 2020\Tableau\Combined Vaccine and Case Dashboard\Race and Ethnicity\OperaDataReport_ALERT_&filedate3..xlsx"
	out= opera_ALERT_imp 
	dbms=xlsx replace; 
	getnames=yes; 
	*guessingrows = 200000;
run;

data opera_alert;
	set opera_ALERT_imp;
	unknown = 0;
	if PersonID = . then delete;
run;

proc sort data = opera_alert nodupkey;
	by PersonID;
run;

data data.opera_alert;
	keep personid unknown;
	set opera_alert;
run;
	
proc export data = data.opera_alert outfile='\\wpohaappl109\EPI\opera_alert.csv' 
	dbms = csv replace;
run;
