/**************************************************************************************************************************/
/**************************************************************************************************************************/
/*CRRU 2022 COVID-19 Master Script for dashboards*/
/*Paige Snow paige.snow@dhsoha.state.or.us*/

/* ONLY RUN THIS SCRIPT ON MONDAYS, if you need parts run, do not overwrite final_imms_&thismonday*/

DM 'clear log; clear output;';
dm 'odsresults; clear';
proc datasets library=work kill nolist; quit;
options compress = YES;

/* Folders where data are pulled from or where data is placed */
libname CVRS "S:\Restricted\Immunization Program\COVID19 Data\CVRS\Excel copies"; *Vivian's folder...;
libname covid "S:\Restricted\Immunization Program\COVID19 Data\CVRS\Excel copies"; *Vivian's folder...;
libname DAILY "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Daily Update Dashboard";
libname METRICS "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Metrics";
libname DATA "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data";
libname POP "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Population";
libname zipcode "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Population\Zip Code";
libname prep "\\wpohaappl109\EPI";
libname reald "\\dhs.sdc.pvt\PSOB\ACDP-Outbreaks\2020 Outbreak & Cluster Investigations\Novel Coronavirus 2020\Tableau\Combined Vaccine and Case Dashboard\Race and Ethnicity";
libname break "\\dhs.sdc.pvt\PSOB\ACDP-Outbreaks\2020 Outbreak & Cluster Investigations\Novel Coronavirus 2020\Tableau\Combined Vaccine and Case Dashboard\Breakthrough";
*libname CDC "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\CDC export"; *where I was storing CDC % vaccinated for Governor's Goal (2021);
*libname CVF "S:\Restricted\Immunization Program\COVID19 Data\Vaccine Finder"; *Has current inventory of vaccine doses, not really used;
*libname deloit "S:\Restricted\Immunization Program\COVID19 Data\Data Requests\I-0007 (Deloitte, race-age x county)"; *was placing data here for Deloitte for a minute;
*libname crru "I:\COVID Response and Recovery\Surveillance Section\Analysis & Reporting Team\VPU-CRRU Combined";

/* STOP - Change the dates */
%let twodays = '28JAN23'd; *two days ago;
%let yesterday = '29JAN23'd; *yesterday;	
%let filedate = 20230129; *yesterday;	
%let today = '30JAN23'd; *today;	

%let thismonday = 2023_0130; *monday file from this week (today); 
%let lastmonday = 2023_0103; *monday file from last week;	

%let filedate2 = 2023_0104; *day you pulled opera R/E data;	

%let filedate3 = 2023_0104; *day you pulled ALERTRequest data, Monday/Tuesday of first full week of month; 
%let break_date = 2023_0102; *Monday of first full week of month; 
%let break_date2 = '02JAN23'd;*Monday of first full week of month;

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
proc format;
  VALUE  county -1 = " "
  				. = " "
				41="Oregon"
				41001="Baker"
				41003="Benton"
				41005="Clackamas"
				41007="Clatsop"
				41009="Columbia"
				41011="Coos"
				41013="Crook"
				41015="Curry"
				41017="Deschutes"
				41019="Douglas"
				41021="Gilliam"
				41023="Grant"
				41025="Harney"
				41027="Hood River"
				41029="Jackson"
				41031="Jefferson"
				41033="Josephine"
				41035="Klamath"
				41037="Lake"
				41039="Lane"
				41041="Lincoln"
				41043="Linn"
				41045="Malheur"
				41047="Marion"
				41049="Morrow"
				41051="Multnomah"
				41053="Polk"
				41055="Sherman"
				41057="Tillamook"
				41059="Umatilla"
				41061="Union"
				41063="Wallowa"
				41065="Wasco"
				41067="Washington"
				41069="Wheeler"
				41071="Yamhill";
	value race	0="AI/AN"
				1="Asian"
				2="Black"
				3="Hispanic"
				4="NH/PI"
				5="Middle Eastern/North African"
				6="White"
				7="Multi"
				8="Other Race"
				10="Unknown"
				11="Decline"
				12="Missing";
	value race_agg	0="AI/AN & Hispanic & Black"
				1="Asian & NH & PI"
				2="AI/AN & Hispanic & Black"
				3="AI/AN & Hispanic & Black"
				4="Asian & NH & PI"
				5="Middle Eastern/North African"
				6="White"
				7="Multi"
				8="Other Race"
				10="Unknown"
				11="Decline"
				12="Missing";
	value $ hisp "2135-2"="Hispanic"
				"2186-5"="Not Hispanic"
				"UNK"="Unknown";
	value race_PUMS 4="AI/AN"
				1="Asian"
				3="Black"
				2="Hispanic"
				5="NH/PI"
				0="White"
				6="Other Race";
	value race_PUMS_agg 4="AI/AN & Hispanic & Black"
				1="Asian & NH & PI"
				3="AI/AN & Hispanic & Black"
				2="AI/AN & Hispanic & Black"
				5="Asian & NH & PI"
				0="White"
				6="Other Race";
	value suppress
				0 -< 11 = "10 or less"
				11 -<51 = "50 or less"
				51 -<101 = "100 or less"
				101 -< 151 = "150 or less"
				151 - high = "Suppressed";
	value age	-1 ="0 to 11" 
				1="12 to 17"
				2="18 to 19"
				3="20 to 29"
				4="30 to 39"
				5="40 to 49"
				6="50 to 59"
				7="60 to 69"
				8="70 to 79"
				9="80+"
				12="All ages";
	value age_65plus
				0 -< 65 = "Under 65"
				65 - high = "65+";
	value age_new
				-1 ="0 to 11" 
				1="12 to 17"
				2="18 to 19"
				3="20 to 29"
				4="30 to 39"
				5="40 to 49"
				6="50 to 59"
				7="60 to 69"
				8="70 to 79"
				9="80+"
				12="All ages";
	value age_dis
				-1 ="0 to 4" 
				1 ="5 to 11" 
				2="12 to 17"
				3="18 to 19"
				4="20 to 29"
				5="30 to 39"
				6="40 to 49"
				7="50 to 59"
				8="60 to 64"
				9="65 to 69"
				10="70 to 79"
				11="80+"
				12="All ages";
	value age_dis_two
				-1 ="0 to 4" 
				1 ="5 to 11" 
				2="12 to 17"
				3="18 to 19"
				4="20 to 29"
				5="30 to 39"
				6="40 to 49"
				7="50 to 59"
				8="60 to 69"
				9="60 to 69"
				10="70 to 79"
				11="80+"
				12="All ages";
	value age_agg
				-1="0 to 4" 
				1 ="5 to 11" 
				2 ="12 to 17"
				3 ="18 to 19"
				4 ="20 to 49"
				5 ="20 to 49"
				6 ="20 to 49"
				7 ="50 to 64"
				8 ="50 to 64"
				9 ="65+"
				10="65+"
				11="65+"
				12="All ages";
	value age_agg_two
				-1="0 to 17" 
				1 ="0 to 17" 
				2 ="0 to 17"
				3 ="18 to 49"
				4 ="18 to 49"
				5 ="18 to 49"
				6 ="18 to 49"
				7 ="50 to 64"
				8 ="50 to 64"
				9 ="65+"
				10="65+"
				11="65+"
				12="All ages";
	value age_agg_three
				-1="0 to 4" 
				1 ="5 to 11" 
				2 ="12 to 17"
				3 ="18+"
				4 ="18+"
				5 ="18+"
				6 ="18+"
				7 ="18+"
				8 ="18+"
				9 ="18+"
				10="18+"
				11="18+"
				12="All ages";
	value age_pums					
				0 -< 5 ="0 to 4"
				5 -< 12 ="5 to 11"
				12 -< 18 ="12 to 17"
				18 -< 20 ="18 to 19"
				20 -< 50 ="20 to 49"
				50 -< 65 ="50 to 64"
				65 - high ="65+";
	value age_pums_new	
				0 -< 5 ="0 to 4"
				5 -< 12 ="5 to 11"
				12 -< 18 ="12 to 17"
				18 -< 20 ="18 to 19"
				20 -< 30 ="20 to 29"
				30 -< 40 ="30 to 39"
				40 -< 50 ="40 to 49"
				50 -< 60 ="50 to 59"
				60 -< 65 ="60 to 64"
				65 -< 70 ="65 to 69"
				70 -< 80 ="70 to 79"
				80 - high ="80+";
	value age_pums_twelve	
				0 -< 12 ="0 to 11"
				12 - high ="12+";
	value age_pums_adult	
				0 -< 18 ="0 to 17"
				18 - high ="18+";
	value age_pums_three	
				0 -< 5 ="0 to 4"
				5 -< 12 ="5 to 11"
				12 -< 18 ="12 to 17"
				18 - high ="18+";
	value age3x -1="0-11yo"
				1="12-49yo" 
				2="50-64yo"
				3="65+";
	value acat	-1 ="0-4yo" 
				1="5-11yo"
				2="12-17yo"
				3="18-19yo"
				4="20-49yo"
				5="50-64yo"
				6="65+yo";
	value cvx	206 = "hMPXV"
				207="Moderna"
				221="Moderna"
				227="Moderna"
				228="Moderna"	
				229="Moderna"
				219="Pfizer"
				218="Pfizer"
				217="Pfizer"
				208="Pfizer"
				300="Pfizer"
				301="Unspecified"
				212="Johnson & Johnson"
				210="Unspecified"	
				211="Novavax"
				213="Unspecified";
				*210="AstraZeneca" 229="Moderna Bivalent" 300="Pfizer Bivalent  12+" 301="Pfizer Bivalent 5 to 11";
	value cvx_biv	
				206 = "hMPXV"
				207="Moderna"
				221="Moderna"
				227="Moderna"
				228="Moderna"	
				229="Moderna Biv"
				219="Pfizer"
				218="Pfizer"
				217="Pfizer"
				208="Pfizer"
				300="Pfizer Biv 12"
				301="Pfizer Biv 5"
				212="Johnson & Johnson"
				210="Unspecified"	
				211="Novavax"
				213="Unspecified";
				*210="AstraZeneca";
	value cvx_ped	
				206 = "hMPXV"
				207="Moderna"
				221="Moderna"
				227="Moderna"
				228="Moderna Under 6"
				218="Pfizer 5 to 11"
				219="Pfizer Under 5"
				208="Pfizer"
				217="Pfizer"
				212="Johnson & Johnson"	
				210="Unspecified"
				211="Novavax"
				213="Unspecified"
				229="Unspecified"
				300="Unspecified"
				301="Unspecified"; 
				*229="Moderna Bivalent 6+" 300="Pfizer Bivalent 12+" 301="Pfizer Bivalent 5 to 11" <-this one may not get approved so we can leave as Unspecified in the cvx_ped;
	value datef 
      low-&twodays = 'Immunizations administered prior to'
      &yesterday-high='Immunizations administered on';
	value weekdayf 1 = "Sunday"
					2 = "Monday"
					3 = "Tuesday"
					4 = "Wednesday"
					5 = "Thursday"
					6 = "Friday"
					7 = "Saturday";
RUN;


/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/* Import data */

/* ALERT all*/
/* Import final_imms from Monday, OIP updates this file daily so we want to save Monday's file to use throughout the week */
data data.final_imms_&thismonday;
	set data.final_imms;
	attrib _all_ label='';
	drop age_cat2;
	format age_cat age_dis.;
run;

/* Delete last Monday's data, this file is large so we want to make sure to delete it when no longer needed */
proc datasets library = data nolist;
	delete final_imms_&lastmonday;
quit;

/* Set the new Monday file to the work library */
data final_imms;
	set data.final_imms_&thismonday;
run;

/* Remove duplicates */
proc sort data = final_imms nodupkey;
	by recip_id vax_event_id;
run;

/*THESE ARE NO LONGER IN USE, BUT KEEPING IT JUST IN CASE IF NEEDED IN THE FUTURE*/
/* Import a file to merge in weekly_update for lines with a zero value */
proc import datafile = "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Tiberius_POST\0_vaccine_line_merge.csv"
	out= vaccine_merge 
	dbms=csv replace; 
	getnames=yes; 
	guessingrows = 7000;
run;

/* Tiberius allocations */
proc import datafile = "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Tiberius_POST\f_timeline_export.xlsx"
	out= allocations_imp_new 
	dbms=xlsx replace; 
run;


/* Tiberius deliveries */
proc import datafile = "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Tiberius_POST\0_deliveries_current.csv"
	out= deliveries_imp 
	dbms=csv replace; 
	getnames=yes; 
	guessingrows = 7000;
run;

proc import datafile = "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Tiberius_POST\0_deliveries_current_new.xlsx"
	out= deliveries_imp_new 
	dbms=xlsx replace; 
run;


/* Tiberius deliveries */
proc import datafile = "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Tiberius_POST\0_federal_doses_current.csv"
	out= federal_imp 
	dbms=csv replace; 
	getnames=yes; 
	guessingrows = 7000;
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

