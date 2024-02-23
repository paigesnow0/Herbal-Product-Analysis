/* Run when needed to update population. Creates population files and places them in data/population folder */
/*Run this annually when the population is updated, or as needed*/
/*%include "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\scripts\12_population_v10_2021popswitchrare.sas";*/

/***************************************************************************************************************/
/***************************************************************************************************************/
/* Run Mondays */

/*** 1 Run */
/******* 1a Import data files ***** IMPORT final_imms ON MONDAY ONLY */
%include "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\scripts\1_import_data_v4_bivalent.sas"; 
/******* 1b Process allocations from Tiberius and place table in data folder. Gets picked up later for weekly update. Used in Weekly Update and Leadership Powerpoint dashs */
/************* vvv Below = archived script with allocations and deliveries, just in case it needs to run again. vvv */
*%include "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\scripts\archive\2_allocations_v2.sas"; 
/******* 1c Process deliveries from Tiberius and place table in data folder. Gets picked up later for weekly update. Used in Weekly Update and Leadership Powerpoint dashs */
/************* vvv Below = archived script with allocations and deliveries, just in case it needs to run again. vvv */
*%include "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\scripts\archive\3_deliveries_v2.sas"; 

/* Check to make sure the correct final_imms was picked up (run on Mondays). Do these proc tabulate's match? */
/***** CRRU Copy *****/ 
proc tabulate data=data.final_imms_&thismonday;
	where recip_address_county in (41001, 41005, 41047, 41067);
 	format recip_address_county county. cvx cvx.; 
  	class recip_address_county CVX;
   	table recip_address_county, CVX*n;
run;

/***** OIP Copy *****/ 
proc tabulate data=data.final_imms;
	where recip_address_county in (41001, 41005, 41047, 41067);
 	format recip_address_county county. cvx cvx.; 
  	class recip_address_county CVX;
   	table recip_address_county, CVX*n;
run;
  
/*** 2 Check allocations and deliveries: NOT NEEDED. WEEKLY UPDATE ARCHIVED AND ALLOCATIONS/DELIVERIES ARCHIVED*/

/*** 3 Run*/
/******* 3a Process final_imms to the analytic data sets for CRRU: tableau_imms_boost (dose level) & completed_extra (person level)
************* This script counts people and flags the date they iniated, completed, got boosted and second boosted. 
************* This script flags whether or not someone is up-to-date and the most recent doses that has them up-to-date or not.
************* tableau_imms_boost is used downstream in all of the following scripts and for data requests to match the dashboards.
************* Kyle picks up the completed_extra table to do his work with LPHAs.*/
%include "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\scripts\4_process_imms_v9_bivalent_v2.sas"; 
/******* 3b Output data validation files to check dashboards and data requests with.
************* These calculations are from Vivian's original coding, and use final_imms to check that tableau_imms_boost was created
************* correctly and that the dashboards, powerpoints, and data requests are built to match these files. */
%include "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\scripts\5_data_validation_v4_2021popswitch.sas";
/******* 3c Make Tableau data sources alert_admin, alert_admin_zip. alert_admin_zip is the only zip data source 
************* This script creates the data sources for the Vaccination Trends dashboards,leadership materials, neighborhood based outreach stuff.
************* All the zip code related stuff is in this script.*/
%include "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\scripts\6_administration_trends_v4_bivalent.sas";
/******* 3d Make Tableau data sources alert_state, alert_county. 
************ This script creates the data sources for the Vaccination Metrics dashboards, and leadership materials.
************ Output files for others (Sarah & ODE, Shannon & Daily)*/
%include "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\scripts\7_metrics_v18_newrare_bivalent.sas";

/******* 3e Process federal data, places a table in the data folder and is used in the next script (supply) */
/************* vvv Below = archived script with federal doses, just in case it needs to run again. vvv */
/************* vvv The supply script picks this up, leaving the supply script as is. vvv */
/*%include "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\scripts\archive\8_federal_doses_v3.sas";*/

/******* 3f Make Tableau data sources supply_countdown(calcs for the 80% equity goal), 
************* supply_percent_change (percent change calcs), supply_spark_all (stacks ALERT and Federal for hidden county tab).
************* This data source feeds into the Vaccination Metrics dashboards and leadership materials.
************* I generally used this script to calculate supply measures that were a pain to do in Tableau.*/
%include "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\scripts\9_supply_v6_weekly_cleaned.sas";

/*** 4 GO and leadership materials, breakthrough denominators can be updated.  */

/*** 5 Trends and Metrics dashboards can be refreshed */

/*** 6 Download R/E data from DUDE */

/*** 7 Run */
/******* Takes REALD and person race tables to determine best source for rarest group for cases. 
************* Also pulls in unknown vax status from ALERT for breakthrough dashboard.
************* This script creates the opera_all data source which feeds into the Combined Dashboard flow and the Breakthrough script.
************* Contributes to Combined Dashboard, Cases by Vaccination Status Dashboard, possibly some leadership materials. */
%include "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\scripts\10_REALD_cases_weekly.sas";

/*** 8 Run flow for combined case and vax data */

/***************************************************************************************************************/
/***************************************************************************************************************/
/* Run Tuesdays: WEEKLY REPORTING HAS BEEN ARCHIVED AND NO LONGER UPDATED AS OF 9/14/2022 */
/*** 9 Receive WSE spreadsheet from Mike Day on Tuesday AM */

/*** 10 Open script and run in chunks*/
/******* Make Tableau data source weekly_update used for the Covid-19 Vaccine Update dashboard. */
/*%include "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\scripts\11_daily_weekly_update_v8_arch_allos_delivs.sas";*/
/************* vvv Below = archived script with allocations and deliveries, just in case it needs to run again. vvv */
*%include "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\scripts\archive\11_daily_weekly_update_v7.sas";

/*** 11 Weekly Update dasboard can be refreshed */

/*** 12 Save files for Breakthrough Report, use Excel Macro: Daily Case Count Table, VB cumulative */

/*** 13 Run flow for Breakthrough Report */
/******* Pulls in case, breakthrough, and vaccination data and makes Tableau data sources breakthrough_week and
************* breakthrough_month for the Cases by Vaccination Status Dashboard.*/
%include "S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\scripts\21_Breakthrough_v3_UTD_v2.sas";

/*** 14 Cases by Vaccination Status / Breakthrough Dashboard can be refreshed */
/***************************************************************************************************************/
/***************************************************************************************************************/

