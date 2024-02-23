/**************************************************************************************************************************/
/**************************************************************************************************************************/
/*VPU 2021 COVID-19 Master Script*/
/*Paige Snow paige.snow@dhsoha.state.or.us*/


/******************/
/*DASHBOARD CHECKS*/
/******************/
data tableau_imms;
	set data.tableau_imms_boost_utd_&thismonday;
run;

data final_imms;
	set data.final_imms_&thismonday;
run;
/**************/
/*DAILY REPORT*/
/**************/
ods excel file="S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Validation\Daily_Validation3_&today..xlsx";
proc freq data = final_imms;
	where find(filename,"_01_ORA_confidential");
	table filename filename*cvx/norow nocol nopercent  missing;
	format cvx cvx_ped.;
run;
proc freq data = tableau_imms;
	table booster*people_count administrations*vaccine_ped/norow nocol nopercent missing;
run;
proc freq data=final_imms; 
where admin_date between (&yesterday - 9) and &yesterday; 
tables admin_date/nocum nocol nopercent norow; 
run;
proc freq data = tableau_imms;
where admin_date between (&yesterday - 9) and &yesterday; 
tables admin_date*people_count admin_date*booster/nocum nocol nopercent norow; 
run;
proc print data = data.allocations;
where date >= (&today -14);
run;
proc print data = deliveries_by_type;
where date >= (&today - 10) and date < &today;
run;
proc print data = deliveries_type_check;
run;
ods excel close;

/******************************************************/
/******************************************************/
/******************************************************/

proc sql;
   create table new_a as 
     select count(distinct(recip_id)) as recip_idcount,
            count(distinct(VAX_EVENT_ID)) as VAX_EVENT_IDcount
            from final_imms;
quit;
proc sql;
   create table new_b as 
     select put(cvx, cvx.) as vaccine,
			count(distinct(VAX_EVENT_ID)) as VAX_EVENT_IDcount
            from final_imms group by vaccine;
quit;

proc sort data=final_imms; by recip_id admin_date; run; 
data demog1;
	set final_imms;
	by recip_id admin_date; 
	firstid= first.recip_id;
	lastid= last.recip_id;

if firstid=1 and lastid=1 then id=1;
else if firstid=0 and lastid=1 then id=2;
else if firstid=1 and lastid=0 then id=3;
else if firstid=0 and lastid=0 then id=4;
run;
proc sort data=final_imms; by recip_id admin_date; run; 
data demog2;
	set final_imms;
	by recip_id admin_date; 
	firstid= first.recip_id;
	lastid= last.recip_id;

if firstid=1 and lastid=1 then id=1;
else if firstid=0 and lastid=1 then id=2;
else if firstid=1 and lastid=0 then id=3;
else if firstid=0 and lastid=0 then id=4;
run;

/******************************************************/
/******************************************************/
/******************************************************/
proc sort data=final_imms; by recip_id admin_date; run;
data demogv;
	set final_imms;
	by recip_id admin_date;
	firstid= first.recip_id;
	lastid= last.recip_id;

if firstid=1 and lastid=1 then id=1;
else if firstid=0 and lastid=1 then id=2;
else if firstid=1 and lastid=0 then id=3;
else if firstid=0 and lastid=0 then id=4;

if VAX_SERIES_COMPLETE="YES" then series_complete=1;
		else if VAX_SERIES_COMPLETE IN ("NO","UNK") then series_complete=0;
run;

proc sql;
    create table complete_ids as
    select recip_id,
			sum(series_complete) as sum_series_complete
    from demogv group by recip_id;
    quit;

data last_ids;
	set demogv; where id IN (1,2);
		keep recip_id age_cat rarest_race RECIP_SEX RECIP_ADDRESS_COUNTY RECIP_ADDRESS_ZIP;
run;
proc sort data=complete_ids; by recip_id; run;
proc sort data=last_ids; by recip_id; run;
data re_sex_comp_county_zip;
	merge last_ids complete_ids; by recip_id;
	if sum_series_complete GE 1 then complete=1;
		else if sum_series_complete=0 then complete=0;
run;
/*	proc contents data=re_sex_comp_county_zip; run;*/
data age_county;
	set demogv; where id IN (1,3);
	keep recip_id;
	run;
/*	proc contents data=age_county; run;*/

proc sort data=age_county; by recip_id; run;
proc sort data=re_sex_comp_county_zip; by recip_id; run;

data county_reporting;
	merge age_county (in=a) re_sex_comp_county_zip (in=b);
	by recip_id; 
	if a=1 and b=1 then merge=1; else merge=0;
	if sum_series_complete GE 1 then complete=1; else complete=0;
run;

/******************************************************/
/******************************************************/
/******************************************************/

proc import out=stevezip datafile="S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Population\Zip Code\SteveZipList2020.csv"
	DBMS=csv REPLACE;
	GETNAMES=YES;
	RUN;

proc contents data=stevezip varnum; run;

data stevezip; set stevezip; 
	rename	cnty=County
			Zip=RECIP_ADDRESS_ZIP;
	drop ID;
		run;

proc contents data=stevezip varnum; run;

proc import out=zipzcta datafile="S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Population\Zip Code\Zip_to_zcta_crosswalk_2021.csv"
	DBMS=csv REPLACE;
	GETNAMES=YES;
	RUN;

proc contents data=zipzcta varnum; run;

data or_zipzcta; 
	set zipzcta; 
	length RECIP_ADDRESS_ZIP 8.;
	where state="OR";* and ZIP_TYPE NE "Post Office or large volume customer"; 
	RECIP_ADDRESS_ZIP = ZIP_CODE;
	drop ZIP_CODE;
run;

proc contents data=or_zipzcta varnum; run;

*persons by zip;
proc sql;
   create table zip_tot as 
     select recip_address_zip,
			count(distinct(recip_id)) as total_persons
            from county_reporting group by recip_address_zip;
quit;

proc contents data=zip_tot varnum; 
run;

data zipcty_counts;
	merge stevezip (in=a) zip_tot (in=b);
	by RECIP_ADDRESS_ZIP;
		if a=1 and b=1 then merge2=1;
		else if a=1 and b=0 then merge2=2;
		else if a=0 and b=1 then merge2=3;

	if merge2 ne 1 then delete; 
	if County="Unk" then delete;
run;
proc contents data=zipcty_counts varnum; run;

proc sort data=or_zipzcta; by RECIP_ADDRESS_ZIP; run;
proc sort data=zipcty_counts; by RECIP_ADDRESS_ZIP; run;
data zipctyzcta_counts;
	merge zipcty_counts (in=a) or_zipzcta (in=b);
		by RECIP_ADDRESS_ZIP;
	if a=1 and b=1 then merge3=1;
	else merge3=-1; 
		*zcta_n = put(ZCTA,$7.);
			drop PO_NAME STATE ZIP_TYPE zip_join_type;
		*rename zcta_n=ZCTA;
		if merge3 ne 1 then delete;
run;
proc contents data=zipctyzcta_counts varnum; run;
proc freq data=zipctyzcta_counts; tables merge3; run;

data zipctyzcta_counts;
retain RECIP_ADDRESS_ZIP ZCTA County total_persons;
	set zipctyzcta_counts;
	drop merge2 merge3;
run;
proc contents data=zipctyzcta_counts varnum; run;

proc sql;
   create table zcta_totals as 
     select zcta,
			sum(total_persons) as total_persons_zcta
            from zipctyzcta_counts group by zcta;
quit;

/******************************************************/
/******************************************************/
/******************************************************/

ods excel file="S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Validation\Trends_Validations3_&today..xlsx";
proc print data=new_a; 
run;
proc print data=new_b; 
run;
proc freq data = tableau_imms;
table vaccinated / missing ;
run;
proc freq data=county_reporting; 
*where lastid=1; 
format age_cat age_dis.; 
tables age_cat; 
run;
proc freq data=county_reporting; 
*where lastid=1; 
format age_cat age_agg.; 
tables age_cat; 
run;
proc freq data=county_reporting; 
*where lastid=1; 
format rarest_race race.;
tables recip_sex rarest_race; 
run;
proc freq data = tableau_imms;
table county*people_count county*vaccinated /norow nocol nopercent;
run;
proc tabulate data=final_imms;
	where recip_address_county in (41001, 41005, 41047, 41067);
 	format recip_address_county county. cvx cvx.; 
  	class recip_address_county CVX;
   	table recip_address_county, CVX*n;
run;
proc freq data=county_reporting; 
format RECIP_ADDRESS_COUNTY county. age_cat age_dis. rarest_race race.; 
tables RECIP_ADDRESS_COUNTY*(age_cat)/nocum nocol nopercent norow; 
run;
proc freq data=county_reporting; 
format RECIP_ADDRESS_COUNTY county. age_cat age_agg. rarest_race race.; 
tables RECIP_ADDRESS_COUNTY*(age_cat recip_sex rarest_race)/nocum nocol nopercent norow; 
run;
proc sort data=zipctyzcta_counts; 
by county ZCTA; 
run;
proc print data=zipctyzcta_counts; 
where county in ('MARION','WASHINGTON');
run;
ods excel close;

ods excel file="S:\Restricted\Immunization Program\COVID19 Data\Visualization\Tableau\Data\Validation\Metrics_Validation3_&today..xlsx";
proc freq data = tableau_imms;
	where age_cat >=3;
	table people_count vaccinated booster_first booster_second booster_biv booster/ norow nocol nopercent;
run;
proc freq data = tableau_imms;
	where people_count = 1 and age_cat >= 3;
	table age_cat*rarest_race/nopercent norow nocol missing;
	format recip_address_county county. age_cat age_agg. rarest_race race.;
run;
proc freq data = tableau_imms;
	where age_cat >= 3;
	table recip_address_county*(people_count vaccinated booster_first booster_second booster_biv booster) /nopercent norow nocol;
	format recip_address_county county.;
run;
proc freq data = tableau_imms;
	where people_count = 1 and age_cat >= 3;
	table recip_address_county*rarest_Race/nopercent norow nocol missing;
	format RECIP_ADDRESS_COUNTY county. rarest_race race.;
run;
proc freq data = tableau_imms;
table people_count vaccinated booster_first booster_second booster_biv booster /norow nocol nopercent;
run;
proc freq data=county_reporting; 
*where lastid=1; 
tables age_cat; 
format age_cat age_agg.; 
run;
proc freq data=tableau_imms; 
where booster_second = 1; 
tables age_cat; 
format age_cat age_agg.; 
run;
proc freq data=demog2; 
where lastid=1; 
tables rarest_race; 
format rarest_race race.;
run;
proc freq data=tableau_imms; 
where booster_second =1; 
tables rarest_race; 
format rarest_race race.;
run;
proc freq data = tableau_imms;
table county*(people_count vaccinated booster_first booster_second booster_biv booster) /norow nocol nopercent;
run;
proc freq data=county_reporting; 
tables RECIP_ADDRESS_COUNTY*(age_cat )/nocum nocol nopercent norow; 
format RECIP_ADDRESS_COUNTY county. age_cat age_agg.; 
run;
proc freq data = tableau_imms;
	where people_count = 1;
	table recip_address_county*rarest_Race/nopercent norow nocol missing;
	format RECIP_ADDRESS_COUNTY county. rarest_race race.;
run;
proc print data=zipctyzcta_counts; 
where county in ('MARION','WASHINGTON','JACKSON');
run;
proc freq data = tableau_imms;
	where recip_address_zip in(97003, 97006, 97076);
	table recip_address_zip*people_count recip_address_zip*booster recip_address_zip*needs_boost_now / norow nocol nopercent;
run;
proc freq data = tableau_imms;
	table needs_boost_now / missing;
run;
ods excel close;

/**************/
/*DAILY CASES UPDATE AND SOCIAL CARD*/
/**************/
proc sql;
	create table daily_social_QA as
		select sum(people_count) as one_dose,
				sum(vaccinated) as series_complete,
				sum(booster_first) as boosted_mono,
				sum(booster_biv) as boosted_biv
		from tableau_imms
		where age >= 18;
quit;

data daily_social_QA;
	set daily_social_QA;
	format DemographicType $20. DemographicCategory $50.;
	DemographicType = 'All Oregonians';
	DemographicCategory = 'All Oregonians';
run;

data daily_social_QA_pop;
	merge daily_social_QA (in = a) pop.state_demog_psu_2021 (in = b);
	by DemographicType DemographicCategory;
	if a = 1 then output;
run;

data daily_social_QA_pop;
	keep Selected_population DemographicCategory one_dose series_complete  boosted_mono boosted_biv population18 percent_one_dose percent_mono_boosted percent_biv_boosted;
	retain Selected_population DemographicCategory one_dose series_complete  boosted_mono boosted_biv population18 percent_one_dose percent_mono_boosted percent_biv_boosted;
	set daily_social_QA_pop;
	format percent_one_dose percent_mono_boosted percent_biv_boosted percent10.1;
	Selected_population = 'Population 18+ years old';
	percent_one_dose = one_dose/population18;
	percent_mono_boosted = boosted_mono/population18;
	percent_biv_boosted = boosted_biv/population18;
	DemographicCategory = 'Statewide';
run;



ods excel file="S:\Offices\Portland (800 NE Oregon St)\ACDP-Outbreaks\2020 Outbreak & Cluster Investigations\Novel Coronavirus 2020\Tableau\Daily Public COVID19 Dashboards\Daily Flows\T2 State Over Time Vax QA.xlsx";
proc print data = daily_social_QA_pop;
run;
ods excel close;

/******************************************************/
/******************************************************/
/******************************************************/
/*Delete large tables from the work library */
proc datasets library = work nolist;
	delete demog1 demog2;
quit;

