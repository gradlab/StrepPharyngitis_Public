* ============================================================================;
* Setup;
* ============================================================================;

* Set path for log; 
proc printto log="/home/kissler/StrepPharyngitis/logs/extract_geography_log.txt" new;
run;

* Set path for printing output;
proc printto print="/home/kissler/StrepPharyngitis/logs/extract_geography_out.txt" new;
run;

* Make sure there's nothing in the 'work' library;
proc datasets lib=work nolist kill; quit; run;

* Specify data and output libraries;
libname out "/home/kissler/StrepPharyngitis/output/private/";
libname dathome "/data/markscan_authorized/data";
libname dat15 "/data/markscan_authorized/data/commercial/2015";
libname dat16 "/data/markscan_authorized/data/commercial/2016";
libname dat17 "/data/markscan_authorized/data/commercial/2017";
libname dat18 "/data/markscan_authorized/data/commercial/2018";

* Import and process table of days per month --------------------------------; 
proc import datafile="/home/kissler/StrepPharyngitis/data/dayspermonth.csv"
        out=dayspermonth
        dbms=csv
        replace;
run;

proc sort data=dayspermonth;
	by DT_MONTH;
run;

* Import and process table of US states --------------------------------------;
proc import datafile="/home/kissler/StrepPharyngitis/data/EGEOLOClist_char.csv"
        out=EGEOLOClist
        dbms=csv
        replace;
run;

* Make sure variable lengths are sufficient to avoid truncation:;
data EGEOLOClist;
	length STATE $30;
    set EGEOLOClist(rename=(STATE=STATE_orig));
    STATE=STATE_orig;
run;

proc sort data=EGEOLOClist;
	by EGEOLOC;
run;

* Import and process table of medical conditions -----------------------------;
proc import datafile="/home/kissler/StrepPharyngitis/data/ccs_map.csv"
        out=ccs_map
        dbms=csv
        replace;
run;

* Make sure variable lengths are sufficient to avoid truncation:;
data ccs_map (keep=DX COND ICD PRIORITY);
	length DX $30 COND $100;
    set ccs_map(rename=(DX=DX_orig COND=COND_orig));
    DX=DX_orig;
    COND=COND_orig;
run;

proc sort data=ccs_map;
	by DX;
run;

* ============================================================================;
* Define extraction/reduction scripts;
* ============================================================================;

* Extract cohort of people who are represented for the entire year;
%macro getcohort(year=,yeartag=);

	* Initial import, ensuring we have RX data;
	data GeoCohort&year. (keep=AGE DT_MONTH EGEOLOC ENROLID MEMDAYS SEX);
		set dat&year..ccaet&year.&yeartag. (keep=AGE DTSTART EGEOLOC ENROLID MEMDAYS SEX);
		DT_MONTH=month(DTSTART);
	run;

	* Restrict to valid states;
	proc sort data=GeoCohort&year.;
		by EGEOLOC;
	run;

	data GeoCohort&year. (keep=AGE DT_MONTH STATE ENROLID MEMDAYS SEX);
		merge EGEOLOClist (in=inleft)
		GeoCohort&year. (in=inright);
		by EGEOLOC; 
		IF inleft & inright; 
	run;

	* Restrict to those with complete coverage through the year; 
	proc sort data=GeoCohort&year.;
		by DT_MONTH;
	run;

	data GeoCohort&year. (keep=AGE DT_MONTH STATE ENROLID MEMDAYS SEX NDAYS where=(MEMDAYS>=NDAYS));
		merge dayspermonth (in=inleft)
		GeoCohort&year. (in=inright);
		by DT_MONTH;
		IF inleft & inright;
	run;

	* Sort so that we extract age in January;
	proc sort data=GeoCohort&year.;
		by ENROLID descending DT_MONTH;
	run;	

	* Count months of enrollment and only keep those with 12;
	* https://stats.idre.ucla.edu/sas/faq/how-can-i-create-an-enumeration-variable-by-groups/;
	data GeoCohort&year. (keep=AGE DT_MONTH STATE ENROLID SEX COUNT where=(COUNT=12));
		set GeoCohort&year.;
		COUNT + 1;
		by ENROLID;
		if first.ENROLID then COUNT = 1;
	run;

	* Turn age into age groups; 
	data GeoCohort&year. (keep=AGEGRP STATE ENROLID SEX);
		set GeoCohort&year.;
		if AGE>=80 then AGEGRP="80plus";
		else if AGE>=70 then AGEGRP="70_79";
		else if AGE>=60 then AGEGRP="60_69";
		else if AGE>=50 then AGEGRP="50_59";
		else if AGE>=40 then AGEGRP="40_49";
		else if AGE>=30 then AGEGRP="30_39";
		else if AGE>=20 then AGEGRP="20_29";
		else if AGE>=10 then AGEGRP="10_19";
		else if AGE>=5 then AGEGRP="05_09";
		else if AGE>=0 then AGEGRP="00_04";
	run;

	* Sort cohort table by ENROLID for joining later;
	proc sort data=GeoCohort&year.;
		by ENROLID;
	run;

%mend;


%macro getvisits(year=,yeartag=);
	
	* Import 'o' (outpatient claims) and reduce to those in the cohort;
	data GeoVisits&year. (keep=DX1 DX2 ENROLID SVCDATE DXVER);
		merge GeoCohort&year. (in=inleft)
		dat&year..ccaeo&year.&yeartag. (in=inright keep=DX1 DX2 ENROLID SVCDATE DXVER);
		by ENROLID; 
		IF inleft & inright; 
	run;

	* Specify lengths for the diagnosis variables to avoid truncation;
	data GeoVisits&year. (keep=DX1 DX2 ENROLID SVCDATE ICD);
		rename DXVER=ICD;
		length DX1 $30 DX2 $30;
	    set GeoVisits&year.(rename=(DX1=DX1_orig DX2=DX2_orig));
	    DX1=DX1_orig;
	    DX2=DX2_orig;
	run;

	* Reduce to unique person-dates for outpatient visits;
	proc sort data=GeoVisits&year. nodupkey;
		by ENROLID SVCDATE;
	run;

	* Append conditions for DX1; 
	proc sort data=GeoVisits&year.;
		by DX1 ICD;
	run;

	data GeoVisits&year. (keep=DX2 ENROLID SVCDATE ICD COND1 PRIORITY1);
		merge ccs_map (rename=(DX=DX1 COND=COND1 PRIORITY=PRIORITY1) in=inleft)
		GeoVisits&year. (in=inright);
		by DX1 ICD;
		if inright;
		if inleft then COND1=COND1;
		else COND1="Other";
		if inleft then PRIORITY1=PRIORITY1;
		else PRIORITY1=1000000;
	run;

	* Append conditions for DX2;
	proc sort data=GeoVisits&year.;
		by DX2 ICD;
	run;

	data GeoVisits&year. (keep=ENROLID SVCDATE ICD COND1 PRIORITY1 COND2 PRIORITY2);
		merge ccs_map (rename=(DX=DX2 COND=COND2 PRIORITY=PRIORITY2) in=inleft)
		GeoVisits&year. (in=inright);
		by DX2 ICD;
		if inright;
		if inleft then COND2=COND2;
		else COND2="Other";
		if inleft then PRIORITY2=PRIORITY2;
		else PRIORITY2=1000000;
	run;

	* Identify the primary condition; 
	data GeoVisits&year. (keep=ENROLID SVCDATE PRIMARYCOND);
		set GeoVisits&year.;
		if PRIORITY1<=PRIORITY2 then PRIMARYCOND=COND1;
		else PRIMARYCOND=COND2;
	run;

	* Sort to prepare for summarizing prevalence;
	proc sort data=GeoVisits&year.;
		by ENROLID SVCDATE;
	run;

%mend;


%macro reducedata(year=,yeartag=);

	* Append demographic data to visits;
	data GeoVisits&year. (keep=STATE AGEGRP SEX PRIMARYCOND);
		merge GeoVisits&year. (keep=ENROLID PRIMARYCOND in=inleft)
		GeoCohort&year. (keep=ENROLID STATE AGEGRP SEX in=inright);
		by ENROLID;
		if inleft;
	run;

	* Arrange visits;
	proc sort data=GeoVisits&year.;
		by STATE AGEGRP SEX PRIMARYCOND;
	run;

	* Sum visits by category;
	data GeoVisits&year. (keep=STATE AGEGRP SEX PRIMARYCOND NVISITS);
		set GeoVisits&year.;
		NVISITS + 1;
		by STATE AGEGRP SEX PRIMARYCOND;
		if first.STATE or first.AGEGRP or first.SEX or first.PRIMARYCOND then NVISITS = 1;
		if last.STATE or last.AGEGRP or last.SEX or last.PRIMARYCOND;
	run;

	* Sum cohort sizes;
	proc sort data=GeoCohort&year.;
		by STATE AGEGRP SEX;
	run;

	data GeoCohort&year. (keep=STATE AGEGRP SEX NMEMB);
		set GeoCohort&year.;
		NMEMB + 1;
		by STATE AGEGRP SEX;
		if first.STATE or first.AGEGRP or first.SEX then NMEMB = 1;
		if last.STATE or last.AGEGRP or last.SEX;
	run;

	* Join cohort sizes;
	data GeoTable&year.;
		merge GeoVisits&year. (in=inleft)
		GeoCohort&year. (in=inright);
		by STATE AGEGRP SEX;
		if inright;
	run;

	* Clean entries;
	data GeoTable&year.;
		set GeoTable&year.;
		if missing(NVISITS) then NVISITS=0;
		if STATE="North Carolin" then STATE="North Carolina";
		if STATE="South Carolin" then STATE="South Carolina";
	run;

%mend;

%macro cleanup(year=,yeartag=);
	proc delete data=GeoCohort&year.; run; 
	proc delete data=GeoVisits&year.; run; 
%mend

* ============================================================================;
* Do the reduction;
* ============================================================================;

%getcohort(year=15, yeartag=1sam); *1sam;
%getvisits(year=15, yeartag=1sam); *1sam;
%reducedata(year=15, yeartag=1sam); *1sam;
%cleanup(year=15, yeartag=1sam); *1sam;

%getcohort(year=16, yeartag=1sam); *1sam;
%getvisits(year=16, yeartag=1sam); *1sam;
%reducedata(year=16, yeartag=1sam); *1sam;
%cleanup(year=16, yeartag=1sam); *1sam;

%getcohort(year=17, yeartag=1sam); *1sam;
%getvisits(year=17, yeartag=1sam); *1sam;
%reducedata(year=17, yeartag=1sam); *1sam;
%cleanup(year=17, yeartag=1sam); *1sam;

%getcohort(year=18, yeartag=1sam); *1sam;
%getvisits(year=18, yeartag=1sam); *1sam;
%reducedata(year=18, yeartag=1sam); *1sam;
%cleanup(year=18, yeartag=1sam); *1sam;

proc export data=GeoTable15
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoTable15_2022-11-16.csv'
	dbms=csv
	replace;
run;

proc export data=GeoTable16
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoTable16_2022-11-16.csv'
	dbms=csv
	replace;
run;

proc export data=GeoTable17
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoTable17_2022-11-16.csv'
	dbms=csv
	replace;
run;

proc export data=GeoTable18
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoTable18_2022-11-16.csv'
	dbms=csv
	replace;
run;

