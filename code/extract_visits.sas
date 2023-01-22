* ============================================================================;
* Setup;
* ============================================================================;

* Set path for log; 
proc printto log="/home/kissler/StrepPharyngitis/logs/extract_visits_log.txt" new;
run;

* Set path for printing output;
proc printto print="/home/kissler/StrepPharyngitis/logs/extract_visits_out.txt" new;
run;

* Make sure there's nothing in the 'work' library;
proc datasets lib=work nolist kill; quit; run;

* Specify data and output libraries;
libname out "/home/kissler/StrepPharyngitis/output/private/";
libname dathome "/data/markscan_authorized/data";
libname dat10 "/data/markscan_authorized/data/commercial/2010";
libname dat11 "/data/markscan_authorized/data/commercial/2011";
libname dat12 "/data/markscan_authorized/data/commercial/2012";
libname dat13 "/data/markscan_authorized/data/commercial/2013";
libname dat14 "/data/markscan_authorized/data/commercial/2014";
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
proc import datafile="/home/kissler/StrepPharyngitis/data/ccs_map_full.csv"
        out=ccs_map
        dbms=csv
        replace;
run;

* Make sure variable lengths are sufficient to avoid truncation:;
data ccs_map (keep=DX COND ICD);
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


%macro getvisits_pre15(year=,yeartag=);
	
	* Import 'o' (outpatient claims) and reduce to those in the cohort;
	data GeoVisits&year. (keep=DX1 DX2 ENROLID SVCDATE);
		merge GeoCohort&year. (in=inleft)
		dat&year..ccaeo&year.&yeartag. (in=inright keep=DX1 DX2 ENROLID SVCDATE);
		by ENROLID; 
		IF inleft & inright; 
	run;

	* Specify lengths for the diagnosis variables to avoid truncation;
	data GeoVisits&year. (keep=DX1 DX2 ENROLID SVCDATE ICD);
		length DX1 $30 DX2 $30;
	    set GeoVisits&year.(rename=(DX1=DX1_orig DX2=DX2_orig));
	    ICD="9";
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

	data GeoVisits&year. (keep=DX1 DX2 ENROLID SVCDATE ICD COND1);
		merge ccs_map (rename=(DX=DX1 COND=COND1) in=inleft)
		GeoVisits&year. (in=inright);
		by DX1 ICD;
		if inright;
		if inleft then COND1=COND1;
		else COND1="Other/None";
	run;

	* Append conditions for DX2;
	proc sort data=GeoVisits&year.;
		by DX2 ICD;
	run;

	data GeoVisits&year. (keep=DX1 DX2 ENROLID SVCDATE ICD COND1 COND2);
		merge ccs_map (rename=(DX=DX2 COND=COND2) in=inleft)
		GeoVisits&year. (in=inright);
		by DX2 ICD;
		if inright;
		if inleft then COND2=COND2;
		else COND2="Other/None";
	run;

	proc sort data=GeoVisits&year.;
		by COND1 COND2;
	run;

	* Make sure that a given condition is only counted once per visit;
	data GeoVisits&year. (keep=DX1 DX2 ENROLID SVCDATE ICD COND1 COND2);
		set GeoVisits&year.;
		if COND1=COND2 then COND2="Other/None";
	run;

	* Add a Month column; 
	data GeoVisits&year. (keep=DX1 DX2 ENROLID SVCDATE MONTH COND1 COND2);
		set GeoVisits&year.;
		MONTH=month(SVCDATE);
	run;

	* Sort to prepare for summarizing prevalence;
	proc sort data=GeoVisits&year.;
		by ENROLID SVCDATE;
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

	data GeoVisits&year. (keep=DX1 DX2 ENROLID SVCDATE ICD COND1);
		merge ccs_map (rename=(DX=DX1 COND=COND1) in=inleft)
		GeoVisits&year. (in=inright);
		by DX1 ICD;
		if inright;
		if inleft then COND1=COND1;
		else COND1="Other/None";
	run;

	* Append conditions for DX2;
	proc sort data=GeoVisits&year.;
		by DX2 ICD;
	run;

	data GeoVisits&year. (keep=DX1 DX2 ENROLID SVCDATE ICD COND1 COND2);
		merge ccs_map (rename=(DX=DX2 COND=COND2) in=inleft)
		GeoVisits&year. (in=inright);
		by DX2 ICD;
		if inright;
		if inleft then COND2=COND2;
		else COND2="Other/None";
	run;

	proc sort data=GeoVisits&year.;
		by COND1 COND2;
	run;

	* Make sure that a given condition is only counted once per visit;
	data GeoVisits&year. (keep=DX1 DX2 ENROLID SVCDATE ICD COND1 COND2);
		set GeoVisits&year.;
		if COND1=COND2 then COND2="Other/None";
	run;

	* Add a Month column; 
	data GeoVisits&year. (keep=DX1 DX2 ENROLID SVCDATE MONTH COND1 COND2);
		set GeoVisits&year.;
		MONTH=month(SVCDATE);
	run;

	* Sort to prepare for summarizing prevalence;
	proc sort data=GeoVisits&year.;
		by ENROLID SVCDATE;
	run;

%mend;


%macro reducedata(year=,yeartag=);

	* Append demographic data to visits;
	data GeoVisits&year. (keep=DX1 DX2 STATE AGEGRP SEX MONTH COND1 COND2);
		merge GeoVisits&year. (keep=DX1 DX2 ENROLID COND1 COND2 MONTH in=inleft)
		GeoCohort&year. (keep=ENROLID STATE AGEGRP SEX in=inright);
		by ENROLID;
		if inleft;
	run;

	* Arrange visits;
	proc sort data=GeoVisits&year.;
		by STATE AGEGRP SEX MONTH COND1;
	run;

	* Sum visits by condition 1;
	data GeoVisitsc1&year. (keep=STATE AGEGRP SEX MONTH COND1 NVISITS);
		set GeoVisits&year.;
		NVISITS + 1;
		by STATE AGEGRP SEX MONTH COND1;
		if first.STATE or first.AGEGRP or first.SEX or first.MONTH or first.COND1 then NVISITS = 1;
		if last.STATE or last.AGEGRP or last.SEX or last.MONTH or last.COND1;
	run;

	proc sort data=GeoVisits&year.;
		by STATE AGEGRP SEX MONTH COND2;
	run;

	* Sum visits by condition 2;
	data GeoVisitsc2&year. (keep=STATE AGEGRP SEX MONTH COND2 NVISITS);
		set GeoVisits&year.;
		NVISITS + 1;
		by STATE AGEGRP SEX MONTH COND2;
		if first.STATE or first.AGEGRP or first.SEX or first.MONTH or first.COND2 then NVISITS = 1;
		if last.STATE or last.AGEGRP or last.SEX or last.MONTH or last.COND2;
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

	* Clean entries;
	data GeoVisitsc1&year.;
		set GeoVisitsc1&year.;
		if missing(NVISITS) then NVISITS=0;
		if STATE="North Carolin" then STATE="North Carolina";
		if STATE="South Carolin" then STATE="South Carolina";
	run;

	data GeoVisitsc2&year.;
		set GeoVisitsc2&year.;
		if missing(NVISITS) then NVISITS=0;
		if STATE="North Carolin" then STATE="North Carolina";
		if STATE="South Carolin" then STATE="South Carolina";
	run;

	data GeoCohort&year.;
		set GeoCohort&year.;
		if STATE="North Carolin" then STATE="North Carolina";
		if STATE="South Carolin" then STATE="South Carolina";
	run;

%mend;

* ============================================================================;
* Do the reduction;
* ============================================================================;

* ---------------------------------------------------------------------------;
%getcohort(year=10, yeartag=1); *1sam;
%getvisits_pre15(year=10, yeartag=1); *1sam;
%reducedata(year=10, yeartag=1); *1sam;

proc export data=GeoVisitsc110
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoVisitsc110_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc export data=GeoVisitsc210
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoVisitsc210_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc export data=GeoCohort10
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoCohort10_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc delete data=GeoVisits10; run; 
proc delete data=GeoVisitsc110; run; 
proc delete data=GeoVisitsc210; run; 
proc delete data=GeoCohort10; run; 

* ---------------------------------------------------------------------------;
%getcohort(year=11, yeartag=1); *1sam;
%getvisits_pre15(year=11, yeartag=1); *1sam;
%reducedata(year=11, yeartag=1); *1sam;

proc export data=GeoVisitsc111
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoVisitsc111_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc export data=GeoVisitsc211
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoVisitsc211_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc export data=GeoCohort11
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoCohort11_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc delete data=GeoVisits11; run; 
proc delete data=GeoVisitsc111; run; 
proc delete data=GeoVisitsc211; run; 
proc delete data=GeoCohort11; run; 

* ---------------------------------------------------------------------------;
%getcohort(year=12, yeartag=1); *1sam;
%getvisits_pre15(year=12, yeartag=1); *1sam;
%reducedata(year=12, yeartag=1); *1sam;

proc export data=GeoVisitsc112
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoVisitsc112_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc export data=GeoVisitsc212
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoVisitsc212_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc export data=GeoCohort12
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoCohort12_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc delete data=GeoVisits12; run; 
proc delete data=GeoVisitsc112; run; 
proc delete data=GeoVisitsc212; run; 
proc delete data=GeoCohort12; run; 

* ---------------------------------------------------------------------------;
%getcohort(year=13, yeartag=1); *1sam;
%getvisits_pre15(year=13, yeartag=1); *1sam;
%reducedata(year=13, yeartag=1); *1sam;

proc export data=GeoVisitsc113
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoVisitsc113_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc export data=GeoVisitsc213
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoVisitsc213_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc export data=GeoCohort13
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoCohort13_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc delete data=GeoVisits13; run; 
proc delete data=GeoVisitsc113; run; 
proc delete data=GeoVisitsc213; run; 
proc delete data=GeoCohort13; run; 

* ---------------------------------------------------------------------------;
%getcohort(year=14, yeartag=1); *1sam;
%getvisits_pre15(year=14, yeartag=1); *1sam;
%reducedata(year=14, yeartag=1); *1sam;

proc export data=GeoVisitsc114
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoVisitsc114_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc export data=GeoVisitsc214
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoVisitsc214_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc export data=GeoCohort14
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoCohort14_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc delete data=GeoVisits14; run; 
proc delete data=GeoVisitsc114; run; 
proc delete data=GeoVisitsc214; run; 
proc delete data=GeoCohort14; run; 

* ---------------------------------------------------------------------------;
%getcohort(year=15, yeartag=1); *1sam;
%getvisits(year=15, yeartag=1); *1sam;
%reducedata(year=15, yeartag=1); *1sam;

proc export data=GeoVisitsc115
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoVisitsc115_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc export data=GeoVisitsc215
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoVisitsc215_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc export data=GeoCohort15
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoCohort15_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc delete data=GeoVisits15; run; 
proc delete data=GeoVisitsc115; run; 
proc delete data=GeoVisitsc215; run; 
proc delete data=GeoCohort15; run; 

* ---------------------------------------------------------------------------;
%getcohort(year=16, yeartag=1); *1sam;
%getvisits(year=16, yeartag=1); *1sam;
%reducedata(year=16, yeartag=1); *1sam;

proc export data=GeoVisitsc116
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoVisitsc116_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc export data=GeoVisitsc216
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoVisitsc216_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc export data=GeoCohort16
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoCohort16_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc delete data=GeoVisits16; run; 
proc delete data=GeoVisitsc116; run; 
proc delete data=GeoVisitsc216; run; 
proc delete data=GeoCohort16; run; 

* ---------------------------------------------------------------------------;
%getcohort(year=17, yeartag=1); *1sam;
%getvisits(year=17, yeartag=1); *1sam;
%reducedata(year=17, yeartag=1); *1sam;

proc export data=GeoVisitsc117
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoVisitsc117_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc export data=GeoVisitsc217
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoVisitsc217_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc export data=GeoCohort17
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoCohort17_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc delete data=GeoVisits17; run; 
proc delete data=GeoVisitsc117; run; 
proc delete data=GeoVisitsc217; run; 
proc delete data=GeoCohort17; run; 

* ---------------------------------------------------------------------------;
%getcohort(year=18, yeartag=1); *1sam;
%getvisits(year=18, yeartag=1); *1sam;
%reducedata(year=18, yeartag=1); *1sam;

proc export data=GeoVisitsc118
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoVisitsc118_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc export data=GeoVisitsc218
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoVisitsc218_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc export data=GeoCohort18
	outfile='/home/kissler/StrepPharyngitis/output/private/GeoCohort18_2023-01-17.csv'
	dbms=csv
	replace;
run;

proc delete data=GeoVisits18; run; 
proc delete data=GeoVisitsc118; run; 
proc delete data=GeoVisitsc218; run; 
proc delete data=GeoCohort18; run; 

