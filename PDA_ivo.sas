*Author: Kubam Ivo, CreationDate: 1/may/2019, Purpose: Project discovering association;

*Importing the data sets;

data count_data;
infile "C:\\Users\\Tania\\OneDrive - Universiteit Hasselt\\2nd_semester\\PMHD\\project_discovering\\count_data_group1.csv" firstobs=2 dlm=',';
input Days	Rater	CutTime	Compound	Garden	RoseType	BushID$	FlowerID;
if days>=20 then fresh=1;else fresh=0; *Creating a variable for fresh yes/no;
*creating dummies for compounds;
if compound="2" then c2=1;else c2=0; label c2="compound2";
if compound="3" then c3=1;else c3=0; label c3="compound3";
if compound="4" then c4=1;else c4=0; label c4="compound4";
if compound="5" then c5=1;else c5=0; label c5="compound5";
if compound="6" then c6=1;else c6=0; label c6="compound6";
if compound="7" then c7=1;else c7=0; label c7="compound7";
if compound="8" then c8=1;else c8=0; label c8="compound8";
if compound="9" then c9=1;else c9=0; label c9="compound9";
if compound="10" then c10=1;else c10=0; label c10="compound10";
if compound="11" then c11=1;else c11=0; label c11="compound11";
if compound="12" then c12=1;else c12=0; label c12="compound12";
if compound="13" then c13=1;else c13=0; label c13="compound13";
if compound="14" then c14=1;else c14=0; label c14="compound14";
if compound="15" then c15=1;else c15=0; label c15="compound15";

run;

*Sorting the datasets by compound;
proc sort data = count_data;
by compound;
run;


proc print data = count_data;
title 'The count data set';
run;

*****************************Summary Statistics*******************************;
*a)Count Data Set;

PROC means data = count_data mode mean max min range stddev MAXDEC= 0;
VAR Days;
run;

/*The highly frequent number of survival days generally was 24. On an average, the flowers survived for 14days with a minimum surving day
of one and max of 24 given us a range of 23.*/

*i)By Compound;
proc means data = count_data mode mean max min range stddev;
class compound;
var Days;
run;

/*Almost all the compounds had the most frequent survival time for the flowers as 24days with few exceptions like compounds 
2,8,10 and 12 with frequent survival time of 13,7,6 and 13 days respectively. Compounds 15 and 4 both have the highest average number of flower survival days of 17 and 16 respectively.
closely followed by compound 3 with 15 days. Compound 1(water)has an average survival days of approximately 13days. Compound 15 with the highest average survival days
interestingly had a minimum surviving days of 3*/
*Question: Is compound showing more better results?;

*ii)By Garden;
proc means data = count_data mode mean max min range stddev;
class Garden;
var Days;
run;
/*There is just a day difference for both gardens as garden 1 and 2 both have approximately 13 and 14 survival days averagely
and most flowers survived for 24days in both gardens*/
*Question:Does this means there is much effect of the gardens?

*iii) By Rose Type;
proc means data = count_data mode mean max min range stddev;
class RoseType;
var Days;
run;
/*Also just one day difference for both roses as Rose 1 and 2 have approximately 14 and 13 survival days averagely
and both rose types had 24 days as the most surviving days*/
*could this mean the rose type has no effect?

*IV) Compound split into garden;
proc means data = count_data mode mean max min range stddev maxdec=0;
class compound Garden;
var Days;
run;
*Most of the compounds showed averagely one day difference in survival except for a few with averagely 2 days difference e.g compound one
Also it can be noted that garden 2 showed averagely higher surviving days than garden for each compound except for 4 12 and 15 with no difference;

*Compound split into rosetype;
proc means data = count_data mode mean max min range stddev maxdec=0;
class compound RoseType;
var Days;
run;
*Rose type 1 showed on average higer survival days for each compound than Rose Type 2. There were few exceptions like compound 15,7,3,2
that showed no differnce.

*V)By Rater;
proc means data = count_data;
class Rater;
var Days;
run;
*Rater Two had the lowest average survival days of 12 while rater one had 13. Rater 3 and 4 both had an average of approximately 15 days;

proc means data = count_data mode mean max min range stddev maxdec=0;
class compound Rater;
var Days;
run;
*Rater 3 and 4 still showed on an average higher survival days for almost all the compounds;
*Question:Could this imply an effect of rater 3 and 4 on the survival days?;

proc freq data = count_data order=data;
tables fresh * compound/nopercent nocol;
run;
*Compound 15 has the highest number 111(13.3%)of fresh flowers upto 20days. compound 4 and 13 follows with 97(11.63%) and 71(8.5%) of fresh flowers;
proc freq data = count_data order=data;
tables fresh * rosetype/nopercent nocol;
run;

*Rose type 1 is better off than rose type 2 as it has higher number of fresh flowers 449 (53.8%) vs 385 (46.2%)for rose type 1; 
 
*Creating plots;
proc sgplot data = count_data;
vbox days / category = compound ;
refline  13/ labelloc = outside label= "Avg surving day for cpd 1(13days)";
Title "Surviving Days for each compound";
run;

proc sgplot data = count_data;
vbox days / category = compound group= rosetype ;
Title "Surviving Days for each compound by RoseType";
run;

proc sgplot data = count_data;
vbox days / category = compound group= garden;
Title "Surviving Days for each compound by garden";
run;





