
proc import DATAFILE="D:\Msc. Biostatistics\Level One\Second Semester\Multivariate and Hierrachical Data\Project Association\count_data_group1.csv" 
dbms=CSV
out=flower replace;
run;

data flower2;
set flower;
if CutTime=1 then ct=1;else ct=0; *Creating dummy for cut time;
if Garden = 1 then gd=1; else gd=0; *Creating dummy for Garden;
if RoseType=1 then rt=1; else rt=0; *Creating dummy for rosetype;
label ct="CutTime(10-14h)"
	  gd="Garden1"
      rt="RoseType1";	
*creating dummies for rater;
if rater=2 then r2=1;else r2=0; label r2="rater2";
if rater=3 then r3=1;else r3=0; label r3="rater3";
if rater=4 then r4=1;else r4=0; label r4="rater4";
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
data flower2(rename=(i=time));
set flower2;
response=0;
do i=1 to 24;
	if i<=days then response=1;
	else response=0;
	output;
end;
run;

proc genmod data=flower2 desc;
class flowerid bushid cuttime compound(ref='1');
model response= cuttime compound*time/ dist=b;
repeated subject=flowerid(bushid)/modelse type=ar;
contrast '3-4' compound*time 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0;
contrast '3-15' compound*time 0 0 1 0 0 0 0 0 0 0 0 0 0 0 -1;
contrast '4-15' compound*time 0 0 0 1 0 0 0 0 0 0 0 0 0 0 -1;
estimate '3' compound*time 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0/ alpha=0.0167;
estimate '4' compound*time 0 0 0 1 0 0 0 0 0 0 0 0 0 0/ alpha=0.0167;
estimate '15' compound*time 0 0 0 0 0 0 0 0 0 0 0 0 0 1/ alpha=0.0167;
run;


*Binary with cluster;
proc nlmixed data = flower2;
title "Model for count out come with random slope";
theta = beta0 + beta1*c2*time + beta2*c3*time + beta3*c4*time  + beta4*c5*time  + beta5*c6*time  + beta6*c7*time  + beta7*c8*time  + beta8*c9*time  + beta9*c10*time  +
beta10*c11*time  + beta11*c12*time  + beta12*c13*time  + beta13*c14*time  + beta14*c15*time + b0*time + beta15*r2 + beta16*r3 + beta17*r4 + beta18*gd + beta19*ct ;
exptheta = exp(theta);
p = exptheta;
model response ~poisson(p);
random b0~normal(0,d)subject= flowerid;
run;

proc genmod data=flower desc;
class flowerid bushid compound(ref='1');
model days=compound garden rosetype/dist=bin link=logit;
repeated subject=bushid/modelse type=exch;
contrast '3-4' compound 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0;
contrast '3-15' compound 0 0 1 0 0 0 0 0 0 0 0 0 0 0 -1;
contrast '4-15' compound 0 0 0 1 0 0 0 0 0 0 0 0 0 0 -1;
estimate '3' compound 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0/ alpha=0.0167;
estimate '4' compound 0 0 0 1 0 0 0 0 0 0 0 0 0 0/ alpha=0.0167;
estimate '15' compound 0 0 0 0 0 0 0 0 0 0 0 0 0 1/ alpha=0.0167;
run;

ods graphics on ;
proc genmod data=flower2 desc;/* plots=all;*/
class flowerid bushid compound(ref='1');
	model response=compound compound*time/ noint dist=b;
	repeated subject=flowerid(bushid)/modelse type=ar;
run;
ods graphics off;

proc genmod data=flower2 desc;/* plots=all;*/
class flowerid bushid compound;
	model response=compound compound*time/ noint dist=b;
	repeated subject=flowerid(bushid)/modelse type=ar;
run;
