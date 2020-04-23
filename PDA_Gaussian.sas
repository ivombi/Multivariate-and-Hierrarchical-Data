*Importing only complete records.;
data flower;
infile "D:\Msc. Biostatistics\Level One\Second Semester\Multivariate and Hierrachical Data\Project Association\gaussian_data_group1.csv" firstobs=2 dlm=',';
input Flower_index	T_0	T_1	T_2	T_3	T_4	T_5	T_6	T_7	T_8	T_9	T_10 T_11 T_12 T_13	T_14 T_15 T_16 T_17	T_18 T_19 T_20 Compound	Rater Type Garden Subplot;
if (T_10 ='.' | T_11 ='.' | T_12 ='.' |T_13 ='.' | T_14 ='.' | T_15 ='.' | T_16 ='.' | T_17 ='.'| T_18 ='.' | T_19 ='.'| T_20 ='.')then delete;
run;

proc sort data = flower;
by compound;
run;

*Print out the dataset;
proc print data = flower;
title 'The Gaussian data set';
run;


*Import revised data set;
data flower_rev;
infile "D:\Msc. Biostatistics\Level One\Second Semester\Multivariate and Hierrachical Data\Project Association\Fix_Gaussian2.csv" firstobs=2 dlm=',';
input Index Width Time$ time2 Compound Rater Type Garden Subplot;
*if Width='.' then delete;
run;

*Print out the dataset;
proc print data = flower_rev;
title 'The Gaussian data set';
run;
PROC means data = flower_rev mode mean max min range stddev MAXDEC= 2;
class compound;
VAR width;
run;

*Ingnoring Clustering;
proc glm data= flower_rev;
class compound(ref=first);
model width = compound ;
run;

proc reg data = flower_rev;
model width = compound;
run;


*Linear Mixed Model with random intercept;
proc mixed data = flower_rev;
Title 'Linear Mixed Model considering Random slope';
class subplot (ref=first) index;
model width = time2 subplot /noint solution;
random intercept / type = un subject =index g gcorr;
run;
*Pairwise comparison;
contrast 'compound1 vs compound 2' time2*compound 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0; 
contrast 'compound1 vs compound 3' time2*compound 1 0 -1 0 0 0 0 0 0 0 0 0 0 0 0;
contrast 'compound1 vs compound 4' time2*compound 1 0 0 -1 0 0 0 0 0 0 0 0 0 0 0;
contrast 'compound1 vs compound 5' time2*compound 1 0 0 0 -1 0 0 0 0 0 0 0 0 0 0;
contrast 'compound1 vs compound 6' time2*compound 1 0 0 0 0 -1 0 0 0 0 0 0 0 0 0;
contrast 'compound1 vs compound 7' time2*compound 1 0 0 0 0 0 -1 0 0 0 0 0 0 0 0;
contrast 'compound1 vs compound 8' time2*compound 1 0 0 0 0 0 0 -1 0 0 0 0 0 0 0;
contrast 'compound1 vs compound 9' time2*compound 1 0 0 0 0 0 0 0 -1 0 0 0 0 0 0;
contrast 'compound1 vs compound 10' time2*compound 1 0 0 0 0 0 0 0 0 -1 0 0 0 0 0;
contrast 'compound1 vs compound 11' time2*compound 1 0 0 0 0 0 0 0 0 0 -1 0 0 0 0;
contrast 'compound1 vs compound 12' time2*compound 1 0 0 0 0 0 0 0 0 0 0 -1 0 0 0;
contrast 'compound1 vs compound 13' time2*compound 1 0 0 0 0 0 0 0 0 0 0 0 -1 0 0;
contrast 'compound1 vs compound 14' time2*compound 1 0 0 0 0 0 0 0 0 0 0 0 0 -1 0;
contrast 'compound1 vs compound 15' time2*compound 1 0 0 0 0 0 0 0 0 0 0 0 0 0 -1;

run;

*Linear Mixed Model with random slope;
proc mixed data = flower_rev;
Title 'Linear Mixed Model considering Random Intercept';
class compound (ref=first) garden(ref=first) rater(ref=first) type(ref=first)subplot index;
model width = rater type time2*compound/ solution;
random  intercept time2/ type = un subject =index g gcorr ;
random  intercept / type = un subject =subplot g gcorr ;
run;

/*************************************************************/
proc mixed data = flower_rev;
Title 'Linear Mixed Model considering Random Intercept';
class compound (ref=first) garden(ref=first) type(ref=first) subplot index;
model width = type time2*compound/ solution;
random  intercept time2/ type = un subject =index g gcorr ;
run;


proc mixed data = flower_rev;
Title 'Linear Mixed Model considering Random Intercept';
class compound (ref=first) garden(ref=first) type(ref=first) subplot index;
model width = type time2*compound/solution;
random  intercept time2/ type = un subject =index(subplot) g gcorr solution ;
ods listing excludesolutionr;
ods output solutionr=out;
contrast 'compound3 vs compound 4' time2*compound 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0; 
contrast 'compound3 vs compound 15' time2*compound 0 0 1 0 0 0 0 0 0 0 0 0 0 0 -1;
contrast 'compound4 vs compound 15' time2*compound 0 0 0 1 0 0 0 0 0 0 0 0 0 0 -1;
run;
/********************************************************************/
*Pairwise comparison;
contrast 'compound3 vs compound 4' time2*compound 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0; 
contrast 'compound3 vs compound 15' time2*compound 0 0 1 0 0 0 0 0 0 0 0 0 0 0 -1;
contrast 'compound4 vs compound 15' time2*compound 0 0 0 1 0 0 0 0 0 0 0 0 0 0 -1;
contrast 'compound1 vs compound 5' time2*compound 1 0 0 0 -1 0 0 0 0 0 0 0 0 0 0;
contrast 'compound1 vs compound 6' time2*compound 1 0 0 0 0 -1 0 0 0 0 0 0 0 0 0;
contrast 'compound1 vs compound 7' time2*compound 1 0 0 0 0 0 -1 0 0 0 0 0 0 0 0;
contrast 'compound1 vs compound 8' time2*compound 1 0 0 0 0 0 0 -1 0 0 0 0 0 0 0;
contrast 'compound1 vs compound 9' time2*compound 1 0 0 0 0 0 0 0 -1 0 0 0 0 0 0;
contrast 'compound1 vs compound 10' time2*compound 1 0 0 0 0 0 0 0 0 -1 0 0 0 0 0;
contrast 'compound1 vs compound 11' time2*compound 1 0 0 0 0 0 0 0 0 0 -1 0 0 0 0;
contrast 'compound1 vs compound 12' time2*compound 1 0 0 0 0 0 0 0 0 0 0 -1 0 0 0;
contrast 'compound1 vs compound 13' time2*compound 1 0 0 0 0 0 0 0 0 0 0 0 -1 0 0;
contrast 'compound1 vs compound 14' time2*compound 1 0 0 0 0 0 0 0 0 0 0 0 0 -1 0;
contrast 'compound1 vs compound 15' time2*compound 1 0 0 0 0 0 0 0 0 0 0 0 0 0 -1;

run;


*Linear Mixed Model with random intercept and random slope;
proc mixed data = flower_rev;
Title 'Linear Mixed Model considering Random Intercept and slope';
class compound (ref=first) subplot index;
model width = time2*compound/ solution;
random intercept time2/ type = un subject =index g gcorr ;
run;


proc mixed data = flower_rev;
    Title 'Linear Mixed Model considering Random Intercept';
    class compound (ref=first) garden(ref=first) type(ref=first) subplot index;
    model width = type time2*compound/solution;
    random  intercept time2/ type = un subject =index(subplot) g gcorr solution ;
    ods listing excludesolutionr;
    ods output solutionr=out;
    contrast 'compound3 vs compound 4' time2*compound 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0; 
    contrast 'compound3 vs compound 15' time2*compound 0 0 1 0 0 0 0 0 0 0 0 0 0 0 -1;
    contrast 'compound4 vs compound 15' time2*compound 0 0 0 1 0 0 0 0 0 0 0 0 0 0 -1;
run;


proc genmod data=flower_rev ;
class compound (ref=first) garden(ref=first) type(ref=first) subplot index;
model width = type time2*compound/ dist=N;
repeated subject=index(subplot)/modelse type=ar;
run;
