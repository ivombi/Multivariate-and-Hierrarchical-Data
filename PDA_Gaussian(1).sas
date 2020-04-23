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
infile "C:\Users\Tania\OneDrive - Universiteit Hasselt\2nd_semester\PMHD\project_discovering\Fix_Gaussian.csv" firstobs=2 dlm=';';
input Index Width Time$ time2 Compound Rater Type Garden Subplot;
*if Width='.' then delete;
run;

proc import datafile = "C:\Users\Tania\OneDrive - Universiteit Hasselt\2nd_semester\PMHD\project_discovering\Fix_Gaussian2.csv"
dbms=CSV
out = flower_rev replace;
delimiter =",";
run;

*Print out the dataset;
proc print data = flower_rev;
title 'The Gaussian data set';
run;

*Ingnoring Clustering;
proc glm data= flower_rev;
class subplot;
model width = time2 subplot time2*subplot;
run;


*Random intercept model;
proc mixed data = flower_rev;
Title 'Random Intercept Model';
class compound(ref=first)index;
model width =  time2*compound /solution;
random intercept /subject=Index;
run;

*Linear Mixed Model with random intercept;
proc mixed data = flower_rev;
Title 'Linear Mixed Model considering Random Intercept and random slope';
class compound (ref=first)index;
model width = time2*compound/ solution;
random intercept time2/ type = un subject =index;
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

*Linear Mixed Model with random intercept and random slope;
proc mixed data = flower_rev;
Title 'Linear Mixed Model considering Random Intercept';
class compound (ref=first) index subplot;
model width = time2  subplot compound time2*compound/ solution;
random intercept time2/ type = un subject =index g gcorr;
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


