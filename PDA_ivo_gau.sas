*Importing only complete records. All records with imcomplete record was not considered;
data gaussian_data;
infile "C:\Users\Tania\OneDrive - Universiteit Hasselt\2nd_semester\PMHD\project_discovering\gaussian_data_group1.csv" firstobs=2 dlm=',';
input Flower_index	T_0	T_1	T_2	T_3	T_4	T_5	T_6	T_7	T_8	T_9	T_10 T_11 T_12 T_13	T_14 T_15 T_16 T_17	T_18 T_19 T_20 Compound	Rater Type Garden Subplot;
if (T_10 ='.' | T_11 ='.' | T_12 ='.' |T_13 ='.' | T_14 ='.' | T_15 ='.' | T_16 ='.' | T_17 ='.'| T_18 ='.' | T_19 ='.'| T_20 ='.')then delete;
run;

proc sort data = gaussian_data;
by compound;
run;

*Print out the dataset;
proc print data = gaussian_data;
title 'The Gaussian data set';
run;
