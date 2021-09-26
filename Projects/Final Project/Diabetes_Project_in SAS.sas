data diabetes;
infile "diabetes.csv" dlm="," firstobs=2 dsd;
input Pregnancy Glucose BloodPressure SkinThickness Insulin BMI DPF Age Outcome;
run;
proc print data=diabetes;
run; 

/* To check is there any missing values present in table*/
proc means data=diabetes nmiss;
run;

/* To check the datatypes of data*/
proc contents data=diabetes;
run;

/* To check the summary  of the data*/
proc summary data=diabetes print n mean median  mode stddev min max;
var Pregnancy Glucose BloodPressure SkinThickness Insulin BMI DPF ;
run;

/* Pie diagram of Diabetic women and Non Diabetic women */
proc sgpie data=diabetes ;
pie Outcome/  datalabelloc=outside;  ;
run;

/* To check the correlation between columns */
proc corr data=diabetes;
run;
/* Result : 
   The chance of Diabetes patients  mainly correlates with the values of Glucose, BMI, Age, Pregnancy*/



/* Histogram of Glucose level in diabetic dataset*/
title"Glucose level in Diabetic and Non Diabetic women";
proc sgplot  data=diabetes;
histogram Glucose/group=Outcome transparency=0.5 fillattrs=(color=olive);
density Glucose /type=normal group=Outcome;
keylegend /location=inside position=topright across=1;
run;

/* Histogram of Preganancy count in diabetic dataset */
title"Number of Pregancy in Diabetic and Non Diabetic women";
proc sgplot  data=diabetes;
histogram Pregnancy/group=Outcome transparency=0.5;
density Pregnancy /type=normal group=Outcome;
keylegend /location=inside position=topright across=1;
run;

/* Box plot of Pregnancy count */
proc sgplot data=diabetes;
vbox Pregnancy/group=Outcome;
run;


/* Histogram ofBMI in Diabetes dataset */
title" BMI in Diabetic and Non Diabetic Women";
proc sgplot  data=diabetes;
histogram BMI/group=Outcome transparency=0.5 fillattrs=(color=teal);
density BMI /type=normal group=Outcome ;
keylegend /location=inside position=topright across=1;
run;

/* To find information of data */
proc contents data=diabetes ;
run;

/* To compare the Median and Maximum values of both diabetes and non diabetes patients */ 
proc means data=diabetes(where=(Outcome=1)) print median  max ;
var Pregnancy Glucose BloodPressure SkinThickness Insulin BMI DPF Age;
title "Diabetes Patients";
proc means data=diabetes(where=(Outcome=0)) print median  max;
var Pregnancy Glucose BloodPressure SkinThickness Insulin BMI DPF Age;
title "Non Diabetes Patients";
run;
/*.............*/ 

/* To add one column to the table */
proc sql;
alter table diabetes add Groups char(20);
quit;
run;

/* update the data by adding age group with conditions */
proc sql;
update diabetes 
set Groups=
CASE WHEN age <= 16 THEN 'Child'
WHEN age <= 30 and age>16 THEN 'Young Adult'
WHEN age <= 45 and age>30 THEN 'Middle-Aged Adult'
ELSE 'Old-Aged Adult'
END;
QUIT;
run;

/* To show the updated data */
proc print data=diabetes;
run;

/* To create Table with Diabtes patients only*/
proc sql;
create table DPatient as 
select Pregnancy,Glucose,BloodPressure,SkinThickness,Insulin,BMI,DPF,Age,Groups from diabetes where Outcome=1;
quit;
proc print data=DPatient;
run;

/* Percentage of Diabetes patients by Age Categories*/
proc sql;
select Groups, ((COUNT( * ) / ( SELECT COUNT( * ) FROM DPatient)) * 100 ) AS Percentage from DPatient group by Groups order by Groups;
quit;
run;
/* Around 45% Diabetes patients were Middle-Aged Adults and 33% were Young Adults.*/

/* To compare the correlation between glucose, BMI and Insulin within all age groups*/
proc corr data=DPatient(where=(Groups='Young Adult'));
var Glucose BMI Insulin;
title"Young Adult";
proc corr data=DPatient(where=(Groups='Middle-Aged Adult'));
var Glucose BMI Insulin;
title"Middle-Aged Adult";
proc corr data=DPatient(where=(Groups='Old-Aged Adult'));
var Glucose BMI Insulin;
title"Old-Aged Adult";
run;

/* Bar graph showing Insulin level by Different age groups */
proc sgplot data=DPatient;
     hbar Groups/response=Insulin stat=mean
     datalabel datalabelattrs=(weight=bold);
     title 'Insulin level in Different Age Groups';

/* Bar graph showing Glucose level by different Age groups */
proc sgplot  data=DPatient;
     hbar Groups/response=Glucose stat=mean
     datalabel datalabelattrs=(weight=bold) fillattrs=(color=cadetblue);
     title 'Glucose level in Different Age Groups';
run;

/*Scatter plot showing relationship between Glucose level and Blood Pressure in Middle Aged women*/
proc sgplot data=DPatient(where=(Groups='Middle-Aged Adult'));
   scatter x=Glucose y=BloodPressure;
   title 'Relationship between Glucose and Blood Pressure';
run;

/*
Conclusion:
Although diabetes affects men and women equally, women are more severely impacted by its consequences. 
There are currently over 199 million women living with diabetes, and this is projected to increase to 313
 million by 20401. Diabetes is the ninth leading direct cause of death in women globally, causing 2.1 million
 deaths each year, most of them were pre-mature1. The issue of women and diabetes is important for several reasons.
 
This data showing increase of Glucose level, BMI, Number of Pregnancies and Age were reasons to become  
diabetic patient. The blood Pressure, Skin Thicknes were not involving greatly to become diabetic.

Diabetes mainly seen on Middle Aged women compared to other age groups. The mean of Insulin level 
is relatively low compared to other age groups as well.
*/




     









