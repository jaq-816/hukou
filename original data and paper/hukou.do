*****************************************************************************************************
**Valuing the Urban Hukou in China:Evidence from a Regression Discontinuity Design for Housing Prices
**Last update: 13-AUG-2019
*****************************************************************************************************

clear
set more off 

use "C:\Users\tang\Dropbox\Chen and Tang\hukou-data&dofile\hukou-original.dta", clear

*****************************************************************************************************
**generate variables to run regression from raw data
*****************************************************************************************************

gen cut=area-90
gen d=0
replace d=1 if cut>=0
gen age=2018-age0
encode floor0,gen(floor)
encode elevator0,gen(elevator)
encode decoration0,gen(decoration)
encode rooms0,gen(rooms)

*****************************************************************************************************
**table1 ：statistical description of the treatment and control group；caculate SMD and Variance Ratio
*****************************************************************************************************

logout, save(myfile) word replace: sum price area rooms floor decoration height age elevator green far totalbuild totalhouse if price!=.&area!=.&area>=80 &area<90
logout, save(myfile) word replace: sum price area rooms floor decoration height age elevator green far totalbuild totalhouse if price!=.&area!=.&area>=90 &area<=100

*****************************************************************************************************
**figure1:Regression discontinuity (RD) plot: Average housing price and floor area
*****************************************************************************************************

rdplot price cut if cut>-20 & cut<20 ,cut(0) p(2)  binselect(es) ci(95) generate(id_var meanx_var meany_var cil_var cir_var)  graph(twoway rcap cil_var cir_var meanx_var)

*****************************************************************************************************
**figure2:: Manipulation test plot
*****************************************************************************************************

rddensity cut , plot  plot_range(-20 20) fitselect(restricted)  genvars(temp) graph_options(title("RDDENSITY PLOT: Housing Data") xtitle("area"))


*****************************************************************************************************
**table2:Manipulation test
*****************************************************************************************************

rddensity cut,p(1) fitselect(restricted)

rddensity cut,p(2) fitselect(restricted)

rddensity cut,p(3) fitselect(restricted)


gl cov "i.floor height i.rooms age  i.elevator i.decoration  green far totalbuild totalhouse"

*****************************************************************************************************
**table 3 :Flexible parametric RD methods: linear 
*****************************************************************************************************

reg price d cut if abs(cut)<=5,r
count if abs(cut)<=5 & d==0
count if abs(cut)<=5 & d==1
reg price d cut $cov if abs(cut)<=5,r
count if abs(cut)<=5 & d==0 & age!=.& floor!=.& height!=.& rooms!=.& elevator!=. &decoration!=. &green!=. & far!=. & totalbuild!=. &totalhouse!=.
count if abs(cut)<=5 & d==1 & age!=.& floor!=.& height!=.& rooms!=.& elevator!=. &decoration!=. &green!=. & far!=. & totalbuild!=. &totalhouse!=.

reg price d cut if abs(cut)<=10,r
count if abs(cut)<=10 & d==0
count if abs(cut)<=10 & d==1
reg price d cut $cov if abs(cut)<=10,r
count if abs(cut)<=10 & d==0 & age!=.& floor!=.& height!=.& rooms!=.& elevator!=. &decoration!=. &green!=. & far!=. & totalbuild!=. &totalhouse!=.
count if abs(cut)<=10 & d==1 & age!=.& floor!=.& height!=.& rooms!=.& elevator!=. &decoration!=. &green!=. & far!=. & totalbuild!=. &totalhouse!=.


reg price d cut if abs(cut)<=20,r
count if abs(cut)<=20 & d==0
count if abs(cut)<=20 & d==1
reg price d cut $cov if abs(cut)<=20,r
count if abs(cut)<=20 & d==0 & age!=.& floor!=.& height!=.& rooms!=.& elevator!=. &decoration!=. &green!=. & far!=. & totalbuild!=. &totalhouse!=.
count if abs(cut)<=20 & d==1 & age!=.& floor!=.& height!=.& rooms!=.& elevator!=. &decoration!=. &green!=. & far!=. & totalbuild!=. &totalhouse!=.

*****************************************************************************************************
**table4:Flexible parametric RD methods: linear interaction
*****************************************************************************************************

reg price d cut d#c.cut if abs(cut)<=5,r
reg price d cut d#c.cut $cov if abs(cut)<=5,r

reg price d cut d#c.cut if abs(cut)<=10,r
reg price d cut d#c.cut $cov if abs(cut)<=10,r


reg price d cut d#c.cut if abs(cut)<=20,r
reg price d cut d#c.cut $cov if abs(cut)<=20,r



*****************************************************************************************************
**table5:Flexible parametric RD methods: quadratic
*****************************************************************************************************
reg price d cut d#c.cut c.cut#c.cut if abs(cut)<=5,r
reg price d cut d#c.cut c.cut#c.cut $cov if abs(cut)<=5,r

reg price d cut d#c.cut c.cut#c.cut if abs(cut)<=10,r
reg price d cut d#c.cut c.cut#c.cut $cov if abs(cut)<=10,r

reg price d cut d#c.cut c.cut#c.cut if abs(cut)<=20,r
reg price d cut d#c.cut c.cut#c.cut $cov if abs(cut)<=20,r



*****************************************************************************************************
**table6:Flexible parametric RD methods: quadratic interaction
*****************************************************************************************************
reg price d cut d#c.cut c.cut#c.cut d#c.cut#c.cut if abs(cut)<=5,r
reg price d cut d#c.cut c.cut#c.cut d#c.cut#c.cut $cov if abs(cut)<=5,r

reg price d cut d#c.cut c.cut#c.cut d#c.cut#c.cut if abs(cut)<=10,r
reg price d cut d#c.cut c.cut#c.cut d#c.cut#c.cut $cov if abs(cut)<=10,r

reg price d cut d#c.cut c.cut#c.cut d#c.cut#c.cut if abs(cut)<=20,r
reg price d cut d#c.cut c.cut#c.cut d#c.cut#c.cut $cov if abs(cut)<=20,r


*****************************************************************************************************
**table 7 nonparametric estimation:Robust bias-corrected local polynomial methods
*****************************************************************************************************

*rdrobust: MSE data-driven bandwidth - local linear regression
rdrobust price cut,p(1)  all
*rdrobust: parametric bandwidth - local linear regression
rdrobust price cut,p(1) h(10) all
*rdrobust: data-driven CER bandwidth selector - local linear regression
rdrobust price cut,p(1) bwselect(cerrd) all


*rdrobust: MSE data-driven bandwidth - local quadratic regression
rdrobust price cut,p(2)  all
*rdrobust: parametric bandwidth - local quadratic regression
rdrobust price cut,p(2) h(10) all
*rdrobust: data-driven CER bandwidth selector - local quadratic regression
rdrobust price cut,p(2) bwselect(cerrd) all


*****************************************************************************************************
**table 8 Placebo test: Assuming false policy discontinuity
*****************************************************************************************************
gen cut1=area-70
gen cut2=area-110
* rdrobust: MSE data-driven bandwidth - local linear regression
rdrobust price cut1,p(1)  all
rdrobust price cut2,p(1) all
* rdrobust: MSE data-driven bandwidth - local quadratic regression
rdrobust price cut1,p(2)  all
rdrobust price cut2,p(2) all


*****************************************************************************************************
**table 9 palcebo tests-rental data
*****************************************************************************************************

use "C:\Users\tang\Dropbox\Chen and Tang\hukou-data&dofile\hukou-rents.dta", clear
* rdrobust: MSE data-driven bandwidth - local linear regression
rdrobust rentpersquare cut,p(1) all
* rdrobust: data-driven CER bandwidth selector - local linear regression
rdrobust rentpersquare cut,p(1) bwselect(cerrd) all
* rdrobust: parametric bandwidth - local linear regression
rdrobust rentpersquare cut,p(1) h(10) all

* rdrobust: MSE data-driven bandwidth - local quadratic regression
rdrobust rentpersquare cut,p(2)  all
* rdrobust: data-driven CER bandwidth selector - local quadratic regression
rdrobust rentpersquare cut,p(2)  all bwselect(cerrd)
* rdrobust: parametric bandwidth - local quadratic regression
rdrobust rentpersquare cut,p(2) h(10)  all

*****************************************************************************************************
** hukou policy reform
*****************************************************************************************************

use "C:\Users\tang\Dropbox\Chen and Tang\hukou-data&dofile\hukou-after policy reform.dta"

encode floor0,gen(floor)
encode towards0,gen(towards)
encode rooms0,gen(rooms)
gen age=2018-age0
gl cov "i.floor totalfl i.rooms age  i.towards"


*****************************************************************************************************
**table 10 Flexible parametric RD method: After the “acquiring the hukou by purchasing a house” policy was abolished
*****************************************************************************************************

*linear regression

reg price d cut d#c.cut $cov if abs(cut)<=5,r
count if abs(cut)<=5 & d==0
count if abs(cut)<=5 & d==1
reg price d cut d#c.cut $cov  if abs(cut)<=10,r
count if abs(cut)<=10 & d==0
count if abs(cut)<=10 & d==1
reg price d cut d#c.cut $cov  if abs(cut)<=20,r
count if abs(cut)<=20 & d==0
count if abs(cut)<=20 & d==1


*quadratic regression

reg price d cut d#c.cut c.cut#c.cut d#c.cut#c.cut $cov if abs(cut)<=5,r

reg price d cut d#c.cut c.cut#c.cut d#c.cut#c.cut $cov if abs(cut)<=10,r

reg price d cut d#c.cut c.cut#c.cut d#c.cut#c.cut $cov if abs(cut)<=20,r


*****************************************************************************************************
**table 11 Robust bias-corrected local polynomial method: After the “acquiring the hukou by purchasing a house” policy was abolished
*****************************************************************************************************

* rdrobust: MSE data-driven bandwidth - local linear regression
rdrobust price cut,p(1)  all
* rdrobust: data-driven CER bandwidth selector - local linear regression
rdrobust price cut,p(1) bwselect(cerrd) all
* rdrobust: parametric bandwidth - local linear regression
rdrobust price cut,p(1) h(10) all

* rdrobust: MSE data-driven bandwidth - quadratic regression
rdrobust price cut,p(2)  all
* rdrobust: data-driven CER bandwidth selector - local quadratic regression
rdrobust price cut,p(2) bwselect(cerrd) all
* rdrobust: parametric bandwidth - local quadratic regression
rdrobust price cut,p(2) h(10) all


*****************************************************************************************************
**Extensions
*****************************************************************************************************

use "C:\Users\tang\Dropbox\Chen and Tang\hukou-data&dofile\hukou-original.dta", clear

gen cut=area-90
gen d=0
replace d=1 if cut>=0
gen age=2018-age0
encode floor0,gen(floor)
encode elevator0,gen(elevator)
encode decoration0,gen(decoration)
encode rooms0,gen(rooms)

*****************************************************************************************************
**Table 12: Flexible parametric RD method: (migrants>50%)
*****************************************************************************************************

reg price d cut d#c.cut if abs(cut)<=5 & pop1==1,r
count if abs(cut)<=5 & d==0 & pop1==1
count if abs(cut)<=5 & d==1 & pop1==1
reg price d cut d#c.cut if abs(cut)<=10 & pop1==1,r
count if abs(cut)<=10 & d==0 & pop1==1
count if abs(cut)<=10 & d==1 & pop1==1
reg price d cut d#c.cut if abs(cut)<=20 & pop1==1,r
count if abs(cut)<=20 & d==0 & pop1==1
count if abs(cut)<=20 & d==1 & pop1==1
reg price d cut c.cut#c.cut d#c.cut d#c.cut#c.cut if abs(cut)<=5 & pop1==1,r
count if abs(cut)<=5 & d==0 & pop1==1
count if abs(cut)<=5 & d==1 & pop1==1
reg price d cut c.cut#c.cut d#c.cut d#c.cut#c.cut if abs(cut)<=10 & pop1==1,r
count if abs(cut)<=10 & d==0 & pop1==1
count if abs(cut)<=10 & d==1 & pop1==1
reg price d cut c.cut#c.cut d#c.cut d#c.cut#c.cut if abs(cut)<=20 & pop1==1,r
count if abs(cut)<=20 & d==0 & pop1==1
count if abs(cut)<=20 & d==1 & pop1==1

*****************************************************************************************************
**Table 13: Flexible parametric RD method: (migrants<=50%)
*****************************************************************************************************

reg price d cut d#c.cut if abs(cut)<=5 & pop1==0,r
count if abs(cut)<=5 & d==0 & pop1==0
count if abs(cut)<=5 & d==1 & pop1==0
reg price d cut d#c.cut if abs(cut)<=10 & pop1==0,r
count if abs(cut)<=10 & d==0 & pop1==0
count if abs(cut)<=10 & d==1 & pop1==0
reg price d cut d#c.cut if abs(cut)<=20 & pop1==0,r
count if abs(cut)<=20 & d==0 & pop1==0
count if abs(cut)<=20 & d==1 & pop1==0
reg price d cut c.cut#c.cut d#c.cut d#c.cut#c.cut if abs(cut)<=5 & pop1==0,r
count if abs(cut)<=5 & d==0 & pop1==0
count if abs(cut)<=5 & d==1 & pop1==0
reg price d cut c.cut#c.cut d#c.cut d#c.cut#c.cut if abs(cut)<=10 & pop1==0,r
count if abs(cut)<=10 & d==0 & pop1==0
count if abs(cut)<=10 & d==1 & pop1==0
reg price d cut c.cut#c.cut d#c.cut d#c.cut#c.cut if abs(cut)<=20 & pop1==0,r
count if abs(cut)<=20 & d==0 & pop1==0
count if abs(cut)<=20 & d==1 & pop1==0

*****************************************************************************************************
**Table 14: Flexible parametric RD method: Top school districts
*****************************************************************************************************

reg price d cut d#c.cut if abs(cut)<=5 & powerschool==1,r
count if abs(cut)<=5 & d==0 & powerschool==1
count if abs(cut)<=5 & d==1 & powerschool==1
reg price d cut d#c.cut if abs(cut)<=10 & powerschool==1,r
count if abs(cut)<=10 & d==0 & powerschool==1
count if abs(cut)<=10 & d==1 & powerschool==1
reg price d cut d#c.cut if abs(cut)<=20 & powerschool==1,r
count if abs(cut)<=20 & d==0 & powerschool==1
count if abs(cut)<=20 & d==1 & powerschool==1
reg price d cut c.cut#c.cut d#c.cut d#c.cut#c.cut if abs(cut)<=5 & powerschool==1,r
count if abs(cut)<=5 & d==0 & powerschool==1
count if abs(cut)<=5 & d==1 & powerschool==1
reg price d cut c.cut#c.cut d#c.cut d#c.cut#c.cut if abs(cut)<=10 & powerschool==1,r
count if abs(cut)<=10 & d==0 & powerschool==1
count if abs(cut)<=10 & d==1 & powerschool==1
reg price d cut c.cut#c.cut d#c.cut d#c.cut#c.cut if abs(cut)<=20 & powerschool==1,r
count if abs(cut)<=20 & d==0 & powerschool==1
count if abs(cut)<=20 & d==1 & powerschool==1

*****************************************************************************************************
**Table 15: Flexible parametric RD method: Ordinary school districts
*****************************************************************************************************

reg price d cut d#c.cut if abs(cut)<=5 & powerschool==0,r
count if abs(cut)<=5 & d==0 & powerschool==0
count if abs(cut)<=5 & d==1 & powerschool==0
reg price d cut d#c.cut if abs(cut)<=10 & powerschool==0,r
count if abs(cut)<=10 & d==0 & powerschool==0
count if abs(cut)<=10 & d==1 & powerschool==0
reg price d cut d#c.cut if abs(cut)<=20 & powerschool==0,r
count if abs(cut)<=20 & d==0 & powerschool==0
count if abs(cut)<=20 & d==1 & powerschool==0
reg price d cut c.cut#c.cut d#c.cut d#c.cut#c.cut if abs(cut)<=5 & powerschool==0,r
count if abs(cut)<=5 & d==0 & powerschool==0
count if abs(cut)<=5 & d==1 & powerschool==0
reg price d cut c.cut#c.cut d#c.cut d#c.cut#c.cut if abs(cut)<=10 & powerschool==0,r
count if abs(cut)<=10 & d==0 & powerschool==0
count if abs(cut)<=10 & d==1 & powerschool==0
reg price d cut c.cut#c.cut d#c.cut d#c.cut#c.cut if abs(cut)<=20 & powerschool==0,r
count if abs(cut)<=20 & d==0 & powerschool==0
count if abs(cut)<=20 & d==1 & powerschool==0

*****************************************************************************************************
*Table 16: Robust bias-corrected local polynomial methods: Local linear model(p=1),migrants>50% and Top school districts
*****************************************************************************************************
 
* rdrobust: MSE data-driven bandwidth - local linear regression

rdrobust price cut if pop1==1,p(1)  all

* rdrobust: CER data-driven bandwidth - local linear regression

rdrobust price cut if pop1==1,p(1) bwselect(cerrd) all

* rdrobust: parametric bandwidth - local linear regression

rdrobust price cut if pop1==1,p(1) h(10) all

* rdrobust: MSE data-driven bandwidth - local linear regression

rdrobust price cut if powerschool==1,p(1)  all

* rdrobust: CER data-driven bandwidth - local linear regression

rdrobust price cut if powerschool==1,p(1) bwselect(cerrd) all

* rdrobust: parametric bandwidth - local linear regression

rdrobust price cut if powerschool==1,p(1) h(10) all

*****************************************************************************************************
**table 17 Robust bias-corrected local polynomial methods: Local linear model (p=1),migrants<=50% and ordinary school districts
*****************************************************************************************************

* rdrobust: MSE data-driven bandwidth - local linear regression
rdrobust price cut if pop1==0,p(1)  all

* rdrobust: CER data-driven bandwidth - local linear regression
rdrobust price cut if pop1==0,p(1) bwselect(cerrd) all

* rdrobust: parametric bandwidth - local linear regression
rdrobust price cut if pop1==0,p(1) h(10) all

* rdrobust: MSE data-driven bandwidth - local linear regression
rdrobust price cut if powerschool==0,p(1)  all

* rdrobust: CER data-driven bandwidth - local linear regression
rdrobust price cut if powerschool==0,p(1) bwselect(cerrd) all

* rdrobust: parametric bandwidth - local linear regression
rdrobust price cut if powerschool==0,p(1) h(10) all

