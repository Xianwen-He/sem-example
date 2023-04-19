**This file is going to reproduce the simulation experiment.**

clear
*Commands below generate Z2star from Z1star + E2 error
drop _all
set obs 500
set seed 54321
gen Z1star=rnormal(0,1)
gen E2=rnormal(0,1)
gen Z2star=  Z1star + E2
gen Z1str=rnormal(0,1)
gen E2b=rnormal(0,1)
gen Z2str=2*Z1str+E2b
*egen STDZ2str=std(Z2str)


*Generate binary Z2 & Z1 version of Z2star & Z1star
* Z2star threshold=0 
gen Z2=0 if Z2star <= 0
replace Z2=1 if Z2star > 0
gen Z1=0 if Z1star <= 0
replace Z1=1 if Z1star > 0
table (Z2) (Z1)

* Generate ordinal Z2ord & Z1ord
gen Z2ord = 0 if Z2star <= -.146
replace Z2ord=1 if Z2star > -.146
replace Z2ord=2 if Z2star > .423
table Z2ord, statistic(percent)

gen Z1ord = 0 if Z1star <= -.878
replace Z1ord=1 if Z1star > -.878
replace Z1ord=2 if Z1star > .080
table Z1ord, statistic(percent)

* Cross-tabulation
table (Z2ord) (Z1ord)

*OLS regression of Z2star on Z1star
reg Z2star Z1star

*probit of Z2 on Z1star
probit Z2 Z1star

*scatter plots
scatter Z2star Z1star, xline(0) yline(0) name(G1, replace)
*change thresholds for 0 cell
scatter Z2star Z1star, xline(-1) yline(1.25) name(G3, replace)
scatter Z2str Z1str, xline(-.75) yline(.75) name(G4, replace)

sample 10
scatter Z2star Z1star, xline(-0.5) yline(1.25) name(G2, replace)


