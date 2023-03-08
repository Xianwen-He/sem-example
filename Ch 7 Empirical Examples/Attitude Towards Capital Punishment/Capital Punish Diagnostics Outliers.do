*** Purpose: To prepare regression diagnostics for capital punishment example
*** Author: S Bauldry
*** Date: May 5, 2013
*** Update: Xianwen He, Mar 8, 2023

*** setting working directory
cd "\\Client\\D$\\Documents\\SEM\\CH7\\Ch 7 Empirical Example\\Attitude Towards Capital Punishment"

****** Program to identify outliers ******
capture program drop OutID
program OutID
	args var LowerBound
	
	*** identify lower bound and upper bound
	qui sum `var', detail
	scalar Q1 = r(p25)
	scalar Q3 = r(p75)
	scalar IQ = Q3 - Q1
	scalar LB = Q1 - 2*IQ
	scalar UB = Q3 + 2*IQ
	
	*** identify outliers
	if `LowerBound' == 1 {
		gen OUT`var' = ( `var' < LB | `var' > UB ) if !mi(`var')
	}
	else {
		gen OUT`var' = ( `var' > UB ) if !mi(`var')
	}
	lab var OUT`var' "Indicator for outlier on `var'"
end




*** Loading prepared data
use "GSS Capital Punish Data", replace


*** Preparing age and education variables
gen lnage = ln(age)
gen lnagesq = ln(age)^2
gen educsq = educ^2


****** Obtaining Mahalanobis Distance's

*** constructing vector of means
local cv cappun lnage lnagesq educ educsq female white black asian hispanic ///
         othrace
tokenize `cv'
mat mu = J(1,11,.)
forval i = 1/11 {
	qui mean ``i''
	mat b = e(b)
	mat mu[1,`i'] = b[1,1]
}
mat list mu

*** constructing covariance matrix
qui cor `cv', cov
mat sigma = r(C)

*** running ado file
MahD `cv', covmat(sigma) meanvec(mu) generate(mahd miss)


*** Obtaining DFBETAs, residuals, leverage, and Cook's D
qui regress cappun lnage lnagesq educ educsq female black asian hispanic ///
            othrace
qui dfbeta
predict resid, residuals
predict rstud, rstudent
predict lever, leverage
predict cooks, cooksd

*** Running program to identify outliers for each measure
foreach x of varlist rstud _dfbeta* {
	OutID `x' 1
}

foreach x of varlist mahd lever cooks {
	OutID `x' 0
}

*** Identifying total number of outliers for each case
egen OUTtotal = rowtotal(OUT*)
tab OUTtotal


*** Flagging any case that is an outlier on 6+ measures
gen outlier = (OUTtotal > 3) 
tab outlier race

gen outlier2 = (OUTtotal > 2)
tab outlier2 race



*** Estimating model with and without outliers
regress cappun lnage lnagesq educ educsq female black asian hispanic ///
            othrace, vce(robust)
eststo m1

regress cappun lnage lnagesq educ educsq female black asian hispanic ///
            othrace if !outlier, vce(robust)
eststo m2

esttab m1 m2 using mod_with_without_outlier.csv, b(%5.3f) se(%5.3f) compress nogaps replace
