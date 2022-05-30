*** Purpose: To check for influential cases in GSS example
*** Author: S Bauldry
*** Date: October 5, 2012
*** Updated on May 23, 2022 by X.H.


****** Mata function to compute MahD ******
clear mata
capture mata: mata drop MahDCal()

mata
function MahDCal() 
{
	mu = st_matrix("mu")
	sigma = st_matrix("sigma")
	X = st_data(.,1..18)
	N = rows(X)
	md = J(N,1,.)
	for (i=1; i <= N; i++) {
		r = 1 :- colmissing(X[i,.])
		rx = select(X[i,.],r)
		rmu = select(mu,r)
		rsigma = select( select(sigma,r), r')
		md[i,1] = (rx - rmu)*invsym(rsigma)*(rx - rmu)'
	}
	st_store(.,"md",md)
}
end 


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



*** Loading data
cd "\\Client\D$\Documents\SEM\CH3\Ch 3 Outlier and Influence Diagnostics"  
use "GSS Outlier Example", replace

*** generating race indicators
qui tab race, gen(rc)

*** Dropping cases missing data for any covariate
local cov educ LNsibs age female rc2 rc3 rc4 rc5 paeducM paNOeduc NApaeduc ///
	      maeducM maNOeduc NAmaeduc papr80M NApapr80 mapr80M NAmapr80
egen nm = rowmiss(`cov')
drop if nm > 0
drop nm

*** Computing M distances

*** reordering variables for mata function
order `cov'

*** obtaining mean vector and covariance matrix
tokenize `cov'
mat mu = J(1,18,.)
forval i = 1/18 {
	qui mean ``i''
	mat mu[1,`i'] = e(b)
}
	
qui corr `cov', cov
mat sigma = r(C)

*** invoking mata program for M distances
gen md = .
mata: MahDCal()

*** Obtaining DFBETAs, residuals, leverage, and Cook's D
local IVs paeducM maeducM paNOedu maNOeduc NApaeduc NAmaeduc papr80M ///
          mapr80M NApapr80 NAmapr80 age female i.race LNsibs
qui regress educ `IVs' 
qui dfbeta
predict resid, residuals
predict rstud, rstudent
predict lever, leverage
predict cooks, cooksd

*** Running program to identify outliers for each measure
foreach x of varlist rstud _dfbeta* {
	OutID `x' 1
}

foreach x of varlist md lever cooks {
	OutID `x' 0
}

*** Identifying total number of outliers for each case
egen OUTtotal = rowtotal(OUT*)
tab OUTtotal


*** Flagging any case that is an outlier on 6+ measures
gen outlier = (OUTtotal > 5) 



*** Estimating model with and without outliers
qui regress educ age LNsibs female black asian hispanic othrace paeducM ///
  maeducM paNOeduc maNOeduc NApaeduc NAmaeduc papr80M mapr80M NApapr80 ///
  NAmapr80
eststo m1

qui regress educ age LNsibs female black asian hispanic othrace paeducM ///
  maeducM paNOeduc maNOeduc NApaeduc NAmaeduc papr80M mapr80M NApapr80 ///
  NAmapr80 if !outlier
eststo m2
 
esttab m1 m2 using compare_model.csv, b(%5.3f) se(%5.3f) compress nogaps replace





