*** Purpose: To prepare GSS data for SEM examples
*** Author: S Bauldry
*** Date: March 5, 2013
*** Updated on May 29, 2022 by X.H.


*** Setting working directory
*cd "~/Dropbox/_RA/Mizzou Presentation/SEM programs"
cd "\\Client\D$\Documents\SEM\CH3\Ch 3 Model Interpretation and Fit" 


*** Data for 2010 file downloaded on September 24, 2012 from
*** http://www3.norc.org/GSS+Website/Download/STATA+v8.0+Format/

*** Load desired variables from 2010 wave
*** note: change path to location of downloaded data
*** note: self-satisfaction not asked in 2010 wave
*** note: all of the questions were asked on each ballot (A, B, C)
use id sex educ paeduc maeduc papres80 mapres80 age hispanic racecen1 ///
    sibs happy family16 paocc maocc prestg80 using gss2010merged_r2b, replace
    
*** Recoding and renaming sex
gen female = (sex == 2)

*** Constructing single race variable
gen hisp = (hispanic != 1) if !mi(hispanic)
replace racecen1 = 16 if hisp == 1
recode racecen1 (1 = 1) (2 = 2) (4/10 = 3) (16 = 4) (3 11/15 = 5), gen(race)
qui tab race, gen(r)
drop racecen1 hispanic hisp
rename r2 black
rename r3 asian
rename r4 hispanic
rename r5 othrace

lab def rc 1 "white" 2 "black" 3 "asian" 4 "hispanic" 5 "other"
lab val race rc
lab var race "race"

*** Generating indicators for NA when missing father's/mother's education
*** substituting mean for these missing values
*** .d = "don't know"
*** .i = "not applicable" (part of a skip pattern)
*** .n = "no answer"
***
*** Note: According to the GSS website "NA" is given when unemployed, no father
*** substitute (mother substitute), not married, disabled, or retired
foreach x of varlist paeduc maeduc { 
	gen na`x' = ( `x' == .i )
	gen `x'M = `x'
	qui sum `x'M
	replace `x'M = r(mean) if `x'M == .i
}

*** Generating indicators for father's/mother's with 0 years education
gen paNOeduc = ( paeducM == 0 ) if !mi(paeducM)
gen maNOeduc = ( maeducM == 0 ) if !mi(maeducM)

*** Adding variable labels
lab var paeducM "father's education (NA = mean)"
lab var napaeduc "I: father's education = NA"
lab var paNOeduc "I: father's education = 0"
lab var maeducM "mother's education (NA = mean)"
lab var namaeduc "I: mother's education = NA"
lab var maNOeduc "I: mother's education = 0"

*** Generating indicators for NA when missing father's/mother's prestige
*** Note: the missing values code for prestige does not distinguish NA from
*** other types of missing. The missing value codes for occupation do 
*** distinguish NA from other types and the two occupation variables are missing
*** for the same cases. So, I use the missing values codes for occupation to
*** determine the NAs for prestige.
foreach s in pa ma {
	gen na`s'pr80 = ( `s'occ == .i )
	gen `s'pr80M = `s'pres80
	qui sum `s'pr80M
	replace `s'pr80M = r(mean) if `s'occ == .i
}

lab var papr80M "father's prestige (NA = mean)"
lab var napapr80 "I: father's prestige = NA"
lab var mapr80M "mother's prestige (NA = mean)"
lab var namapr80 "I: mother's prestige = NA"

*** Generating log of number of siblings
gen lnsibs = ln(sibs + 1)

*** Just keeping cases with complete data
egen nm = rowmiss(prestg80 educ paeducM maeducM paNOeduc maNOeduc napaeduc ///
                  namaeduc papr80M mapr80M napapr80 namapr80 age lnsibs ///
                  female race)
drop if nm > 0
drop nm

*** Saving data for analysis
save GSSEx, replace


capture log close
log using "GSS Regression.txt", replace text

*** Estimating regression model
reg educ paeducM maeducM paNOeduc maNOeduc napaeduc namaeduc papr80M ///
     mapr80M napapr80 namapr80 age lnsibs female black-othrace
***estat gof, stats(all)

capture log close

*** Saving data for analysis in Mplus
keep educ paeducM maeducM paNOedu maNOeduc napaeduc namaeduc papr80M mapr80M ///
     napapr80 namapr80 age female black-othrace prestg80 lnsibs
desc
outsheet using GSSExMp.txt, replace comma noname nolabel





