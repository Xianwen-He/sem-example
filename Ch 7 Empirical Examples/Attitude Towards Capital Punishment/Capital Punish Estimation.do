*** Purpose: To fit the LPM, probit, and logit model for capital punishment example

*** setting working directory
cd "\\Client\\D$\\Documents\\SEM\\CH7\\Ch 7 Empirical Example\\Attitude Towards Capital Punishment"

*** Readin data
use "GSS Capital Punish Data", replace

*** Preparing age and education variables
gen lnage = ln(age)
gen lnagesq = ln(age)^2
gen educsq = educ^2



*** Estimating model
** LMP
regress cappun lnage c.lnage#c.lnage educ c.educ#c.educ female black ///
               asian hispanic othrace, vce(robust)
predict yhat_m1
* hist yhat_m1, title("Distribution of predicted values")

** estimating margins for 
margins, at(lnage = 3.6889 educ = 8 female = 0 black = 0 asian = 0 hispanic = 0 othrace = 0)
margins, at(lnage = 3.6889 educ = 12 female = 0 black = 0 asian = 0 hispanic = 0 othrace = 0)
margins, at(lnage = 3.6889 educ = 16 female = 0 black = 0 asian = 0 hispanic = 0 othrace = 0)
margins, at(lnage = 3.6889 educ = 16 female = 0 black = 1 asian = 0 hispanic = 0 othrace = 0)

** probit
probit cappun lnage c.lnage#c.lnage educ c.educ#c.educ female black ///
              asian hispanic othrace, vce(robust)

* Determining R-square (based on y*)
predict pred_ystar, xb
sum pred_ystar, detail
local var_ystar = r(Var)
dis `var_ystar'/( `var_ystar' + 1 )

margins, at(lnage = 3.6889 educ = 8 female = 0 black = 0 asian = 0 hispanic = 0 othrace = 0)
margins, at(lnage = 3.6889 educ = 12 female = 0 black = 0 asian = 0 hispanic = 0 othrace = 0)
margins, at(lnage = 3.6889 educ = 16 female = 0 black = 0 asian = 0 hispanic = 0 othrace = 0)
margins, at(lnage = 3.6889 educ = 16 female = 0 black = 1 asian = 0 hispanic = 0 othrace = 0)

** logit
logit cappun lnage c.lnage#c.lnage educ c.educ#c.educ female black ///
             asian hispanic othrace, vce(robust)

* Determining R-square (based on y*) 
predict pred_ystar2, xb
sum pred_ystar2, detail
local var_ystar2 = r(Var)
dis `var_ystar2'/( `var_ystar2' + _pi^2/3 )

margins, at(lnage = 3.6889 educ = 8 female = 0 black = 0 asian = 0 hispanic = 0 othrace = 0)
margins, at(lnage = 3.6889 educ = 12 female = 0 black = 0 asian = 0 hispanic = 0 othrace = 0)
margins, at(lnage = 3.6889 educ = 16 female = 0 black = 0 asian = 0 hispanic = 0 othrace = 0)
margins, at(lnage = 3.6889 educ = 16 female = 0 black = 1 asian = 0 hispanic = 0 othrace = 0)



