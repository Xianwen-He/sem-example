# Simultaneous Equation Models

This file attempts to reproduce the empirical examples about education and occupational prestige in Chapter 4.

### Data

The original dataset is gss2010merged_r2b.dta. Run the Stata file GSS Ch 4 Data Preparation.do to produce the dataset GSS_Ordinal_Mplus.txt, which is for Mplus.

EducData.txt is the same as GSS_Ordinal_Mplus.txt only with a different name. This data file is for R.

### ML and OLS/2SLS estimates

Ch4_educ_occprest_M1.inp - Ch4_educ_occprest_M4.inp produce Maximum Likelihood estimates for Model1 - Model4, respectively.

Ch4_GSSEx_OLS.Rmd contains OLS/2SLS estimates for those models. It also contains the estimates for Model5, which will appear in the next section.



