*** Program to calculate Mahalanobis Distance accounting for missing data
*** Note: adapted from program by Stas Kolenikov
*** Author: S Bauldry
*** Date: May 4, 2013


*** Defining Mata function for calculation
capture mata: mata drop _MahDis()
mata

void _MahDis(string scalar varlist, string scalar touse, string scalar mahdist, string scalar nummiss, string scalar covmatrixname, string scalar meanvectorname) {
  
	st_view(data=.,.,tokens(varlist),touse)
	st_view(mahd=.,.,tokens(mahdist),touse)
	st_view(miss=.,.,tokens(nummiss),touse)
	
	covmat = st_matrix(covmatrixname)
	meanvc = st_matrix(meanvectorname)
	
	for(i = 1; i <= rows(data); i++) {
		vars     = 1 :- colmissing( data[i,] )
		sigma    = select( select( covmat, vars ), vars' )
		mu       = select( meanvc, vars )
		x        = select( data[i,], vars )
		mahd[i,] = (x - mu)*invsym(sigma)*(x - mu)'
		miss[i,] = sum( vars )
	}
}
end

capture program drop MahD
program MahD
	syntax varlist(numeric) [if] [in], covmat(name) meanvec(name) generate(string)
	
	capture assert "`: rownames `covmat''" == "`varlist'" & "`: colnames `covmat''" == "`varlist'"
	if _rc {
		dis as error "`covmat' does not appear to be the covariance matrix for the specified variables"
	}
	
	marksample touse, novarlist
	
	local mahdist : word 1 of `generate'
	local nummiss : word 2 of `generate'
	
	gen double `mahdist' = .
	gen byte `nummiss' = .
	
	mata : _MahDis("`varlist'", "`touse'", "`mahdist'", "`nummiss'", "`covmat'", "`meanvec'")
	
	gen `mahdist'p = chi2tail( `nummiss', `mahdist' )
	
	lab var `mahdist' "Mahalanobis Distance"
	lab var `nummiss' "Number of non-missing variables in Mahalanobis Distance"
	lab var `mahdist'p "Mahalanobis Distance p-value (chi-square with `nummiss' df)"
end


