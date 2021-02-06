use "G:\Mi unidad\linkabbddyscriptderpaperestallidosocial\trauma_rate_itsa.dta", clear
use "G:\Mi unidad\linkabbddyscriptderpaperestallidosocial\trauma_hosp_itsa.dta", clear
clear all
use "C:\Users\andre\OneDrive\Escritorio\protests\trauma_hosp_itsa.dta",clear

cap ssc install itsa
cap ssc install  itsamatch
cap ssc install oda

reshape long ht_, i(isoweek) j(year)
sort year isoweek
xtset year isoweek

replace did=0 if year!=2019

rename ht_ ht

cap label variable rt "Trauma Hospitalizations per Consultations (x 1,000)"
cap label variable ht "Trauma Hospitalizations"

label variable year "Year"

itsa ht, trperiod(42) replace ///
figure posttrend prefix(trauma_hosp) treat(2019) ///
contid(2015 2016 2017 2018)
 
 ******************************************************************* 
clear all
use "C:\Users\andre\OneDrive\Escritorio\protests\trauma_hosp_itsa.dta",clear

reshape long ht_, i(isoweek) j(year)
sort year isoweek

xtset year isoweek

******************************************************************* 
clear all
use "C:\Users\andre\OneDrive\Escritorio\protests\trauma_hosp_itsa.dta",clear

 regress ht_2019 ht_2015 ht_2016 ht_2017 ht_2018, vce(robust)
twoway scatter ht_2019 isoweek, msymbol(circle_hollow) || connected mean_ht_2019 isoweek

glm ht did isoweek, family(poisson) link(log)

******************************************************************* 
clear all
use "C:\Users\andre\OneDrive\Escritorio\protests\trauma_hosp_itsa.dta",clear

reshape long ht_, i(isoweek) j(year)
sort year isoweek
xtset year isoweek

xtabond year 

******************************************************************* 
clear all
use "C:\Users\andre\OneDrive\Escritorio\protests\trauma_hosp_itsa.dta",clear

foreach package in estout linkplot sparkline mkdensity sepscatter semean egenmore ///
psmatch2 xttest2 cem ranktest xtoverid hausman didq missingplot subsetplot diff ///
ivreg2 ivreg29 ivreg28 xtoverid rhausman synth outreg2 { 
			capture which `package' 
                if _rc==111 cap ssc install `package' 
        } 
		

reshape long ht_, i(isoweek) j(year)
sort year isoweek
xtset year isoweek

rename ht_ ht
replace did=0 if year!=2019
gen tr=.
replace tr=1 if year==2019
replace tr=0 if year!=2019

gen tr_tot= tr*did

*xtreg ht did, fe
xtreg ht tr_tot isoweek, fe //*cluster(year)
est store mod1

xtreg ht tr_tot i.isoweek, fe //*cluster(year)
est store mod2

estat ic


*Hence, the Driscoll–Kraay estimation is employed, by which the standard error 
*estimates are robust to general forms of cross-sectional and temporal dependence 
*(Hoechle, 2007). Further, we test for multi-collinearity among data in the 
*Driscoll-Kraay estimation using variance inflation factor (VIF) and the VIF 
*values are all below than 10, indicating that there are no multi-collinearity 
*(Table A9 in Appendix). 

*Hoechle, 2007
*D. Hoechle
*Robust standard errors for panel regressions with cross-sectional dependence
*Stata Journal, 7 (2007), pp. 281-312

*Drukker, D. M. 2003.
*Testing for serial correlation in linear panel-data models. Stata Journal  3: 168–177.
*https://fmwww.bc.edu/repec/bocode/x/xtscc.html
cap scc install xtscc

xtscc ht tr_tot i.isoweek, fe 
est table *, b se t

xtreg ht tr_tot i.isoweek, fe 
xtoverid

bysort isoweek: egen mean_ht=mean(ht) if year!=2019

twoway (connect ht isoweek if year==2019) (connect mean_ht isoweek if year!=2019)

cap egen ht_mdn = median(`var'), by(isoweek tr)
cap egen ht_upq = pctile(`var'), by(isoweek tr) p(75)
cap egen ht_loq = pctile(`var'), by(isoweek tr) p(25)

twoway (connect ht isoweek if year==2019) (connect ht_mdn isoweek if year!=2019)


*xtreg, fe vce(robust) - however, this option does not control for autocorrelation according to the article of Hoechle.
*A significant test statistic indicates the presence of serial correlation.
foreach v of varlist T14_Tot_Op T21_AnyOpioid T21_RecHighDos ///
T32_OpBenzoClasses T42_OpTot T52_Total_Mean T57_Total {
xtscc  `v' mand_reg##treat_var i.year_qrt $xlist, fe 
outreg2 using _DiD_estimates_w_covs_BOTH_rob_ci_final_10_09, dec(2) ///
excel e(F rss ll lag rmse p) ///
title(Panel Estimation: Driscoll-Kraay standard errors) ///
stats(beta coef ci) cfmt(2) ///
append ctitle("`v'") nocons ///
keep(1.mand_reg#1.treat_var) 
}

