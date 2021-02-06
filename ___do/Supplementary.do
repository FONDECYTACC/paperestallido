clear all

if `"`c(os)'"' == "Windows" & `"`c(username)'"' == "andre" global sf `"G:/Mi unidad/linkabbddyscriptderpaperestallidosocial"'
if `"`c(os)'"' == "Windows" & `"`c(username)'"' == "CISS Fondecyt" global sf `"G:/Mi unidad/Alvacast/CURES2_DB"'
if `"`c(os)'"' == "MacOSX" global sf `"/volumes/sdrive/data//"'

di `"${sf}"' //here's the base file
*set working directory
cd `"${sf}"'

cap ssc install outreg2
cap install xtscc

*semean
program define semean, rclass byable(recall) sortpreserve
	version 9.0
	syntax varlist(max=1 ts numeric) [if] [in] ///
		[, noPRInt FUNCtion(string)]
	marksample touse
	tempvar target
	if "`function'" == "" {
		local tgt "`varlist'"
	}
	else {
		local tgt "`function'(`varlist')"
	}
	capture tsset
	capture generate double `target' = `tgt' if `touse'
	if _rc > 0 {
		display as err "Error: bad function `tgt'"
		error 198
		}
	quietly summarize `target' 
	scalar semean = r(sd)/sqrt(r(N))
	if ("`print'" ~= "noprint") {
		display _n "Mean of `tgt' = " r(mean) ///
		" S.E. = " semean
	}
	return scalar semean = semean
	return scalar mean = r(mean)
	return scalar N = r(N)
	return local var `tgt'
end

*******************************************************************************
*******************************************************************************
*******************************************************************************
*******************************************************************************
cap copy "https://drive.google.com/uc?export=download&id=1WFHpHwRMg00jLwB23dMyNMvbjS6DSHn7" data15a64_rn_ratio_its_did_downloaded.dta,replace
use data15a64_rn_ratio_its_did_downloaded.dta
*use data15a64_rn_ratio_its_did.dta

xtset year isoweek

replace did=0 if year!=2019
replace did=0 if year==2019 & isoweek<43
generate byte didf=recode(did,0,1)
drop did
gen did= didf

*The Driscoll–Kraay approach provides a specific
*variant of the Newey–West robust covariance estimator computed using the Bartlett
*kernel and a time series of scores’ cross-sectional averages.
local it `" "hosp_circ" "hosp_circ" "cons_circ" "cons_circ" "rate_circ" "rate_circ" "' 
local name `" "Trauma,Hospitalizations" "Respiratory,Hospitalizations" "Trauma,Consultations" "Respiratory,Consultations" "Trauma Hospitalizations,per Consultations (x1000)" "Respiratory Hospitalizations,per Consultations (x1000)" "' 
foreach v of varlist hosp_trauma hosp_resp ///
					 cons_trauma cons_resp ///
					 rate rate_resp {
    gettoken item it : it
	gettoken nam name : name
	xtscc  `v' i.did `item' i.month, fe 
	estimates store `v'_fe_model
	outreg2 using _DiD_fe, dec(2) ///
	excel e(F rss ll lag rmse p) ///
	title(Panel Estimation: Driscoll-Kraay standard errors and controlling for Circulatory Hospitalizations Consultations or Rates) ///
	stats(beta coef ci) cfmt(1) ///
	append ctitle("`nam'") nocons ///
	keep(i.did) 
}

*Another way to obtain results
esttab hosp_trauma_fe_model hosp_resp_fe_model cons_trauma_fe_model cons_resp_fe_model rate_fe_model rate_resp_fe_model ///
using fe_results.csv, append wide varlabels(1.did "Social Protest") keep(1.did) nobaselevels  ///
	 stats(N N_clust r2, fmt(%9.0f %9.0f %4.3f)) ///
	 cells(b(fmt(%7.4f) star label(Coef)) p(fmt(%7.4f) label(p-values))) ///
	 mtitles("Trauma Hospitalizations" "Respiratory Hospitalizations" "Trauma Consultations" "Respiratory Consultations" "Trauma Hospitalizations per Consultations (x1000)" "Respiratory Hospitalizations per Consultations (x1000)") ///
	 title(Panel Estimation: Driscoll-Kraay standard errors and controlling for Circulatory Hospitalizations Consultations or Rates) ///
	 compress

*Boussiala, Mohamed. (2020). The Stata command all commands concerning fixed and random effect. 10.13140/RG.2.2.27371.16162. 
*This will generate the individual effects (IE) that can be viewed in the Data Editor (Edit). Note
*that we can give any name , instead of IE, for example, the command 
*https://mpra.ub.uni-muenchen.de/76869/1/MPRA_paper_76869.pdf
local it `" "hosp_circ" "hosp_circ" "cons_circ" "cons_circ" "rate_circ" "rate_circ" "' 
local name `" "Trauma,Hospitalizations" "Respiratory,Hospitalizations" "Trauma,Consultations" "Respiratory,Consultations" "Trauma Hospitalizations,per Consultations (x1000)" "Respiratory Hospitalizations,per Consultations (x1000)" "' 
foreach v of varlist hosp_trauma hosp_resp ///
					 cons_trauma cons_resp ///
					 rate rate_resp {
    gettoken item it : it
	gettoken nam name : name
	xtscc  `v' i.did `item', fe 
	predict fe_`v'
}
local it `" "hosp_circ" "hosp_circ" "cons_circ" "cons_circ" "rate_circ" "rate_circ" "' 
local name `" "Trauma,Hospitalizations" "Respiratory,Hospitalizations" "Trauma,Consultations" "Respiratory,Consultations" "Trauma Hospitalizations,per Consultations (x1000)" "Respiratory Hospitalizations,per Consultations (x1000)" "' 
foreach v of varlist hosp_trauma hosp_resp ///
					 cons_trauma cons_resp ///
					 rate rate_resp {
    gettoken item it : it
	gettoken nam name : name
	xtscc  `v' i.did `item' i.month, fe 
	predict fe_month_`v'
}
*options ,u o ,xbu not allowed

browse did hosp_trauma fe4_hosp_trauma hosp_resp fe4_hosp_resp cons_trauma fe4_cons_trauma cons_resp fe4_cons_resp rate fe4_rate rate_resp fe4_rate_resp

*the value of consultation or hospitalizations on each value DID (including the omitted value), adjusted for the circulatory hospitalizatons/consultations/rate. 
local it `" "hosp_circ" "hosp_circ" "cons_circ" "cons_circ" "rate_circ" "rate_circ" "' 
local name `" "Trauma,Hospitalizations" "Respiratory,Hospitalizations" "Trauma,Consultations" "Respiratory,Consultations" "Trauma Hospitalizations,per Consultations (x1000)" "Respiratory Hospitalizations,per Consultations (x1000)" "' 
foreach v of varlist hosp_trauma hosp_resp ///
					 cons_trauma cons_resp ///
					 rate rate_resp {
    gettoken item it : it
	gettoken nam name : name
	xtscc  `v' i.tx##i.txtime `item' i.month, fe
	margins i.tx##i.txtime
}
*margins i.did
*nlcom (ratio1: -_b[1.did]/_b[0.did])

tabstat hosp_trauma hosp_resp ///
					 cons_trauma cons_resp ///
					 rate rate_resp, by(did) statistics(mean sd count)


*******************************************************************************
*******************************************************************************
*PLOT

cap clonevar _isoweek = isoweek
replace _isoweek = cond(tx==0, _isoweek - 0.1, _isoweek + 0.0)
local it `" "Trauma,Hospitalizations" "Respiratory,Hospitalizations" "Trauma,Consultations" "Respiratory,Consultations" "Trauma Hospitalizations,per Consultations (x1000)" "Respiratory Hospitalizations,per Consultations (x1000)" "' 
foreach var of varlist hosp_trauma hosp_resp ///
					 cons_trauma cons_resp ///
					 rate rate_resp {
	gettoken item it : it
	cap egen `var'_mean = mean(`var'), by(isoweek tx)
	cap egen `var'_se = semean(`var'), by(isoweek tx)
	cap gen lbci`var' = `var'_mean - (`var'_se * 1.96)
	cap gen ubci`var' = `var'_mean + (`var'_se * 1.96)
twoway (rcap lbci`var' ubci`var' _isoweek if tx==0, lcolor(gs7) msize(*.7)) ///
	(rcap lbci`var' ubci`var' _isoweek if tx==1, lcolor(black) msize(*.7)) ///
	(connect `var'_mean _isoweek if tx==0, sort lpattern("-") lcolor(gs7) msymbol(O) msize(medsmall) mcolor(gs7)) ///   
	(connect `var'_mean _isoweek if tx==1, sort lcolor(black) msymbol(O) msize(medsmall) mcolor(black)), ///
	 xtitle("Week No. (ISO 8601)", size(medsmall)) ///
	xlabel(1(4)52) ///
	tline(42, lpattern("_") lwidth(1) lcolor(red*0.15)) ///
	ytitle("", size(small)) scheme(sj) graphregion(color(white)) ///
	legend(pos(1) ring(0) col(1) symysize(zero) keygap(1) symxsize(large) order( 3 4) lab(3 "2015-2018") lab(4 "2019") size(small)) ///
	title("Comparison of Means and SEs `item'", size(small)) name(gmdns_`var', replace) 
drop `var'_mean `var'_se lbci`var' ubci`var'
	graph export "_Comp_means_cov_`var'_BOTH.pdf", as(pdf) replace
	graph export "_Comp_means_cov_`var'_BOTH.emf", as(emf) replace
}
*note("{it:Note. Other states: FL (12) & WA(53)}",size(small)) ///

 line hosp_trauma fe4_hosp_trauma  rn, yaxis(1 2) xaxis(1 2)
 
  line hosp_trauma fe_hosp_trauma fe4_hosp_trauma  rn, yaxis(1 2) xaxis(1 2)
