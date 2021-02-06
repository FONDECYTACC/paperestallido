cap ssc install outreg2
cap install xtscc
cap ssc install coefplot
******************
clear all
******************
if _rc == 0 { 
	if `"`c(os)'"' == "Windows" & `"`c(username)'"' == "andre" global sf `"G:\Mi unidad\linkabbddyscriptderpaperestallidosocial"'
	cd `"${sf}"'
} 
else if _rc == 0 { 
	if `"`c(os)'"' == "Windows" & `"`c(username)'"' == "CISS Fondecyt" global sf `"G:\Mi unidad\Alvacast\CURES2_DB"'
	cd `"${sf}"'
	} 
	else if _rc == 0 { 
	if `"`c(os)'"' == "MacOSX" global sf `"/volumes/sdrive/data//"'
	cd `"${sf}"'
	}
	else if _rc == 0 { 
	mkdir "~\Stata_data" 
	} 
	else rmdir "~\Stata_data" 
 global sf `"~\Stata_data"' //if does not work
 *set working directory
 cd `"${sf}"'
 
	di `"${sf}"' 
	pwd

cap copy "https://drive.google.com/uc?export=download&id=1WFHpHwRMg00jLwB23dMyNMvbjS6DSHn7" data15a64_rn_ratio_its_did.dta,replace
use data15a64_rn_ratio_its_did.dta
*use data15a64_rn_ratio_its_did.dta

xtset year isoweek

replace did=0 if year!=2019
replace did=0 if year==2019 & isoweek<43
generate byte didf=recode(did,0,1)
drop did
gen did= didf

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


* Graficar tendencias paralelas:

local name `" "Trauma,Hospitalizations" "Respiratory,Hospitalizations" "Trauma,Consultations" "Respiratory,Consultations" "Trauma Hospitalizations,per Consultations (x1000)" "Respiratory Hospitalizations,per Consultations (x1000)" "' 
		foreach v of varlist hosp_trauma hosp_resp ///
							 cons_trauma cons_resp ///
							 rate rate_resp {
			gettoken item it : it
			gettoken nam name : name

		tabulate isoweek tx, s(`v') means
		bys isoweek: egen `v'_mean_trat = mean(`v' * (tx==1) )
		bys isoweek: egen `v'_mean_contr = mean(`v' * (tx==0) )
		g `v'_dif_tend = `v'_mean_trat - `v'_mean_contr
		label var `v'_mean_trat "`nam'"
		label var `v'_mean_contr "Average `nam'"
}

		foreach v of varlist hosp_trauma hosp_resp ///
							 cons_trauma cons_resp ///
							 rate rate_resp {
					gettoken nam name : name					 
tw scatter `v'_mean_trat `v'_mean_contr isoweek || ///
	lfit `v'_mean_trat isoweek if tx==1 & txtime==0 || ///
	lfit `v'_mean_trat isoweek if tx==1 & txtime==1  || ///
	lfit `v'_mean_contr isoweek if tx==0 & txtime==0 || ///
	lfit `v'_mean_contr isoweek if tx==0 & txtime==1 , ///
	xlabel(1(4)52) ///
	msize(vsmall) ///
	legend(region(lstyle(none)col(none)) pos(1) ring(0) col(1) symysize(zero) /// 
	keygap(1) symxsize(large) order(1 2 3 4) lab(1 "2019") lab(2 "2015-2018") lab(3 "2019 Pre Int") lab (4 "2019 Post Int") size(tiny)) ///
	ytitle("`nam'", size(tiny)) ///
	tline(42, lpattern("_") lwidth(1) lcolor(red*0.15)) ///
	graphregion(color(gs16)) 
	graph save `v'_mean_interrupt,replace
}
graph combine hosp_trauma_mean_interrupt.gph hosp_resp_mean_interrupt.gph, graphregion(color(gs16)) 
graph combine cons_trauma_mean_interrupt.gph cons_resp_mean_interrupt.gph, graphregion(color(gs16)) 
graph combine rate_mean_interrupt.gph rate_resp_mean_interrupt.gph, graphregion(color(gs16)) 

*cons_resp_mean_interrupt cons_trauma_mean_interrupt hosp_resp_mean_interrupt ** se ven raro
*******************************************************************************
*******************************************************************************
*******************************************************************************
*COMPROBAR HETEROCEDASTICIDAD, DEPENDENCIA SERIAL, AUTOCORRELACION
*******************************************************************************
*******************************************************************************
*
local it `" "hosp_circ" "hosp_circ" "cons_circ" "cons_circ" "rate_circ" "rate_circ" "' 
local name `" "Trauma,Hospitalizations" "Respiratory,Hospitalizations" "Trauma,Consultations" "Respiratory,Consultations" "Trauma Hospitalizations,per Consultations (x1000)" "Respiratory Hospitalizations,per Consultations (x1000)" "' 
foreach v of varlist hosp_trauma hosp_resp ///
					 cons_trauma cons_resp ///
					 rate rate_resp {
				xtunitroot fisher `v', dfuller trend lags(1)
				xtunitroot fisher `v', dfuller trend lags(2)
				xtunitroot fisher `v', dfuller trend lags(3)
				xtunitroot fisher `v', dfuller trend lags(4)
				*How to do:
				** Breusch-Pagan LM test **
				xtreg `v' i.tx##i.txtime hosp_circ i.month, fe
				xtcsd, pesaran abs

				** Modified Wald test **
				xtreg `v' i.tx##i.txtime hosp_circ i.month, fe
				xttest3

				** Breusch-Pagan LM test **
				xtreg `v' i.tx##i.txtime hosp_circ i.month, fe
				xttest2
}

*******************************************************************************
*******************************************************************************
*MODELO 
*******************************************************************************
*******************************************************************************

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
	xtscc  `v' i.did `item' month, fe 
	outreg2 using _DiD_fe, dec(2) ///
	excel e(F rss ll lag rmse p) ///
	title(Panel Estimation: Driscoll-Kraay standard errors and controlling for Circulatory Hospitalizations Consultations or Rates) ///
	stats(beta coef ci) cfmt(1) ///
	append ctitle("`nam'") nocons ///
	keep(i.did) 
}

local it `" "hosp_circ" "hosp_circ" "cons_circ" "cons_circ" "rate_circ" "rate_circ" "' 
local name `" "Trauma,Hospitalizations" "Respiratory,Hospitalizations" "Trauma,Consultations" "Respiratory,Consultations" "Trauma Hospitalizations,per Consultations (x1000)" "Respiratory Hospitalizations,per Consultations (x1000)" "' 
foreach v of varlist hosp_trauma hosp_resp ///
					 cons_trauma cons_resp ///
					 rate rate_resp {
    gettoken item it : it
	gettoken nam name : name
	xtscc  `v' i.did `item' i.month, fe 
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

*options ,u o ,xbu not allowed

*browse did hosp_trauma fe_hosp_trauma fe_month_hosp_trauma ///
			*hosp_resp fe_hosp_resp fe_month_hosp_resp ///
			*cons_trauma fe_cons_trauma fe_month_cons_trauma ///
			*cons_resp fe_cons_resp fe_month_cons_resp ///
			*rate fe_rate fe_month_rate ///
			*rate_resp fe_rate_resp fe_month_rate_resp

*the value of consultation or hospitalizations on each value DID (including the omitted value), adjusted for the circulatory hospitalizatons/consultations/rate. 
cap drop fe_*
local it `" "hosp_circ" "hosp_circ" "cons_circ" "cons_circ" "rate_circ" "rate_circ" "' 
local name `" "Trauma,Hospitalizations" "Respiratory,Hospitalizations" "Trauma,Consultations" "Respiratory,Consultations" "Trauma Hospitalizations,per Consultations (x1000)" "Respiratory Hospitalizations,per Consultations (x1000)" "' 
foreach v of varlist hosp_trauma hosp_resp ///
					 cons_trauma cons_resp ///
					 rate rate_resp {
    gettoken item it : it
	gettoken nam name : name
	xtscc  `v' i.tx##i.txtime `item', fe
	estimates store xtscc_`v'_none
	margins i.tx##i.txtime
	predict fe_no_month_`v'
	xtscc  `v' i.tx##i.txtime `item' c.month, fe
	estimates store xtscc_`v'_cnt_mth
	margins i.tx##i.txtime
	predict fe_cont_month_`v'
	xtscc  `v' i.tx##i.txtime `item' i.month, fe
	estimates store xtscc_`v'_month
	predict fe_month_`v'
	margins i.tx##i.txtime
	xtscc  `v' i.tx##i.txtime `item' c.month#c.month, fe
	estimates store xtscc_`v'_cd_mth
	margins i.tx##i.txtime
	predict fe_cuad_`v'
}
*matrix list r(table) ** para ver todos los términsos


local it `" "hosp_circ" "hosp_circ" "cons_circ" "cons_circ" "rate_circ" "rate_circ" "' 
local name `" "Trauma,Hospitalizations" "Respiratory,Hospitalizations" "Trauma,Consultations" "Respiratory,Consultations" "Trauma Hospitalizations,per Consultations (x1000)" "Respiratory Hospitalizations,per Consultations (x1000)" "' 
foreach v of varlist hosp_trauma hosp_resp ///
					 cons_trauma cons_resp ///
					 rate rate_resp {
	gettoken item it : it
	gettoken nam name : name
	qui xtreg `v' i.tx##i.txtime `item', fe
	estimates store xtreg_`v'_none
	qui xtreg `v' i.tx##i.txtime `item' c.month, fe
	estimates store xtreg_`v'_cont				 
	qui xtreg `v' i.tx##i.txtime `item' i.month, fe
	estimates store xtreg_`v'_fact
	qui xtreg `v' i.tx##i.txtime `item' c.month#c.month, fe
	estimates store xtreg_`v'_cuad
}
*****************************************
*****************************************
***********TABLES************************
est table xtreg_hosp_trauma_*, star b(%7.4f) stats(N r2_a aic bic rmse) keep(1.txtime)
return list
matrix stats = r(stats)'
matrix list stats
est table xtreg_hosp_resp_*, star b(%7.4f) stats(N r2_a aic bic rmse) keep(1.txtime)
return list
matrix stats = r(stats)'
matrix list stats
est table xtreg_cons_trauma_*, star b(%7.4f) stats(N r2_a aic bic rmse) keep(1.txtime)
return list
matrix stats = r(stats)'
matrix list stats
est table xtreg_cons_resp_*, star b(%7.4f) stats(N r2_a aic bic rmse) keep(1.txtime)
return list
matrix stats = r(stats)'
matrix list stats
est table xtreg_rate_resp xtreg_rate_none xtreg_rate_cont xtreg_rate_fact xtreg_rate_cuad , star b(%7.4f) stats(N r2_a aic bic rmse) keep(1.txtime)
return list
matrix stats = r(stats)'
matrix list stats
est table xtreg_rate_resp_*, star b(%7.4f) stats(N r2_a aic bic rmse) keep(1.txtime)
return list
matrix stats = r(stats)'
matrix list stats
*margins i.did
*nlcom (ratio1: -_b[1.did]/_b[0.did])
*****************************************
*****************************************
*****************************************

*PLOT OF LINEAR TRENDS
xtline hosp_trauma hosp_resp if year==2019,overlay graphregion(color(gs16)) 
xtline cons_trauma cons_resp if year==2019,graphregion(color(gs16)) 
xtline rate rate_resp if year==2019,graphregion(color(gs16)) 

*SPECIFIC PLOTS
line hosp_trauma fe_no_month_hosp_trauma fe_cont_month_hosp_trauma fe_month_hosp_trauma fe_cuad_hosp_trauma isoweek if year==2019, ///
legend(region(lstyle(none)col(none)) pos(4) ring(0) col(1) symysize(zero) /// 
keygap(1) symxsize(large) order(1 2 3 4 5) lab(1 "Actual") lab(2 "FE") lab(3 "FE +Month Cont") lab(4 "FE +Month Fact") lab(5 "FE +Month Cuad") size(vsmall)) ///
xtitle("Week No. (ISO 8601)", size(small)) ///
ytitle("Trauma Hospitalizations", size(medsmall)) ///
xlabel(1(4)52) ///
tline(42, lpattern("_") lwidth(1) lcolor(red*0.15)) ///
graphregion(color(gs16)) 

line hosp_resp fe_no_month_hosp_resp fe_cont_month_hosp_resp fe_month_hosp_resp fe_cuad_hosp_resp isoweek if year==2019, ///
legend(region(lstyle(none)col(none)) pos(2) ring(0) col(1) symysize(zero) /// 
keygap(1) symxsize(large) order(1 2 3 4 5) lab(1 "Actual") lab(2 "FE") lab(3 "FE +Month Cont") lab(4 "FE +Month Fact") lab(5 "FE +Month Cuad") size(vsmall)) ///
xtitle("Week No. (ISO 8601)", size(small)) ///
ytitle("Respiratory Hospitalizations", size(medsmall)) ///
xlabel(1(4)52) ///
tline(42, lpattern("_") lwidth(1) lcolor(red*0.15)) ///
graphregion(color(gs16)) 

line cons_trauma fe_no_month_cons_trauma fe_cont_month_cons_trauma fe_month_cons_trauma fe_cuad_cons_trauma isoweek if year==2019, ///
legend(region(lstyle(none)col(none)) pos(4) ring(0) col(1) symysize(zero) /// 
keygap(1) symxsize(large) order(1 2 3 4 5) lab(1 "Actual") lab(2 "FE") lab(3 "FE +Month Cont") lab(4 "FE +Month Fact") lab(5 "FE +Month Cuad") size(vsmall)) ///
xtitle("Week No. (ISO 8601)", size(small)) ///
ytitle("Trauma Consultations", size(medsmall)) ///
xlabel(1(4)52) ///
tline(42, lpattern("_") lwidth(1) lcolor(red*0.15)) ///
graphregion(color(gs16)) 

line cons_resp fe_no_month_cons_resp fe_cont_month_cons_resp fe_month_cons_resp fe_cuad_cons_resp isoweek if year==2019, ///
legend(region(lstyle(none)col(none)) pos(2) ring(0) col(1) symysize(zero) /// 
keygap(1) symxsize(large) order(1 2 3 4 5) lab(1 "Actual") lab(2 "FE") lab(3 "FE +Month Cont") lab(4 "FE +Month Fact") lab(5 "FE +Month Cuad") size(vsmall)) ///
xtitle("Week No. (ISO 8601)", size(small)) ///
ytitle("Respiratory Consultations", size(medsmall)) ///
xlabel(1(4)52) ///
tline(42, lpattern("_") lwidth(1) lcolor(red*0.15)) ///
graphregion(color(gs16)) 

line rate fe_no_month_rate fe_cont_month_rate fe_month_rate fe_cuad_rate isoweek if year==2019, ///
legend(region(lstyle(none)col(none)) pos(4) ring(0) col(1) symysize(zero) /// 
keygap(1) symxsize(large) order(1 2 3 4 5) lab(1 "Actual") lab(2 "FE") lab(3 "FE +Month Cont") lab(4 "FE +Month Fact") lab(5 "FE +Month Cuad") size(vsmall)) ///
xtitle("Week No. (ISO 8601)", size(small)) ///
ytitle("Trauma Hospitalizations per Consultations (x1000)", size(small)) ///
xlabel(1(4)52) ///
tline(42, lpattern("_") lwidth(1) lcolor(red*0.15)) ///
graphregion(color(gs16)) 

line rate_resp fe_no_month_rate_resp fe_cont_month_rate_resp fe_month_rate_resp fe_cuad_rate_resp isoweek if year==2019, ///
legend(region(lstyle(none)col(none)) pos(4) ring(0) col(1) symysize(zero) /// 
keygap(1) symxsize(large) order(1 2 3 4 5) lab(1 "Actual") lab(2 "FE") lab(3 "FE +Month Cont") lab(4 "FE +Month Fact") lab(5 "FE +Month Cuad") size(vsmall)) ///
xtitle("Week No. (ISO 8601)", size(small)) ///
ytitle("Respiratory Hospitalizations per Consultations (x1000)", size(small)) ///
xlabel(1(4)52) ///
tline(42, lpattern("_") lwidth(1) lcolor(red*0.15)) ///
graphregion(color(gs16)) 

*******************************************************************************
*******************************************************************************
**Coefficients
*est table xtscc_hosp_trauma_none, star b(%7.4f) stats(N r2 r2_a aic bic rmse) keep(1.txtime)

coefplot (xtscc_hosp_trauma_none, asequation \ xtscc_hosp_trauma_cnt_mth, asequation \ xtscc_hosp_trauma_month, asequation \ xtscc_hosp_trauma_cd_mth), ///
eqlabels("None" "Continuous" "Factor" "Cuadratic", asheadings) ///
graphregion(color(gs16)) ///
 mlabsize(small) ///
 xlabel(-10(5)30,labsize(small)) ///
 ylabel(,labsize(small)) ///
 legend(off) ///
 keep(1.txtime) ///
 xline(0) ///
 xtitle("Trauma Hospitalizations", size(small))
  
 coefplot (xtscc_hosp_resp_none, asequation \ xtscc_hosp_resp_cnt_mth, asequation \ xtscc_hosp_resp_month, asequation \ xtscc_hosp_resp_cd_mth), ///
eqlabels("None" "Continuous" "Factor" "Cuadratic", asheadings) ///
graphregion(color(gs16)) ///
 mlabsize(small) ///
 xlabel(-15(3)15,labsize(small)) ///
 ylabel(,labsize(small)) ///
 legend(off) ///
 keep(1.txtime) ///
 xline(0) ///
 xtitle("Respiratory Hospitalizations", size(small))
 
coefplot (xtscc_cons_trauma_none, asequation \ xtscc_cons_trauma_cnt_mth, asequation \ xtscc_cons_trauma_month, asequation \ xtscc_cons_trauma_cd_mth), ///
eqlabels("None" "Continuous" "Factor" "Cuadratic", asheadings) ///
graphregion(color(gs16)) ///
 mlabsize(small) ///
 xlabel(-250(50)50,labsize(small)) ///
 ylabel(,labsize(small)) ///
 legend(off) ///
 keep(1.txtime) ///
 xline(0) ///
 xtitle("Trauma Consultations", size(small))
	
coefplot (xtscc_cons_resp_none, asequation \ xtscc_cons_resp_cnt_mth, asequation \ xtscc_cons_resp_month, asequation \ xtscc_cons_resp_cd_mth), ///
eqlabels("None" "Continuous" "Factor" "Cuadratic", asheadings) ///
graphregion(color(gs16)) ///
 mlabsize(small) ///
 xlabel(-150(25)0,labsize(small)) ///
 ylabel(,labsize(small)) ///
 legend(off) ///
 keep(1.txtime) ///
 xline(0) ///
 xtitle("Respiratory Consultations", size(small))
 
 coefplot (xtscc_rate_none, asequation \ xtscc_rate_cnt_mth, asequation \ xtscc_rate_month, asequation \ xtscc_rate_cd_mth), ///
eqlabels("None" "Continuous" "Factor" "Cuadratic", asheadings) ///
graphregion(color(gs16)) ///
 mlabsize(small) ///
 xlabel(0(5)40,labsize(small)) ///
 ylabel(,labsize(small)) ///
 legend(off) ///
 keep(1.txtime) ///
 xline(0) ///
 xtitle("Rate of Trauma Hospitalizations per Consultations (x1,000)", size(vsmall))

coefplot (xtscc_rate_resp_none, asequation \ xtscc_rate_resp_cnt_mth, asequation \ xtscc_rate_resp_month, asequation \ xtscc_rate_resp_cd_mth), ///
eqlabels("None" "Continuous" "Factor" "Cuadratic", asheadings) ///
graphregion(color(gs16)) ///
 mlabsize(small) ///
 xlabel(0(25)125,labsize(small)) ///
 ylabel(,labsize(small)) ///
 legend(off) ///
 keep(1.txtime) ///
 xline(0) ///
 xtitle("Rate of Respiratory Hospitalizations per Consultations(x1,000)", size(vsmall))
