clear all 

cap ssc install outreg2c
cap install xtscc
cap ssc install coefplot
cap ssc install xttest3
cap ssc install xttest2
cap ssc install xtcsd
cap ssc install estout

******************
******************

cap copy "https://github.com/FONDECYTACC/paperestallido/blob/main/data15a64_rn_ratio_its.dta?raw=true" data15a64_rn_ratio_its_did.dta,replace
use data15a64_rn_ratio_its_did.dta

gen year2 = real(year)
drop year
rename year2 year

gen month2 = real(month)
drop month
rename month2 month

xtset year isoweek

replace did=0 if year!=2019
replace did=0 if year==2019 & isoweek<43
generate byte didf=recode(did,0,1)
drop did
gen did= didf

*____________________________________________________________________________*
***GENERAR XTSCC FINAL
*____________________________________________________________________________*
*the value of consultation or hospitalizations on each value DID (including the omitted value), adjusted for the circulatory hospitalizatons/consultations/rate. 
*
*Because graphical evidence suggests
*a periodic behavior, the analysis includes the sin1 and cos1 variables, which are sine and cosine
*transformations of scaled time, respectively.

*add a fixed seasonality component based on the cosine of the season (month of year) scaled to the range (0, 2π) 
cap gen month_sin = sin(month*2*c(pi)/12)
cap gen month_cos = cos(month*2*c(pi)/12) //*(month*2*c(pi)



cap drop fe_*
cap drop xtscc_*
cap drop mar_xtscc_*

*local it `" "hosp_circ" "hosp_circ" "cons_circ" "cons_circ" "rate_circ" "rate_circ" "' 
local name `" "Trauma,Hospitalizations" "Respiratory,Hospitalizations" "Trauma,Consultations" "Respiratory,Consultations" "Trauma Hospitalizations,per Consultations (x1000)" "Respiratory Hospitalizations,per Consultations (x1000)" "' 
foreach v of varlist hosp_trauma hosp_resp ///
                     cons_trauma cons_resp ///
                     rate rate_resp {
    gettoken item it : it
    gettoken nam name : name
    qui xtscc  `v' i.tx##i.txtime, fe 
    estimates store xtscc_`v'_n
    qui margins i.tx##i.txtime, post
    eststo mar_xtscc_`v'_n
    estimates restore xtscc_`v'_n
    predict fe_no_month_`v'
    qui xtscc  `v' i.tx##i.txtime c.month, fe 
    estimates store xtscc_`v'_ct_m
    qui margins i.tx##i.txtime, post
    eststo mar_xtscc_`v'_ct_m
    estimates restore xtscc_`v'_ct_m
    predict fe_cont_month_`v'
    qui xtscc  `v' i.tx##i.txtime i.month, fe 
    estimates store xtscc_`v'_mth
    qui margins i.tx##i.txtime, post
    eststo mar_xtscc_`v'_mth
    estimates restore xtscc_`v'_mth
    predict fe_month_`v' 
    eststo mar_xtscc_`v'_mth
    qui xtscc  `v' i.tx##i.txtime c.month#c.month, fe  
    estimates store xtscc_`v'_cd_m
    qui margins i.tx##i.txtime, post
    eststo mar_xtscc_`v'_cd_m
    estimates restore xtscc_`v'_cd_m
    predict fe_cuad_`v'
    qui xtscc  `v' i.tx##i.txtime month_cos month_sin, fe 
    estimates store xtscc_`v'_sc
    qui margins i.tx##i.txtime, post
    eststo mar_xtscc_`v'_sc
    estimates restore xtscc_`v'_sc
    predict fe_sin_cos_`v'
}

*matrix list r(table) ** para ver todos los términsos
*Another way to obtain results
cap erase fe_results.csv
esttab xtscc_* ///
using fe_results.csv, append varlabels(1.txtime "Social Protest") keep(1.txtime) nobaselevels  ///
     stats(N N_clust r2, fmt(%9.0f %9.0f %4.3f)) ///
     cells("b(star fmt(3) label(Coef)) ci_l(fmt(2) label(CI95_Lo)) ci_u(fmt(2) label(CI95_Up)) p(fmt(%7.3f) label(p-values))") ///
     mtitles("Trauma Hospitalizations-None" "Trauma Hospitalizations-Continuous" "Trauma Hospitalizations-Factor" "Trauma Hospitalizations-Cuadratic" "Trauma Hospitalizations-SinCos" /// 
            "Respiratory Hospitalizations-None" "Respiratory Hospitalizations-Continuous" "Respiratory Hospitalizations-Factor" "Respiratory Hospitalizations-Cuadratic" "Respiratory Hospitalizations-SinCos" ///
            "Trauma Consultations-None" "Trauma Consultations-Continuous" "Trauma Consultations-Factor" "Trauma Consultations-Cuadratic" "Trauma Consultations-SinCos" ///
            "Respiratory Consultations-None" "Respiratory Consultations-Continuous" "Respiratory Consultations-Factor" "Respiratory Consultations-Cuadratic" "Respiratory Consultations-SinCos" ///
            "Trauma Hospitalizations per Trauma Consultations(x1000)-None" "Trauma Hospitalizations per Trauma Consultations(x1000)-Continuous" "Trauma Hospitalizations per Trauma Consultations(x1000)-Factor" "Trauma Hospitalizations per Trauma Consultations(x1000)-Cuadratic" "Trauma Hospitalizations per Trauma Consultations(x1000)-SinCos" ///
            "Respiratory Hospitalizations per Respiratory Consultations(x1000)-None" "Respiratory Hospitalizations per Respiratory Consultations(x1000)-Continuous" "Respiratory Hospitalizations per Respiratory Consultations(x1000)-Factor" "Respiratory Hospitalizations per Respiratory Consultations(x1000)-Cuadratic" "Respiratory Hospitalizations per Respiratory Consultations(x1000)-SinCos") ///
            legend label varwidth(25) ///
     title(Panel Estimation: Driscoll-Kraay standard errors) ///
     compress
	 
*******************************************************************************
**** XTREG LET ME ESTIMATE AIC & BIC, SHOULD NOT DIFFER WITH XTSCC
*******************************************************************************


local name `" "Trauma,Hospitalizations" "Respiratory,Hospitalizations" "Trauma,Consultations" "Respiratory,Consultations" "Trauma Hospitalizations,per Consultations (x1000)" "Respiratory Hospitalizations,per Consultations (x1000)" "' 
foreach v of varlist hosp_trauma hosp_resp ///
					 cons_trauma cons_resp ///
					 rate rate_resp {
	gettoken item it : it
	gettoken nam name : name
	qui xtreg `v' i.tx##i.txtime, fe 
	estimates store xtreg_`v'_none
	qui xtreg `v' i.tx##i.txtime c.month, fe 
	estimates store xtreg_`v'_cont				 
	qui xtreg `v' i.tx##i.txtime i.month, fe 
	estimates store xtreg_`v'_fact
	qui xtreg `v' i.tx##i.txtime c.month#c.month, fe 
	estimates store xtreg_`v'_cuad
	qui xtreg `v' i.tx##i.txtime month_sin month_cos, fe 
	estimates store xtreg_`v'_sin_cos
}
*****************************************v**************************************
*****************************************v**************************************
***********TABLES************************v**************************************

qui est table xtreg_hosp_trauma_*, star b(%7.4f) stats(aic bic rmse) keep(1.txtime)
qui return list
matrix stats_a = r(stats)'
matrix rownames stats_a = "TH-None" "TH-Cont" "TH-Fact" "TH-Cuad" "TH-SinCos"
svmat stats_a 
qui est table xtreg_hosp_resp_*, star b(%7.4f) stats(aic bic rmse) keep(1.txtime)
qui return list
matrix stats_b = r(stats)'
matrix rownames stats_b = "RH-None" "RH-Cont" "RH-Fact" "RH-Cuad" "RH-SinCos"
svmat stats_b
qui est table xtreg_cons_trauma_*, star b(%7.4f) stats(aic bic rmse) keep(1.txtime)
qui return list
matrix stats_c = r(stats)'
matrix rownames stats_c = "TC-None" "TC-Cont" "TC-Fact" "TC-Cuad" "TC-SinCos"
svmat stats_c
qui est table xtreg_cons_resp_*, star b(%7.4f) stats(aic bic rmse) keep(1.txtime)
qui return list
matrix stats_d = r(stats)'
matrix rownames stats_d = "RC-None" "RC-Cont" "RC-Fact" "RC-Cuad" "RC-SinCos"
svmat stats_d
qui est table xtreg_rate_none xtreg_rate_cont xtreg_rate_fact xtreg_rate_cuad xtreg_rate_sin_cos, star b(%7.4f) stats(aic bic rmse) keep(1.txtime)
qui return list
matrix stats_e = r(stats)'
matrix rownames stats_e = "TR-None" "TR-Cont" "TR-Fact" "TR-Cuad" "TR-SinCos"
svmat stats_e
qui est table xtreg_rate_resp_*, star b(%7.4f) stats(aic bic rmse) keep(1.txtime)
qui return list
matrix stats_f = r(stats)'
matrix rownames stats_f = "RR-None" "RR-Cont" "RR-Fact" "RR-Cuad" "RR-SinCos"
svmat stats_f 

matrix comb = (stats_a \ stats_b \ stats_c \ stats_d \ stats_e \ stats_f)

esttab matrix(comb) ///
using fe_results_fit.csv


*****************************************v**************************************
*****************************************v**************************************
*****************************************v**************************************
* TH-SinCos  RH-Fact  TC-Fact RC-SinCos  TR-Fact RR-Fact


*if((r(table)[1,4]/r(table)[1,9])<1) {
*	local: di %9.2f (1-(r(table)[1,4]/r(table)[1,9]))*100
*	}
*else if((r(table)[1,4]/r(table)[1,9])>1){
*	di %9.2f ((r(table)[1,4]/r(table)[1,9])-1)*100
*	}

* TH-SinCos 
qui xtscc  hosp_trauma i.tx##i.txtime month_cos month_sin, fe 
qui margins,eydx(txtime) atmeans

scalar beti_TH_SinCos = r(table)[1,2]*100
scalar beti_TH_SinCos = round(`=scalar(beti_TH_SinCos)', .01)
scalar CI95_lo_TH_SinCos = r(table)[5,2]*100
scalar CI95_lo_TH_SinCos = round(`=scalar(CI95_lo_TH_SinCos)', .01)
scalar CI95_up_TH_SinCos = r(table)[6,2]*100
scalar CI95_up_TH_SinCos = round(`=scalar(CI95_up_TH_SinCos)', .01)

*  RH_Fact
qui xtscc  hosp_resp i.tx##i.txtime i.month, fe 
qui margins,eydx(txtime) atmeans

scalar beti_RH_Fact = r(table)[1,2]*100
scalar beti_RH_Fact = round(`=scalar(beti_RH_Fact)', .01)
scalar CI95_lo_RH_Fact = r(table)[5,2]*100
scalar CI95_lo_RH_Fact = round(`=scalar(CI95_lo_RH_Fact)', .01)
scalar CI95_up_RH_Fact = r(table)[6,2]*100
scalar CI95_up_RH_Fact = round(`=scalar(CI95_up_RH_Fact)', .01)

* TC_Fact
qui xtscc  cons_trauma i.tx##i.txtime i.month, fe 
qui margins,eydx(txtime) atmeans

scalar beti_TC_Fact = r(table)[1,2]*100
scalar beti_TC_Fact = round(`=scalar(beti_TC_Fact)', .01)
scalar CI95_lo_TC_Fact = r(table)[5,2]*100
scalar CI95_lo_TC_Fact = round(`=scalar(CI95_lo_TC_Fact)', .01)
scalar CI95_up_TC_Fact = r(table)[6,2]*100
scalar CI95_up_TC_Fact = round(`=scalar(CI95_up_TC_Fact)', .01)

* RC_SinCos
qui xtscc  cons_resp i.tx##i.txtime month_cos month_sin, fe 
qui margins,eydx(txtime) atmeans

scalar beti_RC_SinCos = r(table)[1,2]*100
scalar beti_RC_SinCos = round(`=scalar(beti_RC_SinCos)', .01)
scalar CI95_lo_RC_SinCos = r(table)[5,2]*100
scalar CI95_lo_RC_SinCos = round(`=scalar(CI95_lo_RC_SinCos)', .01)
scalar CI95_up_RC_SinCos = r(table)[6,2]*100
scalar CI95_up_RC_SinCos = round(`=scalar(CI95_up_RC_SinCos)', .01)

* TR_Fact
qui xtscc  rate i.tx##i.txtime i.month, fe 
qui margins,eydx(txtime) atmeans

scalar beti_TR_Fact = r(table)[1,2]*100
scalar beti_TR_Fact = round(`=scalar(beti_TR_Fact)', .01)
scalar CI95_lo_TR_Fact = r(table)[5,2]*100
scalar CI95_lo_TR_Fact = round(`=scalar(CI95_lo_TR_Fact)', .01)
scalar CI95_up_TR_Fact = r(table)[6,2]*100
scalar CI95_up_TR_Fact = round(`=scalar(CI95_up_TR_Fact)', .01)

* RR_Fact
qui xtscc  rate_resp i.tx##i.txtime i.month, fe 
qui margins,eydx(txtime) atmeans

scalar beti_RR_Fact = r(table)[1,2]*100
scalar beti_RR_Fact = round(`=scalar(beti_RR_Fact)', .01)
scalar CI95_lo_RR_Fact = r(table)[5,2]*100
scalar CI95_lo_RR_Fact = round(`=scalar(CI95_lo_RR_Fact)', .01)
scalar CI95_up_RR_Fact = r(table)[6,2]*100
scalar CI95_up_RR_Fact = round(`=scalar(CI95_up_RR_Fact)', .01)

cap erase fe_results_selection.csv
*# https://www.statalist.org/forums/forum/general-stata-discussion/general/1437172-margins-and-diff-in-diff-estimates
*# https://stats.stackexchange.com/questions/482869/differences-between-calculating-the-relative-change-and-taking-the-natural-log-t
esttab xtscc_hosp_trauma_sc  xtscc_hosp_resp_mth  ///
xtscc_cons_trauma_mth  xtscc_cons_resp_sc xtscc_rate_mth xtscc_rate_resp_mth ///
using fe_results_selection.csv, ///
append varlabels(1.txtime "Social Protest") keep(1.txtime) nobaselevels  ///
     stats(N r2_w, fmt(%9.0f %4.2f)) ///
     cells(b(fmt(2) label(Coef)) ci_l(fmt(2) label(CI95_Lo)) ci_u(fmt(2) label(CI95_Up)) p(fmt(%7.3f) label(p-values))) ///
	 mtitles("Trauma Hospitalizations-SinCos" "Respiratory Hospitalizations-Factor" "Trauma Consultations-Factor" "Respiratory Consultations-SinCos" ///
             "Trauma Hospitalizations per Trauma Consultations(x1000)-Factor" ///
             "Respiratory Hospitalizations per Respiratory Consultations(x1000)-Factor") ///	 
     legend label ///
     title(Panel Estimation: Driscoll-Kraay standard errors) ///
     compress nogap 

esttab xtscc_hosp_trauma_sc  xtscc_hosp_resp_mth  ///
xtscc_cons_trauma_mth  xtscc_cons_resp_sc xtscc_rate_mth xtscc_rate_resp_mth, ///
varlabels(1.txtime "Social Protest") keep(1.txtime) nobaselevels  ///
     stats(N r2_w, fmt(%9.0f %4.2f)) ///
     cells(b(fmt(2) label(Coef)) ci_l(fmt(2) label(CI95_Lo)) ci_u(fmt(2) label(CI95_Up)) p(fmt(%7.3f) label(p-values))) ///
     mtitles("TH-SinCos" "RH-Factor" "TC-Factor" "RC-SinCos" ///
             "TR-Factor" "RR-Factor") ///     
     legend label ///
     title(Panel Estimation: Driscoll-Kraay standard errors) ///
     compress nogap 
	 
*#:#:#:#:#:#:##:#:#:#:#:#:#:#:#:#:#:#:#:##:#:#:#:#:#:#:#:#:#:#:#:#:##:#:#:#:#:#:#:
*#:#:#:#:#:#:##:#:#:#:#:#:#:#:#:#:#:#:#:##:#:#:#:#:#:#:#:#:#:#:#:#:##:#:#:#:#:#:#:
*#:#:#:#:#:#:##:#:#:#:#:#:#:#:#:#:#:#:#:##:#:#:#:#:#:#:#:#:#:#:#:#:##:#:#:#:#:#:#:	 
** Relative differences:

log using log.txt, replace text
di "`=scalar(beti_TH_SinCos)' [`=scalar(CI95_lo_TH_SinCos)',`=scalar(CI95_up_TH_SinCos)']" 
di "`=scalar(beti_RH_Fact)' [`=scalar(CI95_lo_RH_Fact)',`=scalar(CI95_up_RH_Fact)']" 
di "`=scalar(beti_TC_Fact)' [`=scalar(CI95_lo_TC_Fact)',`=scalar(CI95_up_TC_Fact)']" 
di "`=scalar(beti_RC_SinCos)' [`=scalar(CI95_lo_RC_SinCos)',`=scalar(CI95_up_RC_SinCos)']" 
di "`=scalar(beti_TR_Fact)' [`=scalar(CI95_lo_TR_Fact)',`=scalar(CI95_up_TR_Fact)']" 
di "`=scalar(beti_RR_Fact)' [`=scalar(CI95_lo_RR_Fact)',`=scalar(CI95_up_RR_Fact)']" 
log close 

			   
