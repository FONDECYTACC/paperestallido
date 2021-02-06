clear all

*bajar fuentes de datos, 52 semanas año outcome(value) y control
copy "https://drive.google.com/uc?export=download&id=1VQG3DaA3LG5GmB7H4f6aQ3pc3QzUzkpY" cons_trauma_downloaded.dta,replace
copy "https://drive.google.com/uc?export=download&id=1VOowJ8qAj2MyLNGJ88TDhisMvwkMoyac" cons_resp_downloaded.dta,replace
copy "https://drive.google.com/uc?export=download&id=1Vb3_RW5XcMXCSaqnymlJStPlOLI6OXAm" hosp_trauma_downloaded.dta,replace
copy "https://drive.google.com/uc?export=download&id=1VaakgLdi6JW00OwXbM0civO8PkeBGhuL" hosp_resp_downloaded.dta,replace
copy "https://drive.google.com/uc?export=download&id=1VFDidWUmGB5ACkDfwDC021V0nkUeSWjV" trauma_rate_downloaded.dta,replace
copy "https://drive.google.com/uc?export=download&id=1VCH7uxfQsrAr1bzBEGOAB2r-Yv06vRFa" resp_rate_downloaded.dta,replace

*elegir el outcome a analizar
*use hosp_trauma_downloaded.dta, clear
*use cons_trauma_downloaded.dta, clear
*use trauma_rate_downloaded.dta, clear
*use hosp_resp_downloaded.dta, clear
*use cons_resp_downloaded.dta, clear
*use resp_rate_downloaded.dta, clear


*generar formato long
cap reshape long ht_ hosp_circ, i(isoweek) j(year)
cap reshape long ct_ cons_circ, i(isoweek) j(year)
cap reshape long rt_ rate_circ, i(isoweek) j(year)
cap reshape long rt_ hosp_circ, i(isoweek) j(year)
cap reshape long cr_ cons_circ, i(isoweek) j(year)
cap reshape long rr_ rate_circ, i(isoweek) j(year)

*settear base de datos para análisis
sort year isoweek
xtset year isoweek
replace did=0 if year!=2019

*etiquetar variables
cap label variable ht_ "Trauma Hospitalizations"
cap label variable ct_ "Trauma Consultations"
cap label variable rt_ "Trauma Hospitalizations per Consultations (x 1,000)"
cap label variable hr_ "Respiratory Hospitalizations"
cap label variable cr_ "Respiratory Consultations"
cap label variable rr_ "Respiratory Hospitalizations per Consultations (x 1,000)"
cap label variable hosp_circ "Circulatory Hospitalizations"
cap label variable cons_circ "Circulatory Consultations"
cap label variable rate_circ "Circulatory Hospitalizations per Consultations (x 1,000)"
label variable year "Year"

*renombrar variables para estandarizar variables
cap rename ht_ value
cap rename ct_ value
cap rename rt_ value
cap rename hr_ value
cap rename cr_ value
cap rename rr_ value
cap rename hosp_circ control
cap rename cons_circ control
cap rename rate_circ control

******ITSA
cap ssc install itsa
*itsa value control, single treat(2019) trperiod(43 52) replace prais rhotype(tscorr) vce(robust) figure posttrend prefix(value)
itsa value control, treat(2019) trperiod(43) lag(1) replace figure posttrend contid(2015 2016 2017 2018)
*https://journals.sagepub.com/doi/pdf/10.1177/1536867X1501500208
* _x_t43

***DiD FE
cap scc install xtscc

xtreg value did i.isoweek, fe //*cluster(year)
est store did_fe
xtreg value did i.isoweek, fe cluster(year)
est store did_fe_clus
xtscc value did i.isoweek, fe 
est store did_xtscc

xtreg value did control i.isoweek, fe //*cluster(year)
est store did_control_fe
xtreg value did control i.isoweek, fe cluster(year)
est store did_control_fe_clus
xtscc value did control i.isoweek, fe 
est store did_control_xtscc
est table *, b se t p


****Synth
synth value control, trunit(2019) trperiod(43) figure keep(`v') replace
