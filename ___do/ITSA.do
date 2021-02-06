use "G:\Mi unidad\linkabbddyscriptderpaperestallidosocial\trauma_hosp_itsa.dta", clear
use "G:\Mi unidad\linkabbddyscriptderpaperestallidosocial\trauma_cons_itsa.dta", clear
use "G:\Mi unidad\linkabbddyscriptderpaperestallidosocial\trauma_rate_itsa.dta", clear
use "G:\Mi unidad\linkabbddyscriptderpaperestallidosocial\resp_hosp_itsa.dta", clear
use "G:\Mi unidad\linkabbddyscriptderpaperestallidosocial\resp_cons_itsa.dta", clear
use "G:\Mi unidad\linkabbddyscriptderpaperestallidosocial\resp_rate_itsa.dta", clear

cap ssc install itsa

cap reshape long ht_, i(isoweek) j(year)
cap reshape long ct_, i(isoweek) j(year)
cap reshape long rt_, i(isoweek) j(year)
cap reshape long rt_, i(isoweek) j(year)
cap reshape long cr_, i(isoweek) j(year)
cap reshape long rr_, i(isoweek) j(year)

sort year isoweek
xtset year isoweek

replace did=0 if year!=2019

cap label variable ht_ "Trauma Hospitalizations"
cap label variable ct_ "Trauma Consultations"
cap label variable rt_ "Trauma Hospitalizations per Consultations (x 1,000)"
cap label variable ht_ "Respiratory Hospitalizations"
cap label variable cr_ "Respiratory Consultations"
cap label variable rt_ "Respiratory Hospitalizations per Consultations (x 1,000)"
label variable year "Year"

cap rename ht_ value
cap rename ct_ value
cap rename rt_ value
cap rename hr_ value
cap rename cr_ value
cap rename rr_ value


itsa ht, trperiod(43) replace ///
figure posttrend prefix(trauma_hosp) treat(2019) ///
contid(2015 2016 2017 2018)
 
xtreg value did i.isoweek, fe

