use "D:\linkabbddyscriptderpaperestallidosocial\data15a64_rn2.dta"
xtset year isoweek

cap ssc install synth
*http://fmwww.bc.edu/RePEc/bocode/s/synth.html
*https://yiqingxu.org/teaching/17802/synth.pdf
*https://economics.mit.edu/files/11859
https://www.urban.org/sites/default/files/publication/89246/the_synthetic_control_method_as_a_tool_0.pdf*
drop if isoweek==53
synth log_hosp_trauma log_hosp_trauma(1) log_hosp_trauma(21) log_hosp_trauma(41) log_hosp_circ log_hosp_resp log_difftrc , trunit(2019) ///
trperiod(43) figure keep(`v') nested allopt replace 

cap ado uninstall synth_runner //in-case already installed
net install synth_runner, from(https://raw.github.com/bquistorff/synth_runner/master/) replace
net describe st0500, from(http://www.stata-journal.com/software/sj17-4)
ssc install synthrunner

*https://www.statalist.org/forums/forum/general-stata-discussion/general/1470974-synth_runner-package
*https://www.statalist.org/forums/forum/general-stata-discussion/general/1485219-synth_runner-interpret-the-statistical-outcome-and-general-questions-stata
*https://github.com/bquistorff/synth_runner
*http://econweb.umd.edu/~galiani/files/synth_runner.pdf

synth_runner log_hosp_trauma log_hosp_trauma(1) log_hosp_trauma(21) log_hosp_trauma(41) log_hosp_circ log_hosp_resp log_difftrc , trunit(2019) ///
trperiod(43) keep(`v') nested fig allopt replace 

keep(2019, replace) nested fig

cap drop pre_rmspe-log_hosp_trauma_synth
cap drop log_hosp_trauma_scaled_synth 
cap drop log_hosp_trauma_scaled
synth_runner log_hosp_trauma log_hosp_trauma(1) log_hosp_trauma(21) log_hosp_trauma(41) log_hosp_circ log_hosp_resp log_difftrc , trunit(2019) ///
trperiod(43) keep(2019, replace) nested allopt replace trends gen_vars trace 

single_treatment_graphs, trlinediff(-1) raw_gname(cigsale1_raw) ///
	effects_gname(cigsale1_effects) effects_ylabels(-30(1)30) effects_ymax(35) effects_ymin(-35)

pval_graphs , pvals_gname(cigsale1_pval) pvals_std_gname(cigsale1_pval_t)

