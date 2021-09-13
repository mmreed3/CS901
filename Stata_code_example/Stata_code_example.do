*************  Longitudional  MEPS  Paper - EITC and Liquidity ****************
* Madeline Reed
* January 25 2021
*************************
* This is the 5th do file of 7 files.
* 1) Longitudional Demographics - loads in the MEPS longitudional demographics files for Panels 15 - 20. 
* 2) Longitudional Monthly Expend - loads in the expenditure for 2011 - 2016 and aggregates spending to the person month
* 3) Plan Longitudional Dec1 -- puts people in their plans . keeps only those with private health  insuarnce not missing-- merges in their monthly data  health care spending from (Office, outpatient, inpatient and ER)
* 4) SummarystatsLongitudional  11-16. do --Cleans the demographics characteristics for modeling 
* 5) Longitudional tables -- produces the  descriptive tables.
* 6) Longitudional analysis--- runs the anaysis
* 7) Histogram_deb_Longitudional -- produces the histogram  for out-of-pocket costs. 

***************************************************************************
******************* Summary Stats table   ************************************
************************************************************************

clear
cd "/Users/madelinereed/Dropbox/EITC_Liquidity_Jan26"

use   "data/Longitudional_11_16_ALLPEOPLE"  //  277,032 person month 

**** Descriptive table "this is my sample of HDHP and low deductible and full sample of privately insured indviduals. 
** NOTE: this is how to create a local in stata-- so you not have to repeat all of the controls in each of the 3 samples (FULL sample, )
local X_personlevel  poor age  female totalkid  some_college married white black hisp poorhealthstatus  poormentalhealth adjust_eitc_amount2   northeast midwest south west Noded Low_ded High_ded

************ ************ ************ ************ ************ ************ ************ ************ ************ 
************ Each section of the code is creating an estimate of all of the covariates for the specfic sample. 
************ ************ ************ ************ ************ ************ ************ ************ ************ 
* full sample  of privately insured lower income
eststo Fullsample_svy1: svy, subpop(adult_lowinc_1):mean `X_personlevel'
*estat sd
*estadd r(sd)
*, listwise 
local SUB_POP =  e(N_sub)



*EITC Eligible  of privately insured lower income
eststo lowinc_eligible_1: svy, subpop(adult_lowinc_eligible_1):mean `X_personlevel' 
local SUB_POP =  e(N_sub) 
*estat sd
*estadd r(sd)





* NOT Eligible of privately insured lower income
eststo lowinc_NOTeligible_1: svy, subpop(adult_lowinc_NOTeligible_1):mean `X_personlevel' 
local SUB_POP =  e(N_sub)
*estat sd
*estadd r(sd)




************************************
*This line is what produces the table . 
esttab  Fullsample_svy1 lowinc_eligible_1 lowinc_NOTeligible_1 using /Users/madelinereed/Dropbox/Tex_examples/TableA_April26.tex, replace cells("b(fmt(2)) se(fmt(2))") label nodepvar   mtitle("All"  "EITC Eligible" "Not EITC Eligible") f stats(N_sub, fmt(0) )
** took out ci(fmt(2))
************************************



************************************
**** EXAMPLE CODE for line graph *********
lgraph monthly_total_outofpocket  month, by(earned_simtax) xscale(range(0 12))  xlabel(1 "Jan" 2 "Feb" 3 "March" 4 "April" 5 "May" 6 "June" 7 "July" 8 "Aug" 9 "Sept" 10 "Oct" 11 "Nov" 12 " Dec") saving(g1log,replace) legend(rows(1)) ytitle("Mean Out-of-pocket") text(16.5 2 "EITC months", color(red))
graph export  "Monthly_line.png", replace


************************************************************************************
*********************. Big fancy table :) *********************************************
************************************************************************************
la var earned_simtax "EITC eligible"
la def earned_simtax 1 "EITC eligible", modify
label val earned_simtax earned_simtax

 la var feb_march "Feb or March"
la def feb_march 1 "Feb or March", modify
label val feb_march feb_march


gen feb_march_earned_simtax = feb_march* earned_simtax
la var feb_march_earned_simtax  "Feb or March $\times$ EITC eligible "
la def feb_march_earned_simtax  1 "Feb or March $\times$ EITC eligible", modify
label val feb_march_earned_simtax  feb_march_earned_simtax 

gen feb_march_simtax = feb_march*adjust_eitc_amount2
la var feb_march_simtax  "Feb or March $\times$ EITC Amount"
la def feb_march_simtax  1 "Feb or March $\times$ EITC Amount", modify
label val feb_march_simtax  feb_march_simtax 
global DD2 feb_march_simtax 

gen age2 = age*age
gen age3 = age*age*age


gen adult_lowinc_noded=(adult_lowinc==1 & plan3==1)
gen adult_lowinc_low=(adult_lowinc==1 & plan3==2)	
gen adult_lowinc_HDHP=(adult_lowinc==1 & plan3==3)	
   


* create monthly dummies ( 1- 12 )
tab month, gen(m_)

** Controls for models !!!!!!
global controls_plan  poor age age2  age3 female totalkid HH_size  some_college married white black hisp poorhealthstatus  poormentalhealth  northeast midwest south west  i.plan4 i.year m_2 m_3 m_4 m_5 m_6 m_7 m_8 m_9 m_10 m_11 m_12


* this is becasue MEPS data is complex and have to account for the complex survey design see: 
*source: https://meps.ahrq.gov/survey_comp/standard_errors.jsp 
svyset  [pweight=poolwt], strata(varstr_l) psu(varpsu_l) singleunit(scaled)

********************************* ******************************************************************
********************************* actual table**************************************************************
***************************************************************************************************


 eststo m1all:svy, subpop(adult_lowinc): reg monthly_total_outofpocket feb_march  earned_simtax feb_march_earned_simtax  $controls_plan
 estadd ysumm
  local SUB_POP =  e(N_sub)
*Office based 
 eststo m2all:svy, subpop(adult_lowinc): reg  adjust_sf_ob2  feb_march    earned_simtax feb_march_earned_simtax  $controls_plan  
 estadd ysumm
  local SUB_POP =  e(N_sub)
*Out patient
 eststo m3all:svy, subpop(adult_lowinc):reg  adjust_sf_op2  feb_march   earned_simtax feb_march_earned_simtax  $controls_plan  
  estadd ysumm
   local SUB_POP =  e(N_sub)
*Inpatient
 eststo m4all:svy, subpop(adult_lowinc): reg adjust_sf_ip2  feb_march   earned_simtax feb_march_earned_simtax  $controls_plan 
 estadd ysumm
  local SUB_POP =  e(N_sub)
* ER expenditure
 eststo m5all:svy, subpop(adult_lowinc): reg adjust_sf_er2  feb_march   earned_simtax feb_march_earned_simtax  $controls_plan 
 estadd ysumm
  local SUB_POP =  e(N_sub)


 esttab m1all m2all m3all m4all m5all using  /Users/madelinereed/Dropbox/Tex_examples/EITCm1_ALLPEOPLE_private_V1.tex, replace ///
		nogaps nodep modelwidth(5) varwidth(9) width(\textwidth) se(2) b(%7.2fc)  ///
		obslast booktabs alignment(S) nobaselevels  ///
		nonote fragment star(* 0.10 ** 0.05 *** 0.01)  ///
		stats(N_sub ymean ysd,   ///
			labels("\midrule N" "Mean (\\$)" "Sd")  ///
			layout("\multicolumn{1}c{@}""\multicolumn{1}c{@}""\multicolumn{1}c{@}")  ///
			fmt(%9.0fc %9.0fc %9.0fc))  ///
			keep (*feb_march* *earned_simtax* ) ///	
		order(feb_march earned_simtax feb_march_earned_simtax)   ///
		 substitute (\_ _)  ///
		varlabels(	feb_march "\textsc{Feb or March}"   ///
					earned_simtax "\textsc{EITC eligible}"  ///
					feb_march_earned_simtax "\textsc{Feb or March X EITC eligible}")   ///
		mtitles("\specialcell{Total \\ Monthly OOP}" "\specialcell{Office \\ Based}" "\specialcell{Outpatient}" "\specialcell{Inpatient}" "\specialcell{ER}")  ///
		posthead(" \midrule & \multicolumn{5}c{\textit{Panel A. All low income adults}} \\ \cmidrule(r){2-6}")

 

** PANEL 2 of the table 
 eststo m1priv:svy, subpop(adult_lowinc_private): reg monthly_total_outofpocket feb_march  earned_simtax feb_march_earned_simtax   $controls_plan 
 estadd ysumm
  local SUB_POP =  e(N_sub)
*Office based 
 eststo m2priv:svy, subpop(adult_lowinc_private): reg  adjust_sf_ob2   feb_march  earned_simtax feb_march_earned_simtax  $controls_plan 
 estadd ysumm
  local SUB_POP =  e(N_sub)
*Out patient
 eststo m3priv:svy, subpop(adult_lowinc_private):reg  adjust_sf_op2  feb_march  earned_simtax feb_march_earned_simtax  $controls_plan   
  estadd ysumm
   local SUB_POP =  e(N_sub)
*Inpatient
 eststo m4priv:svy, subpop(adult_lowinc_private): reg adjust_sf_ip2  feb_march  earned_simtax feb_march_earned_simtax $controls_plan  
 estadd ysumm
  local SUB_POP =  e(N_sub)
* ER expenditure
 eststo m5priv:svy, subpop(adult_lowinc_private): reg adjust_sf_er2  feb_march  earned_simtax feb_march_earned_simtax  $controls_plan  
 estadd ysumm
  local SUB_POP =  e(N_sub)
 

esttab m1priv m2priv m3priv m4priv m5priv using  /Users/madelinereed/Dropbox/Tex_examples/EITCm1_ALLPEOPLE_private_V1.tex, append ///
		nogaps nodep nomtitles nonumbers modelwidth(5) varwidth(9) width(\textwidth) se(2) b(%7.2fc)  ///
		obslast booktabs alignment(S)   nobaselevels ///
		nonote fragment star(* 0.10 ** 0.05 *** 0.01)  ///
		stats(N_sub ymean ysd,   ///
			labels("\midrule N" "Mean (\\$)" "Sd")  ///
			layout("\multicolumn{1}c{@}""\multicolumn{1}c{@}""\multicolumn{1}c{@}")  ///
			fmt(%9.0fc %9.0fc %9.0fc))  ///
			keep (*feb_march* *earned_simtax* ) ///	
		 substitute (\_ _)  ///
		varlabels(	feb_march "\textsc{Feb or March}"   ///
					earned_simtax "\textsc{EITC eligible}"  ///
					feb_march_earned_simtax "\textsc{Feb or March X EITC eligible}")   ///
		posthead(" \midrule & \multicolumn{5}c{\textit{Panel B. Low income adults with ESI}} \\ \cmidrule(r){2-6}")

		
		
		
		
** PANEL 3 of the table 	
*LOW deductible
* All Out-of-pocket costs
 eststo m1Low_ded:svy, subpop(adult_lowinc_low): reg monthly_total_outofpocket feb_march  earned_simtax feb_march_earned_simtax   $controls_plan 
 estadd ysumm
  local SUB_POP =  e(N_sub)
*Office based 
 eststo m2Low_ded:svy, subpop(adult_lowinc_low): reg  adjust_sf_ob2   feb_march  earned_simtax feb_march_earned_simtax  $controls_plan 
 estadd ysumm
  local SUB_POP =  e(N_sub)
*Out patient
 eststo m3Low_ded:svy, subpop(adult_lowinc_low):reg  adjust_sf_op2  feb_march  earned_simtax feb_march_earned_simtax  $controls_plan   
  estadd ysumm
   local SUB_POP =  e(N_sub)
*Inpatient
 eststo m4Low_ded:svy, subpop(adult_lowinc_low): reg adjust_sf_ip2  feb_march  earned_simtax feb_march_earned_simtax $controls_plan  
 estadd ysumm
  local SUB_POP =  e(N_sub)
* ER expenditure
 eststo m5Low_ded:svy, subpop(adult_lowinc_low): reg adjust_sf_er2  feb_march  earned_simtax feb_march_earned_simtax  $controls_plan  
 estadd ysumm
  local SUB_POP =  e(N_sub)
 

* Panel 3 of the table 
esttab m1Low_ded m2Low_ded m3Low_ded m4Low_ded m5Low_ded using  /Users/madelinereed/Dropbox/Tex_examples/EITCm1_ALLPEOPLE_private_V1.tex, append ///
		nogaps nodep nomtitles nonumbers modelwidth(5) varwidth(9) width(\textwidth) se(2) b(%7.2fc)  ///
		obslast booktabs alignment(S)   nobaselevels ///
		nonote fragment star(* 0.10 ** 0.05 *** 0.01)  ///
		stats(N_sub ymean ysd,   ///
			labels("\midrule N" "Mean (\\$)" "Sd")  ///
			layout("\multicolumn{1}c{@}""\multicolumn{1}c{@}""\multicolumn{1}c{@}")  ///
			fmt(%9.0fc %9.0fc %9.0fc))  ///
			keep (*feb_march* *earned_simtax* ) ///	
		 substitute (\_ _)  ///
		varlabels(	feb_march "\textsc{Feb or March}"   ///
					earned_simtax "\textsc{EITC eligible}"  ///
					feb_march_earned_simtax "\textsc{Feb or March X EITC eligible}")   ///
		posthead(" \midrule & \multicolumn{5}c{\textit{Panel C. Low income adults with Low Deductible}} \\ \cmidrule(r){2-6}")


* Panel 4 of the table 		
** HIGH
* All Out-of-pocket costs
 eststo m1HDHP:svy, subpop(adult_lowinc_HDHP): reg monthly_total_outofpocket feb_march  earned_simtax feb_march_earned_simtax   $controls_plan 
 estadd ysumm
  local SUB_POP =  e(N_sub)
*Office based 
 eststo m2HDHP:svy, subpop(adult_lowinc_HDHP): reg  adjust_sf_ob2   feb_march  earned_simtax feb_march_earned_simtax  $controls_plan 
 estadd ysumm
  local SUB_POP =  e(N_sub)
*Out patient
 eststo m3HDHP:svy, subpop(adult_lowinc_HDHP):reg  adjust_sf_op2  feb_march  earned_simtax feb_march_earned_simtax  $controls_plan   
  estadd ysumm
   local SUB_POP =  e(N_sub)
*Inpatient
 eststo m4HDHP:svy, subpop(adult_lowinc_HDHP): reg adjust_sf_ip2  feb_march  earned_simtax feb_march_earned_simtax $controls_plan  
 estadd ysumm
  local SUB_POP =  e(N_sub)
* ER expenditure
 eststo m5HDHP:svy, subpop(adult_lowinc_HDHP): reg adjust_sf_er2  feb_march  earned_simtax feb_march_earned_simtax  $controls_plan  
 estadd ysumm
  local SUB_POP =  e(N_sub)
 



esttab m1HDHP m2HDHP m3HDHP m4HDHP m5HDHP using  /Users/madelinereed/Dropbox/Tex_examples/EITCm1_ALLPEOPLE_private_V1.tex, append ///
		nogaps nodep nomtitles nonumbers modelwidth(5) varwidth(9) width(\textwidth) se(2) b(%7.2fc)  ///
		obslast booktabs alignment(S)   nobaselevels ///
		nonote fragment star(* 0.10 ** 0.05 *** 0.01)  ///
		stats(N_sub ymean ysd,   ///
			labels("\midrule N" "Mean (\\$)" "Sd")  ///
			layout("\multicolumn{1}c{@}""\multicolumn{1}c{@}""\multicolumn{1}c{@}")  ///
			fmt(%9.0fc %9.0fc %9.0fc))  ///
			keep (*feb_march* *earned_simtax* ) ///	
		 substitute (\_ _)  ///
		varlabels(	feb_march "\textsc{Feb or March}"   ///
					earned_simtax "\textsc{EITC eligible}"  ///
					feb_march_earned_simtax "\textsc{Feb or March X EITC eligible}")   ///
		posthead(" \midrule & \multicolumn{5}c{\textit{Panel D. Low income adults with High Deductible}} \\ \cmidrule(r){2-6}")
		
		


********************** Coefficient plot ***********
*****Appendix Table 1 --- Coefficient plot of  Sample of Low income adults with ESI	
set scheme s1mono
  coefplot  ( m1priv, label(Total OOP) pstyle(p3) msymbol(X)) ///
(m2priv, label(Office Based) pstyle(p3) msymbol(S)) ///
(m3priv, label(Outpatient) pstyle(p3) msymbol(D)) ///
(m4priv, label(Inpatient) pstyle(p3) msymbol(T)) ///
(m5priv, label(ER) pstyle(p3) msymbol(O)) ///
        , keep (*earned_simtax* *feb_march*   *feb_march_earned_simtax* ) xline(0) ci(90)  ///
		order(feb_march earned_simtax  feb_march_earned_simtax )  ///
		coeflabels(feb_march_earned_simtax = "Feb or March X EITC eligible")	
graph export "/Users/madelinereed/Dropbox/Tex_examples/EITCAppendix_table1.png" , replace






