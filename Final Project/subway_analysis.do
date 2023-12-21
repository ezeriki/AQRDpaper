use "data/subway_analysis_use.dta", clear


// filter for fsj2 == 0
keep if fsj2 == 0


// Extension on age

tab Mayor_age
tab PS_age

// understanding tenure
tab Mayor_c_tenure

//keep Per_pop_2

replace Per_pop_2 = (Per_pop_2 - 300)/100
gen iv1 = (Per_pop_2 >= 0)
gen iv1_int = iv1*Per_pop_2
gen iv1_int2 = iv1 * Per_pop_2^2
gen iv1_int3 = iv1 * Per_pop_2^3
gen iv1_int4 = iv1 * Per_pop_2^4

* Define GLOBAL VARIABLES *
		global mayor_cont3 Mayor_age gender2 race6 Mayor_c_edu Mayor_c_central_exp Mayor_c_prov_exp Mayor_c_county_exp Mayor_c_soe_exp Mayor_c_univ_exp Mayor_c_league Mayor_connection_work
	
	
	
// for diff in diff extension, examining age and tenure
tab Mayor_c_tenure
tab Mayor_age
	
	
* BANDWIDTH(BW) SELECTION*
	rdbwselect Mayor_plan Per_pop_2, c(0)
	ereturn list
	*BW=1.058 
		  
	* KEEP ONLY QUALIFIED CITIES *
	keep if Budget_income_2 > 1000000 & GRP_2 > 10000000
	
	
// global option for DID
global mayor_cont gender2 race6 Mayor_age Mayor_c_edu Mayor_c_central_exp Mayor_c_prov_exp Mayor_c_county_exp Mayor_c_soe_exp Mayor_c_univ_exp Mayor_c_league Mayor_connection_work
	global mayor_cont2 gender2 race6 Mayor_age Mayor_c_edu Mayor_c_central_exp Mayor_c_prov_exp Mayor_c_county_exp Mayor_c_soe_exp Mayor_c_univ_exp Mayor_c_league
	global base_cont lpop_1 lgdp_1 lrev_1 GRP_growth_1
	global PS_cont PS_age PS_gender2 PS_race8 PS_connection_work PS_c_2currentsec2 PS_c_prov_exp PS_c_central_exp PS_c_edu PS_c_soe_exp PS_c_univ_exp PS_c_league 
	

// DID testingeststo clear
	eststo: qui reghdfe Mayor_promotion3y Mayor_plan if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_promotion3y Mayor_plan $mayor_cont if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_promotion3y Mayor_plan $mayor_cont $base_cont if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_promotion3y Mayor_plan $mayor_cont $base_cont if fsj2 == 0, absorb(Year#pro_code City_Code) vce(cluster City_Code)
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(Mayor_plan) replace


// RD testing
eststo clear
		eststo: ivreg2 Mayor_promotion3y (Mayor_plan =iv1) Per_pop_2 iv1_int i.provinceyear i.City_Code if abs(Per_pop_2) <= 1.058  & fsj2 == 0, cluster(City_Code) first
		eststo: ivreg2 Mayor_promotion3y (Mayor_plan =iv1) Per_pop_2 iv1_int $mayor_cont3 i.provinceyear i.City_Code if abs(Per_pop_2) <= 1.058  & fsj2 == 0, cluster(City_Code) first
		eststo: ivreg2 Mayor_promotion3y (Mayor_plan =iv1) Per_pop_2 iv1_int $mayor_cont3 lgdp_per_1 lrev_per_1 GRP_growth_1 i.provinceyear i.City_Code if abs(Per_pop_2) <= 1.058  & fsj2 == 0, cluster(City_Code) first
		esttab , se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) drop(*.provinceyear *.City_Code) replace	
		esttab using table_rd.rtf, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) drop(*.provinceyear *.City_Code) replace	

	
tab iv1
tab Per_pop_2

rdbwselect Mayor_plan Per_pop_2, c(0)
	ereturn list
	
ssc install rdrobust
	
	
tab Mayor_age
	
	
list Mayor_plan

. ssc install ivreg2

keep if Budget_income_2 > 1000000 & GRP_2 > 10000000


eststo clear
		eststo: ivreg2 Mayor_promotion3y (Mayor_plan =iv1) Per_pop_2 iv1_int i.provinceyear i.City_Code if abs(Per_pop_2) <= 1.058  & fsj2 == 0, cluster(City_Code) first
		eststo: ivreg2 Mayor_promotion3y (Mayor_plan =iv1) Per_pop_2 iv1_int $mayor_cont3 i.provinceyear i.City_Code if abs(Per_pop_2) <= 1.058  & fsj2 == 0, cluster(City_Code) first
		eststo: ivreg2 Mayor_promotion3y (Mayor_plan =iv1) Per_pop_2 iv1_int $mayor_cont3 lgdp_per_1 lrev_per_1 GRP_growth_1 i.provinceyear i.City_Code if abs(Per_pop_2) <= 1.058  & fsj2 == 0, cluster(City_Code) first
		esttab , se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) drop(*.provinceyear *.City_Code) replace	
		esttab using table_rd.rtf, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) drop(*.provinceyear *.City_Code) replace	

		
		
		
*****************************************************
************** Testing with new data****************
use infrastructure_use.dta , clear


collapse (sum) lroad_inv_per, by(cityID year)

rename cityID City_Code

merge m:m City_Code using "data/subway_analysis_use.dta"




drop if _merge == 1

PACKAGE NEEDED: estout;  coefplot; reghdfe

	ssc install estout, replace 
	//ssc install coefplot, replace
	ssc install reghdfe, replace



eststo clear
eststo: qui reghdfe Mayor_promotion3y lroad_inv_per if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code) 
	
	
	
reghdfe Mayor_promotion5y lroad_inv_per if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code) 	