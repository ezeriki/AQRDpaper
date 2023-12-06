use "data/subway_analysis_use.dta", clear

//keep Per_pop_2

replace Per_pop_2 = (Per_pop_2 - 300)/100
gen iv1 = (Per_pop_2 >= 0)
gen iv1_int = iv1*Per_pop_2
gen iv1_int2 = iv1 * Per_pop_2^2
gen iv1_int3 = iv1 * Per_pop_2^3
gen iv1_int4 = iv1 * Per_pop_2^4

* Define GLOBAL VARIABLES *
		global mayor_cont3 Mayor_age gender2 race6 Mayor_c_edu Mayor_c_central_exp Mayor_c_prov_exp Mayor_c_county_exp Mayor_c_soe_exp Mayor_c_univ_exp Mayor_c_league Mayor_connection_work
	
	
	
* BANDWIDTH(BW) SELECTION*
	rdbwselect Mayor_plan Per_pop_2, c(0)
	ereturn list
	*BW=1.058 
		  
	* KEEP ONLY QUALIFIED CITIES *
	keep if Budget_income_2 > 1000000 & GRP_2 > 10000000




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

	