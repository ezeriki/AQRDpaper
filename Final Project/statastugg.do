summ gender2

summ Mayor_plan if gender2 == 0
tab gender2
tab Mayor_age

tab Mayor_connection_work
tab Mayor_c_prov_exp
tab Mayor_c_edu
tab Mayor_c_edu, nolabel

codebook inv1_per
summ inv1_per