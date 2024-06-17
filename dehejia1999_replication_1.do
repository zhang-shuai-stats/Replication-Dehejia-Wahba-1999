* 2024/5/28
* 2024/6/13添加删除临时文件
clear
cd /Users/zhangshuai/Desktop/interest/dehejia1999_replication // 修改默认目录

*-----------------------------------------------------------------------------
* data preparation and label variable 
*-----------------------------------------------------------------------------
* combine all datasets
use nsw, clear
gen id = 1
local j = 2
foreach i in nsw_dw psid_controls psid_controls2 psid_controls3 cps_controls cps_controls2 cps_controls3 {
	append using `i'
	replace id = `j' if mi(id)
	local ++j
}

gen u74 = re74 == 0 if !mi(re74)
gen u75 = re75 == 0 if !mi(re75)

* label values
labmask id, values(data_id)
label define treat 0 "Control" 1 "Treated"
label values treat treat
label define black 0 "Not Black" 1 "Black"
label values black black
label define hispanic 0 "Not Hispanic" 1 "Hispanic"
label values hispanic hispanic
label define married 0 "Not Married" 1 "Married"
label values married married
label define nodegree 0 "Degree" 1 "No Degree"
label values nodegree nodegree
label define unemploy 0 "Employed" 1 "Unemployed"
label values u74 unemploy
label values u75 unemploy
labmask age, values(age)
labmask education, values(education)

order id data_id-nodegree u74 re74 u75 re75
drop data_id

save dehejia1999_data, replace 

*-----------------------------------------------------------------------------
* Table 1 Sample Means of Characteristics for NSW and Comparison Samples
*-----------------------------------------------------------------------------
use dehejia1999_data, clear

* variable names
local varnames age education black hispanic nodegree married  re74  re75
local var_count  `:word count `varnames''

collect clear
forvalues i = 1/`var_count' {
	if `i' < 3 {
		qui: table treat if id==`i', stat(freq) stat(mean `varnames') stat(semean `varnames') nototals append 
	}
	else {
		qui: table  if id==`i', stat(freq)  nototals append	
	}	
}

foreach var in `varnames' {
	forvalues i = 3/`var_count' {
		qui: reg `var' i.treat if (id == 2 & treat == 1) | id == `i'
		scalar mean = _r_b[_cons]
		scalar se = _r_se[1.treat]
		scalar pval = _r_p[1.treat]
		collect get mean = mean  semean = se pval = pval, tags(cmdset[`i'] var[`var'])
	}
}

*********
* 修改格式
*********
collect dims
collect levelsof cmdset
collect levelsof result
collect levelsof var
collect levelsof colname


* add significance star
collect stars pval 0.1 "*" 0.05 "**" 0.01 "***", attach(mean)

* 行
local ind
forvalues i = 3/`var_count' {
	local ind `ind' `i'
}
collect remap cmdset[`ind'] = cmdset1[`ind']
collect style header result, level(hide)
collect style header treat, title(hide)
collect style header cmdset, title(hide)

collect label levels cmdset 1 "NSW/Lalonde:" 2 "RE74 subset:", modify
collect label dim cmdset1 "Comparison groups:", modify
local colname `" "PSID-1" "PSID-2" "PSID-3" "CPS-1" "CPS-2" "CPS-3" "'
local i = 3
foreach j in `colname' {
	collect label levels cmdset1 `i' "`j'", modify
	local ++i
}

collect recode result `"frequency"' = `"mean"'
collect style autolevels treat 1 0

* 列
collect recode var `"_hide"' = `"obs"'
local colname1 `" "No. of observations" "Age" "Education" "Black" "Hispanic" "No degree" "Married" "RE74(U.S.$)" "RE75(U.S.$)" "'
local varnames obs age education black hispanic nodegree married  re74  re75
local var_count  `:word count `varnames''
forvalues i = 1/`var_count' {
	local tmp `:word `i' of `varnames''
	local tmp1 `:word `i' of `colname1''
	collect label levels var `"`tmp'"' `"`tmp1'"', modify
}

* 数字格式
collect style cell var, warn halign(center) valign(center) nformat(%6.2fc)
collect style cell var[obs re74 re75],  nformat(%6.0fc)
collect style cell cmdset#result[semean], sformat("(%s)")
collect style cell cmdset1#result[semean], sformat("[%s]")

* 布局
collect style cell border_block, border(right, pattern(nil))
collect title "Table 1. Sample Means of Characteristics for NSW and Comparison Samples"

* 导出
collect layout  (cmdset#treat#result cmdset1#result[mean semean]) (var)
collect export "table1", as(docx) replace	


************************
* test if randomization
************************
use dehejia1999_data, clear 

drop if id > 2
collect clear
foreach var of varlist age-re75 {
	forvalues i = 1/2 {
		capture: {
			collect get coef = _r_b[1.treat] se = _r_se[1.treat] p = _r_p[1.treat], tags(row[`var'] col[`i']): reg `var' i.treat if id == `i'
		}
	}
}

collect dims
collect levelsof cmdset
collect levelsof result
collect levelsof colname

* add significance star
collect stars p 0.1 "*" 0.05 "**" 0.01 "***", attach(coef)

* row header format
collect style header result, level(hide)
local colname `" "Age" "Education" "Black" "Hispanic" "No degree" "Married" "Unemployment 1974" "RE74(U.S.$)" "Unemployment 1975" "RE75(U.S.$)" "'
local varnames age education black hispanic nodegree married u74 re74 u75 re75
local var_count  `:word count `varnames''
forvalues i = 1/`var_count' {
	local tmp `:word `i' of `varnames''
	local tmp1 `:word `i' of `colname''
	collect label levels row `"`tmp'"' `"`tmp1'"', modify
}

* column header format	
collect label levels col `"1"' `"NSW/Lalonde"', modify
collect label levels col `"2"' `"RE74 subset"', modify

* cell format
collect style cell col, warn halign(center) valign(center) nformat(%6.2fc)
collect style cell result[se], sformat("(%s)")

* table format
collect style cell border_block, border(right, pattern(nil))
collect style column, nodelimiter position(top) extraspace(1)  width(asis)
collect title "Randomization test for NSW and RE74 Samples"

collect layout  (row#result[coef se]) (col)

*-------------------------------------------------------------------------------
* Table 2: Lalonde's Earnings Comparisons and Estimated Training Effects for the NSW male Participants using Comparison Groups from the PSID and the CPS
*-------------------------------------------------------------------------------
use dehejia1999_data, clear

* independent variables
global x age c.age#c.age education nodegree black hispanic

collect clear
* part a
forvalues i = 1/8 {
	if `i' != 2 {
		capture: {
			collect get coef = _r_b[1.treat] se = _r_se[1.treat] pval = _r_p[1.treat],tags(row[`i'] col1[1] col[a]):reg re78 i.treat if (treat == 1 & id == 1) | id == `i'
			collect get coef = _r_b[1.treat] se = _r_se[1.treat] pval = _r_p[1.treat],tags(row[`i'] col1[2] col[a]):reg re78 i.treat $x if (treat == 1 & id == 1) | id == `i'
			collect get coef = _r_b[1.treat] se = _r_se[1.treat] pval = _r_p[1.treat],tags(row[`i'] col2[3] col[a]):reg re78 i.treat re75 if (treat == 1 & id == 1) | id == `i'
			collect get coef = _r_b[1.treat] se = _r_se[1.treat] pval = _r_p[1.treat],tags(row[`i'] col2[4] col[a]):reg re78 i.treat re75 $x if (treat == 1 & id == 1) | id == `i'
		}
	}
}

* part b
forvalues i = 1/8 {
	if `i' != 1 {
		capture: {
			collect get coef = _r_b[1.treat] se = _r_se[1.treat] pval = _r_p[1.treat],tags(row[`i'] col1[1] col[b]):reg re78 i.treat if (treat == 1 & id == 2) | id == `i'
			collect get coef = _r_b[1.treat] se = _r_se[1.treat] pval = _r_p[1.treat],tags(row[`i'] col1[2] col[b]):reg re78 i.treat $x if (treat == 1 & id == 2) | id == `i'
			collect get coef = _r_b[1.treat] se = _r_se[1.treat] pval = _r_p[1.treat],tags(row[`i'] col2[3] col[b]):reg re78 i.treat re75 if (treat == 1 & id == 2) | id == `i'
			collect get coef = _r_b[1.treat] se = _r_se[1.treat] pval = _r_p[1.treat],tags(row[`i'] col2[4] col[b]):reg re78 i.treat re75 $x if (treat == 1 & id == 2) | id == `i'
		}
	}
}

* part c
forvalues i = 1/8 {
	if `i' != 1 {
		capture: {
			collect get coef = _r_b[1.treat] se = _r_se[1.treat] pval = _r_p[1.treat],tags(row[`i'] col1[1] col[c]):reg re78 i.treat if (treat == 1 & id == 2) | id == `i'
			collect get coef = _r_b[1.treat] se = _r_se[1.treat] pval = _r_p[1.treat],tags(row[`i'] col1[2] col[c]):reg re78 i.treat re74 $x if (treat == 1 & id == 2) | id == `i'
			collect get coef = _r_b[1.treat] se = _r_se[1.treat] pval = _r_p[1.treat],tags(row[`i'] col2[3] col[c]):reg re78 i.treat re75 if (treat == 1 & id == 2) | id == `i'
			collect get coef = _r_b[1.treat] se = _r_se[1.treat] pval = _r_p[1.treat],tags(row[`i'] col2[4] col[c]):reg re78 i.treat re74 re75 $x if (treat == 1 & id == 2) | id == `i'
		}
	}
}

* format
collect dims
collect levelsof cmdset
collect levelsof result
collect levelsof colname

* significance star
collect stars pval 0.1 "*" 0.05 "**" 0.01 "***", attach(coef)

* row format 
collect recode row `"1"' = `"2"'
collect style autolevels row 2 3 4 5 6 7 8

collect style header result, level(hide)
local colname `" "NSW" "PSID-1" "PSID-2" "PSID-3" "CPS-1" "CPS-2" "CPS-3" "'
local i = 2
foreach j in `colname' {
	collect label levels row `i' "`j'", modify
	local ++i
}

* column format
collect style column, nodelimiter dups(center) position(top) width(asis)
collect label levels col a "A. Lalonde's original sample" b "B. RE74 subsample (results do not use RE74)" c "B. RE74 subsample (results use RE74)", modify

collect label dim col1 "NSW treatment earnings less comparison group earnings 1978", modify
collect label dim col2 "Unrestricted differences in differences: Quasi-difference in earnings growth 1975-1978", modify

collect style header col1 col2, title(label)
collect label levels col1 1 "Unadjusted" 2 "Adjusted", modify
collect label levels col2 3 "Unadjusted" 4 "Adjusted", modify

* cell format
collect style cell col, warn halign(center) valign(center) nformat(%9.0fc)
collect style cell result[se], sformat("(%s)")

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "Table 2. Lalonde's Earnings Comparisons and Estimated Training Effects for the NSW male Participants using Comparison Groups from the PSID and the CPS"

collect layout (row#result[coef se]) (col[a]#col1 col[a]#col2 col[b]#col1 col[b]#col2 col[c]#col1 col[c]#col2)
collect export "table2", as(docx) replace	

collect style cell col, warn halign(left) valign(center) nformat(%9.0fc)
collect layout (col[a]#col1 col[a]#col2 col[b]#col1 col[b]#col2 col[c]#col1 col[c]#col2) (row#result[coef se]) 
*-------------------------------------------------------------------------------
* Figure 1 & 2. Histogram of the Estimated Propensity Score for NSW Treated Units and PSID / CPS Comparison Units. 
*-------------------------------------------------------------------------------
*******************
* propensity score
*******************
use dehejia1999_data, clear

drop if id == 1 | (id == 2 & treat == 0)
global x1 age c.age#c.age education c.education#c.education married nodegree black hispanic re74 re75 c.re74#c.re74 c.re75#c.re75

* PSID-1
logit treat $x1 c.u74#c.black if inlist(id,2,3)
predict psid1_score if e(sample)

* PSID-2 & PSID-3
logit treat $x1 u74 u75 if inlist(id,2,4)
predict psid2_score if e(sample)

logit treat $x1 u74 u75 if inlist(id,2,5)
predict psid3_score if e(sample)

* CPS-1 & CPS-2 & CPS-3
global x2 age c.age#c.age education c.education#c.education married nodegree black hispanic re74 re75 u74 u75 c.education#c.re74 c.age#c.age#c.age

logit treat $x2 if inlist(id,2,6)
predict cps1_score if e(sample)

logit treat $x2 if inlist(id,2,7)
predict cps2_score if e(sample)

logit treat $x2 if inlist(id,2,8)
predict cps3_score if e(sample)

save data_psm, replace 

*************************
* statistics in page 1058
*************************
* the numbers of discarded observations are different from that in the paper
use data_psm, clear
summarize psid1_score if treat == 1
scalar min_psid1 = r(min)
scalar max_psid2 = r(max)
summarize psid1_score if treat == 0
summarize psid1_score if treat == 0 & psid1_score < min_psid1

summarize cps1_score if treat == 1 
scalar min_cps1 = `r(min)'
summarize cps1_score if treat == 0 
summarize cps1_score if treat == 0 & cps1_score < min_cps1

* the number of observations with propensity score > 0.8
sort treat
by treat: summarize psid1_score if psid1_score > 0.8
by treat: summarize cps1_score if cps1_score > 0.8

*----------
* figure 1
*----------
use data_psm, clear
summarize psid1_score if treat == 1
drop if  psid1_score < `r(min)'
save psid1_temp, replace 

use data_psm, clear
summarize cps1_score if treat == 1 
drop if  cps1_score < `r(min)'
save cps1_temp, replace 

* 最小的一组只对照组保留150个观测值
use psid1_temp, clear
gen min_group = psid1_score >= 0.05
table treat if min_group == 0

sort min_group treat
by min_group treat: gen cid = _n
keep if (cid < 151 & min_group == 0 & treat == 0) | (min_group == 0 & treat == 1) | psid1_score >= 0.05

twoway (histogram psid1_score if treat == 0, frequency bin(20) fcolor(none) lpattern(dash)) (histogram psid1_score if treat == 1, frequency bin(20) fcolor(none)), ylabel(0(50)150) 

graph export "figure1.jpg", as(jpg) quality(100) replace

*----------
* figure 2
*----------
use cps1_temp, clear

gen min_group = cps1_score >= 0.05
table treat if min_group == 0

sort min_group treat
by min_group treat: gen cid = _n
keep if (cid < 201 & min_group == 0 & treat == 0) | (min_group == 0 & treat == 1) | cps1_score >= 0.05

twoway (histogram cps1_score if treat == 0, frequency bin(20) fcolor(none) lpattern(dash)) (histogram cps1_score if treat == 1, frequency bin(20) fcolor(none)), ylabel(0(50)200) 

graph export "figure2.jpg", as(jpg) quality(100) replace

*----------------------------------------------------------------------------
* psm的过程之一：检验是否平衡，结果与pscore命令一致
*----------------------------------------------------------------------------
use data_psm, clear

* common  support
summarize psid1_score if treat == 1
drop if psid1_score < r(min) | psid1_score > r(max)
summarize psid1_score

* group
gen psm_id2 = 1
replace psm_id2 = 2 if psid1_score > 0.2
replace psm_id2 = 3 if psid1_score > 0.4
replace psm_id2 = 4 if psid1_score > 0.6
replace psm_id2 = 5 if psid1_score > 0.8
table psm_id2

* test propensity score
sort treat
by treat: summarize psid1_score if psm_id2 == 5

collect clear
forvalues i = 1/5 {
	qui: ttest psid1_score if psm_id2 == `i', by(treat)
	collect get pval = r(p), tags(row[`i'] col[1])
}
* 在0.05的显著水平下第一组存在显著差异
collect layout (row#result) (col)

* split the first group 
replace psm_id2 = psm_id2 + 1 if psid1_score > 0.1
forvalues i = 1/2 {
	qui: ttest psid1_score if psm_id2 == `i', by(treat)
	collect get pval = r(p), tags(row[`i'] col[2])
}
collect layout (row#result) (col)
* 第一组存在差异

replace psm_id2 = psm_id2 + 1 if psid1_score > 0.05
forvalues i = 1/2 {
	qui: ttest psid1_score if psm_id2 == `i', by(treat)
	collect get pval = r(p), tags(row[`i'] col[3])
}
collect layout (row#result) (col)

* 不存在显著差异
* test variables
collect clear
local x age education black hispanic married nodegree re74 re75
forvalues i = 1/7 {
	foreach var of local x {
		qui:ttest `var' if psm_id2 == `i', by(treat)
		collect get pval=r(p), tags(row[`var'] col[`i'])
	}
}
collect layout (row#result) (col)

* test variables jointly
local x age education black hispanic married nodegree re74 re75
collect clear
forvalues i = 1/7 {
	qui: reg treat `x' if psm_id2 == `i'
	scalar pfval = Ftail(e(df_m),e(df_r),e(F))
	collect get f = pfval
}
collect layout (cmdset) (result)

*----------------------------------------------------------------------------
* Table 3. Estimated Training Effects for the NSW Male Participants Using Comparison Groups From PSID and CPS
*----------------------------------------------------------------------------
* use pscore command to generate propensity score and group 
* 由于分组过程非常麻烦，采用pscore的命令进行分组
use dehejia1999_data, clear
drop if id == 1 | (id == 2 & treat == 0)

gen age2 = age^2
gen educ2 = education^2
gen RE742 = re74^2
gen RE752 = re75^2
gen blackU74 = black * u74
gen edure74 = education * re74
gen age3 = age^3

pscore treat age age2 education educ2 married nodegree black hispanic re74 re75 RE742 RE752 blackU74 if inlist(id,2,3), pscore(psid1_score) blockid(psid1_id) comsup numblo(5) level(0.005) logit

* 结果显示分组平衡不满足
pscore treat age age2 education educ2 married nodegree black hispanic re74 re75 RE742 RE752 u74 u75 if inlist(id,2,4), pscore(psid2_score) blockid(psid2_id) comsup numblo(5) level(0.005) logit

* 结果显示分组平衡不满足
pscore treat age age2 education educ2 married nodegree black hispanic re74 re75 RE742 RE752 u74 u75 if inlist(id,2,5), pscore(psid3_score) blockid(psid3_id) comsup numblo(5) level(0.005) logit

forval i = 1/3 {
	local j = `i' + 5
	pscore treat age age2 education educ2 married nodegree black hispanic re74 re75  u74 u75 edure74 age3 if inlist(id,2,`j'), pscore(cps`i'_score) blockid(cps`i'_id) comsup numblo(5) level(0.005) logit
}

save dehejia1999_psm, replace

***************
* column 1 & 2 
***************
use dehejia1999_data, clear
collect clear
global x age c.age#c.age education nodegree black hispanic
forvalues i = 2/8 {	
	capture: {
		collect get coef = _r_b[1.treat] se = _r_se[1.treat], tags(row[`i'] col1[1] col[a]):reg re78 i.treat if (treat == 1 & id == 2) | id == `i'
		collect get coef = _r_b[1.treat] se = _r_se[1.treat], tags(row[`i'] col1[2] col[a]):reg re78 i.treat $x if (treat == 1 & id == 2) | id == `i'
	}
}
***********
* column 3: 
***********
* regression with quadratic propensity score as independent variable
use dehejia1999_psm, clear
local j = 3
foreach var in psid cps {
	forvalues i = 1/3 {
		capture: collect get coef = _r_b[1.treat] se = _r_se[1.treat], tags(row[`j'] col2[3] col[b]): reg re78 i.treat `var'`i'_score c.`var'`i'_score#c.`var'`i'_score if !mi(`var'`i'_id)
		local ++j
	}
}

*******************
* column 4， 5 & 6
*******************
* column 4: match estimator with block id as group identifier
local j = 1
local jj = 1
gen diff = .
gen diff_se = .
gen nobs_t = .
gen nobs_c = .
gen group = .

foreach var in psid cps {
	forvalues i = 1/3 {
		capture : {
			levelsof `var'`i'_id 
			foreach level in `r(levels)' {
				ttest re78 if `var'`i'_id == `level', by(treat)   // 每一个组的均值与方差
				replace diff = r(mu_2)-r(mu_1) if _n == `j'
				replace diff_se = r(se) if _n == `j'
				replace nobs_t = r(N_2) if _n == `j'
				replace nobs_c = r(N_1) if _n == `j'
				replace group = `jj' if _n == `j'				
				local ++j
			}
			local ++jj	
		}
	}	
}

* column 5: similar as column 4 but considering control variables in each block
global x1 age c.age#c.age education c.education#c.education married nodegree black hispanic re74 re75 c.re74#c.re74 c.re75#c.re75
global x2 age c.age#c.age education c.education#c.education married nodegree black hispanic re74 re75 u74 u75 c.education#c.re74 c.age#c.age#c.age

local j = 1
gen diff1 = .
gen diff_se1 = .
gen nobs1 = .

foreach var in psid cps {
	qui: {
		forvalues i = 1/3 {
			if `"`var'"' == "psid" & `i' == 1 {
				levelsof `var'`i'_id 
				foreach level in `r(levels)' {
					reg re78 i.treat $x1 c.u74#c.black  if `var'`i'_id == `level'
					replace diff1 = _b[1.treat] if _n == `j'
					replace diff_se1 = _se[1.treat] if _n == `j'
					replace nobs1 = e(N) if _n == `j'
					local ++j
				}
			}
			else if `"`var'"' == "psid" & inlist(`i',2,3) {
				levelsof `var'`i'_id 
				foreach level in `r(levels)' {
					reg re78 i.treat $x1 u74 u75  if `var'`i'_id == `level'
					replace diff1 = _b[1.treat] if _n == `j'
					replace diff_se1 = _se[1.treat] if _n == `j'
					replace nobs1 = e(N) if _n == `j'
					local ++j													
				}
			}
			else {
				levelsof `var'`i'_id 								
				foreach level in `r(levels)' {
					reg re78 i.treat $x2  if `var'`i'_id == `level'
					replace diff1 = _b[1.treat] if _n == `j'
					replace diff_se1 = _se[1.treat] if _n == `j'
					replace nobs1 = e(N) if _n == `j'
					local ++j														
				}
			}	
		}
	}
}

gen nobs2 = nobs_t + nobs_c // 验证总数是否相等
egen nobs_sum = sum(nobs_t) if !mi(nobs_t), by(group)
gen weight = nobs_t/nobs_sum if !mi(nobs_t)
egen coef = sum(diff*weight) if !mi(nobs_t), by(group)
egen sd = sum(diff_se^2*weight^2) if !mi(nobs_t), by(group)
egen coef1 = sum(diff1*weight) if !mi(nobs_t), by(group)
egen sd1 = sum(diff_se1^2*weight^2) if !mi(nobs_t), by(group)
egen obs = sum(nobs1) if !mi(nobs_t), by(group)
replace sd = sqrt(sd)
replace sd1 = sqrt(sd1)

keep coef sd coef1 sd1 obs group
duplicates drop

local j = 3
forvalues i = 1/6 {
	collect get coef = coef[`i'] se = sd[`i'], tags(row[`j'] col3[4] col[b])
	collect get coef = coef1[`i'] se = sd1[`i'], tags(row[`j'] col3[5] col[b])
	collect get coef = obs[`i'], tags(row[`j'] col3[6] col[b])	
	local ++j	
}

* 第4列的结果与atts的命令一致
* 注意：coefficent is negative for the psid-3 sample
use dehejia1999_psm, clear
atts re78 treat, pscore(psid1_score) blockid(psid1_id) comsup 
atts re78 treat, pscore(psid3_score) blockid(psid3_id) comsup 
atts re78 treat, pscore(cps2_score) blockid(cps2_id) comsup


***********
* column 7
***********
*----------------------------------------
* a program to match the propensity score 
*-----------------------------------------
capture program drop ps_match 
program ps_match
	qui:{
		args ps_score 
		
		tempvar psid treat_id  control_id  rep_count
		gen `psid' = _n
		
		tempfile temp
		save `temp' /* for final combination */
		
		keep if !mi(`ps_score')  /* drop all missing value */
		egen `rep_count' = count(treat), by(`ps_score' treat)  /* find replicate observations */
		summarize `rep_count' if treat == 0 /* replicate observations in control group */
		local max_count = r(max)
		local i = 0
		while `i' < `max_count' { /* generate more variables indicating selected observations if needed */
			tempvar select_`i'
			gen `select_`i'' = .
			local ++i	
		}
		
		sort `ps_score' treat `psid'  /* sort observations */
		gen `treat_id' = _n if treat == 1 /* gen identifier for observations in  treated group */
		gen `control_id' = _n if treat == 0  /* gen identifier for observations in control group */
		
		levelsof `treat_id', local(mylevel) /* index of each observation in treated group */
		foreach v in `mylevel' {
				
			local v1 = `v' - 1
			local v2 = `v' + 1 
			
			while mi(`control_id'[`v1']) & `v1' > 0 { /* find the  index of low nearest control observation */
				local --v1
			}
			
			while mi(`control_id'[`v2']) & `v2' < _N { /* find the index of high nearest control observation */
				local ++v2
			}
			
			local delta1 = abs(`ps_score'[`v'] - `ps_score'[`v1'])
			local delta2 = abs(`ps_score'[`v'] - `ps_score'[`v2'])
			
			if (mi(`control_id'[`v1']) | `delta1' > `delta2') & !mi(`control_id'[`v2']) { /* if v1 == 0, then select v2 */
				local v2_count = `rep_count'[`v2']  /* the frequncy of propensity score for v2 */
				if `v2_count' == 1 { /* no replicates */
					replace `select_0' = `psid'[`v2'] if `treat_id' == `v'
				}
				else { /* replicates */
					local i = 0
					local v3_val
					while `i' < `v2_count' { /* putting the index of replicate observations  together */
						local v3 = `v2' + `i'
						local v3_val `v3_val' `=`psid'[`v3']'
						local ++i
					}
					local v3_val = subinstr("`v3_val'", " ", "\ ", .)
					mata: st_local("v3_direction",  invtokens(strofreal(sort(`v3_val',1))'))
					local i = 0
					while `i' < `v2_count' {
						local j = `i' + 1
						local vj : word `j' of `v3_direction'
						replace `select_`i'' =  `vj' if `treat_id' == `v'
						local ++i
					}	
				}
			}
			
			else  {
				local v1_count = `rep_count'[`v1']
				if `v1_count' == 1 {
					replace `select_0' = `psid'[`v1'] if `treat_id' == `v'
				}
				else {
					local i = 0
					local v3_val
					while `i' < `v1_count' {
						local v3 = `v1' - `i'
						local v3_val `v3_val' `=`psid'[`v3']'
						local ++i
					}
					local v3_val = subinstr("`v3_val'", " ", "\ ", .)
					mata: st_local("v3_direction",  invtokens(strofreal(sort(`v3_val',1))'))
					local i = 0
					while `i' < `v1_count' {
						local j = `i' + 1
						local vj : word `j' of `v3_direction'
						replace `select_`i'' =  `vj' if `treat_id' == `v'
						local ++i
					}					
				}
			}
		}
		
		
		local i = 0
		while `i' < `max_count' { /* create new variables indicating selected observations in the dataset */
			gen select_`i' = `select_`i'' 
			local ++i
		}
		
		keep `psid' select_*
		merge 1:1 `psid' using `temp', nogen /* merge original dataset */
		order select_*, last
		
			
		foreach var of varlist select_* { /* dropping missing variables */
			capture assert missing(`var')
			if !_rc {
				drop `var'
			}
		}
		
		* indicate selected control variables
		foreach var of varlist select_* {
			replace `var' = `psid' if treat == 0
			tempvar dup_count
			egen `dup_count' = count(treat), by(`var')
			replace `var' = . if treat == 0 & `dup_count' == 1	
		}	
		
		* group index and weight for each observation
		tempvar group weight
		egen `group' = group(select_*) if treat == 1, missing
		egen `weight' = count(treat) if treat == 1, by(select_0) // 第一个控制组编号确定后，后面选择的编号也就确定了
		
		foreach var of varlist select_* {
			sort `var' `group'
			by `var': replace `group' = `group'[_n-1] if mi(`group') & !mi(`var')
			by `var': replace `weight' = `weight'[_n-1] if mi(`weight') & !mi(`var')
		}
		replace `weight' = 1 if treat == 1	
		
		gen group_`ps_score' = `group'
		gen weight_`ps_score' = `weight'
		
		sort `psid'
	}
		
end

* column 7: nearest neighbour match with propensity score
use dehejia1999_psm, clear

drop if id == 1 | (id == 2 & treat == 0)
local varnames age education black hispanic nodegree married  re74  re75

local j = 3
foreach var in psid cps {
	qui: {
		forvalues i = 1/3 {
			preserve
			ps_match `var'`i'_score
			replace group_`var'`i'_score = _n if treat == 1
			collapse (mean) re78 weight_`var'`i'_score `varnames', by(treat group_`var'`i'_score)
			reg re78 i.treat [fweight = weight_`var'`i'_score]
			collect get coef = _b[1.treat] se = _se[1.treat], tags(row[`j'] col4[7] col[b])

			reg re78 i.treat `varnames' [fweight = weight_`var'`i'_score]
			collect get coef = _b[1.treat] se = _se[1.treat], tags(row[`j'] col4[8] col[b])			
			restore
			local ++j
		}
	}
}


* 对比teffects的结果
global x1 age c.age#c.age education c.education#c.education married  black nodegree hispanic re74 re75 c.re74#c.re74 c.re75#c.re75  // 对比becker2002可知，是否添加nodegree对结果产生重大影响，所以psm的方法并不是像dehejia声称的那么稳健
global x2 age c.age#c.age education c.education#c.education married nodegree black hispanic re74 re75 u74 u75 c.education#c.re74 c.age#c.age#c.age

local j = 3
foreach var in psid cps {
	qui: {
		forvalues i = 1/3 {
			if `"`var'"' == "psid" & `i' == 1 {
				teffects psmatch (re78) (treat $x1 c.u74#c.black ) if inlist(id,2,`j'), atet
				collect get coef = r(table)[1,1] se = r(table)[2,1], tags(row[`j'] col4[9] col[b])			
				local ++j
	
			}
			else if `"`var'"' == "psid" & inlist(`i',2,3) {
				teffects psmatch (re78) (treat $x1 u74 u75) if inlist(id,2,`j'), atet 
				collect get coef = r(table)[1,1] se = r(table)[2,1], tags(row[`j'] col4[9] col[b])			

				local ++j
			}
			else {
				teffects psmatch (re78) (treat $x2) if inlist(id,2,`j'), atet 
				collect get coef = r(table)[1,1] se = r(table)[2,1], tags(row[`j'] col4[9] col[b])			
				local ++j
			}	
		}
	}
}

*********
* format
*********
collect dims
collect levelsof cmdset
collect levelsof result
collect levelsof colname


* row format 
collect style header result, level(hide)
local colname `" "NSW" "PSID-1" "PSID-2" "PSID-3" "CPS-1" "CPS-2" "CPS-3" "'
local i = 2
foreach j in `colname' {
	collect label levels row `i' "`j'", modify
	local ++i
}

* column format
collect style column, nodelimiter dups(center) position(top) width(asis)
collect label levels col a "NSW earnings less comparison group earnings" b "NSW treatment earnings less comparison group earnings, conditional on the estimated propensity score" , modify

collect label dim col3 "Stratifying on the score", modify
collect label dim col4 "Matching on the score", modify
collect style header col3 col4, title(label)

collect label levels col1 1 "Unadjusted" 2 "Adjusted", modify
collect label levels col2 3 "Quadratic in score", modify
collect label levels col3 4 "Unadjusted" 5 "Adjusted" 6 "Observations", modify
collect label levels col4 7 "Unadjusted" 8 "Adjusted" 9 "Teffects result", modify


* cell format
collect style cell col, warn halign(center) valign(center) nformat(%9.0fc)
collect style cell result[se], sformat("(%s)")

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "Table 3. Estimated Training Effects for the NSW Male Participants Using Comparison Groups From PSID and CPS"

collect layout (row#result) (col[a]#col1 col[b]#col2 col[b]#col3 col[b]#col4)
collect export "table3", as(docx) replace	


*----------------------------------------------------------------------
* Table 4. Sample Means of Characteristics for Matched Control Samples
*----------------------------------------------------------------------
use dehejia1999_psm, clear

* variable names
local varnames age education black hispanic nodegree married re74 re75

collect clear
qui: table if treat==1, stat(freq) stat(mean `varnames') nototals 

foreach var in psid cps {
	qui: {
		forvalues i = 1/3 {
			ps_match `var'`i'_score
			drop select_*
			table if !mi(group_`var'`i'_score) & treat==0, stat(freq) nototals append 
		}
	}
}


foreach var1 in `varnames' {
	local j = 2
	foreach var in psid cps {
		qui: {
			forvalues i = 1/3 {
				reg `var1' i.treat if !mi(group_`var'`i'_score)
				scalar mean = _r_b[_cons]
				scalar se = _r_se[1.treat]
				scalar pval = _r_p[1.treat]
				collect get mean = mean  semean = se pval = pval, tags(cmdset[`j'] var[`var1'])
				local ++j	
			}
		}
	}
}

*********
* 修改格式
*********
collect dims
collect levelsof cmdset
collect levelsof result
collect levelsof var
collect levelsof colname

* add significance star
collect stars pval 0.1 "*" 0.05 "**" 0.01 "***", attach(mean)

* 行
collect style header result, level(hide)
collect style header cmdset, title(hide)
local colname `" "NSW" "MPSID-1" "MPSID-2" "MPSID-3" "MCPS-1" "MCPS-2" "MCPS-3" "'
local i = 1
foreach j in `colname' {
	collect label levels cmdset `i' "`j'", modify
	local ++i
}
collect recode result `"frequency"' = `"mean"'

* 列
collect recode var `"_hide"' = `"obs"'
local colname1 `" "No. of observations" "Age" "Education" "Black" "Hispanic" "No degree" "Married" "RE74(U.S.$)" "RE75(U.S.$)" "'
local varnames obs age education black hispanic nodegree married  re74  re75
local var_count  `:word count `varnames''
forvalues i = 1/`var_count' {
	local tmp `:word `i' of `varnames''
	local tmp1 `:word `i' of `colname1''
	collect label levels var `"`tmp'"' `"`tmp1'"', modify
}

* 数字格式
collect style cell var, warn halign(center) valign(center) nformat(%6.2fc)
collect style cell var[obs re74 re75],  nformat(%6.0fc)
collect style cell result[semean], sformat("[%s]")

* 布局
collect style cell border_block, border(right, pattern(nil))
collect title "Table 4. Sample Means of Characteristics for Matched Control Samples"

* 导出
collect layout  (cmdset#result[mean semean]) (var)
collect export "table4", as(docx) replace	
* 结果显示匹配的控制组在个体特征方面与实验组仍然存在巨大差异，再次表明Dehejia结果的不稳健性

*-----------------------------------------------------------------------------
* 删除中间数据
*-----------------------------------------------------------------------------
local files : dir . files "*.dta" // 遍历STATA格式文件
foreach f in `files' {
	local ff = substr(`"`f'"',-4,.)
	if inlist(`"`ff'"',"temp") {
		erase `f'
	}
}
