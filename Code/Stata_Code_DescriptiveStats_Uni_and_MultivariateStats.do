/*RESEARCH QUESTIONS*/
relationship between GBS prsence and all other variables
relationship between  ng GBS DNA/ng starting DNA and all other variables

*GBS BINARY Definition*/
gen GBS_new=.
replace GBS_new=1 if GBSPresence=="Yes"
replace GBS_new=0 if GBSPresence=="No"
tab GBS_new

/*Duplicates drop for regression*/
rename spid SPID
drop if SPID==.
drop if SPID=="spid"
destring SPID, replace
duplicates drop SPID, force

drop if _merge==2
drop _merge


/*GBS CONTINUOUS DEFINITION*/
replace AvgCopyNumbercopiesng="." if AvgCopyNumbercopiesng=="N/A"
destring AvgCopyNumbercopiesng, replace
drop if AvgCopyNumbercopiesng==.


/*REPLACE STRINGS*/
foreach var of varlist DHQ145000-DHQ151000 {
                replace `var'="." if `var'=="*"
       }	      
foreach var of varlist DHQ145000-DHQ151000 {
                replace `var'="1" if `var'=="a"
				replace `var'="2" if `var'=="b"
				replace `var'="3" if `var'=="c"
				replace `var'="4" if `var'=="d"
				replace `var'="5" if `var'=="e"
				replace `var'="6" if `var'=="f"

       }	    

 foreach var of varlist HMI020 RHQ010 RHQ060 RHQ540 SIQ171 SIQ190 HMI010 HMI025 RHQ042 RHQ13 SIQ170 SIQ180 {
                replace `var'="." if `var'=="R"
				replace `var'="." if `var'=="D"
				replace `var'="." if `var'=="C"


       }
 foreach var of varlist INCOME_HH_MAX INCOME_HH_MID INCOME_HH_MIN INCOME_INDIV_MAX INCOME_INDIV_MID INCOME_INDIV_MIN {
                replace `var'="." if `var'=="R"
				replace `var'="." if `var'=="D"

       }
foreach var of varlist HHQ150 HHQ210 HHQ480 HMI100 HHQ120 HHQ160 HHQ230_R2 HMI070 WHQ040 HHQ130 HHQ180 SDQ270 HMI080 WHQ070_R2 {
                replace `var'="." if `var'=="D"
       }
replace SDQ270="." if SDQ270=="R"

foreach var of varlist INCOME_HH_MAX INCOME_HH_MID INCOME_HH_MIN INCOME_INDIV_MAX INCOME_INDIV_MID INCOME_INDIV_MIN {
                replace `var'="." if `var'=="R"
				replace `var'="." if `var'=="D"

       }

replace SXQ011="." if SXQ011=="R"
replace SXQ011="." if SXQ011=="D"

destring, replace

/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/

/*DESCRIPTIVE STATISTICS - BINARY/CATEGORICAL*/
tab2 GENDER GBS_new, chi2 row miss
tab2 CENSUS_UAUC_2010 GBS_new, chi2 row miss
tab2 RACE_ETHNICITY_4CAT GBS_new, chi2 row miss
tab2 RACE_7CAT GBS_new, chi2 row miss
tab2 CANCER GBS_new, chi2 row miss
tab2 OSTEOARTHRITIS GBS_new, chi2 row miss
tab2 DIABETES_2 GBS_new, chi2 row miss
tab2 HHQ480 GBS_new, chi2 row miss
tab2 HHQ580_26 GBS_new, chi2 row miss
tab2 HMI070 GBS_new, chi2 row miss
tab2 INC037_R GBS_new, exact row miss
tab2 RHQ543 GBS_new, exact row miss
tab2 SMQ_DER_FORMER_NEVER_CURRENT_R2 GBS_new, exact row miss


/*DESCRIPTIVE STATISTICS - CONTINUOUS/NUMERIC*/
tabstat ALANINE ARGININE ASH ASPARTIC_ACID CALC  CHOLINE COPPER  D_CHEESE D_TOTAL DHQ149000 DHQ152003 DHQ153012 DIETARY_FIBER FPED_PF_MEAT FPED_PF_ORGAN FPED_PF_SEAFD_LOW FPED_V_STARCH_TOT FPED_V_TOTAL  GLUTAMIC_ACID GLYCINE HISTIDINE INSOLUBLE_DIETARY_FIBER M221 MAGNESIUM MFA_18_1_OCTADECENOIC_ACID MFA_20_1_EICOSENOIC_ACID MUFA_14_1_MYRISTOLEIC_ACID NATURAL_ALPHA_TOCOPHEROL NITROGEN OMEGA_3_FATTY_ACIDS P183 PF_LEGUMES PHENYLALANINE PHOS POTASSIUM PROTEIN PSH040 PSH080 PSH090 RHQ544 SELE SELENIUM SERINE SFA_16_0_HEXADECANOIC_ACID SFA_18_0_OCTADECANOIC_ACID SMQ_DER_PACK_YEAR_R2 SUCRALOSE SUPP_VITAMIN_B12 THREONINE TOTAL_FAT TOTAL_MONOUNSATURATED TOTAL_PROTEIN TOTAL_SATURATED_FATTY_ACIDS TOTAL_VITAMIN_A_MCG TRANS_16_1_HEXADECENOIC TRYPTOPHAN VALINE VB6 VEGETABLE_PROTEIN VEGETABLES_ADJUST_FREQ VITAMIN_B12 V_LEGUMES XYLITOL  SPID GBS_new, by(GBS_new) stat(median iqr) long format

bysort GBS_new: missings report
missings table

/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*TABLE 1 DEMOGRAPHICS - BINARY OUTCOME UNIVARIATE*/
frame create results str32 Independentvariable float (OR STD_ERR Z PVALUE LB95 UB95)
foreach v of varlist AGE_CONSENT-SXQ011 {
    logistic GBS_new `v'
    matrix M = r(table)
    frame post results ("`v'") (M["b", "GBS_new:`v'"]) (M["se", "GBS_new:`v'"]) ///
        (M["z", "GBS_new:`v'"]) (M["pvalue", "GBS_new:`v'"]) (M["ll", "GBS_new:`v'"]) ///
        (M["ul", "GBS_new:`v'"])
}

frame change results
format OR STD_ERR LB UB %4.1f
format Z PVALUE %05.3f
quietly compress
list, noobs clean



/*TABLE 1 PFED - BINARY OUTCOME UNIVARIATE*/
frame create results str32 Independentvariable float (OR STD_ERR Z PVALUE LB95 UB95)
foreach v of varlist FPED_ADD_SUGARS-FPED_V_TOTAL {
    logistic GBS_new `v'
    matrix M = r(table)
    frame post results ("`v'") (M["b", "GBS_new:`v'"]) (M["se", "GBS_new:`v'"]) ///
        (M["z", "GBS_new:`v'"]) (M["pvalue", "GBS_new:`v'"]) (M["ll", "GBS_new:`v'"]) ///
        (M["ul", "GBS_new:`v'"])
}
frame change results
format OR STD_ERR LB UB %5.3f
format Z PVALUE %05.3f
quietly compress
list, noobs clean



/*TABLE 1 HEI - BINARY OUTCOME UNIVARIATE*/
frame create results str32 Independentvariable float (OR STD_ERR Z PVALUE LB95 UB95)
foreach v of varlist HEI2015C1_TOTALVEG-HEI2015_TOTAL_SCORE {
    logistic GBS_new `v'
    matrix M = r(table)
    frame post results ("`v'") (M["b", "GBS_new:`v'"]) (M["se", "GBS_new:`v'"]) ///
        (M["z", "GBS_new:`v'"]) (M["pvalue", "GBS_new:`v'"]) (M["ll", "GBS_new:`v'"]) ///
        (M["ul", "GBS_new:`v'"])
}
frame change results
format OR STD_ERR LB UB %2.1f
format Z PVALUE %05.3f
quietly compress
list, noobs clean



/*TABLE 1 INDIVIDUAL DIET - BINARY OUTCOME UNIVARIATE*/
frame create results str32 Independentvariable float (OR STD_ERR Z PVALUE LB95 UB95)
foreach v of varlist DHQ145000-ADDSUG_PERC {
    logistic GBS_new `v'
    matrix M = r(table)
    frame post results ("`v'") (M["b", "GBS_new:`v'"]) (M["se", "GBS_new:`v'"]) ///
        (M["z", "GBS_new:`v'"]) (M["pvalue", "GBS_new:`v'"]) (M["ll", "GBS_new:`v'"]) ///
        (M["ul", "GBS_new:`v'"])
}
frame change results
format OR STD_ERR LB UB %2.1f
format Z PVALUE %05.3f
quietly compress
list, noobs clean




/*TABLE 1 INDIVIDUAL DISEASE - BINARY OUTCOME UNIVARIATE*/
frame create results str32 Independentvariable float (OR STD_ERR Z PVALUE LB95 UB95)
foreach v of varlist ALQ_NUM_DRINKS_R2-WHQ145_R2 {
    logistic GBS_new `v'
    matrix M = r(table)
    frame post results ("`v'") (M["b", "GBS_new:`v'"]) (M["se", "GBS_new:`v'"]) ///
        (M["z", "GBS_new:`v'"]) (M["pvalue", "GBS_new:`v'"]) (M["ll", "GBS_new:`v'"]) ///
        (M["ul", "GBS_new:`v'"])
}
frame change results
format OR STD_ERR LB UB %2.1f
format Z PVALUE %05.3f
quietly compress
list, noobs clean



/*TABLE 1 SUMMARY DISEASE - BINARY OUTCOME UNIVARIATE*/
frame create results str32 Independentvariable float (OR STD_ERR Z PVALUE LB95 UB95)
foreach v of varlist CHRONIC_COND_5CAT-CANCER {
    logistic GBS_new `v'
    matrix M = r(table)
    frame post results ("`v'") (M["b", "GBS_new:`v'"]) (M["se", "GBS_new:`v'"]) ///
        (M["z", "GBS_new:`v'"]) (M["pvalue", "GBS_new:`v'"]) (M["ll", "GBS_new:`v'"]) ///
        (M["ul", "GBS_new:`v'"])
}
frame change results
format OR STD_ERR LB UB %2.1f
format Z PVALUE %05.3f
quietly compress
list, noobs clean




/*TABLE 1 REPRODUCTIVE HEALTH - BINARY OUTCOME UNIVARIATE*/
keep if GENDER==2
frame create results str32 Independentvariable float (OR STD_ERR Z PVALUE LB95 UB95)
foreach v of varlist LIFE_12-LARGE_BABY {
    logistic GBS_new `v'
    matrix M = r(table)
    frame post results ("`v'") (M["b", "GBS_new:`v'"]) (M["se", "GBS_new:`v'"]) ///
        (M["z", "GBS_new:`v'"]) (M["pvalue", "GBS_new:`v'"]) (M["ll", "GBS_new:`v'"]) ///
        (M["ul", "GBS_new:`v'"])
}
frame change results
format OR STD_ERR LB UB %2.1f
format Z PVALUE %05.3f
quietly compress
list, noobs clean


/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/

/*TABLE 2 DEMOGRAPHICS - CONTINUOUS OUTCOME UNIVARIATE*/
frame create results str32 Independentvariable float (COEF STD_ERR T PVALUE LB95 UB95)
foreach v of varlist AGE_CONSENT-SXQ011 {
    regress AvgCopy `v'
    matrix M = r(table)
    frame post results ("`v'") (M["b", "`v'"]) (M["se", "`v'"]) ///
        (M["t", "`v'"]) (M["pvalue", "`v'"]) (M["ll", "`v'"]) ///
        (M["ul", "`v'"])
}
frame change results
format COEF STD_ERR LB UB %2.1f
format T PVALUE %05.3f
quietly compress
list, noobs clean


/*TABLE 2 PFED - CONTINUOUS OUTCOME UNIVARIATE*/
frame create results str32 Independentvariable float (COEF STD_ERR T PVALUE LB95 UB95)
foreach v of varlist FPED_ADD_SUGARS-FPED_V_TOTAL{
    regress AvgCopy `v'
    matrix M = r(table)
    frame post results ("`v'") (M["b", "`v'"]) (M["se", "`v'"]) ///
        (M["t", "`v'"]) (M["pvalue", "`v'"]) (M["ll", "`v'"]) ///
        (M["ul", "`v'"])
}
frame change results
format COEF STD_ERR LB UB %2.1f
format T PVALUE %05.3f
quietly compress
list, noobs clean


/*TABLE 2 HEI - CONTINUOUS OUTCOME UNIVARIATE*/
frame create results str32 Independentvariable float (COEF STD_ERR T PVALUE LB95 UB95)
foreach v of varlist HEI2015C1_TOTALVEG-HEI2015_TOTAL_SCORE {
    regress AvgCopy `v'
    matrix M = r(table)
    frame post results ("`v'") (M["b", "`v'"]) (M["se", "`v'"]) ///
        (M["t", "`v'"]) (M["pvalue", "`v'"]) (M["ll", "`v'"]) ///
        (M["ul", "`v'"])
}
frame change results
format COEF STD_ERR LB UB %2.1f
format T PVALUE %05.3f
quietly compress
list, noobs clean


/*TABLE 2 INDIVIDUAL DIET - CONTINUOUS OUTCOME UNIVARIATE*/
frame create results str32 Independentvariable float (COEF STD_ERR T PVALUE LB95 UB95)
foreach v of varlist DHQ145000-ADDSUG_PERC {
    regress AvgCopy `v'
    matrix M = r(table)
    frame post results ("`v'") (M["b", "`v'"]) (M["se", "`v'"]) ///
        (M["t", "`v'"]) (M["pvalue", "`v'"]) (M["ll", "`v'"]) ///
        (M["ul", "`v'"])
}
frame change results
format COEF STD_ERR LB UB %2.1f
format T PVALUE %05.3f
quietly compress
list, noobs clean


/*TABLE 2 INDIVIDUAL DISEASE - CONTINUOUS OUTCOME UNIVARIATE*/
frame create results str32 Independentvariable float (COEF STD_ERR T PVALUE LB95 UB95)
foreach v of varlist ALQ_NUM_DRINKS_R2-WHQ145_R2 {
    regress AvgCopy `v'
    matrix M = r(table)
    frame post results ("`v'") (M["b", "`v'"]) (M["se", "`v'"]) ///
        (M["t", "`v'"]) (M["pvalue", "`v'"]) (M["ll", "`v'"]) ///
        (M["ul", "`v'"])
}
frame change results
format COEF STD_ERR LB UB %2.1f
format T PVALUE %05.3f
quietly compress
list, noobs clean


/*TABLE 2 SUMMARY DISEASE - CONTINUOUS OUTCOME UNIVARIATE*/
frame create results str32 Independentvariable float (COEF STD_ERR T PVALUE LB95 UB95)
foreach v of varlist CHRONIC_COND_5CAT-CANCER {
    regress AvgCopy `v'
    matrix M = r(table)
    frame post results ("`v'") (M["b", "`v'"]) (M["se", "`v'"]) ///
        (M["t", "`v'"]) (M["pvalue", "`v'"]) (M["ll", "`v'"]) ///
        (M["ul", "`v'"])
}
frame change results
format COEF STD_ERR LB UB %2.1f
format T PVALUE %05.3f
quietly compress
list, noobs clean




/*TABLE 2 REPRODUCTIVE HEALTH - CONTINUOUS OUTCOME UNIVARIATE*/
frame create results str32 Independentvariable float (COEF STD_ERR T PVALUE LB95 UB95)
foreach v of varlist LIFE_12-LARGE_BABY {
    regress AvgCopy `v'
    matrix M = r(table)
    frame post results ("`v'") (M["b", "`v'"]) (M["se", "`v'"]) ///
        (M["t", "`v'"]) (M["pvalue", "`v'"]) (M["ll", "`v'"]) ///
        (M["ul", "`v'"])
}
frame change results
format COEF STD_ERR LB UB %2.1f
format T PVALUE %05.3f
quietly compress
list, noobs clean


/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------*/

/*TABLE 3 - BINARY OUTCOME MULTIVARIATE MODELS*/
logistic GBS_new DHQ153012-D_CHEESE
logistic GBS_new AGE_CONSENT-D_CHEESE
logistic GBS_new DHQ153012-SXQ010
logistic GBS_new DHQ153012-HMI070
logistic GBS_new AGE_CONSENT-HMI070


/*TABLE 3 - CONTINUOUS OUTCOME MULTIVARIATE MODELS*/
regress AvgCopy DHQ149000 DHQ152003 SUPP_VITAMIN_B12
regress AvgCopy AGE_CONSENT DHQ149000 DHQ152003 SUPP_VITAMIN_B12
regress AvgCopy SXQ010 DHQ149000 DHQ152003 SUPP_VITAMIN_B12
regress AvgCopy HMI070 DHQ149000 DHQ152003 SUPP_VITAMIN_B12
regress AvgCopy AGE_CONSENT SXQ010 HMI070 DHQ149000 DHQ152003 SUPP_VITAMIN_B12
