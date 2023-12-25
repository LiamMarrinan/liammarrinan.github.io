// Liam Marrinan Research Project

// RESEARCH TOPIC
// This is a research project I started investigating the impact of Dodd Frank 1502 on Gold Price Shocks and Their Influence on Conflict in the Democratic Republic of Congo

// DESCRIPTION
// This Do-File begins by cleaning and reorganizing a replication dataset(Mortality2013master.dta) which contains some of data I want, though has the wrong unit of observation.
// This replication data comes from the Paper: Unintended Consequences of Sanctions for Human Rights: Conflict Minerals and Infant Mortality by Dominic P. Parker, Jeremy D. Foltz, and David Elsea
// This Do-File creates MarrinanMortalityDataset.dta

// Note: DHS enumeration sites / clusters / villages - all refer to the variable dhsclust.

// Main Goals
// 1. Drop all unnecessary/unusable variables
// 2. Redefine all usable variables so the unit of observation is DHS enumeration sites instead of individuals
// 3. Create a panel dataset for all DHS villages 2007-2012
// 4. Add additional variables of interest
// 4. Regression analysis (differnce-in-differences)

// importing dataset
use "C:\Users\LEMARR24\Downloads\Mortality2013master.dta", clear

// Dropping varaibles - These variables are either unrelated to my research or are specific to individuals and not DHS villages.
drop childid caseid bidx weight cmc mother_age educ educ_years hhsize marital birth_month birth_year cmcbirth agedeath_months u1dead u0dead urban child_male married health_dist health_money bednet_mom bednet_kid prenatal_care birth_asst prenatal_visit prenatal_visit_ind postnatal_visit employed employed_partner postdiff3month prediff3month wealth_poorest wealth_poorer wealth_middle wealth_richer wealth_richest literate
drop dftreat_T10 dftreat_G10 dbter_T10 dbter_G10 dftreat_T20 dftreat_G20 dbter_T20 dbter_G20 dftreat_T30 dftreat_G30 dbter_T30 dbter_G30 dftreat_T40 dftreat_G40 dbter_T40 dbter_G40 dftreat_T50 dftreat_G50 dbter_T50 dbter_G50

// Dropping variables - These variables are village level and include the alternative distance thresholds used in Parker's paper
drop adm1name datum pstDFTTT20 pstDFG20 pstDFTTT20IND pstDFG20IND dftreat_M20 dftreat_T20IND dftreat_G20IND dftreat_T20IND dftreat_G20IND
drop DF TTT10 Gold10 TTT30 Gold30 TTT40 Gold40 TTT50 Gold50 TTT20_AG TTT20_FARDC wet dry dhsyear conflict12aall conflict12abat conflict12avac conflict3aall


// Modifying the urban_rura variable to a 0/1 indicator, where Urban = 1 if the enumeration site is in a village
gen Urban = 0
replace Urban = 1 if urban_rura=="U"

// Collapsing data 
sort dhsclust year
collapse (median) TTT20 Gold20 latnum longnum alt_gps alt_dem Policy_Territory banter doddter doddbanter dftreat TTT20_IND Gold20_IND Mine20_Ind Urban time , by(dhsclust year)

// Expanding to a monthly panel
sort dhsclust year
expand 12
bysort dhsclust year : gen month = _n 

// Adding time variable
drop time
sort dhsclust year month
by dhsclust : gen time = _n

// Merging in monthly gold price data (World Bank Commodity Price Data)
merge m:1 time using "C:\Users\LEMARR24\Downloads\GoldPriceData.dta"
drop Year Month

// Merging CPI data (OECD)
merge m:1 time using "C:\Users\LEMARR24\Downloads\cpiadjuster.dta", gen(m3)
drop if m3!=3

// Conflict Data merge (ACLED DATA)
// Conflict data created in R using 20km threshold 
merge 1:m dhsclust month year using "C:\Users\LEMARR24\Downloads\villages-conflict-expanded-1.dta", gen(m2)
drop if m2!=3

// DID Creation

// Generating Treatment variable
gen Treat = Policy_Territory*Gold20_IND

// Generating Post variable
gen Post = (time>42)

// Generating additional conflict variables to account for nonlinear relationships
gen lnconflict = ln(conflict)
gen conflictdummy = (conflict>=1)

// Adjusting for inflation
gen CPIindicator = CPI / CPI[1]
gen GoldTroyozAdj =  GoldTroyoz / CPIindicator
gen Treatpostgold = Treat*Post*GoldTroyozAdj
gen conflict1 = conflict + 1
gen lnconflict1 = ln(conflict1)
gen gtreat = GoldTroyozAdj * Treat
gen gpost = GoldTroyozAdj * Post
gen treatpost = Treat * Post

//Gold Percent Change Variable Generation
sort dhsclust year
by dhsclust: gen GoldChange = 100 * (GoldTroyozAdj[_n] - GoldTroyozAdj[_n-1]) / GoldTroyozAdj[_n-1]
gen GoldChange2 = GoldChange*GoldChange
gen GoldChangeTreatPost = GoldChange*Treat*Post
rename treatpost TreatPost

// Panel creation
xtset dhsclust time
 
//Initial regression for pelimary regression presentation
xtreg conflict Treat Post TreatPost GoldTroyozAdj Treatpostgold i.time, fe vce(cluster dhsclust)

//Fixed vs. Random Effects Test
//due to large number of time-invariant variables the hausman test results may be unrelaible, though I still include the tests
//I am currently experimenting with other possilble test (wald/LR)
xtreg conflictdummy Treat Post GoldChange TreatPost GoldChangeTreatPost GoldChange2 Urban i.time, re
estimates store random
xtreg conflictdummy Treat Post GoldChange TreatPost GoldChangeTreatPost GoldChange2 Urban i.time, fe
estimates store fixed
hausman fixed random, sigmamore

//Parrallel Trends Graph - This cannot be run by default, as it collapses the data
*collapse (sum) conflict_events = conflictdummy, by(time Treat)
*twoway (line conflict_events time if Treat == 0, lcolor(blue) sort)(line conflict_events time if Treat == 1, lcolor(red) sort), xline(43) legend(order(1 "Control" 2 "Treat"))

//DID Regression - This regression is used for the results section of my paper
xtreg conflictdummy Treat Post GoldChange TreatPost GoldChangeTreatPost GoldChange2 Urban i.time, vce(cluster dhsclust)

//labeling variables used in regression
label variable conflictdummy "Conflict indicator -- = 1 if dhsclust experiences a conflict within 20km"
label variable Treat "Treatment variable --  =1 if a treatment village"
label variable Post "Post period indicator -- =1 after July 2010"
label variable GoldChange "Gold Price Monthly Percent Change"
label variable GoldChange2 "Gold Price Monthly Percent Change Squared"
label variable GoldTroyozAdj "Average monthly gold price (Real USD) (adjusted with CPI)"
label variable Urban "Urban indicator variable =1 if dhsclust is in city"
label variable time "Time variable (1-72) (Monthly)
label variable year "Year variable (2007-2012)


