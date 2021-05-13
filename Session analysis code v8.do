//Danielle Ducket Request - 3-16-21
//Request:
/*	• Re-run "members introducing bipartisan legislation list", sent on 11/8/20
		○ Full Session, and YTD 21-22
	• List all bills where 1st sponsor is the opposite party
		○ Both years
	• List of all bills referred to judiciary in House
		○ Both Sessions
*/




//Reproduction of Original Table
//tab sponsor2 bipartisan_flag2 if sequence5==1 & body=="H" & type=="B"



/***********************************************
Pennsylvania General Assembly Data Analysis Code
	Eric Dice, House Appropriations Committee (D)
	March 2021
	
	This update is to assist Danielle Ducket/Rep. Rabb with a data request.
	
	Specific questions requested by Danielle for 3/15 request
	• Re-run "members introducing bipartisan legislation list", sent on 11/8/20
		○ Full Session, and YTD 21-22
	• List all bills where 1st sponsor is the opposite party
		○ Both years
	• List of all bills referred to judiciary in House
		○ Both Sessions
	
	

Acknowledgements: 
	Michael Passiment, Administrative Office of Pennsylvania Courts; Penn State Harrisburg
	Brent McClintock, Legislative Data Processing Center
	Chloe Bohm, House Appropriations Committee (D)
	Brad Keen, House Appropriations Committee (D)
	Danielle Duckett, office of Rep. Chris Rabb for the member demographic data


Data source: https://www.legis.state.pa.us/data/

Data Loading Procedure:
1) Open the XML file into Excel to tabularize, 
2) correct all the variable names to be lower case, 
3) save the file then
4) Run the stata import command 
5) Save as .dta file
6) Run other commands as needed

Known Issues with the data import procedure to watch:
	- import method can affect the capitalization of the variable names, which can upset Stata. Fix by being consistent.
	- Excel can get messed up with a long short title, and turn on word-wrap. This messes up the procedure by which the bill data is replicated to subsequent entries, and affect the counting for certain events (bill movement, for example)


***********************************************/
frame reset
clear
set more off




//import excel "C:\Users\laptop\Desktop\Session Analytics\Rabb\Session Data as of 3-31-21.xlsx", sheet("Sheet1") firstrow

//use "C:\Users\laptop\Desktop\Session Analytics\Rabb\PA-Bill-History-2019-RegularSession 12-31-20.dta", clear

//use "C:\Users\laptop\Desktop\Session Analytics\Rabb\PA-Bill-History-2021-RegularSession 3-15-21.dta", clear

use "C:\Users\laptop\Desktop\Session Analytics\Rabb\PA-Bill-History-2021-RegularSession 3-31-21.dta", clear

**NOTE!!! - the party series split hangs...adjust accordingly


/*
NOTES
1) Be careful using number, since resolutions and bills can have the same number. id is better for a unique bill identifier
2) How LDP counts joint resolutions will not align with "enacted" via act number
3) You'll need to update the graph labels each time
*/

//restrict to HBs only
//drop if body == "S"
//keep if type == "B"

//original order
gen original_order = _n

//Convert date format
gen date1= date(date,"MD20Y") //adjust this argument if looking at session data pre-2000
format date1 %tdnn/dd/YY


// Bills and Resolutions Introduced - Both Chambers
tab description  if fillsequence == . & sequence==1 // Fill Sequence may be "", if a different import method is used that produces null rather than missing for blank cells.

//Assign sponsor party to all bill entries
gen party2 = 0
replace party2 = 1 if sequencenumber==1 & party == "R"
replace party2 = 2 if sequencenumber==1 & party == "D"
replace party2 = 3 if sequencenumber==1 & party =="" & sponsor == "PRIME SPONSOR WITHDREW"
egen all_party = max(party2), by(id)
label define partylabel 0 "Other" 1 "R" 2 "D" 3 "Prime Sponsor Withdrew"
label values all_party partylabel

//assign bill sponsor to all bill entries. The prime sponsor is always in the first location of the bill's action array.
sort original_order
egen groupid = group(id)
gen sponsor2 = sponsor if sequencenumber ==1
sort groupid
by groupid: replace sponsor2 = sponsor2[1]


/* Join external characteristics to the dataset
frame create member_demographics
frame change member_demographics
import excel "C:\Users\laptop\Desktop\Session Analytics\Rabb\dd_demographic_data_flat-2.xlsx", sheet("Sheet1") firstrow
frame change default
frlink m:1 sponsor2, frame(member_demographics)
frget dd_gender dd_party dd_eth dd_poc dd_yr_elected dd_years_in_office dd_freshman dd_leader_chair nominate1 nominate2 majority_chair minority_chair committee_chair,  from(member_demographics)
*/

// House bills by committee of original referral
tab committee all_party if sequence5==1 & description=="House Bill"
gen HB_referred_House = .
replace HB_referred_House =1 if sequence5==1 & description=="House Bill"
egen all_HB_referred_House = max(HB_referred_House), by(id)

// House bills reported from committee
tab committee all_party  if  description == "House Bill" & actionchamber == "H" & (verb == "Reported as committed," | verb == "Reported as amended,")
gen HB_reported =.
replace HB_reported =1  if  description == "House Bill" & actionchamber == "H" & (verb == "Reported as committed," | verb == "Reported as amended,")

// House bills receiving first consideration
tab sessionyear if description == "House Bill" & verb == "First consideration," & actionchamber == "H"
gen HB_firstconsideration_house =1 if description == "House Bill" & verb == "First consideration," & actionchamber == "H"
tab HB_firstconsideration_house
egen all_HB_firstconsideration_house = max(HB_firstconsideration_house), by(id)

//House bills receiving second consideration
tab sessionyear if description == "House Bill" & (verb == "Second consideration," | verb == "Second consideration, with amendments,") & actionchamber == "H"
tab sessionyear if description == "House Bill" & verb == "Second consideration, with amendments," & actionchamber == "H"
gen HB_secondconsideration_house =.
replace HB_secondconsideration_house = 1 if description == "House Bill" & (verb == "Second consideration," | verb == "Second consideration, with amendments,")& actionchamber == "H" 

egen all_HB_secondconsideration_house = max(HB_secondconsideration_house), by(id)


// House bills receiving final passage
tab sessionyear if description == "House Bill" & (verb == "Final passage," | verb == "Third consideration and final passage,") & actionchamber == "H"
gen HB_finalpassage_house =.
replace HB_finalpassage_house = 1 if description == "House Bill" & (verb == "Final passage," | verb == "Third consideration and final passage,") & actionchamber == "H"
egen all_HB_finalpassage_house = max(HB_finalpassage_house), by(id)
tab sessionyear if HB_finalpassage_house==1

//House bills referred to senate committee
//may double capture bills that went back twice on concurrence
tab committee if description == "House Bill" & all_HB_finalpassage_house ==1 & verb == "Referred to" & actionchamber =="S"
gen HB_senatereferred =1 if description == "House Bill" & all_HB_finalpassage_house ==1 & verb == "Referred to" & actionchamber =="S"
egen all_HB_senatereferred = max(HB_senatereferred), by(id)

//Bills reported from Senate committee
//May double count bills in Rules, but I don't think so
tab committee if  description == "House Bill" & actionchamber == "S" & (verb == "Reported as committed," | verb == "Reported as amended,")


//House bills getting first consideration in senate
tab sessionyear if description == "House Bill" & verb == "First consideration," & actionchamber == "S"
gen HB_firstconsideration_senate =1 if description == "House Bill" & verb == "First consideration," & actionchamber == "S"
tab HB_firstconsideration_senate
egen all_HB_firstconsideration_senate = max(HB_firstconsideration_senate), by(id)


//House bills receiving second consideration in senate
tab sessionyear if description == "House Bill" & (verb == "Second consideration," | verb == "Second consideration, with amendments,") & actionchamber == "S"
gen HB_secondconsideration_senate =.
replace HB_secondconsideration_senate = 1 if description == "House Bill" & (verb == "Second consideration," | verb == "Second consideration, with amendments,") & actionchamber == "S"
egen all_HB_second_senate = max(HB_secondconsideration_senate), by(id)

//House bills receiving final passage in the senate
tab sessionyear if description == "House Bill" & (verb == "Final passage," | verb == "Third consideration and final passage,") & actionchamber == "S"
gen HB_finalpassage_senate =.
replace HB_finalpassage_senate = 1 if description == "House Bill" & (verb == "Final passage," | verb == "Third consideration and final passage,") & actionchamber == "S"
egen all_HB_finalpassage_senate = max(HB_finalpassage_senate), by(id)
tab sessionyear if HB_finalpassage_senate==1

//House bills that passed both chambers
tab session year if description == "House Bill" & verb == "Presented to the Governor,"
gen HB_sent_to_gov =.
replace HB_sent_to_gov = 1 if description == "House Bill" & verb == "Presented to the Governor,"
egen all_HB_sent_to_gov = max(HB_sent_to_gov), by(id)

//House bills enacted into law
tab session year if description == "House Bill" & verb == "Act No."
gen HB_enacted =.
replace HB_enacted =1 if description == "House Bill" & verb == "Act No."
egen all_HB_enacted = max(HB_enacted), by(id)


// Bills residing at each step in the process. Does not generate the "progression curve", i.e., it's not a cumulative distribution
gen house_process_2 =.
replace house_process_2 = 1 if all_HB_referred_House  ==1
replace house_process_2 = 2 if all_HB_firstconsideration_house ==1
replace house_process_2 = 3 if all_HB_secondconsideration_house ==1
replace house_process_2 = 4 if all_HB_finalpassage_house ==1
replace house_process_2 = 5 if all_HB_firstconsideration_senate ==1 
replace house_process_2 = 6 if all_HB_second_senate ==1
replace house_process_2 = 7 if all_HB_finalpassage_senate ==1
replace house_process_2 = 8 if all_HB_sent_to_gov ==1
replace house_process_2 = 9 if all_HB_enacted ==1

label define house_process_2_l 1 "Currently In Committee - House" 2 "On First- House " 3 "On Second - House" 4 "Final - House" 5 "On First - Senate" 6 "On Second - Senate" 7 "Final - Senate" 8 "Sent to Gov" 9 "Enacted"
label values house_process_2 house_process_2_l

//Generate cosponsor counts
egen cosponsor_count = count(sponsor), by(id)
// -- Counts (D)
egen cosponsor_count_d = count(sponsor) if party == "D", by(id)
replace cosponsor_count_d = 0 if cosponsor_count_d ==.
egen cosponsor_count_d_all = max(cosponsor_count_d), by(id)
// -- Counts (R)
egen cosponsor_count_r = count(sponsor) if party == "R", by(id)
replace cosponsor_count_r = 0 if cosponsor_count_r ==.
egen cosponsor_count_r_all = max(cosponsor_count_r), by(id)

//Variable switch
replace cosponsor_count_d = cosponsor_count_d_all
replace cosponsor_count_r = cosponsor_count_r_all
drop cosponsor_count_d_all
drop cosponsor_count_r_all
//Percent of cosponsors D and R
gen cosponsor_pct_d = cosponsor_count_d / cosponsor_count
gen cosponsor_pct_r = cosponsor_count_r / cosponsor_count

//Enacted flag
gen enacted2 = 0
replace enacted2 = 1 if all_HB_enacted==1


//process_any flag, although in practice, this is equivalent to the counts reported from committee
gen process_any = 0
replace process_any = 1 if inrange(house_process_2,2,9)
label define process_any_l 0 "no movement" 1 "any movement"
label values process_any process_any_l

// Flags for standing committee (used later to measure how busy the committees are)

//-- Assign standing committee of origin to all bills
sort sequence5
gen standing_committee = committee if sequence5 ==1
sort id sequence5
by id: replace standing_committee = standing_committee[1]
sort original_order

//Flag for bills introduced by Saylor that are budget bills coming out of appropriations
//This is a special situation, because the budget package is a large group of bills
gen saylorflag =0
replace saylorflag = 1 if inlist(sponsor2,"SAYLOR","BRADFORD") & standing_committee == "APPROPRIATIONS"
label define saylorflagl 0 "All Other Bills" 1 "Saylor/Bradford out of Approps"
label values saylorflag saylorflagl

//Split the party variable into separate series
separate session, by(all_party) generate(party_series)
//rename party_series0 all_party_other
rename party_series1  all_party_R
rename party_series2  all_party_D
//rename party_series3  all_party_withdrawn
 


	
*******************************************
// Graphs
*******************************************

// House bill process chart - Total;
/*#delimit ;
graph bar (count) 	HB_referred_House HB_firstconsideration_house HB_secondconsideration_house HB_finalpassage_house 
					HB_firstconsideration_senate HB_secondconsideration_senate HB_finalpassage_senate HB_sent_to_gov HB_enacted, 
	plotregion(margin(0 15 0 5))
	blabel(total, size(small) format(%8.0fc))
	bargap(5)
	bar(1, color(navy*.95)) 
	bar(2, color(navy*.90))
	bar(3, color(navy*.85))
	bar(4, color(navy*.80))
	bar(5, color(gold*.75))
	bar(6, color(gold*.70))
	bar(7, color(gold*.65))
	bar(8, color(red*.60))
	bar(9, color(red*.55))

	title("2019/20 House Bills Clearing Each Procedural Step")
	subtitle("2019/20 Session through 11/26/20", size(small))
	legend(
		order (1 5 2 6 3 7 4 8 0 9) 
		label(1 "House Bills Introduced") 
		label(2 "First Consideration") 
		label(3 "Second Consideration") 
		label(4 "Final Passage - House") 
		label(5 "Senate First Consideration") 
		label(6 "Senate Second") 
		label(7 "Senate Final") 
		label(8 "Sent to Governor") 
		label(9 "Enacted into Law")
		)
		graphregion(color(white))
	name(HouseBillProcess,replace);
#delimit cr

*/




// Cosponsorship Plots


// bills with republican cosponsors
sort cosponsor_count_r
//list id sponsor2 cosponsor_count_d cosponsor_count_r cosponsor_count if dd_party =="Democrat" & cosponsor_count_r >0 & cosponsor_count_r !=. & type=="B" & HB_referred_House ==1


//tab cosponsor_count_r if dd_party =="Democrat"  & type=="B" & HB_referred_House ==1
 


by process_any, sort : summarize cosponsor_count if HB_referred_House==1 & type=="B", detail

gen dflag = 0
replace dflag = 1 if all_party == 2

gen rflag = 0
replace rflag = 1 if all_party == 1



//Calculate standing committee workloads
tab standing_committee if HB_firstconsideration_house ==1
bysort standing_committee: egen standing_committee_count = count(all_HB_firstconsideration_house) if sequence5 ==1
bysort standing_committee: egen standing_committee_count_ref = count(all_HB_referred_House) if sequence5 ==1

//Number of bills reported from standing committee, assigned to all bill entries
egen standing_committee_count2 = max(standing_committee_count), by(id)
egen standing_committee_count_ref2 = max(standing_committee_count_ref), by(id)
gen standing_pct_reported = standing_committee_count2 / standing_committee_count_ref2
sort original_order

// Percent of total bills assigned to a standing committee
egen total_bills_count = count(id) if body=="H" & type == "B" & HB_referred_House ==1
egen total_bills_count2 = max(total_bills_count), by(id)
gen standing_referred_pct_hb_total = standing_committee_count_ref2 / total_bills_count2
gen standing_ref_pct = standing_referred_pct_hb_total *100



//Danielle's bipartisan flag
gen bipartisan_flag = .
replace bipartisan_flag = 0 if body =="H" & type == "B"
replace bipartisan_flag = 1 if all_party == 2 & cosponsor_count_r > 0 & body =="H" & type == "B"
replace bipartisan_flag = 2 if all_party == 1 & cosponsor_count_d > 0 & body =="H" & type == "B"
replace bipartisan_flag = 3 if all_party == 3 & body =="H" & type == "B"
tab bipartisan_flag if sequence5 == 1

label define bipartisanl 0 "0: bills with no bipartisan cosponsors" 1 "1: d bills with r cosponsors >0" 2 "2: r bills with d cosponsors >0" 3 "3: prime sponsor withdrawn"
label values bipartisan_flag bipartisanl


gen bipartisan_flag2 = 0
replace bipartisan_flag2 =1 if inlist(bipartisan_flag,1,2)
label define bipartisan_flag2l 0 "Not Bipartisan" 1 "Bipartisan"
label values bipartisan_flag2 bipartisan_flag2l


//count of bills that are bipartisan or not
tab sponsor2 bipartisan_flag2 if sequence5 ==1 & body =="H" & type == "B"

//Danielle question 11/25/20
/*
Bills that have 20 co-prime from both democratic party and republic party.
All legislation that has a democratic and republican lead co-prime
*/

//20+ cosponsors from each party
gen cosponsor2020 = 0
replace cosponsor2020 = 1 if cosponsor_count_d >=20 & cosponsor_count_r >=20
tab party2 if cosponsor2020 ==1 & HB_referred_House ==1 

// Democratic and Republican Coprime
sort original_order
gen primesponsor = party if sequencenumber ==1
sort groupid
by groupid: replace primesponsor = primesponsor[1]

sort original_order
gen coprime = party if sequencenumber == 2
gen coprimesponsor = sponsor if sequencenumber ==2
sort groupid
by groupid: replace coprime = coprime[2]
by groupid: replace coprimesponsor = coprimesponsor[2]
sort original_order

gen bipartisan_coprime = 0
replace bipartisan_coprime = 1 if primesponsor == "R" & coprime == "D"
replace bipartisan_coprime = 1 if primesponsor == "D" & coprime == "R"

tab party2 bipartisan_coprime if HB_referred_House ==1


//Danielle Questions - 3/15/21
//tab sponsor2 bipartisan_flag2 if sequence5 ==1 & body =="H" & type == "B"
//list id sponsor2 if bipartisan_coprime == 1 & HB_referred_House ==1
//list id sponsor2 year if body== "H" & type=="B" & HB_referred_House ==1 & standing_committee== "JUDICIARY"

//Maeve and Danielle's questions 3/31/21
//Request bills referred to the following committees:
//Commerce, Health, Aging, Agriculture, Finance

/*

list id sponsor2 year if body== "H" & type=="B" & HB_referred_House ==1 & standing_committee== "COMMERCE", noobs sepby(year)

list id sponsor2 year if body== "H" & type=="B" & HB_referred_House ==1 & standing_committee== "HEALTH", noobs sepby(year)

list id sponsor2 year if body== "H" & type=="B" & HB_referred_House ==1 & standing_committee== "AGING AND OLDER ADULT SERVICES", noobs sepby(year)

list id sponsor2 year if body== "H" & type=="B" & HB_referred_House ==1 & standing_committee== "AGRICULTURE AND RURAL AFFAIRS", noobs sepby(year)

list id sponsor2 year if body== "H" & type=="B" & HB_referred_House ==1 & standing_committee== "FINANCE", noobs sepby(year)

gen plbc19 = 0
replace plbc19 = 1 if inlist(sponsor2, "KINSEY", "BULLOCK", "HILL-EVANS", "LEE", "BURGOS", "CEPHAS", "CRUZ", "DAVIDSON", "A. DAVIS")

replace plbc19 = 2 if inlist(sponsor2, "DAWKINS", "FITZGERALD", "GAINEY", "HARRIS", "JOHNSON-HARRELL", "KENYATTA", "KIM", "KIRKLAND") 

replace plbc19 = 3 if inlist(sponsor2, "McCLINTON", "RABB", "ROEBUCK", "WHEATLEY", "D. WILLIAMS", "YOUNGBLOOD")
replace plbc19 = 1 if inrange(plbc19,1,3)




keep if plbc19 ==1
tab plbc19

tab sponsor2 if plbc19 == 1 & HB_referred_House ==1
tab2xl sponsor2 standing_committee if HB_referred_House ==1 using "C:\Users\laptop\Desktop\Session Analytics\Rabb\Maeve PLBC Committee Crosstab 19-20.xlsx", row(1) col(1) replace

*/


gen plbc21 = 0
replace plbc21 = 1 if inlist(sponsor2, "BULLOCK", "LEE", "HILL-EVANS", "GUZMAN", "A. BROWN","BURGOS", "CEPHAS")

replace plbc21 = 2 if inlist(sponsor2, "CRUZ", "DAVIDSON", "A. DAVIS", "DAWKINS", "FITZGERALD", "GAINEY", "HARRIS", "KENYATTA") 

replace plbc21 = 3 if inlist(sponsor2, "KIM", "KINSEY", "KIRKLAND", "KRAJEWSKI", "McCLINTON", "N. NELSON", "PARKER")

replace plbc21 = 4 if inlist(sponsor2, "RABB", "WHEATLEY", "D. WILLIAMS", "YOUNG")
replace plbc21 = 1 if inrange(plbc21,1,4)

keep if plbc21 ==1
tab plbc21

tab sponsor2 if plbc21 == 1 & HB_referred_House ==1
tab2xl sponsor2 standing_committee if HB_referred_House ==1 using "C:\Users\laptop\Desktop\Session Analytics\Rabb\Maeve PLBC Committee Crosstab 21-22.xlsx", row(1) col(1) replace
