***ECONOMETRICS PROJECT*** 
***CHILDREN AND LIFE SATISFACTION OF PARENTS***
***GROUP 19***
	
	//sorting the datasets
	cd "C:\Users\hp\Desktop\Fall 24\Econometric\MICS 2017-18 (Punjab)"
	use hl, clear 
	rename HL1 ln
	sort HH1 HH2 ln
	save hl, replace
	
	use wm, clear
	sort HH1 HH2 ln
	save wm, replace
	
	use mn, clear
	sort HH1 HH2 ln
	save mn, replace
	
	use ch, clear
	sort HH1 HH2 ln
	save ch, replace
	
	use bh, clear 
	sort HH1 HH2 ln
	save bh, replace
	
	use fs, clear
	sort HH1 HH2 ln
	save fs, replace
	
	use hh, clear
	sort HH1 HH2 
	save hh, replace
	
	//cleaning the datasets and appending them 
	use hl, clear
	des
	keep wscore windex5 windex10 HL7B HL4 HL6 HH1 HH2 ln mline fline disability wscoreu wscorer 
	
	save individuals, replace 
	
	use mn, clear 
	des
	keep HH1 HH2 ln wscore windex5 windex10 wscoreu wscorer MMA7 MCM3 MCM4 MCM6 MCM7 MCM9 MCM10	MCM11	MCM12	MCM17	MMA1	MMA6	MLS1	MLS2	MLS3	MLS4 
	rename (MLS1 MLS2 MLS3 MLS4) (LS1 LS2 LS3 LS4)
	rename (MCM3 MCM4 MCM6 MCM7 MCM9 MCM10 MCM11 MCM12 MMA1 MMA6 ) (CM3 CM4 CM6 CM7 CM9 CM10 CM11 CM12 MA1 MA6)
	des
	save men, replace
	
	use wm, clear
	keep HH1 HH2 ln windex5 windex10 wscore wscoreu wscorer CM11 CM12 CM3 CM4 CM6 CM7 CM9 CM10 MA7  MA1  MA6 LS1 LS2 LS3 LS4
	save women, replace 
	
	use men, clear
	append using women
	save parents, replace 
	
	//merging the individual level dataset
	use individuals, clear
	des
	merge 1:1 HH1 HH2 ln using parents, gen(m1)
	gen parent = .
	replace parent= 1 if m1==3
	save individuals, replace
	
	use ch, clear 
	keep AN4 cdisability HH1 HH2 ln
	des
	save children, replace
	
	use fs, clear
	rename CB3 AN4
	rename fsdisability cdisability
	keep AN4 cdisability HH1 HH2 ln
	save fs, replace
	
	use children, clear
	append using fs
// 	drop FS1 FS2 FS3
	rename AN4 child_age
	gen child=1
	save children, replace
	
	use individuals, clear
	merge 1:1 HH1 HH2 ln using children, gen(m2)
	br
	gen P_or_C=. 
	replace P_or_C=1 if parent == 1 
	replace P_or_C =0 if child == 1  
	
	gen check=5 if parent==child & parent!=.
	drop if check==5
	drop check
	tab P_or_C, m
	drop if P_or_C==.
	rename HL4 sex
	rename HL6 age
	rename HL7B marital_status 
	drop wscorer wscoreu
	rename CM11 children_born
	label variable cdisability "Functional Difficulties of Children"
	
	//we will now create our dependant variable 
	drop if LS2==. & child!=1 & parent==1
	bysort HH1 HH2: egen avg_LS2= mean(LS2)
	save individuals, replace
	egen tag = tag(HH1 HH2)
	
	//avg age of child in household
	bysort HH1 HH2: egen avg_age_child= mean(child_age)
	
	//creating chilren dead, children with you and disability_score 
	egen total_dead = rowtotal(CM9 CM10)
	bysort HH1 HH2: egen children_dead= sum(total_dead)
	egen row_withyou = rowtotal(CM3 CM4)
	bysort HH1 HH2: egen children_withyou =sum(row_withyou)
	bysort HH1 HH2: egen disability_score = mean(cdisability)
	
	des
	save individuals, replace
	use individuals, clear
	
	tab LS1,nolab m
	drop if LS1==9 | LS1==.
	
	//REGRESSION 1
	label variable LS1 "Life Satisfaction Estimate"
	label variable children_born "Total Nunber of Children born in houshold"
	label variable children_dead "Total Number of children in household that died"
	label variable children_withyou "Children that live with Parents"
	label variable wscore "Wealth Score"
	label variable disability_score "Disability Score"
	gen interactor1 = children_born*wscore
	drop if children_born==. | children_dead==. | children_withyou==. | avg_age_child==. | disability_score==.
	drop if LS1 ==. 
	oprobit LS1 children_born children_dead children_withyou wscore  avg_age_child disability_score interactor1
	estimates store Individual
	outreg2 using "Regression_Individual.doc", replace label
	
	//REGRESSION ON GENDER BASIS 
	oprobit LS1 children_born children_dead children_withyou wscore avg_age_child disability_score interactor1 if sex==1
	outreg2 using "Regression_Male.doc", replace label 
	estimates store male 
	
	
	oprobit LS1 children_born children_dead children_withyou wscore avg_age_child disability_score interactor1 if sex==2
	estimates store female
	outreg2 using "Regression_Female.doc", replace label
	
	bysort HH1 HH2: egen avg_LS1= mean(LS1)
	save individuals, replace
	
	//REGRESSION 2
	use hh, clear
	keep windex5 windex10 wscore HH1 HH2 HH51 HH52 
	merge 1:m HH1 HH2 using "individuals", gen (M5)
	des
	label variable cdisability "Functional Difficulties in Children"
	drop ln sex age mline fline disability cdisability tag 
	save house, replace
	drop if avg_LS1==.
	drop if children_born==. | children_dead==. | children_withyou==. | avg_age_child==. | disability_score==. 
	egen tag2=tag(HH1 HH2)
	regress avg_LS1 children_born children_dead children_withyou wscore interactor1 avg_age_child disability_score if tag2==0
		estimates store Household
	label variable avg_LS1 "Average Life Satisfaction Estimate in Household"
	outreg2 using "Regression_HH.doc", replace label
	save house, replace
	
	coefplot(Individual, label(Parents' Individual Life Satisfaction)) (Household, label(Average Life Satisfaction of Parents in a Household)) , drop(_cons) xline(0)

	coefplot(male, label(Father's Life Satisfaction)) (female, label(Mother's Life Satisfaction)), drop(_cons) xline(0) 
	
	
	
	
	
	
	
	
	
	
	
	
	
	