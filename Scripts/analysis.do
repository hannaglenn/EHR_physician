 /* Set Working Directory */
 cd "C:\Users\hkagele\OneDrive - Emory University\Documents\EHR_physician"

/* Import Physician Level Dataset */
 import delimited ".\CreatedData\Physician_Data.csv"
 
 /* Change string variables to numeric */
 encode avg_beds, generate(avg_beds2)
 encode claim_count_total, generate(claim_count_total2)
 encode pos_office, generate(pos_office2_num)
 gen pos_office2=pos_office2_num/100000


 /* Tell stata the dataset is panel */
 xtset docnpi year
 
 /* --------------------------------------------------------------------- */
 /* --------------------   RETIREMENT ----------------------------------- */
 /* --------------------------------------------------------------------- */
 
 /* Retirement Event Study: Full Sample of Physicians */
 xtevent retire female experience avg_beds2 if (experience>30), pol(anyehr_exposed) window(4) cluster(docnpi)
 
  xteventplot , nopostpval scheme(s1mono) ylabel(, grid angle(0) labsize(small)) xlabel(, labsize(small)) ytitle("Estimate and 95% CI" " ", size(small)) xtitle(" " "Event Time", size(small)) title("Event Study, Dep. Variable: Retirement (Full Sample)" " ", size(medsmall)) ysc(r(-.1 .1)) 
  
 /* --------------------------------------------------------------------- */
 /* --------------------   OFFICE --------------------------------------- */
 /* --------------------------------------------------------------------- */
xtevent pos_office2 experience, pol(anyehr_exposed) window(4) cluster(docnpi)
  
xteventplot , nopostpval scheme(s1mono) ylabel(, grid angle(0) labsize(small)) xlabel(, labsize(small)) ytitle("Estimate and 95% CI" " ", size(small)) xtitle(" " "Event Time", size(small)) title("Event Study, Dep. Variable: Retirement (Full Sample)" " ", size(medsmall)) ysc(r(-.1 .1)) 
	
xtevent pos_office2 experience if experience>30, pol(anyehr_exposed) window(4) cluster(docnpi)
  
xteventplot , nopostpval scheme(s1mono) ylabel(, grid angle(0) labsize(small)) xlabel(, labsize(small)) ytitle("Estimate and 95% CI" " ", size(small)) xtitle(" " "Event Time", size(small)) title("Event Study, Dep. Variable: Retirement (Full Sample)" " ", size(medsmall)) ysc(r(-.1 .1)) 
 
 
 /* --------------------------------------------------------------------- */
 /* -------------------- PRODUCTIVITY ----------------------------------- */
 /* --------------------------------------------------------------------- */
 
 
 /* Productivity Event Study: Full Sample of Physicians */
 xtevent hosp_patient_count if (ever_retire==0), pol(anyehr_exposed) window(4) cluster(docnpi)
 
 xteventplot , nopostpval scheme(s1mono) ylabel(, grid angle(0) labsize(small)) xlabel(, labsize(small)) ytitle("Estimate and 95% CI" " ", size(small)) xtitle(" " "Event Time", size(small)) title("Event Study, Dep. Variable: Number Patients (Full Sample)" " ", size(medsmall)) ysc(r(-150 150)) 
 graph export "./Objects/prod_eventstudy_fullsample.pdf"
 
 /* Productivity Event Study: Old Sample of Physicians */
 xtevent claim_count_total2 if (ever_retire==0 & experience>35), pol(anyehr_exposed) window(4) cluster(docnpi)
 
 xteventplot , nopostpval scheme(s1mono) ylabel(, grid angle(0) labsize(small)) xlabel(, labsize(small)) ytitle("Estimate and 95% CI" " ", size(small)) xtitle(" " "Event Time", size(small)) title("Event Study, Dep. Variable: Number Patients (Old Sample)" " ", size(medsmall)) ysc(r(-150 150)) 
 graph export "./Objects/prod_eventstudy_oldsample.pdf"
 
 /* Productivity using continuous treatment */
 xtreg hosp_count frac_ehr avg_beds2 avg_oper_days2 experience i.year if (working_allyears==1 & year<2015), fe robust
 
 xtreg hosp_count frac_ehr_patients avg_beds2 avg_oper_days2 experience i.year if (working_allyears==1 & year<2015), fe robust
 