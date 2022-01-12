 /* Set Working Directory */
 cd "C:\Users\hkagele\OneDrive - Emory University\Documents\EHR_physician"

/* Import Physician Level Dataset */
 import delimited ".\CreatedData\Physician_Data.csv"
 
 /* Change string variables to numeric */
 encode avg_beds, generate(avg_beds2)
 encode avg_oper_days, generate(avg_oper_days2)

 /* Tell stata the dataset is panel */
 xtset docnpi year
 
 /* --------------------------------------------------------------------- */
 /* -------------------- PRODUCTIVITY ----------------------------------- */
 /* --------------------------------------------------------------------- */
 
 
 /* Productivity Event Study: Full Sample of Physicians */
 xtevent hosp_count avg_beds2 avg_oper_days2 if (year<2015 & working_allyears==1), pol(anyehr_exposed) window(4) cluster(docnpi)
 
 xteventplot , nopostpval scheme(s1mono) ylabel(, grid angle(0) labsize(small)) xlabel(, labsize(small)) ytitle("Estimate and 95% CI" " ", size(small)) xtitle(" " "Event Time", size(small)) title("Event Study, Dep. Variable: Number Patients (Full Sample)" " ", size(medsmall)) ysc(r(-150 150)) 
 graph export "./Objects/prod_eventstudy_fullsample.pdf"
 
 /* Productivity Event Study: Old Sample of Physicians */
 xtevent hosp_count avg_beds2 avg_oper_days2 experience if (year<2015 & experience>35 & working_allyears==1), pol(anyehr_exposed) window(3) cluster(docnpi)
 
 xteventplot , nopostpval scheme(s1mono) ylabel(, grid angle(0) labsize(small)) xlabel(, labsize(small)) ytitle("Estimate and 95% CI" " ", size(small)) xtitle(" " "Event Time", size(small)) title("Event Study, Dep. Variable: Number Patients (Old Sample)" " ", size(medsmall)) ysc(r(-150 150)) 
 graph export "./Objects/prod_eventstudy_oldsample.pdf"
 
 /* Productivity using continuous treatment */
 xtreg hosp_count frac_ehr avg_beds2 avg_oper_days2 experience i.year if (working_allyears==1 & year<2015), fe robust
 
 xtreg hosp_count frac_ehr_patients avg_beds2 avg_oper_days2 experience i.year if (working_allyears==1 & year<2015), fe robust
 