library(readr)
library(tidyr)
library(stringr)
library(dplyr)
library(lubridate)

# ------------------------------------- INITIALIZING PHYSICIAN DATA WITH MDPPAS ------------------------------------
#                                       Hanna Glenn, Emory University
#                                       1/21/2022

# ORDER:::::: 5

# After using the MDPPAS to limit pairs included in preliminary data, this script combines the constructed pairs with the MDPPAS
# data to create dependent variables used in analysis. 

# Read in MDPPAS data
for (i in 2009:2017) {
  year <- read_csv(paste0(raw_data_path,"/MDPPAS/PhysicianData_",i,".csv"))
  year <- year %>%
    dplyr::select(-name_middle, -name_first, -spec_broad, -spec_prim_2,
                  -phy_zip7, -claim_count7, -phy_zip8, -claim_count8, -phy_zip9, -claim_count9,
                  -phy_zip10, -claim_count10, -phy_zip11, -claim_count11, -phy_zip12, 
                  -claim_count12, -spec_prim_2_name, -tin1_legal_name, -tin2_legal_name,
                  -group1, -group2)
  assign(paste0("MDPPAS",i),year)
}

MDPPAS <- rbind(MDPPAS2009, MDPPAS2010, MDPPAS2011, MDPPAS2012, MDPPAS2013, MDPPAS2014, MDPPAS2015, MDPPAS2016, MDPPAS2017)

# Read in Physician Utilization Files for 2013-2015
for (i in 13:15){
  year <- read_csv(paste0(raw_data_path,"/MUP_PHY/MUP_PHY_R19_P04_V10_D",i,"_Prov_Svc_zip",".csv")) %>%
    mutate(year=2000+i)
  assign(paste0("MUP_PHY",i),year)
}

MUP_PHY <- rbind(MUP_PHY13, MUP_PHY14, MUP_PHY15)
rm(MUP_PHY13, MUP_PHY14, MUP_PHY15, year, MDPPAS2009, MDPPAS2010, MDPPAS2011, MDPPAS2012, MDPPAS2013, MDPPAS2014, MDPPAS2015, MDPPAS2016, MDPPAS2017)

# summarize the public use data to the physician level for specific claims I need
MUP_PHY <- MUP_PHY %>%
  group_by(Rndrng_NPI, year) %>%
  mutate(total_claims = sum(Tot_Srvcs, na.rm=T)) %>%
  ungroup()

MUP_PHY <- MUP_PHY %>%
  filter(str_detect(HCPCS_Cd, "^992"))

# group HCPCS codes that are the same (with varied time components)
MUP_PHY <- MUP_PHY %>%
  mutate(group = ifelse(str_detect(HCPCS_Desc, "Critical care delivery critically ill or injured patient"), "crit_care_delivery", NA),
         group = ifelse(str_detect(HCPCS_Desc, "Established patient office or other outpatient"), "est_patient_office", group),
         group = ifelse(str_detect(HCPCS_Desc, "Hospital discharge day management"), "hosp_disch_day", group),
         group = ifelse(str_detect(HCPCS_Desc, "Hospital observation"), "hosp_obs_care", group),
         group = ifelse(str_detect(HCPCS_Desc, "Initial hospital inpatient care"), "initial_hosp_inpat", group),
         group = ifelse(str_detect(HCPCS_Desc, "New patient office or other outpatient visit"), "new_patient_office", group),
         group = ifelse(str_detect(HCPCS_Desc, "Subsequent hospital inpatient care"), "subs_hosp_inpat", group),
         group = ifelse(str_detect(HCPCS_Desc, "Subsequent observation care"), "subs_obs_care", group))

MUP_PHY <- MUP_PHY %>%
  filter(!is.na(group)) %>%
  group_by(Rndrng_NPI, group, year) %>%
  mutate(num_services = sum(Tot_Srvcs, na.rm=T)) %>%
  ungroup() %>%
  distinct(year, Rndrng_NPI, group, num_services, total_claims, Rndrng_Prvdr_State_Abrvtn)

# long to wide
MUP_PHY_wide <- MUP_PHY %>%
  mutate(frac_services = num_services/total_claims) %>%
  select(-num_services, -total_claims) %>%
  pivot_wider(id_cols = c("Rndrng_NPI", "year", "Rndrng_Prvdr_State_Abrvtn"), names_from = "group", values_from = "frac_services") %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.),0,.)))


# Read in Aggregated Pairs data from "data4_physician_level.R"
Aggregated_Pairs <- read_rds(paste0(created_data_path,"Aggregated_Pairs.rds"))


# Merge MDPPAS to main dataset and fill time invariant variables
Physician_Data <- Aggregated_Pairs %>%
  dplyr::left_join(MDPPAS, by=c("DocNPI"="npi", "year"="Year")) %>%
  dplyr::group_by(DocNPI) %>%
  tidyr::fill(sex, .direction="downup") %>%
  tidyr::fill(birth_dt, .direction="downup") %>%
  tidyr::fill(frac_EHR,.direction = "downup") %>%
  tidyr::fill(never_newnpi,.direction="downup") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(female=ifelse(sex=='F',1,0)) 


# Create age variable
Physician_Data <- Physician_Data %>%
  dplyr::mutate(birth_year=substr(birth_dt,start=6, stop=9)) %>%
  dplyr::mutate(birth_year=as.numeric(birth_year)) %>%
  dplyr::mutate(age=year-birth_year) %>%
  dplyr::group_by(DocNPI) %>%
  dplyr::mutate(max_age=max(age)) %>%
  dplyr::ungroup() %>%
  filter(age!=24)

# Drop any physicians with less than 70% of patients in hospitals
Physician_Data <- Physician_Data %>%
  dplyr::group_by(DocNPI) %>%
  dplyr::mutate(max=max(pos_inpat,na.rm=T)) %>%
  dplyr::filter(max>.7) 
  # 178k obs

# join public use file 
Physician_Data <- Physician_Data %>%
  left_join(MUP_PHY_wide, by=c("DocNPI"="Rndrng_NPI", "year"))


## Read in Medicare Opt-Out Data
opt_out <- read_csv(paste0(raw_data_path,"/Study_01.164.01_2022.04.15_Opt_Out_List_March.csv"), 
                                                          col_types = cols(`Optout Effective Date` = col_date(format = "%m/%d/%Y"), 
                                                                           `Optout End Date` = col_date(format = "%m/%d/%Y")))

opt_out <- opt_out %>%
  mutate(optout_year=year(`Optout Effective Date`)) %>%
  filter(optout_year>2008 & optout_year<2018) %>%
  select(NPI,Specialty,optout_year) 

Physician_Data <- Physician_Data %>%
  left_join(opt_out,by=c("DocNPI"="NPI"))
# I see zero doctors actually opt out of medicare.


# CREATE DEPENDENT VARIABLES -----------------------------------------------------------------------------------------

# RETIREMENT ####



# For retirement, I can combine all claim counts into one
Physician_Data <- Physician_Data %>%
  dplyr::rowwise() %>%
  dplyr::mutate(claim_count_total=ifelse(is.na(claim_count1),NA,sum(dplyr::c_across(tidyr::starts_with("claim_count")),na.rm=T))) %>%
  dplyr::ungroup() 


# Create a variable that sums up the claim count in all future years
Physician_Data <- Physician_Data %>%
  dplyr::group_by(DocNPI) %>%
  dplyr::mutate(future_claims=ifelse(year==2009,sum(claim_count_total[year>2009],na.rm=T),NA))

for (i in 2010:2017){
  Physician_Data <- Physician_Data %>%
    dplyr::group_by(DocNPI) %>%
    dplyr::mutate(future_claims=ifelse(year==i,sum(claim_count_total[year>i],na.rm=T),future_claims))
} 

# Create retirement variable
minyr_retire <- Physician_Data %>%
  dplyr::filter(future_claims==0 & year<2017) %>%
  dplyr::group_by(DocNPI) %>%
  dplyr::mutate(minyr_retire=min(year)) %>%
  dplyr::distinct(DocNPI, minyr_retire) %>%
  dplyr::ungroup()


# This creates minimum year one year too early. Fix this
Physician_Data <- Physician_Data %>%
  dplyr::left_join(minyr_retire, by="DocNPI") %>%
  dplyr::mutate(minyr_retire=ifelse(!is.na(minyr_retire),minyr_retire+1,minyr_retire)) %>%
  dplyr::mutate(minyr_retire=ifelse(minyr_retire==2017,NA,minyr_retire))


Physician_Data <- Physician_Data %>%
  dplyr::mutate(retire=ifelse(is.na(minyr_retire),0,ifelse(year==minyr_retire,1,0)))

# Create variable for whether the physician ever retires
Physician_Data <- Physician_Data %>% dplyr::ungroup() %>%
  dplyr::mutate(ever_retire=ifelse(is.na(minyr_retire),0,1)) 


# OFFICE ####

# Now I need to think about how to handle NAs for claims. 
# Let's see what missings look like after I drop physicians who retire
observe <- Physician_Data %>%
  dplyr::filter(ever_retire==0) %>%
  dplyr::mutate(missings=ifelse(is.na(claim_count_total),1,0)) %>%
  dplyr::group_by(DocNPI) %>%
  dplyr::mutate(sum=sum(missings)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(sum>0) %>%
  dplyr::select(DocNPI,year,hosp_patient_count,claim_count_total)


# I'm thinking to replace all NAs with 0.
Physician_Data <- Physician_Data %>%
  dplyr::mutate(claim_count_total=ifelse(is.na(claim_count_total),0,claim_count_total)) %>%
  dplyr::mutate(pos_office=ifelse(is.na(pos_office),0,pos_office))

# Since there is already a variable of fraction of claims in office setting, I don't need to do much. 
# I'll create an indicator or have positive patients in office setting to capture different variation. 
Physician_Data <- Physician_Data %>%
  dplyr::mutate(work_in_office=ifelse(pos_office>0,1,0))

# Create different fraction variables for whether they worked in an office already or not
Physician_Data <- Physician_Data %>%
  mutate(office_yearprior=ifelse(year==minyr_EHR-1 & work_in_office==1, 1, NA)) %>%
  mutate(office_yearprior=ifelse(year==minyr_EHR-1 & work_in_office==0, 0, office_yearprior)) %>%
  group_by(DocNPI) %>%
  fill(office_yearprior, .direction="downup")

Physician_Data <- Physician_Data %>%
  mutate(pos_office_prior=ifelse(office_yearprior==1,pos_office,NA),
         pos_office_noprior=ifelse(office_yearprior==0,pos_office,NA))

# Additionally, I can create a variable for whether the physician works in hospitals
Physician_Data <-Physician_Data %>%
  dplyr::mutate(work_in_hosp=ifelse(pos_inpat>0,1,0))

# Create a variable for whether the physician ever works in an office
Physician_Data <- Physician_Data %>%
  dplyr::group_by(DocNPI) %>%
  dplyr::mutate(sum=sum(work_in_office)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(ever_work_in_office=ifelse(sum>0,1,0)) 
  
  # The NAs that are left in this variable are fine because they only occur when minyr_EHR is either 0
  # or 2009, and these observations get dropped in the analysis anyway. 

# Create variable for total number of patients in office
Physician_Data <- Physician_Data %>%
  mutate(total_office=pos_office*npi_unq_benes)

Physician_Data <- Physician_Data %>%
  mutate(total_office_prior=ifelse(office_yearprior==1,total_office,NA),
         total_office_noprior=ifelse(office_yearprior==0,total_office,NA))

# Create variable for majority patients in office
Physician_Data <- Physician_Data %>%
  mutate(majority_in_office=ifelse(pos_office>pos_inpat,1,0))


# PRODUCTIVITY ####

# For productivity, I will simply use claim count,  patient count and claims per patient as the dependent variable.
# The key for this variable is to limit the sample appropriately. 

Physician_Data <- Physician_Data %>%
  mutate(npi_unq_benes=ifelse(is.na(npi_unq_benes),0,npi_unq_benes))

Physician_Data <- Physician_Data %>%
  mutate(claim_per_patient=claim_count_total/npi_unq_benes)

# create variable that indicates the physician left the hospital completely
Physician_Data <- Physician_Data %>%
  mutate(only_office = ifelse(pos_inpat<.05,1,0))



# Create relative year variables
Physician_Data <- Physician_Data %>%
  mutate(rel_expandyear=ifelse(minyr_EHR==0,NA,
                               ifelse(minyr_EHR>0,year-minyr_EHR,NA)))

Physician_Data <- Physician_Data %>%
  mutate(rel_m4=1*(rel_expandyear==-4),
         rel_m3=1*(rel_expandyear==-3),
         rel_m2=1*(rel_expandyear==-2),
         rel_m1=1*(rel_expandyear==-1),
         rel_0=1*(rel_expandyear==0),
         rel_p1=1*(rel_expandyear==1),
         rel_p2=1*(rel_expandyear==2),
         rel_p3=1*(rel_expandyear==3),
         rel_p4=1*(rel_expandyear==4))


# Limit to physicians who have majority patients in hospital the year prior to exposure (I don't want to pick up
# people becoming hospitalists at the time a hospital implements an EHR)
Physician_Data <- Physician_Data %>%
  mutate(yearbefore=ifelse(year==minyr_EHR-1,pos_inpat,NA)) %>%
  group_by(DocNPI) %>%
  fill(yearbefore,.direction="downup")

Physician_Data <- Physician_Data %>%
  filter(yearbefore>.5 | is.na(yearbefore))

Physician_Data <- Physician_Data %>%
  group_by(DocNPI) %>%
  fill(Rndrng_Prvdr_State_Abrvtn, .direction="downup") %>%
  ungroup()

# Save the data
saveRDS(Physician_Data,file=paste0(created_data_path,"Physician_Data.rds"))




  
  
  
  
  
  
  











