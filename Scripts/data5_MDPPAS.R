library(readr)
library(tidyr)
library(plm)

# ------------------------------------- INITIALIZING PHYSICIAN DATA WITH MDPPAS ------------------------------------
#                                       Hanna Glenn, Emory University
#                                       1/21/2022

# ORDER:::::: 5

# After using the MDPPAS to limit pairs included in preliminary data, this script combines the constructed pairs with the MDPPAS
# data to create dependent variables used in analysis. 

# Read in MDPPAS data
for (i in 2009:2017) {
  year <- read_csv(paste0(raw_data_path,"/MDPPAS/PhysicianData_",i,".csv"))
  assign(paste0("MDPPAS",i),year)
}

MDPPAS <- rbind(MDPPAS2009, MDPPAS2010, MDPPAS2011, MDPPAS2012, MDPPAS2013, MDPPAS2014, MDPPAS2015, MDPPAS2016, MDPPAS2017)

MDPPAS <- MDPPAS %>%
  dplyr::select(-name_middle, -spec_broad, -spec_prim_1, -spec_prim_2,
                -phy_zip7, -claim_count7, -phy_zip8, -claim_count8, -phy_zip9, -claim_count9,
                -phy_zip10, -claim_count10, -phy_zip11, -claim_count11, -phy_zip12, 
                -claim_count12)

# Read in Aggregated Pairs data from "data4_physician_level.R"
Aggregated_Pairs <- read_rds(paste0(created_data_path,"Aggregated_Pairs.rds"))

Aggregated_Pairs <- complete(Aggregated_Pairs,DocNPI,year=2009:2017)

# Merge MDPPAS to main dataset and fill time invariant variables
Physician_Data <- Aggregated_Pairs %>%
  dplyr::left_join(MDPPAS, by=c("DocNPI"="npi", "year"="Year")) %>%
  dplyr::group_by(DocNPI) %>%
  tidyr::fill(sex, .direction="downup") %>%
  tidyr::fill(name_last, .direction="downup") %>%
  tidyr::fill(name_first, .direction="downup") %>%
  tidyr::fill(birth_dt, .direction="downup") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(female=ifelse(sex=='F',1,0)) 




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
  dplyr::mutate(future_claims=ifelse(year==2009,sum(claim_count_total[year>2009],na.rm=T),NA)) %>%
  dplyr::mutate(future_patients=ifelse(year==2009,sum(hosp_patient_count[year>2009],na.rm=T),NA))

for (i in 2010:2017){
  Physician_Data <- Physician_Data %>%
    dplyr::group_by(DocNPI) %>%
    dplyr::mutate(future_claims=ifelse(year==i,sum(claim_count_total[year>i],na.rm=T),future_claims)) %>%
    dplyr::mutate(future_patients=ifelse(year==i,sum(hosp_patient_count[year>i],na.rm=T),future_patients))
} 

# I only consider it retirement if all future patients AND claims are zero. Create this variable
Physician_Data <- Physician_Data %>% dplyr::ungroup() %>%
  dplyr::mutate(retire=ifelse(future_claims==0 & future_patients==0,1,0)) 

# Create variable for whether the physician ever retires
Physician_Data <- Physician_Data %>%
  dplyr::group_by(DocNPI) %>%
  dplyr::mutate(sum=sum(retire[year<2016])) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(ever_retire=ifelse(sum>0,1,0)) %>%
  dplyr::select(-sum)

# Now that the retirement variable is created I no longer need 2016 and 2017 years
Physician_Data <- Physician_Data %>%
  dplyr::filter(year<2016)


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

# I'm thinking to replace all NAs with 0 if hospital patient count is zero. This will give a lower bound of claim count
Physician_Data <- Physician_Data %>%
  dplyr::mutate(claim_count_total=ifelse(is.na(claim_count_total) & hosp_patient_count==0,0,claim_count_total))


# Since there is already a variable of fraction of claims in office setting, I don't need to do much. 
# I'll create an indicator or have positive patients in office setting to capture different variation. 
Physician_Data <- Physician_Data %>%
  dplyr::mutate(work_in_office=ifelse(pos_office>0,1,0))

# Additionally, I can create a variable for whether the physician works in hospitals
Physician_Data <-Physician_Data %>%
  dplyr::mutate(work_in_hosp=ifelse(pos_inpat>0,1,0))




# PRODUCTIVITY ####

# For productivity, I will simply use claim count and patient count as the dependent variable.
# I may need to do additional cleaning of the claim variable to code some of the missings if I can do so reasonably




# Save the data
saveRDS(Physician_Data,file=paste0(created_data_path,"Physician_Data.rds"))
write.csv(Physician_Data,file=paste0(created_data_path,"Physician_Data.csv"))





  
  
  
  
  
  
  
  
  
  
  











