library(readr)
library(tidyr)
library(plm)
library(stringr)

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
  dplyr::select(-name_middle, -name_last, -name_first, -spec_broad, -spec_prim_1, -spec_prim_2,
                -phy_zip7, -claim_count7, -phy_zip8, -claim_count8, -phy_zip9, -claim_count9,
                -phy_zip10, -claim_count10, -phy_zip11, -claim_count11, -phy_zip12, 
                -claim_count12, -tin1_legal_name, -tin2_legal_name)

# Read in Aggregated Pairs data from "data4_physician_level.R"
Aggregated_Pairs <- read_rds(paste0(created_data_path,"Aggregated_Pairs.rds"))

Aggregated_Pairs <- complete(Aggregated_Pairs,DocNPI,year=2009:2017)

# fill time invariant variables 
Aggregated_Pairs <- Aggregated_Pairs %>%
  dplyr::group_by(DocNPI) %>%
  tidyr::fill(grad_year,.direction="downup") %>%
  tidyr::fill(minyr_EHR,.direction="downup") %>%
  tidyr::fill(anyEHR_exposed,.direction="down") %>%
  tidyr::fill(num_hospitals_constant,.direction="downup") %>%
  tidyr::fill(num_systems,.direction="downup") %>%
  tidyr::fill(exposed_ever,.direction="downup") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(experience=ifelse(is.na(experience),year-grad_year,experience))
  

# Merge MDPPAS to main dataset and fill time invariant variables
Physician_Data <- Aggregated_Pairs %>%
  dplyr::left_join(MDPPAS, by=c("DocNPI"="npi", "year"="Year")) %>%
  dplyr::group_by(DocNPI) %>%
  tidyr::fill(sex, .direction="downup") %>%
  tidyr::fill(birth_dt, .direction="downup") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(female=ifelse(sex=='F',1,0)) 


# Create age variable
Physician_Data <- Physician_Data %>%
  dplyr::mutate(birth_year=substr(birth_dt,start=6, stop=9)) %>%
  dplyr::mutate(birth_year=as.numeric(birth_year)) %>%
  dplyr::mutate(age=year-birth_year)

# Drop any physicians with less than 30% of patients in hospitals
Physician_Data <- Physician_Data %>%
  dplyr::group_by(DocNPI) %>%
  dplyr::mutate(max=max(pos_inpat,na.rm=T)) %>%
  dplyr::filter(max>.3)

# CREATE DEPENDENT VARIABLES -----------------------------------------------------------------------------------------

# RETIREMENT ####

# For retirement, I can combine all claim counts into one
Physician_Data <- Physician_Data %>%
  dplyr::rowwise() %>%
  dplyr::mutate(claim_count_total=ifelse(is.na(claim_count1),NA,sum(dplyr::c_across(tidyr::starts_with("claim_count")),na.rm=T))) %>%
  dplyr::ungroup()

# Remove those who only appear in the data for one year 
Physician_Data <- Physician_Data %>%
  dplyr::mutate(pos=ifelse(is.na(claim_count_total),0,1)) %>%
  dplyr::group_by(DocNPI) %>%
  dplyr::mutate(sum=sum(pos)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(sum>1) %>%
  dplyr::select(-pos,-sum)

# Create a variable that sums up the claim count in all future years
Physician_Data <- Physician_Data %>%
  dplyr::group_by(DocNPI) %>%
  dplyr::mutate(future_claims=ifelse(year==2009,sum(claim_count_total[year>2009],na.rm=T),NA))

for (i in 2010:2017){
  Physician_Data <- Physician_Data %>%
    dplyr::group_by(DocNPI) %>%
    dplyr::mutate(future_claims=ifelse(year==i,sum(claim_count_total[year>i],na.rm=T),future_claims)) 
} 

# I only consider it retirement if all future claims are zero. Create this variable
Physician_Data <- Physician_Data %>% dplyr::ungroup() %>%
  dplyr::mutate(retire=ifelse(future_claims==0,1,0)) %>%
  dplyr::mutate(retire_2016=ifelse(year==2016 & retire==1,1,NA)) %>%
  dplyr::group_by(DocNPI) %>%
  tidyr::fill(retire_2016,.direction="downup") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(retire=ifelse(year==2017 & retire_2016==1,1,retire)) %>%
  dplyr::mutate(retire=ifelse(year==2017 & is.na(retire_2016),0,retire))

# This variable creates the retirement indicator one year too early. I want the first year of retirement to be the first year showing zeros.
# Fix this
minyr_retire <- Physician_Data %>%
  dplyr::filter(retire==1) %>%
  dplyr::group_by(DocNPI) %>%
  dplyr::mutate(minyr_retire=min(year)) %>%
  dplyr::distinct(DocNPI, minyr_retire) %>%
  dplyr::ungroup()

Physician_Data <- Physician_Data %>%
  dplyr::left_join(minyr_retire, by="DocNPI")

Physician_Data <- Physician_Data %>%
  dplyr::mutate(retire=ifelse(year==minyr_retire,0,retire)) %>%
  dplyr::mutate(retire=ifelse(is.na(retire),0,retire))

# Create variable for whether the physician ever retires
Physician_Data <- Physician_Data %>% dplyr::ungroup() %>%
  dplyr::mutate(ever_retire=ifelse(is.na(minyr_retire),0,1)) 

observe <- Physician_Data %>%
  dplyr::filter(ever_retire==1) %>%
  dplyr::select(DocNPI, year,age,retire,hosp_patient_count,claim_count_total)

# Consider looking at those physicians who have low claim counts (barely working)
Physician_Data <- Physician_Data %>%
  dplyr::mutate(high_claim=ifelse(claim_count_total>200,1,0)) %>%
  dplyr::group_by(DocNPI) %>%
  dplyr::mutate(future_high_claims=ifelse(year==2009,sum(high_claim[year>2009],na.rm=T),NA))

for (i in 2010:2017){
  Physician_Data <- Physician_Data %>%
    dplyr::group_by(DocNPI) %>%
    dplyr::mutate(future_high_claims=ifelse(year==i,sum(high_claim[year>i],na.rm=T),future_high_claims)) 
} 

Physician_Data <- Physician_Data %>% dplyr::ungroup() %>%
  dplyr::mutate(retire_lowclaims=ifelse(future_high_claims==0,1,0)) %>%
  dplyr::mutate(retire_low2016=ifelse(year==2016 & retire_lowclaims==1,1,NA)) %>%
  dplyr::group_by(DocNPI) %>%
  tidyr::fill(retire_low2016,.direction="downup") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(retire_lowclaims=ifelse(year==2017 & retire_low2016==1,1,retire_lowclaims)) %>%
  dplyr::mutate(retire_lowclaims=ifelse(year==2017 & is.na(retire_low2016),0,retire_lowclaims))

minyr_retire_lowclaims <- Physician_Data %>%
  dplyr::filter(retire_lowclaims==1) %>%
  dplyr::group_by(DocNPI) %>%
  dplyr::mutate(minyr_retire_lowclaims=min(year)) %>%
  dplyr::distinct(DocNPI, minyr_retire_lowclaims) %>%
  dplyr::ungroup()

Physician_Data <- Physician_Data %>%
  dplyr::left_join(minyr_retire_lowclaims, by="DocNPI")

Physician_Data <- Physician_Data %>%
  dplyr::mutate(retire_lowclaims=ifelse(year==minyr_retire_lowclaims,0,retire_lowclaims)) %>%
  dplyr::mutate(retire_lowclaims=ifelse(is.na(retire_lowclaims),0,retire_lowclaims))


Physician_Data <- Physician_Data %>% dplyr::ungroup() %>%
  dplyr::mutate(ever_retire_lowclaims=ifelse(is.na(minyr_retire_lowclaims),0,1))



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

# Additionally, I can create a variable for whether the physician works in hospitals
Physician_Data <-Physician_Data %>%
  dplyr::mutate(work_in_hosp=ifelse(pos_inpat>0,1,0))

# Create a variable for whether the physician ever works in an office
Physician_Data <- Physician_Data %>%
  dplyr::group_by(DocNPI) %>%
  dplyr::mutate(sum=sum(work_in_office)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(ever_work_in_office=ifelse(sum>0,1,0)) 



# PRODUCTIVITY ####

# For productivity, I will simply use claim count and patient count as the dependent variable.
# The key for this variable is to limit the sample appropriately. 


# ZIP CODES ####
Physician_Data <- Physician_Data %>% dplyr::ungroup() %>%
  dplyr::mutate(multiple_zip=ifelse(is.na(phy_zip2),0,1))






# Save the data
saveRDS(Physician_Data,file=paste0(created_data_path,"Physician_Data.rds"))
write.csv(Physician_Data,file=paste0(created_data_path,"Physician_Data.csv"))


balance_check <- Physician_Data %>%
  dplyr::select(DocNPI,year)

is.pbalanced(balance_check)

data <- Physician_Data %>%
  dplyr::select(-future_patients, -spec_prim_1_name, -spec_prim_2_name, -name_last,-name_first, -sex, -birth_dt)
  
  
  
  
  
  
  
  
  
  
  











