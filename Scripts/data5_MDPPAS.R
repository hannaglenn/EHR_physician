library(readr)
library(tidyr)
library(stringr)
library(dplyr)
library(stringi)

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
    dplyr::select(-name_middle, -name_first, -spec_broad, -spec_prim_1, -spec_prim_2,
                  -phy_zip7, -claim_count7, -phy_zip8, -claim_count8, -phy_zip9, -claim_count9,
                  -phy_zip10, -claim_count10, -phy_zip11, -claim_count11, -phy_zip12, 
                  -claim_count12, -spec_prim_2_name, -tin1_legal_name, -tin2_legal_name,
                  -group1, -group2)
  assign(paste0("MDPPAS",i),year)
}

MDPPAS <- rbind(MDPPAS2009, MDPPAS2010, MDPPAS2011, MDPPAS2012, MDPPAS2013, MDPPAS2014, MDPPAS2015, MDPPAS2016, MDPPAS2017)


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
  dplyr::ungroup()

# Drop any physicians with less than 20% of patients in hospitals
Physician_Data <- Physician_Data %>%
  dplyr::group_by(DocNPI) %>%
  dplyr::mutate(max=max(pos_inpat,na.rm=T)) %>%
  dplyr::filter(max>.2)
  # 403k obs

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

observe <- Physician_Data %>%
  dplyr::filter(ever_retire==1) %>%
  dplyr::select(DocNPI, year,age,retire,hosp_patient_count,claim_count_total,missingbefore)


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


# ZIP CODES ####
Physician_Data <- Physician_Data %>% dplyr::ungroup() %>%
  dplyr::mutate(multiple_zip=ifelse(is.na(phy_zip2),0,1),
                claim_count1=ifelse(is.na(claim_count1),0,claim_count1)) 

for (i in 1:9){
zipyear <- Physician_Data %>%
  dplyr::filter(year==2008+i) %>%
  dplyr::mutate(zip_list=ifelse(year==2008+i & is.na(phy_zip2),phy_zip1,NA),
                zip_list=ifelse(is.na(zip_list) & year==2008+i & is.na(phy_zip3),
                                    str_c(phy_zip1,phy_zip2,sep = ),zip_list),
                zip_list=ifelse(is.na(zip_list) & year==2008+i & is.na(phy_zip4),
                                    str_c(phy_zip1,phy_zip2,phy_zip3),zip_list),
                zip_list=ifelse(is.na(zip_list) & year==2008+i & is.na(phy_zip5),
                                    str_c(phy_zip1,phy_zip2,phy_zip3,phy_zip4),zip_list),
                zip_list=ifelse(is.na(zip_list) & year==2008+i & is.na(phy_zip6),
                                    str_c(phy_zip1,phy_zip2,phy_zip3,phy_zip4,phy_zip5),zip_list),
                zip_list=ifelse(is.na(zip_list) & year==2008+i,
                                    str_c(phy_zip1,phy_zip2,phy_zip3,phy_zip4,phy_zip5,phy_zip6),zip_list)) %>%
  dplyr::select(DocNPI,zip_list)

assign(paste0("zip",i+2008),zipyear)

}

zip2009 <- zip2009 %>%
  dplyr::rename(zip_list2009=zip_list)
zip2010 <- zip2010 %>%
  dplyr::rename(zip_list2010=zip_list)
zip2011 <- zip2011 %>%
  dplyr::rename(zip_list2011=zip_list)
zip2012 <- zip2012 %>%
  dplyr::rename(zip_list2012=zip_list)
zip2013 <- zip2013 %>%
  dplyr::rename(zip_list2013=zip_list)
zip2014 <- zip2014 %>%
  dplyr::rename(zip_list2014=zip_list)
zip2015 <- zip2015 %>%
  dplyr::rename(zip_list2015=zip_list)
zip2016 <- zip2016 %>%
  dplyr::rename(zip_list2016=zip_list)
zip2017 <- zip2017 %>%
  dplyr::rename(zip_list2017=zip_list)

Physician_Data <- Physician_Data %>%
  dplyr::left_join(zip2009,by="DocNPI") %>%
  dplyr::left_join(zip2010,by="DocNPI") %>%
  dplyr::left_join(zip2011,by="DocNPI") %>%
  dplyr::left_join(zip2012,by="DocNPI") %>%
  dplyr::left_join(zip2013,by="DocNPI") %>%
  dplyr::left_join(zip2014,by="DocNPI") %>%
  dplyr::left_join(zip2015,by="DocNPI") %>%
  dplyr::left_join(zip2016,by="DocNPI") %>%
  dplyr::left_join(zip2017,by="DocNPI")

Physician_Data <- Physician_Data %>%
  dplyr::rowwise() %>%
  dplyr::mutate(change_zip=ifelse(year==2010 & !(str_detect(zip_list2009,zip_list2010)[1] |
                                                   str_detect(zip_list2010,zip_list2009)[1]) ,1,NA)) %>%
  dplyr::mutate(change_zip=ifelse(year==2010 & is.na(change_zip) & !(is.na(zip_list2009) | 
                                                                       is.na(zip_list2010)),0,change_zip)) %>%
  dplyr::mutate(change_zip=ifelse(year==2011 & !(str_detect(zip_list2010,zip_list2011)[1] |
                                                   str_detect(zip_list2011,zip_list2010)[1]) ,1,change_zip)) %>%
  dplyr::mutate(change_zip=ifelse(year==2011 & is.na(change_zip) & !(is.na(zip_list2010) |
                                                                       is.na(zip_list2011)),0,change_zip)) %>%
  dplyr::mutate(change_zip=ifelse(year==2012 & !(str_detect(zip_list2011,zip_list2012)[1] |
                                                   str_detect(zip_list2012,zip_list2011)[1]) ,1,change_zip)) %>%
  dplyr::mutate(change_zip=ifelse(year==2012 & is.na(change_zip) & !(is.na(zip_list2011) | 
                                                                       is.na(zip_list2012)),0,change_zip)) %>%
  dplyr::mutate(change_zip=ifelse(year==2013 & !(str_detect(zip_list2012,zip_list2013)[1] |
                                                   str_detect(zip_list2013,zip_list2012)[1]) ,1,change_zip)) %>%
  dplyr::mutate(change_zip=ifelse(year==2013 & is.na(change_zip) & !(is.na(zip_list2012) | 
                                                                       is.na(zip_list2013)),0,change_zip)) %>%
  dplyr::mutate(change_zip=ifelse(year==2014 & !(str_detect(zip_list2013,zip_list2014)[1] |
                                                   str_detect(zip_list2014,zip_list2013)[1]) ,1,change_zip)) %>%
  dplyr::mutate(change_zip=ifelse(year==2014 & is.na(change_zip) & !(is.na(zip_list2013) |
                                                                       is.na(zip_list2014)),0,change_zip)) %>%
  dplyr::mutate(change_zip=ifelse(year==2015 & !(str_detect(zip_list2014,zip_list2015)[1] |
                                                   str_detect(zip_list2015,zip_list2014)[1]) ,1,change_zip)) %>%
  dplyr::mutate(change_zip=ifelse(year==2015 & is.na(change_zip) & !(is.na(zip_list2014) | 
                                                                       is.na(zip_list2015)),0,change_zip)) %>%
  dplyr::mutate(change_zip=ifelse(year==2016 & !(str_detect(zip_list2015,zip_list2016)[1] |
                                                   str_detect(zip_list2016,zip_list2015)[1]) ,1,change_zip)) %>%
  dplyr::mutate(change_zip=ifelse(year==2016 & is.na(change_zip) & !(is.na(zip_list2015) | 
                                                                       is.na(zip_list2016)),0,change_zip)) %>%
  dplyr::mutate(change_zip=ifelse(year==2017 & !(str_detect(zip_list2016,zip_list2017)[1] |
                                                   str_detect(zip_list2017,zip_list2016)[1]) ,1,change_zip)) %>%
  dplyr::mutate(change_zip=ifelse(year==2017 & is.na(change_zip) & !(is.na(zip_list2016) | is.na(zip_list2017)),0,change_zip))


Physician_Data <- Physician_Data %>%
  dplyr::mutate(change_zip=ifelse(year==2009,0,change_zip)) 


# PRODUCTIVITY ####

# For productivity, I will simply use claim count and  patient count as the dependent variable.
# The key for this variable is to limit the sample appropriately. 

Physician_Data <- Physician_Data %>%
  mutate(hosp_patient_count_EHRhosp=ifelse(is.na(hosp_patient_count_EHRhosp),0,hosp_patient_count_EHRhosp),
         hosp_patient_count_noEHRhosp=ifelse(is.na(hosp_patient_count_noEHRhosp),0,hosp_patient_count_noEHRhosp),
         hosp_patient_count=ifelse(is.na(hosp_patient_count),0,hosp_patient_count))

Physician_Data <- Physician_Data %>%
  mutate(npi_unq_benes=ifelse(is.na(npi_unq_benes),0,npi_unq_benes))


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


# Save the data
saveRDS(Physician_Data,file=paste0(created_data_path,"Physician_Data.rds"))
write.csv(Physician_Data,file=paste0(created_data_path,"Physician_Data.csv"))




  
  
  
  
  
  
  











