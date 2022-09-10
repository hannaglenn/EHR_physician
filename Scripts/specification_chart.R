library(tidyr)
library(readr)
library(stringr)
library(dplyr)
library(did)
library(ggplot2)


## DATA 3------------------------------------------------ ####
phys_npidata <- read_rds(paste0(created_data_path,"phys_npidata.rds"))
hosp_npidata <- read_rds(paste0(created_data_path,"hosp_npidata.rds"))

temp_npidata <- rbind(phys_npidata, hosp_npidata)

rm(phys_npidata, hosp_npidata)

# Read in MDPPAS NPI list 
npi_list <- read_rds(paste0(created_data_path,"npi_list.rds"))

npi_list <- npi_list %>%
  dplyr::mutate(keep=1)

#  in Shared Patient Data, merge to temp_npi along the way 
for (t in 2009:2015){
  SP.year <- read_csv(paste0(raw_data_path,"/PSPP/pspp",t,"_30.csv"),
                      col_types=cols(npi1=col_character(),
                                     npi2=col_character()
                      ))
  SP.year <- SP.year %>%
    select(npi1,npi2,year,samedaycount) %>%
    distinct()
  
  SP.year<- SP.year %>%
    left_join(temp_npidata, by=c("npi1"="npi")) %>%
    rename(PCP1=PCP,hospital1=hospital) 
  
  SP.year <- SP.year %>%
    left_join(temp_npidata, by=c("npi2"="npi")) %>%
    rename(PCP2=PCP,hospital2=hospital) %>%
    select(npi1,npi2,samedaycount,year,PCP1,PCP2,hospital1,hospital2)
  
  # filter so that all is left is hospital-physician pairs
  SP.year <- SP.year %>%
    filter((PCP1==1 & hospital2==1) | (hospital1==1 & PCP2==1))
  
  assign(paste0("PSPD",t),SP.year)
}

phys_hosp_pairs <- rbind(PSPD2009, PSPD2010, PSPD2011, PSPD2012, PSPD2013, PSPD2014, PSPD2015)
# 12.6 mill observations

rm(PSPD2009, PSPD2010, PSPD2011, PSPD2012, PSPD2013, PSPD2014, PSPD2015, 
   temp_npidata)

# Change to physician and hospital npis instead of npi1 and npi2
phys_hosp_pairs <- phys_hosp_pairs %>%
  mutate(PCP1=ifelse(is.na(PCP1),0,PCP1), PCP2=ifelse(is.na(PCP2),0,PCP2),
         hospital1=ifelse(is.na(hospital1),0,hospital1), hospital2=ifelse(is.na(hospital2),0,hospital2)) %>%
  mutate(HospNPI=ifelse(hospital1==1,npi1,npi2),
         DocNPI=ifelse(PCP1==1,npi1,npi2)) %>%
  select(year,HospNPI,DocNPI,samedaycount)

phys_hosp_pairs <- phys_hosp_pairs %>%
  group_by(year,DocNPI, HospNPI) %>%
  mutate(samedaycount_combine=sum(samedaycount,na.rm=T))

phys_hosp_pairs <- phys_hosp_pairs %>%
  ungroup() %>%
  select(year,HospNPI,DocNPI,samedaycount_combine) %>%
  distinct() %>%
  rename(samedaycount=samedaycount_combine)
# 7.1 million obs. 


# Merge to NPI List of Physicians
phys_hosp_pairs <- phys_hosp_pairs %>%
  mutate(DocNPI=as.numeric(DocNPI)) %>%
  left_join(npi_list, by=c("DocNPI"="npi"))

phys_hosp_pairs <- phys_hosp_pairs %>%
  filter(keep==1) %>%
  select(-keep)
# 3.7 mill obs.

rm(npi_list, SP.year)

# Create indicators for which type of restriction to make on hospital-physician pairs
phys_hosp_pairs <- phys_hosp_pairs %>% ungroup() %>%
  mutate(count=1) %>%
  group_by(DocNPI,HospNPI) %>%
  mutate(yrs=sum(count)) %>%
  mutate(sum=sum(samedaycount,na.rm=T)) %>%
  mutate(samedaycount_30=ifelse(sum>30*yrs,1,0),
         samedaycount_10=ifelse(sum>10*yrs,1,0),
         samedaycount_60=ifelse(sum>60*yrs,1,0)) %>%
  select(-sum, -yrs, -count) %>%
  ungroup() %>%
  filter(samedaycount_30==1 | samedaycount_10==1 | samedaycount_60==1)

Physician_Data_10 <- phys_hosp_pairs %>%
  filter(samedaycount_10==1)
Physician_Data_30 <- phys_hosp_pairs %>%
  filter(samedaycount_30==1)
Physician_Data_60 <- phys_hosp_pairs %>%
  filter(samedaycount_60==1)

Data_List <- list(Physician_Data_10, Physician_Data_30, Physician_Data_60)

Data_List <- lapply(Data_List, function(x){
  data <- x %>%
    ungroup() %>%
    group_by(DocNPI,HospNPI) %>%
    mutate(pairID = cur_group_id())
})

### DATA 4 -------------------------------------------------------------------------------------
PhysCompare <- read_csv(paste0(raw_data_path,"DAC_NationalDownloadableFile.csv"))

PhysCompare <- PhysCompare %>%
  dplyr::rename(gender=gndr,grad_year=Grd_yr) %>%
  select(NPI,grad_year) 

Data_List <- lapply(Data_List, function(x){
  data <- x %>%
    left_join(PhysCompare, by=c("DocNPI"="NPI")) %>%
    distinct() %>%
    filter(grad_year<2005)
})

rm(PhysCompare)

# Read in AHA crosswalk
AHANPI_cw <- read_rds(paste0(created_data_path,"AHANPI_cw.rds"))

# Merge crosswalk to main data
Data_List <- lapply(Data_List, function(x){
  data <- x %>%
    mutate(HospNPI=as.double(HospNPI)) %>%
    left_join(AHANPI_cw,by=c("HospNPI"="NPI")) %>%
    filter(!is.na(AHAID))
})

# Read in AHA main survey
AHAmainsurvey <- read_csv(paste0(raw_data_path,"AHA_mainsurvey.csv"))

# I define low integration and high integration as in Madison (2004, HSR)
AHAmainsurvey <- AHAmainsurvey %>%
  mutate(ID=as.character(ID)) %>%
  mutate(low_integration=ifelse(IPAHOS==1 | OPHOHOS==1 | CPHOHOS==1,1,0),
         high_integration=ifelse(GPWWHOS==1 | MSOHOS==1 | ISMHOS==1 | EQMODHOS==1,1,0)) %>%
  dplyr::rename(Hospital_name=MNAME, year=YEAR, full_year=FYR, 
                joint_phys=JNTPH, total_physicians=FTMT, phys_owned=PHYGP, beds=BDTOT ) %>%
  select(-IPAHOS, -OPHOHOS, -CPHOHOS, -GPWWHOS, -MSOHOS, -ISMHOS, -EQMODHOS) %>%
  select(-IPAP, -GPWP, -OPHP, -CPHP, -ISMP, -EQMP, -FNDP, -full_year, -EHLTHI, -EHLTHS,
         -total_physicians, -joint_phys, -phys_owned)


Data_List <- lapply(Data_List, function(x){
  data <- x %>% ungroup() %>%
    mutate(AHAID=as.character(AHAID)) %>%
    left_join(AHAmainsurvey,by=c("AHAID"="ID","year"),na_matches="never")
})

Data_List <- lapply(Data_List, function(x){
  data <- x %>% group_by(AHAID) %>%
    mutate(firstyear_0=min(year[EHLTH==0],na.rm=T),lastyear_0=max(year[EHLTH==0],na.rm=T)) %>%
    mutate(firstyear_0=ifelse(is.infinite(firstyear_0),NA,firstyear_0),lastyear_0=ifelse(is.infinite(lastyear_0),NA,lastyear_0)) %>%
    mutate(EHLTH=ifelse(firstyear_0<year & year<lastyear_0 & is.na(EHLTH),0,EHLTH)) %>%
    mutate(firstyear_1=min(year[EHLTH==1],na.rm=T),lastyear_1=max(year[EHLTH==1],na.rm=T)) %>%
    mutate(firstyear_1=ifelse(is.infinite(firstyear_1),NA,firstyear_1),lastyear_1=ifelse(is.infinite(lastyear_1),NA,lastyear_1)) %>%
    mutate(EHLTH=ifelse(firstyear_1<year & year<lastyear_1 & is.na(EHLTH),1,EHLTH)) %>%
    mutate(firstyear_2=min(year[EHLTH==2],na.rm=T),lastyear_2=max(year[EHLTH==2],na.rm=T)) %>%
    mutate(firstyear_2=ifelse(is.infinite(firstyear_2),NA,firstyear_2),lastyear_2=ifelse(is.infinite(lastyear_2),NA,lastyear_2)) %>%
    mutate(EHLTH=ifelse(firstyear_2<year & year<lastyear_2 & is.na(EHLTH),2,EHLTH)) %>%
    ungroup() %>%
    select(-firstyear_0, -firstyear_1, -firstyear_2, -lastyear_0, -lastyear_1, -lastyear_2)
})

# Read in hospitals with data assistants
Hosp_with_DA <- readRDS(paste0(created_data_path,"/Hosp_with_DA.rds"))

Hosp_with_DA <- Hosp_with_DA %>%
  mutate(DA=1) %>%
  mutate(HospNPI=as.numeric(HospNPI))

Data_List <- lapply(Data_List, function(x){
  data <- x %>%
    left_join(Hosp_with_DA, by=c("year","HospNPI")) %>%
    mutate(DA=ifelse(is.na(DA),0,DA))
})

# Get rid of physicians who only work on VA hospitals

Data_List <- lapply(Data_List, function(x){
  VA <- x %>%
    mutate(VA=ifelse(str_detect(Hospital_name,"Veteran"),1,0)) %>%
    filter(VA==1) %>%
    distinct(pairID,VA)
  
  data <- x %>%
    left_join(VA,by="pairID") %>%
    filter(is.na(VA)) %>%
    select(-VA)
  
  return(data)
})

# Descriptive Variables
Data_List <- lapply(Data_List, function(x){
  low_beds <- x %>% ungroup() %>%
    distinct(HospNPI, year, beds) %>%
    filter(beds<10) %>%
    distinct(HospNPI) %>%
    mutate(low_beds=1)
  
  data <- x %>%
    left_join(low_beds, by="HospNPI") %>%
    filter(is.na(low_beds)) %>%
    select(-low_beds) %>%
    group_by(DocNPI,year) %>%
    mutate(avg_beds=mean(beds,na.rm=T)) %>%
    mutate(avg_beds=round(avg_beds,1)) %>%
    ungroup() %>%
    mutate(experience=year-grad_year)
  
  return(data)
})

# EHR Variables
Data_List <- lapply(Data_List, function(x){
  data <- x %>%
    mutate(EHR=ifelse(EHLTH==2,1,ifelse(EHLTH==0 | EHLTH==1,0,NA))) %>%
    select(-EHLTH)
  
  # In some cases, 2009 or 2015 can be filled in to not have a missing value. 
  # Create dataset to fill in 2009 or 2015 conditionally
  fill_in <- data %>%
    filter((year==2010 & EHR==0) | (year==2014 & EHR==1)) %>%
    mutate(change2009=ifelse(year==2010,1,0),
           change2015=ifelse(year==2014,1,0)) %>%
    select(HospNPI,DocNPI,year,EHR,change2009,change2015) %>%
    distinct(HospNPI,year,EHR,change2009,change2015)
  
  fill_in <- fill_in %>%
    group_by(HospNPI) %>%
    mutate(change2009=ifelse(sum(change2009)>0,1,0),
           change2015=ifelse(sum(change2015)>0,1,0)) %>%
    ungroup() %>%
    distinct(HospNPI,change2009,change2015)
  
  # Merge it back to original
  data <- data %>%
    left_join(fill_in,by="HospNPI")
  
  # Fill in the qualifying missing values
  data <- data %>%
    mutate(EHR=ifelse(year==2009 & change2009==1 & is.na(EHR),0,EHR)) %>%
    mutate(EHR=ifelse(year==2015 & change2015==1 & is.na(EHR),1,EHR)) %>%
    select(-change2009, -change2015) %>%
    ungroup()
  
  return(data)
})

Data_List <- lapply(Data_List, function(x){
  # Create measure of number of hospitals a physician works with that use an EHR
  # Create number of hospitals variables
  data <- x %>%
    group_by(DocNPI,year) %>%
    mutate(count=1) %>%
    mutate(num_hospitals=sum(count)) %>%
    select(-count)
  
  
  # Create number hospitals with general EHR, dec EHR, doc EHR
  data <- data %>% ungroup() %>%
    group_by(DocNPI,year) %>%
    mutate(num_hosp_EHR=sum(EHR,na.rm=T)) %>%
    ungroup()
  
  
  # Variables: fraction of hospitals with EHR (three types)
  data <- data %>% ungroup() %>%
    mutate(frac_EHR=ifelse(num_hospitals>0,num_hosp_EHR/num_hospitals,NA))

  return(data)
  
})



Data_List <- lapply(Data_List, function(x){
  # Treatment Variable: Indicator for Exposed to EHR in any hospital (general EHR and decision making EHR)
  # Create variable for first year a doc was exposed to EHR
  minyr_EHR <- x %>% ungroup() %>%
    distinct(DocNPI, year, frac_EHR) %>%
    filter(frac_EHR>0) %>%
    group_by(DocNPI) %>%
    mutate(minyr_EHR=min(year)) %>%
    ungroup() %>%
    distinct(DocNPI,minyr_EHR)
  
  data <- x %>% ungroup() %>%
    left_join(minyr_EHR, by="DocNPI") %>%
    mutate(minyr_EHR=ifelse(is.na(minyr_EHR),0,minyr_EHR))
  
  return(data)
})

Data_List <- lapply(Data_List, function(x){
  minyr_EHR_int <- x %>% ungroup() %>%
    distinct(DocNPI, year, frac_EHR,high_integration) %>%
    filter(frac_EHR>0 & high_integration==0) %>%
    group_by(DocNPI) %>%
    mutate(minyr_EHR_int=min(year)) %>%
    ungroup() %>%
    distinct(DocNPI,minyr_EHR_int)
  
  data <- x %>% ungroup() %>%
    left_join(minyr_EHR_int, by="DocNPI") %>%
    mutate(minyr_EHR_int=ifelse(is.na(minyr_EHR_int),0,minyr_EHR_int))
  
  return(data)
  
})

Data_List <- lapply(Data_List, function(x){
  count_sys <- x %>%
    ungroup() %>%
    distinct(DocNPI,year,SYSID) %>%
    count(DocNPI,year,name="num_systems")
  
  data <- x %>%
    left_join(count_sys,by=c("year","DocNPI"))
  
  return(data)
})

Data_List <- lapply(Data_List, function(x){
  # Create variable: new NPI introduced
  data <- x %>%
    group_by(DocNPI) %>%
    mutate(max_numhosp = max(num_hospitals)) %>%
    ungroup()
  
  data <- data %>%
    mutate(same=ifelse(num_hospitals==max_numhosp,1,0))
  
  data <- data %>%
    mutate(same2009=ifelse(year==2009 & same==1, 1,NA),
           same2009=ifelse(year==2009 & same==0, 0,same2009),
           same2010=ifelse(year==2010 & same==1, 1,NA),
           same2010=ifelse(year==2010 & same==0, 0,same2010),
           same2011=ifelse(year==2011 & same==1, 1,NA),
           same2011=ifelse(year==2011 & same==0, 0,same2011),
           same2012=ifelse(year==2012 & same==1, 1,NA),
           same2012=ifelse(year==2012 & same==0, 0,same2012),
           same2013=ifelse(year==2013 & same==1, 1,NA),
           same2013=ifelse(year==2013 & same==0, 0,same2013),
           same2014=ifelse(year==2014 & same==1, 1,NA),
           same2014=ifelse(year==2014 & same==0, 0,same2014),
           same2015=ifelse(year==2015 & same==1, 1,NA),
           same2015=ifelse(year==2015 & same==0, 0,same2015)) %>%
    group_by(DocNPI) %>%
    fill(same2009,.direction="downup") %>%
    fill(same2010,.direction="downup") %>%
    fill(same2011,.direction="downup") %>%
    fill(same2012,.direction="downup") %>%
    fill(same2013,.direction="downup") %>%
    fill(same2014,.direction="downup") %>%
    fill(same2015,.direction="downup") %>%
    ungroup()
  
  data <- data %>%
    mutate(newnpi=ifelse(year==2010 & same2009==0 & same2010==1,1,NA)) %>%
    mutate(newnpi=ifelse(year==2011 & same2010==0 & same2011==1,1,newnpi)) %>%
    mutate(newnpi=ifelse(year==2012 & same2011==0 & same2012==1,1,newnpi)) %>%
    mutate(newnpi=ifelse(year==2013 & same2012==0 & same2013==1,1,newnpi)) %>%
    mutate(newnpi=ifelse(year==2014 & same2013==0 & same2014==1,1,newnpi)) %>%
    mutate(newnpi=ifelse(year==2015 & same2014==0 & same2015==1,1,newnpi)) %>%
    mutate(newnpi=ifelse(is.na(newnpi),0,newnpi))
  
  data <- data %>%
    select(-same,-same2009,-same2010,-same2011,-same2012,-same2013,-same2014,-same2015)
  
  # Create variable for never adds a new npi
  data <- data %>%
    group_by(DocNPI) %>%
    mutate(sum=sum(newnpi)) %>%
    ungroup() %>%
    mutate(never_newnpi=ifelse(sum==0,1,0)) %>%
    select(-sum)
  
  # Create variable for "works with hospital with DA"
  data <- data %>%
    group_by(DocNPI,year) %>%
    mutate(sum=sum(DA)) %>%
    mutate(works_with_DA=ifelse(sum>0,1,0)) %>%
    ungroup()
  
  return(data)
  
})

Aggregated_Pairs_list <- lapply(Data_List, function(x){
  data <- x %>%
    distinct(year, DocNPI,grad_year, avg_beds, experience, 
             num_hospitals, works_with_DA,
             frac_EHR, minyr_EHR, minyr_EHR_int,
             num_systems, 
             newnpi, never_newnpi)
  
  return(data)
})

Aggregated_Pairs_list <- lapply(Aggregated_Pairs_list, function(x){
  # Now complete the data
  data <- complete(x,DocNPI,year=2009:2017)
  
  data <- data %>%
    group_by(DocNPI) %>%
    fill(minyr_EHR,.direction="downup") %>%
    fill(minyr_EHR_int,.direction="downup") %>%
    fill(grad_year,.direction="downup") %>%
    fill(avg_beds,.direction="downup") %>%
    fill(num_hospitals,.direction="downup") %>%
    fill(num_systems,.direction="downup") %>%
    ungroup() %>%
    mutate(experience=year-grad_year) %>%
    mutate(anyEHR_exposed=ifelse(minyr_EHR>0 & year>=minyr_EHR,1,0),
           anyEHR_LI_exposed=ifelse(minyr_EHR_int>0 & year>=minyr_EHR_int,1,0))

  
  return(data)
})

#### DATA 5 -------------------------------------------------

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

Physician_Data_list <- lapply(Aggregated_Pairs_list, function(x){
  Physician_Data <- x %>%
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
  
  return(Physician_Data)
})

Physician_Data_list <- lapply(Physician_Data_list, function(x){
  data <- x %>%
    dplyr::group_by(DocNPI) %>%
    dplyr::mutate(max=max(pos_inpat,na.rm=T)) %>%
    mutate(max_70=ifelse(max>.7,1,0),
           max_90=ifelse(max>.9,1,0),
           max_50=ifelse(max>.5,1,0))%>%
    filter(max_50==1 | max_70==1 | max_90==1)
  
  data <- data %>%
    mutate(yearbefore=ifelse(year==minyr_EHR-1,pos_inpat,NA)) %>%
    group_by(DocNPI) %>%
    fill(yearbefore,.direction="downup")
  
  data <- data %>%
    filter(yearbefore>.5 | is.na(yearbefore))
  
  return(data)
})

rm(MDPPAS2009, MDPPAS2010, MDPPAS2011, MDPPAS2012, MDPPAS2013, MDPPAS2014, MDPPAS2015, 
   MDPPAS2016, MDPPAS2017, observe, Physician_Data_10, Physician_Data_30, Physician_Data_60, 
   year, AHAmainsurvey, AHANPI_cw, Hosp_with_DA, phys_hosp_pairs)

# Retirement
Physician_Data_list <- lapply(Physician_Data_list, function(x){
  # For retirement, I can combine all claim counts into one
  Physician_Data <- x %>%
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
  
  return(Physician_Data)
  
})

# Office
Physician_Data_list <- lapply(Physician_Data_list, function(x){
  Physician_Data <- x %>%
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

  # Create variable for total number of patients in office
  Physician_Data <- Physician_Data %>%
    mutate(total_office=pos_office*npi_unq_benes)
  
  Physician_Data <- Physician_Data %>%
    mutate(total_office_prior=ifelse(office_yearprior==1,total_office,NA),
           total_office_noprior=ifelse(office_yearprior==0,total_office,NA))
  
  # Create variable for majority patients in office
  Physician_Data <- Physician_Data %>%
    mutate(majority_in_office=ifelse(pos_office>pos_inpat,1,0))
  
  return(Physician_Data)
})

#Zip Codes
Physician_Data_list <- lapply(Physician_Data_list, function(x){
  Physician_Data <- x %>% dplyr::ungroup() %>%
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
})



# Productivity
Physician_Data_list <- lapply(Physician_Data_list, function(x){
  Physician_Data <- x %>%
    mutate(npi_unq_benes=ifelse(is.na(npi_unq_benes),0,npi_unq_benes))
  
  Physician_Data <- Physician_Data %>%
    mutate(claim_per_patient=claim_count_total/npi_unq_benes)
  
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
  
  # Create age bins to do analysis separately 
  Physician_Data <- Physician_Data %>% ungroup() %>%
    select(DocNPI, year, grad_year, minyr_EHR, minyr_EHR_int, never_newnpi, works_with_DA,
           pos_office, npi_unq_benes, max_70, max_90, max_50, claim_count_total, retire,
           ever_retire, work_in_office, change_zip, num_hospitals, max_age)
  
  
  return(Physician_Data)
})

data_coms <- crossing(inpat_70=c(TRUE, FALSE), inpat_90=c(TRUE, FALSE),
                      inpat_50=c(TRUE, FALSE), main_EHR=c(TRUE, FALSE), no_DA=c(TRUE, FALSE),
                      sp_30=c(TRUE,FALSE), sp_10=c(TRUE,FALSE), sp_60=c(TRUE,FALSE),
                      anticipation=c(TRUE,FALSE), limit_years=c(TRUE,FALSE)) %>%
  filter((inpat_70==FALSE & inpat_50==FALSE & inpat_90==TRUE) |
           (inpat_70==FALSE & inpat_50==TRUE & inpat_90==FALSE) |
           (inpat_70==TRUE & inpat_50==FALSE & inpat_90==FALSE)) %>%
  filter((sp_30==FALSE & sp_10==FALSE & inpat_90==TRUE) |
           (sp_30==FALSE & sp_10==TRUE & sp_60==FALSE) |
           (sp_30==TRUE & sp_10==FALSE & sp_60==FALSE))

Subsets_Data <- list()


for (k in 1:128){
  row <- data_coms[k,]
  
  if (row$sp_30==TRUE){
    subset_data <- Physician_Data_list[[2]] %>%
      {if (row$inpat_50==TRUE) dplyr::filter(.,max_50==1) else if (row$inpat_90==TRUE) dplyr::filter(.,max_90==1) else if (row$inpat_70==TRUE) dplyr::filter(.,max_70==1)} %>%
      mutate(treat_variable=ifelse(row$main_EHR==TRUE,"minyr_EHR", "minyr_EHR_int")) %>%
      {if (row$no_DA==TRUE) filter(.,works_with_DA==0) else filter(.,year>2000)} %>%
      mutate(retiredrop=ifelse(treat_variable=='minyr_EHR' & minyr_EHR==0,1,0)) %>%
      mutate(retiredrop=ifelse(treat_variable=='minyr_EHR_int' & minyr_EHR_int==0,1,retiredrop))}
  
  if (row$sp_10==TRUE){
    subset_data <- Physician_Data_list[[1]] %>%
      {if (row$inpat_50==TRUE) dplyr::filter(.,max_50==1) else if (row$inpat_90==TRUE) dplyr::filter(.,max_90==1) else if (row$inpat_70==TRUE) dplyr::filter(.,max_70==1)} %>%
      mutate(treat_variable=ifelse(row$main_EHR==TRUE,"minyr_EHR", "minyr_EHR_int")) %>%
      {if (row$no_DA==TRUE) filter(.,works_with_DA==0) else filter(.,year>2000)}  %>%
      mutate(retiredrop=ifelse(treat_variable=='minyr_EHR' & minyr_EHR==0,1,0)) %>%
      mutate(retiredrop=ifelse(treat_variable=='minyr_EHR_int' & minyr_EHR_int==0,1,retiredrop))}
    
  
  if (row$sp_60==TRUE){
    subset_data <- Physician_Data_list[[3]] %>%
      {if (row$inpat_50==TRUE) dplyr::filter(.,max_50==1) else if (row$inpat_90==TRUE) dplyr::filter(.,max_90==1) else if (row$inpat_70==TRUE) dplyr::filter(.,max_70==1)} %>%
      mutate(treat_variable=ifelse(row$main_EHR==TRUE,"minyr_EHR", "minyr_EHR_int")) %>%
      {if (row$no_DA==TRUE) filter(.,works_with_DA==0) else filter(.,year>2000)}  %>%
      mutate(retiredrop=ifelse(treat_variable=='minyr_EHR' & minyr_EHR==0,1,0)) %>%
      mutate(retiredrop=ifelse(treat_variable=='minyr_EHR_int' & minyr_EHR_int==0,1,retiredrop))}
  
  Subsets_Data[[k]] <- list(subset_data,row)
}

saveRDS(Subsets_Data, "CreatedData/Subsets_Data.rds")

Subsets_Data_Young <- lapply(Subsets_Data, function(x){
  row <- x[[2]]
  data <- x[[1]] %>%
    filter(max_age<60)
  list(data,row)
})


#RETIRE SPECIFICATION CHART ---------------------------------------------------------------------
models <- lapply(Subsets_Data, function(x) {
  row <- x[[2]]
  
  all <- att_gt(yname = "retire",                # LHS Variable
                gname = x[[1]]$treat_variable[[1]],          # First year a unit is treated. (set to 0 if never treated)
                idname = "DocNPI",               # ID
                tname = "year",                  # Time Variable
                xformla = ~grad_year,            # Time-invariant controls
                data=if (row$limit_years==TRUE) (dplyr::filter(x[[1]],retiredrop!=1 & year<2016)) else (dplyr::filter(x[[1]],retiredrop!=1)),
                est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
                control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
                clustervars = "DocNPI",          # Cluster Variables          
                anticipation=ifelse(row$anticipation==TRUE,1,0),
                base_period = "varying" # can set a number of years to account for anticipation effects
  )
  
  list(all=all, row=row)
})

models_agg <- lapply(models, function(x) {
  row <- x[[2]]
  
  agg <- aggte(x[[1]], type = "dynamic",na.rm=T)
  
  list(agg=agg, row=row)
})

chart_data <- lapply(models_agg, function(x) {
  row <- x[[2]] %>%
    mutate(coef=x[["agg"]][["overall.att"]],
           se=x[["agg"]][["overall.se"]])
  trow <- t(row)
  list(trow)
})

chart_data_frame <- t(as.data.frame(chart_data))
rownames(chart_data_frame) <- NULL

chart_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, inpat_50, inpat_70, inpat_90, 
         sp_10, sp_30, sp_60, main_EHR, no_DA, anticipation, limit_years) 

par(oma=c(1,0,1,1))
source(paste0(function_path,"spec_chart_function.R"))

# Create plot
labels <- list("Perc. Inpatient Threshold: "=c("50% ", "70% ", "90% "),
               "Shared Patient Threshold: "=c("10 ", "30 ", "60 "),
               "Main EHR Definition ",
               "Only Hospitals without DA ",
               "Anticipation: 1 year ",
               "Limited Years ")

mu  <- mean(chart_data_frame$coef) # mean of all coefficients
sd  <- sd(chart_data_frame$coef)

schart(chart_data_frame, labels, highlight=117, 
       order="increasing", heights=c(1,.75),
       band.ref=c(mu+sd, mu-sd),
       col.band.ref="#E69F00",
       col.est=c("grey70","#009E73"),
       col.dot=c("grey70", "grey90", "#009E73", "#009E73"),
       ylab="Coefficient and 95% C.I.")
legend("bottomright", lwd=2:2, col=c("#E69F00", "#009E73"), c("Average Conf. Interval", "Main Specification"), inset=.02)







# FRACTION IN OFFICE SPECIFICATION CHART --------------------------------------------
models <- lapply(Subsets_Data, function(x) {
  row <- x[[2]]
  
  all <- att_gt(yname = "pos_office",                # LHS Variable
                gname = x[[1]]$treat_variable[[1]],          # First year a unit is treated. (set to 0 if never treated)
                idname = "DocNPI",               # ID
                tname = "year",                  # Time Variable
                xformla = ~grad_year,            # Time-invariant controls
                data=if (row$limit_years==TRUE) (dplyr::filter(x[[1]],year<2016 & ever_retire==0)) else (dplyr::filter(x[[1]],retiredrop!=1 & ever_retire==0)),
                est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
                control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
                clustervars = "DocNPI",          # Cluster Variables          
                anticipation=ifelse(row$anticipation==TRUE,1,0),
                base_period = "varying" # can set a number of years to account for anticipation effects
  )
  
  list(all=all, row=row)
})

models_agg <- lapply(models, function(x) {
  row <- x[[2]]
  
  agg <- aggte(x[[1]], type = "dynamic",na.rm=T)
  
  list(agg=agg, row=row)
})

chart_data <- lapply(models_agg, function(x) {
  row <- x[[2]] %>%
    mutate(coef=x[["agg"]][["overall.att"]],
           se=x[["agg"]][["overall.se"]])
  trow <- t(row)
  list(trow)
})

chart_data_frame <- t(as.data.frame(chart_data))
rownames(chart_data_frame) <- NULL

chart_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, inpat_50, inpat_70, inpat_90, 
         sp_10, sp_30, sp_60, main_EHR, no_DA, anticipation, limit_years) 

par(oma=c(1,0,1,1))

# Create plot
mu  <- mean(chart_data_frame$coef) # mean of all coefficients
sd  <- sd(chart_data_frame$coef)

schart(chart_data_frame, labels, highlight=117, 
       order="increasing", heights=c(1,.75),
       band.ref=c(mu+sd, mu-sd),
       col.band.ref="#E69F00",
       col.est=c("grey70","#009E73"),
       col.dot=c("grey70", "grey90", "#009E73", "#009E73"),
       ylab="Coefficient and 95% C.I.")
legend("bottomright", lwd=2:2, col=c("#E69F00", "#009E73"), c("Average Conf. Interval", "Main Specification"), inset=.02)




# OFFICE INDICATOR SPECIFICATION CHART -------------------------------------------
models <- lapply(Subsets_Data, function(x) {
  row <- x[[2]]
  
  all <- att_gt(yname = "work_in_office",                # LHS Variable
                gname = x[[1]]$treat_variable[[1]],          # First year a unit is treated. (set to 0 if never treated)
                idname = "DocNPI",               # ID
                tname = "year",                  # Time Variable
                xformla = ~grad_year,            # Time-invariant controls
                data=if (row$limit_years==TRUE) (dplyr::filter(x[[1]],year<2016 & ever_retire==0)) else (dplyr::filter(x[[1]],retiredrop!=1 & ever_retire==0)),
                est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
                control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
                clustervars = "DocNPI",          # Cluster Variables          
                anticipation=ifelse(row$anticipation==TRUE,1,0),
                base_period = "varying" # can set a number of years to account for anticipation effects
  )
  
  list(all=all, row=row)
})

models_agg <- lapply(models, function(x) {
  row <- x[[2]]
  
  agg <- aggte(x[[1]], type = "dynamic",na.rm=T)
  
  list(agg=agg, row=row)
})

chart_data <- lapply(models_agg, function(x) {
  row <- x[[2]] %>%
    mutate(coef=x[["agg"]][["overall.att"]],
           se=x[["agg"]][["overall.se"]])
  trow <- t(row)
  list(trow)
})

chart_data_frame <- t(as.data.frame(chart_data))
rownames(chart_data_frame) <- NULL

chart_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, inpat_50, inpat_70, inpat_90, 
         sp_10, sp_30, sp_60, main_EHR, no_DA, anticipation,
         limit_years) 

par(oma=c(1,0,1,1))

# Create plot
mu  <- mean(chart_data_frame$coef) # mean of all coefficients
sd  <- sd(chart_data_frame$coef)

schart(chart_data_frame, labels, highlight=117, 
       order="increasing", heights=c(1,.75),
       band.ref=c(mu+sd, mu-sd),
       col.band.ref="#E69F00",
       col.est=c("grey70","#009E73"),
       col.dot=c("grey70", "grey90", "#009E73", "#009E73"),
       ylab="Coefficient and 95% C.I.")
legend("bottomright", lwd=2:2, col=c("#E69F00", "#009E73"), c("Average Conf. Interval", "Main Specification"), inset=.02)


# CHANGE ZIP SPECIFICATION CHART ---------------------------------------------- ####
models <- lapply(Subsets_Data, function(x) {
  row <- x[[2]]
  
  all <- att_gt(yname = "change_zip",                # LHS Variable
                gname = x[[1]]$treat_variable[[1]],          # First year a unit is treated. (set to 0 if never treated)
                idname = "DocNPI",               # ID
                tname = "year",                  # Time Variable
                xformla = ~grad_year,            # Time-invariant controls
                data=if (row$limit_years==TRUE) (dplyr::filter(x[[1]],year<2016 & ever_retire==0)) else (dplyr::filter(x[[1]],retiredrop!=1 & ever_retire==0)),
                est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
                control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
                clustervars = "DocNPI",          # Cluster Variables          
                anticipation=ifelse(row$anticipation==TRUE,1,0),
                base_period = "varying" # can set a number of years to account for anticipation effects
  )
  
  list(all=all, row=row)
})

models_agg <- lapply(models, function(x) {
  row <- x[[2]]
  
  agg <- aggte(x[[1]], type = "dynamic",na.rm=T)
  
  list(agg=agg, row=row)
})

chart_data <- lapply(models_agg, function(x) {
  row <- x[[2]] %>%
    mutate(coef=x[["agg"]][["overall.att"]],
           se=x[["agg"]][["overall.se"]])
  trow <- t(row)
  list(trow)
})

chart_data_frame <- t(as.data.frame(chart_data))
rownames(chart_data_frame) <- NULL

chart_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, inpat_50, inpat_70, inpat_90, 
         sp_10, sp_30, sp_60, main_EHR, no_DA, anticipation,
         limit_years) 

par(oma=c(1,0,1,1))

# Create plot
mu  <- mean(chart_data_frame$coef) # mean of all coefficients
sd  <- sd(chart_data_frame$coef)

schart(chart_data_frame, labels, highlight=117, 
       order="increasing", heights=c(1,.75),
       band.ref=c(mu+sd, mu-sd),
       col.band.ref="#E69F00",
       col.est=c("grey70","#009E73"),
       col.dot=c("grey70", "grey90", "#009E73", "#009E73"),
       ylab="Coefficient and 95% C.I.")
legend("bottomright", lwd=2:2, col=c("#E69F00", "#009E73"), c("Average Conf. Interval", "Main Specification"), inset=.02)


# PATIENT COUNT ------------------------------------------------------------- #####

models <- lapply(Subsets_Data, function(x) {
  row <- x[[2]]
  
  all <- att_gt(yname = "npi_unq_benes",                # LHS Variable
                gname = x[[1]]$treat_variable[[1]],          # First year a unit is treated. (set to 0 if never treated)
                idname = "DocNPI",               # ID
                tname = "year",                  # Time Variable
                xformla = ~grad_year,            # Time-invariant controls
                data=if (row$limit_years==TRUE) (dplyr::filter(x[[1]],year<2016 & ever_retire==0 & never_newnpi==1)) else (dplyr::filter(x[[1]],retiredrop!=1 & ever_retire==0 & never_newnpi==1)),
                est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
                control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
                clustervars = "DocNPI",          # Cluster Variables          
                anticipation=ifelse(row$anticipation==TRUE,1,0),
                base_period = "varying" # can set a number of years to account for anticipation effects
  )
  
  list(all=all, row=row)
})

models_agg <- lapply(models, function(x) {
  row <- x[[2]]
  
  agg <- aggte(x[[1]], type = "dynamic",na.rm=T)
  
  list(agg=agg, row=row)
})

chart_data <- lapply(models_agg, function(x) {
  row <- x[[2]] %>%
    mutate(coef=x[["agg"]][["overall.att"]],
           se=x[["agg"]][["overall.se"]])
  trow <- t(row)
  list(trow)
})

chart_data_frame <- t(as.data.frame(chart_data))
rownames(chart_data_frame) <- NULL

chart_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, inpat_50, inpat_70, inpat_90, 
         sp_10, sp_30, sp_60, main_EHR, no_DA, anticipation,
         limit_years) 

par(oma=c(1,0,1,1))

# Create plot
mu  <- mean(chart_data_frame$coef) # mean of all coefficients
sd  <- sd(chart_data_frame$coef)

schart(chart_data_frame, labels, highlight=117, 
       order="increasing", heights=c(1,.75),
       band.ref=c(mu+sd, mu-sd),
       col.band.ref="#E69F00",
       col.est=c("grey70","#009E73"),
       col.dot=c("grey70", "grey90", "#009E73", "#009E73"),
       ylab="Coefficient and 95% C.I.")
legend("bottomright", lwd=2:2, col=c("#E69F00", "#009E73"), c("Average Conf. Interval", "Main Specification"), inset=.02)


Subsets_Data <- lapply(Subsets_Data, function(x){
  row <- x[[2]]
  data <- x[[1]] %>%
    mutate(claim_per_patient=claim_count_total/npi_unq_benes)
list(data,row)
  })


# CLAIMS PER PATIENT SPECIFICATION CHART ------------------------------------ ####
models <- lapply(Subsets_Data, function(x) {
  row <- x[[2]]
  
  all <- att_gt(yname = "claim_per_patient",                # LHS Variable
                gname = x[[1]]$treat_variable[[1]],          # First year a unit is treated. (set to 0 if never treated)
                idname = "DocNPI",               # ID
                tname = "year",                  # Time Variable
                xformla = ~grad_year,            # Time-invariant controls
                data=if (row$limit_years==TRUE) (dplyr::filter(x[[1]],year<2016 & ever_retire==0 & never_newnpi==1)) else (dplyr::filter(x[[1]],retiredrop!=1 & ever_retire==0 & never_newnpi==1)),
                est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
                control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
                clustervars = "DocNPI",          # Cluster Variables          
                anticipation=ifelse(row$anticipation==TRUE,1,0),
                base_period = "varying" # can set a number of years to account for anticipation effects
  )
  
  list(all=all, row=row)
})

models_agg <- lapply(models, function(x) {
  row <- x[[2]]
  
  agg <- aggte(x[[1]], type = "dynamic",na.rm=T)
  
  list(agg=agg, row=row)
})

chart_data <- lapply(models_agg, function(x) {
  row <- x[[2]] %>%
    mutate(coef=x[["agg"]][["overall.att"]],
           se=x[["agg"]][["overall.se"]])
  trow <- t(row)
  list(trow)
})

chart_data_frame <- t(as.data.frame(chart_data))
rownames(chart_data_frame) <- NULL

chart_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, inpat_50, inpat_70, inpat_90, 
         sp_10, sp_30, sp_60, main_EHR, no_DA, anticipation,
         limit_years) 

par(oma=c(1,0,1,1))

# Create plot
mu  <- mean(chart_data_frame$coef) # mean of all coefficients
sd  <- sd(chart_data_frame$coef)

schart(chart_data_frame, labels, highlight=117, 
       order="increasing", heights=c(1,.75),
       band.ref=c(mu+sd, mu-sd),
       col.band.ref="#E69F00",
       col.est=c("grey70","#009E73"),
       col.dot=c("grey70", "grey90", "#009E73", "#009E73"),
       ylab="Coefficient and 95% C.I.")
legend("bottomright", lwd=2:2, col=c("#E69F00", "#009E73"), c("Average Conf. Interval", "Main Specification"), inset=.02)





