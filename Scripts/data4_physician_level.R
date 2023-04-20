library(tidyverse)
library(readr)
library(plm)
library(kableExtra)


# --------------------    Build Variables Onto Main Hospital-Physician Pair Data --------------------------
#                         Hanna Glenn, Emory University
#                         9/3/2021

# ORDER::::: 4

# This script develops the main dataset to be used in the paper.
# This builds on the physician-hospital pairs created in "data3_pairs.R"
# First, I bring in (raw) Physician Compare for information about medical school graduation and gender. 
# For EHR information, I use the (raw) main and supplement AHA surveys.
# The final dataset is called "Data.rds"

# Read in physician hospital pairs created in "phys_hosp_pairs.r"
data <- read_rds(paste0(created_data_path, "phys_hosp_pairs.rds"))
  # This data does not have any repeats of year, HospNPI, DocNPI
  # 1.3 mill obs, 5 variables

data <- data %>%
  rename(pairID=ID) 

# Create Experience Variable ------------------------------------------------------------
# Read in Physician Compare
PhysCompare <- read_csv(paste0(raw_data_path,"DAC_NationalDownloadableFile.csv"))

PhysCompare <- PhysCompare %>%
  dplyr::rename(gender=gndr,grad_year=`Grd_yr`) %>%
  select(NPI,grad_year) 

# Merge, then drop any physicians that graduated after 2005
data <- data %>%
  left_join(PhysCompare, by=c("DocNPI"="NPI")) %>%
  distinct() %>%
  filter(grad_year<2005)
  # Now have 902k obs


#### Create EHR Variables ---------------------------------------------------------------------- ####
#Read in AHA-NPI crosswalk data
AHANPI_cw <- read_rds(paste0(created_data_path,"AHANPI_cw.rds"))

# Merge crosswalk to main data
data <- data %>%
  mutate(HospNPI=as.double(HospNPI)) %>%
  left_join(AHANPI_cw,by=c("HospNPI"="NPI"))

# See how many unique hospitals in the dataset have an AHAID
num_hosp <- data %>% ungroup() %>%
  filter(!is.na(AHAID)) %>%
  distinct(HospNPI)
  # There are 4267 unique hospitals in the data that are not missing AHAID (good)

# Only keep pairs with AHA hospitals since that is where EHR information comes from
data <- data %>%
  filter(!is.na(AHAID))
  #777k obs

# Read in main AHA survey data
# I need to think about adding more hospital characteristics to this dataset
AHAmainsurvey <- read_csv(paste0(raw_data_path,"AHA_mainsurvey.csv"))

AHAmainsurvey <- AHAmainsurvey %>%
  mutate(ID=as.character(ID)) %>%
  dplyr::rename(Hospital_name=MNAME, year=YEAR, full_year=FYR, 
         joint_phys=JNTPH, total_physicians=FTMT, phys_owned=PHYGP, beds=BDTOT ) %>%
  select(-IPAHOS, -OPHOHOS, -CPHOHOS, -GPWWHOS, -MSOHOS, -ISMHOS, -EQMODHOS) %>%
  select(-IPAP, -GPWP, -OPHP, -CPHP, -ISMP, -EQMP, -FNDP, -full_year, -EHLTHI, -EHLTHS,
         -total_physicians, -joint_phys, -phys_owned)

data <- data %>% ungroup() %>%
  mutate(AHAID=as.character(AHAID)) %>%
  left_join(AHAmainsurvey,by=c("AHAID"="ID","year"),na_matches="never")

# Read in HIMSS data for additional EHR information
for (t in 2009:2017){
  year <- readxl::read_xlsx(paste0(raw_data_path,"/HIMSS/HostedSoftware",t,".xlsx")) %>%
    mutate(year=t) %>%
    select(HAEntityId, SurveyId, VendorName, year)
  
  assign(paste0("HostedSoftware",t),year)
}

HostedSoftware <- rbind(HostedSoftware2009, HostedSoftware2010, HostedSoftware2011, HostedSoftware2012, HostedSoftware2013, HostedSoftware2014, HostedSoftware2015, HostedSoftware2016, HostedSoftware2017)
rm(HostedSoftware2009, HostedSoftware2010, HostedSoftware2011, HostedSoftware2012, HostedSoftware2013, HostedSoftware2014, HostedSoftware2015, HostedSoftware2016, HostedSoftware2017)

# Remove duplicates in Hosted Software
HostedSoftware <- HostedSoftware %>%
  distinct()

for (t in 2009:2017){
  year <- readxl::read_xlsx(paste0(raw_data_path,"/HIMSS/HAEntity",t,".xlsx")) %>%
    mutate(year=t) %>%
    select(HAEntityId, SurveyId, ParentId, UniqueId, year, HAEntityType, MedicareNumber)
  
  assign(paste0("HAEntity",t),year)
}

HAEntity <- rbind(HAEntity2009, HAEntity2010, HAEntity2011, HAEntity2012, HAEntity2013, HAEntity2014, HAEntity2015, HAEntity2016, HAEntity2017)
rm(HAEntity2009, HAEntity2010, HAEntity2011, HAEntity2012, HAEntity2013, HAEntity2014, HAEntity2015, HAEntity2016, HAEntity2017)

# Merge HIMSS data sets into one
HIMSS <- HAEntity %>%
  left_join(HostedSoftware, by=c("HAEntityId","SurveyId", "year"), na.matches="never") 
# This created repeats because some hospitals have multiple EHR systems.

# Convert HIMSS from long to wide in Vendor name
HIMSS <- HIMSS %>%
  distinct() %>%
  mutate(VendorName=ifelse(VendorName %in% c("ALLSCRIPTS", "CERNER CORPORATION", "eClinicalWorks",
                                             "ECLIPSYS CORPORATION", "EPIC", "ATHENAHEALTH",
                                             "MCKESSON PROVIDER TECHNOLOGIES", "MEDITECH", "NEXTGEN HEALTHCARE",
                                             "QUADRAMED", "SAGE", "SAGE SOFTWARE INC.", "SIEMENS HEALTHCARE",
                                             "SELF-DEVELOPED"), VendorName, NA)) %>%
  filter(!is.na(VendorName) & !is.na(MedicareNumber)) %>%
  group_by(MedicareNumber, year) %>%
  mutate(num=row_number()) %>%
  ungroup() %>%
  select(year, MedicareNumber, VendorName, num) %>%
  filter(num<4)

HIMSS <- HIMSS %>% ungroup() %>%
  mutate(EHR_HIMSS1=ifelse(num==1, VendorName, NA)) %>%
  mutate(EHR_HIMSS2=ifelse(num==2, VendorName, NA)) %>%
  mutate(EHR_HIMSS3=ifelse(num==3, VendorName, NA)) %>%
  group_by(MedicareNumber, year) %>%
  fill(EHR_HIMSS1, .direction="downup") %>%
  fill(EHR_HIMSS2, .direction="downup") %>%
  fill(EHR_HIMSS3, .direction="downup") %>%
  select(-VendorName, -num) %>%
  distinct() %>%
  select(year, MedicareNumber, EHR_HIMSS1)

rm(HAEntity, HostedSoftware, year, observe)

# merge HIMSS to data set
data <- data %>%
  left_join(HIMSS, by=c("year","MCRNUM"="MedicareNumber"), na_matches="never")
  
# Find out how many hospital years have NA for EHR question in either data sets
num_missing_EHR <- data %>%
  filter(is.na(EHLTH) & is.na(EHR_HIMSS1)) %>%
  group_by(year) %>%
  distinct(HospNPI)
  # 7314 missing 

# Find out how many hospitals are missing an answer in every year
num_always_missing_EHR <- data %>% ungroup() %>%
  filter(is.na(EHLTH) & is.na(EHR_HIMSS1)) %>%
  mutate(EHLTH=ifelse(is.na(EHLTH),10,EHLTH)) %>%
  group_by(HospNPI) %>%
  mutate(always_missing=ifelse(sum(EHLTH)==60,1,0)) %>%
  filter(always_missing==1) %>%
  ungroup() %>%
  distinct(HospNPI, .keep_all = T)
  # There are only 82 hospitals that never answer this question, but they typically have answers to the other questions in the AHA survey

# Fill in missing year for EHR if it's between two years that have the same answer for EHR question
data <- data %>% group_by(AHAID) %>%
  mutate(firstyear_0=min(year[EHLTH==0],na.rm=T),lastyear_0=max(year[EHLTH==0],na.rm=T)) %>%
  mutate(firstyear_0=ifelse(is.infinite(firstyear_0),NA,firstyear_0),lastyear_0=ifelse(is.infinite(lastyear_0),NA,lastyear_0)) %>%
  mutate(EHLTH=ifelse(firstyear_0<year & year<lastyear_0 & is.na(EHLTH),0,EHLTH))


data <- data %>% group_by(AHAID) %>%
  mutate(firstyear_1=min(year[EHLTH==1],na.rm=T),lastyear_1=max(year[EHLTH==1],na.rm=T)) %>%
  mutate(firstyear_1=ifelse(is.infinite(firstyear_1),NA,firstyear_1),lastyear_1=ifelse(is.infinite(lastyear_1),NA,lastyear_1)) %>%
  mutate(EHLTH=ifelse(firstyear_1<year & year<lastyear_1 & is.na(EHLTH),1,EHLTH))


data <- data %>% group_by(AHAID) %>%
  mutate(firstyear_2=min(year[EHLTH==2],na.rm=T),lastyear_2=max(year[EHLTH==2],na.rm=T)) %>%
  mutate(firstyear_2=ifelse(is.infinite(firstyear_2),NA,firstyear_2),lastyear_2=ifelse(is.infinite(lastyear_2),NA,lastyear_2)) %>%
  mutate(EHLTH=ifelse(firstyear_2<year & year<lastyear_2 & is.na(EHLTH),2,EHLTH)) %>%
  ungroup()

# Get rid of unneeded variables 
data <- data %>% ungroup() %>%
  select(-firstyear_0, -firstyear_1, -firstyear_2, -lastyear_0, -lastyear_1, -lastyear_2)

# Get rid of physicians who only work on VA hospitals
VA <- data %>%
  mutate(VA=ifelse(str_detect(Hospital_name,"Veteran"),1,0)) %>%
  filter(VA==1) %>%
  distinct(pairID,VA)

data <- data %>%
  left_join(VA,by="pairID") %>%
  filter(is.na(VA)) %>%
  select(-VA)



# Create/Clean Variables (Reading in data is complete) --------------------------------------------

### Descriptive Variables to Drop Hospitals ------------------------------------------------------
# Descriptive Variable: Average size of hospital worked with
# Drop hospitals with really low bed count
low_beds <- data %>% ungroup() %>%
  distinct(HospNPI, year, beds) %>%
  filter(beds<10) %>%
  distinct(HospNPI) %>%
  mutate(low_beds=1)
  # 58 hospitals

data <- data %>%
  left_join(low_beds, by="HospNPI") %>%
  filter(is.na(low_beds)) %>%
  select(-low_beds)
  # 775k obs

data <- data %>%
  group_by(DocNPI,year) %>%
  mutate(avg_beds=mean(beds,na.rm=T)) %>%
  mutate(avg_beds=round(avg_beds,1)) %>%
  ungroup()

# Descriptive Variable: Years of Experience
data <- data %>% ungroup() %>%
  mutate(experience=year-grad_year)

### EHR VARIABLES --------------------------------------------------------------------------------
# Create indicator for general hospital EHR use
data <- data %>%
  mutate(EHR=ifelse(EHLTH==2 | !is.na(EHR_HIMSS1),1,ifelse(EHLTH==0 | EHLTH==1,0,NA))) %>%
  select(-EHLTH, -EHR_HIMSS1)

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
  ungroup() %>%
  mutate(EHR=ifelse(is.na(EHR),0,EHR))


# Create measure of number of hospitals a physician works with that use an EHR
# Create number of hospitals variables
data <- data %>%
  group_by(DocNPI,year) %>%
  mutate(count=1) %>%
  mutate(num_hospitals=sum(count)) %>%
  select(-count)

# Create number hospitals with general EHR
data <- data %>% ungroup() %>%
  group_by(DocNPI,year) %>%
  mutate(num_hosp_EHR=sum(EHR,na.rm=T)) %>%
  ungroup()

# Variables: fraction of hospitals with EHR
data <- data %>% ungroup() %>%
  mutate(frac_EHR=ifelse(num_hospitals>0,num_hosp_EHR/num_hospitals,NA))


# Treatment Variable: Indicator for Exposed to EHR in any hospital
# Create variable for first year a doc was exposed to EHR
minyr_EHR <- data %>% ungroup() %>%
  distinct(DocNPI, year, frac_EHR) %>%
  filter(frac_EHR>0) %>%
  group_by(DocNPI) %>%
  mutate(minyr_EHR=min(year)) %>%
  ungroup() %>%
  distinct(DocNPI,minyr_EHR)

data <- data %>% ungroup() %>%
  left_join(minyr_EHR, by="DocNPI") %>%
  mutate(minyr_EHR=ifelse(is.na(minyr_EHR),0,minyr_EHR))

# More Descriptive Variables --------------------------------------------------------------
# Descriptive Variable: Number of systems worked with
count_sys <- data %>%
  ungroup() %>%
  distinct(DocNPI,year,SYSID) %>%
  count(DocNPI,year,name="num_systems")

data <- data %>%
  left_join(count_sys,by=c("year","DocNPI"))



## OUTCOME VARIABLES ----------------------------------------------------------- # 

# Create physician level patient count with hospitals
data <- data %>%
  ungroup() %>%
  group_by(DocNPI,year) %>%
  mutate(hosp_patient_count=sum(samedaycount)) %>%
  ungroup()

# Create physician level patient count with EHR hospitals and merge back to data
EHR_hosp <- data %>%
  filter(EHR==1) %>%
  group_by(year, DocNPI) %>%
  mutate(hosp_patient_count_EHRhosp=sum(samedaycount)) %>%
  ungroup() %>%
  distinct(DocNPI,year,hosp_patient_count_EHRhosp)

data <- data %>%
  left_join(EHR_hosp, by=c("year", "DocNPI")) %>%
  mutate(hosp_patient_count_EHRhosp=ifelse(is.na(hosp_patient_count_EHRhosp),0,hosp_patient_count_EHRhosp))

# Create physician level patient count with non-EHR hospitals and merge back to data
noEHR_hosp <- data %>%
  filter(EHR==0) %>%
  group_by(year, DocNPI) %>%
  mutate(hosp_patient_count_noEHRhosp=sum(samedaycount)) %>%
  ungroup() %>%
  distinct(DocNPI,year,hosp_patient_count_noEHRhosp)

data <- data %>%
  left_join(noEHR_hosp, by=c("year", "DocNPI")) %>%
  mutate(hosp_patient_count_noEHRhosp=ifelse(is.na(hosp_patient_count_noEHRhosp),0,hosp_patient_count_noEHRhosp))


# Create variable: new NPI introduced
data <- data %>%
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


# Aggregate the data to the physician level -------------------------------------------------------------

Aggregated_Pairs <- data %>%
  distinct(year, DocNPI, grad_year, avg_beds, experience, 
           num_hospitals, 
           frac_EHR, minyr_EHR, 
           num_systems, hosp_patient_count, hosp_patient_count_EHRhosp, hosp_patient_count_noEHRhosp, 
           newnpi, never_newnpi)
  #526k obs

# Now complete the data
Aggregated_Pairs <- complete(Aggregated_Pairs,DocNPI,year=2009:2017)

Aggregated_Pairs <- Aggregated_Pairs %>%
  group_by(DocNPI) %>%
  fill(minyr_EHR,.direction="downup") %>%
  fill(grad_year,.direction="downup") %>%
  fill(avg_beds,.direction="downup") %>%
  fill(num_hospitals,.direction="downup") %>%
  fill(num_systems,.direction="downup") %>%
  ungroup() %>%
  mutate(experience=year-grad_year) %>%
  mutate(anyEHR_exposed=ifelse(minyr_EHR>0 & year>=minyr_EHR,1,0))



# Save the Data for Analysis -----------------------------------------------------------------
saveRDS(Aggregated_Pairs,file=paste0(created_data_path,"Aggregated_Pairs2.rds"))


# Balance Check
balance_check <- Aggregated_Pairs %>%
  select(year,DocNPI)

is.pbalanced(balance_check)

