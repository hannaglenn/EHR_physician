library(tidyverse)
library(readr)
library(plm)
library(kableExtra)


# --------------------    Build Variables Onto Main Hospital-Physician Pair Data --------------------------
#                         Hanna Glenn, Emory University
#                         9/3/2021

# ORDER::::: 4

# This script develops the main dataset to be used in the paper.
# This builds on the physician-hospital pairs created in "data2_pairs.R"
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
  dplyr::rename(gender=gndr,grad_year=Grd_yr) %>%
  select(NPI,grad_year) 

# Merge, then drop any physicians that graduated after 2005
data <- data %>%
  left_join(PhysCompare, by=c("DocNPI"="NPI")) %>%
  distinct() %>%
  filter(grad_year<2005)
  # Now have 900k obs


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
  # There are 4253 unique hospitals in the data that are not missing AHAID (good)

# Only keep pairs with AHA hospitals since that is where EHR information comes from
data <- data %>%
  filter(!is.na(AHAID))
  #780k obs

# Read in main AHA survey data
# I need to think about adding more hospital characteristics to this dataset
AHAmainsurvey <- read_csv(paste0(raw_data_path,"AHA_mainsurvey.csv"))

# I define low integration and high integration as in Madison (2004, HSR)
AHAmainsurvey <- AHAmainsurvey %>%
  mutate(ID=as.character(ID)) %>%
  mutate(low_integration=ifelse(IPAHOS==1 | OPHOHOS==1 | CPHOHOS==1,1,0),
         high_integration=ifelse(GPWWHOS==1 | MSOHOS==1 | ISMHOS==1 | EQMODHOS==1,1,0)) %>%
  rename(Hospital_name=MNAME, year=YEAR, full_year=FYR, 
         joint_phys=JNTPH, total_physicians=FTMT, phys_owned=PHYGP, beds=BDTOT ) %>%
  select(-IPAHOS, -OPHOHOS, -CPHOHOS, -GPWWHOS, -MSOHOS, -ISMHOS, -EQMODHOS) %>%
  select(-IPAP, -GPWP, -OPHP, -CPHP, -ISMP, -EQMP, -FNDP, -full_year, -EHLTHI, -EHLTHS,
         -total_physicians, -joint_phys, -phys_owned)

data <- data %>% ungroup() %>%
  mutate(AHAID=as.character(AHAID)) %>%
  left_join(AHAmainsurvey,by=c("AHAID"="ID","year"),na_matches="never")


  
# Find out how many hospital years have NA for EHR question (conditional on having an AHA ID)
num_missing_EHR <- data %>%
  filter(is.na(EHLTH)) %>%
  group_by(year) %>%
  distinct(HospNPI)
  # 7911 missing 

# Find out how many hospitals are missing an answer in every year
num_always_missing_EHR <- data %>% ungroup() %>%
  filter(is.na(EHLTH)) %>%
  mutate(EHLTH=ifelse(is.na(EHLTH),10,EHLTH)) %>%
  group_by(HospNPI) %>%
  mutate(always_missing=ifelse(sum(EHLTH)==60,1,0)) %>%
  filter(always_missing==1) %>%
  ungroup() %>%
  distinct(HospNPI, .keep_all = T)
  # There are only 89 hospitals that never answer this question, but they typically have answers to the other questions in the survey

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
  # Now down to 4253 missing 

# Get rid of unneeded variables 
data <- data %>% ungroup() %>%
  select(-firstyear_0, -firstyear_1, -firstyear_2, -lastyear_0, -lastyear_1, -lastyear_2)

# Read in AHA IT Survey --------------------------------------------------------------------------------------
# Note: before 2012, the year listed in the data represents the year before survey answers were given
for (i in 2008:2010){
  AHAITyear <- read_csv(paste0(raw_data_path,"AHAIT Survey 2008-2015/AHAIT",i,".csv"))
  AHAITyear <- AHAITyear %>%
    mutate(YEAR=i+1) %>%
    select(ID, YEAR, MCNTRL, CSEDPD, CSEDNA, CSEDPL, CSEDML, CSEDDS, CSEDAD, CSDSCG, CSDSCR, CSDSDA, CSDSDD, CSDSDL, CSDSDS) 
  assign(paste0("AHAIT",i+1),AHAITyear)
}
for (i in 2012:2015){
  AHAITyear <- read_csv(paste0(raw_data_path,"AHAIT Survey 2008-2015/AHAIT",i,".csv"))
  AHAITyear <- AHAITyear %>%
    select(ID, YEAR, MCNTRL, CSEDPD, CSEDNA, CSEDPL, CSEDML, CSEDDS, CSEDAD, CSDSCG, CSDSCR, CSDSDA, CSDSDD, CSDSDL, CSDSDS) 
  assign(paste0("AHAIT",i),AHAITyear)
}

AHAIT <- rbind(AHAIT2009, AHAIT2010, AHAIT2011, AHAIT2012, AHAIT2013, AHAIT2014, AHAIT2015)

# Create index out of supplement survey answers
AHAIT <- AHAIT %>%
  mutate(CSEDPD=as.numeric(CSEDPD), CSEDNA=as.numeric(CSEDNA), CSEDPL=as.numeric(CSEDPL), CSEDML=as.numeric(CSEDML), CSEDDS=as.numeric(CSEDDS), CSEDAD=as.numeric(CSEDAD), CSDSCG=as.numeric(CSDSCG), CSDSCR=as.numeric(CSDSCR), CSDSDA=as.numeric(CSDSDA), CSDSDD=as.numeric(CSDSDD), CSDSDL=as.numeric(CSDSDL), CSDSDS=as.numeric(CSDSDS)) %>%
  rowwise() %>%
  mutate(documentation_index=sum(CSEDPD,CSEDNA, CSEDPL, CSEDML, CSEDDS, CSEDAD, na.rm=TRUE), decision_index=sum(CSDSCG, CSDSCR, CSDSDA, CSDSDD, CSDSDL, CSDSDS, na.rm = TRUE)) %>%
  mutate(documentation_index=ifelse(documentation_index==0,NA,documentation_index),decision_index=ifelse(decision_index==0,NA,decision_index)) %>%
  select(ID,YEAR,MCNTRL,documentation_index,decision_index) %>%
  mutate(EHR_doc=ifelse(documentation_index>16,1,0),
         EHR_dec=ifelse(decision_index>16,1,0))

# Merge
data <- data %>%
  mutate(AHAID=as.character(AHAID)) %>%
  left_join(AHAIT,by=c("AHAID"="ID","year"="YEAR"), na_matches="never") %>%
  select(-Hospital_name, -decision_index,-documentation_index)

# If the hospital doesn't have an EHR then make the indicators for decision/documentation 0
data <- data %>%
  dplyr::mutate(EHR_dec=ifelse(EHLTH==0 & is.na(EHR_dec),0,EHR_dec),
         EHR_doc=ifelse(EHLTH==0 & is.na(EHR_doc),0,EHR_doc))

## Read in Hospitals with Data Assistants
Hosp_with_DA <- readRDS(paste0(created_data_path,"/Hosp_with_DA.rds"))

Hosp_with_DA <- Hosp_with_DA %>%
  mutate(DA=1) %>%
  mutate(HospNPI=as.numeric(HospNPI))

data <- data %>%
  left_join(Hosp_with_DA, by=c("year","HospNPI"))

data <- data %>%
  mutate(DA=ifelse(is.na(DA),0,DA))


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
  # 779k obs

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

# Do the same thing for EHR_dec
fill_in <- data %>%
  filter((year==2010 & EHR_dec==0) | (year==2014 & EHR_dec==1)) %>%
  mutate(change2009=ifelse(year==2010,1,0),
         change2015=ifelse(year==2014,1,0)) %>%
  select(HospNPI,DocNPI,year,EHR_dec,change2009,change2015) %>%
  distinct(HospNPI,year,EHR_dec,change2009,change2015)

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
  mutate(EHR_dec=ifelse(year==2009 & change2009==1 & is.na(EHR_dec),0,EHR_dec)) %>%
  mutate(EHR_dec=ifelse(year==2015 & change2015==1 & is.na(EHR_dec),1,EHR_dec)) %>%
  select(-change2009, -change2015) %>%
  ungroup()

# Do the same thing for EHR_doc
fill_in <- data %>%
  filter((year==2010 & EHR_doc==0) | (year==2014 & EHR_doc==1)) %>%
  mutate(change2009=ifelse(year==2010,1,0),
         change2015=ifelse(year==2014,1,0)) %>%
  select(HospNPI,DocNPI,year,EHR_doc,change2009,change2015) %>%
  distinct(HospNPI,year,EHR_doc,change2009,change2015)

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
  mutate(EHR_doc=ifelse(year==2009 & change2009==1 & is.na(EHR_doc),0,EHR_doc)) %>%
  mutate(EHR_doc=ifelse(year==2015 & change2015==1 & is.na(EHR_doc),1,EHR_doc)) %>%
  select(-change2009, -change2015) %>%
  ungroup()



# Create measure of number of hospitals a physician works with that use an EHR
# Create number of hospitals variables
data <- data %>%
  group_by(DocNPI,year) %>%
  mutate(count=1) %>%
  mutate(num_hospitals=sum(count)) %>%
  select(-count)


# Create number hospitals with general EHR, dec EHR, doc EHR
data <- data %>% ungroup() %>%
  group_by(DocNPI,year) %>%
  mutate(num_hosp_EHR=sum(EHR,na.rm=T),
         num_hosp_EHR_dec=sum(EHR_dec,na.rm=T),
         num_hosp_EHR_doc=sum(EHR_doc,na.rm=T)) %>%
  ungroup()


# Variables: fraction of hospitals with EHR (three types)
data <- data %>% ungroup() %>%
  mutate(frac_EHR=ifelse(num_hospitals>0,num_hosp_EHR/num_hospitals,NA),
         frac_EHR_dec=ifelse(num_hospitals>0,num_hosp_EHR_dec/num_hospitals,NA),
         frac_EHR_doc=ifelse(num_hospitals>0,num_hosp_EHR_doc/num_hospitals,NA))


# Treatment Variable: Indicator for Exposed to EHR in any hospital (general EHR and decision making EHR)
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


# Create variable for first year a doc was exposed to decision making EHR
minyr_EHR_dec <- data %>% ungroup() %>%
  distinct(DocNPI, year, frac_EHR_dec) %>%
  filter(frac_EHR_dec>0) %>%
  group_by(DocNPI) %>%
  mutate(minyr_EHR_dec=min(year)) %>%
  ungroup() %>%
  distinct(DocNPI,minyr_EHR_dec)

data <- data %>% ungroup() %>%
  left_join(minyr_EHR_dec, by="DocNPI") %>%
  mutate(minyr_EHR_dec=ifelse(is.na(minyr_EHR_dec),0,minyr_EHR_dec))


# Create the same variables but only count exposure in non-high integration hospitals
# Create variable for first year a doc was exposed to EHR IN NON-HIGH INTEGRATION
minyr_EHR_int <- data %>% ungroup() %>%
  distinct(DocNPI, year, frac_EHR,high_integration) %>%
  filter(frac_EHR>0 & high_integration==0) %>%
  group_by(DocNPI) %>%
  mutate(minyr_EHR_int=min(year)) %>%
  ungroup() %>%
  distinct(DocNPI,minyr_EHR_int)

data <- data %>% ungroup() %>%
  left_join(minyr_EHR_int, by="DocNPI") %>%
  mutate(minyr_EHR_int=ifelse(is.na(minyr_EHR_int),0,minyr_EHR_int))

# Create variable for first year a doc was exposed to decision making EHR IN NONHIGH INT
minyr_EHR_dec_int <- data %>% ungroup() %>%
  distinct(DocNPI, year, frac_EHR_dec, high_integration) %>%
  filter(frac_EHR_dec>0 & high_integration==0) %>%
  group_by(DocNPI) %>%
  mutate(minyr_EHR_dec_int=min(year)) %>%
  ungroup() %>%
  distinct(DocNPI,minyr_EHR_dec_int)

data <- data %>% ungroup() %>%
  left_join(minyr_EHR_dec_int, by="DocNPI") %>%
  mutate(minyr_EHR_dec_int=ifelse(is.na(minyr_EHR_dec_int),0,minyr_EHR_dec_int))


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

# Create variable for "works with hospital with DA"
data <- data %>%
  group_by(DocNPI,year) %>%
  mutate(sum=sum(DA)) %>%
  mutate(works_with_DA=ifelse(sum>0,1,0)) %>%
  ungroup()

# Aggregate the data to the physician level -------------------------------------------------------------

Aggregated_Pairs <- data %>%
  distinct(year, DocNPI,grad_year, avg_beds, experience, 
           num_hospitals, works_with_DA,
           frac_EHR, frac_EHR_dec, frac_EHR_doc, minyr_EHR, minyr_EHR_dec, minyr_EHR_int, minyr_EHR_dec_int,
           num_systems, hosp_patient_count, hosp_patient_count_EHRhosp, hosp_patient_count_noEHRhosp, 
           newnpi, never_newnpi)
  #528k obs

# Now complete the data
Aggregated_Pairs <- complete(Aggregated_Pairs,DocNPI,year=2009:2017)

Aggregated_Pairs <- Aggregated_Pairs %>%
  group_by(DocNPI) %>%
  fill(minyr_EHR,.direction="downup") %>%
  fill(minyr_EHR_dec,.direction="downup") %>%
  fill(minyr_EHR_int,.direction="downup") %>%
  fill(minyr_EHR_dec_int,.direction="downup") %>%
  fill(grad_year,.direction="downup") %>%
  fill(avg_beds,.direction="downup") %>%
  fill(num_hospitals,.direction="downup") %>%
  fill(num_systems,.direction="downup") %>%
  ungroup() %>%
  mutate(experience=year-grad_year) 

# Create exposure variables
Aggregated_Pairs <- Aggregated_Pairs %>%
  mutate(anyEHR_exposed=ifelse(minyr_EHR>0 & year>=minyr_EHR,1,0),
         decEHR_exposed=ifelse(minyr_EHR_dec>0 & year>=minyr_EHR_dec,1,0),
         anyEHR_LI_exposed=ifelse(minyr_EHR_int>0 & year>=minyr_EHR_int,1,0),
         decEHR_LI_exposed=ifelse(minyr_EHR_dec_int>0 & year>=minyr_EHR_dec_int,1,0))



# Save the Data for Analysis -----------------------------------------------------------------
saveRDS(Aggregated_Pairs,file=paste0(created_data_path,"Aggregated_Pairs.rds"))
write.csv(Aggregated_Pairs,file=paste0(created_data_path,"Aggregated_Pairs.csv"))


# Balance Check
balance_check <- Aggregated_Pairs %>%
  select(year,DocNPI)

is.pbalanced(balance_check)

