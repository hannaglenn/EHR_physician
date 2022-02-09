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
  # 1.75 mill obs, 6 variables

data <- data %>%
  rename(pairID=ID) %>%
  select(-keep)

# Create Experience Variable ------------------------------------------------------------
# Read in Physician Compare
PhysCompare <- read_csv(paste0(raw_data_path,"DAC_NationalDownloadableFile.csv"))

PhysCompare <- PhysCompare %>%
  dplyr::rename(gender=gndr,grad_year=Grd_yr) %>%
  select(NPI,grad_year) 

# Merge, then drop any physicians that graduated after 2008
data <- data %>%
  left_join(PhysCompare, by=c("DocNPI"="NPI")) %>%
  distinct() %>%
  filter(grad_year<2009)
  # Now have 1.27 mill obs


#### Create EHR Variables ---------------------------------------------------------------------- ####
#Read in AHA-NPI crosswalk data
AHANPI_cw <- read_rds(paste0(created_data_path,"AHANPI_cw.rds"))

# Merge crosswalk to main data
data <- data %>%
  left_join(AHANPI_cw,by=c("HospNPI"="NPI"))

# See how many unique hospitals in the dataset have an AHAID
num_hosp <- data %>% ungroup() %>%
  filter(!is.na(AHAID)) %>%
  distinct(HospNPI)
  # There are 4225 unique hospitals in the data that are not missing AHAID (good)

# Only keep pairs with AHA hospitals since that is where EHR information comes from
data <- data %>%
  filter(!is.na(AHAID))
  #1 mill

balance_check <- data %>% ungroup() %>%
  distinct(year,pairID)

# Check if still balanced
is.pbalanced(balance_check)
  #TRUE

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
  select(-IPAP, -GPWP, -OPHP, -CPHP, -ISMP, -EQMP, -FNDP, -full_year, -EHLTHI, -EHLTHS)

data <- data %>% ungroup() %>%
  mutate(AHAID=as.character(AHAID)) %>%
  left_join(AHAmainsurvey,by=c("AHAID"="ID","year"),na_matches="never")


  
# Find out how many hospital years have NA for EHR question (conditional on having an AHA ID)
num_missing_EHR <- data %>%
  filter(is.na(EHLTH)) %>%
  group_by(year) %>%
  distinct(HospNPI)
  # 8904 missing 

# Find out how many hospitals are missing an answer in every year
num_always_missing_EHR <- data %>% ungroup() %>%
  filter(is.na(EHLTH)) %>%
  mutate(EHLTH=ifelse(is.na(EHLTH),10,EHLTH)) %>%
  group_by(HospNPI) %>%
  mutate(always_missing=ifelse(sum(EHLTH)==60,1,0)) %>%
  filter(always_missing==1) %>%
  ungroup() %>%
  distinct(HospNPI, .keep_all = T)
  # There are only 91 hospitals that never answer this question, but they typically have answers to the other questions in the survey

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
  # Now down to 7739 missing 

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
  select(ID,YEAR,MCNTRL,documentation_index,decision_index)

# Merge
data <- data %>%
  mutate(AHAID=as.character(AHAID)) %>%
  left_join(AHAIT,by=c("AHAID"="ID","year"="YEAR"), na_matches="never") %>%
  select(-Hospital_name, -decision_index,-documentation_index)


# Create/Clean Variables (Reading in data is complete) --------------------------------------------

### Descriptive Variables to Drop Hospitals ------------------------------------------------------
# Descriptive Variable: Average size of hospital worked with
# Drop hospitals with really low bed count
low_beds <- data %>% ungroup() %>%
  distinct(HospNPI, year, beds) %>%
  filter(beds<10) %>%
  distinct(HospNPI) %>%
  mutate(low_beds=1)
  # 60 hospitals

data <- data %>%
  left_join(low_beds, by="HospNPI") %>%
  filter(is.na(low_beds)) %>%
  select(-low_beds)
  # 1.08 mill obs

data <- data %>%
  group_by(DocNPI,year) %>%
  mutate(avg_beds=mean(beds,na.rm=T))

# Descriptive Variable: Years of Experience
data <- data %>% ungroup() %>%
  mutate(experience=year-grad_year)

# Summarize the integration variables into physician level
data <- data %>%
  group_by(DocNPI,year) %>%
  mutate(sum_low=sum(low_integration,na.rm=T),
         sum_high=sum(high_integration,na.rm=T)) %>%
  ungroup() %>%
  mutate(phys_lowintegration=ifelse(sum_low>0,1,0),
         phys_highintegration=ifelse(sum_high>0,1,0)) %>%
  select(-sum_low,-sum_high)

### EHR VARIABLES --------------------------------------------------------------------------------
# Create indicator for hospital EHR use
data <- data %>%
  mutate(EHR=ifelse(EHLTH==2,1,ifelse(EHLTH==0 | EHLTH==1,0,NA)))

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
  select(-change2009, -change2015, -EHLTH) %>%
  ungroup()


# Create measure of number of hospitals a physician works with that use an EHR


# Number of Hospital Variables -----------------------------------------------

# Create two different number of hospitals variables. (1) is the total number of hospitals ever worked with in the data. This is 
# a time invariant physician level variable. (2) is the number of hospitals with positive share count in that year. This is a time varying 
# physician level variable. 
data <- data %>%
  mutate(pos_patients=ifelse(samedaycount>0,1,0),
         count=1) %>%
  group_by(DocNPI,year) %>%
  mutate(num_hospitals_pos=sum(pos_patients),
         num_hospitals_constant=sum(count)) %>%
  select(-count)


# Create number hospitals with EHR (same thing, two different variables depending on positive patient counts)
data <- data %>% ungroup() %>%
  group_by(DocNPI,year) %>%
  mutate(num_hosp_EHR_pos=sum(EHR[pos_patients==1],na.rm=T),
         num_hosp_EHR_constant=sum(EHR,na.rm=T))


# Variables: fraction of hospitals with EHR
data <- data %>% ungroup() %>%
  mutate(frac_EHR_pos=ifelse(num_hospitals_pos>0,num_hosp_EHR_pos/num_hospitals_pos,NA),
         frac_EHR_constant=ifelse(num_hospitals_constant>0,num_hosp_EHR_constant/num_hospitals_constant,NA))



# Treatment Variable: Indicator for Exposed to EHR in any hospital
# Create variable for first year a doc was exposed to EHR
minyr_EHR <- data %>% ungroup() %>%
  distinct(DocNPI, year, frac_EHR_pos) %>%
  filter(frac_EHR_pos>0) %>%
  group_by(DocNPI) %>%
  mutate(minyr_EHR=min(year)) %>%
  ungroup() %>%
  distinct(DocNPI,minyr_EHR)

data <- data %>% ungroup() %>%
  left_join(minyr_EHR, by="DocNPI") %>%
  mutate(minyr_EHR=ifelse(is.na(minyr_EHR),0,minyr_EHR))

# Create "exposed" indicator based on year and min year of EHR
data <- data %>%
  mutate(anyEHR_exposed=ifelse(minyr_EHR>0 & year>=minyr_EHR,1,0))




# More Descriptive Variables --------------------------------------------------------------
# Descriptive Variable: Number of systems worked with
count_sys <- data %>%
  ungroup() %>%
  distinct(DocNPI,year,SYSID) %>%
  count(DocNPI,year,name="num_systems")

data <- data %>%
  left_join(count_sys,by=c("year","DocNPI"))




# Create physician level patient count with hospitals
data <- data %>%
  ungroup() %>%
  group_by(DocNPI,year) %>%
  mutate(hosp_patient_count=sum(samedaycount)) %>%
  ungroup()



# Aggregate the data to the physician level -------------------------------------------------------------

Aggregated_Pairs <- data %>%
  distinct(DocNPI,year,phys_lowintegration,phys_highintegration,grad_year, hosp_patient_count, num_hospitals_constant, num_hospitals_pos, num_hosp_EHR_pos, num_hosp_EHR_constant,
           frac_EHR_pos, frac_EHR_constant, avg_beds, experience, minyr_EHR, anyEHR_exposed,
           num_systems)
  #660k obs


# Create an indicator for exposed ever
Aggregated_Pairs <- Aggregated_Pairs %>%
  group_by(DocNPI) %>%
  mutate(n=sum(anyEHR_exposed)) %>%
  ungroup() %>%
  mutate(exposed_ever=ifelse(n>0,1,0)) %>%
  select(-n)


# Save the Data for Analysis -----------------------------------------------------------------
saveRDS(Aggregated_Pairs,file=paste0(created_data_path,"Aggregated_Pairs.rds"))
write.csv(Aggregated_Pairs,file=paste0(created_data_path,"Aggregated_Pairs.csv"))


# Balance Check
balance_check <- Aggregated_Pairs %>%
  select(year,DocNPI)

is.pbalanced(balance_check)

