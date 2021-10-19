library(tidyverse)
library(readr)
library(plm)

# --------------------    Build Variables Onto Main Hospital-Physician Pair Data --------------------------
#                         Hanna Glenn, Emory University
#                         9/3/2021

# This script develops the main dataset to be used in my third year paper.
# First, I bring in Physician Compare for information about medical school graduation and gender. Next, I bring in 
# MedPhys_PUF, which has information on total Part B claims for a physician from 2012-2015. 
# For EHR information, I use the main and supplement AHA surveys and CMS data on hospitals which received meaningful use subsidies.
# I limit 
# The final dataset is called "Final_Pairs.rds"

# Read in physician hospital pairs created in "phys_hosp_pairs.r"
Final_Pairs <- read_rds(paste0(created_data_path, "phys_hosp_pairs.rds"))
  # This data does not have any repeats of year, HospNPI, DocNPI


# Create Experience and Gender Variables ------------------------------------------------------------
# Read in Physician Compare
PhysCompare <- read_csv(paste0(raw_data_path,"DAC_NationalDownloadableFile.csv"))

PhysCompare <- PhysCompare %>%
  rename(gender=gndr,grad_year=Grd_yr) %>%
  select(NPI,gender,grad_year) %>%
  mutate(female=ifelse(gender=='F',1,ifelse(gender=='M',0,NA)))

# Merge, then drop any physicians that graduated after 2008
Final_Pairs <- Final_Pairs %>%
  left_join(PhysCompare, by=c("DocNPI"="NPI"))

Final_Pairs <- Final_Pairs %>%
  distinct()

Final_Pairs <- Final_Pairs %>%
  filter(grad_year<2009)

#### Create total_services Variable --------------------------------------------------------- ####
# Read in MedPhys_PUF.R (created from Physician Compare in "MedPhys_PUF.R)
#MedPhys_PUF <- read_rds(paste0(created_data_path,"MedPhys_PUF.rds"))

#Final_Pairs <- Final_Pairs %>%
  #left_join(MedPhys_PUF,by=c("DocNPI"="NPI","year"))






# Create EHR Variables ---------------------------------------------------------------------- ####
#Read in AHA-NPI crosswalk
AHANPI_cw <- read_rds(paste0(created_data_path,"AHANPI_cw.rds"))

# Merge crosswalk to main data
Final_Pairs <- Final_Pairs %>%
  left_join(AHANPI_cw,by=c("HospNPI"="NPI"))

# See how many unique hospitals in the dataset have an AHAID
num_hosp <- Final_Pairs %>% ungroup() %>%
  filter(!is.na(AHAID)) %>%
  distinct(HospNPI)
  # There are 4483 unique hospitals in the data that are not missing AHAID

# I'll only keep pairs with AHA hospitals since that is where EHR information comes from
Final_Pairs <- Final_Pairs %>%
  filter(!is.na(AHAID))

balance_check <- Final_Pairs %>% ungroup() %>%
  distinct(year,ID)

# Check if still balanced
is.pbalanced(balance_check)

# Read in main AHA survey data
# I need to think about adding more hospital characteristics to this dataset
AHAmainsurvey <- read_csv(paste0(raw_data_path,"AHA_mainsurvey.csv"))

AHAmainsurvey <- AHAmainsurvey %>%
  mutate(ID=as.character(ID))

Final_Pairs <- Final_Pairs %>% ungroup() %>%
  mutate(AHAID=as.character(AHAID)) %>%
  left_join(AHAmainsurvey,by=c("AHAID"="ID","year"="YEAR"),na_matches="never") %>%
  rename(SystemID="SYSID","days_hosp_operating"=DCOV, beds=BDTOT) %>%
  select(-REG)

  
# Find out how many hospital years have NA for this question (conditional on having an AHA ID)
num_missing_EHR <- Final_Pairs %>%
  filter(is.na(EHLTH)) %>%
  group_by(year) %>%
  distinct(HospNPI)
  # 9811 missing out of 26,898 total hospital years

# Find out how many hospitals are missing an answer in each year
num_always_missing_EHR <- Final_Pairs %>% ungroup() %>%
  filter(is.na(EHLTH)) %>%
  mutate(EHLTH=ifelse(is.na(EHLTH),10,EHLTH)) %>%
  group_by(HospNPI) %>%
  mutate(always_missing=ifelse(sum(EHLTH)==60,1,0)) %>%
  filter(always_missing==1) %>%
  ungroup() %>%
  distinct(HospNPI, .keep_all = T)
  # There are only 40 hospitals that never answer this question, but they typically have answers to the other questions in the survey

# Fill in missing year for EHR if it's between two years that have the same answer for EHR question
Final_Pairs <- Final_Pairs %>% group_by(AHAID) %>%
  mutate(firstyear_0=min(year[EHLTH==0],na.rm=T),lastyear_0=max(year[EHLTH==0],na.rm=T)) %>%
  mutate(firstyear_0=ifelse(is.infinite(firstyear_0),NA,firstyear_0),lastyear_0=ifelse(is.infinite(lastyear_0),NA,lastyear_0)) %>%
  mutate(EHLTH=ifelse(firstyear_0<year & year<lastyear_0 & is.na(EHLTH),0,EHLTH))
  # Fill in 542

Final_Pairs <- Final_Pairs %>% group_by(AHAID) %>%
  mutate(firstyear_1=min(year[EHLTH==1],na.rm=T),lastyear_1=max(year[EHLTH==1],na.rm=T)) %>%
  mutate(firstyear_1=ifelse(is.infinite(firstyear_1),NA,firstyear_1),lastyear_1=ifelse(is.infinite(lastyear_1),NA,lastyear_1)) %>%
  mutate(EHLTH=ifelse(firstyear_1<year & year<lastyear_1 & is.na(EHLTH),1,EHLTH))
  # Fill in 379

Final_Pairs <- Final_Pairs %>% group_by(AHAID) %>%
  mutate(firstyear_2=min(year[EHLTH==2],na.rm=T),lastyear_2=max(year[EHLTH==2],na.rm=T)) %>%
  mutate(firstyear_2=ifelse(is.infinite(firstyear_2),NA,firstyear_2),lastyear_2=ifelse(is.infinite(lastyear_2),NA,lastyear_2)) %>%
  mutate(EHLTH=ifelse(firstyear_2<year & year<lastyear_2 & is.na(EHLTH),2,EHLTH)) %>%
  ungroup()
  # Fill in 346

# Get rid of unneeded variables 
Final_Pairs <- Final_Pairs %>% ungroup() %>%
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

# Create index out of survey answers
AHAIT <- AHAIT %>%
  mutate(CSEDPD=as.numeric(CSEDPD), CSEDNA=as.numeric(CSEDNA), CSEDPL=as.numeric(CSEDPL), CSEDML=as.numeric(CSEDML), CSEDDS=as.numeric(CSEDDS), CSEDAD=as.numeric(CSEDAD), CSDSCG=as.numeric(CSDSCG), CSDSCR=as.numeric(CSDSCR), CSDSDA=as.numeric(CSDSDA), CSDSDD=as.numeric(CSDSDD), CSDSDL=as.numeric(CSDSDL), CSDSDS=as.numeric(CSDSDS)) %>%
  rowwise() %>%
  mutate(documentation_index=sum(CSEDPD,CSEDNA, CSEDPL, CSEDML, CSEDDS, CSEDAD, na.rm=TRUE), decision_index=sum(CSDSCG, CSDSCR, CSDSDA, CSDSDD, CSDSDL, CSDSDS, na.rm = TRUE)) %>%
  mutate(documentation_index=ifelse(documentation_index==0,NA,documentation_index),decision_index=ifelse(decision_index==0,NA,decision_index)) %>%
  select(ID,YEAR,MCNTRL,documentation_index,decision_index)

# Merge
Final_Pairs <- Final_Pairs %>%
  mutate(AHAID=as.character(AHAID))

Final_Pairs <- Final_Pairs %>%
  left_join(AHAIT,by=c("AHAID"="ID","year"="YEAR"), na_matches="never")


#### Meaningful Use Data ####


    # Read in Meaningful Use Data 
    #mean_use <- read_csv(paste0(raw_data_path,"HOSP_ProvidersPaidByEHR_06_2018.csv"))

    # Clean up and create indicators for each stage
    #mean_use <- mean_use %>%
        #rename(NPI="PROVIDER NPI", EHRstage="STAGE NUMBER", year="PROGRAM YEAR") %>%
        #select(NPI,EHRstage,year) %>%
        #filter(!is.na(EHRstage)) %>%
        #mutate(NPI=as.numeric(NPI)) %>%
        #mutate(stage1=if_else(str_detect(EHRstage,"Stage 1"),1,0), stage2=if_else(str_detect(EHRstage,"Stage 2"),1,0),
              #stage3=if_else(str_detect(EHRstage,"Stage 3"),1,0))

    #  How many distinct hospitals are in the data?
    #distinct <- mean_use %>%
    #distinct(NPI)
  # 4555 unique NPIs

    # Drop multiples since appearing twice certainly implies appearing once (for same stage)
        #mean_use <- mean_use %>%
        #group_by(year) %>%
        #distinct(NPI,.keep_all=T) %>%
        #ungroup()
  #14509 obs

      # Merge
      #Final_Pairs <- Final_Pairs %>%
       # left_join(mean_use, by=c("HospNPI"="NPI","year"))

      #Final_Pairs <- Final_Pairs %>%
        #mutate(getsubsidy=ifelse(is.na(EHRstage),0,1), stage1=ifelse(is.na(stage1),0,stage1), 
              #stage2=ifelse(is.na(stage2),0,stage2), stage3=ifelse(is.na(stage3),0,stage3))


# Bring in Working variable from other shared patient entities--------------------------------------------
phys_working <- read_rds(paste0(created_data_path,"phys_working.rds"))

# merge by year & DocNPI
Final_Pairs <- Final_Pairs %>%
  mutate(DocNPI=as.character(DocNPI)) %>%
  left_join(phys_working, by=c("year","DocNPI"))


# Bring in SK&A -----------------------------------------------------------------------------------------

  

# Create Extra Variables (Reading in data is complete) --------------------------------------------

# Create indicator for hospital EHR use
Final_Pairs <- Final_Pairs %>%
  mutate(EHR=ifelse(EHLTH==2,1,ifelse(EHLTH==0 | EHLTH==1,0,NA)))

# In some cases, 2009 or 2015 can be filled in to not have a missing value. 
# Create dataset to fill in 2009 or 2015 conditionally
fill_in <- Final_Pairs %>%
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
Final_Pairs <- Final_Pairs %>%
  left_join(fill_in,by="HospNPI")

# Fill in the qualifying missing values
Final_Pairs <- Final_Pairs %>%
  mutate(EHR=ifelse(year==2009 & change2009==1 & is.na(EHR),0,EHR)) %>%
  mutate(EHR=ifelse(year==2015 & change2015==1 & is.na(EHR),1,EHR)) %>%
  select(-change2009, -change2015) %>%
  ungroup()

# Create measure of number of hospitals a physician works with that use an EHR

# Figure out if any hospitals only have NAs for all years
always_missing_EHR <- Final_Pairs %>% ungroup() %>%
  distinct(HospNPI,year,EHR) %>% ungroup() %>%
  group_by(HospNPI) %>%
  mutate(EHR=ifelse(is.na(EHR),-1,EHR)) %>%
  mutate(sum=sum(EHR,na.rm=T)) %>%
  filter(sum==-7) %>%
  distinct(HospNPI,sum)
  # 370 hospitals never answer the question about EHRs. I will drop these.
  # This leaves 4113 unique hospitals in the data

Final_Pairs <- Final_Pairs %>% ungroup() %>%
  left_join(always_missing_EHR, by="HospNPI") %>%
  filter(is.na(sum)) %>%
  select(-sum)

# Create total number of hospitals a physician works with
count <- Final_Pairs %>%
  ungroup() %>%
  count(year,DocNPI,name="num_hospitals")

Final_Pairs <- Final_Pairs %>%
  left_join(count,by=c("year","DocNPI"))

# Create number hospitals with EHR
Final_Pairs <- Final_Pairs %>% ungroup() %>%
  group_by(DocNPI,year) %>%
  mutate(hosp_EHR=sum(EHR,na.rm=T))


# Variable: fraction of hospitals with EHR
Final_Pairs <- Final_Pairs %>%
  mutate(frac_EHR=hosp_EHR/num_hospitals)

# Descriptive Variable: Average size of hospital worked with
# Drop hospitals with really low bed count
low_beds <- Final_Pairs %>% ungroup() %>%
  distinct(HospNPI, year, beds) %>%
  filter(beds<7) %>%
  distinct(HospNPI) %>%
  mutate(low_beds=1)

Final_Pairs <- Final_Pairs %>%
  left_join(low_beds, by="HospNPI") %>%
  filter(is.na(low_beds)) %>%
  select(-low_beds)

Final_Pairs <- Final_Pairs %>%
  group_by(DocNPI,year) %>%
  mutate(avg_beds=mean(beds,na.rm=T))

# Descriptive Variable: Average Operating Days of Hospitals Worked With
# Drop hospitals that ever have 0 days operating
# (When looking at the data ofthese hospitals, they are missing most other characteristics)
no_operating <- Final_Pairs %>% ungroup() %>%
  filter(days_hosp_operating==0) %>%
  distinct(HospNPI) %>%
  mutate(drop=1)

Final_Pairs <- Final_Pairs %>% ungroup() %>%
  left_join(no_operating, by="HospNPI") %>%
  filter(is.na(drop)) %>%
  select(-drop)

Final_Pairs <- Final_Pairs %>%
  ungroup() %>%
  group_by(DocNPI, year) %>%
  mutate(avg_oper_days=mean(days_hosp_operating,na.rm=T))

# Descriptive Variable: Years of Experience
Final_Pairs <- Final_Pairs %>% ungroup() %>%
  mutate(experience=year-grad_year)


# Treatment Variable: Indicator for Exposed to EHR
# Create variable for first year a doc was exposed to EHR
minyr_EHR <- Final_Pairs %>% ungroup() %>%
  distinct(DocNPI, year, frac_EHR) %>%
  filter(frac_EHR>0) %>%
  group_by(DocNPI) %>%
  mutate(minyr_EHR=min(year)) %>%
  ungroup() %>%
  distinct(DocNPI,minyr_EHR)

Final_Pairs <- Final_Pairs %>% ungroup() %>%
  left_join(minyr_EHR, by="DocNPI") %>%
  mutate(minyr_EHR=ifelse(is.na(minyr_EHR),0,minyr_EHR))

# Create "exposed" indicator based on year and min year of EHR
Final_Pairs <- Final_Pairs %>%
  mutate(exposed=ifelse(year>=minyr_EHR,1,0))

# Potential Treatment Variables: Documentation and Decision Making Indicators
Final_Pairs <- Final_Pairs %>%
  mutate(docEHR=ifelse(EHR==0,0,ifelse(EHR==1 & (documentation_index>24 | is.na(documentation_index)),0,1)),
         decEHR=ifelse(EHR==0,0,ifelse(EHR==1 & (decision_index>21 | is.na(decision_index)),0,1)))

# Descriptive Variable: Number of systems worked with
count_sys <- Final_Pairs %>%
  ungroup() %>%
  distinct(DocNPI,year,SystemID) %>%
  count(DocNPI,year,name="num_systems")

Final_Pairs <- Final_Pairs %>%
  left_join(count_sys,by=c("year","DocNPI"))

# Treatment Variable: fraction of patients at EHR hospital
Final_Pairs <- Final_Pairs %>% 
  group_by(DocNPI,year) %>%
  mutate(num_patients=sum(samedaycount)) %>%
  mutate(num_patients_EHR=sum(samedaycount[EHR==1],na.rm=T)) %>%
  ungroup() %>%
  mutate(frac_EHR_patients=ifelse(num_patients>0,num_patients_EHR/num_patients,0))


# Aggregate the data to the physician level ------------------------------------------
Physician_Data <- Final_Pairs %>%
  distinct(DocNPI,year,grad_year,female,phys_working,num_hospitals, hosp_EHR, frac_EHR, avg_beds, 
           avg_oper_days, experience, minyr_EHR, exposed, num_patients, num_patients_EHR, frac_EHR_patients,
           num_systems)

# Create working indicator and relative year variables
Physician_Data <- Physician_Data %>%
  mutate(working_ind=ifelse(phys_working>0,1,0)) %>%
  mutate(rel_exposedyr=ifelse(minyr_EHR>0,year-minyr_EHR,NA))

# Create relative year dummies for event study
Physician_Data <- Physician_Data %>%
  mutate(rel_m6=1*(rel_exposedyr<=-6),
         rel_m5=1*(rel_exposedyr==-5),
         rel_m4=1*(rel_exposedyr==-4),
         rel_m3=1*(rel_exposedyr==-3),
         rel_m2=1*(rel_exposedyr==-2),
         rel_m1=1*(rel_exposedyr==-1),
         rel_0=1*(rel_exposedyr==0),
         rel_p1=1*(rel_exposedyr==1),
         rel_p2=1*(rel_exposedyr==2),
         rel_p3=1*(rel_exposedyr==3),
         rel_p4=1*(rel_exposedyr==4),
         rel_p5=1*(rel_exposedyr==5),
         rel_p6=1*(rel_exposedyr==6)) %>%
  mutate(rel_m1=ifelse(minyr_EHR==0,1,rel_m1))

# Create indicators for exposure and "post_year" to use in year-specific diff in diff 
Physician_Data <- Physician_Data %>%
  mutate(exposed_2010=1*(minyr_EHR==2010),
         exposed_2011=1*(minyr_EHR==2011),
         exposed_2012=1*(minyr_EHR==2012),
         exposed_2013=1*(minyr_EHR==2013),
         exposed_2014=1*(minyr_EHR==2014),
         post_2010=1*(year>=2010),
         post_2011=1*(year>=2011),
         post_2012=1*(year>=2012),
         post_2013=1*(year>=2013),
         post_2014=1*(year>=2014))

# Create an indicator for whether the physician stays working in all years
Physician_Data <- Physician_Data %>%
  group_by(DocNPI) %>%
  mutate(n=sum(working_ind)) %>%
  ungroup() %>%
  mutate(working_allyears=1*(n==7)) %>%
  select(-n)


# Save the Data for Analysis -----------------------------------------------------------------
saveRDS(Physician_Data,file=paste0(created_data_path,"Physician_Data.rds"))


# Balance Check
balance_check <- Final_Pairs %>%
  select(year,ID)

is.pbalanced(balance_check)



