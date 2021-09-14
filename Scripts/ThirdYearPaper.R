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
# The final dataset is called "ThirdYearPaper.rds"

# Read in physician hospital pairs master file
ThirdYearPaper <- read_rds(paste0(created_data_path, "phys_hosp_pairs.rds"))
  # This data does onto have any repeats of year, HospNPI, DocNPI

# Create Experience and Gender Variables ------------------------------------------------------------
# Read in Physician Compare
PhysCompare <- read_csv(paste0(raw_data_path,"DAC_NationalDownloadableFile.csv"))

PhysCompare <- PhysCompare %>%
  rename(gender=gndr,grad_year=Grd_yr) %>%
  select(NPI,gender,grad_year)

# Merge
ThirdYearPaper <- ThirdYearPaper %>%
  left_join(PhysCompare, by=c("DocNPI"="NPI"))


ThirdYearPaper <- ThirdYearPaper %>%
  distinct()

# Create total_services Variable --------------------------------------------------------------------
# Read in MedPhys_PUF.R (created from Physician Compare in "MedPhys_PUF.R)
MedPhys_PUF <- read_rds(paste0(created_data_path,"MedPhys_PUF.rds"))

ThirdYearPaper <- ThirdYearPaper %>%
  left_join(MedPhys_PUF,by=c("DocNPI"="NPI","year"))


# Create EHR Variables ----------------------------------------------------------------------------
#Read in AHA-NPI crosswalk
AHANPI_cw <- read_rds(paste0(created_data_path,"AHANPI_cw.rds"))

# Merge crosswalk to main data
ThirdYearPaper <- ThirdYearPaper %>%
  left_join(AHANPI_cw,by=c("HospNPI"="NPI"))

# See how many unique hospitals in the dataset have an AHAID
num_hosp <- ThirdYearPaper %>%
  filter(!is.na(AHAID)) %>%
  distinct(HospNPI)
  # There are 4500 unique hospitals in the data that are not missing AHAID

# Read in main AHA survey data
# I need to think about adding more hospital characteristics to this dataset
AHAmainsurvey <- read_csv(paste0(raw_data_path,"AHAmainsurvey.csv"))

ThirdYearPaper <- ThirdYearPaper %>%
  left_join(AHAmainsurvey,by=c("AHAID"="ID","year"="YEAR"),na_matches="never")
  
# Find out how many hospitals have NA for this question (conditional on having an AHA ID)
num_missing_EHR <- ThirdYearPaper %>%
  filter(!is.na(AHAID)) %>%
  filter(is.na(EHLTH)) %>%
  group_by(year) %>%
  distinct(HospNPI)
  # 10197 missing

# Fill in missing year for EHR if it's between two years that have the same answer for EHR question
ThirdYearPaper <- ThirdYearPaper %>% group_by(AHAID) %>%
  mutate(firstyear_0=min(year[EHLTH==0],na.rm=T),lastyear_0=max(year[EHLTH==0],na.rm=T)) %>%
  mutate(firstyear_0=ifelse(is.infinite(firstyear_0),NA,firstyear_0),lastyear_0=ifelse(is.infinite(lastyear_0),NA,lastyear_0)) %>%
  mutate(EHLTH=ifelse(firstyear_0<year & year<lastyear_0 & is.na(EHLTH),0,EHLTH))
  # Fill in 586

ThirdYearPaper <- ThirdYearPaper %>% group_by(AHAID) %>%
  mutate(firstyear_1=min(year[EHLTH==1],na.rm=T),lastyear_1=max(year[EHLTH==1],na.rm=T)) %>%
  mutate(firstyear_1=ifelse(is.infinite(firstyear_1),NA,firstyear_1),lastyear_1=ifelse(is.infinite(lastyear_1),NA,lastyear_1)) %>%
  mutate(EHLTH=ifelse(firstyear_1<year & year<lastyear_1 & is.na(EHLTH),1,EHLTH))
  # Fill in 378

ThirdYearPaper <- ThirdYearPaper %>% group_by(AHAID) %>%
  mutate(firstyear_2=min(year[EHLTH==2],na.rm=T),lastyear_2=max(year[EHLTH==2],na.rm=T)) %>%
  mutate(firstyear_2=ifelse(is.infinite(firstyear_2),NA,firstyear_2),lastyear_2=ifelse(is.infinite(lastyear_2),NA,lastyear_2)) %>%
  mutate(EHLTH=ifelse(firstyear_2<year & year<lastyear_2 & is.na(EHLTH),2,EHLTH)) %>%
  ungroup()
  # Fill in 346

saveRDS(ThirdYearPaper,file=paste0(created_data_path,"ThirdYearPaper.rds"))

# Read in AHA IT Survey 

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
ThirdYearPaper <- ThirdYearPaper %>%
  mutate(AHAID=as.character(AHAID))

ThirdYearPaper <- ThirdYearPaper %>%
  left_join(AHAIT,by=c("AHAID"="ID","year"="YEAR"), na_matches="never")

# Clean up the data, get rid of unneeded
ThirdYearPaper <- ThirdYearPaper %>%
  select(-firstyear_0, -lastyear_0, -firstyear_1, -lastyear_1, -firstyear_2, -lastyear_2)

# Create indicators out of existing variables
ThirdYearPaper <- ThirdYearPaper %>% ungroup() %>%
  mutate(usesanyEHR=ifelse(EHLTH==0,0,ifelse(is.na(EHLTH),NA,1)),
         usesfullEHR=ifelse(EHLTH==2,1,ifelse(is.na(EHLTH),NA,0)))

ThirdYearPaper <- ThirdYearPaper %>%
  mutate(usesanyEHRdec=if_else(usesanyEHR==0,0,if_else(usesanyEHR==1 & (decision_index>21 | is.na(decision_index)),0,1)),
         usesanyEHRdoc=if_else(usesanyEHR==0,0,if_else(usesanyEHR==1 & (documentation_index>21 | is.na(documentation_index)),0,1)),
         usesfullEHRdec=if_else(usesfullEHR==0,0,if_else(usesfullEHR==1 & (decision_index>21 | is.na(decision_index)),0,1)),
         usesfullEHRdoc=if_else(usesfullEHR==0,0,if_else(usesfullEHR==1 & (documentation_index>21 | is.na(documentation_index)),0,1))
         )

# Read in Meaningful Use Data 

mean_use <- read_csv(paste0(raw_data_path,"HOSP_ProvidersPaidByEHR_06_2018.csv"))

# Clean up and create indicators for each stage
mean_use <- mean_use %>%
  rename(NPI="PROVIDER NPI", EHRstage="STAGE NUMBER", year="PROGRAM YEAR") %>%
  select(NPI,EHRstage,year) %>%
  filter(!is.na(EHRstage)) %>%
  mutate(NPI=as.numeric(NPI)) %>%
  mutate(stage1=if_else(str_detect(EHRstage,"Stage 1"),1,0), stage2=if_else(str_detect(EHRstage,"Stage 2"),1,0),
         stage3=if_else(str_detect(EHRstage,"Stage 3"),1,0))

# How many distinct hospitals are in the data?
distinct <- mean_use %>%
  distinct(NPI)
  # 4555 unique NPIs

# Drop multiples since appearing twice certainly implies appearing once (for same stage)
mean_use <- mean_use %>%
  group_by(year) %>%
  distinct(NPI,.keep_all=T) %>%
  ungroup()
  #14509 obs

# Merge
ThirdYearPaper <- ThirdYearPaper %>%
  left_join(mean_use, by=c("HospNPI"="NPI","year"))

ThirdYearPaper <- ThirdYearPaper %>%
  mutate(getsubsidy=ifelse(is.na(EHRstage),0,1), stage1=ifelse(is.na(stage1),0,stage1), 
         stage2=ifelse(is.na(stage2),0,stage2), stage3=ifelse(is.na(stage3),0,stage3))



# Create Extra Variables (Data is complete) --------------------------------------------

# Number of hospitals worked with (physician level)
count <- ThirdYearPaper %>%
  count(year,DocNPI,name="num_hospitals")

ThirdYearPaper <- ThirdYearPaper %>%
  left_join(count,by=c("year","DocNPI"))

# Total Shared Patients
ThirdYearPaper <- ThirdYearPaper %>%
  group_by(year,DocNPI) %>%
  mutate(total_sharedpatients=sum(samedaycount)) %>%
  ungroup()

ThirdYearPaper <- ThirdYearPaper %>%
  group_by(DocNPI) %>%
  mutate(total_sharedpatients_allyears=sum(total_sharedpatients)) %>%
  ungroup()

observe <- ThirdYearPaper %>%
  filter(total_sharedpatients_allyears==0) %>%
  distinct(DocNPI)
  # There are 3810 doctors that have no sameday counts in the entire dataset. I will drop these (they do not have a close relationship
  # with any hospital)

  # Total distinct doctor NPIs in the dataset: 316698

ThirdYearPaper <- ThirdYearPaper %>%
  filter(total_sharedpatients_allyears!=0)

# Share of Shared Patients at that hospital
ThirdYearPaper <- ThirdYearPaper %>%
  group_by(year,DocNPI) %>%
  mutate(perc_sharedpatients=samedaycount/total_sharedpatients) %>%
  ungroup()

# Indicator for main hospital in the first year the physician appears in the data
firstyear_data <- ThirdYearPaper %>%
  filter(total_sharedpatients>0) %>%
  group_by(DocNPI) %>%
  mutate(firstyear=min(year)) %>%
  distinct(DocNPI,.keep_all=T) %>%
  select(DocNPI,firstyear) %>%
  ungroup()

ThirdYearPaper <- ThirdYearPaper %>%
  left_join(firstyear_data,by="DocNPI")

ThirdYearPaper <- ThirdYearPaper %>%
  group_by(year, DocNPI) %>%
  mutate(main_hospNPI=ifelse(year==firstyear& samedaycount==max(samedaycount,na.rm=T),HospNPI,NA)) %>%
  ungroup()

ThirdYearPaper <- ThirdYearPaper %>% 
  group_by(DocNPI) %>%
  fill(main_hospNPI, .direction="up") %>%
  fill(main_hospNPI, .direction="down") %>%
  mutate(mainhosp=ifelse(HospNPI==main_hospNPI,1,0)) %>%
  ungroup()

ThirdYearPaper <- ThirdYearPaper %>%
  group_by(year,DocNPI) %>%
  mutate(main_samedaycount=ifelse(mainhosp==1,samedaycount,NA)) %>%
  fill(main_samedaycount, .direction="up") %>%
  fill(main_samedaycount, .direction="down") %>%
  ungroup()

observe <- ThirdYearPaper %>%
  select(ID,year)
  

is.pbalanced(observe)

# Save the Data -----------------------------------------------------------------

saveRDS(ThirdYearPaper,file="CreatedData/ThirdYearPaper.rds")






