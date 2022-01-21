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
  # 4 mill obs, 5 variables

data <- data %>%
  rename(pairID=ID)

# Create Experience and Gender Variables ------------------------------------------------------------
# Read in Physician Compare
PhysCompare <- read_csv(paste0(raw_data_path,"DAC_NationalDownloadableFile.csv"))

PhysCompare <- PhysCompare %>%
  dplyr::rename(gender=gndr,grad_year=Grd_yr) %>%
  select(NPI,gender,grad_year) %>%
  mutate(female=ifelse(gender=='F',1,ifelse(gender=='M',0,NA))) %>%
  select(-gender)

# Merge, then drop any physicians that graduated after 2008
data <- data %>%
  left_join(PhysCompare, by=c("DocNPI"="NPI")) %>%
  distinct() %>%
  filter(grad_year<2009)
  # Now have 4.4 mill obs


#### Create num_billings_partB Variable --------------------------------------------------------- ####
#Read in MedPhys_PUF.R (created from Physician Compare in "MedPhys_PUF.R)
#MedPhys_PUF <- read_rds(paste0(created_data_path,"MedPhys_PUF.rds"))

#Final_Pairs <- Final_Pairs %>%
#  left_join(MedPhys_PUF,by=c("DocNPI"="NPI","year")) %>%
#  rename(num_billings_partB=total_services)




#skip this, not using in my analysis for now since years are so limited


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
  # There are 4418 unique hospitals in the data that are not missing AHAID (good)

# Only keep pairs with AHA hospitals since that is where EHR information comes from
data <- data %>%
  filter(!is.na(AHAID))
  #2.5 mill

balance_check <- data %>% ungroup() %>%
  distinct(year,pairID)

# Check if still balanced
is.pbalanced(balance_check)
  #TRUE

# Read in main AHA survey data
# I need to think about adding more hospital characteristics to this dataset
AHAmainsurvey <- read_csv(paste0(raw_data_path,"AHA_mainsurvey.csv"))

AHAmainsurvey <- AHAmainsurvey %>%
  mutate(ID=as.character(ID))

data <- data %>% ungroup() %>%
  mutate(AHAID=as.character(AHAID)) %>%
  left_join(AHAmainsurvey,by=c("AHAID"="ID","year"="YEAR"),na_matches="never") %>%
  rename(SystemID="SYSID","days_hosp_operating"=DCOV, beds=BDTOT) %>%
  select(-REG)

  
# Find out how many hospital years have NA for this question (conditional on having an AHA ID)
num_missing_EHR <- data %>%
  filter(is.na(EHLTH)) %>%
  group_by(year) %>%
  distinct(HospNPI)
  # 9585 missing out of 30,926 total hospital years

# Find out how many hospitals are missing an answer in every year
num_always_missing_EHR <- data %>% ungroup() %>%
  filter(is.na(EHLTH)) %>%
  mutate(EHLTH=ifelse(is.na(EHLTH),10,EHLTH)) %>%
  group_by(HospNPI) %>%
  mutate(always_missing=ifelse(sum(EHLTH)==60,1,0)) %>%
  filter(always_missing==1) %>%
  ungroup() %>%
  distinct(HospNPI, .keep_all = T)
  # There are only 57 hospitals that never answer this question, but they typically have answers to the other questions in the survey

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
  # Now down to 8343 missing 

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
  left_join(AHAIT,by=c("AHAID"="ID","year"="YEAR"), na_matches="never")



#### Meaningful Use Data ---------------------------------####


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


#skip for now, not using in analysis

# Bring in Working variable from all hospital shared patient entities--------------------------------------------
phys_working <- read_rds(paste0(created_data_path,"phys_working.rds"))

# merge by year & DocNPI
data <- data %>%
  mutate(DocNPI=as.character(DocNPI)) %>%
  left_join(phys_working, by=c("year","DocNPI"))

# Create a labor market variable for just sum of hospital shared patients in a year for a doctor
data <- data %>%
  group_by(DocNPI, year) %>%
  mutate(hosp_count=sum(samedaycount)) %>%
  rename(other_count=phys_working) %>%
  ungroup()

# Create indicator for working, but not in hospitals
data <- data %>% 
  group_by(DocNPI, year) %>%
  mutate(nonhosp_ind=ifelse(hosp_count==0 & other_count>0, 1, 0)) %>%
  ungroup()





# Bring in SK&A (FIX CODE NAMES BEFORE RUNNING) -----------------------------------------------------------------------------------------
#ska2009 <- read_csv(paste0(raw_data_path,"SK&A/SKA2009.csv"))
#ska2009 <- ska2009 %>%
  #mutate(year=2009) %>%
  #select(npi,year)

#ska2010 <- read_csv(paste0(raw_data_path,"SK&A/SKA2010.csv"))
#ska2010 <- ska2010 %>%
  #mutate(year=2010) %>%
  #select(npi,year)

#ska2011 <- read_csv(paste0(raw_data_path,"SK&A/SKA2011.csv"))
#ska2011 <- ska2011 %>%
  #mutate(year=2011) %>%
  #select(npi,year)

#ska2012 <- read_csv(paste0(raw_data_path,"SK&A/SKA2012.csv"))
#ska2012 <- ska2012 %>%
  #mutate(year=2012) %>%
  #select(npi,year)

#ska2013 <- read_csv(paste0(raw_data_path,"SK&A/SKA2013.csv"))
#ska2013 <- ska2013 %>%
  #mutate(year=2013) %>%
  #select(npi,year)

#ska2014 <- read_csv(paste0(raw_data_path,"SK&A/SKA2014.csv"))
#ska2014 <- ska2014 %>%
  #mutate(year=2014) %>%
  #select(npi,year)

#ska2015 <- read_csv(paste0(raw_data_path,"SK&A/SKA2015.csv"))
#ska2015 <- ska2015 %>%
  #mutate(year=2015) %>%
  #select(npi,year)

#ska <- rbind(ska2009, ska2010, ska2011, ska2012, ska2013, ska2014, ska2015)

#ska <- ska %>%
  #mutate(ska=1, npi=as.character(npi))

# Merge Final_Pairs and SKA
#Final_Pairs <- Final_Pairs %>%
  #left_join(ska, by=c("DocNPI"="npi","year")) %>%
  #mutate(ska=if_else(is.na(ska),0,ska))

  


# Create/Clean Variables (Reading in data is complete) --------------------------------------------

### Descriptive Variables to Drop Hospitals ------------------------------------------------------
# Descriptive Variable: Average size of hospital worked with
# Drop hospitals with really low bed count
low_beds <- data %>% ungroup() %>%
  distinct(HospNPI, year, beds) %>%
  filter(beds<10) %>%
  distinct(HospNPI) %>%
  mutate(low_beds=1)
  # 72 hospitals

data <- data %>%
  left_join(low_beds, by="HospNPI") %>%
  filter(is.na(low_beds)) %>%
  select(-low_beds)
  # 3.7 mill obs

data <- data %>%
  group_by(DocNPI,year) %>%
  mutate(avg_beds=mean(beds,na.rm=T))

# Average Operating Days of Hospitals Worked With
# Drop hospitals that ever have 0 days operating
# (When looking at the data of these hospitals, they are missing most other characteristics)
no_operating <- data %>% ungroup() %>%
  filter(days_hosp_operating==0) %>%
  distinct(HospNPI) %>%
  mutate(drop=1)

low_operating <- data %>% ungroup() %>%
  filter(days_hosp_operating<360)

data <-data %>% ungroup() %>%
  left_join(no_operating, by="HospNPI") %>%
  filter(is.na(drop)) %>%
  select(-drop)
  # 1.96 mill obs

data <- data %>%
  ungroup() %>%
  group_by(DocNPI, year) %>%
  mutate(avg_oper_days=mean(days_hosp_operating,na.rm=T))

# Descriptive Variable: Years of Experience
data <- data %>% ungroup() %>%
  mutate(experience=year-grad_year)



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

# Figure out if any hospitals only have NAs for all years
always_missing_EHR <- data %>% ungroup() %>%
  distinct(HospNPI,year,EHR) %>% ungroup() %>%
  mutate(EHR=ifelse(is.na(EHR),-1,EHR)) %>%
  group_by(HospNPI) %>%
  mutate(sum=sum(EHR,na.rm=T)) %>%
  filter(sum==-7) %>%
  distinct(HospNPI,sum) %>%
  mutate(alwaysmissingEHR=1) %>%
  select(-sum)
  # 35 hospitals never answer the question about EHRs.
  # I need to figure out whether these hospitals differ from the population of hospitals. 

data <- data %>% ungroup() %>%
  left_join(always_missing_EHR, by="HospNPI") %>%
  mutate(alwaysmissingEHR=ifelse(is.na(alwaysmissingEHR),0,alwaysmissingEHR))

# Going to drop these hospitals

data <- data %>%
  filter(alwaysmissingEHR==0) %>%
  select(-alwaysmissingEHR)


<<<<<<< HEAD:Scripts/Final_Pairs.R
info_missing <- data %>% filter(alwaysmissingEHR==1) %>%
  group_by(year) %>%
  summarise_at(c("samedaycount", "beds"),
      list(m=mean,sd=sd,min=min,max=max,n=~sum(!is.na(.))), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,n,m,sd,min,max)

info_nonmissing <- data %>% filter(is.na(alwaysmissingEHR)) %>%
  group_by(year) %>%
  summarise_at(c("samedaycount", "beds"),
               list(m=mean,sd=sd,min=min,max=max,n=~sum(!is.na(.))), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,n,m,sd,min,max)
=======
>>>>>>> 2e8fed26805c026dfdb9be60886366b019fe91e1:Scripts/data4_physician_level.R

# Create total number of hospitals a physician works with
count <- data %>%
  ungroup() %>%
  count(year,DocNPI,name="num_hosp_total")

data <- data %>%
  left_join(count,by=c("year","DocNPI"))

# Create number hospitals with EHR
data <- data %>% ungroup() %>%
  group_by(DocNPI,year) %>%
  mutate(num_hosp_EHR=sum(EHR,na.rm=T))


# Variable: fraction of hospitals with EHR
data <- data %>%
  mutate(frac_EHR=num_hosp_EHR/num_hosp_total)



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

# Create "exposed" indicator based on year and min year of EHR
data <- data %>%
  mutate(anyEHR_exposed=ifelse(minyr_EHR>0 & year>=minyr_EHR,1,0))

# Treatment Variable: "Main" hospital exposed to EHR


# Treatment Variable: fraction of patients at EHR hospital
data <- data %>% 
  group_by(DocNPI,year) %>%
  mutate(hosp_count_EHR=sum(samedaycount[EHR==1],na.rm=T)) %>%
  ungroup() %>%
  mutate(frac_EHR_patients=ifelse(hosp_count>0,hosp_count_EHR/hosp_count,0))


# More Descriptive Variables --------------------------------------------------------------
# Descriptive Variable: Number of systems worked with
count_sys <- data %>%
  ungroup() %>%
  distinct(DocNPI,year,SystemID) %>%
  count(DocNPI,year,name="num_systems")

data <- data %>%
  left_join(count_sys,by=c("year","DocNPI"))



# Treatment Variable: start office work
# Create variable for first year a doc was in SKA data
#minyr_ska <- Final_Pairs %>% ungroup() %>%
  #distinct(DocNPI, year, ska) %>%
  #filter(ska==1) %>%
  #group_by(DocNPI) %>%
  #mutate(minyr_ska=min(year)) %>%
  #ungroup() %>%
  #distinct(DocNPI,minyr_ska)

#Final_Pairs <- Final_Pairs %>% ungroup() %>%
  #left_join(minyr_ska, by="DocNPI") %>%
  #mutate(minyr_ska=ifelse(is.na(minyr_ska),0,minyr_ska))

# Create "to_office" indicator based on year and min year of ska
#Final_Pairs <- Final_Pairs %>%
  #mutate(to_office=ifelse(minyr_ska>0 & year>=minyr_ska,1,0))



# Aggregate the data to the physician level -------------------------------------------------------------

Physician_Data <- data %>%
  distinct(DocNPI,year,grad_year,female, hosp_count, hosp_count_EHR, other_count, nonhosp_ind,num_hosp_total, num_hosp_EHR, frac_EHR, avg_beds, 
           avg_oper_days, experience, minyr_EHR, anyEHR_exposed, frac_EHR_patients,
           num_systems)
  #1 mill obs


# Create an indicator for whether the physician stays working in all years (using hospital patients)
# I use this variable when studying the productivity dependent variable
Physician_Data <- Physician_Data %>%
  group_by(DocNPI,year) %>%
  mutate(working=ifelse(hosp_count>0,1,0)) %>%
  group_by(DocNPI) %>%
  mutate(n=sum(working)) %>%
  ungroup() %>%
  mutate(working_allyears=1*(n==7)) %>%
  select(-n)

# Create an indicator for exposed ever
Physician_Data <- Physician_Data %>%
  group_by(DocNPI) %>%
  mutate(n=sum(exposed)) %>%
  ungroup() %>%
  mutate(exposed_ever=ifelse(n>0,1,0)) %>%
  select(-n)


# Save the Data for Analysis -----------------------------------------------------------------
saveRDS(Physician_Data,file=paste0(created_data_path,"Physician_Data.rds"))
write.csv(Physician_Data,file=paste0(created_data_path,"Physician_Data.csv"))


# Balance Check
balance_check <- Physician_Data %>%
  select(year,DocNPI)

is.pbalanced(balance_check)

