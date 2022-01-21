library(tidyr)
library(readr)
library(stringr)
library(dplyr)
library(plm)
library(janitor)


# ------------------------------------- Building Balanced Physician Hospital Pairs ------------------------------------
#                                       Hanna Glenn, Emory University
#                                       9/2/2021

# ORDER:::::: 2

# This script reads in (1) the CMS Physician Shared Patient Data from 2009-2015, (2) phys_npidata and hosp_npidata that were
# created in "data1_npidata.R". These data are all merged to end up with physician hospital pairs and their shared count. 
# The resulting dataset, called "data2_pairs.rds" consists of a balanced dataset of physician hospital pairs 
# from 2009-2015.
# Note: preliminary filtering is done on these pairs to only save those with more than 30 shared patients total. 

# Read in npi datasets and combine into one------------------
phys_npidata <- read_rds(paste0(created_data_path,"phys_npidata.rds"))
hosp_npidata <- read_rds(paste0(created_data_path,"hosp_npidata.rds"))

temp_npidata <- rbind(phys_npidata, hosp_npidata)

##### Read in Shared Patient Data, merge to temp_npi along the way -------------------------------------######
for (t in 2009:2015){
  SP.year <- read_csv(paste0(raw_data_path,"/PSPP/pspp",t,"_90.csv"),
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
  # 16.3 mill observations

# Change to physician and hospital npis instead of npi1 and npi2
phys_hosp_pairs <- phys_hosp_pairs %>%
  mutate(PCP1=ifelse(is.na(PCP1),0,PCP1), PCP2=ifelse(is.na(PCP2),0,PCP2),
         hospital1=ifelse(is.na(hospital1),0,hospital1), hospital2=ifelse(is.na(hospital2),0,hospital2)) %>%
  mutate(HospNPI=ifelse(hospital1==1,npi1,npi2),
         DocNPI=ifelse(PCP1==1,npi1,npi2)) %>%
  select(year,HospNPI,DocNPI,samedaycount)


# Understanding duplicate rows in the data
# Create a dataset where HospNPI, DocNPI pairs are repeated in the same year
duplicates <- phys_hosp_pairs %>%
  get_dupes(year, DocNPI, HospNPI)
  # A typical duplicate has one positive samedaycount and one zero samedaycount. I take the sum and combine them into one

phys_hosp_pairs <- phys_hosp_pairs %>%
  group_by(year,DocNPI, HospNPI) %>%
  mutate(samedaycount_combine=sum(samedaycount,na.rm=T))

phys_hosp_pairs <- phys_hosp_pairs %>%
  ungroup() %>%
  select(year,HospNPI,DocNPI,samedaycount_combine) %>%
  distinct() %>%
  rename(samedaycount=samedaycount_combine)
  # 9.5 million obs. 

# Filter out any hospital-physician pairs that have sum less than 30 shared patients in all years
phys_hosp_pairs <- phys_hosp_pairs %>% ungroup() %>%
  group_by(DocNPI,HospNPI) %>%
  mutate(sum=sum(samedaycount,na.rm=T)) %>%
  filter(sum>30) %>%
  select(-sum) %>%
  ungroup()
    #4.4 mill obs

# Filter out any pairs for which all years have less than 30 shared patients
phys_hosp_pairs <- phys_hosp_pairs %>% ungroup() %>%
  group_by(DocNPI, HospNPI) %>%
  mutate(max=max(samedaycount,na.rm=T)) %>%
  filter(max>30) %>%
  select(-max) %>%
  ungroup()
  # 2.9 mill obs


# Expand so that each doctor, hospital pair appears in each year (with samedaycount 0 if they were not previously in the data)
phys_hosp_pairs <- phys_hosp_pairs %>%
  mutate(HospNPI=as.numeric(HospNPI),
         DocNPI=as.numeric(DocNPI)) %>%
  ungroup()

phys_hosp_pairs <- complete(phys_hosp_pairs, expand(phys_hosp_pairs, nesting(HospNPI,DocNPI), year), fill=list(samedaycount=0))
  # 4 mill obs.

phys_hosp_pairs <- phys_hosp_pairs %>%
  ungroup() %>%
  group_by(DocNPI,HospNPI) %>%
  mutate(ID = cur_group_id())

balance_check <- phys_hosp_pairs %>%
  ungroup() %>%
  select(ID,year)

is.pbalanced(balance_check)
  #TRUE

saveRDS(phys_hosp_pairs,paste0(created_data_path,"phys_hosp_pairs.rds"))


