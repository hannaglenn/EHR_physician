library(tidyverse)
library(readr)
library(plm)

# --------------------    Creating Dependent Variable from Aggregated Shared Patient --------------------------
#                         Hanna Glenn, Emory University
#                         10/6/2021

# ORDER::::: 3

# This script takes each doctor's shared patients with many different entities and aggregates it into one variable
# which is used as a dependent variable for my third year paper. The datasets used to create this are "phys_npidata.rds",
# "other_npidata.rds", "hosp_npidata.rds" and the Shared Patient Data from 2009-2015, the former two created 
# in "type_npidata.R". 
# The final dataset is called "phys_working.rds" and contains physician npi and the aggregated working variable. 

# Read in physician and other npi information
phys_npidata <- read_rds(paste0(created_data_path,"phys_npidata.rds"))
other_npidata <- read_rds(paste0(created_data_path,"other_npidata.rds"))
hosp_npidata <- read_rds(paste0(created_data_path,"hosp_npidata.rds"))

# Combine these into one
temp_npidata <- rbind(phys_npidata, other_npidata, hosp_npidata)

# Read in Shared Patient Data and merge to npi data
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
    rename(PCP1=PCP,hospital1=hospital,other_type1=other_type) 
  
  SP.year <- SP.year %>%
    left_join(temp_npidata, by=c("npi2"="npi")) %>%
    rename(PCP2=PCP,hospital2=hospital, other_type2=other_type) %>%
    select(npi1,npi2,samedaycount,year,PCP1,PCP2,hospital1,hospital2, other_type1, other_type2)
  
  # filter so that all is left is physician pairs with other entities
  SP.year <- SP.year %>%
    filter((PCP1==1 & hospital2==1) | (hospital1==1 & PCP2==1) | 
             (PCP1==1 & other_type2==1) | (other_type1==1 & PCP2==1))
  
  # Label which NPI is which type
  SP.year <- SP.year %>%
    mutate(DocNPI=ifelse(PCP1==1,npi1,npi2),
           otherNPI=ifelse(PCP1==1,npi2,npi1))
  
  # Aggregate same day count to the physician level
  SP.year <- SP.year %>%
    group_by(DocNPI,year) %>%
    mutate(phys_working=sum(samedaycount,na.rm=T)) %>%
    distinct(DocNPI,year,phys_working)
    
  
  assign(paste0("PSPD",t),SP.year)
}

phys_working <- rbind(PSPD2009, PSPD2010, PSPD2011, PSPD2012, PSPD2013, PSPD2014, PSPD2015)

# Make balanced dataset, where phys_working is zero if missing from data
# Note: Make sure the data is ungrouped or complete will not work
phys_working <- phys_working %>%
  ungroup()

phys_working <- complete(phys_working, year, DocNPI, fill = list(phys_working = 0))

# Check if balanced
balance_check <- phys_working %>%
  select(year, DocNPI)

is.pbalanced(balance_check)

# Save data 
saveRDS(phys_working,file=paste0(created_data_path,"phys_working.rds"))



