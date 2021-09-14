library(dplyr)
library(readr)

# -----------------------------   Read in and Clean Provider Utilization and Payment Files ------------------------
#                                 Hanna Glenn, Emory University
#                                 8/31/2021


# This script reads in the "Medicare Physician and Other Supplier PUF" data from 2012-2015.
# The data will provide the dependent variable for my third year paper. 

# Read in Provider Utilization and Payment Files ----------------------------------------------------------------------
for (t in 2012:2013){
  PartByear <- read_tsv(paste0(path.currentdata,"Medicare_Provider_Util_Payment_PUF_CY",t,".txt"))
  PartByear <- PartByear %>%
    select(NPI,HCPCS_CODE,BENE_UNIQUE_CNT,BENE_DAY_SRVC_CNT) %>%
    rename(num_services=BENE_UNIQUE_CNT,num_services_day=BENE_DAY_SRVC_CNT) %>%
    filter(NPI>1) %>%
    mutate(year=t)
  
  assign(paste0("PartB",t),PartByear)
}

for (t in 2014:2015){
  PartByear <- read_tsv(paste0(path.currentdata,"Medicare_Provider_Util_Payment_PUF_CY",t,".txt"))
  PartByear <- PartByear %>%
    select(npi,hcpcs_code,bene_unique_cnt,bene_day_srvc_cnt) %>%
    rename(num_services=bene_unique_cnt,num_services_day=bene_day_srvc_cnt, NPI=npi, HCPCS_CODE=hcpcs_code) %>%
    filter(NPI>1) %>%
    mutate(year=t)
  
  assign(paste0("PartB",t),PartByear)
}

MedPhys_PUF <- rbind(PartB2012, PartB2013, PartB2014, PartB2015)

MedPhys_PUF <- MedPhys_PUF %>%
  distinct()

# I need to create an aggregated version so that each NPI has one utilization measure
MedPhys_PUF <- MedPhys_PUF %>%
  group_by(NPI,year) %>%
  mutate(total_services=sum(num_services),NPI=as.numeric(NPI)) %>%
  mutate(total_services=ifelse(total_services==0,NA,total_services)) %>%
  select(year,NPI,total_services) %>%
  distinct()

MedPhys_PUF <- MedPhys_PUF %>%
  mutate(NPI=as.numeric(NPI))

saveRDS(MedPhys_PUF,file=paste0(path.currentdata,"MedPhys_PUF.rds"))

