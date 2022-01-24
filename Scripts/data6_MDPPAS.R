library(readr)
library(tidyr)
library(plm)

# ------------------------------------- INITIALIZING PHYSICIAN DATA WITH MDPPAS ------------------------------------
#                                       Hanna Glenn, Emory University
#                                       1/21/2022

# ORDER:::::: 1

# This script reads in the MDPPAS data from 2009-2017 and uses it to create a balanced physician
# level dataset which will be built upon using shared patient data. MDPPAS allows me to filter 
# to only hospitalists in each year. I take the list of npis that are hospitalists and use it 
# to limit the pairs of physicians and hospitals in the shared patient data. Once the shared patient
# and EHR information is complete, I merge it back to this physician level data. 

# Read in MDPPAS data
for (i in 2009:2011) {
year <- read_csv(paste0(raw_data_path,"/MDPPAS/PhysicianData_",i,".csv"))
assign(paste0("MDPPAS",i),year)
}

MDPPAS <- rbind(MDPPAS2009, MDPPAS2010, MDPPAS2011, MDPPAS2012, MDPPAS2013, MDPPAS2014, MDPPAS2015,
                MDPPAS2016, MDPPAS2017)

observe2 <- observe %>%
  dplyr::filter(!is.na(claim_count6))

observe <- MDPPAS %>%
  dplyr::filter(spec_prim_1_name=='Hospitalist') %>%
  dplyr::select(-name_middle, -spec_broad, -spec_prim_1, -spec_prim_2,
                -phy_zip7, -claim_count7, -phy_zip8, -claim_count8, -phy_zip9, -claim_count9,
                -phy_zip10, -claim_count10, -phy_zip11, -claim_count11, -phy_zip12, -claim_count12)

# MDPPAS is not balanced. Physicians who drop out are no longer included. 
# Complete the data to have each physician in each year. 
balance_check <- MDPPAS %>%
  select(npi,Year)

is.pbalanced(balance_check)
  #FALSE

balanced <- observe %>%
  dplyr::distinct(npi, Year)
test <- complete(balanced, npi,year)

# Create a list of npi numbers to keep in the pair data
npi_list <- MDPPAS %>%
  dplyr::filter(spec_prim_1_name=='Hospitalist' | spec_prim_2_name=='Hospitalist') %>%
  dplyr::distinct(npi)

saveRDS(npi_list,file=paste0(created_data_path,"npi_list.rds"))




# Merge MDPPAS to Physician Data
Physician_data <- Physician_data %>%
  mutate(DocNPI=as.numeric(DocNPI)) %>%
  left_join(MDPPAS2009, by=c("DocNPI"="npi", "year"="Year"))
