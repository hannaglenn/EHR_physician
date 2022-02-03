library(readr)
library(tidyr)
library(plm)

# ------------------------------------- INITIALIZING PHYSICIAN DATA WITH MDPPAS ------------------------------------
#                                       Hanna Glenn, Emory University
#                                       1/21/2022

# ORDER:::::: 1

# This script reads in the MDPPAS data from 2009-2017 and uses it to create a balanced physician
# level dataset which will be built upon using shared patient data. MDPPAS allows me to filter 
# to only hospitalists and primary care physicians in each year. I take the list of npis that are hospitalists and use it 
# to limit the pairs of physicians and hospitals in the shared patient data. Once the shared patient
# and EHR information is complete, I merge it back to this physician level data. 

# Read in MDPPAS data
for (i in 2009:2017) {
year <- read_csv(paste0(raw_data_path,"/MDPPAS/PhysicianData_",i,".csv"))
assign(paste0("MDPPAS",i),year)
}

MDPPAS <- rbind(MDPPAS2009, MDPPAS2010, MDPPAS2011, MDPPAS2012, MDPPAS2013, MDPPAS2014, MDPPAS2015, MDPPAS2016, MDPPAS2017)

# Here are the specifications for physicians to keep in the data. First, if they are ever classified as a hospitalist. Second, if 
# they are family practice or internal medicine. (Some of these will get dropped later when I filter doctor-hospital pairs.)
MDPPAS <- MDPPAS %>%
  dplyr::mutate(n=ifelse(spec_prim_1_name=='Hospitalist' | spec_prim_1_name=='Internal Medicine' |
                           spec_prim_1_name=="Pediatric Medicine" | spec_prim_1_name=='Family Practice' |
                           spec_prim_1_name=="General Practice",1,0)) %>%
  dplyr::group_by(npi) %>%
  dplyr::mutate(sum=sum(n)) %>%
  dplyr::ungroup()

# Keep anyone in the data who meets the qualifications described above
MDPPAS <- MDPPAS %>%
  dplyr::filter(sum>0) 

# Create a list of npi numbers to use when creating the shared patient data
npi_list <- MDPPAS %>%
  dplyr::distinct(npi)
  # 245k obs

saveRDS(npi_list,file=paste0(created_data_path,"npi_list.rds"))




