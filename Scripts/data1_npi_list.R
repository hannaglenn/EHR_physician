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
for (i in 2009:2017) {
year <- read_csv(paste0(raw_data_path,"/MDPPAS/PhysicianData_",i,".csv"))
assign(paste0("MDPPAS",i),year)
}

MDPPAS <- rbind(MDPPAS2009, MDPPAS2010, MDPPAS2011, MDPPAS2012, MDPPAS2013, MDPPAS2014, MDPPAS2015, MDPPAS2016, MDPPAS2017)

# I need to filter to anyone who is a hospitalist at some point in the data. First, I need to
# check if anyone ever claims hospitalist in one year and another title in another.
# This way I don't drop observations that are meant to stay.
MDPPAS <- MDPPAS %>%
  dplyr::mutate(n=ifelse(spec_prim_1_name=='Hospitalist',1,0),
                in_data=1) %>%
  dplyr::group_by(npi) %>%
  dplyr::mutate(sum_hosp=sum(n), sum_total=sum(in_data))

# Keep anyone in the data who has the title hospitalist for at least one year
MDPPAS <- MDPPAS %>%
  dplyr::filter(sum_hosp>0) %>%
  dplyr::select(-name_middle, -spec_broad, -spec_prim_1, -spec_prim_2,
                -phy_zip7, -claim_count7, -phy_zip8, -claim_count8, -phy_zip9, -claim_count9,
                -phy_zip10, -claim_count10, -phy_zip11, -claim_count11, -phy_zip12, 
                -claim_count12)



# Create a list of npi numbers to use when creating the shared patient data
npi_list <- MDPPAS %>%
  dplyr::distinct(npi)
  # 53k obs

saveRDS(npi_list,file=paste0(created_data_path,"npi_list.rds"))




