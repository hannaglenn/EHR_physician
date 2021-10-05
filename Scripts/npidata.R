library(readr)
library(dplyr)

# ------------------  Building NPI Taxonomy Dataset ------------------------------------- 
#                     Hanna Glenn, Emory University
#                     Edited 10/5/2021


# This document uses data from National Uniform Claim Committee (NUCC) on taxonomy code definitions
# as well as the NPPES dataset on all NPI agents. This file will merge the two to result in a 
# dataset called "npidata.csv", which contains all NPI agents matched to their entity type and other
# characteristics that come from their tax code. 


# First, we read in the NUCC taxonomy code file. 
tax.define <- read_csv(paste0(raw_data_path,"nucc_taxonomy_210.csv"))
tax.define <- tax.define %>%
  select(Code, Grouping, Classification) %>%
  rename(t_code=Code,t_group=Grouping, t_class=Classification)
  # This should have 864 obs of 3 variables 

# Now we read in the NPI data which contains taxonomy code of each NPI
# For the current project I don't need it, but the file also contains enumeration date of the NPI number
npi.taxcodes <- read_csv(paste0(raw_data_path,"npidata_pfile.csv"),
                         col_types = cols(NPI=col_character()))
npi.taxcodes <- npi.taxcodes %>%
  rename(npi=NPI,t_code='Healthcare Provider Taxonomy Code_1') %>%
  select(npi,t_code)
  # Should be 6843782 obs of 2 variables

# Merge these two datasets 
npidata <- merge(tax.define,npi.taxcodes,by="t_code")

# Save the data
saveRDS(npidata,file=paste0(created_data_path,"/npidata.rds"))





















