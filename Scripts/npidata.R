library(readr)
library(dplyr)

# ------------------  Building NPI Taxonomy Dataset ------------------------------------- 
#                     Hanna Glenn, Emory University
#                     8/26/2021


# This document uses data from National Uniform Claim Committee (NUCC) on taxonomy code definitions
# as well as the NPPES dataset on all NPI agents. This file will merge the two to result in a 
# dataset called "npidata.csv", which contains all NPI agents matched to their entity type and other
# characteristics that come from their tax code. 


# First, we read in the NUCC taxonomy code file. 
tax.define <- read_csv(paste0(path.currentdata,"nucc_taxonomy_210.csv"))
tax.define <- tax.define %>%
  select(Code, Grouping) %>%
  rename(t_code=Code,group=Grouping)
  # This should have 864 obs of 2 variables (as of 8/26/2021)

# Now we read in the NPI data which contains taxonomy code of each NPI
# For the current project I don't need it, but the file also contins enumeration date of the NPI number
npi.taxcodes <- read_csv(paste0(path.currentdata,"npidata_pfile.csv"),
                         col_types = cols(NPI=col_character()))
npi.taxcodes <- npi.taxcodes %>%
  rename(npi=NPI,enumdate='Provider Enumeration Date',entitytype='Entity Type Code',t_code='Healthcare Provider Taxonomy Code_1',zip='Provider Business Practice Location Address Postal Code', state='Provider Business Practice Location Address State Name') %>%
  select(npi,t_code,zip,state)
  # Should be 6843788 obs of 4 variables

# Merge these two datasets 
npi.data <- merge(tax.define,npi.taxcodes,by="t_code")

# Filter this data to only the types of NPI I need for my question (How does EHR implementation affect
# working decicisions of physicians?). I only need hospitals and physicians. These are defined in the
# group names. 
# I may use the indicators for specific entities later on
npi.data <- npi.data %>%
  filter(str_detect(t_group,fixed("hospital", ignore_case=TRUE)) | str_detect(t_group,fixed("physician", ignore_case=TRUE))) %>%
  mutate(GeneralAcuteHosp=ifelse(str_detect(t_code, "^282N"),1,0)) %>%
  mutate(Hospitalist=ifelse(str_detect(t_code, "^208M"),1,0)) 
  # 1942812 (almost 2% hospitals, the rest physicians)

saveRDS(npidata,file="npidata.rds")



















