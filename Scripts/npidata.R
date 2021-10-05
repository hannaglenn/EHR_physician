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
tax.define <- read_csv(paste0(raw_data_path,"nucc_taxonomy_210.csv"))
tax.define <- tax.define %>%
  select(Code, Grouping, Classification) %>%
  rename(t_code=Code,t_group=Grouping, t_class=Classification)
  # This should have 864 obs of 3 variables (as of 10/4/2021)

# Now we read in the NPI data which contains taxonomy code of each NPI
# For the current project I don't need it, but the file also contins enumeration date of the NPI number
npi.taxcodes <- read_csv(paste0(raw_data_path,"npidata_pfile.csv"),
                         col_types = cols(NPI=col_character()))
npi.taxcodes <- npi.taxcodes %>%
  rename(npi=NPI,t_code='Healthcare Provider Taxonomy Code_1') %>%
  select(npi,t_code)
  # Should be 6843788 obs of 2 variables

# Merge these two datasets 
npidata <- merge(tax.define,npi.taxcodes,by="t_code")

# Filter this data to only the types of NPI I need to form PCP-hospital pairs. 
npidata <- npidata %>%
  filter(str_detect(t_group,fixed("hospital", ignore_case=TRUE)) | str_detect(t_group,fixed("physician", ignore_case=TRUE)))
# 1942812 (almost 2% hospitals, the rest physicians)

# Create PCP and hospital indicators
npidata <- npidata %>%
  mutate(PCP=ifelse(str_detect(t_class,"Internal Medicine") |
                      str_detect(t_class,"Hospitalist") | 
                      str_detect(t_class,"Family Medicine") |
                      str_detect(t_class,"General Practice"),1,0),
         hospital=ifelse(str_detect(t_group,fixed("hospital", ignore_case=TRUE),1,0)))

saveRDS(npidata,file="npidata.rds")



















