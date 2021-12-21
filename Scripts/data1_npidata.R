library(readr)
library(dplyr)

# ------------------  Building NPI Taxonomy Dataset ------------------------------------- 
#                     Hanna Glenn, Emory University
#                     Edited 10/5/2021

# ORDER:::: 1

# This document uses data from National Uniform Claim Committee (NUCC) on taxonomy code definitions
# as well as the NPPES dataset on all NPI agents. This file will merge the two to result in a 
# dataset called "npidata.csv", which contains all NPI agents matched to their entity type and other
# characteristics that come from their tax code. From this data I create subsets of data based on the type of entity.
# The subsets are saved in CreatedData folder.


# First, we read in the NUCC taxonomy code file. 
tax.define <- read_csv(paste0(raw_data_path,"nucc_taxonomy_210.csv"))
tax.define <- tax.define %>%
  select(Code, Grouping, Classification) %>%
  rename(t_code=Code,t_group=Grouping, t_class=Classification)
  # This should have 865 obs of 3 variables 

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


# Classify entities by tax code groupings
npidata_type <- npidata %>%
  mutate(PCP=ifelse(str_detect(t_class,"Internal Medicine") |
                      str_detect(t_class,"Hospitalist") | 
                      str_detect(t_class,"Family Medicine") |
                      str_detect(t_class,"General Practice"),1,0),
         hospital=ifelse(str_detect(t_group,fixed("hospital", ignore_case=TRUE)),1,0))

# Create specialist indicator
npidata_type <- npidata_type %>%
  mutate(specialist=ifelse(str_detect(t_class,fixed("surgeon",ignore_case=T)) |
                             str_detect(t_class,fixed("specialist",ignore_case=T)) |
                             str_detect(t_class,fixed("phlebology",ignore_case=T)) |
                             str_detect(t_class,fixed("otolaryngology",ignore_case=T)) |
                             str_detect(t_class,fixed("urology",ignore_case=T)) |
                             str_detect(t_class,fixed("podiatrist",ignore_case=T)),1,0))

# Create lab indicator
npidata_type <- npidata_type %>%
  mutate(lab=ifelse(str_detect(t_group,fixed("laboratories",ignore_case=T)),1,0))

# Create Indicator for Speech and Language Professionals
npidata_type <- npidata_type %>%
  mutate(speech=ifelse(str_detect(t_group,fixed("hearing servicee providers",ignore_case=T)),1,0))

# Create indicator for physical therapist
npidata_type <- npidata_type %>%
  mutate(phystherapist=ifelse(str_detect(t_class,fixed("physical therapist",ignore_case=T)),1,0))

# Create indicator for nurse
npidata_type <- npidata_type %>%
  mutate(nurse=ifelse(str_detect(t_class,fixed("nurse",ignore_case=T)),1,0))

# Create indicator for pharmacist
npidata_type <- npidata_type %>%
  mutate(pharmacist=ifelse(str_detect(t_class,fixed("pharmacist",ignore_case=T)),1,0))

# Create indicator for social service
npidata_type <- npidata_type %>%
  mutate(socialservice=ifelse(str_detect(t_group,fixed("social service providers",ignore_case=T)),1,0))

# Create "other" indicator
npidata_type <- npidata_type %>%
  mutate(other_type=ifelse(lab==1 | speech==1 | phystherapist==1 | nurse==1 |
                             pharmacist==1 | socialservice==1 | specialist==1,1,0))

# Get rid of unnecessary variables
npidata_type <- npidata_type %>%
  select(npi,t_code,PCP,hospital,other_type)



# Create datasets that filter to what I need
phys_npidata <- npidata_type %>%
  filter(PCP==1)

hosp_npidata <- npidata_type %>%
  filter(hospital==1) 

other_npidata <- npidata_type %>%
  filter(other_type==1)


# Save these datasets 
saveRDS(phys_npidata,file=paste0(created_data_path,"phys_npidata.rds"))
saveRDS(hosp_npidata,file=paste0(created_data_path,"hosp_npidata.rds"))
saveRDS(other_npidata,file=paste0(created_data_path,"other_npidata.rds"))





















