library(readr)
library(dplyr)
library(stringr)

# ------------------  Filtering NPI + Tax Codes to Certain Types ------------------------------------- 
#                     Hanna Glenn, Emory University
#                     8/26/2021


# This script uses the "npidata.rds" created in the corresponding script. I take this data and filter it 
# to separate datasets based on type (defined by tax grouping). These will be used to connect physicians to
# hospitals and other entities for my third year paper. 

# Read in npidata
npidata <- read_rds(paste0(created_data_path,"npidata.rds"))


# Create PCP and hospital indicators
npidata <- npidata %>%
  mutate(PCP=ifelse(str_detect(t_class,"Internal Medicine") |
                      str_detect(t_class,"Hospitalist") | 
                      str_detect(t_class,"Family Medicine") |
                      str_detect(t_class,"General Practice"),1,0),
         hospital=ifelse(str_detect(t_group,fixed("hospital", ignore_case=TRUE)),1,0))

# Create specialist indicator
npidata <- npidata %>%
  mutate(specialist=ifelse(str_detect(t_class,fixed("surgeon",ignore_case=T)) |
                             str_detect(t_class,fixed("specialist",ignore_case=T)) |
                             str_detect(t_class,fixed("phlebology",ignore_case=T)) |
                             str_detect(t_class,fixed("otolaryngology",ignore_case=T)) |
                             str_detect(t_class,fixed("urology",ignore_case=T)) |
                             str_detect(t_class,fixed("podiatrist",ignore_case=T)),1,0))

# Create lab indicator
npidata <- npidata %>%
  mutate(lab=ifelse(str_detect(t_group,fixed("laboratories",ignore_case=T)),1,0))

# Create Indicator for Speech and Language Professionals
npidata <- npidata %>%
  mutate(speech=ifelse(str_detect(t_group,fixed("hearing servicee providers",ignore_case=T)),1,0))

# Create indicator for physical therapist
npidata <- npidata %>%
  mutate(phystherapist=ifelse(str_detect(t_class,fixed("physical therapist",ignore_case=T)),1,0))

# Create indicator for nurse
npidata <- npidata %>%
  mutate(nurse=ifelse(str_detect(t_class,fixed("nurse",ignore_case=T)),1,0))

# Create indicator for pharmacist
npidata <- npidata %>%
  mutate(pharmacist=ifelse(str_detect(t_class,fixed("pharmacist",ignore_case=T)),1,0))

# Create indicator for social service
npidata <- npidata %>%
  mutate(socialservice=ifelse(str_detect(t_group,fixed("social service providers",ignore_case=T)),1,0))

# Create "other" indicator
npidata <- npidata %>%
  mutate(other_type=ifelse(lab==1 | speech==1 | phystherapist==1 | nurse==1 |
           pharmacist==1 | socialservice==1 | specialist==1,1,0))

# Get rid of unnecessary variables
npidata <- npidata %>%
  select(npi,t_code,PCP,hospital,other_type)



# Create datasets that filter to what I need
phys_npidata <- npidata %>%
  filter(PCP==1)

hosp_npidata <- npidata %>%
  filter(hospital==1) 

other_npidata <- npidata %>%
  filter(other_type==1)


# Save these datasets 

saveRDS(phys_npidata,file=paste0(created_data_path,"phys_npidata.rds"))
saveRDS(hosp_npidata,file=paste0(created_data_path,"hosp_npidata.rds"))
saveRDS(other_npidata,file=paste0(created_data_path,"other_npidata.rds"))








         
         
