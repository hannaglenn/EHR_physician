library(tidyverse)
library(readr)

# --------------------    Assessing the Existence of Data Assistants --------------------------
#                         Hanna Glenn, Emory University
#                         1/13/2021

# # ORDER :::::::::: 6 (Separate Analysis)

# This script analyzes the existence of data assistants in the npi data over
# 2009-2017.

npi.taxcodes <- read_csv(paste0(raw_data_path,"npidata_pfile.csv"),
                         col_types = cols(NPI=col_character()))

npi.taxcodes <- npi.taxcodes %>%
  dplyr::rename(npi=NPI,t_code='Healthcare Provider Taxonomy Code_1',
                enum_date='Provider Enumeration Date',
                state='Provider Business Mailing Address State Name',
                zip='Provider Business Mailing Address Postal Code') %>%
  select(npi, t_code, enum_date, state, zip) %>%
  filter(t_code=="247000000X" | t_code=="246YR1600X" | t_code=="246Y00000X" | t_code=="246YC3301X" |
           t_code=="2470A2800X")

npi.taxcodes <- npi.taxcodes %>%
  separate(enum_date, sep="/", into = c("month", "day", "enum_year"))

npi.taxcodes <- npi.taxcodes %>%
   mutate(enum_year=as.numeric(enum_year))


ggplot(npi.taxcodes, aes(x=enum_year)) + 
  geom_histogram(binwidth=1, colour="black", fill="white") + ylab("Count\n") + xlab("\nNPI Enumeration Year") +
  theme(text=element_text(size=15))
ggsave("Objects/dataassistant_histogram.pdf", width=8, height=5 , units = "in")


## Create database of physicians who work with Data Assistants
# Read in npi datasets and combine into one------------------
hosp_npidata <- read_rds(paste0(created_data_path,"hosp_npidata.rds"))
phys_npidata <- read_rds(paste0(created_data_path,"phys_npidata.rds"))

hosp_npidata <- hosp_npidata %>%
  dplyr::mutate(DA=0)
phys_npidata <- phys_npidata %>%
  dplyr::mutate(DA=0)

da_npidata <- npi.taxcodes %>%
  select(npi, t_code) %>%
  mutate(PCP=0, hospital=0, other_type=0, DA=1)

temp_npidata <- rbind(da_npidata, phys_npidata)



##### Read in Shared Patient Data, merge to temp_npi along the way -------------------------------------######
for (t in 2009:2015){
  SP.year <- read_csv(paste0(raw_data_path,"/PSPP/pspp",t,"_30.csv"),
                      col_types=cols(npi1=col_character(),
                                     npi2=col_character()
                      ))
  SP.year <- SP.year %>%
    select(npi1,npi2,year) %>%
    distinct()
  
  SP.year<- SP.year %>%
    left_join(temp_npidata, by=c("npi1"="npi")) %>%
    dplyr::rename(DA1=DA,PCP1=PCP) 
  
  SP.year <- SP.year %>%
    left_join(temp_npidata, by=c("npi2"="npi")) %>%
    dplyr::rename(DA2="DA",PCP2="PCP") %>%
    select(npi1,npi2,year,DA1,DA2,PCP1,PCP2)
  
  # filter so that all is left is hospital-data assistant pairs
  SP.year <- SP.year %>%
    filter((DA1==1 & PCP2==1) | (PCP1==1 & DA2==1))
  
  assign(paste0("PSPD",t),SP.year)
}

da_phys_pairs <- rbind(PSPD2009, PSPD2010, PSPD2011, PSPD2012, PSPD2013, PSPD2014, PSPD2015)
# 75 observations

# Change to DA and phys npis instead of npi1 and npi2
da_phys_pairs <- da_phys_pairs %>%
  mutate(DA1=ifelse(is.na(DA1),0,DA1), DA2=ifelse(is.na(DA2),0,DA2),
         PCP1=ifelse(is.na(PCP1),0,PCP1), PCP2=ifelse(is.na(PCP2),0,PCP2)) %>%
  mutate(DocNPI=ifelse(PCP1==1,npi1,npi2),
         DANPI=ifelse(DA1==1,npi1,npi2)) %>%
  select(year,DocNPI,DANPI)

# Aggregate to hospital level
Phys_with_DA <- da_phys_pairs %>%
  distinct(DocNPI,year)

# Save data
saveRDS(Phys_with_DA, file=paste0(created_data_path,"/Phys_with_DA.rds"))

