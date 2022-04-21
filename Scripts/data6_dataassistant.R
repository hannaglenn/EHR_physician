library(tidyverse)
library(readr)
library(showtext)

font_add_google("Cormorant Garamond", "corm")

font_add("lm","C:/Users/hkagele/Downloads/Latin-Modern-Roman/lmroman10-regular.otf")

## Automatically use showtext to render text
showtext_auto()


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
                enum_date='Provider Enumeration Date') %>%
  select(npi, t_code, enum_date) %>%
  filter(t_code=="247000000X" | t_code=="246YR1600X" | t_code=="246Y00000X")

npi.taxcodes <- npi.taxcodes %>%
  separate(enum_date, sep="/", into = c("month", "day", "enum_year"))

npi.taxcodes <- npi.taxcodes %>%
   mutate(enum_year=as.numeric(enum_year))


ggplot(npi.taxcodes, aes(x=enum_year)) + 
  geom_histogram(binwidth=1, colour="black", fill="white") + ylab("Count\n") + xlab("\nNPI Enumeration Year") +
  theme(text=element_text(size=17,family="lm"))
ggsave("Objects/dataassistant_histogram.pdf", width=8, height=5 , units = "in")


## Create database of hospitals who work with Data Assistants
# Read in npi datasets and combine into one------------------
hosp_npidata <- read_rds(paste0(created_data_path,"hosp_npidata.rds"))

hosp_npidata <- hosp_npidata %>%
  dplyr::mutate(DA=0)

da_npidata <- npi.taxcodes %>%
  select(npi, t_code) %>%
  mutate(PCP=0, hospital=0, other_type=0, DA=1)

temp_npidata <- rbind(da_npidata, hosp_npidata)



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
    dplyr::rename(DA1=DA,hospital1=hospital) 
  
  SP.year <- SP.year %>%
    left_join(temp_npidata, by=c("npi2"="npi")) %>%
    dplyr::rename(DA2="DA",hospital2="hospital") %>%
    select(npi1,npi2,year,DA1,DA2,hospital1,hospital2)
  
  # filter so that all is left is hospital-data assistant pairs
  SP.year <- SP.year %>%
    filter((DA1==1 & hospital2==1) | (hospital1==1 & DA2==1))
  
  assign(paste0("PSPD",t),SP.year)
}

da_hosp_pairs <- rbind(PSPD2009, PSPD2010, PSPD2011, PSPD2012, PSPD2013, PSPD2014, PSPD2015)
# 75 observations

# Change to DA and hospital npis instead of npi1 and npi2
da_hosp_pairs <- da_hosp_pairs %>%
  mutate(DA1=ifelse(is.na(DA1),0,DA1), DA2=ifelse(is.na(DA2),0,DA2),
         hospital1=ifelse(is.na(hospital1),0,hospital1), hospital2=ifelse(is.na(hospital2),0,hospital2)) %>%
  mutate(HospNPI=ifelse(hospital1==1,npi1,npi2),
         DANPI=ifelse(DA1==1,npi1,npi2)) %>%
  select(year,HospNPI,DANPI)

# Aggregate to hospital level
Hosp_with_DA <- da_hosp_pairs %>%
  distinct(HospNPI,year)

# Save data
saveRDS(Hosp_with_DA, file=paste0(created_data_path,"/Hosp_with_DA.rds"))



