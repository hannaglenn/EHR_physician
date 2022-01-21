library(tidyverse)
library(readr)

# --------------------    Assessing the Existence of Data Assistants --------------------------
#                         Hanna Glenn, Emory University
#                         1/13/2021

# # ORDER :::::::::: 5 (Separate Analysis)

# This script analyzes the existence of data assistants in the npi data over
# the time period of my data.

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
  geom_histogram(binwidth=1, colour="black", fill="white") + ylab("Count\n") + xlab("\n Enumeration Year")
ggsave("Objects/dataassistant_histogram.pdf", width=8, height=5 , units = "in")
