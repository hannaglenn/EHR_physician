library(tidyverse)
library(readr)





# --------------------    Build Extra Variables Onto Main Hospital-Physician Pair Data --------------------------
#                         Hanna Glenn, Emory University
#                         9/14/2021


# This script expands the dataset "Final_Pairs.rds" to include more characteristics that are built using 
# variables already present in the data. The script is split into EHR characteristics and Labor characteristics. 
# The final dataset is called "Final_Pairs_Variables.rds"

Final_Pairs_Variables <- read_rds(paste0(created_data_path,"Final_Pairs.rds"))

# Drop anyone that graduated medical school 2009 or later
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  filter(grad_year<2009)


# Create share of samedaycount variable
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  group_by(DocNPI,year) %>%
  mutate(share_samedaycount=samedaycount/sum(samedaycount,na.rm=T)) %>%
  mutate(share_samedaycount=ifelse(is.na(share_samedaycount),0,share_samedaycount)) %>%
  ungroup()




# EHR Variables -----------------------------------------------------------------------------------------------


# Create an Indicator for whether the hospital uses an EHR fully (=2 on survey answer)
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  mutate(usesEHR=ifelse(EHLTH==2,1,ifelse(is.na(EHLTH),NA,0)))


# Create indicators for using EHR for documentation or decision making
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  mutate(usesEHRdoc=if_else(usesEHR==0,0,if_else(usesEHR==1 & 
                                                   (documentation_index>24 | is.na(documentation_index)),0,1)),
         usesEHRdec=if_else(usesEHR==0,0,if_else(usesEHR==1 & 
                                                   (decision_index>21 | is.na(decision_index)),0,1)))


# Create variable for when a physician was first exposed to an EHR at their main hospital
  # Filter out any doctors that never work closely with hospitals in the entire dataset
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  group_by(year, DocNPI) %>%
  mutate(totalsharedpatients=sum(samedaycount)) %>%
  ungroup() %>%
  filter(sum(totalsharedpatients)!=0)

  # Find out the first year doctors have positive working variables
firstyear_data <- Final_Pairs_Variables %>%
  group_by(DocNPI,year) %>%
  filter(sum(samedaycount)>0) %>%
  ungroup() %>%
  group_by(DocNPI) %>%
  mutate(firstyear_working=min(year)) %>%
  distinct(DocNPI,.keep_all=T) %>%
  select(DocNPI,firstyear_working)

  # Join minimum year back to original dataset
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  left_join(firstyear_data, by="DocNPI") 

  # Record NPI of main hospital in minimum year and create indicator for main hospital
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  group_by(DocNPI,year) %>%
  mutate(mainhosp_NPI=ifelse(year==firstyear_working & share_samedaycount==max(share_samedaycount),HospNPI,NA)) %>%
  ungroup() %>%
  group_by(DocNPI) %>%
  fill(mainhosp_NPI,.direction="up") %>%
  fill(mainhosp_NPI,.direction="down") %>%
  mutate(mainhosp=ifelse(HospNPI==mainhosp_NPI,1,0)) %>%
  ungroup()
  
# I need an indicator for if main hospital in that year uses an EHR (by AHA measure)
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  group_by(DocNPI,year) %>%
  mutate(mainhosp_usesEHR=ifelse(main,1,0)) %>%
  ungroup()

# Now I need to create a variable that shows the first year a physician was exposed
firstyear_anyhosp_usesEHR <- Final_Pairs_Variables %>%
  filter(anyhosp_usesEHR==1) %>%
  group_by(DocNPI) %>%
  mutate(firstyear_anyhosp_usesEHR=min(year)) %>%
  ungroup() %>%
  distinct(DocNPI,firstyear_anyhosp_usesEHR)

# Merge the minimum years back to original dataset
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  left_join(firstyear_anyhosp_usesEHR,by="DocNPI")
  
#Fix NAs that represent a physician not being exposed to EHRs
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  mutate(firstyear_anyhosp_usesEHR)




# Labor Variables ---------------------------------------------------------------------------------------------