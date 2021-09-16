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



# Labor Variables ---------------------------------------------------------------------------------------------


# Filter out any doctors that never work closely with hospitals in the entire dataset
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  group_by(year, DocNPI) %>%
  mutate(totalsharedpatients=sum(samedaycount)) %>%
  ungroup() %>%
  group_by(DocNPI) %>%
  mutate(totalsharedpatients_allyears=sum(totalsharedpatients)) %>%
  filter(totalsharedpatients_allyears>30)


# Create share of samedaycount variable
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  group_by(DocNPI,year) %>%
  mutate(share_samedaycount=samedaycount/sum(samedaycount,na.rm=T)) %>%
  mutate(share_samedaycount=ifelse(is.na(share_samedaycount),0,share_samedaycount)) %>%
  ungroup()

# Concentration
Final_Pairs_Variables <- Final_Pairs_Variables %>% ungroup() %>%
  mutate(share_squared=(share_samedaycount*100)^2) %>%
  group_by(DocNPI, year) %>%
  mutate(hhi=sum(share_squared))





# EHR Variables -----------------------------------------------------------------------------------------------


# Create an Indicator for whether the hospital uses an EHR fully (=2 on survey answer)
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  mutate(usesEHR=ifelse(EHLTH==2,1,ifelse(is.na(EHLTH),NA,0))) 

# Create dataset to fill in 2009 or 2015 conditionally
fill_in <- Final_Pairs_Variables %>%
  filter((year==2010 & usesEHR==0) | (year==2014 & usesEHR==1)) %>%
  mutate(change2009=ifelse(year==2010,1,0),
         change2015=ifelse(year==2014,1,0)) %>%
  select(HospNPI,DocNPI,year,usesEHR,change2009,change2015) %>%
  distinct(HospNPI,year,usesEHR,change2009,change2015)

fill_in <- fill_in %>%
  group_by(HospNPI) %>%
  mutate(change2009=ifelse(sum(change2009)>0,1,0),
         change2015=ifelse(sum(change2015)>0,1,0)) %>%
  ungroup() %>%
  distinct(HospNPI,change2009,change2015)

# Merge it back to original
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  left_join(fill_in,by="HospNPI")

# Fill in the qualifying missing values
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  mutate(usesEHR=ifelse(year==2009 & change2009==1 & is.na(usesEHR),0,usesEHR)) %>%
  mutate(usesEHR=ifelse(year==2015 & change2015==1 & is.na(usesEHR),1,usesEHR)) %>%
  select(-change2009, -change2015) %>%
  ungroup()


# Create indicators for using EHR for documentation or decision making
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  mutate(usesEHRdoc=if_else(usesEHR==0,0,if_else(usesEHR==1 & 
                                                   (documentation_index>24 | is.na(documentation_index)),0,1)),
         usesEHRdec=if_else(usesEHR==0,0,if_else(usesEHR==1 & 
                                                   (decision_index>21 | is.na(decision_index)),0,1)))


# Create variable for when a physician was first exposed to an EHR at their main hospital
  # Find out the first year doctors have positive working variables
firstyear_data <- Final_Pairs_Variables %>%
  ungroup() %>%
  filter(totalsharedpatients>0) %>%
  group_by(DocNPI) %>%
  mutate(firstyear_working=min(year)) %>%
  distinct(DocNPI,.keep_all=T) %>%
  select(DocNPI,firstyear_working) %>%
  ungroup()

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
  ungroup() %>%
  mutate(mainhosp=ifelse(HospNPI==mainhosp_NPI,1,0)) 
  
  # I need an indicator for if main hospital in that year uses an EHR (by AHA measure)
mainhosp_EHR <- Final_Pairs_Variables %>%
  filter(mainhosp==1) %>%
  mutate(mainhosp_EHR=ifelse(usesEHR==1,1,ifelse(is.na(usesEHR),NA,0))) %>%
  select(HospNPI,DocNPI,year,mainhosp_NPI,mainhosp,mainhosp_EHR)

  # Merge it back to main data
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  left_join(mainhosp_EHR,by=c("DocNPI","HospNPI","year","mainhosp_NPI","mainhosp"))

  # Fill in for the hospitals that are not main
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  group_by(DocNPI,year) %>%
  fill(mainhosp_EHR,.direction="up") %>%
  fill(mainhosp_EHR,.direction="down") %>%
  ungroup()

  # Now I need to create a variable that shows the first year a physician was exposed at their main hospital
firstyear_mainhosp_usesEHR <- Final_Pairs_Variables %>%
  filter(mainhosp_EHR==1) %>%
  group_by(DocNPI) %>%
  mutate(firstyear_mainhosp_usesEHR=min(year)) %>%
  ungroup() %>%
  distinct(DocNPI,firstyear_mainhosp_usesEHR)

  # Merge the minimum years back to original dataset
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  left_join(firstyear_mainhosp_usesEHR,by="DocNPI")
  
  #Fix NAs that represent a physician not being exposed to EHRs ever in their main hospital
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  group_by(DocNPI,HospNPI) %>%
  mutate(sum_mainhosp_EHR=sum(mainhosp_EHR)) %>%
  mutate(main_neveruses_EHR=ifelse(sum(mainhosp_EHR)==0,1,NA)) 

Final_Pairs_Variables <- Final_Pairs_Variables %>%
    mutate(firstyear_mainhosp_usesEHR=
             ifelse(is.na(firstyear_mainhosp_usesEHR) & main_neveruses_EHR==1,0,firstyear_mainhosp_usesEHR))

# Create relative year variable
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  mutate(rel_expandyear=ifelse(firstyear_mainhosp_usesEHR==0,-1,ifelse(firstyear_mainhosp_usesEHR>0,year-firstyear_mainhosp_usesEHR,NA)))

# Descriptive Variables --------------------------------------------------------------------------------------------

# Number of Hospitals Worked With
count <- Final_Pairs_Variables %>%
  ungroup() %>%
  count(year,DocNPI,name="num_hospitals")

Final_Pairs_Variables <- Final_Pairs_Variables %>%
  left_join(count,by=c("year","DocNPI"))


# Number of systems worked with
count_sys <- Final_Pairs_Variables %>%
  ungroup() %>%
  distinct(DocNPI,year,SystemID) %>%
  count(DocNPI,year,name="num_systems")

Final_Pairs_Variables <- Final_Pairs_Variables %>%
  left_join(count_sys,by=c("year","DocNPI"))

# Gender indicator
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  mutate(female=ifelse(gender=="F",1,ifelse(gender=="M",0,NA)))

# Years since graduation
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  mutate(yrssince_grad=year-grad_year)


# More Labor Variables --------------------------------------------------------------------------------------------

# Main hospital's percent share
Final_Pairs_Variables <- Final_Pairs_Variables %>%
  mutate(mainhosp_share=ifelse(mainhosp==1,share_samedaycount,NA)) %>%
  group_by(DocNPI,year) %>%
  fill(mainhosp_share,.direction="down") %>%
  fill(mainhosp_share, .direction="up") %>%
  ungroup()


# Save the Data ---------------------------------------------------------------------------------------------------
saveRDS(Final_Pairs_Variables,file="Final_Pairs_Variables.rds")
