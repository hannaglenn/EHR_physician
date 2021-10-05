library(tidyverse)
library(did)
library(readr)

## This script houses the main analysis for my third year paper.
# The data is developed in another script called Final_Pairs_Variables

Final_Pairs_Variables <- read_rds(paste0(created_data_path,"Final_Pairs_Variables.rds"))

# Let's give Sant'Anna and Callaway (2020) a shot...

did_data <- Final_Pairs_Variables %>%
  filter(!is.na(firstyear_usesEHR)) %>%
  filter(!is.na(beds)) %>%
  filter(firstyear_usesEHR!=2009)


did1 <- att_gt(yname="samedaycount",tname="year",idname="ID",gname="firstyear_usesEHR", 
               xformla= ~ beds + days_hosp_operating + female, data=did_data,
               control_group="notyettreated", clustervars="DocNPI")

did2 <- att_gt(yname="share_samedaycount",tname="year",idname="ID",gname="firstyear_usesEHR", 
               xformla= ~ beds + days_hosp_operating + female, data=did_data,
               control_group="notyettreated", clustervars="DocNPI")
ggdid(did2,ylim = c(-5,5))
summary(did1)

ggsave("objects/test.pdf", width=9, height=7, units="in")

# Look at whether EHR use affects total same day count
phys_did_data <- did_data %>%
  distinct(DocNPI, year, totalsharedpatients, .keep_all=T)

did_totalcount <- att_gt(yname="totalsharedpatients",tname="year",idname="DocNPI",gname="firstyear_usesEHR",
               xformla= ~ beds + days_hosp_operating + female + yrssince_grad, data=phys_did_data,
               control_group="notyettreated", clustervars="HospNPI")
did_totalcount_group <- aggte(did_totalcount,type="group",na.rm=T)
did_totalcount_dynamic <- aggte(did_totalcount,type="dynamic",na.rm=T)
ggdid(did_totalcount_dynamic)

# Look at whether EHR use impacts working variables
did_working <- att_gt(yname="working",tname="year",idname="DocNPI",gname="firstyear_usesEHR",
                         xformla= ~ beds + days_hosp_operating + female + yrssince_grad, data=phys_did_data,
                         control_group="notyettreated")
did_working_group <- aggte(did_working,type="group",na.rm=T)
did_working_dynamic <- aggte(did_working,type="dynamic",na.rm=T)
ggdid(did_working_dynamic)

# Do the same thing but limit the sample to old physician
old_phys_did_data <- phys_did_data %>%
  filter(yrssince_grad>25)

did_working_old <- att_gt(yname="working",tname="year",idname="DocNPI",gname="firstyear_usesEHR",
                      xformla= ~ beds + days_hosp_operating + female, data=old_phys_did_data,
                      control_group="notyettreated")
did_working_old_group <- aggte(did_working_old,type="group",na.rm=T)
did_working_old_dynamic <- aggte(did_working_old,type="dynamic",na.rm=T)
ggdid(did_working_old_dynamic)


