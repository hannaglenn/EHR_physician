library(did)
library(dplyr)
library(ggplot2)
library(readr)
library(fixest)
library(did2s)

# ------------------------------------- ANALYSIS  ------------------------------------
#                                       Hanna Glenn, Emory University
#                                       1/31/2022

# This script reads in "Physician_Data.rds" from data5, the final dataset used in my third year paper. 
# The first portion of the script considers different potential estimators to use as the main specification for the paper.
# In this portion, I only consider "retire" as the dependent outcome and I compare estimators. 

# Read in the main data created in "data5_MDPPAS.R"
Physician_Data <- readRDS(paste0(created_data_path,"Physician_Data.rds"))

# CONSIDERING ESTIMATORS WITH RETIRE AS DEPENDENT VARIABLE ----------------------------------

# TWFE ----------------------------------------------------------------------------------------
retire_twfe <- feols(retire~ rel_m4+rel_m3+ rel_m2+ rel_0+ rel_p1+
                         rel_p2+ rel_p3 + rel_p4 | DocNPI + year, 
                       data=filter(Physician_Data,minyr_EHR>0))

reference=4
names(reference)<-"-1"

coefplot(list(retire_twfe), keep=c("-4","-3","-2","-1","0",
                                     "1", "2","3","4"),
         ref=reference, dict=c("rel_m4"="-4", "rel_m3"="-3", "rel_m2"="-2", "rel_0"="0",
                               "rel_p1"="1","rel_p2"="2","rel_p3"="3","rel_p4"="4"),
         main="",
         zero=TRUE,
         col=1:2)

wald<-wald(retire_twfe, keep=c("rel_m4","rel_m3","rel_m2"))
p <- wald$p

mtext(text=paste0("p-value for pre-trends: ",round(p,3)),side=1,line = 3, cex = 0.8, adj = 0)





# Stacked Regression (Cengiz et al 2019) ------------------------------------------------------

# This method reframes event study data into groups of sub-experiments with identical event windows that are stacked on top of each other. 
# One first chooses an event study window that is consistent for all “groups” (different treatment timing). 
# Then, treatment variables and clean controls are defined for each subgroup. Once these sub-experiments are filtered, they are 
# all stacked on top of each other. Finally, regress the dependent variable on relative year variables plus relative 
# year variables interacted with treatment, as well as ID-treatment year fixed effects. Cluster standard errors at 
# the individual level to account for duplicates in controls. The trade-off for this estimator is choosing an event window 
# and thus losing groups of treated observations. To compare this estimator with others, I will create a table of estimates based on the 
# event window chosen. 

# Event window: 1 pre and post
for (d in 2010:2015){
  subgroup <- Physician_Data %>%
    mutate(treated_unit=ifelse(minyr_EHR==d,1,0),
           clean_control=ifelse(minyr_EHR>d,1,0),
           years_included=ifelse(year>=d-1 & year<=d+1,1,0)) %>%
    mutate(inclusion=years_included*(treated_unit+clean_control)) %>%
    filter(inclusion==1)
  
  assign(paste0("subgroup_",d),subgroup)
}

stacked_data_1 <- rbind(subgroup_2010, subgroup_2011,subgroup_2012,subgroup_2013,subgroup_2014, subgroup_2015)

stacked_reg_1 <- feols(retire ~ treated_unit:rel_m1 + treated_unit:rel_0 +
                       treated_unit:rel_p1 + experience |
                       DocNPI:minyr_EHR,
                     cluster="DocNPI",
                     data=stacked_data_1)

# Event Window: 2 pre and post
for (d in 2011:2015){
  subgroup <- Physician_Data %>%
    mutate(treated_unit=ifelse(minyr_EHR==d,1,0),
           clean_control=ifelse(minyr_EHR>d,1,0),
           years_included=ifelse(year>=d-2 & year<=d+2,1,0)) %>%
    mutate(inclusion=years_included*(treated_unit+clean_control)) %>%
    filter(inclusion==1)
  
  assign(paste0("subgroup_",d),subgroup)
}

stacked_data_2 <- rbind(subgroup_2011,subgroup_2012,subgroup_2013,subgroup_2014, subgroup_2015)

stacked_reg_2 <- feols(retire ~ treated_unit:rel_m2 + treated_unit:rel_m1 + treated_unit:rel_0 +
                       treated_unit:rel_p1 + treated_unit:rel_p2 + experience |
                       DocNPI:minyr_EHR,
                     cluster="DocNPI",
                     data=stacked_data_2)

# Event Window: 3 pre and post
for (d in 2012:2014){
  subgroup <- Physician_Data %>%
    mutate(treated_unit=ifelse(minyr_EHR==d,1,0),
           clean_control=ifelse(minyr_EHR>d,1,0),
           years_included=ifelse(year>=d-3 & year<=d+3,1,0)) %>%
    mutate(inclusion=years_included*(treated_unit+clean_control)) %>%
    filter(inclusion==1)
  
  assign(paste0("subgroup_",d),subgroup)
}

stacked_data_3 <- rbind(subgroup_2012,subgroup_2013,subgroup_2014)

stacked_reg_3 <- feols(retire ~ treated_unit:rel_m3 + treated_unit:rel_m2 + treated_unit:rel_m1 + treated_unit:rel_0 +
                         treated_unit:rel_p1 + treated_unit:rel_p2 + treated_unit:rel_p3 + experience |
                         DocNPI:minyr_EHR,
                       cluster="DocNPI",
                       data=stacked_data_3)

# Create table with coefficients 
stacked_table1 <- as.data.frame(stacked_reg_1[["coefficients"]])
stacked_table1 <- cbind(rownames(stacked_table1), stacked_table1) %>%
  rename(name="rownames(stacked_table1)")
rownames(stacked_table1) <- NULL
stacked_table2 <- as.data.frame(stacked_reg_2[["coefficients"]])
stacked_table2 <- cbind(rownames(stacked_table2), stacked_table2) %>%
  rename(name="rownames(stacked_table2)")
rownames(stacked_table2) <- NULL
stacked_table3 <- as.data.frame(stacked_reg_3[["coefficients"]])
stacked_table3 <- cbind(rownames(stacked_table3), stacked_table3) %>%
  rename(name="rownames(stacked_table3)")
rownames(stacked_table3) <- NULL

stacked_table <- left_join(stacked_table3,stacked_table2, by=c("name"), all=TRUE) %>%
  left_join(stacked_table1, by=c("name"), all=TRUE) %>%
  filter(name!="experience") %>%
  rename("prepost1"='stacked_reg_1[["coefficients"]]',
         "prepost2"='stacked_reg_2[["coefficients"]]',
         "prepost3"='stacked_reg_3[["coefficients"]]') %>%
  dplyr::mutate("Event +-1"=round(prepost1,5),
         "Event +-2"=round(prepost2,5),
         "Event +-3"=round(prepost3,5)) %>%
  select(name, "Event +-1", "Event +-2", "Event +-3")


# 2SDiD  -------------------------------------------------------------------------------
#This estimator is a very clever way to get around the issues with two way fixed effects. As pointed out 
# by Gardner, “Mis-specified difference-in-differences regression models [TWFE] project heterogenous 
# treatment effects onto group and period fixed effects rather than the treatment status itself.” To address 
# this issue, a two stage procedure is suggested. In the first stage, consistent fixed effects can be estimated 
# only for treated units. These fixed effects are differenced out of the dependent variable and this new 
# constructed dependent variable is then regressed on treatment in the second stage. There is a strong
# condition for this estimator that there must be a pool of never treated units. This is a major drawback 
# for my setting since the never treated units are likely very different from treated units.

# Use Kyle Butts' 2sDiD package
retire_2sdid <- did2s(Physician_Data,
                      yname = "retire", 
                      first_stage = ~ 0 | DocNPI + year, 
                      second_stage = ~rel_m4 + rel_m3 + rel_m2 + rel_0 + rel_p1 +
                                      rel_p2 + rel_p3 + rel_p4, 
                      treatment = "anyEHR_exposed",
                      cluster_var="DocNPI",
                      bootstrap=TRUE,
                      n_bootstraps = 250)

coefplot(retire_2sdid, keep=c("-4","-3","-2","-1","0",
                                                     "1", "2","3","4"),
         ref=reference, dict=c("rel_m4"="-4", "rel_m3"="-3", "rel_m2"="-2", "rel_0"="0",
                               "rel_p1"="1","rel_p2"="2","rel_p3"="3","rel_p4"="4"),
         main="",
         zero=TRUE,
         col=1:2)



# Callaway and Sant'Anna (CS) ------------------------------------------

retire_cs <- att_gt(
              yname = "retire",                # LHS Variable
              gname = "minyr_EHR",             # First year a unit is treated. (set to 0 if never treated)
              idname = "DocNPI",               # ID
              tname = "year",                  # Time Variable
              # xformla = NULL                 # No covariates
              xformla = ~grad_year,            # Time-invariant controls
              data = dplyr::filter(
                Physician_Data,minyr_EHR>0),   # Remove never-treated units
              # data = Physician_Data
              est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
              control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
              clustervars = "DocNPI",          # Cluster Variables          
              anticipation=0                   # can set a number of years to account for anticipation effects
)
# Save p-value for pre-trends to put in footnote of table
p<-retire_cs$Wpval

# Aggregate the effects
retire_cs_dyn <- aggte(retire_cs, type = "dynamic", na.rm=T)

# Create a plot
ggdid(retire_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Average Effect of EHR Exposure on Retirement by Length of Exposure") + 
  labs(caption=paste0("\n Note: p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00")) + ylim(-.01,.01)

# Save the plot
ggsave(file="CS_retire_allEHR.pdf",path="Objects")




# OTHER RETIREMENT OUTCOMES --------------------------------------------------------------------------------

# Senior Physicians
retireold_cs <- att_gt(
  yname = "retire",                # LHS Variable
  gname = "minyr_EHR",             # First year a unit is treated. (set to 0 if never treated)
  idname = "DocNPI",               # ID
  tname = "year",                  # Time Variable
  # xformla = NULL                 # No covariates
  xformla = ~grad_year,            # Time-invariant controls
  data = dplyr::filter(
    Physician_Data,minyr_EHR>0 & max_age>=60),   # Remove never-treated units and young physicians
  # data = Physician_Data
  est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
  control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
  clustervars = "DocNPI",          # Cluster Variables          
  anticipation=0                   # can set a number of years to account for anticipation effects
)
# Save p-value for pre-trends to put in footnote of table
p<-retireold_cs$Wpval

# Aggregate the effects
retireold_cs_dyn <- aggte(retireold_cs, type = "dynamic", na.rm=T)

# Create a plot
ggdid(retireold_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Average Effect of EHR Exposure on Retirement by Length of Exposure \nSenior Physicians") + 
  labs(caption=paste0("\n Note: p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00")) + ylim(-.01,.01)

# Save the plot
ggsave(file="CS_retireold_allEHR.pdf",path="Objects")

# Young Physicians
retireyoung_cs <- att_gt(
  yname = "retire",                # LHS Variable
  gname = "minyr_EHR",             # First year a unit is treated. (set to 0 if never treated)
  idname = "DocNPI",               # ID
  tname = "year",                  # Time Variable
  # xformla = NULL                 # No covariates
  xformla = ~grad_year,            # Time-invariant controls
  data = dplyr::filter(
  Physician_Data,minyr_EHR>0 & max_age<60),   # Remove never-treated units and young physicians
  # data = Physician_Data
  est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
  control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
  clustervars = "DocNPI",          # Cluster Variables          
  anticipation=0                   # can set a number of years to account for anticipation effects
)
# Save p-value for pre-trends to put in footnote of table
p<-retireyoung_cs$Wpval

# Aggregate the effects
retireyoung_cs_dyn <- aggte(retireyoung_cs, type = "dynamic", na.rm=T)

# Create a plot
ggdid(retireyoung_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Average Effect of EHR Exposure on Retirement by Length of Exposure \nNon-Senior Physicians") + 
  labs(caption=paste0("\n Note: p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00")) + ylim(-.01,.01)

# Save the plot
ggsave(file="CS_retireyoung_allEHR.pdf",path="Objects")







# OFFICE BASED OUTCOMES -------------------------------------------------------------

# Full Sample using fraction of patients in office
office_frac_cs <- att_gt(yname = "pos_office",
                    gname = "minyr_EHR",
                    idname = "DocNPI",
                    tname = "year",
                    xformla = ~grad_year,
                    data = dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0),
                    est_method = "dr",
                    control_group = "notyettreated"
)

p<-office_frac_cs$Wpval

# Aggregate the effects
office_frac_cs_dyn <- aggte(office_frac_cs, type = "dynamic", na.rm=T)
ggdid(office_frac_cs)
# Create a plot
ggdid(office_frac_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Average Effect of EHR Exposure on Frac. of Patients in \nOffice Setting by Length of Exposure") + 
  labs(caption=paste0("\n Note: p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00")) 

# Save the plot
ggsave(file="CS_office_frac_allEHR.pdf",path="Objects")


# Old Sample using fraction of patients in office
office_fracold_cs <- att_gt(yname = "pos_office",
                         gname = "minyr_EHR",
                         idname = "DocNPI",
                         tname = "year",
                         xformla = ~grad_year,
                         data = dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0 & max_age>=60),
                         est_method = "dr",
                         control_group = "notyettreated"
)

p<-office_fracold_cs$Wpval

# Aggregate the effects
office_fracold_cs_dyn <- aggte(office_fracold_cs, type = "dynamic", na.rm=T)

# Create a plot
ggdid(office_fracold_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Average Effect of EHR Exposure on Frac. of Patients in \nOffice Setting by Length of Exposure, Senior Physicians") + 
  labs(caption=paste0("\n Note: p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00")) 

# Save the plot
ggsave(file="CS_office_fracold_allEHR.pdf",path="Objects")


# Young Sample using fraction of patients in office
office_fracyoung_cs <- att_gt(yname = "pos_office",
                            gname = "minyr_EHR",
                            idname = "DocNPI",
                            tname = "year",
                            xformla = ~grad_year,
                            data = dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0 & max_age<60),
                            est_method = "dr",
                            control_group = "notyettreated"
)

p<-office_fracyoung_cs$Wpval

# Aggregate the effects
office_fracyoung_cs_dyn <- aggte(office_fracyoung_cs, type = "dynamic", na.rm=T)

# Create a plot
ggdid(office_fracyoung_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Average Effect of EHR Exposure on Frac. of Patients in \nOffice Setting by Length of Exposure, Non-Senior Physicians") + 
  labs(caption=paste0("\n Note: p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00")) 

# Save the plot
ggsave(file="CS_office_fracyoung_allEHR.pdf",path="Objects")

# Full Sample using indicator for working in office
office_ind_cs <- att_gt(yname = "work_in_office",
                         gname = "minyr_EHR",
                         idname = "DocNPI",
                         tname = "year",
                         xformla = ~grad_year,
                         data = dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0),
                         est_method = "dr",
                         control_group = "notyettreated"
)

p<-office_ind_cs$Wpval

# Aggregate the effects
office_ind_cs_dyn <- aggte(office_ind_cs, type = "dynamic", na.rm=T)

# Create a plot
ggdid(office_ind_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Average Effect of EHR Exposure on Likelihood of Working in \nOffice Setting by Length of Exposure") + 
  labs(caption=paste0("\n Note: p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00")) 

# Save the plot
ggsave(file="CS_office_ind_allEHR.pdf",path="Objects")

# Old Sample using indicator for working in office
office_indold_cs <- att_gt(yname = "work_in_office",
                        gname = "minyr_EHR",
                        idname = "DocNPI",
                        tname = "year",
                        xformla = ~grad_year,
                        data = dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0 & max_age>=60),
                        est_method = "dr",
                        control_group = "notyettreated"
)

p<-office_indold_cs$Wpval

# Aggregate the effects
office_indold_cs_dyn <- aggte(office_indold_cs, type = "dynamic", na.rm=T)

# Create a plot
ggdid(office_indold_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Average Effect of EHR Exposure on Likelihood of Working in \nOffice Setting by Length of Exposure, Senior Physicians") + 
  labs(caption=paste0("\n Note: p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00")) 

# Save the plot
ggsave(file="CS_office_indold_allEHR.pdf",path="Objects")

# Young Sample using indicator for working in office
office_indyoung_cs <- att_gt(yname = "work_in_office",
                        gname = "minyr_EHR",
                        idname = "DocNPI",
                        tname = "year",
                        xformla = ~grad_year,
                        data = dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0 & max_age<60),
                        est_method = "dr",
                        control_group = "notyettreated"
)

p<-office_indyoung_cs$Wpval

# Aggregate the effects
office_indyoung_cs_dyn <- aggte(office_indyoung_cs, type = "dynamic", na.rm=T)

# Create a plot
ggdid(office_indyoung_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Average Effect of EHR Exposure on Likelihood of Working in \nOffice Setting by Length of Exposure, Non-Senior Physicians") + 
  labs(caption=paste0("\n Note: p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00")) 

# Save the plot
ggsave(file="CS_office_indyoung_allEHR.pdf",path="Objects")




## PRODUCTIVITY ####

# Full sample total claim count
claim_es <- att_gt(yname = "claim_count_total",
                            gname = "minyr_EHR",
                            idname = "DocNPI",
                            tname = "year",
                            xformla = ~age,
                            data = dplyr::filter(Physician_Data,ever_retire==0 & ever_work_in_office==0),
                            est_method = "reg",
                            control_group = "notyettreated"
)

ggdid(claim_es)

claim_es_dyn <- aggte(claim_es, type = "dynamic")
ggdid(claim_es_dyn)

# Old sample total claim count
old_claim_es <- att_gt(yname = "claim_count_total",
                   gname = "minyr_EHR",
                   idname = "DocNPI",
                   tname = "year",
                   xformla = ~age,
                   data = dplyr::filter(Physician_Data,ever_retire==0 & ever_work_in_office==0 & age>50),
                   est_method = "reg",
                   control_group = "notyettreated"
)

ggdid(old_claim_es)

old_claim_es_dyn <- aggte(old_claim_es, type = "dynamic")
ggdid(old_claim_es_dyn)





