library(showtext)
library(ggplot2)
library(did)
library(ggpubr)




### --------------------  Sensitivity Analysis --------------------------
#                         Hanna Glenn, Emory University
#                         4/19/2022

# # ORDER :::::::::: 6

font_add_google("Cormorant Garamond", "corm")

font_add("lm","C:/Users/hkagele/Downloads/Latin-Modern-Roman/lmroman10-regular.otf")

## Automatically use showtext to render text
showtext_auto()

# This script reads in "Physician_Data.rds" from data5, the final dataset used in my third year paper. 
# The first portion of the script considers different potential estimators to use as the main specification for the paper.
# In this portion, I only consider "retire" as the dependent outcome and I compare estimators. 

# Read in the main data created in "data5_MDPPAS.R"
Physician_Data <- readRDS(paste0(created_data_path,"Physician_Data.rds"))

### LOW INTEGRATION DEFINITION #####################################################################################
 # Retire

# Senior Physicians
retire_LI <- att_gt(
  yname = "retire",                # LHS Variable
  gname = "minyr_EHR_int",             # First year a unit is treated. (set to 0 if never treated)
  idname = "DocNPI",               # ID
  tname = "year",                  # Time Variable
  # xformla = NULL                 # No covariates
  xformla = ~grad_year,            # Time-invariant controls
  data = dplyr::filter(
    Physician_Data,minyr_EHR_int>0),   # Remove never-treated units and young physicians
  # data = Physician_Data
  est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
  control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
  clustervars = "DocNPI",          # Cluster Variables          
  anticipation=0                   # can set a number of years to account for anticipation effects
)

# OTHER RETIREMENT OUTCOMES --------------------------------------------------------------------------------

# Senior Physicians
retireold_LI <- att_gt(
  yname = "retire",                # LHS Variable
  gname = "minyr_EHR_int",             # First year a unit is treated. (set to 0 if never treated)
  idname = "DocNPI",               # ID
  tname = "year",                  # Time Variable
  # xformla = NULL                 # No covariates
  xformla = ~grad_year,            # Time-invariant controls
  data = dplyr::filter(
    Physician_Data,minyr_EHR_int>0 & max_age>=60),   # Remove never-treated units and young physicians
  # data = Physician_Data
  est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
  control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
  clustervars = "DocNPI",          # Cluster Variables          
  anticipation=0                   # can set a number of years to account for anticipation effects
)



# Young Physicians
retireyoung_LI <- att_gt(
  yname = "retire",                # LHS Variable
  gname = "minyr_EHR_int",             # First year a unit is treated. (set to 0 if never treated)
  idname = "DocNPI",               # ID
  tname = "year",                  # Time Variable
  # xformla = NULL                 # No covariates
  xformla = ~grad_year,            # Time-invariant controls
  data = dplyr::filter(
    Physician_Data,minyr_EHR_int>0 & max_age<60),   # Remove never-treated units and young physicians
  # data = Physician_Data
  est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
  control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
  clustervars = "DocNPI",          # Cluster Variables          
  anticipation=0                   # can set a number of years to account for anticipation effects
)






# OFFICE BASED OUTCOMES -------------------------------------------------------------

# Full Sample using fraction of patients in office
office_frac_LI <- att_gt(yname = "pos_office",
                         gname = "minyr_EHR_int",
                         idname = "DocNPI",
                         tname = "year",
                         xformla = ~grad_year,
                         data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0),
                         est_method = "dr",
                         control_group = "notyettreated"
)





# Old Sample using fraction of patients in office
office_fracold_LI <- att_gt(yname = "pos_office",
                            gname = "minyr_EHR_int",
                            idname = "DocNPI",
                            tname = "year",
                            xformla = ~grad_year,
                            data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age>=60),
                            est_method = "dr",
                            control_group = "notyettreated"
)



# Young Sample using fraction of patients in office
office_fracyoung_LI <- att_gt(yname = "pos_office",
                              gname = "minyr_EHR_int",
                              idname = "DocNPI",
                              tname = "year",
                              xformla = ~grad_year,
                              data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age<60),
                              est_method = "dr",
                              control_group = "notyettreated"
)


# Full Sample using indicator for working in office
office_ind_LI <- att_gt(yname = "work_in_office",
                        gname = "minyr_EHR_int",
                        idname = "DocNPI",
                        tname = "year",
                        xformla = ~grad_year,
                        data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0),
                        est_method = "dr",
                        control_group = "notyettreated"
)




# Old Sample using indicator for working in office
office_indold_LI <- att_gt(yname = "work_in_office",
                           gname = "minyr_EHR_int",
                           idname = "DocNPI",
                           tname = "year",
                           xformla = ~grad_year,
                           data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age>=60),
                           est_method = "dr",
                           control_group = "notyettreated"
)




# Young Sample using indicator for working in office
office_indyoung_LI <- att_gt(yname = "work_in_office",
                             gname = "minyr_EHR_int",
                             idname = "DocNPI",
                             tname = "year",
                             xformla = ~grad_year,
                             data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age<60),
                             est_method = "dr",
                             control_group = "notyettreated"
)




## CHANGE ZIP CODE ###
# Full Sample using indicator for working in office
zip_LI <- att_gt(yname = "change_zip",
                 gname = "minyr_EHR_int",
                 idname = "DocNPI",
                 tname = "year",
                 xformla = ~grad_year,
                 data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0),
                 est_method = "dr",
                 control_group = "notyettreated"
)



# Old Sample using indicator for working in office
zipold_LI <- att_gt(yname = "change_zip",
                    gname = "minyr_EHR_int",
                    idname = "DocNPI",
                    tname = "year",
                    xformla = ~grad_year,
                    data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age>=60),
                    est_method = "dr",
                    control_group = "notyettreated"
)




# Young Sample using indicator for working in office
zipyoung_LI <- att_gt(yname = "change_zip",
                      gname = "minyr_EHR_int",
                      idname = "DocNPI",
                      tname = "year",
                      xformla = ~grad_year,
                      data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age<60),
                      est_method = "dr",
                      control_group = "notyettreated"
)







## PRODUCTIVITY ####

# Full Sample using indicator for working in office
patient_LI <- att_gt(yname = "npi_unq_benes",
                     gname = "minyr_EHR_int",
                     idname = "DocNPI",
                     tname = "year",
                     xformla = ~grad_year,
                     data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0),
                     est_method = "dr",
                     control_group = "notyettreated"
)




# Old Sample using indicator for working in office
patientold_LI <- att_gt(yname = "npi_unq_benes",
                        gname = "minyr_EHR_int",
                        idname = "DocNPI",
                        tname = "year",
                        xformla = ~grad_year,
                        data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age>=60),
                        est_method = "dr",
                        control_group = "notyettreated"
)




# Young Sample using indicator for working in office
patientyoung_LI <- att_gt(yname = "npi_unq_benes",
                          gname = "minyr_EHR_int",
                          idname = "DocNPI",
                          tname = "year",
                          xformla = ~grad_year,
                          data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age<60),
                          est_method = "dr",
                          control_group = "notyettreated"
)




## CLAIM COUNT
# Full Sample 
claim_LI <- att_gt(yname = "claim_count_total",
                   gname = "minyr_EHR_int",
                   idname = "DocNPI",
                   tname = "year",
                   xformla = ~grad_year,
                   data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0),
                   est_method = "dr",
                   control_group = "notyettreated"
)


# Old Sample using indicator for working in office
claimold_LI <- att_gt(yname = "claim_count_total",
                      gname = "minyr_EHR_int",
                      idname = "DocNPI",
                      tname = "year",
                      xformla = ~grad_year,
                      data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age>=60),
                      est_method = "dr",
                      control_group = "notyettreated"
)



# Young Sample using indicator for working in office
claimyoung_LI <- att_gt(yname = "claim_count_total",
                        gname = "minyr_EHR_int",
                        idname = "DocNPI",
                        tname = "year",
                        xformla = ~grad_year,
                        data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age<60),
                        est_method = "dr",
                        control_group = "notyettreated"
)


# Create a dataframe of all singel value ATT plus SE and CI ---------------------------------------
single_ATT_values_LI <- data.frame(matrix(ncol = 7, nrow = 0))

#provide column names
colnames(single_ATT_values_LI) <- c('Variable', 'ATT', 'SE', 'Lower', 'Upper', 'Age', 'Specification')

# Retire
retire_cs_simple <- aggte(retire_LI, type = "simple")
retire_list <- c("Retire", 
                 round(retire_cs_simple$overall.att,5), 
                 round(retire_cs_simple$overall.se, 5),
                 round(retire_cs_simple$overall.att-(1.959*retire_cs_simple$overall.se),5),
                 round(retire_cs_simple$overall.att+(1.959*retire_cs_simple$overall.se),5),
                 "Any",
                 "Low-Integration Hosp.")

single_ATT_values_LI[nrow(single_ATT_values_LI) + 1,] <- retire_list

# Retire Old 
retireold_cs_simple <- aggte(retireold_LI, type = "simple")
retireold_list <- c("Retire", 
                    round(retireold_cs_simple$overall.att,5), 
                    round(retireold_cs_simple$overall.se,5),
                    round(retireold_cs_simple$overall.att-(1.959*retireold_cs_simple$overall.se),5),
                    round(retireold_cs_simple$overall.att+(1.959*retireold_cs_simple$overall.se),5),
                    ">= 60",
                    "Low-Integration Hosp.")

single_ATT_values_LI[nrow(single_ATT_values_LI) + 1,] <- retireold_list

# Retire Young
retireyoung_cs_simple <- aggte(retireyoung_LI, type = "simple")
retireyoung_list <- c("Retire", 
                      round(retireyoung_cs_simple$overall.att,5), 
                      round(retireyoung_cs_simple$overall.se,5),
                      round(retireyoung_cs_simple$overall.att-(1.959*retireyoung_cs_simple$overall.se),5),
                      round(retireyoung_cs_simple$overall.att+(1.959*retireyoung_cs_simple$overall.se),5),
                      "< 60",
                      "Low-Integration Hosp.")

single_ATT_values_LI[nrow(single_ATT_values_LI) + 1,] <- retireyoung_list

# Office Frac
office_frac_cs_simple <- aggte(office_frac_LI, type = "simple")
office_frac_list <- c("Frac. Patients in Office", 
                      round(office_frac_cs_simple$overall.att,5), 
                      round(office_frac_cs_simple$overall.se,5),
                      round(office_frac_cs_simple$overall.att-(1.959*office_frac_cs_simple$overall.se),5),
                      round(office_frac_cs_simple$overall.att+(1.959*office_frac_cs_simple$overall.se),5),
                      "Any",
                      "Low-Integration Hosp.")

single_ATT_values_LI[nrow(single_ATT_values_LI) + 1,] <- office_frac_list

# Office Frac Old
office_fracold_cs_simple <- aggte(office_fracold_LI, type = "simple")
office_fracold_list <- c("Frac. Patients in Office", 
                         round(office_fracold_cs_simple$overall.att,5), 
                         round(office_fracold_cs_simple$overall.se,5),
                         round(office_fracold_cs_simple$overall.att-(1.959*office_fracold_cs_simple$overall.se),5),
                         round(office_fracold_cs_simple$overall.att+(1.959*office_fracold_cs_simple$overall.se),5),
                         ">= 60",
                         "Low-Integration Hosp.")

single_ATT_values_LI[nrow(single_ATT_values_LI) + 1,] <- office_fracold_list

# Office Frac Young
office_fracyoung_cs_simple <- aggte(office_fracyoung_LI, type = "simple")
office_fracyoung_list <- c("Frac. Patients in Office", 
                           round(office_fracyoung_cs_simple$overall.att,5), 
                           round(office_fracyoung_cs_simple$overall.se,5),
                           round(office_fracyoung_cs_simple$overall.att-(1.959*office_fracyoung_cs_simple$overall.se),5),
                           round(office_fracyoung_cs_simple$overall.att+(1.959*office_fracyoung_cs_simple$overall.se),5),
                           "< 60",
                           "Low-Integration Hosp.")

single_ATT_values_LI[nrow(single_ATT_values_LI) + 1,] <- office_fracyoung_list

# Office Indicator
office_ind_cs_simple <- aggte(office_ind_LI, type = "simple")
office_ind_list <- c("Prob. Working in Office", 
                     round(office_ind_cs_simple$overall.att,5), 
                     round(office_ind_cs_simple$overall.se,5),
                     round(office_ind_cs_simple$overall.att-(1.959*office_ind_cs_simple$overall.se),5),
                     round(office_ind_cs_simple$overall.att+(1.959*office_ind_cs_simple$overall.se),5),
                     "Any",
                     "Low-Integration Hosp.")

single_ATT_values_LI[nrow(single_ATT_values_LI) + 1,] <- office_ind_list

# Office Indicator Old
office_indold_cs_simple <- aggte(office_indold_LI, type = "simple")
office_indold_list <- c("Prob. Working in Office", 
                        round(office_indold_cs_simple$overall.att,5), 
                        round(office_indold_cs_simple$overall.se,5),
                        round(office_indold_cs_simple$overall.att-(1.959*office_indold_cs_simple$overall.se),5),
                        round(office_indold_cs_simple$overall.att+(1.959*office_indold_cs_simple$overall.se),5),
                        ">= 60",
                        "Low-Integration Hosp.")

single_ATT_values_LI[nrow(single_ATT_values_LI) + 1,] <- office_indold_list

# Office Indicator Young
office_indyoung_cs_simple <- aggte(office_indyoung_LI, type = "simple")
office_indyoung_list <- c("Prob. Working in Office", 
                          round(office_indyoung_cs_simple$overall.att,5), 
                          round(office_indyoung_cs_simple$overall.se,5),
                          round(office_indyoung_cs_simple$overall.att-(1.959*office_indyoung_cs_simple$overall.se),5),
                          round(office_indyoung_cs_simple$overall.att+(1.959*office_indyoung_cs_simple$overall.se),5),
                          "< 60",
                          "Low-Integration Hosp.")

single_ATT_values_LI[nrow(single_ATT_values_LI) + 1,] <- office_indyoung_list

# Patient Count
patient_cs_simple <- aggte(patient_LI, type = "simple")
patient_list <- c("Number Patients", 
                  round(patient_cs_simple$overall.att,5), 
                  round(patient_cs_simple$overall.se,5),
                  round(patient_cs_simple$overall.att-(1.959*patient_cs_simple$overall.se),5),
                  round(patient_cs_simple$overall.att+(1.959*patient_cs_simple$overall.se),5),
                  "Any",
                  "Low-Integration Hosp.")

single_ATT_values_LI[nrow(single_ATT_values_LI) + 1,] <- patient_list

# Patient Old
patientold_cs_simple <- aggte(patientold_LI, type = "simple")
patientold_list <- c("Number Patients", 
                     round(patientold_cs_simple$overall.att,5), 
                     round(patientold_cs_simple$overall.se,5),
                     round(patientold_cs_simple$overall.att-(1.959*patientold_cs_simple$overall.se),5),
                     round(patientold_cs_simple$overall.att+(1.959*patientold_cs_simple$overall.se),5),
                     ">= 60",
                     "Low-Integration Hosp.")

single_ATT_values_LI[nrow(single_ATT_values_LI) + 1,] <- patientold_list

# Patient Young
patientyoung_cs_simple <- aggte(patientyoung_LI, type = "simple")
patientyoung_list <- c("Number Patients", 
                       round(patientyoung_cs_simple$overall.att,5), 
                       round(patientyoung_cs_simple$overall.se,5),
                       round(patientyoung_cs_simple$overall.att-(1.959*patientyoung_cs_simple$overall.se),5),
                       round(patientyoung_cs_simple$overall.att+(1.959*patientyoung_cs_simple$overall.se),5),
                       "< 60",
                       "Low-Integration Hosp.")

single_ATT_values_LI[nrow(single_ATT_values_LI) + 1,] <- patientyoung_list

# Claim Count
claim_cs_simple <- aggte(claim_LI, type = "simple")
claim_list <- c("Claim Count", 
                round(claim_cs_simple$overall.att,5), 
                round(claim_cs_simple$overall.se,5),
                round(claim_cs_simple$overall.att-(1.959*claim_cs_simple$overall.se),5),
                round(claim_cs_simple$overall.att+(1.959*claim_cs_simple$overall.se),5),
                "Any",
                "Low-Integration Hosp.")

single_ATT_values_LI[nrow(single_ATT_values_LI) + 1,] <- claim_list

# Claim Count Old
claimold_cs_simple <- aggte(claimold_LI, type = "simple")
claimold_list <- c("Claim Count", 
                   round(claimold_cs_simple$overall.att,5), 
                   round(claimold_cs_simple$overall.se,5),
                   round(claimold_cs_simple$overall.att-(1.959*claimold_cs_simple$overall.se),5),
                   round(claimold_cs_simple$overall.att+(1.959*claimold_cs_simple$overall.se),5),
                   ">= 60",
                   "Low-Integration Hosp.")

single_ATT_values_LI[nrow(single_ATT_values_LI) + 1,] <- claimold_list

# Claim Count
claimyoung_cs_simple <- aggte(claimyoung_LI, type = "simple")
claimyoung_list <- c("Claim Count", 
                     round(claimyoung_cs_simple$overall.att,5), 
                     round(claimyoung_cs_simple$overall.se,5),
                     round(claimyoung_cs_simple$overall.att-(1.959*claimyoung_cs_simple$overall.se),5),
                     round(claimyoung_cs_simple$overall.att+(1.959*claimyoung_cs_simple$overall.se),5),
                     "< 60",
                     "Low-Integration Hosp.")

single_ATT_values_LI[nrow(single_ATT_values_LI) + 1,] <- claimyoung_list

# Zip
zip_cs_simple <- aggte(zip_LI, type = "simple")
zip_list <- c("Prob. Change Zip", 
              round(zip_cs_simple$overall.att,5), 
              round(zip_cs_simple$overall.se,5),
              round(zip_cs_simple$overall.att-(1.959*zip_cs_simple$overall.se),5),
              round(zip_cs_simple$overall.att+(1.959*zip_cs_simple$overall.se),5),
              "Any",
              "Low-Integration Hosp.")

single_ATT_values_LI[nrow(single_ATT_values_LI) + 1,] <- zip_list

# Zip Old
zipold_cs_simple <- aggte(zipold_LI, type = "simple")
zipold_list <- c("Prob. Change Zip", 
                 round(zipold_cs_simple$overall.att,5), 
                 round(zipold_cs_simple$overall.se,5),
                 round(zipold_cs_simple$overall.att-(1.959*zipold_cs_simple$overall.se),5),
                 round(zipold_cs_simple$overall.att+(1.959*zipold_cs_simple$overall.se),5),
                 ">= 60",
                 "Low Integration Hosp.")

single_ATT_values_LI[nrow(single_ATT_values_LI) + 1,] <- zipold_list

# Zip Young
zipyoung_cs_simple <- aggte(zipyoung_LI, type = "simple")
zipyoung_list <- c("Prob. Change Zip", 
                   round(zipyoung_cs_simple$overall.att,5), 
                   round(zipyoung_cs_simple$overall.se,5),
                   round(zipyoung_cs_simple$overall.att-(1.959*zipyoung_cs_simple$overall.se),5),
                   round(zipyoung_cs_simple$overall.att+(1.959*zipyoung_cs_simple$overall.se),5),
                   "< 60",
                   "Low-Integration Hosp.")

single_ATT_values_LI[nrow(single_ATT_values_LI) + 1,] <- zipyoung_list






saveRDS(single_ATT_values_LI,file=paste0(created_data_path,"/single_ATT_values_LI.rds"))

ATT <- rbind(single_ATT_values,single_ATT_values_LI)

ATT <- ATT %>%
  mutate(ATT=as.numeric(ATT),
         SE=as.numeric(SE),
         Upper=as.numeric(Upper),
         Lower=as.numeric(Lower))

# Create plot

dodge <- position_dodge(width=.75)
small_values <- ggplot(filter(ATT, Variable!="Claim Count" & Variable!="Number Patients" & Age=="Any"),
       aes(x=Variable, y=ATT, color=Specification)) +
  geom_point(position=dodge) +
  geom_errorbar(aes(ymax=Upper,ymin=Lower, width=.5),position = dodge) + 
  geom_hline(yintercept=0, linetype="dashed", size=.1) +
  coord_flip() + theme_bw() + 
  theme(text=element_text(size=17, family="lm")) +
  xlab(" ") + ylab(" ") +
  scale_color_manual(values=c("#999999", "#E69F00"))


large_values <- ggplot(filter(ATT, (Variable=="Claim Count" | Variable=="Number Patients") & Age=="Any"),
       aes(x=Variable, y=ATT, color=Specification)) +
  geom_point(position=dodge) +
  geom_errorbar(aes(ymax=Upper,ymin=Lower, width=.3),position = dodge)  +
  geom_hline(yintercept=0, linetype="dashed", size=.1) +
  coord_flip() + theme_bw() + 
  theme(text=element_text(size=17, family="lm")) +
  xlab(" ") + ylab("\nATT and 95% CI") +
  scale_color_manual(values=c("#999999", "#E69F00"))

ggarrange(
  small_values,
  large_values,
  nrow=2,
  common.legend = TRUE,
  legend="right",
  heights  = c(1,1),
  align="v"
)

ggsave("Objects/LI_results.pdf")











### PHYSICIANS NOT EXPOSED TO DATA ASSISTANTS #####################################################################

da_cs <- att_gt(
  yname = "retire",                # LHS Variable
  gname = "minyr_EHR",             # First year a unit is treated. (set to 0 if never treated)
  idname = "DocNPI",               # ID
  tname = "year",                  # Time Variable
  # xformla = NULL                 # No covariates
  xformla = ~grad_year,            # Time-invariant controls
  data = dplyr::filter(
  Physician_Data,minyr_EHR>0 & works_with_DA==0 & max_age<=60),   # Remove never-treated units
  # data = Physician_Data
  est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
  control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
  clustervars = "DocNPI",          # Cluster Variables          
  anticipation=0                   # can set a number of years to account for anticipation effects
)
# Save p-value for pre-trends to put in footnote of table
p<-da_cs$Wpval

# Aggregate the effects
da_cs_dyn <- aggte(da_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_da_allEHR 
ggdid(da_cs_dyn, xlab="\nEvent Time", title="All Physicians") + 
  labs(caption=paste0("p-value= ",round(p,3),"\n")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="")  +
  theme(text = element_text(size = 17, family="lm"))






