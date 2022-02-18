library(did)
library(dplyr)
library(ggplot2)

# ------------------------------------- ANALYSIS  ------------------------------------
#                                       Hanna Glenn, Emory University
#                                       1/31/2022

# This script reads in "Physician_Data.rds" from data5, the final dataset used in my third year paper. 
# The analysis in this script focuses mainly on Pedro Sant'Anna and Brent Callaway's difference in
# difference methodologies. 

Physician_Data <- readRDS(paste0(created_data_path,"Physician_Data.rds"))


# Retirement ---------------------------------------------------------------------------

# Full sample (exclude 2017 because I cannot observe retirement)
retire_es <- att_gt(yname = "retire",
              gname = "minyr_EHR",
              idname = "DocNPI",
              tname = "year",
              xformla = ~grad_year,
              data = filter(Physician_Data, minyr_EHR>0),
              est_method = "dr",
              control_group = "notyettreated",
              anticipation=0,
)
# Save p-value to put in footnote
p<-retire_es$Wpval

# Aggregate the effects
retire_es_dyn <- aggte(retire_es, type = "dynamic")

# Create a plot
ggdid(retire_es_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Average Effect of EHR Exposure on Retirement by Length of Exposure") + 
  labs(caption=paste0("\n Note: p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"))

# Save the plot
ggsave(file="ggdid_retire_allEHR.pdf",path="Objects")



# Older Physicians
old_retire_es <- att_gt(yname = "retire",
                    gname = "minyr_EHR",
                    idname = "DocNPI",
                    tname = "year",
                    xformla = ~grad_year,
                    data = dplyr::filter(Physician_Data,age>50 & minyr_EHR>0),
                    est_method = "dr",
                    control_group = "notyettreated",
                    anticipation=0
)
# Save p-value to put in footnote
p<-old_retire_es$Wpval

# Aggregate the effects
old_retire_es_dyn <- aggte(old_retire_es, type = "dynamic")

# Create a plot
ggdid(old_retire_es_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Average Effect of EHR Exposure on Retirement by Length of Exposure \n In Physicians > 50") + 
  labs(caption=paste0("\n Note: p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"))

# Save the plot
ggsave(file="ggdid_retire_allEHR_old.pdf",path="Objects")


# Do the same thing but with low-integrated hospitals -----------------------------------------------

retire_es_li <- att_gt(yname = "retire",
                    gname = "minyr_EHR_int",
                    idname = "DocNPI",
                    tname = "year",
                    xformla = ~grad_year,
                    data = filter(Physician_Data, minyr_EHR_int>0),
                    est_method = "dr",
                    control_group = "notyettreated",
                    anticipation=0
)
# Save p-value to put in footnote
p<-retire_es_li$Wpval

# Aggregate the effects
retire_es_dyn_li <- aggte(retire_es_li, type = "dynamic")

# Create a plot
ggdid(retire_es_dyn_li, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Average Effect of EHR Exposure in Low- Integration Hospitals on Retirement by Length of Exposure") + 
  labs(caption=paste0("\n Note: p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"))

# Save the plot
ggsave(file="ggdid_retire_allEHR_li.pdf",path="Objects")



# Older Physicians
old_retire_es_li <- att_gt(yname = "retire",
                        gname = "minyr_EHR_int",
                        idname = "DocNPI",
                        tname = "year",
                        xformla = ~grad_year,
                        data = dplyr::filter(Physician_Data,age>50 & minyr_EHR>0),
                        est_method = "dr",
                        control_group = "notyettreated",
                        anticipation=0
)
# Save p-value to put in footnote
p<-old_retire_es_li$Wpval

# Aggregate the effects
old_retire_es_dyn_li <- aggte(old_retire_es_li, type = "dynamic")

# Create a plot
ggdid(old_retire_es_dyn_li, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Average Effect of EHR Exposure in Low-Integration Hospitals on Retirement by Length of Exposure \n In Physicians > 50") + 
  labs(caption=paste0("\n Note: p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"))

# Save the plot
ggsave(file="ggdid_retire_allEHR_old_li.pdf",path="Objects")


# Office- based outcomes -------------------------------------------------------------

# Full Sample using fraction of patients in office
office_frac_es <- att_gt(yname = "pos_office",
                    gname = "minyr_EHR",
                    idname = "DocNPI",
                    tname = "year",
                    xformla = ~age,
                    data = dplyr::filter(Physician_Data,ever_retire==0),
                    est_method = "reg",
                    control_group = "notyettreated"
)

ggdid(office_frac_es)

office_frac_es_dyn <- aggte(office_frac_es, type = "dynamic")
ggdid(office_frac_es_dyn)

# Old sample using fraction of claims in office
old_office_frac_es <- att_gt(yname = "pos_office",
                        gname = "minyr_EHR",
                        idname = "DocNPI",
                        tname = "year",
                        xformla = ~age,
                        data = dplyr::filter(Physician_Data,age>50 & ever_retire==0),
                        est_method = "reg",
                        control_group = "notyettreated"
)

old_office_frac_es_dyn <- aggte(old_office_frac_es, type = "dynamic")
ggdid(old_office_frac_es_dyn)

# Full Sample using indicator for working in office
office_ind_es <- att_gt(yname = "work_in_office",
                        gname = "minyr_EHR",
                        idname = "DocNPI",
                        tname = "year",
                        xformla = ~age,
                        data = dplyr::filter(Physician_Data,ever_retire==0),
                        est_method = "reg",
                        control_group = "notyettreated"
)

ggdid(office_ind_es)

office_ind_es_dyn <- aggte(office_ind_es, type = "dynamic")
ggdid(office_ind_es_dyn)

# Old Sample using indicator for working in office
old_office_ind_es <- att_gt(yname = "work_in_office",
                        gname = "minyr_EHR",
                        idname = "DocNPI",
                        tname = "year",
                        xformla = ~age,
                        data = dplyr::filter(Physician_Data,ever_retire==0 & age>50),
                        est_method = "reg",
                        control_group = "notyettreated"
)

ggdid(old_office_ind_es)

old_office_ind_es_dyn <- aggte(old_office_ind_es, type = "dynamic")
ggdid(old_office_ind_es_dyn)


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





