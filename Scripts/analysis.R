library(did)
library(dplyr)
library(ggplot2)
library(readr)
library(fixest)

# ------------------------------------- ANALYSIS  ------------------------------------
#                                       Hanna Glenn, Emory University
#                                       1/31/2022

# This script reads in "Physician_Data.rds" from data5, the final dataset used in my third year paper. 
# The analysis in this script focuses mainly on Pedro Sant'Anna and Brent Callaway's difference in
# difference methodologies. 

Physician_Data <- readRDS(paste0(created_data_path,"Physician_Data.rds"))

# Create stacked regression data ------------------------------------------------------
for (d in 2011:2014){
  subgroup <- Physician_Data %>%
    mutate(treated_unit=ifelse(minyr_EHR==d,1,0),
           clean_control=ifelse(minyr_EHR>d,1,0),
           years_included=ifelse(year>=d-2 & year<=d+3,1,0)) %>%
    mutate(inclusion=years_included*(treated_unit+clean_control)) %>%
    filter(inclusion==1)
  
  assign(paste0("subgroup_",d),subgroup)
}

stacked_data <- rbind(subgroup_2011,subgroup_2012,subgroup_2013,subgroup_2014,subgroup_2015)

stacked_reg <- feols(retire ~ rel_m2 + rel_m1 + rel_0 + rel_p1 + rel_p2 + rel_p3 +
                       treated_unit:rel_m2 + treated_unit:rel_m1 + 
                       treated_unit:rel_p1 + treated_unit:rel_p2 +
                       treated_unit:rel_p3 |
                       DocNPI:minyr_EHR,
                     cluster="DocNPI",
                     data=stacked_data)

coefplot(stacked_reg, keep="treated_unit")


# Retirement ---------------------------------------------------------------------------

# Try event study with and without fixed effects to see how much results differ
retire_es_all <- feols(retire~ rel_m4+rel_m3+ rel_m2+ rel_0+ rel_p1+
                         rel_p2+ rel_p3 + rel_p4 + max_age| year, 
                       data=filter(Physician_Data,minyr_EHR>0 & minyr_retire!=2016))
retire_es_young <- feols(retire~ rel_m4+rel_m3+ rel_m2+ rel_0+ rel_p1+
                           rel_p2+ rel_p3 + rel_p4 + max_age| year,
                         data=filter(Physician_Data,age<50 & minyr_EHR>0 & minyr_retire!=2016))
retire_es_old <- feols(retire~ rel_m4+rel_m3+ rel_m2+ rel_0+ rel_p1+
                         rel_p2+ rel_p3 + rel_p4 + max_age| year,
                       data=filter(Physician_Data,age>=50 & minyr_EHR>0 & minyr_retire!=2016))
reference=4
names(reference)<-"-1"

coefplot(list(retire_es_young,retire_es_old), keep=c("-4","-3","-2","-1","0",
                                                   "1", "2","3","4"),
         ref=reference, dict=c("rel_m4"="-4", "rel_m3"="-3", "rel_m2"="-2", "rel_0"="0",
                               "rel_p1"="1","rel_p2"="2","rel_p3"="3","rel_p4"="4"),
         main="",
         zero=TRUE,
         col=1:2)

wald_young<-wald(retire_es_young, keep=c("rel_m4","rel_m3","rel_m2"))
p_young <- wald_young$p

wald_old<-wald(retire_es_old, keep=c("rel_m4","rel_m3","rel_m2"))
p_old <- wald_old$p

legend("topright", col = 1:2, pch = 20, lwd = 1, lty = 1:2,
       legend = c("Young", "Old"))

mtext(text=paste0("p-value for pre-trends (young): ",round(p_young,3)),side=1,line = 3, cex = 0.8, adj = 0)
mtext(text=paste0("p-value for pre-trends (old): ",round(p_old,3)),side=1,line = 4, cex = 0.8, adj = 0)


# CALLAWAY AND SANTANNA ESTIMATES (Retire) ------------------------------------------
# Full sample 
young_retire_es <- att_gt(yname = "retire",
              gname = "minyr_EHR",
              idname = "DocNPI",
              tname = "year",
              xformla = ~grad_year,
              data = dplyr::filter(Physician_Data,max_age<51 & minyr_EHR>0 & minyr_retire!=2016),
              est_method = "dr",
              control_group = "notyettreated",
              anticipation=0
)
# Save p-value to put in footnote
p<-young_retire_es$Wpval

# Aggregate the effects
retire_es_young_dyn <- aggte(young_retire_es, type = "dynamic", na.rm=T)

# Create a plot
ggdid(retire_es_young_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Average Effect of EHR Exposure on Retirement by Length of Exposure") + 
  labs(caption=paste0("\n Note: p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00")) + ylim(-1,1)

# Save the plot
ggsave(file="ggdid_retire_allEHR.pdf",path="Objects")



# Older Physicians
old_retire_es <- att_gt(yname = "retire",
                    gname = "minyr_EHR",
                    idname = "DocNPI",
                    tname = "year",
                    xformla = ~grad_year,
                    data = dplyr::filter(Physician_Data,max_age>50 & minyr_EHR>0 & minyr_retire!=2016),
                    est_method = "dr",
                    control_group = "notyettreated",
                    anticipation=0
)
# Save p-value to put in footnote
p<-old_retire_es$Wpval

ggdid(old_retire_es)

# Aggregate the effects
old_retire_es_dyn <- aggte(old_retire_es, type = "dynamic")

# Create a plot
ggdid(old_retire_es_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Average Effect of EHR Exposure on Retirement by Length of Exposure \n In Physicians >= 60") + 
  labs(caption=paste0("\n Note: p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00")) + ylim(-1,1)

# Save the plot
ggsave(file="ggdid_retire_allEHR_old.pdf",path="Objects")


# Do the same thing but with low-integrated hospitals -----------------------------------------------

retire_es_li <- att_gt(yname = "retire",
                    gname = "minyr_EHR_int",
                    idname = "DocNPI",
                    tname = "year",
                    xformla = ~max_age,
                    data = filter(Physician_Data, minyr_EHR_int>0 & minyr_retire!=2016),
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
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00")) + ylim(-1,1)

# Save the plot
ggsave(file="ggdid_retire_allEHR_li.pdf",path="Objects")



# Older Physicians
old_retire_es_li <- att_gt(yname = "retire",
                        gname = "minyr_EHR_int",
                        idname = "DocNPI",
                        tname = "year",
                        xformla = ~max_age,
                        data = dplyr::filter(Physician_Data,max_age>55 & minyr_EHR_int>0 & minyr_retire!=2016),
                        est_method = "dr",
                        control_group = "notyettreated",
                        anticipation=0
)
# Save p-value to put in footnote
p<-old_retire_es_li$Wpval

# Aggregate the effects
old_retire_es_dyn_li <- aggte(old_retire_es_li, type = "dynamic")

# Create a plot
ggdid(old_retire_es_dyn_li, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Average Effect of EHR Exposure in Low-Integration Hospitals on Retirement by Length of Exposure \n In Physicians > 55") + 
  labs(caption=paste0("\n Note: p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00")) +ylim(-1,1)

# Save the plot
ggsave(file="ggdid_retire_allEHR_old_li.pdf",path="Objects")


# OFFICE BASED OUTCOMES -------------------------------------------------------------

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





