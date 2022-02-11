library(did)
library(dplyr)
library(fixest)

# ------------------------------------- ANALYSIS  ------------------------------------
#                                       Hanna Glenn, Emory University
#                                       1/31/2022

# This script reads in "Physician_Data.rds" from data5, the final dataset used in my third year paper. 
# The analysis in this script focuses mainly on Pedro Sant'Anna and Brent Callaway's difference in
# difference methodologies. 

Physician_Data <- readRDS(paste0(created_data_path,"Physician_Data.rds"))

# Retirement ---------------------------------------------------------------------------

retire_es_all <- feols(retire~ rel_m4+rel_m3+ rel_m2+ rel_0+ rel_p1+
                                rel_p2+ rel_p3 + rel_p4 + age| DocNPI+year, 
                              data=filter(Physician_Data,year<2017))
retire_es_young <- feols(retire~ rel_m4+rel_m3+ rel_m2+ rel_0+ rel_p1+
                         rel_p2+ rel_p3 + rel_p4 + age| DocNPI+year, 
                       cluster="DocNPI",
                       data=filter(Physician_Data,age<50 & year<2017))
retire_es_old <- feols(retire~ rel_m4+rel_m3+ rel_m2+ rel_0+ rel_p1+
                         rel_p2+ rel_p3 + rel_p4 + age| DocNPI+year, 
                       cluster="DocNPI",
                       data=filter(Physician_Data,age>=50 & year<2017))
reference=4
names(reference)<-"-1"

coefplot(list(retire_es_all,retire_es_old), keep=c("-4","-3","-2","-1","0",
                                                    "1", "2","3","4"),
         ref=reference, dict=c("rel_m4"="-4", "rel_m3"="-3", "rel_m2"="-2", "rel_0"="0",
                               "rel_p1"="1","rel_p2"="2","rel_p3"="3","rel_p4"="4"),
         main="",
         zero=TRUE,
         col=1:2)

wald_young<-wald(any_chronic_es_young, keep=c("rel_m4","rel_m3","rel_m2"), cluster="st_fips")
p_young <- wald_young$p

wald_old<-wald(any_chronic_es_old, keep=c("rel_m4","rel_m3","rel_m2"), cluster="st_fips")
p_old <- wald_old$p

legend("topright", col = 1:2, pch = 20, lwd = 1, lty = 1:2,
       legend = c("< 35 Yrs.", "35-55 Yrs."))

mtext(text=paste0("p-value for pre-trends (< 35 yrs): ",round(p_young,3)),side=1,line = 3, cex = 0.8, adj = 0)
mtext(text=paste0("p-value for pre-trends (35-55 yrs): ",round(p_old,3)),side=1,line = 4, cex = 0.8, adj = 0)









# Full sample (exclude 2017 because I cannot observe retirement. Also think about whether to exclude 2016)
retire_es <- att_gt(yname = "retire",
              gname = "minyr_EHR",
              idname = "DocNPI",
              tname = "year",
              xformla = ~age,
              data = Physician_Data,
              est_method = "reg",
              control_group = "notyettreated"
)
ggdid(retire_es)

retire_es_dyn <- aggte(retire_es, type = "dynamic")
ggdid(retire_es_dyn)



# Older Physicians
old_retire_es <- att_gt(yname = "retire_lowclaims",
                    gname = "minyr_EHR",
                    idname = "DocNPI",
                    tname = "year",
                    xformla = ~age,
                    data = dplyr::filter(Physician_Data,year<2017 & age>50),
                    est_method = "reg",
                    control_group = "notyettreated"
)
ggdid(old_retire_es)

old_retire_es_dyn <- aggte(old_retire_es, type = "dynamic")
ggdid(old_retire_es_dyn)

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





