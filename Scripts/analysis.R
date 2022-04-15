library(did)
library(dplyr)
library(ggplot2)
library(readr)
library(fixest)
library(did2s)
library(ggpubr)

# ------------------------------------- ANALYSIS  ------------------------------------
#                                       Hanna Glenn, Emory University
#                                       1/31/2022

font_add_google("Cormorant Garamond", "corm")

font_add("lm","C:/Users/hkagele/Downloads/Latin-Modern-Roman/lmroman10-regular.otf")

## Automatically use showtext to render text
showtext_auto()

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
cs_retire_allEHR <- ggdid(retire_cs_dyn, xlab="\nEvent Time", title="All Physicians") + 
  labs(caption=paste0("p-value= ",round(p,3),"\n")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") + ylim(-.0025,.007) +
    theme(text = element_text(size = 17, family="lm"))



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
cs_retireold_allEHR <- ggdid(retireold_cs_dyn, theming=FALSE, legend=FALSE, title="Physicians >= 60") + 
  labs(caption=paste0("p-value= ",round(p,3))) +
  theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +ylim(-.005,.01) +
  theme(legend.position = "none", text = element_text(size = 17, family="lm"))


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
cs_retireyoung_allEHR <- ggdid(retireyoung_cs_dyn,  theming=FALSE, legend=FALSE, ref_line=0, 
    title="Physicians < 60") + 
  labs(caption=paste0("p-value= ",round(p,3))) +
  theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"), name="") + ylim(-.005,.01) +
  theme(legend.position = "none", text=element_text(size=17, family="lm"))



# Combine all retire graphs
retire_plot <- ggarrange(
  cs_retire_allEHR,
  ggarrange(cs_retireyoung_allEHR, cs_retireold_allEHR,
            ncol=2),
  nrow=2,
  common.legend = TRUE,
  legend="right",
  heights  = c(1.25,1)
)

ggsave(retire_plot,filename="Objects/retire_plot.pdf", width=11.4, height = 9.08, units="in")





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

# Create a plot
cs_officefrac_allEHR <- ggdid(office_frac_cs_dyn, xlab="\n Event Time", theming=FALSE, legend=FALSE, title="All Physicians") + 
  labs(caption=paste0("p-value= ",round(p,3),"\n")) +
  theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  geom_hline(yintercept=0, size=.2, linetype="dashed") + ylim(-.03,.03) +
  theme(text=element_text(size=17,family="lm"))




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
cs_officefracold_allEHR <- ggdid(office_fracold_cs_dyn, theming=FALSE, legend=FALSE, title="Physicians >= 60") + 
  labs(caption=paste0("p-value= ",round(p,3))) +
  theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  geom_hline(yintercept = 0, size=.2, linetype="dashed") +ylim(-.075,.04) + 
  theme(legend.position = "none", text=element_text(size=17,family="lm"))


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
cs_officefracyoung_allEHR <- ggdid(office_fracyoung_cs_dyn, theming=FALSE, legend=FALSE, title="Physicians < 60") + 
  labs(caption=paste0("p-value= ",round(p,3))) +
  theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="none") +
  geom_hline(yintercept = 0,size=.2,linetype="dashed")+ylim(-.075,.04) + 
  theme(legend.position ="none", text=element_text(size=17,family="lm"))

# Plot office frac graphs
officefrac_plot <- ggarrange(
  cs_officefrac_allEHR,
  ggarrange(cs_officefracyoung_allEHR, cs_officefracold_allEHR,
            ncol=2),
  nrow=2,
  common.legend = TRUE,
  legend="right",
  heights  = c(1.25,1)
)

ggsave(officefrac_plot,filename="Objects/officefrac_plot.pdf", width=11.4, height = 9.08, units="in")



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
cs_officeind_allEHR <- ggdid(office_ind_cs_dyn, xlab="\n Event Time", theming=FALSE, legend=FALSE, title="All Physicians") + 
  labs(caption=paste0("p-value= ",round(p,3),"\n")) +
  theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") + 
  geom_hline(yintercept = 0,size=.2,linetype="dashed") + ylim(-.05,.05) +
  theme(text=element_text(size=17,family="lm"))



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
cs_officeindold_allEHR <- ggdid(office_indold_cs_dyn, theming=FALSE, legend=FALSE, title="Physicians >= 60") + 
  labs(caption=paste0("p-value= ",round(p,3))) +
  theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  geom_hline(yintercept = 0,size=.2,linetype="dashed") + theme(legend.position = "none", text=element_text(size=17,family="lm")) + ylim(-.08,.08)



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
cs_officeindyoung_allEHR <- ggdid(office_indyoung_cs_dyn, theming=FALSE, legend=FALSE, title="Physicians < 60") + 
  labs(caption=paste0("p-value= ",round(p,3))) +
  theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  geom_hline(yintercept = 0,size=.2,linetype="dashed") + theme(legend.position = "none",text=element_text(size=17,family="lm")) + ylim(-.08,.08)


# Create office ind plot 
officeind_plot <- ggarrange(
  cs_officeind_allEHR,
  ggarrange(cs_officeindyoung_allEHR, cs_officeindold_allEHR,
            ncol=2),
  nrow=2,
  common.legend = TRUE,
  legend="right",
  heights  = c(1.25,1)
)

ggsave(officeind_plot,filename="Objects/officeind_plot.pdf", width=11.4, height = 9.08, units="in")




## CHANGE ZIP CODE ###
# Full Sample using indicator for working in office
zip_cs <- att_gt(yname = "change_zip",
                        gname = "minyr_EHR",
                        idname = "DocNPI",
                        tname = "year",
                        xformla = ~grad_year,
                        data = dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0),
                        est_method = "dr",
                        control_group = "notyettreated"
)

p<-zip_cs$Wpval

# Aggregate the effects
zip_cs_dyn <- aggte(zip_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_zip_allEHR <- ggdid(zip_cs_dyn, xlab="\n Event Time", theming=FALSE, legend=FALSE, title="All Physicians") + 
  labs(caption=paste0("p-value= ",round(p,3),"\n")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") + 
  geom_hline(yintercept = 0,size=.2,linetype="dashed") + ylim(-.05,.08) + 
  theme(text=element_text(size=17,family="lm"))



# Old Sample using indicator for working in office
zipold_cs <- att_gt(yname = "change_zip",
                           gname = "minyr_EHR",
                           idname = "DocNPI",
                           tname = "year",
                           xformla = ~grad_year,
                           data = dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0 & max_age>=60),
                           est_method = "dr",
                           control_group = "notyettreated"
)

p<-zipold_cs$Wpval

# Aggregate the effects
zipold_cs_dyn <- aggte(zipold_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_zipold_allEHR <- ggdid(zipold_cs_dyn, theming=FALSE, legend=FALSE, title="Physicians >= 60") + 
  labs(caption=paste0("p-value= ",round(p,3))) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  geom_hline(yintercept = 0,size=.2,linetype="dashed") + 
  theme(legend.position = "none", text=element_text(size=17,family="lm")) + ylim(-.085,.09)



# Young Sample using indicator for working in office
zipyoung_cs <- att_gt(yname = "change_zip",
                             gname = "minyr_EHR",
                             idname = "DocNPI",
                             tname = "year",
                             xformla = ~grad_year,
                             data = dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0 & max_age<60),
                             est_method = "dr",
                             control_group = "notyettreated"
)

p<-zipyoung_cs$Wpval

# Aggregate the effects
zipyoung_cs_dyn <- aggte(zipyoung_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_zipyoung_allEHR <- ggdid(zipyoung_cs_dyn, theming=FALSE, legend=FALSE, title="Physicians < 60") + 
  labs(caption=paste0("p-value= ",round(p,3))) +
  theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  geom_hline(yintercept = 0,size=.2,linetype="dashed") + 
  theme(legend.position = "none", text=element_text(size=17,family="lm")) + ylim(-.085,.085)


# Create office ind plot 
zip_plot 
ggarrange(
  cs_zip_allEHR,
  ggarrange(cs_zipyoung_allEHR, cs_zipold_allEHR,
            ncol=2),
  nrow=2,
  common.legend = TRUE,
  legend="right",
  heights  = c(1.25,1)
)

ggsave(zip_plot,filename="Objects/zip_plot.pdf", width=11.4, height = 9.08, units="in")






## PRODUCTIVITY ####

# Full Sample using indicator for working in office
patient_cs <- att_gt(yname = "npi_unq_benes",
                        gname = "minyr_EHR",
                        idname = "DocNPI",
                        tname = "year",
                        xformla = ~grad_year,
                        data = dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0),
                        est_method = "dr",
                        control_group = "notyettreated"
)

p<-patient_cs$Wpval

# Aggregate the effects
patient_cs_dyn <- aggte(patient_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_patient_allEHR <- ggdid(patient_cs_dyn, xlab="\n Event Time", theming=FALSE, legend=FALSE, title="All Physicians") + 
  labs(caption=paste0("p-value= ",round(p,3),"\n")) +
  theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") + 
  geom_hline(yintercept = 0,size=.2,linetype="dashed") +ylim(-25,30) +
  theme(text=element_text(size=17,family="lm"))



# Old Sample using indicator for working in office
patientold_cs <- att_gt(yname = "npi_unq_benes",
                           gname = "minyr_EHR",
                           idname = "DocNPI",
                           tname = "year",
                           xformla = ~grad_year,
                           data = dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0 & max_age>=60),
                           est_method = "dr",
                           control_group = "notyettreated"
)

p<-patientold_cs$Wpval

# Aggregate the effects
patientold_cs_dyn <- aggte(patientold_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_patientold_allEHR <- ggdid(patientold_cs_dyn, theming=FALSE, legend=FALSE, title="Physicians >= 60") + 
  labs(caption=paste0("p-value= ",round(p,3))) +
  theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  geom_hline(yintercept = 0,size=.2,linetype="dashed") +
  theme(legend.position = "none", text=element_text(size=17,family="lm")) +ylim(-50,50)



# Young Sample using indicator for working in office
patientyoung_cs <- att_gt(yname = "npi_unq_benes",
                             gname = "minyr_EHR",
                             idname = "DocNPI",
                             tname = "year",
                             xformla = ~grad_year,
                             data = dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0 & max_age<60),
                             est_method = "dr",
                             control_group = "notyettreated"
)

p<-patientyoung_cs$Wpval

# Aggregate the effects
patientyoung_cs_dyn <- aggte(patientyoung_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_patientyoung_allEHR <- ggdid(patientyoung_cs_dyn, theming=FALSE, legend=FALSE, title="Physicians < 60") + 
  labs(caption=paste0("p-value= ",round(p,3))) +
  theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  geom_hline(yintercept = 0,size=.2,linetype="dashed") + 
  theme(legend.position = "none", text=element_text(size=17,family="lm")) +ylim(-50,50)


# Create office ind plot 
patient_plot <- ggarrange(
  cs_patient_allEHR,
  ggarrange(cs_patientyoung_allEHR, cs_patientold_allEHR,
            ncol=2),
  nrow=2,
  common.legend = TRUE,
  legend="right",
  heights  = c(1.25,1)
)

ggsave(patient_plot,filename="Objects/patient_plot.pdf", width=11.4, height = 9.08, units="in")


## CLAIM COUNT
# Full Sample 
claim_cs <- att_gt(yname = "claim_count_total",
                        gname = "minyr_EHR",
                        idname = "DocNPI",
                        tname = "year",
                        xformla = ~grad_year,
                        data = dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0),
                        est_method = "dr",
                        control_group = "notyettreated"
)

p<-claim_cs$Wpval

# Aggregate the effects
claim_cs_dyn <- aggte(claim_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_claim_allEHR <- ggdid(claim_cs_dyn, xlab="\n Event Time", theming=FALSE, legend=FALSE, title="All Physicians") + 
  labs(caption=paste0("p-value= ",round(p,3),"\n")) +
  theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") + 
  geom_hline(yintercept = 0,size=.2,linetype="dashed") + ylim(-300,200) +
  theme(text=element_text(size=17,family="lm"))



# Old Sample using indicator for working in office
claimold_cs <- att_gt(yname = "claim_count_total",
                           gname = "minyr_EHR",
                           idname = "DocNPI",
                           tname = "year",
                           xformla = ~grad_year,
                           data = dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0 & max_age>=60),
                           est_method = "dr",
                           control_group = "notyettreated"
)

p<-claimold_cs$Wpval

# Aggregate the effects
claimold_cs_dyn <- aggte(claimold_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_claimold_allEHR <- ggdid(claimold_cs_dyn, theming=FALSE, legend=FALSE, title="Physicians >= 60") + 
  labs(caption=paste0("p-value= ",round(p,3))) +
  theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  geom_hline(yintercept = 0,size=.2,linetype="dashed") + 
  theme(legend.position = "none", text=element_text(size=17, family="lm")) + ylim(-580,400)



# Young Sample using indicator for working in office
claimyoung_cs <- att_gt(yname = "claim_count_total",
                             gname = "minyr_EHR",
                             idname = "DocNPI",
                             tname = "year",
                             xformla = ~grad_year,
                             data = dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0 & max_age<60),
                             est_method = "dr",
                             control_group = "notyettreated"
)

p<-claimyoung_cs$Wpval

# Aggregate the effects
claimyoung_cs_dyn <- aggte(claimyoung_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_claimyoung_allEHR <- ggdid(claimyoung_cs_dyn, theming=FALSE, legend=FALSE, title="Physicians < 60") + 
  labs(caption=paste0("p-value= ",round(p,3))) +
  theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  geom_hline(yintercept = 0,size=.2,linetype="dashed") + 
  theme(legend.position = "none", text=element_text(size=17,family="lm")) +ylim(-550,400)


# Create  plot 
claim_plot <- ggarrange(
  cs_claim_allEHR,
  ggarrange(cs_claimyoung_allEHR, cs_claimold_allEHR,
            ncol=2),
  nrow=2,
  common.legend = TRUE,
  legend="right",
  heights  = c(1.25,1)
)

ggsave(claim_plot,filename="Objects/claim_plot.pdf", width=11.4, height = 9.08, units="in")





##### APPENDIX: SECONDARY TREATMENT VARIABLE #####################################################################
##################################################################################################################
#################################################################################################################
##################################################################################################################


retire_cs <- att_gt(
  yname = "retire",                # LHS Variable
  gname = "minyr_EHR_int",             # First year a unit is treated. (set to 0 if never treated)
  idname = "DocNPI",               # ID
  tname = "year",                  # Time Variable
  # xformla = NULL                 # No covariates
  xformla = ~grad_year,            # Time-invariant controls
  data = dplyr::filter(
    Physician_Data,minyr_EHR_int>0),   # Remove never-treated units
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
cs_retire_allEHR <- ggdid(retire_cs_dyn, ylab = "Estimate and 95% CI", xlab="Relative Year", title="All Physicians") + 
  labs(caption=paste0("p-value for pre-test of parallel trends assumption= ",round(p,3),"\n")) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") + ylim(-.01,.01)



# OTHER RETIREMENT OUTCOMES --------------------------------------------------------------------------------

# Senior Physicians
retireold_cs <- att_gt(
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
# Save p-value for pre-trends to put in footnote of table
p<-retireold_cs$Wpval

# Aggregate the effects
retireold_cs_dyn <- aggte(retireold_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_retireold_allEHR <- ggdid(retireold_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Physicians >= 60") + 
  labs(caption=paste0("p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +ylim(-.015,.015) +
  theme(legend.position = "none")


# Young Physicians
retireyoung_cs <- att_gt(
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
# Save p-value for pre-trends to put in footnote of table
p<-retireyoung_cs$Wpval

# Aggregate the effects
retireyoung_cs_dyn <- aggte(retireyoung_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_retireyoung_allEHR <- ggdid(retireyoung_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, ref_line=0, 
                               title="Physicians < 60") + 
  labs(caption=paste0("p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"), name="") + ylim(-.015,.015) +
  theme(legend.position = "none")



# Combine all retire graphs
retire_plot <- ggarrange(
  cs_retire_allEHR,
  ggarrange(cs_retireyoung_allEHR, cs_retireold_allEHR,
            ncol=2),
  nrow=2,
  common.legend = TRUE,
  legend="bottom",
  heights  = c(1.25,1)
)

retire_plot

ggsave(retire_plot,filename="Objects/retire_plot_LI.pdf", width=11.4, height = 9.08, units="in")





# OFFICE BASED OUTCOMES -------------------------------------------------------------

# Full Sample using fraction of patients in office
office_frac_cs <- att_gt(yname = "pos_office",
                         gname = "minyr_EHR_int",
                         idname = "DocNPI",
                         tname = "year",
                         xformla = ~grad_year,
                         data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0),
                         est_method = "dr",
                         control_group = "notyettreated"
)

p<-office_frac_cs$Wpval

# Aggregate the effects
office_frac_cs_dyn <- aggte(office_frac_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_officefrac_allEHR <- ggdid(office_frac_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="All Physicians") + 
  labs(caption=paste0("p-value for pre-test of parallel trends assumption= ",round(p,3),"\n")) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  geom_hline(yintercept=0, size=.2, linetype="dashed") + ylim(-.075,.075)




# Old Sample using fraction of patients in office
office_fracold_cs <- att_gt(yname = "pos_office",
                            gname = "minyr_EHR_int",
                            idname = "DocNPI",
                            tname = "year",
                            xformla = ~grad_year,
                            data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age>=60),
                            est_method = "dr",
                            control_group = "notyettreated"
)

p<-office_fracold_cs$Wpval

# Aggregate the effects
office_fracold_cs_dyn <- aggte(office_fracold_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_officefracold_allEHR <- ggdid(office_fracold_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Physicians >= 60") + 
  labs(caption=paste0("p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  geom_hline(yintercept = 0, size=.2, linetype="dashed") +ylim(-.075,.075) + theme(legend.position = "none")


# Young Sample using fraction of patients in office
office_fracyoung_cs <- att_gt(yname = "pos_office",
                              gname = "minyr_EHR_int",
                              idname = "DocNPI",
                              tname = "year",
                              xformla = ~grad_year,
                              data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age<60),
                              est_method = "dr",
                              control_group = "notyettreated"
)

p<-office_fracyoung_cs$Wpval

# Aggregate the effects
office_fracyoung_cs_dyn <- aggte(office_fracyoung_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_officefracyoung_allEHR <- ggdid(office_fracyoung_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Physicians < 60") + 
  labs(caption=paste0("p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="none") +
  geom_hline(yintercept = 0,size=.2,linetype="dashed")+ylim(-.075,.075) + theme(legend.position ="none")

# Plot office frac graphs
officefrac_plot <- ggarrange(
  cs_officefrac_allEHR,
  ggarrange(cs_officefracyoung_allEHR, cs_officefracold_allEHR,
            ncol=2),
  nrow=2,
  common.legend = TRUE,
  legend="bottom",
  heights  = c(1.25,1)
)

officefrac_plot

ggsave(officefrac_plot,filename="Objects/officefrac_plot_LI.pdf", width=11.4, height = 9.08, units="in")



# Full Sample using indicator for working in office
office_ind_cs <- att_gt(yname = "work_in_office",
                        gname = "minyr_EHR_int",
                        idname = "DocNPI",
                        tname = "year",
                        xformla = ~grad_year,
                        data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0),
                        est_method = "dr",
                        control_group = "notyettreated"
)

p<-office_ind_cs$Wpval

# Aggregate the effects
office_ind_cs_dyn <- aggte(office_ind_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_officeind_allEHR <- ggdid(office_ind_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="All Physicians") + 
  labs(caption=paste0("p-value for pre-test of parallel trends assumption= ",round(p,3),"\n")) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") + 
  geom_hline(yintercept = 0,size=.2,linetype="dashed") + ylim(-.08,.08)



# Old Sample using indicator for working in office
office_indold_cs <- att_gt(yname = "work_in_office",
                           gname = "minyr_EHR_int",
                           idname = "DocNPI",
                           tname = "year",
                           xformla = ~grad_year,
                           data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age>=60),
                           est_method = "dr",
                           control_group = "notyettreated"
)

p<-office_indold_cs$Wpval

# Aggregate the effects
office_indold_cs_dyn <- aggte(office_indold_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_officeindold_allEHR <- ggdid(office_indold_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Physicians >= 60") + 
  labs(caption=paste0("p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  geom_hline(yintercept = 0,size=.2,linetype="dashed") + theme(legend.position = "none") + ylim(-.08,.08)



# Young Sample using indicator for working in office
office_indyoung_cs <- att_gt(yname = "work_in_office",
                             gname = "minyr_EHR_int",
                             idname = "DocNPI",
                             tname = "year",
                             xformla = ~grad_year,
                             data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age<60),
                             est_method = "dr",
                             control_group = "notyettreated"
)

p<-office_indyoung_cs$Wpval

# Aggregate the effects
office_indyoung_cs_dyn <- aggte(office_indyoung_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_officeindyoung_allEHR <- ggdid(office_indyoung_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Physicians < 60") + 
  labs(caption=paste0("p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  geom_hline(yintercept = 0,size=.2,linetype="dashed") + theme(legend.position = "none") + ylim(-.08,.08)


# Create office ind plot 
officeind_plot <- ggarrange(
  cs_officeind_allEHR,
  ggarrange(cs_officeindyoung_allEHR, cs_officeindold_allEHR,
            ncol=2),
  nrow=2,
  common.legend = TRUE,
  legend="bottom",
  heights  = c(1.25,1)
)

officeind_plot

ggsave(officeind_plot,filename="Objects/officeind_plot_LI.pdf", width=11.4, height = 9.08, units="in")




## CHANGE ZIP CODE ###
# Full Sample using indicator for working in office
zip_cs <- att_gt(yname = "change_zip",
                 gname = "minyr_EHR_int",
                 idname = "DocNPI",
                 tname = "year",
                 xformla = ~grad_year,
                 data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0),
                 est_method = "dr",
                 control_group = "notyettreated"
)

p<-zip_cs$Wpval

# Aggregate the effects
zip_cs_dyn <- aggte(zip_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_zip_allEHR <- ggdid(zip_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="All Physicians") + 
  labs(caption=paste0("p-value for pre-test of parallel trends assumption= ",round(p,3),"\n")) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") + 
  geom_hline(yintercept = 0,size=.2,linetype="dashed") + ylim(-.08,.08)



# Old Sample using indicator for working in office
zipold_cs <- att_gt(yname = "change_zip",
                    gname = "minyr_EHR_int",
                    idname = "DocNPI",
                    tname = "year",
                    xformla = ~grad_year,
                    data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age>=60),
                    est_method = "dr",
                    control_group = "notyettreated"
)

p<-zipold_cs$Wpval

# Aggregate the effects
zipold_cs_dyn <- aggte(zipold_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_zipold_allEHR <- ggdid(zipold_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Physicians >= 60") + 
  labs(caption=paste0("p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  geom_hline(yintercept = 0,size=.2,linetype="dashed") + theme(legend.position = "none") + ylim(-.1,.1)



# Young Sample using indicator for working in office
zipyoung_cs <- att_gt(yname = "change_zip",
                      gname = "minyr_EHR_int",
                      idname = "DocNPI",
                      tname = "year",
                      xformla = ~grad_year,
                      data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age<60),
                      est_method = "dr",
                      control_group = "notyettreated"
)

p<-zipyoung_cs$Wpval

# Aggregate the effects
zipyoung_cs_dyn <- aggte(zipyoung_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_zipyoung_allEHR <- ggdid(zipyoung_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Physicians < 60") + 
  labs(caption=paste0("p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  geom_hline(yintercept = 0,size=.2,linetype="dashed") + theme(legend.position = "none") + ylim(-.1,.1)


# Create office ind plot 
zip_plot <- ggarrange(
  cs_zip_allEHR,
  ggarrange(cs_zipyoung_allEHR, cs_zipold_allEHR,
            ncol=2),
  nrow=2,
  common.legend = TRUE,
  legend="bottom",
  heights  = c(1.25,1)
)

zip_plot

ggsave(zip_plot,filename="Objects/zip_plot_LI.pdf", width=11.4, height = 9.08, units="in")






## PRODUCTIVITY ####

# Full Sample using indicator for working in office
patient_cs <- att_gt(yname = "npi_unq_benes",
                     gname = "minyr_EHR_int",
                     idname = "DocNPI",
                     tname = "year",
                     xformla = ~grad_year,
                     data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0),
                     est_method = "dr",
                     control_group = "notyettreated"
)

p<-patient_cs$Wpval

# Aggregate the effects
patient_cs_dyn <- aggte(patient_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_patient_allEHR <- ggdid(patient_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="All Physicians") + 
  labs(caption=paste0("p-value for pre-test of parallel trends assumption= ",round(p,3),"\n")) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") + 
  geom_hline(yintercept = 0,size=.2,linetype="dashed") +ylim(-50,50)



# Old Sample using indicator for working in office
patientold_cs <- att_gt(yname = "npi_unq_benes",
                        gname = "minyr_EHR_int",
                        idname = "DocNPI",
                        tname = "year",
                        xformla = ~grad_year,
                        data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age>=60),
                        est_method = "dr",
                        control_group = "notyettreated"
)

p<-patientold_cs$Wpval

# Aggregate the effects
patientold_cs_dyn <- aggte(patientold_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_patientold_allEHR <- ggdid(patientold_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Physicians >= 60") + 
  labs(caption=paste0("p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  geom_hline(yintercept = 0,size=.2,linetype="dashed") + theme(legend.position = "none") +ylim(-50,50)



# Young Sample using indicator for working in office
patientyoung_cs <- att_gt(yname = "npi_unq_benes",
                          gname = "minyr_EHR_int",
                          idname = "DocNPI",
                          tname = "year",
                          xformla = ~grad_year,
                          data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age<60),
                          est_method = "dr",
                          control_group = "notyettreated"
)

p<-patientyoung_cs$Wpval

# Aggregate the effects
patientyoung_cs_dyn <- aggte(patientyoung_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_patientyoung_allEHR <- ggdid(patientyoung_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Physicians < 60") + 
  labs(caption=paste0("p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  geom_hline(yintercept = 0,size=.2,linetype="dashed") + theme(legend.position = "none") +ylim(-50,50)


# Create office ind plot 
patient_plot <- ggarrange(
  cs_patient_allEHR,
  ggarrange(cs_patientyoung_allEHR, cs_patientold_allEHR,
            ncol=2),
  nrow=2,
  common.legend = TRUE,
  legend="bottom",
  heights  = c(1.25,1)
)

patient_plot

ggsave(patient_plot,filename="Objects/patient_plot_LI.pdf", width=11.4, height = 9.08, units="in")


## CLAIM COUNT
# Full Sample 
claim_cs <- att_gt(yname = "claim_count_total",
                   gname = "minyr_EHR_int",
                   idname = "DocNPI",
                   tname = "year",
                   xformla = ~grad_year,
                   data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0),
                   est_method = "dr",
                   control_group = "notyettreated"
)

p<-claim_cs$Wpval

# Aggregate the effects
claim_cs_dyn <- aggte(claim_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_claim_allEHR <- ggdid(claim_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="All Physicians") + 
  labs(caption=paste0("p-value for pre-test of parallel trends assumption= ",round(p,3),"\n")) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") + 
  geom_hline(yintercept = 0,size=.2,linetype="dashed") + ylim(-550,550)



# Old Sample using indicator for working in office
claimold_cs <- att_gt(yname = "claim_count_total",
                      gname = "minyr_EHR_int",
                      idname = "DocNPI",
                      tname = "year",
                      xformla = ~grad_year,
                      data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age>=60),
                      est_method = "dr",
                      control_group = "notyettreated"
)

p<-claimold_cs$Wpval

# Aggregate the effects
claimold_cs_dyn <- aggte(claimold_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_claimold_allEHR <- ggdid(claimold_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Physicians >= 60") + 
  labs(caption=paste0("p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  geom_hline(yintercept = 0,size=.2,linetype="dashed") + theme(legend.position = "none") + ylim(-550,550)



# Young Sample using indicator for working in office
claimyoung_cs <- att_gt(yname = "claim_count_total",
                        gname = "minyr_EHR_int",
                        idname = "DocNPI",
                        tname = "year",
                        xformla = ~grad_year,
                        data = dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age<60),
                        est_method = "dr",
                        control_group = "notyettreated"
)

p<-claimyoung_cs$Wpval

# Aggregate the effects
claimyoung_cs_dyn <- aggte(claimyoung_cs, type = "dynamic", na.rm=T)

# Create a plot
cs_claimyoung_allEHR <- ggdid(claimyoung_cs_dyn, xlab="\n Relative Year", theming=FALSE, legend=FALSE, title="Physicians < 60") + 
  labs(caption=paste0("p-value for pre-test of parallel trends assumption= ",round(p,3))) +
  theme(plot.caption = element_text(hjust = 0, face= "italic")) + theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  geom_hline(yintercept = 0,size=.2,linetype="dashed") + theme(legend.position = "none") +ylim(-550,550)


# Create  plot 
claim_plot <- ggarrange(
  cs_claim_allEHR,
  ggarrange(cs_claimyoung_allEHR, cs_claimold_allEHR,
            ncol=2),
  nrow=2,
  common.legend = TRUE,
  legend="bottom",
  heights  = c(1.25,1)
)

claim_plot

ggsave(claim_plot,filename="Objects/claim_plot_LI.pdf", width=11.4, height = 9.08, units="in")





