library(did)
library(dplyr)
library(ggplot2)
library(readr)
library(fixest)
library(did2s)
library(ggpubr)
library(showtext)

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
    dplyr::filter(inclusion==1)
  
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



## ANALYSIS ---------------------------------------------- #########
## Write results for each variable in a loop and create graphs ----- ##

varlist <- list("retire", "pos_office", "work_in_office", "change_zip", "npi_unq_benes", "claim_count_total")

# Get results for ATTGT
models <- lapply(varlist, function(x) {
  all <- att_gt(yname = x,                # LHS Variable
    gname = "minyr_EHR",             # First year a unit is treated. (set to 0 if never treated)
    idname = "DocNPI",               # ID
    tname = "year",                  # Time Variable
    # xformla = NULL                 # No covariates
    xformla = ~grad_year,            # Time-invariant controls
    data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1)),
    est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
    control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
    clustervars = "DocNPI",          # Cluster Variables          
    anticipation=0                   # can set a number of years to account for anticipation effects
  )
  
  young <- att_gt(yname = x,                # LHS Variable
         gname = "minyr_EHR",             # First year a unit is treated. (set to 0 if never treated)
         idname = "DocNPI",               # ID
         tname = "year",                  # Time Variable
         # xformla = NULL                 # No covariates
         xformla = ~grad_year,            # Time-invariant controls
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0 & max_age<60) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & max_age<60) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & max_age<60)),
         est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
         control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
         clustervars = "DocNPI",          # Cluster Variables          
         anticipation=0                   # can set a number of years to account for anticipation effects
  )
  
  old <- att_gt(yname = x,                # LHS Variable
         gname = "minyr_EHR",             # First year a unit is treated. (set to 0 if never treated)
         idname = "DocNPI",               # ID
         tname = "year",                  # Time Variable
         # xformla = NULL                 # No covariates
         xformla = ~grad_year,            # Time-invariant controls
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0 & max_age>=60) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & max_age>=60) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & max_age>=60)),
         est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
         control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
         clustervars = "DocNPI",          # Cluster Variables          
         anticipation=0                   # can set a number of years to account for anticipation effects
  )
  
  list(all=all,young=young,old=old)
})


# Aggregate the previous results
Models_agg <- lapply(models, function(x){
  agg_all <- aggte(x$all, type = "dynamic",na.rm=T)
  p_all <- x$all$Wpval
  
  agg_young <- aggte(x$young, type = "dynamic",na.rm=T)
  p_young <- x$young$Wpval
  
  agg_old <- aggte(x$old, type = "dynamic",na.rm=T)
  p_old <- x$old$Wpval
  
  list(agg_all=agg_all, p_all=p_all, agg_young=agg_young, p_young=p_young, agg_old=agg_old, p_old=p_old)
})



# Create ggdid for each group
graphs <- lapply(Models_agg, function(x){
  
  # Define confidence intervals to find max for graph bounds
  upper_all <- mapply(FUN=function(att,se){att+(1.96*se)},
                      att=x[["agg_all"]][["att.egt"]],
                      se=x[["agg_all"]][["se.egt"]],
                      SIMPLIFY=T)
  upper_young <- mapply(FUN=function(att,se){att+(1.96*se)},
                      att=x[["agg_young"]][["att.egt"]],
                      se=x[["agg_young"]][["se.egt"]],
                      SIMPLIFY=T)
  upper_old <- mapply(FUN=function(att,se){att+(1.96*se)},
                      att=x[["agg_old"]][["att.egt"]],
                      se=x[["agg_old"]][["se.egt"]],
                      SIMPLIFY=T)
  lower_all <- mapply(FUN=function(att,se){att-(1.96*se)},
                      att=x[["agg_all"]][["att.egt"]],
                      se=x[["agg_all"]][["se.egt"]],
                      SIMPLIFY=T)
  lower_young <- mapply(FUN=function(att,se){att-(1.96*se)},
                        att=x[["agg_young"]][["att.egt"]],
                        se=x[["agg_young"]][["se.egt"]],
                        SIMPLIFY=T)
  lower_old <- mapply(FUN=function(att,se){att-(1.96*se)},
                      att=x[["agg_old"]][["att.egt"]],
                      se=x[["agg_old"]][["se.egt"]],
                      SIMPLIFY=T)
  
  max <- max(max(upper_all),max(upper_young),max(upper_old))
  min <- min(min(lower_all),min(lower_young),min(lower_old))
    
  all <- ggdid(x[["agg_all"]], theming=FALSE, legend=FALSE, title="All Hospitalists") + 
  labs(caption=paste0("p-value= ",round(x$p_all,3))) +
  theme_bw() +
  scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 17, family="lm"))
  
  young <- ggdid(x[["agg_young"]], theming=FALSE, legend=FALSE, title="Hospitalists < 60") + 
    labs(caption=paste0("p-value= ",round(x$p_young,3))) +
    theme_bw() +
    scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
    ylim(1.5*min,1.5*max) +
    theme(legend.position = "none", text = element_text(size = 17, family="lm"))
  
  old <- ggdid(x[["agg_old"]], theming=FALSE, legend=FALSE, title="Hospitalists >= 60") + 
    labs(caption=paste0("p-value= ",round(x$p_old,3))) +
    theme_bw() +
    scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
    ylim(1.5*min,1.5*max) +
    theme(legend.position = "none", text = element_text(size = 17, family="lm"))
  
  list(all=all,young=young,old=old)

  })

plots <- lapply(graphs, function(x){
  ggarrange(
    x$all,
    ggarrange(x$young, x$old,
              ncol=2),
    nrow=2,
    common.legend = TRUE,
    legend="right",
    heights  = c(1.25,1)
  )
})


observe <- Physician_Data %>%
  filter(ever_retire==1) %>%
  select(year, phy_zip1, DocNPI, age, retire, pos_office, claim_count_total)








