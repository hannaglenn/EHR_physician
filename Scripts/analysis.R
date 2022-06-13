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
    ylim(1.8*min,1.5*max) +
    theme(legend.position = "none", text = element_text(size = 17, family="lm"))
  
  old <- ggdid(x[["agg_old"]], theming=FALSE, legend=FALSE, title="Hospitalists >= 60") + 
    labs(caption=paste0("p-value= ",round(x$p_old,3))) +
    theme_bw() +
    scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
    ylim(1.8*min,1.5*max) +
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
  ) %>%
    annotate_figure(
      bottom=text_grob("\nEvent Time", family="lm", hjust=.5, vjust=.5, size=15)
    )
})

# Save plots
ggsave(file="Objects/retire_plot.pdf",plot=plots[[1]], width=10, height=7, units="in")
ggsave(file="Objects/officefrac_plot.pdf",plot=plots[[2]], width=10, height=7, units="in")
ggsave(file="Objects/officeind_plot.pdf",plot=plots[[3]], width=10, height=7, units="in")
ggsave(file="Objects/zip_plot.pdf",plot=plots[[4]], width=10, height=7, units="in")
ggsave(file="Objects/patient_plot.pdf",plot=plots[[5]], width=10, height=7, units="in")
ggsave(file="Objects/claim_plot.pdf",plot=plots[[6]], width=10, height=7, units="in")


# Save specific values to reference in paper
cat(round(Models_agg[[1]][["agg_all"]][["overall.att"]],3),file="Results.tex")


observe <- Physician_Data %>%
  filter(ever_retire==1) %>%
  select(year, phy_zip1, DocNPI, age, retire, pos_office, claim_count_total)








