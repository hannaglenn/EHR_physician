library(did)
library(dplyr)
library(ggplot2)
library(readr)
library(fixest)
library(did2s)
library(ggpubr)
library(tidyr)
library(extrafont)


# ------------------------------------- ANALYSIS  ------------------------------------
#                                       Hanna Glenn, Emory University
#                                       1/31/2022


# This script reads in "Physician_Data.rds" from data5, the final dataset used in my third year paper. 
# The first portion of the script considers different potential estimators to use as the main specification for the paper.
# In this portion, I only consider "retire" as the dependent outcome and I compare estimators. 

# Read in the main data created in "data5_MDPPAS.R"
Physician_Data <- readRDS(paste0(created_data_path,"Physician_Data.rds"))

Physician_Data <- Physician_Data %>%
  mutate(DocNPI=as.double(DocNPI))


## ANALYSIS ---------------------------------------------- #########
## Write results for each variable in a loop and create graphs ----- ##

varlist <- list("retire", "pos_office", "work_in_office", "npi_unq_benes", "claim_per_patient", "only_office")

# Get results for ATTGT 
models <- lapply(varlist, function(x) {
  all <- att_gt(yname = x,                # LHS Variable
    gname = "minyr_EHR",             # First year a unit is treated. (set to 0 if never treated)
    idname = "DocNPI",               # ID
    tname = "year",                  # Time Variable
    # xformla = NULL                 # No covariates
    xformla = ~grad_year,            # Time-invariant controls
    data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0) else (if (x=="pos_office" | x=="work_in_office" | x=="only_office") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1)),
    est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
    control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
    clustervars = "DocNPI",          # Cluster Variables          
    anticipation=0,
    base_period = "varying" # can set a number of years to account for anticipation effects
  )
  
  young <- att_gt(yname = x,                # LHS Variable
         gname = "minyr_EHR",             # First year a unit is treated. (set to 0 if never treated)
         idname = "DocNPI",               # ID
         tname = "year",                  # Time Variable
         # xformla = NULL                 # No covariates
         xformla = ~grad_year,            # Time-invariant controls
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0 & max_age<60) else (if (x=="pos_office" | x=="pos_office_prior" | x=="pos_office_noprior" | x=="work_in_office" | x=="change_zip" | x=="pos_opd") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & max_age<60) else if (x=="npi_unq_benes_noDA") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & ever_Phys_DA==0 & max_age<60) else if (x=="npi_unq_benes_DA") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & ever_Phys_DA==1 & max_age<60) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & max_age<60)),
         est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
         control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
         clustervars = "DocNPI",          # Cluster Variables          
         anticipation=0,
         base_period = "varying" # can set a number of years to account for anticipation effects
  )
  
  old <- att_gt(yname = x,                # LHS Variable
         gname = "minyr_EHR",             # First year a unit is treated. (set to 0 if never treated)
         idname = "DocNPI",               # ID
         tname = "year",                  # Time Variable
         # xformla = NULL                 # No covariates
         xformla = ~grad_year,            # Time-invariant controls
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0 & max_age>=60) else (if (x=="pos_office" | x=="pos_office_prior" | x=="pos_office_noprior" | x=="work_in_office" | x=="change_zip" | x=="pos_opd") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & max_age>=60) else if (x=="npi_unq_benes_noDA") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & ever_Phys_DA==0) else if (x=="npi_unq_benes_DA") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & ever_Phys_DA==1 & max_age>=60) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & max_age>=60)),
         est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
         control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
         clustervars = "DocNPI",          # Cluster Variables          
         anticipation=0,
         base_period = "varying" # can set a number of years to account for anticipation effects
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

graph_data <- lapply(Models_agg, function(x){
  data_allages <- as.data.frame(x[["agg_all"]][["egt"]]) %>%
    dplyr::rename(year=1) %>%
    cbind(as.data.frame(x[["agg_all"]][["att.egt"]])) %>%
    dplyr::rename(att=2) %>%
    cbind(as.data.frame(x[["agg_all"]][["se.egt"]])) %>%
    dplyr::rename(se=3) %>%
    mutate(upper=att+(1.96*se),
           lower=att-(1.96*se),
           group="All",
           year=as.factor(year)
    )
  
  data_young <- as.data.frame(x[["agg_young"]][["egt"]]) %>%
    dplyr::rename(year=1) %>%
    cbind(as.data.frame(x[["agg_young"]][["att.egt"]])) %>%
    dplyr::rename(att=2) %>%
    cbind(as.data.frame(x[["agg_young"]][["se.egt"]])) %>%
    dplyr::rename(se=3) %>%
    mutate(upper=att+(1.96*se),
           lower=att-(1.96*se),
           group="Age < 60",
           year=as.factor(year))
  
  data_old <- as.data.frame(x[["agg_old"]][["egt"]]) %>%
    dplyr::rename(year=1) %>%
    cbind(as.data.frame(x[["agg_old"]][["att.egt"]])) %>%
    dplyr::rename(att=2) %>%
    cbind(as.data.frame(x[["agg_old"]][["se.egt"]])) %>%
    dplyr::rename(se=3) %>%
    mutate(upper=att+(1.96*se),
           lower=att-(1.96*se),
           group="Age >= 60",
           year=as.factor(year))

  
  data <- rbind(data_allages, data_young, data_old)
  
  p_all <- x[["p_all"]]
  p_young <- x[["p_young"]]
  p_old <- x[["p_old"]]
  
  list(data, p_all, p_young, p_old)
})

dodge <- position_dodge(width=0.3) 
graphs <- lapply(graph_data, function(x){
  
  all_data <- x[[1]] %>%
    filter(group=="All")
  
  ages_data <- x[[1]] %>%
    filter(group!="All")
  
  all <- ggplot(all_data, aes(year, att)) +  
    geom_vline(xintercept="0", linetype="dashed", colour="red") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width=.0, size=1.1) +
    geom_point(size=2.5) +
    theme_bw() +
    theme_light() +
    geom_hline(yintercept=0, linetype="dashed") +
    theme(legend.position = "none", text = element_text(size = 15)) +
    xlab("") + ylab("Point Estimate and 95% CI\n") +
    geom_vline(xintercept=0, linetype="dashed", color="red") +
    theme(panel.grid.major.x = element_blank() ,
          panel.grid.major.y = element_line(size=.05, color="lightgray" )) +
    labs(caption=paste0("pre-trends test p-value: ",round(x[[2]],2))) + 
    theme(plot.caption=element_text(hjust = 0, size=15))
  
  ages <- ggplot(ages_data, aes(year, att, color=group)) +  
    geom_vline(xintercept="0", linetype="dashed", colour="red") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width=.0, size=1.1, position=dodge) +
    geom_point(size=2.5, position=dodge) +
    theme_bw() +
    theme_light() +
    geom_hline(yintercept=0, linetype="dashed") +
    theme(text = element_text(size = 15)) +
    xlab("") + ylab("Point Estimate and 95% CI\n") +
    geom_vline(xintercept=0, linetype="dashed", color="red") +
    theme(panel.grid.major.x = element_blank() ,
          panel.grid.major.y = element_line(size=.05, color="lightgray" )) +
    paletteer::scale_colour_paletteer_d("ggthemes::excel_Badge") +
    theme(legend.position="bottom") + 
    labs(color='Age Group', caption=paste0("pre-trends test p-value (young): ",round(x[[3]],2),
                                           "\npre-trends test p-value (old): ", round(x[[4]],2))) + 
    theme(plot.caption=element_text(hjust = 0, size=15))
  
  list(all=all, ages=ages)
})

# Save plots -------------------------
ggsave(file="Objects/retire_plot_all.pdf",plot=graphs[[1]][["all"]], width=10, height=4, units="in")
ggsave(file="Objects/retire_plot_ages.pdf",plot=graphs[[1]][["ages"]], width=10, height=4.5, units="in")
ggsave(file="Objects/officefrac_plot_all.pdf",plot=graphs[[2]][["all"]], width=10, height=4, units="in")
ggsave(file="Objects/officefrac_plot_ages.pdf",plot=graphs[[2]][["ages"]], width=10, height=4.5, units="in")
ggsave(file="Objects/officeind_plot_all.pdf",plot=graphs[[3]][["all"]], width=10, height=4, units="in")
ggsave(file="Objects/officeind_plot_ages.pdf",plot=graphs[[3]][["ages"]], width=10, height=4.5, units="in")
ggsave(file="Objects/patient_plot_all.pdf",plot=graphs[[4]][["all"]], width=10, height=4, units="in")
ggsave(file="Objects/patient_plot_ages.pdf",plot=graphs[[4]][["ages"]], width=10, height=4.5, units="in")
ggsave(file="Objects/claim_per_patient_plot_all.pdf",plot=graphs[[5]][["all"]], width=10, height=4, units="in")
ggsave(file="Objects/claim_per_patient_plot_ages.pdf",plot=graphs[[5]][["ages"]], width=10, height=4.5, units="in")




plots <- lapply(graphs, function(x){
  ggarrange(
    x$all,
    x$ages,
    nrow=2,
    common.legend = TRUE,
    legend="right",
    align="v",
    heights  = c(1,1)
  ) %>%
    annotate_figure(
      bottom=text_grob("Year Relative to Exposure", hjust=.5, vjust=.5, size=15)
    )
})


# Save plots -------------------------
ggsave(file="Objects/retire_plot.pdf",plot=plots[[1]], width=10, height=7, units="in")
ggsave(file="Objects/officefrac_plot.pdf",plot=plots[[2]], width=10, height=7, units="in")
ggsave(file="Objects/officeind_plot.pdf",plot=plots[[5]], width=10, height=7, units="in")
ggsave(file="Objects/patient_plot.pdf",plot=plots[[7]], width=10, height=7, units="in")
ggsave(file="Objects/claim_per_patient_plot.pdf",plot=plots[[8]], width=10, height=7, units="in")

# Save the different plots separately for presentations
ggsave(file="Objects/Presentation_retire_all.pdf",plot=graphs[[1]]$all, width=10, height=7, units="in")
ggsave(file="Objects/Presentation_retire_ages.pdf",plot=graphs[[1]]$ages, width=10, height=7, units="in")

ggsave(file="Objects/Presentation_office_all.pdf",plot=graphs[[4]]$all, width=10, height=7, units="in")
ggsave(file="Objects/Presentation_office_ages.pdf",plot=graphs[[4]]$ages, width=10, height=7, units="in")


ggsave(file="Objects/Presentation_zip_all.pdf",plot=graphs[[5]]$all, width=10, height=7, units="in")
ggsave(file="Objects/Presentation_zip_ages.pdf",plot=graphs[[5]]$ages, width=10, height=7, units="in")

ggsave(file="Objects/Presentation_patients_all.pdf",plot=graphs[[6]]$all, width=10, height=7, units="in")
ggsave(file="Objects/Presentation_patients_ages.pdf",plot=graphs[[6]]$ages, width=10, height=7, units="in")

ggsave(file="Objects/Presentation_claimperpatient_all.pdf",plot=graphs[[8]]$all, width=10, height=7, units="in")




#Save plot of patient count dis-aggregated
ggdid(models[[5]][["all"]]) + scale_color_manual(labels = c("Pre", "Post"), values=c("#000000", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(colour="black",size = 15)) + labs(title="Dis-aggregated Effects of EHR Exposure on Patient Count")

ggsave(filename = "Objects/patient_group.pdf", width=10, height=12, units="in")


## analysis of specific types of claims (only for 2013-2015)
varlist <- list("initial_hosp_inpat", "subs_hosp_inpat", "hosp_disch_day", "new_patient_office", "est_patient_office", "hosp_obs_care",
                "subs_obs_care", "crit_care_delivery")

subset_years_data <- Physician_Data %>%
  filter(year>=2013 & year<=2015)

# Get results for ATTGT 
models <- lapply(varlist, function(x) {
  all <- att_gt(yname = x,                # LHS Variable
                gname = "minyr_EHR",             # First year a unit is treated. (set to 0 if never treated)
                idname = "DocNPI",               # ID
                tname = "year",                  # Time Variable
                # xformla = NULL                 # No covariates
                xformla = ~grad_year,            # Time-invariant controls
                data= dplyr::filter(subset_years_data, ever_retire==0),
                est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
                control_group = "nevertreated", # Set the control group to notyettreated or nevertreated
                clustervars = "DocNPI",          # Cluster Variables          
                anticipation=0,
                base_period = "varying" # can set a number of years to account for anticipation effects
  )
  list(all=all)
})


# Aggregate the previous results
Models_agg <- lapply(models, function(x){
  agg_all <- aggte(x$all, type = "dynamic",na.rm=T)
  p_all <- x$all$Wpval
  
  list(agg_all=agg_all, p_all=p_all)
})

graph_data <- lapply(Models_agg, function(x){
  data_allages <- as.data.frame(x[["agg_all"]][["egt"]]) %>%
    dplyr::rename(year=1) %>%
    cbind(as.data.frame(x[["agg_all"]][["att.egt"]])) %>%
    dplyr::rename(att=2) %>%
    cbind(as.data.frame(x[["agg_all"]][["se.egt"]])) %>%
    dplyr::rename(se=3) %>%
    mutate(upper=att+(1.96*se),
           lower=att-(1.96*se),
           group="All",
           year=as.factor(year)
    )
  
  
  data <- data_allages
  
  p_all <- x[["p_all"]]
  
  list(data, p_all)
})

dodge <- position_dodge(width=0.3) 
graphs <- lapply(graph_data, function(x){
  
  all_data <- x[[1]] %>%
    filter(group=="All")
  
  all <- ggplot(all_data, aes(year, att)) +  
    geom_vline(xintercept="0", linetype="dashed", colour="red") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width=.0, size=1.1) +
    geom_point(size=2.5) +
    theme_bw() +
    theme_light() +
    geom_hline(yintercept=0, linetype="dashed") +
    theme(legend.position = "none", text = element_text(size = 15)) +
    xlab("") + ylab("Point Estimate and 95% CI\n") +
    geom_vline(xintercept=0, linetype="dashed", color="red") +
    theme(panel.grid.major.x = element_blank() ,
          panel.grid.major.y = element_line(size=.05, color="lightgray" )) +
    labs(caption=paste0("pre-trends test p-value: ",round(x[[2]],2))) + 
    theme(plot.caption=element_text(hjust = 0, size=15))
  
  list(all=all)
})

chart_data <- data.frame(est=as.numeric(), upper = as.numeric(), lower=as.numeric(), outcome = as.character())
for (j in 1:length(Models_agg)){
  est <- Models_agg[[j]][["agg_all"]][["overall.att"]]
  se <- Models_agg[[j]][["agg_all"]][["overall.se"]]
  outcome <- Models_agg[[j]][["agg_all"]][["DIDparams"]][["yname"]]
  
  upper <- est+(1.96*se)
  lower <- est-(1.96*se)
  
  chart_data <- chart_data %>%
    add_row(est=est, upper=upper, lower=lower, outcome=outcome) %>%
    mutate(outcome = ifelse(outcome=="crit_care_delivery", "Critical Care Delivery", outcome),
           outcome = ifelse(outcome=="est_patient_office", "Established Patient: Office", outcome),
           outcome = ifelse(outcome=="hosp_disch_day", "Discharge: Hospital", outcome),
           outcome = ifelse(outcome=="hosp_obs_care", "Observation: Hospital", outcome),
           outcome = ifelse(outcome=="initial_hosp_inpat", "Initial Care: Hospital", outcome),
           outcome = ifelse(outcome=="new_patient_office", "New Patient: Office", outcome),
           outcome = ifelse(outcome=="subs_hosp_inpat", "Subsequent Care: Hospital", outcome),
           outcome = ifelse(outcome=="subs_obs_care", "Subsequent Observation: Hospital", outcome))
}

chart <- ggplot(chart_data, aes(y=outcome, x=est)) + geom_point() + geom_vline(xintercept = 0, color="red", linetype = "dashed") +
  geom_errorbar(aes(xmin = lower, xmax = upper), width=.0, size=1.1) + 
  theme_bw() + theme_light() + theme(text = element_text(size = 15)) +
  ylab("Type of Claim as Fraction of all Claims") + xlab("\nEstimate and 95% CI") +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(size=.05, color="lightgray" ))

ggsave(chart, filename = "Objects/claim_type_results.pdf")





