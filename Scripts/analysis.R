library(did)
library(dplyr)
library(ggplot2)
library(readr)
library(fixest)
library(did2s)
library(ggpubr)
library(extrafont)


# ------------------------------------- ANALYSIS  ------------------------------------
#                                       Hanna Glenn, Emory University
#                                       1/31/2022

font_import()
loadfonts(device="win")
windowsFonts(A = windowsFont("Times New Roman"))

# This script reads in "Physician_Data.rds" from data5, the final dataset used in my third year paper. 
# The first portion of the script considers different potential estimators to use as the main specification for the paper.
# In this portion, I only consider "retire" as the dependent outcome and I compare estimators. 

# Read in the main data created in "data5_MDPPAS.R"
Physician_Data <- readRDS(paste0(created_data_path,"Physician_Data.rds"))


## ANALYSIS ---------------------------------------------- #########
## Write results for each variable in a loop and create graphs ----- ##

varlist <- list("retire", "pos_office_prior", "pos_office_noprior", "work_in_office", "change_zip", "npi_unq_benes", "claim_count_total", "claim_per_patient")



# Get results for ATTGT 
models <- lapply(varlist, function(x) {
  all <- att_gt(yname = x,                # LHS Variable
    gname = "minyr_EHR",             # First year a unit is treated. (set to 0 if never treated)
    idname = "DocNPI",               # ID
    tname = "year",                  # Time Variable
    # xformla = NULL                 # No covariates
    xformla = ~grad_year,            # Time-invariant controls
    data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0) else (if (x=="pos_office_prior" | x=="pos_office_noprior" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1)),
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
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0 & max_age<60) else (if (x=="pos_office_prior" | x=="pos_office_noprior" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & max_age<60) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & max_age<60)),
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
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0 & max_age>=60) else (if (x=="pos_office_prior" | x=="pos_office_noprior" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & max_age>=60) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & max_age>=60)),
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
  
  list(data)
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
    xlab("\n") + ylab("Point Estimate and 95% CI\n") +
    geom_vline(xintercept=0, linetype="dashed", color="red") +
    theme(panel.grid.major.x = element_blank() ,
          panel.grid.major.y = element_line(size=.05, color="lightgray" ))
  
  ages <- ggplot(ages_data, aes(year, att, color=group)) +  
    geom_vline(xintercept="0", linetype="dashed", colour="red") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width=.0, size=1.1, position=dodge) +
    geom_point(size=2.5, position=dodge) +
    theme_bw() +
    theme_light() +
    geom_hline(yintercept=0, linetype="dashed") +
    theme(text = element_text(size = 15)) +
    xlab("\n") + ylab("Point Estimate and 95% CI\n") +
    geom_vline(xintercept=0, linetype="dashed", color="red") +
    theme(panel.grid.major.x = element_blank() ,
          panel.grid.major.y = element_line(size=.05, color="lightgray" )) +
    paletteer::scale_colour_paletteer_d("ggthemes::excel_Badge") +
    theme(legend.position="bottom") + labs(color='Age Group')
  
  list(all=all, ages=ages)
})



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

# Save plots
ggsave(file="Objects/retire_plot.pdf",plot=plots[[1]], width=10, height=7, units="in")
ggsave(file="Objects/officefrac_prior_plot.pdf",plot=plots[[2]], width=10, height=7, units="in")
ggsave(file="Objects/officefrac_noprior_plot.pdf",plot=plots[[3]], width=10, height=7, units="in")
ggsave(file="Objects/officeind_plot.pdf",plot=plots[[4]], width=10, height=7, units="in")
ggsave(file="Objects/zip_plot.pdf",plot=plots[[5]], width=10, height=7, units="in")
ggsave(file="Objects/patient_plot.pdf",plot=plots[[6]], width=10, height=7, units="in")
ggsave(file="Objects/claim_plot.pdf",plot=plots[[7]], width=10, height=7, units="in")
ggsave(file="Objects/claim_per_patient_plot.pdf",plot=plots[[8]], width=10, height=7, units="in")

# Save the different plots separately for presentations
ggsave(file="Objects/Presentation_retire_all.pdf",plot=plots[[1]]$all, width=10, height=7, units="in")
ggsave(file="Objects/Presentation_retire_ages.pdf",plot=plots[[1]]$ages, width=10, height=7, units="in")

ggsave(file="Objects/Presentation_frac_prior_all.pdf",plot=plots[[2]]$all, width=10, height=7, units="in")

ggsave(file="Objects/Presentation_frac_noprior_all.pdf",plot=plots[[3]]$all, width=10, height=7, units="in")
ggsave(file="Objects/Presentation_office_all.pdf",plot=plots[[4]]$all, width=10, height=7, units="in")

ggsave(file="Objects/Presentation_patients_all.pdf",plot=plots[[6]]$all, width=10, height=7, units="in")
ggsave(file="Objects/Presentation_patients_ages.pdf",plot=plots[[6]]$ages, width=10, height=7, units="in")

ggsave(file="Objects/Presentation_claimperpatient_all.pdf",plot=plots[[8]]$all, width=10, height=7, units="in")




#Save plot of patient count dis-aggregated
ggdid(models[[6]][["all"]]) + scale_color_manual(labels = c("Pre", "Post"), values=c("#000000", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(colour="black",size = 15)) + labs(title="Dis-aggregated Effects of EHR Exposure on Patient Count", colour=)

ggsave(filename = "Objects/patient_group.pdf", width=10, height=12, units="in")



# Create data frame of overall ATT values

ATT_all <- lapply(Models_agg, function(x){
  c(x[["agg_all"]][["DIDparams"]][["yname"]],
    round(x[["agg_all"]][["overall.att"]],5), 
    round(x[["agg_all"]][["overall.se"]], 5),
    round(x[["agg_all"]][["overall.att"]]-(1.959*x[["agg_all"]][["overall.se"]]),5),
    round(x[["agg_all"]][["overall.att"]]+(1.959*x[["agg_all"]][["overall.se"]]),5),
    "Any",
    "Main")
})

ATT_young <- lapply(Models_agg, function(x){
  c(x[["agg_young"]][["DIDparams"]][["yname"]],
    round(x[["agg_young"]][["overall.att"]],5), 
    round(x[["agg_young"]][["overall.se"]], 5),
    round(x[["agg_young"]][["overall.att"]]-(1.959*x[["agg_young"]][["overall.se"]]),5),
    round(x[["agg_young"]][["overall.att"]]+(1.959*x[["agg_young"]][["overall.se"]]),5),
    "< 60",
    "Main")
})

ATT_old <- lapply(Models_agg, function(x){
  c(x[["agg_old"]][["DIDparams"]][["yname"]],
    round(x[["agg_old"]][["overall.att"]],5), 
    round(x[["agg_old"]][["overall.se"]], 5),
    round(x[["agg_old"]][["overall.att"]]-(1.959*x[["agg_old"]][["overall.se"]]),5),
    round(x[["agg_old"]][["overall.att"]]+(1.959*x[["agg_old"]][["overall.se"]]),5),
    ">= 60",
    "Main")
})

# Convert to data frame
values <- as.data.frame(do.call(rbind, ATT_all)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

values_young <- as.data.frame(do.call(rbind, ATT_young)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

values_old <- as.data.frame(do.call(rbind, ATT_old)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

# Merge all ages and main specification results
merged <- rbind(values, values_young, values_old) %>%
  dplyr::mutate(ATT=as.numeric(ATT),
                Lower=as.numeric(Lower),
                Upper=as.numeric(Upper)) %>%
  dplyr::mutate(Variable=ifelse(Variable=="retire","Retire",Variable),
                Variable=ifelse(Variable=="work_in_office","Prob. Working in Office",Variable),
                Variable=ifelse(Variable=="pos_office","Frac. Patients in Office",Variable),
                Variable=ifelse(Variable=="npi_unq_benes","Number Patients",Variable),
                Variable=ifelse(Variable=="claim_count_total","Claim Count",Variable),
                Variable=ifelse(Variable=="change_zip","Prob. Change Zip",Variable))

saveRDS(merged,file="CreatedData/main_ATT.rds")

