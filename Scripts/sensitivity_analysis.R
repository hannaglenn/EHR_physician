library(showtext)
library(ggplot2)
library(did)
library(ggpubr)
library(fixest)
library(did2s)
library(readxl)
library(HonestDiD)
library(mvtnorm)




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
varlist <- list("retire", "pos_office", "work_in_office", "change_zip", "npi_unq_benes", "claim_count_total")

# Create List of Results for each age group
models_LI <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR_int",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR_int>0) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR_int>0 & ever_retire==0) else dplyr::filter(Physician_Data, minyr_EHR_int>0 & ever_retire==0 & never_newnpi==1)),
         est_method = "dr",
         control_group = "notyettreated")
  
})

models_LI_young <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR_int",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR_int>0 & max_age<60) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR_int>0 & ever_retire==0 & max_age<60) else dplyr::filter(Physician_Data, minyr_EHR_int>0 & ever_retire==0 & never_newnpi==1 & max_age<60)),
         est_method = "dr",
         control_group = "notyettreated")
  
})

models_LI_old <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR_int",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR_int>0 & max_age>=60) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR_int>0 & ever_retire==0 & max_age>=60) else dplyr::filter(Physician_Data, minyr_EHR_int>0 & ever_retire==0 & never_newnpi==1 & max_age>=60)),
         est_method = "dr",
         control_group = "notyettreated")
  
})

# Translate to single ATT value for each age group
Simple_LI <- lapply(models_LI, function(x){
  aggte(x, type = "simple")
})

Simple_LI_young <- lapply(models_LI_young, function(x){
  aggte(x, type = "simple")
})

Simple_LI_old <- lapply(models_LI_old, function(x){
  aggte(x, type = "simple")
})

# Save relevant information for each age group
ATT_LI <- lapply(Simple_LI, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    "Any",
    "Low-Integration")
})

ATT_LI_young <- lapply(Simple_LI_young, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    "< 60",
    "Low-Integration")
})

ATT_LI_old <- lapply(Simple_LI_old, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    ">= 60",
    "Low-Integration")
})

# Convert to data frame for each age group 
LI_values <- as.data.frame(do.call(rbind, ATT_LI)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

LI_values_young <- as.data.frame(do.call(rbind, ATT_LI_young)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

LI_values_old <- as.data.frame(do.call(rbind, ATT_LI_old)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)
  
# Merge all ages and main specification results
LI_merged <- rbind(LI_values, LI_values_young, LI_values_old, main_ATT) %>%
  dplyr::mutate(ATT=as.numeric(ATT),
                Lower=as.numeric(Lower),
                Upper=as.numeric(Upper)) %>%
  dplyr::mutate(Variable=ifelse(Variable=="retire","Retire",Variable),
                Variable=ifelse(Variable=="work_in_office","Prob. Working in Office",Variable),
                Variable=ifelse(Variable=="pos_office","Frac. Patients in Office",Variable),
                Variable=ifelse(Variable=="npi_unq_benes","Number Patients",Variable),
                Variable=ifelse(Variable=="claim_count_total","Claim Count",Variable),
                Variable=ifelse(Variable=="change_zip","Prob. Change Zip",Variable))


# Create Graph
dodge <- position_dodge(width=.75)
small_values <- ggplot(dplyr::filter(LI_merged, Variable!="Claim Count" & Variable!="Number Patients" & Age=="Any"),
                       aes(x=Variable, y=ATT, color=Specification)) +
  geom_point(position=dodge) +
  geom_errorbar(aes(ymax=Upper,ymin=Lower, width=.5),position = dodge) + 
  geom_hline(yintercept=0, linetype="dashed", size=.1) +
  coord_flip() + theme_bw() + 
  theme(text=element_text(size=17, family="lm")) +
  xlab(" ") + ylab(" ") +
  scale_color_manual(values=c(Main="#999999", "Low-Integration"="#E69F00"))


large_values <- ggplot(dplyr::filter(LI_merged, (Variable=="Claim Count" | Variable=="Number Patients") & Age=="Any"),
                       aes(x=Variable, y=ATT, color=Specification)) +
  geom_point(position=dodge) +
  geom_errorbar(aes(ymax=Upper,ymin=Lower, width=.3),position = dodge)  +
  geom_hline(yintercept=0, linetype="dashed", size=.1) +
  coord_flip() + theme_bw() + 
  theme(text=element_text(size=17, family="lm")) +
  xlab(" ") + ylab("\nATT and 95% CI") +
  scale_color_manual(values=c(Main="#999999", "Low-Integration"="#E69F00"))

ggarrange(
  small_values,
  large_values,
  nrow=2,
  common.legend = TRUE,
  legend="right",
  heights  = c(1,1),
  align="v"
)

# Save graph
ggsave("Objects/LI_graph.pdf", height=7, width=10, units = "in")





###  ANTICIPATION ########################################################################

varlist <- list("retire", "pos_office", "work_in_office", "change_zip", "npi_unq_benes", "claim_count_total")

models_anticipation <- lapply(varlist, function(x) {
  att_gt(yname = x,
        gname = "minyr_EHR",
        idname = "DocNPI",
        tname = "year",
        xformla = ~grad_year,
        data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1)),
        est_method = "dr",
        control_group = "notyettreated",
        anticipation = 1)
                       
})

Simple_anticipation <- lapply(models_anticipation, function(x){
  aggte(x, type = "simple")
})

ATT_anticipation <- lapply(Simple_anticipation, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    "Any",
    "1-Year Anticipation")
})


anticipation_values <- as.data.frame(do.call(rbind, ATT_anticipation)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7) %>%
  dplyr::mutate(Variable=ifelse(Variable=="retire","Retire",Variable),
                Variable=ifelse(Variable=="work_in_office","Prob. Working in Office",Variable),
                Variable=ifelse(Variable=="pos_office","Frac. Patients in Office",Variable),
                Variable=ifelse(Variable=="npi_unq_benes","Number Patients",Variable),
                Variable=ifelse(Variable=="claim_count_total","Claim Count",Variable),
                Variable=ifelse(Variable=="change_zip","Prob. Change Zip",Variable))

anticipation_merged <- rbind(anticipation_values, main_ATT) %>%
  dplyr::mutate(ATT=as.numeric(ATT),
                Lower=as.numeric(Lower),
                Upper=as.numeric(Upper))


 # Create Graph
dodge <- position_dodge(width=.75)
small_values <- ggplot(dplyr::filter(anticipation_merged, Variable!="Claim Count" & Variable!="Number Patients" & Age=="Any"),
       aes(x=Variable, y=ATT, color=Specification)) +
  geom_point(position=dodge) +
  geom_errorbar(aes(ymax=Upper,ymin=Lower, width=.5),position = dodge) + 
  geom_hline(yintercept=0, linetype="dashed", size=.1) +
  coord_flip() + theme_bw() + 
  theme(text=element_text(size=17, family="lm")) +
  xlab(" ") + ylab(" ") +
  scale_color_manual(values=c(Main="#999999","1-Year Anticipation"="#E69F00"))


large_values <- ggplot(dplyr::filter(anticipation_merged, (Variable=="Claim Count" | Variable=="Number Patients") & Age=="Any"),
       aes(x=Variable, y=ATT, color=Specification)) +
  geom_point(position=dodge) +
  geom_errorbar(aes(ymax=Upper,ymin=Lower, width=.3),position = dodge)  +
  geom_hline(yintercept=0, linetype="dashed", size=.1) +
  coord_flip() + theme_bw() + 
  theme(text=element_text(size=17, family="lm")) +
  xlab(" ") + ylab("\nATT and 95% CI") +
  scale_color_manual(values=c(Main="#999999","1-Year Anticipation"="#E69F00"))

ggarrange(
  small_values,
  large_values,
  nrow=2,
  common.legend = TRUE,
  legend="right",
  heights  = c(1,1),
  align="v"
)

ggsave("Objects/anticipation_graph.pdf", height=7, width=10, units = "in")



### LIMITED YEARS #####################################################################################
varlist <- list("retire", "pos_office", "work_in_office", "change_zip", "npi_unq_benes", "claim_count_total")

# Create List of Results for each age group
models_years <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0 & year<2016) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & year<2016) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & year<2016)),
         est_method = "dr",
         control_group = "notyettreated")
  
})



# Translate to single ATT value for each age group
Simple_years <- lapply(models_years, function(x){
  aggte(x, type = "simple")
})


# Save relevant information for each age group
ATT_years <- lapply(Simple_years, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    "Any",
    "Limited Years: 2009-2015")
})


# Convert to data frame for each age group 
years_values <- as.data.frame(do.call(rbind, ATT_years)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)


# Merge all ages and main specification results
years_merged <- rbind(years_values, main_ATT) %>%
  dplyr::mutate(ATT=as.numeric(ATT),
                Lower=as.numeric(Lower),
                Upper=as.numeric(Upper)) %>%
  dplyr::mutate(Variable=ifelse(Variable=="retire","Retire",Variable),
                Variable=ifelse(Variable=="work_in_office","Prob. Working in Office",Variable),
                Variable=ifelse(Variable=="pos_office","Frac. Patients in Office",Variable),
                Variable=ifelse(Variable=="npi_unq_benes","Number Patients",Variable),
                Variable=ifelse(Variable=="claim_count_total","Claim Count",Variable),
                Variable=ifelse(Variable=="change_zip","Prob. Change Zip",Variable))


# Create Graph
dodge <- position_dodge(width=.75)
small_values <- ggplot(dplyr::filter(years_merged, Variable!="Claim Count" & Variable!="Number Patients" & Age=="Any"),
                       aes(x=Variable, y=ATT, color=Specification)) +
  geom_point(position=dodge) +
  geom_errorbar(aes(ymax=Upper,ymin=Lower, width=.5),position = dodge) + 
  geom_hline(yintercept=0, linetype="dashed", size=.1) +
  coord_flip() + theme_bw() + 
  theme(text=element_text(size=17, family="lm")) +
  xlab(" ") + ylab(" ") +
  scale_color_manual(values=c(Main="#999999","Limited Years: 2009-2015"="#E69F00"))


large_values <- ggplot(dplyr::filter(years_merged, (Variable=="Claim Count" | Variable=="Number Patients") & Age=="Any"),
                       aes(x=Variable, y=ATT, color=Specification)) +
  geom_point(position=dodge) +
  geom_errorbar(aes(ymax=Upper,ymin=Lower, width=.3),position = dodge)  +
  geom_hline(yintercept=0, linetype="dashed", size=.1) +
  coord_flip() + theme_bw() + 
  theme(text=element_text(size=17, family="lm")) +
  xlab(" ") + ylab("\nATT and 95% CI") +
  scale_color_manual(values=c(Main="#999999", "Limited Years: 2009-2015"="#E69F00"))

ggarrange(
  small_values,
  large_values,
  nrow=2,
  common.legend = TRUE,
  legend="right",
  heights  = c(1,1),
  align="v"
)

# Save graph
ggsave("Objects/years_graph.pdf", height=7, width=10, units = "in")





### PHYSICIANS NOT EXPOSED TO DATA ASSISTANTS #####################################################################

varlist <- list("retire", "pos_office", "work_in_office", "change_zip", "npi_unq_benes", "claim_count_total")

# Create List of Results for each age group
models_DA <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0 & works_with_DA==0) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & works_with_DA==0) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & works_with_DA==0)),
         est_method = "dr",
         control_group = "notyettreated")
  
})


# Translate to single ATT value for each age group
Simple_DA <- lapply(models_DA, function(x){
  aggte(x, type = "simple")
})


# Save relevant information for each age group
ATT_DA <- lapply(Simple_DA, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    "Any",
    "No Data Assistants")
})


# Convert to data frame for each age group 
DA_values <- as.data.frame(do.call(rbind, ATT_DA)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)


# Merge all ages and main specification results
DA_merged <- rbind(DA_values, main_ATT) %>%
  dplyr::mutate(ATT=as.numeric(ATT),
                Lower=as.numeric(Lower),
                Upper=as.numeric(Upper)) %>%
  dplyr::mutate(Variable=ifelse(Variable=="retire","Retire",Variable),
                Variable=ifelse(Variable=="work_in_office","Prob. Working in Office",Variable),
                Variable=ifelse(Variable=="pos_office","Frac. Patients in Office",Variable),
                Variable=ifelse(Variable=="npi_unq_benes","Number Patients",Variable),
                Variable=ifelse(Variable=="claim_count_total","Claim Count",Variable),
                Variable=ifelse(Variable=="change_zip","Prob. Change Zip",Variable))


# Create Graph
dodge <- position_dodge(width=.75)
small_values <- ggplot(dplyr::filter(DA_merged, Variable!="Claim Count" & Variable!="Number Patients" & Age=="Any"),
                       aes(x=Variable, y=ATT, color=Specification)) +
  geom_point(position=dodge) +
  geom_errorbar(aes(ymax=Upper,ymin=Lower, width=.5),position = dodge) + 
  geom_hline(yintercept=0, linetype="dashed", size=.1) +
  coord_flip() + theme_bw() + 
  theme(text=element_text(size=17, family="lm")) +
  xlab(" ") + ylab(" ") +
  scale_color_manual(values=c(Main="#999999","No Data Assistants"="#E69F00"))


large_values <- ggplot(dplyr::filter(DA_merged, (Variable=="Claim Count" | Variable=="Number Patients") & Age=="Any"),
                       aes(x=Variable, y=ATT, color=Specification)) +
  geom_point(position=dodge) +
  geom_errorbar(aes(ymax=Upper,ymin=Lower, width=.3),position = dodge)  +
  geom_hline(yintercept=0, linetype="dashed", size=.1) +
  coord_flip() + theme_bw() + 
  theme(text=element_text(size=17, family="lm")) +
  xlab(" ") + ylab("\nATT and 95% CI") +
  scale_color_manual(values=c(Main="#999999", "No Data Assistants"="#E69F00"))

ggarrange(
  small_values,
  large_values,
  nrow=2,
  common.legend = TRUE,
  legend="right",
  heights  = c(1,1),
  align="v"
)

# Save graph
ggsave("Objects/DA_graph.pdf", height=7, width=10, units = "in")



## DIFFERENT ESTIMATORS ############################################################################################
varlist <- list("retire", "pos_office", "work_in_office", "change_zip", "npi_unq_benes", "claim_count_total")

# Stacked Regression ###
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


models_stacked <- lapply(varlist, function(x){
  feols(xpd(..lhs ~ treated_unit:rel_m1 + treated_unit:rel_0 + treated_unit:rel_p1 + experience | DocNPI:minyr_EHR, ..lhs = x),
        cluster="DocNPI",
        data=stacked_data_1)
})

ATT_stacked <- lapply(models_stacked, function(x){
  c(x$fml[[2]],
    round(x[["coefficients"]][["treated_unit:rel_p1"]],5), 
    round(x[["se"]][["treated_unit:rel_p1"]], 5),
    round(x[["coefficients"]][["treated_unit:rel_p1"]]-(1.959*x[["se"]][["treated_unit:rel_p1"]]),5),
    round(x[["coefficients"]][["treated_unit:rel_p1"]]+(1.959*x[["se"]][["treated_unit:rel_p1"]]),5),
    "Any",
    "Stacked Regression")
})

stacked_values <- as.data.frame(do.call(rbind, ATT_stacked)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)


# 2sDid #####
models_2sdid <- lapply(varlist, function(x){
  did2s(Physician_Data,
        yname = x, 
        first_stage = ~ 0 | DocNPI + year, 
        second_stage = ~rel_m4 + rel_m3 + rel_m2 + rel_0 + rel_p1 +
          rel_p2 + rel_p3 + rel_p4, 
        treatment = "anyEHR_exposed",
        cluster_var="DocNPI",
        bootstrap=TRUE,
        n_bootstraps = 250)
})

agg_2sdid <- lapply(models_2sdid, function(x){
  aggregate(x, agg="(rel_p)")
})

ATT_2sdid <- lapply(agg_2sdid, function(x){
  c("name",
    round(x[[1]],5), 
    round(x[[2]], 5),
    round(x[[1]]-(1.959*x[[2]]),5),
    round(x[[1]]+(1.959*x[[2]]),5),
    "Any",
    "Two-Stage DiD")
})

ATT_2sdid[[1]][[1]] <- "retire"
ATT_2sdid[[2]][[1]] <- "pos_office"
ATT_2sdid[[3]][[1]] <- "work_in_office"
ATT_2sdid[[4]][[1]] <- "change_zip"
ATT_2sdid[[5]][[1]] <- "npi_unq_benes"
ATT_2sdid[[6]][[1]] <- "claim_count_total"

tsdid_values <- as.data.frame(do.call(rbind, ATT_2sdid)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)







estimates_merged <- rbind(stacked_values, tsdid_values, main_ATT) %>%
  dplyr::mutate(ATT=as.numeric(ATT),
                Lower=as.numeric(Lower),
                Upper=as.numeric(Upper),
                SE=as.numeric(SE),
                Variable=as.character(Variable),
                Specification=as.character(Specification)) %>%
  dplyr::mutate(Variable=ifelse(Variable=="retire","Retire",Variable),
                Variable=ifelse(Variable=="work_in_office","Prob. Working in Office",Variable),
                Variable=ifelse(Variable=="pos_office","Frac. Patients in Office",Variable),
                Variable=ifelse(Variable=="npi_unq_benes","Number Patients",Variable),
                Variable=ifelse(Variable=="claim_count_total","Claim Count",Variable),
                Variable=ifelse(Variable=="change_zip","Prob. Change Zip",Variable))

dodge <- position_dodge(width=.75)
small_values <- ggplot(dplyr::filter(estimates_merged, Variable!="Claim Count" & Variable!="Number Patients" & Age=="Any"),
                       aes(x=Variable, y=ATT, color=Specification)) +
  geom_point(position=dodge) +
  geom_errorbar(aes(ymax=Upper,ymin=Lower, width=.5),position = dodge) + 
  geom_hline(yintercept=0, linetype="dashed", size=.1) +
  coord_flip() + theme_bw() + 
  theme(text=element_text(size=17, family="lm")) +
  xlab(" ") + ylab(" ") +
  scale_color_manual(values=c(Main="#999999","Stacked Regression"="#E69F00","Two-Stage DiD"="#56B4E9"))


large_values <- ggplot(dplyr::filter(estimates_merged, (Variable=="Claim Count" | Variable=="Number Patients") & Age=="Any"),
                       aes(x=Variable, y=ATT, color=Specification)) +
  geom_point(position=dodge) +
  geom_errorbar(aes(ymax=Upper,ymin=Lower, width=.3),position = dodge)  +
  geom_hline(yintercept=0, linetype="dashed", size=.1) +
  coord_flip() + theme_bw() + 
  theme(text=element_text(size=17, family="lm")) +
  xlab(" ") + ylab("\nATT and 95% CI") +
  scale_color_manual(values=c(Main="#999999","Stacked Regression"="#E69F00","Two-Stage Did"="#56B4E9"))

ggarrange(
  small_values,
  large_values,
  nrow=2,
  common.legend = TRUE,
  legend="right",
  heights  = c(1,1),
  align="v"
)

# Save graph
ggsave("Objects/estimators_graph.pdf", height=7, width=10, units = "in")



### HETEROGENEITY ANALYSIS: MALE VS. FEMALE #####################################################################

varlist <- list("retire", "pos_office", "work_in_office", "change_zip", "npi_unq_benes", "claim_count_total")

# Create List of Results for each gender
models_male <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0 & female==0) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & female==0) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & female==0)),
         est_method = "dr",
         control_group = "notyettreated")
  
})

models_male_young <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0 & female==0 & max_age<60) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & female==0 & max_age<60) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & female==0 & max_age<60)),
         est_method = "dr",
         control_group = "notyettreated")
  
})

models_male_old <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0 & female==0 & max_age>=60) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & female==0 & max_age>=60) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & female==0 & max_age>=60)),
         est_method = "dr",
         control_group = "notyettreated")
  
})

models_female <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0 & female==1) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & female==1) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & female==1)),
         est_method = "dr",
         control_group = "notyettreated")
  
})

models_female_young <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0 & female==1 & max_age<60) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & female==1 & max_age<60) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & female==1 & max_age<60)),
         est_method = "dr",
         control_group = "notyettreated")
  
})

models_female_old <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0 & female==1 & max_age>=60) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & female==1 & max_age>=60) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & female==1 & max_age>=60)),
         est_method = "dr",
         control_group = "notyettreated")
  
})

# Translate to single ATT value for each gender
Simple_male <- lapply(models_male, function(x){
  aggte(x, type = "simple")
})

Simple_male_young <- lapply(models_male_young, function(x){
  aggte(x, type = "simple")
})

Simple_male_old <- lapply(models_male_old, function(x){
  aggte(x, type = "simple")
})

Simple_female <- lapply(models_female, function(x){
  aggte(x, type = "simple")
})

Simple_female_young <- lapply(models_female_young, function(x){
  aggte(x, type = "simple")
})

Simple_female_old <- lapply(models_female_old, function(x){
  aggte(x, type = "simple")
})

# Save relevant information for each gender
ATT_male <- lapply(Simple_male, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    "Any",
    "Male")
})

ATT_male_young <- lapply(Simple_male_young, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    "< 60",
    "Male")
})

ATT_male_old <- lapply(Simple_male_old, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    ">= 60",
    "Male")
})

ATT_female <- lapply(Simple_female, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    "Any",
    "Female")
})

ATT_female_young <- lapply(Simple_female_young, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    "< 60",
    "Female")
})

ATT_female_old <- lapply(Simple_female_old, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    ">= 60",
    "Female")
})

# Convert to data frame for each age group 
male_values <- as.data.frame(do.call(rbind, ATT_male)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

male_values_young <- as.data.frame(do.call(rbind, ATT_male_young)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

male_values_old <- as.data.frame(do.call(rbind, ATT_male_old)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

female_values <- as.data.frame(do.call(rbind, ATT_female)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

female_values_young <- as.data.frame(do.call(rbind, ATT_female_young)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

female_values_old <- as.data.frame(do.call(rbind, ATT_female_old)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

# Merge all genders and main specification results
gender_merged <- rbind(male_values, male_values_young, male_values_old,female_values, female_values_young, female_values_old, main_ATT) %>%
  dplyr::mutate(ATT=as.numeric(ATT),
                Lower=as.numeric(Lower),
                Upper=as.numeric(Upper)) %>%
  dplyr::mutate(Variable=ifelse(Variable=="retire","Retire",Variable),
                Variable=ifelse(Variable=="work_in_office","Prob. Working in Office",Variable),
                Variable=ifelse(Variable=="pos_office","Frac. Patients in Office",Variable),
                Variable=ifelse(Variable=="npi_unq_benes","Number Patients",Variable),
                Variable=ifelse(Variable=="claim_count_total","Claim Count",Variable),
                Variable=ifelse(Variable=="change_zip","Prob. Change Zip",Variable))


# Create Graph
dodge <- position_dodge(width=.75)
small_values <- ggplot(dplyr::filter(gender_merged, Variable!="Claim Count" & Variable!="Number Patients" & Age=="Any"),
                       aes(x=Variable, y=ATT, color=Specification)) +
  geom_point(position=dodge) +
  geom_errorbar(aes(ymax=Upper,ymin=Lower, width=.5),position = dodge) + 
  geom_hline(yintercept=0, linetype="dashed", size=.1) +
  coord_flip() + theme_bw() + 
  theme(text=element_text(size=17, family="lm")) +
  xlab(" ") + ylab(" ") +
  scale_color_manual(values=c("#999999", "#E69F00","#56B4E9"))


large_values <- ggplot(dplyr::filter(gender_merged, (Variable=="Claim Count" | Variable=="Number Patients") & Age=="Any"),
                       aes(x=Variable, y=ATT, color=Specification)) +
  geom_point(position=dodge) +
  geom_errorbar(aes(ymax=Upper,ymin=Lower, width=.3),position = dodge)  +
  geom_hline(yintercept=0, linetype="dashed", size=.1) +
  coord_flip() + theme_bw() + 
  theme(text=element_text(size=17, family="lm")) +
  xlab(" ") + ylab("\nATT and 95% CI") +
  scale_color_manual(values=c("#999999", "#E69F00","#56B4E9"))

ggarrange(
  small_values,
  large_values,
  nrow=2,
  common.legend = TRUE,
  legend="right",
  heights  = c(1,1),
  align="v"
)

# Save graph
ggsave("Objects/gender_graph.pdf", height=6, width=11, units = "in")


## RURAL vs. URBAN ZIP CODES ------------------------------------------------------------------- #####
# Read in rural zip code data
RUCA2010zipcode <- read_excel(paste0(raw_data_path,"/RUCA2010zipcode.xlsx"), 
                              sheet = "Data")

Physician_Data <- Physician_Data %>%
  mutate(phy_zip1=as.character(phy_zip1)) %>%
  dplyr::left_join(RUCA2010zipcode, by=c("phy_zip1"="ZIP_CODE"))


# Create rural and urban indicators
Physician_Data <- Physician_Data %>%
  mutate(rural=ifelse(RUCA1==8 | RUCA1==9 | RUCA1==10,1,0),
         urban=ifelse(RUCA1==1 | RUCA1==2 | RUCA1==3,1,0))

# Create "ever urban" and "ever rural"
Physician_Data <- Physician_Data %>%
  dplyr::group_by(DocNPI) %>%
  dplyr::mutate(sum=sum(rural),
         sum1=sum(urban)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(ever_rural=ifelse(sum>0,1,0),
         ever_urban=ifelse(sum1>0,1,0)) %>%
  dplyr::select(-sum,-sum1)

# Run regressions
varlist <- list("retire", "pos_office", "work_in_office", "change_zip", "npi_unq_benes", "claim_count_total")

# Create List of Results for each gender
models_rural <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0 & ever_rural==1) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & ever_rural==1) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & ever_rural==1)),
         est_method = "dr",
         control_group = "notyettreated")
  
})

models_rural_young <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0 & ever_rural==1 & max_age<60) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & ever_rural==1 & max_age<60) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & ever_rural==1 & max_age<60)),
         est_method = "dr",
         control_group = "notyettreated")
  
})

models_rural_old <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0 & ever_rural==1 & max_age>=60) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & ever_rural==1 & max_age>=60) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & ever_rural==1 & max_age>=60)),
         est_method = "dr",
         control_group = "notyettreated")
  
})

models_urban <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0 & ever_rural==0) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & ever_rural==0) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & ever_rural==0)),
         est_method = "dr",
         control_group = "notyettreated")
  
})

models_urban_young <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0 & ever_rural==0 & max_age<60) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & ever_rural==0 & max_age<60) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & ever_rural==0 & max_age<60)),
         est_method = "dr",
         control_group = "notyettreated")
  
})

models_urban_old <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0 & ever_rural==0 & max_age>=60) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & ever_rural==0 & max_age>=60) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & ever_rural==0 & max_age>=60)),
         est_method = "dr",
         control_group = "notyettreated")
  
})

# Translate to single ATT value for each gender
Simple_rural <- lapply(models_rural, function(x){
  aggte(x, type = "simple")
})

Simple_rural_young <- lapply(models_rural_young, function(x){
  aggte(x, type = "simple")
})

Simple_rural_old <- lapply(models_rural_old, function(x){
  aggte(x, type = "simple")
})

Simple_urban <- lapply(models_urban, function(x){
  aggte(x, type = "simple")
})

Simple_urban_young <- lapply(models_urban_young, function(x){
  aggte(x, type = "simple")
})

Simple_urban_old <- lapply(models_urban_old, function(x){
  aggte(x, type = "simple")
})

# Save relevant information for each gender
ATT_rural <- lapply(Simple_rural, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    "Any",
    "Rural")
})

ATT_rural_young <- lapply(Simple_rural_young, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    "< 60",
    "Rural")
})

ATT_rural_old <- lapply(Simple_rural_old, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    ">= 60",
    "Rural")
})

ATT_urban <- lapply(Simple_urban, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    "Any",
    "Urban")
})

ATT_urban_young <- lapply(Simple_urban_young, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    "< 60",
    "Urban")
})

ATT_urban_old <- lapply(Simple_urban_old, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    ">= 60",
    "Urban")
})

# Convert to data frame for each age group 
rural_values <- as.data.frame(do.call(rbind, ATT_rural)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

rural_values_young <- as.data.frame(do.call(rbind, ATT_rural_young)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

rural_values_old <- as.data.frame(do.call(rbind, ATT_rural_old)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

urban_values <- as.data.frame(do.call(rbind, ATT_urban)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

urban_values_young <- as.data.frame(do.call(rbind, ATT_urban_young)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

urban_values_old <- as.data.frame(do.call(rbind, ATT_urban_old)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

# Merge all genders and main specification results
rural_merged <- rbind(rural_values, rural_values_young, rural_values_old,urban_values, urban_values_young, urban_values_old, main_ATT) %>%
  dplyr::mutate(ATT=as.numeric(ATT),
                Lower=as.numeric(Lower),
                Upper=as.numeric(Upper)) %>%
  dplyr::mutate(Variable=ifelse(Variable=="retire","Retire",Variable),
                Variable=ifelse(Variable=="work_in_office","Prob. Working in Office",Variable),
                Variable=ifelse(Variable=="pos_office","Frac. Patients in Office",Variable),
                Variable=ifelse(Variable=="npi_unq_benes","Number Patients",Variable),
                Variable=ifelse(Variable=="claim_count_total","Claim Count",Variable),
                Variable=ifelse(Variable=="change_zip","Prob. Change Zip",Variable))


# Create Graph
dodge <- position_dodge(width=.75)
small_values <- ggplot(dplyr::filter(rural_merged, Variable!="Claim Count" & Variable!="Number Patients" & Age=="Any"),
                       aes(x=Variable, y=ATT, color=Specification)) +
  geom_point(position=dodge) +
  geom_errorbar(aes(ymax=Upper,ymin=Lower, width=.5),position = dodge) + 
  geom_hline(yintercept=0, linetype="dashed", size=.1) +
  coord_flip() + theme_bw() + 
  theme(text=element_text(size=17, family="lm")) +
  xlab(" ") + ylab(" ") +
  scale_color_manual(values=c("#999999", "#E69F00","#56B4E9"))


large_values <- ggplot(dplyr::filter(rural_merged, (Variable=="Claim Count" | Variable=="Number Patients") & Age=="Any"),
                       aes(x=Variable, y=ATT, color=Specification)) +
  geom_point(position=dodge) +
  geom_errorbar(aes(ymax=Upper,ymin=Lower, width=.3),position = dodge)  +
  geom_hline(yintercept=0, linetype="dashed", size=.1) +
  coord_flip() + theme_bw() + 
  theme(text=element_text(size=17, family="lm")) +
  xlab(" ") + ylab("\nATT and 95% CI") +
  scale_color_manual(values=c("#999999", "#E69F00","#56B4E9"))

ggarrange(
  small_values,
  large_values,
  nrow=2,
  common.legend = TRUE,
  legend="right",
  heights  = c(1,1),
  align="v"
)

# Save graph
ggsave("Objects/gender_graph.pdf", height=6, width=11, units = "in")



## PHYSICIANS WHO SEE PRIMARILY WHITE PATIENTS ------------------------------------------------------------#

varlist <- list("retire", "pos_office", "work_in_office", "change_zip", "npi_unq_benes", "claim_count_total")

models_wht <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data= if (x=="retire") dplyr::filter(Physician_Data,minyr_EHR>0 & perc_white>.5) else (if (x=="pos_office" | x=="work_in_office" | x=="change_zip") dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & perc_white>.5) else dplyr::filter(Physician_Data, minyr_EHR>0 & ever_retire==0 & never_newnpi==1 & perc_white>.5)),
         est_method = "dr",
         control_group = "notyettreated",
         anticipation = 1)
  
})

Simple_wht <- lapply(models_wht, function(x){
  aggte(x, type = "simple")
})

ATT_wht <- lapply(Simple_wht, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    "Any",
    "Sees Majority White Patients")
})


wht_values <- as.data.frame(do.call(rbind, ATT_wht)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7) %>%
  dplyr::mutate(Variable=ifelse(Variable=="retire","Retire",Variable),
                Variable=ifelse(Variable=="work_in_office","Prob. Working in Office",Variable),
                Variable=ifelse(Variable=="pos_office","Frac. Patients in Office",Variable),
                Variable=ifelse(Variable=="npi_unq_benes","Number Patients",Variable),
                Variable=ifelse(Variable=="claim_count_total","Claim Count",Variable),
                Variable=ifelse(Variable=="change_zip","Prob. Change Zip",Variable))

wht_merged <- rbind(wht_values, main_ATT) %>%
  dplyr::mutate(ATT=as.numeric(ATT),
                Lower=as.numeric(Lower),
                Upper=as.numeric(Upper))


# Create Graph
dodge <- position_dodge(width=.75)
small_values <- ggplot(dplyr::filter(wht_merged, Variable!="Claim Count" & Variable!="Number Patients" & Age=="Any"),
                       aes(x=Variable, y=ATT, color=Specification)) +
  geom_point(position=dodge) +
  geom_errorbar(aes(ymax=Upper,ymin=Lower, width=.5),position = dodge) + 
  geom_hline(yintercept=0, linetype="dashed", size=.1) +
  coord_flip() + theme_bw() + 
  theme(text=element_text(size=17, family="lm")) +
  xlab(" ") + ylab(" ") +
  scale_color_manual(values=c("#999999", "#E69F00"))


large_values <- ggplot(dplyr::filter(wht_merged, (Variable=="Claim Count" | Variable=="Number Patients") & Age=="Any"),
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

ggsave("Objects/wht_graph.pdf", height=6, width=11, units = "in")




# Make one large heterogeneity graph ------------------------------------------------

heterogeneity_merged <- rbind(wht_values, female_values, male_values, rural_values, urban_values, main_ATT) %>%
  dplyr::mutate(ATT=as.numeric(ATT),
                Lower=as.numeric(Lower),
                Upper=as.numeric(Upper)) %>%
  dplyr::mutate(Variable=ifelse(Variable=="retire","Retire",Variable),
                Variable=ifelse(Variable=="work_in_office","Prob. Working in Office",Variable),
                Variable=ifelse(Variable=="pos_office","Frac. Patients in Office",Variable),
                Variable=ifelse(Variable=="npi_unq_benes","Number Patients",Variable),
                Variable=ifelse(Variable=="claim_count_total","Claim Count",Variable),
                Variable=ifelse(Variable=="change_zip","Prob. Change Zip",Variable))

# Create Graph
dodge <- position_dodge(width=1)
small_values <- ggplot(dplyr::filter(heterogeneity_merged, Variable!="Claim Count" & Variable!="Number Patients" & Age=="Any"),
                       aes(x=Variable, y=ATT, color=Specification)) +
  geom_point(position=dodge) +
  geom_errorbar(aes(ymax=Upper,ymin=Lower, width=.5),position = dodge) + 
  geom_hline(yintercept=0, linetype="dashed", size=.1) +
  coord_flip() + theme_bw() + 
  theme(text=element_text(size=17, family="lm")) +
  xlab(" ") + ylab(" ") +
  scale_color_manual(values=c(Main="#000000","Sees Majority White Patients"="#E69F00","Female"="#009E73","Male"="#F0E442","Rural"="#0072B2","Urban"="#D55E00"))


large_values <- ggplot(dplyr::filter(heterogeneity_merged, (Variable=="Claim Count" | Variable=="Number Patients") & Age=="Any"),
                       aes(x=Variable, y=ATT, color=Specification)) +
  geom_point(position=dodge) +
  geom_errorbar(aes(ymax=Upper,ymin=Lower, width=.3),position = dodge)  +
  geom_hline(yintercept=0, linetype="dashed", size=.1) +
  coord_flip() + theme_bw() + 
  theme(text=element_text(size=17, family="lm")) +
  xlab(" ") + ylab("\nATT and 95% CI") +
  scale_color_manual(values=c(Main="#000000","Sees Majority White Patients"="#E69F00","Female"="#009E73","Male"="#F0E442","Rural"="#0072B2","Urban"="#D55E00"))

ggarrange(
  small_values,
  large_values,
  nrow=2,
  common.legend = TRUE,
  legend="right",
  heights  = c(1,1),
  align="v"
)

ggsave("Objects/heterogeneity _graph.pdf", height=7, width=10, units = "in")




## TWO WAY FIXED EFFECTS ------------------------------------- ####

varlist <- list("retire", "pos_office", "work_in_office", "change_zip", "npi_unq_benes", "claim_count_total")


twfe_models <- lapply(varlist, function(x){
  feols(xpd(..lhs ~ rel_m4+rel_m3+ rel_m2+ rel_0+ rel_p1+
              rel_p2+ rel_p3 + rel_p4 | DocNPI + year, ..lhs = x),
        cluster="DocNPI",
        data = if (x!= "retire") dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0) else dplyr::filter(Physician_Data,minyr_EHR>0)
  )
})

#Extract Needed Info from Regressions
coefs <- lapply(twfe_models, function(x){
  y <- as.data.frame(list(x$coefficients,x$se))
   y <- y %>% rename(Estimate=names(y)[[1]]) %>%
     rename(se=names(y)[[2]]) 
   
   y <- rbind(y[1:3, ],            
                     c(0,0,"rel_m1"),
                     y[4:8, ])
   rownames(y)[rownames(y)=="4"] <- "rel_m1"
   
   y <- y %>%
     mutate(vars=row.names(y),
            se=as.numeric(se),
            Estimate=as.numeric(Estimate)) %>%
     mutate(vars=ifelse(vars=="rel_m4", "-4",vars),
            vars=ifelse(vars=="rel_m3", "-3",vars),
            vars=ifelse(vars=="rel_m2", "-2",vars),
            vars=ifelse(vars=="rel_m1", "-1",vars),
            vars=ifelse(vars=="rel_0", "0",vars),
            vars=ifelse(vars=="rel_p1", "1",vars),
            vars=ifelse(vars=="rel_p2", "2",vars),
            vars=ifelse(vars=="rel_p3", "3",vars),
            vars=ifelse(vars=="rel_p4", "4",vars)) %>%
     mutate(vars=factor(vars,levels=unique(vars))) %>%
     mutate(category=ifelse(vars=="-4"|vars=="-3"|vars=="-2"|vars=="-1","Pre","Post")) %>%
     mutate(title=as.character(x[["fml"]][[2]]))      
      
    y <- y %>%
      mutate(title=ifelse(title=="retire", "Outcome: Retire",title),
             title=ifelse(title=="pos_office", "Outcome: Fraction of Patients Seen in Office",title),
             title=ifelse(title=="work_in_office", "Outcome: Works in an Office",title),
             title=ifelse(title=="change_zip", "Outcome: Change Zip Codes",title),
             title=ifelse(title=="npi_unq_benes", "Outcome: Number of Patients",title),
             title=ifelse(title=="claim_count_total", "Outcome: Number of Claims",title))
      
    return(y)
})

graphs <- lapply(coefs, function(x){
  pd <- position_dodge(0.2) # move them .05 to the left and right
  
  y <- ggplot(x, aes(vars, Estimate)) + 
    geom_hline(yintercept=0, lty=2, lwd=.5, colour="black") +
    geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96*se, color=category), 
                  lwd=.5, width=.18) +
    geom_point(aes(fill=category, color=category), size=1.5, pch=21) +
    theme_bw() + theme(text=element_text(size=10,family="lm"),
                       legend.title = element_blank()) + xlab("Event Time") +
    ylab("Estimate and 95% CI") +
    scale_color_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") +
  scale_fill_manual(labels = c("Pre", "Post"), values=c("#999999", "#E69F00"),name="") + labs(title=x$title[[1]])
  
  return(y)
})

twfe_finalplot <- ggarrange(graphs[[1]]+ rremove("ylab") + rremove("xlab"),
          graphs[[2]] + rremove("ylab") + rremove("xlab"),
          graphs[[3]] + rremove("ylab") + rremove("xlab"),
          graphs[[4]] + rremove("ylab") + rremove("xlab"),
          graphs[[5]] + rremove("ylab") + rremove("xlab"),
          graphs[[6]] + rremove("ylab") + rremove("xlab"),
          ncol=2, nrow=3,
          common.legend = TRUE,
          labels=NULL,
          legend="bottom")

ggsave("Objects/twfe_plot.pdf", twfe_finalplot, height=9.5, width=8, units = "in")



## Roth Prettrends Stuff ---------------------------------- ####

#(Need results from analysis loaded)

honest_did <- function(es, ...) {
  UseMethod("honest_did", es)
}

honest_did.AGGTEobj <- function(es,
                                e=0,
                                type=c("smoothness", "relative_magnitude"),
                                method=NULL,
                                bound="deviation from parallel trends",
                                Mvec=NULL,
                                Mbarvec=NULL,
                                monotonicityDirection=NULL,
                                biasDirection=NULL,
                                alpha=0.05,
                                parallel=FALSE,
                                gridPoints=10^3,
                                grid.ub=NA,
                                grid.lb=NA,
                                ...) {
  
  
  type <- type[1]
  
  # make sure that user is passing in an event study
  if (es$type != "dynamic") {
    stop("need to pass in an event study")
  }
  
  # check if used universal base period and warn otherwise
  if (es$DIDparams$base_period != "universal") {
    warning("it is recommended to use a universal base period for honest_did")
  }
  
  # recover influence function for event study estimates
  es_inf_func <- es$inf.function$dynamic.inf.func.e
  
  # recover variance-covariance matrix
  n <- nrow(es_inf_func)
  V <- t(es_inf_func) %*% es_inf_func / (n*n) 
  
  
  nperiods <- nrow(V)
  npre <- sum(1*(es$egt < 0))
  npost <- nperiods - npre
  
  baseVec1 <- basisVector(index=(e+1),size=npost)
  
  orig_ci <- constructOriginalCS(betahat = es$att.egt,
                                 sigma = V, numPrePeriods = npre,
                                 numPostPeriods = npost,
                                 l_vec = baseVec1)
  
  if (type=="relative_magnitude") {
    if (is.null(method)) method <- "C-LF"
    robust_ci <- createSensitivityResults_relativeMagnitudes(betahat = es$att.egt, sigma = V, 
                                                             numPrePeriods = npre, 
                                                             numPostPeriods = npost,
                                                             bound=bound,
                                                             method=method,
                                                             l_vec = baseVec1,
                                                             Mbarvec = Mbarvec,
                                                             monotonicityDirection=monotonicityDirection,
                                                             biasDirection=biasDirection,
                                                             alpha=alpha,
                                                             gridPoints=100,
                                                             grid.lb=-1,
                                                             grid.ub=1,
                                                             parallel=parallel)
    
  } else if (type=="smoothness") {
    robust_ci <- createSensitivityResults(betahat = es$att.egt,
                                          sigma = V, 
                                          numPrePeriods = npre, 
                                          numPostPeriods = npost,
                                          method=method,
                                          l_vec = baseVec1,
                                          monotonicityDirection=monotonicityDirection,
                                          biasDirection=biasDirection,
                                          alpha=alpha,
                                          parallel=parallel)
  }
  
  list(robust_ci=robust_ci, orig_ci=orig_ci, type=type)
}


smooth_year1 <- lapply(Models_agg, function(x){
  all <- honest_did(x[["agg_all"]], type="smoothness")
  young <- honest_did(x[["agg_young"]], type="smoothness")
  old <- honest_did(x[["agg_old"]], type="smoothness")
  
  list(all=all,young=young,old=old)
})

plot_smooth_year1 <- lapply(smooth_year1, function(x){
  all <- createSensitivityPlot(x$all$robust_ci, x$all$orig_ci)
  young <- createSensitivityPlot(x$young$robust_ci, x$young$orig_ci)
  old <- createSensitivityPlot(x$old$robust_ci, x$old$orig_ci)
  
  list(all,young,old)
})

smooth_year2 <- lapply(Models_agg, function(x){
  all <- honest_did(x[["agg_all"]], type="smoothness")
  young <- honest_did(x[["agg_young"]], type="smoothness")
  old <- honest_did(x[["agg_old"]], type="smoothness")
  
  list(all=all,young=young,old=old)
})

plot_smooth_year2 <- lapply(smooth_year2, function(x){
  all <- createSensitivityPlot(x$all$robust_ci, x$all$orig_ci)
  young <- createSensitivityPlot(x$young$robust_ci, x$young$orig_ci)
  old <- createSensitivityPlot(x$old$robust_ci, x$old$orig_ci)
  
  list(all,young,old)
})

smooth_year0 <- lapply(Models_agg, function(x){
  all <- honest_did(x[["agg_all"]], type="smoothness")
  young <- honest_did(x[["agg_young"]], type="smoothness")
  old <- honest_did(x[["agg_old"]], type="smoot whness")
  
  list(all=all,young=young,old=old)
})

plot_smooth_year0 <- lapply(smooth_year0, function(x){
  all <- createSensitivityPlot(x$all$robust_ci, x$all$orig_ci)
  young <- createSensitivityPlot(x$young$robust_ci, x$young$orig_ci)
  old <- createSensitivityPlot(x$old$robust_ci, x$old$orig_ci)
  
  list(all,young,old)
})

# Now that I have the plots saved, I need to fix them up for the paper.

# Retire
retire_all_yr1 <- plot_smooth_year1[[1]][[1]] + 
  theme_bw() + labs(title="Robust Confidence Intervals at Event Time = 1") +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12, family="lm")) + 
  ylim(-.02,.03)
retire_all_yr2 <- plot_smooth_year2[[1]][[1]] + 
  theme_bw() + labs(title="Robust Confidence Intervals at Event Time = 2") +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12, family="lm")) +
  ylim(-.02,.03)

retire_pretrends_plot <- ggarrange(
  retire_all_yr1,
  retire_all_yr2,
  nrow = 2,
  common.legend = TRUE,
  legend="bottom"
) %>% annotate_figure(
  top=text_grob("\nOutcome: Retire\n", family="lm", hjust=.5, vjust=.5, size=15)
)

ggsave(retire_pretrends_plot, file="Objects/retire_pretrends_plot.pdf", height=7, width=10, units="in")  


# Work in Office
work_all_yr0 <- plot_smooth_year0[[3]][[1]] + 
  theme_bw() + labs(title="Robust Confidence Intervals at \nEvent Time = 0 (All Ages)") +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12, family="lm")) 
work_all_yr1 <- plot_smooth_year1[[3]][[1]] + 
  theme_bw() + labs(title="Robust Confidence Intervals at \nEvent Time = 1 (All Ages)") +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12, family="lm")) 
work_old_yr0 <- plot_smooth_year0[[3]][[3]] + 
  theme_bw() + labs(title="Robust Confidence Intervals at \nEvent Time = 0 (>= 60)") +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12, family="lm")) 
work_old_yr1 <- plot_smooth_year1[[3]][[3]] + 
  theme_bw() + labs(title="Robust Confidence Intervals at \nEvent Time = 1 (>=60)") +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12, family="lm")) 


work_pretrends_plot <- ggarrange(
  work_all_yr0,
  work_old_yr0,
  work_all_yr1,
  work_old_yr1,
  nrow = 2,
  ncol = 2,
  common.legend = TRUE,
  legend="bottom"
) %>% annotate_figure(
  top=text_grob("\nOutcome: Work in Office\n", family="lm", hjust=.5, vjust=.5, size=15)
)

ggsave(work_pretrends_plot, file="Objects/work_pretrends_plot.pdf", height=7, width=10, units="in")  

# Fraction Patients in Office
frac_all_yr0 <- plot_smooth_year0[[2]][[1]] + 
  theme_bw() + labs(title="Robust Confidence Intervals at Event Time = 0") +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12, family="lm"))
frac_all_yr1 <- plot_smooth_year1[[2]][[1]] + 
  theme_bw() + labs(title="Robust Confidence Intervals at Event Time = 1") +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12, family="lm")) 

frac_pretrends_plot <- ggarrange(
  frac_all_yr0,
  frac_all_yr1,
  nrow = 2,
  common.legend = TRUE,
  legend="bottom"
) %>% annotate_figure(
  top=text_grob("\nOutcome: Fraction Patients Seen in Office\n", family="lm", hjust=.5, vjust=.5, size=15)
)

ggsave(frac_pretrends_plot, file="Objects/frac_pretrends_plot.pdf", height=7, width=10, units="in")  

#Change Zip Codes
zip_young_yr0 <- plot_smooth_year0[[4]][[2]] + 
  theme_bw() + labs(title="Robust Confidence Intervals at \nEvent Time = 0 (< 60)") +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12, family="lm")) 
zip_young_yr1 <- plot_smooth_year1[[4]][[2]] + 
  theme_bw() + labs(title="Robust Confidence Intervals at \nEvent Time = 1 (< 60)") +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12, family="lm")) 
zip_old_yr0 <- plot_smooth_year0[[4]][[3]] + 
  theme_bw() + labs(title="Robust Confidence Intervals at \nEvent Time = 0 (>= 60)") +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12, family="lm")) 
zip_old_yr1 <- plot_smooth_year1[[4]][[3]] + 
  theme_bw() + labs(title="Robust Confidence Intervals at \nEvent Time = 1 (>=60)") +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12, family="lm")) 


zip_pretrends_plot <- ggarrange(
  zip_young_yr0,
  zip_old_yr0,
  zip_young_yr1,
  zip_old_yr1,
  nrow = 2,
  ncol = 2,
  common.legend = TRUE,
  legend="bottom"
) %>% annotate_figure(
  top=text_grob("\nOutcome: Change Zip Code\n", family="lm", hjust=.5, vjust=.5, size=15)
)

ggsave(zip_pretrends_plot, file="Objects/zip_pretrends_plot.pdf", height=7, width=10, units="in")  


#Patient Count
pat_all_yr0 <- plot_smooth_year0[[5]][[1]] + 
  theme_bw() + labs(title="Robust Confidence Intervals at Event Time = 0") +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12, family="lm"))
pat_all_yr1 <- plot_smooth_year1[[5]][[1]] + 
  theme_bw() + labs(title="Robust Confidence Intervals at Event Time = 1") +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12, family="lm")) 

pat_pretrends_plot <- ggarrange(
  pat_all_yr0,
  pat_all_yr1,
  nrow = 2,
  common.legend = TRUE,
  legend="bottom"
) %>% annotate_figure(
  top=text_grob("\nOutcome: Number of Patients\n", family="lm", hjust=.5, vjust=.5, size=15)
)

ggsave(pat_pretrends_plot, file="Objects/patient_pretrends_plot.pdf", height=7, width=10, units="in")  

# Claim Count
claim_young_yr0 <- plot_smooth_year0[[6]][[2]] + 
  theme_bw() + labs(title="Robust Confidence Intervals at \nEvent Time = 0 (< 60)") +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12, family="lm")) 
claim_young_yr1 <- plot_smooth_year1[[6]][[2]] + 
  theme_bw() + labs(title="Robust Confidence Intervals at \nEvent Time = 1 (< 60)") +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12, family="lm")) 
claim_old_yr0 <- plot_smooth_year0[[6]][[3]] + 
  theme_bw() + labs(title="Robust Confidence Intervals at \nEvent Time = 0 (>= 60)") +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12, family="lm")) 
claim_old_yr1 <- plot_smooth_year1[[6]][[3]] + 
  theme_bw() + labs(title="Robust Confidence Intervals at \nEvent Time = 1 (>=60)") +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12, family="lm")) 


claim_pretrends_plot <- ggarrange(
  claim_young_yr0,
  claim_old_yr0,
  claim_young_yr1,
  claim_old_yr1,
  nrow = 2,
  ncol = 2,
  common.legend = TRUE,
  legend="bottom"
) %>% annotate_figure(
  top=text_grob("\nOutcome: Claim Count\n", family="lm", hjust=.5, vjust=.5, size=15)
)

ggsave(claim_pretrends_plot, file="Objects/claim_pretrends_plot.pdf", height=7, width=10, units="in")  




## ANALYSIS ON WHETHER EHR IMPACTS NUMBER OF PHYSICIANS

# Read in data on physician hospital pairs
data <- read_rds(paste0(created_data_path, "phys_hosp_pairs.rds"))

#Read in AHA-NPI crosswalk data
AHANPI_cw <- read_rds(paste0(created_data_path,"AHANPI_cw.rds"))

# Merge crosswalk to main data
data <- data %>%
  mutate(HospNPI=as.double(HospNPI)) %>%
  left_join(AHANPI_cw,by=c("HospNPI"="NPI"))

data <- data %>%
  filter(!is.na(AHAID))


# Read in main AHA survey data
# I need to think about adding more hospital characteristics to this dataset
AHAmainsurvey <- read_csv(paste0(raw_data_path,"AHA_mainsurvey.csv"))

# I define low integration and high integration as in Madison (2004, HSR)
AHAmainsurvey <- AHAmainsurvey %>%
  mutate(ID=as.character(ID)) %>%
  mutate(low_integration=ifelse(IPAHOS==1 | OPHOHOS==1 | CPHOHOS==1,1,0),
         high_integration=ifelse(GPWWHOS==1 | MSOHOS==1 | ISMHOS==1 | EQMODHOS==1,1,0)) %>%
  dplyr::rename(Hospital_name=MNAME, year=YEAR, full_year=FYR, 
                joint_phys=JNTPH, total_physicians=FTMT, phys_owned=PHYGP, beds=BDTOT ) %>%
  select(-IPAHOS, -OPHOHOS, -CPHOHOS, -GPWWHOS, -MSOHOS, -ISMHOS, -EQMODHOS) %>%
  select(-IPAP, -GPWP, -OPHP, -CPHP, -ISMP, -EQMP, -FNDP, -full_year, -EHLTHI, -EHLTHS,
         -total_physicians, -joint_phys, -phys_owned)

data <- data %>% ungroup() %>%
  mutate(AHAID=as.character(AHAID)) %>%
  left_join(AHAmainsurvey,by=c("AHAID"="ID","year"),na_matches="never")



# Find out how many hospital years have NA for EHR question (conditional on having an AHA ID)
num_missing_EHR <- data %>%
  filter(is.na(EHLTH)) %>%
  group_by(year) %>%
  distinct(HospNPI)
# 7902 missing 

# Find out how many hospitals are missing an answer in every year
num_always_missing_EHR <- data %>% ungroup() %>%
  filter(is.na(EHLTH)) %>%
  mutate(EHLTH=ifelse(is.na(EHLTH),10,EHLTH)) %>%
  group_by(HospNPI) %>%
  mutate(always_missing=ifelse(sum(EHLTH)==60,1,0)) %>%
  filter(always_missing==1) %>%
  ungroup() %>%
  distinct(HospNPI, .keep_all = T)
# There are only 90 hospitals that never answer this question, but they typically have answers to the other questions in the survey

# Fill in missing year for EHR if it's between two years that have the same answer for EHR question
data <- data %>% group_by(AHAID) %>%
  mutate(firstyear_0=min(year[EHLTH==0],na.rm=T),lastyear_0=max(year[EHLTH==0],na.rm=T)) %>%
  mutate(firstyear_0=ifelse(is.infinite(firstyear_0),NA,firstyear_0),lastyear_0=ifelse(is.infinite(lastyear_0),NA,lastyear_0)) %>%
  mutate(EHLTH=ifelse(firstyear_0<year & year<lastyear_0 & is.na(EHLTH),0,EHLTH))


data <- data %>% group_by(AHAID) %>%
  mutate(firstyear_1=min(year[EHLTH==1],na.rm=T),lastyear_1=max(year[EHLTH==1],na.rm=T)) %>%
  mutate(firstyear_1=ifelse(is.infinite(firstyear_1),NA,firstyear_1),lastyear_1=ifelse(is.infinite(lastyear_1),NA,lastyear_1)) %>%
  mutate(EHLTH=ifelse(firstyear_1<year & year<lastyear_1 & is.na(EHLTH),1,EHLTH))


data <- data %>% group_by(AHAID) %>%
  mutate(firstyear_2=min(year[EHLTH==2],na.rm=T),lastyear_2=max(year[EHLTH==2],na.rm=T)) %>%
  mutate(firstyear_2=ifelse(is.infinite(firstyear_2),NA,firstyear_2),lastyear_2=ifelse(is.infinite(lastyear_2),NA,lastyear_2)) %>%
  mutate(EHLTH=ifelse(firstyear_2<year & year<lastyear_2 & is.na(EHLTH),2,EHLTH)) %>%
  ungroup()
# Now down to 6822 missing 

# Get rid of unneeded variables 
data <- data %>% ungroup() %>%
  select(-firstyear_0, -firstyear_1, -firstyear_2, -lastyear_0, -lastyear_1, -lastyear_2)

low_beds <- data %>% ungroup() %>%
  distinct(HospNPI, year, beds) %>%
  filter(beds<10) %>%
  distinct(HospNPI) %>%
  mutate(low_beds=1)
# 58 hospitals

data <- data %>%
  left_join(low_beds, by="HospNPI") %>%
  filter(is.na(low_beds)) %>%
  select(-low_beds)

data <- data %>%
  mutate(EHR=ifelse(EHLTH==2,1,ifelse(EHLTH==0 | EHLTH==1,0,NA))) %>%
  select(-EHLTH)

# In some cases, 2009 or 2015 can be filled in to not have a missing value. 
# Create dataset to fill in 2009 or 2015 conditionally
fill_in <- data %>%
  filter((year==2010 & EHR==0) | (year==2014 & EHR==1)) %>%
  mutate(change2009=ifelse(year==2010,1,0),
         change2015=ifelse(year==2014,1,0)) %>%
  select(HospNPI,DocNPI,year,EHR,change2009,change2015) %>%
  distinct(HospNPI,year,EHR,change2009,change2015)

fill_in <- fill_in %>%
  group_by(HospNPI) %>%
  mutate(change2009=ifelse(sum(change2009)>0,1,0),
         change2015=ifelse(sum(change2015)>0,1,0)) %>%
  ungroup() %>%
  distinct(HospNPI,change2009,change2015)

# Merge it back to original
data <- data %>%
  left_join(fill_in,by="HospNPI")

# Fill in the qualifying missing values
data <- data %>%
  mutate(EHR=ifelse(year==2009 & change2009==1 & is.na(EHR),0,EHR)) %>%
  mutate(EHR=ifelse(year==2015 & change2015==1 & is.na(EHR),1,EHR)) %>%
  select(-change2009, -change2015) %>%
  ungroup()

# Create number of physicians variable
data <- data %>%
  mutate(n=1) %>%
  group_by(HospNPI,year) %>%
  mutate(num_phys=sum(n)) %>%
  ungroup() %>%
  select(-n)

data <- data %>%
  distinct(HospNPI, year, EHR, num_phys)

minyr_EHR <- data %>% 
  filter(EHR>0) %>%
  group_by(HospNPI) %>%
  mutate(minyr_EHR=min(year)) %>%
  ungroup() %>%
  distinct(HospNPI,minyr_EHR)

data <- data %>% ungroup() %>%
  left_join(minyr_EHR, by="HospNPI") %>%
  mutate(minyr_EHR=ifelse(is.na(minyr_EHR),0,minyr_EHR))


hosp_analysis <- att_gt(yname = "num_phys",                # LHS Variable
              gname = "minyr_EHR",             # First year a unit is treated. (set to 0 if never treated)
              idname = "HospNPI",               # ID
              tname = "year",                  # Time Variable
              xformla = NULL,      
              data=data,
              est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
              control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
              clustervars = "HospNPI",          # Cluster Variables          
              anticipation=0,
              base_period = "varying" # can set a number of years to account for anticipation effects
)
agg_hosp <- aggte(hosp_analysis, type = "dynamic",na.rm=T)

ggdid(agg_hosp)


data_hosp <- as.data.frame(agg_hosp[["egt"]]) %>%
    dplyr::rename(year=1) %>%
    cbind(as.data.frame(agg_hosp[["att.egt"]])) %>%
    dplyr::rename(att=2) %>%
    cbind(as.data.frame(agg_hosp[["se.egt"]])) %>%
    dplyr::rename(se=3) %>%
    mutate(upper=att+(1.96*se),
           lower=att-(1.96*se),
           year=as.factor(year))

hosp_plot <- ggplot(data_hosp, aes(year, att)) +  
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
  theme(plot.caption=element_text(hjust = 0))

ggsave(file="Objects/numberofphysicians_plot.pdf",plot=hosp_plot, width=10, height=7, units="in")

  
