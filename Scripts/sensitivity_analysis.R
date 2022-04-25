library(showtext)
library(ggplot2)
library(did)
library(ggpubr)
library(fixest)
library(did2s)




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
         data = if (x!= "retire") dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0) else dplyr::filter(Physician_Data,minyr_EHR_int>0),
         est_method = "dr",
         control_group = "notyettreated")
  
})

models_LI_young <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR_int",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data = if (x!= "retire") dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age<60) else dplyr::filter(Physician_Data,minyr_EHR_int>0 & max_age<60),
         est_method = "dr",
         control_group = "notyettreated")
  
})

models_LI_old <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR_int",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data = if (x!= "retire") dplyr::filter(Physician_Data,minyr_EHR_int>0 & ever_retire==0 & max_age>=60) else dplyr::filter(Physician_Data,minyr_EHR_int>0 & max_age>=60),
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
LI_merged <- rbind(LI_values, LI_values_young, LI_values_old, singel_ATT_values) %>%
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
  scale_color_manual(values=c("#999999", "#E69F00"))


large_values <- ggplot(dplyr::filter(LI_merged, (Variable=="Claim Count" | Variable=="Number Patients") & Age=="Any"),
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

# Save graph
ggsave("Objects/LI_graph.pdf", height=6, width=11, units = "in")





###  ANTICIPATION ########################################################################

varlist <- list("retire", "pos_office", "work_in_office", "change_zip", "npi_unq_benes", "claim_count_total")

models_anticipation <- lapply(varlist, function(x) {
  att_gt(yname = x,
        gname = "minyr_EHR",
        idname = "DocNPI",
        tname = "year",
        xformla = ~grad_year,
        data = if (x!= "retire") dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0) else dplyr::filter(Physician_Data,minyr_EHR>0),
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

anticipation_merged <- rbind(anticipation_values, singel_ATT_values) %>%
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
  scale_color_manual(values=c("#999999", "#E69F00"))


large_values <- ggplot(dplyr::filter(anticipation_merged, (Variable=="Claim Count" | Variable=="Number Patients") & Age=="Any"),
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

ggsave("Objects/anticipation_graph.pdf", height=6, width=11, units = "in")



### LIMITED YEARS #####################################################################################
varlist <- list("retire", "pos_office", "work_in_office", "change_zip", "npi_unq_benes", "claim_count_total")

# Create List of Results for each age group
models_years <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data = if (x!= "retire") dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0 & year<2016) else dplyr::filter(Physician_Data,minyr_EHR>0 & year<2016),
         est_method = "dr",
         control_group = "notyettreated")
  
})

models_years_young <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data = if (x!= "retire") dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0 & max_age<60 & year<2016) else dplyr::filter(Physician_Data,minyr_EHR>0 & max_age<60 & year<2016),
         est_method = "dr",
         control_group = "notyettreated")
  
})

models_years_old <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data = if (x!= "retire") dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0 & max_age>=60 & year<2016) else dplyr::filter(Physician_Data,minyr_EHR>0 & max_age>=60 & year<2016),
         est_method = "dr",
         control_group = "notyettreated")
  
})

# Translate to single ATT value for each age group
Simple_years <- lapply(models_years, function(x){
  aggte(x, type = "simple")
})

Simple_years_young <- lapply(models_years_young, function(x){
  aggte(x, type = "simple")
})

Simple_years_old <- lapply(models_years_old, function(x){
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

ATT_years_young <- lapply(Simple_years_young, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    "< 60",
    "Limited Years: 2009-2015")
})

ATT_years_old <- lapply(Simple_years_old, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    ">= 60",
    "Limited Years: 2009-2015")
})

# Convert to data frame for each age group 
years_values <- as.data.frame(do.call(rbind, ATT_years)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

years_values_young <- as.data.frame(do.call(rbind, ATT_years_young)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

years_values_old <- as.data.frame(do.call(rbind, ATT_years_old)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

# Merge all ages and main specification results
years_merged <- rbind(years_values, years_values_young, years_values_old, singel_ATT_values) %>%
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
  scale_color_manual(values=c("#999999", "#E69F00"))


large_values <- ggplot(dplyr::filter(years_merged, (Variable=="Claim Count" | Variable=="Number Patients") & Age=="Any"),
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

# Save graph
ggsave("Objects/years_graph.pdf", height=6, width=11, units = "in")





### PHYSICIANS NOT EXPOSED TO DATA ASSISTANTS #####################################################################

varlist <- list("retire", "pos_office", "work_in_office", "change_zip", "npi_unq_benes", "claim_count_total")

# Create List of Results for each age group
models_DA <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data = if (x!= "retire") dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0 & year<2016 & works_with_DA==0) else dplyr::filter(Physician_Data,minyr_EHR>0 & year<2016 & works_with_DA==0),
         est_method = "dr",
         control_group = "notyettreated")
  
})

models_DA_young <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data = if (x!= "retire") dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0 & max_age<60 & year<2016 & works_with_DA==0) else dplyr::filter(Physician_Data,minyr_EHR>0 & max_age<60 & year<2016 & works_with_DA==0),
         est_method = "dr",
         control_group = "notyettreated")
  
})

models_DA_old <- lapply(varlist, function(x) {
  att_gt(yname = x,
         gname = "minyr_EHR",
         idname = "DocNPI",
         tname = "year",
         xformla = ~grad_year,
         data = if (x!= "retire") dplyr::filter(Physician_Data,minyr_EHR>0 & ever_retire==0 & max_age>=60 & year<2016 & works_with_DA==0) else dplyr::filter(Physician_Data,minyr_EHR>0 & max_age>=60 & year<2016 & works_with_DA==0),
         est_method = "dr",
         control_group = "notyettreated")
  
})

# Translate to single ATT value for each age group
Simple_DA <- lapply(models_DA, function(x){
  aggte(x, type = "simple")
})

Simple_DA_young <- lapply(models_DA_young, function(x){
  aggte(x, type = "simple")
})

Simple_DA_old <- lapply(models_DA_old, function(x){
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

ATT_DA_young <- lapply(Simple_DA_young, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    "< 60",
    "No Data Assistants")
})

ATT_DA_old <- lapply(Simple_DA_old, function(x){
  c(x$DIDparams$yname,
    round(x$overall.att,5), 
    round(x$overall.se, 5),
    round(x$overall.att-(1.959*x$overall.se),5),
    round(x$overall.att+(1.959*x$overall.se),5),
    ">= 60",
    "No Data Assistants")
})

# Convert to data frame for each age group 
DA_values <- as.data.frame(do.call(rbind, ATT_DA)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

DA_values_young <- as.data.frame(do.call(rbind, ATT_DA_young)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

DA_values_old <- as.data.frame(do.call(rbind, ATT_DA_old)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Lower=V4, Upper=V5, Age=V6, Specification=V7)

# Merge all ages and main specification results
DA_merged <- rbind(DA_values, DA_values_young, DA_values_old, singel_ATT_values) %>%
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
  scale_color_manual(values=c("#999999", "#E69F00"))


large_values <- ggplot(dplyr::filter(DA_merged, (Variable=="Claim Count" | Variable=="Number Patients") & Age=="Any"),
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

# Save graph
ggsave("Objects/DA_graph.pdf", height=6, width=11, units = "in")



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







estimates_merged <- rbind(stacked_values, tsdid_values, singel_ATT_values) %>%
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
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))


large_values <- ggplot(dplyr::filter(estimates_merged, (Variable=="Claim Count" | Variable=="Number Patients") & Age=="Any"),
                       aes(x=Variable, y=ATT, color=Specification)) +
  geom_point(position=dodge) +
  geom_errorbar(aes(ymax=Upper,ymin=Lower, width=.3),position = dodge)  +
  geom_hline(yintercept=0, linetype="dashed", size=.1) +
  coord_flip() + theme_bw() + 
  theme(text=element_text(size=17, family="lm")) +
  xlab(" ") + ylab("\nATT and 95% CI") +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))

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
ggsave("Objects/estimators_graph.pdf", height=6, width=11, units = "in")



