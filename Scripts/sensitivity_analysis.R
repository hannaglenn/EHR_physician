library(showtext)
library(ggplot2)
library(did)
library(ggpubr)
library(fixest)
library(did2s)
library(readxl)




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









