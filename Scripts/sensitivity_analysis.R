library(showtext)
library(ggplot2)
library(did)
library(ggpubr)
library(fixest)
library(did2s)
library(readxl)
library(HonestDiD)
library(mvtnorm)
library(eventstudyr)
library(dplyr)
library(modelsummary)


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


### --------------------  Sensitivity Analysis --------------------------
#                         Hanna Glenn, Emory University
#                         4/19/2022

# # ORDER :::::::::: 6

# This script reads in "Physician_Data.rds" from data5, the final dataset used in my third year paper. 
# The first portion of the script considers different potential estimators to use as the main specification for the paper.
# In this portion, I only consider "retire" as the dependent outcome and I compare estimators. 

# Read in the main data created in "data5_MDPPAS.R"
Physician_Data <- readRDS(paste0(created_data_path,"Physician_Data.rds")) %>%
  mutate(DocNPI=as.numeric(DocNPI))




## DIFFERENT ESTIMATORS ############################################################################################
varlist <- list("retire", "pos_office", "work_in_office", "npi_unq_benes", "claim_per_patient")

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

models_stacked_IV <- lapply(varlist, function(x){
  feols(xpd(..lhs ~ 1 | DocNPI + year | anyEHR_exposed ~ weightedavg_exposure, ..lhs = x),
        cluster="DocNPI",
        data=stacked_data_1)
})

etable(models_stacked_IV[[1]], stage=1, fitstat='ivf', tex=TRUE)


ATT_stacked <- lapply(models_stacked, function(x){
  c(x$fml[[2]],
    round(x[["coefficients"]][["treated_unit:rel_p1"]],5), 
    round(x[["se"]][["treated_unit:rel_p1"]], 5),
    "Stacked Regression")
})

stacked_values <- as.data.frame(do.call(rbind, ATT_stacked)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Specification=V4) %>%
  mutate(Variable=ifelse(Variable=="retire", "Retire",Variable),
         Variable=ifelse(Variable=="pos_office", "Frac. Patients in Office",Variable),
         Variable=ifelse(Variable=="work_in_office", "Work in Office",Variable),
         Variable=ifelse(Variable=="npi_unq_benes", "Number Patients",Variable),
         Variable=ifelse(Variable=="claim_per_patient", "Claims per Patient",Variable))


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
    "Two-Stage DiD")
})

ATT_2sdid[[1]][[1]] <- "Retire"
ATT_2sdid[[2]][[1]] <- "Frac. Patients in Office"
ATT_2sdid[[3]][[1]] <- "Work in Office"
ATT_2sdid[[4]][[1]] <- "Number Patients"
ATT_2sdid[[5]][[1]] <- "Claims per Patient"

tsdid_values <- as.data.frame(do.call(rbind, ATT_2sdid)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Specification=V4)

estimators <- rbind(stacked_values, tsdid_values,
                    c("Retire", .0026, .00095, "Main (CS)"),
                    c("Frac. Patients in Office", .0076, .0035, "Main (CS)"),
                    c("Work in Office", .070, .0081, "Main (CS)"),
                    c("Number Patients", 27.93, 4.89, "Main (CS)"),
                    c("Claims per Patient", .33, .085, "Main (CS)")) %>%
  rename(coef=ATT, se=SE) %>%
  mutate(`Stacked Regression`=ifelse(Specification=="Stacked Regression",1,0),
         `Two-Stage DiD`=ifelse(Specification=="Two-Stage DiD",1,0),
         `Main (CS)`=ifelse(Specification=="Main (CS)",1,0)) %>%
  select(-Specification) %>%
  mutate(coef=as.numeric(coef),
         se=as.numeric(se)) %>%
  mutate(Estimator=ifelse(`Stacked Regression`==1,"Stacked Regression",NA),
         Estimator=ifelse(`Two-Stage DiD`==1,"Two-Stage DiD", Estimator),
         Estimator=ifelse(`Main (CS)`==1,"Main (CS)", Estimator)
  ) %>%
  mutate(Variable=as.character(Variable)) %>%
  mutate(mean=ifelse(Variable=="Retire",.03,NA),
         mean=ifelse(Variable=="Frac. Patients in Office",.08,mean),
         mean=ifelse(Variable=="Work in Office",.27,mean),
          mean=ifelse(Variable=="Number Patients",331,mean),
         mean=ifelse(Variable=="Claims per Patient",4.09,mean)) %>%
  mutate(estimate_perc=coef/mean) %>%
  mutate(upper=(coef+(1.96*se))/mean,
         lower=(coef-(1.96*se))/mean)
estimators$Variable <- factor(estimators$Variable, levels=c("Retire", "Frac. Patients in Office", "Work in Office", "Number Patients", "Claims per Patient"))


dodge <- position_dodge(width=0.5)
ggplot(data=estimators, aes(x=Variable, y=estimate_perc, color=Estimator)) + geom_point(position = dodge) +
  geom_errorbar(aes(ymax=upper,ymin=lower, color=Estimator),size=.5, position = dodge, width=.2) + 
  scale_colour_manual(values=cbbPalette) + theme_bw() + geom_hline(yintercept=0, linetype="dashed") +
  xlab("\nVariable") + ylab("Estimate and 95% CI\n")

ggsave("Objects/estimators_plot.pdf", width=8, height=5, units = "in")


## TWO WAY FIXED EFFECTS ------------------------------------- ####
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
             title=ifelse(title=="npi_unq_benes", "Outcome: Number of Patients",title),
             title=ifelse(title=="claim_per_patient", "Outcome: Number of Claims",title))
      
    return(y)
})

graphs <- lapply(coefs, function(x){
  pd <- position_dodge(0.2) # move them .05 to the left and right
  
  y <- ggplot(x, aes(vars, Estimate)) + 
    geom_hline(yintercept=0, lty=2, lwd=.5, colour="black") +
    geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96*se, color=category), 
                  lwd=.5, width=.18) +
    geom_point(aes(fill=category, color=category), size=1.5, pch=21) +
    theme_bw() + theme(text=element_text(size=10),
                       legend.title = element_blank()) + xlab("Event Time") +
    ylab("Estimate and 95% CI") +
    scale_color_manual(labels = c("Post", "Pre"), values=c("#E69F00", "#999999"),name="") +
  scale_fill_manual(labels = c("Post", "Pre"), values=c("#E69F00", "#999999"),name="") + labs(title=x$title[[1]])
  
  return(y)
})

twfe_finalplot <- ggarrange(graphs[[1]]+ rremove("ylab") + rremove("xlab"),
          graphs[[2]] + rremove("ylab") + rremove("xlab"),
          graphs[[3]] + rremove("ylab") + rremove("xlab"),
          graphs[[4]] + rremove("ylab") + rremove("xlab"),
          graphs[[5]] + rremove("ylab") + rremove("xlab"),
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
                                e=2,
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
  
  list(all=all)
})

plot_smooth_year1 <- lapply(smooth_year1, function(x){
  all <- createSensitivityPlot(x$all$robust_ci, x$all$orig_ci)
  
  list(all)
})

smooth_year2 <- lapply(Models_agg, function(x){
  all <- honest_did(x[["agg_all"]], type="smoothness")
  
  list(all=all)
})

plot_smooth_year2 <- lapply(smooth_year2, function(x){
  all <- createSensitivityPlot(x$all$robust_ci, x$all$orig_ci)
  
  list(all)
})

# Now that I have the plots saved, I need to fix them up for the paper.
# Retire
retire_all_yr1 <- plot_smooth_year1[[1]][[1]] + 
  theme_bw()  +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12)) 

retire_all_yr2 <- plot_smooth_year2[[1]][[1]] + 
  theme_bw()  +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12)) 


ggsave(retire_all_yr1, file="Objects/retire_pretrends_plot_yr1.pdf", height=3.5, width=5, units="in")  
ggsave(retire_all_yr2, file="Objects/retire_pretrends_plot_yr2.pdf", height=3.5, width=5, units="in")  


# Work in Office
work_all_yr1 <- plot_smooth_year1[[3]][[1]] + 
  theme_bw() +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12)) 

ggsave(work_all_yr1, file="Objects/office_ind_pretrends_plot.pdf", height=3.5, width=5, units="in")  

# Fraction Patients in Office
frac_all_yr1 <- plot_smooth_year1[[2]][[1]] + 
  theme_bw() +
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12)) 

ggsave(frac_all_yr1, file="Objects/office_frac_pretrends_plot.pdf", height=3.5, width=5, units="in")  

#Change Zip Codes
zip_all_yr1 <- plot_smooth_year1[[4]][[1]] + 
  theme_bw() + 
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12)) 

ggsave(zip_all_yr1, file="Objects/zip_pretrends_plot.pdf", height=3.5, width=5, units="in")  


#Patient Count
pat_all_yr1 <- plot_smooth_year1[[5]][[1]] + 
  theme_bw() + 
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12)) 

ggsave(pat_all_yr1, file="Objects/patient_pretrends_plot.pdf", height=3.5, width=5, units="in")  

# Claim Count
claim_all_yr1 <- plot_smooth_year1[[6]][[1]] + 
  theme_bw() + 
  scale_color_manual(labels = c("FLCI", "Original"), values=c("#999999", "#E69F00"),name="") +
  theme(legend.position = "none", text = element_text(size = 12)) 

ggsave(claim_all_yr1, file="Objects/claim_pretrends_plot.pdf", height=3.5, width=5, units="in")  




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

  
# Look for physicians who follow the trend for fraction office results
investigate <- Physician_Data %>%
  mutate(pos_office_2009=ifelse(year==2009,pos_office,NA),
         pos_office_2015=ifelse(year==2015,pos_office,NA)) %>%
  group_by(DocNPI) %>%
  fill(pos_office_2009,.direction="downup") %>%
  fill(pos_office_2015,.direction="downup") %>%
  ungroup() %>%
  mutate(diff=pos_office_2015-pos_office_2009) %>%
  distinct(DocNPI, diff, minyr_EHR, office_yearprior) %>%
  filter(minyr_EHR!=2009 & minyr_EHR!=0)

Physician_Data <- Physician_Data %>%
  mutate(miss=ifelse(is.na(spec_prim_1_name),1,0)) %>%
  group_by(DocNPI) %>%
  mutate(sum=sum(miss)) %>%
  filter(sum==0)


# LEE (2009) BOUNDS ------------------------------------------------------------------------------------ #### 
leebounds <- Physician_Data %>%
  filter(minyr_EHR!=2009) %>%
  mutate(ctrl=ifelse(minyr_EHR==0,1,0),
         treat=ifelse(minyr_EHR>0,1,0),
         attrition=ifelse(never_newnpi==0 | ever_retire==1,1,0)) %>%
  mutate(claim_per_patient=ifelse(attrition==1,NA,claim_per_patient))

observe <- leebounds %>%
  filter(treat==1) 
summary(observe$claim_per_patient)

leebounds <- leebounds %>%
  mutate(pctile=ntile(npi_unq_benes,100))

observe <- leebounds %>%
  filter(pctile>22) %>%
  filter(treat==0)
summary(observe$claim_per_patient)



# EVENT STUDY WITH LEAST WIGGLY CONFOUNDER
wiggly <- lapply(varlist, function(x){
  es <- EventStudy(estimator = "OLS",
                   data = dplyr::filter(Physician_Data, minyr_EHR>0 & minyr_EHR!=2009),
                   outcomevar = x,
                   policyvar = "anyEHR_exposed",
                   idvar = "DocNPI",
                   timevar = "year",
                   pre = 0, post = 2)
  plot <- EventStudyPlot(estimates = es)
  return(plot=plot)
})



