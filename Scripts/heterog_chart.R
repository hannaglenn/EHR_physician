library(tidyr)
library(readr)
library(stringr)
library(dplyr)
library(plm)
library(janitor)
library(lubridate)
library(did)
library(ggplot2)
library(readxl)
library(dplyr)



### HETEROGENEITY GRAPHS ------------------------------------------- ####

# Read in the main data created in "data5_MDPPAS.R"
Physician_Data <- readRDS(paste0(created_data_path,"Physician_Data.rds")) %>%
  mutate(DocNPI=as.numeric(DocNPI))


# Read in rural zip code data
RUCA2010zipcode <- read_excel(paste0(raw_data_path,"/RUCA2010zipcode.xlsx"), 
                              sheet = "Data")

Physician_Data <- Physician_Data %>%
  mutate(phy_zip1=as.character(phy_zip1)) %>%
  dplyr::left_join(RUCA2010zipcode, by=c("phy_zip1"="ZIP_CODE"))


# Create rural and urban indicators
Physician_Data <- Physician_Data %>%
  mutate(rural=ifelse(RUCA1==6 | RUCA1==7 | RUCA1==8 | RUCA1==9 | RUCA1==10,1,0),
         urban=ifelse(RUCA1==1 | RUCA1==2 | RUCA1==3 | RUCA1==4 | RUCA1==5,1,0),
         small=ifelse(avg_beds<278,1,0),
         large=ifelse(avg_beds>=278,1,0))

# Create "ever urban" and "ever rural"
Physician_Data <- Physician_Data %>%
  dplyr::group_by(DocNPI) %>%
  dplyr::mutate(sum=sum(rural, na.rm=TRUE),
                sum1=sum(urban, na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(ever_rural=ifelse(sum>0,1,0),
                ever_urban=ifelse(sum1>0,1,0)) %>%
  dplyr::select(-sum,-sum1)

# Define different data sets
Rural_Data <- Physician_Data %>%
  dplyr::filter(ever_rural==1)
Urban_Data <- Physician_Data %>%
  dplyr::filter(ever_urban==1)
Smallhosp_Data <- Physician_Data %>%
  dplyr::filter(small==1)
Largehosp_Data <- Physician_Data %>%
  dplyr::filter(large==1)
onehosp_data <- Physician_Data %>%
  dplyr::filter(num_systems==1)
manyhosp_data <- Physician_Data %>%
  dplyr::filter(num_systems>1)

data_coms <- tidyr::crossing(rural=c(TRUE,FALSE), urban=c(TRUE,FALSE), small=c(TRUE,FALSE), large=c(TRUE,FALSE), one=c(TRUE, FALSE), many=c(TRUE,FALSE)) %>%
  filter(rural==TRUE & urban==FALSE & small==FALSE & large==FALSE & one == FALSE & many == FALSE |
           rural==FALSE & urban==TRUE & small==FALSE & large==FALSE & one == FALSE & many == FALSE |
           rural==FALSE & urban==FALSE & small==TRUE & large==FALSE & one == FALSE & many == FALSE |
           rural==FALSE & urban==FALSE & small==FALSE & large==TRUE & one == FALSE & many == FALSE |
           rural==FALSE & urban==FALSE & small==FALSE & large==FALSE & one == FALSE & many == FALSE |
           rural==FALSE & urban==FALSE & small==FALSE & large==FALSE & one == TRUE & many == FALSE |
           rural==FALSE & urban==FALSE & small==FALSE & large==FALSE & one == FALSE & many == TRUE)

Het_Data <- list(list(Rural_Data, data_coms[7,]), list(Urban_Data, data_coms[6,]),
                 list(Smallhosp_Data,data_coms[5,]), list(Largehosp_Data, data_coms[4,]),
                 list(Physician_Data,data_coms[1,]), list(onehosp_data,data_coms[3,]),
                 list(manyhosp_data,data_coms[2,]))


# create another dataset for different levels of integration with the hospital
IPA_data <- Physician_Data %>%
  filter(phys_ever_IPA==1)
OPHO_data <- Physician_Data %>%
  filter(phys_ever_OPHO==1) 
CPHO_data <- Physician_Data %>%
  filter(phys_ever_CPHO==1) 
ISM_data <- Physician_Data %>%
  filter(phys_ever_ISM==1) 

data_coms_int <- crossing(IPA=c(TRUE,FALSE), OPHO=c(TRUE,FALSE), CPHO=c(TRUE,FALSE), ISM=c(TRUE,FALSE)) %>%
  filter(IPA==TRUE & OPHO==FALSE & CPHO==FALSE & ISM==FALSE |
           IPA==FALSE & OPHO==TRUE & CPHO==FALSE & ISM==FALSE |
           IPA==FALSE & OPHO==FALSE & CPHO==TRUE & ISM==FALSE |
           IPA==FALSE & OPHO==FALSE & CPHO==FALSE & ISM==TRUE |
           IPA==FALSE & OPHO==FALSE & CPHO==FALSE & ISM==FALSE)

Het_Data_int <- list(list(IPA_data, data_coms_int[5,]), list(OPHO_data, data_coms_int[4,]),
                 list(CPHO_data,data_coms_int[3,]), list(ISM_data, data_coms_int[2,]),
                 list(Physician_Data,data_coms_int[1,]))




# RETIREMENT ------------------------------------------
models <- lapply(Het_Data, function(x) {
  row <- x[[2]]
  
  all <- att_gt(yname = "retire",                # LHS Variable
                gname ="minyr_EHR",          # First year a unit is treated. (set to 0 if never treated)
                idname = "DocNPI",               # ID
                tname = "year",                  # Time Variable
                xformla = ~grad_year,            # Time-invariant controls
                data= dplyr::filter(x[[1]],minyr_EHR>0),
                est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
                control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
                clustervars = "DocNPI",          # Cluster Variables          
                anticipation=0,
                base_period = "varying" # can set a number of years to account for anticipation effects
  )
  
  list(all=all, row=row)
})

models_agg <- lapply(models, function(x) {
  row <- x[[2]]
  
  agg <- aggte(x[[1]], type = "dynamic",na.rm=T)
  
  list(agg=agg, row=row)
})

chart_data <- lapply(models_agg, function(x) {
  row <- x[[2]] %>%
    mutate(coef=x[["agg"]][["overall.att"]],
           se=x[["agg"]][["overall.se"]])
  trow <- t(row)
  list(trow)
})

chart_data_frame <- t(as.data.frame(chart_data))
rownames(chart_data_frame) <- NULL

retire_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, rural, urban, small, large, one, many) %>%
  mutate(Outcome="Exit")



# OFFICE FRAC ------------------------------------------
models <- lapply(Het_Data, function(x) {
  row <- x[[2]]
  
  all <- att_gt(yname = "pos_office",                # LHS Variable
                gname ="minyr_EHR",          # First year a unit is treated. (set to 0 if never treated)
                idname = "DocNPI",               # ID
                tname = "year",                  # Time Variable
                xformla = ~grad_year,            # Time-invariant controls
                data= dplyr::filter(x[[1]],minyr_EHR>0 & ever_retire==0),
                est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
                control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
                clustervars = "DocNPI",          # Cluster Variables          
                anticipation=0,
                base_period = "varying" # can set a number of years to account for anticipation effects
  )
  
  list(all=all, row=row)
})

models_agg <- lapply(models, function(x) {
  row <- x[[2]]
  
  agg <- aggte(x[[1]], type = "dynamic",na.rm=T)
  
  list(agg=agg, row=row)
})

chart_data <- lapply(models_agg, function(x) {
  row <- x[[2]] %>%
    mutate(coef=x[["agg"]][["overall.att"]],
           se=x[["agg"]][["overall.se"]])
  trow <- t(row)
  list(trow)
})

chart_data_frame <- t(as.data.frame(chart_data))
rownames(chart_data_frame) <- NULL

fracoffice_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, rural, urban, small, large, one, many) %>%
  mutate(Outcome="Frac. Patients in Office")





# OFFICE INDICATOR ------------------------------------------
models <- lapply(Het_Data, function(x) {
  row <- x[[2]]
  
  all <- att_gt(yname = "work_in_office",                # LHS Variable
                gname ="minyr_EHR",          # First year a unit is treated. (set to 0 if never treated)
                idname = "DocNPI",               # ID
                tname = "year",                  # Time Variable
                xformla = ~grad_year,            # Time-invariant controls
                data= dplyr::filter(x[[1]],minyr_EHR>0 & ever_retire==0),
                est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
                control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
                clustervars = "DocNPI",          # Cluster Variables          
                anticipation=0,
                base_period = "varying" # can set a number of years to account for anticipation effects
  )
  
  list(all=all, row=row)
})

models_agg <- lapply(models, function(x) {
  row <- x[[2]]
  
  agg <- aggte(x[[1]], type = "dynamic",na.rm=T)
  
  list(agg=agg, row=row)
})

chart_data <- lapply(models_agg, function(x) {
  row <- x[[2]] %>%
    mutate(coef=x[["agg"]][["overall.att"]],
           se=x[["agg"]][["overall.se"]])
  trow <- t(row)
  list(trow)
})

chart_data_frame <- t(as.data.frame(chart_data))
rownames(chart_data_frame) <- NULL

indoffice_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, rural, urban, small, large, one, many)%>%
  mutate(Outcome="Work in Office")




# PATIENT COUNT ------------------------------------------
models <- lapply(Het_Data, function(x) {
  row <- x[[2]]
  
  all <- att_gt(yname = "npi_unq_benes",                # LHS Variable
                gname ="minyr_EHR",          # First year a unit is treated. (set to 0 if never treated)
                idname = "DocNPI",               # ID
                tname = "year",                  # Time Variable
                xformla = ~grad_year,            # Time-invariant controls
                data= dplyr::filter(x[[1]],minyr_EHR>0 & ever_retire==0 & never_newnpi==1),
                est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
                control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
                clustervars = "DocNPI",          # Cluster Variables          
                anticipation=0,
                base_period = "varying" # can set a number of years to account for anticipation effects
  )
  
  list(all=all, row=row)
})

models_agg <- lapply(models, function(x) {
  row <- x[[2]]
  
  agg <- aggte(x[[1]], type = "dynamic",na.rm=T)
  
  list(agg=agg, row=row)
})

chart_data <- lapply(models_agg, function(x) {
  row <- x[[2]] %>%
    mutate(coef=x[["agg"]][["overall.att"]],
           se=x[["agg"]][["overall.se"]])
  trow <- t(row)
  list(trow)
})

chart_data_frame <- t(as.data.frame(chart_data))
rownames(chart_data_frame) <- NULL

patcount_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, rural, urban, small, large, one, many) %>%
  mutate(Outcome="Number Patients")





# CLAIM PER PATIENT ------------------------------------------
models <- lapply(Het_Data, function(x) {
  row <- x[[2]]
  
  all <- att_gt(yname = "claim_per_patient",                # LHS Variable
                gname ="minyr_EHR",          # First year a unit is treated. (set to 0 if never treated)
                idname = "DocNPI",               # ID
                tname = "year",                  # Time Variable
                xformla = ~grad_year,            # Time-invariant controls
                data= dplyr::filter(x[[1]],minyr_EHR>0 & ever_retire==0 & never_newnpi==1),
                est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
                control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
                clustervars = "DocNPI",          # Cluster Variables          
                anticipation=0,
                base_period = "varying" # can set a number of years to account for anticipation effects
  )
  
  list(all=all, row=row)
})

models_agg <- lapply(models, function(x) {
  row <- x[[2]]
  
  agg <- aggte(x[[1]], type = "dynamic",na.rm=T)
  
  list(agg=agg, row=row)
})

chart_data <- lapply(models_agg, function(x) {
  row <- x[[2]] %>%
    mutate(coef=x[["agg"]][["overall.att"]],
           se=x[["agg"]][["overall.se"]])
  trow <- t(row)
  list(trow)
})

chart_data_frame <- t(as.data.frame(chart_data))
rownames(chart_data_frame) <- NULL

cpp_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, rural, urban, small, large, one, many) %>%
  mutate(Outcome="Claims per Patient")



# Create Chart
plot_data <- rbind(retire_data_frame, fracoffice_data_frame, indoffice_data_frame,
                   patcount_data_frame, cpp_data_frame) %>%
  mutate(mean=ifelse(Outcome=="Exit",.03,NA),
         mean=ifelse(Outcome=="Frac. Patients in Office",.08,mean),
         mean=ifelse(Outcome=="Work in Office",.27,mean),
         mean=ifelse(Outcome=="Number Patients",331,mean),
         mean=ifelse(Outcome=="Claims per Patient",4.09,mean)) %>%
  mutate(Sample=ifelse(rural==1, "Rural", NA),
         Sample=ifelse(urban==1, "Urban", Sample),
         Sample=ifelse(small==1, "Small Hosp.", Sample),
         Sample=ifelse(large==1, "Large Hosp.", Sample),
         Sample = ifelse(one==1, "Works w/ 1 Sys.", Sample),
         Sample = ifelse(many==1, "Works w/ >1 Sys.", Sample),
         Sample=ifelse(is.na(Sample),"Main", Sample)) %>%
  select(-rural, -urban, -small, -large, -one, -many) %>%
  mutate(estimate_perc=coef/mean) %>%
  mutate(upper=(coef+(1.96*se))/mean,
         lower=(coef-(1.96*se))/mean)
plot_data$Outcome <- factor(plot_data$Outcome, levels=c("Exit", "Frac. Patients in Office", "Work in Office", "Number Patients", "Claims per Patient"))
plot_data$Sample <- factor(plot_data$Sample, levels = c("Main", "Small Hosp.", "Large Hosp.", "Rural", "Urban", "Works w/ 1 Sys.",
                                                        "Works w/ >1 Sys."))

cbbPalette <- c("#000000", "#E69F00", "#D55E00", "#56B4E9", "#0072B2", "#009E73", "#006400")
dodge <- position_dodge(width=0.5)
ggplot(data=plot_data, aes(x=Outcome, y=estimate_perc, color=Sample)) + geom_point(position = dodge) +
  geom_errorbar(aes(ymax=upper,ymin=lower, color=Sample),size=.5, position = dodge, width=.2) + 
  scale_colour_manual(values=cbbPalette) + theme_bw() + geom_hline(yintercept=0, linetype="dashed") +
  xlab("\nVariable") + ylab("Estimate and 95% CI as Percent of Mean\n") +theme(text = element_text(size = 15))

ggsave("Objects/heterog_plot.pdf", width=10, height=6, units = "in")





### make heterogeneity plot for integration subsamples
# RETIREMENT ------------------------------------------
models <- lapply(Het_Data_int, function(x) {
  row <- x[[2]]
  
  all <- att_gt(yname = "retire",                # LHS Variable
                gname ="minyr_EHR",          # First year a unit is treated. (set to 0 if never treated)
                idname = "DocNPI",               # ID
                tname = "year",                  # Time Variable
                xformla = ~grad_year,            # Time-invariant controls
                data= dplyr::filter(x[[1]],minyr_EHR>0),
                est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
                control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
                clustervars = "DocNPI",          # Cluster Variables          
                anticipation=0,
                base_period = "varying" # can set a number of years to account for anticipation effects
  )
  
  list(all=all, row=row)
})

models_agg <- lapply(models, function(x) {
  row <- x[[2]]
  
  agg <- aggte(x[[1]], type = "dynamic",na.rm=T)
  
  list(agg=agg, row=row)
})

chart_data <- lapply(models_agg, function(x) {
  row <- x[[2]] %>%
    mutate(coef=x[["agg"]][["overall.att"]],
           se=x[["agg"]][["overall.se"]])
  trow <- t(row)
  list(trow)
})

chart_data_frame <- t(as.data.frame(chart_data))
rownames(chart_data_frame) <- NULL

retire_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, IPA, OPHO, CPHO, ISM) %>%
  mutate(Outcome="Exit")



# OFFICE FRAC ------------------------------------------
models <- lapply(Het_Data_int, function(x) {
  row <- x[[2]]
  
  all <- att_gt(yname = "pos_office",                # LHS Variable
                gname ="minyr_EHR",          # First year a unit is treated. (set to 0 if never treated)
                idname = "DocNPI",               # ID
                tname = "year",                  # Time Variable
                xformla = ~grad_year,            # Time-invariant controls
                data= dplyr::filter(x[[1]],minyr_EHR>0 & ever_retire==0),
                est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
                control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
                clustervars = "DocNPI",          # Cluster Variables          
                anticipation=0,
                base_period = "varying" # can set a number of years to account for anticipation effects
  )
  
  list(all=all, row=row)
})

models_agg <- lapply(models, function(x) {
  row <- x[[2]]
  
  agg <- aggte(x[[1]], type = "dynamic",na.rm=T)
  
  list(agg=agg, row=row)
})

chart_data <- lapply(models_agg, function(x) {
  row <- x[[2]] %>%
    mutate(coef=x[["agg"]][["overall.att"]],
           se=x[["agg"]][["overall.se"]])
  trow <- t(row)
  list(trow)
})

chart_data_frame <- t(as.data.frame(chart_data))
rownames(chart_data_frame) <- NULL

fracoffice_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, IPA, OPHO, CPHO, ISM) %>%
  mutate(Outcome="Frac. Patients in Office")





# OFFICE INDICATOR ------------------------------------------
models <- lapply(Het_Data_int, function(x) {
  row <- x[[2]]
  
  all <- att_gt(yname = "work_in_office",                # LHS Variable
                gname ="minyr_EHR",          # First year a unit is treated. (set to 0 if never treated)
                idname = "DocNPI",               # ID
                tname = "year",                  # Time Variable
                xformla = ~grad_year,            # Time-invariant controls
                data= dplyr::filter(x[[1]],minyr_EHR>0 & ever_retire==0),
                est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
                control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
                clustervars = "DocNPI",          # Cluster Variables          
                anticipation=0,
                base_period = "varying" # can set a number of years to account for anticipation effects
  )
  
  list(all=all, row=row)
})

models_agg <- lapply(models, function(x) {
  row <- x[[2]]
  
  agg <- aggte(x[[1]], type = "dynamic",na.rm=T)
  
  list(agg=agg, row=row)
})

chart_data <- lapply(models_agg, function(x) {
  row <- x[[2]] %>%
    mutate(coef=x[["agg"]][["overall.att"]],
           se=x[["agg"]][["overall.se"]])
  trow <- t(row)
  list(trow)
})

chart_data_frame <- t(as.data.frame(chart_data))
rownames(chart_data_frame) <- NULL

indoffice_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, IPA, OPHO, CPHO, ISM)%>%
  mutate(Outcome="Work in Office")




# PATIENT COUNT ------------------------------------------
models <- lapply(Het_Data_int, function(x) {
  row <- x[[2]]
  
  all <- att_gt(yname = "npi_unq_benes",                # LHS Variable
                gname ="minyr_EHR",          # First year a unit is treated. (set to 0 if never treated)
                idname = "DocNPI",               # ID
                tname = "year",                  # Time Variable
                xformla = ~grad_year,            # Time-invariant controls
                data= dplyr::filter(x[[1]],minyr_EHR>0 & ever_retire==0 & never_newnpi==1),
                est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
                control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
                clustervars = "DocNPI",          # Cluster Variables          
                anticipation=0,
                base_period = "varying" # can set a number of years to account for anticipation effects
  )
  
  list(all=all, row=row)
})

models_agg <- lapply(models, function(x) {
  row <- x[[2]]
  
  agg <- aggte(x[[1]], type = "dynamic",na.rm=T)
  
  list(agg=agg, row=row)
})

chart_data <- lapply(models_agg, function(x) {
  row <- x[[2]] %>%
    mutate(coef=x[["agg"]][["overall.att"]],
           se=x[["agg"]][["overall.se"]])
  trow <- t(row)
  list(trow)
})

chart_data_frame <- t(as.data.frame(chart_data))
rownames(chart_data_frame) <- NULL

patcount_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, IPA, OPHO, CPHO, ISM) %>%
  mutate(Outcome="Number Patients")





# CLAIM PER PATIENT ------------------------------------------
models <- lapply(Het_Data_int, function(x) {
  row <- x[[2]]
  
  all <- att_gt(yname = "claim_per_patient",                # LHS Variable
                gname ="minyr_EHR",          # First year a unit is treated. (set to 0 if never treated)
                idname = "DocNPI",               # ID
                tname = "year",                  # Time Variable
                xformla = ~grad_year,            # Time-invariant controls
                data= dplyr::filter(x[[1]],minyr_EHR>0 & ever_retire==0 & never_newnpi==1),
                est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
                control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
                clustervars = "DocNPI",          # Cluster Variables          
                anticipation=0,
                base_period = "varying" # can set a number of years to account for anticipation effects
  )
  
  list(all=all, row=row)
})

models_agg <- lapply(models, function(x) {
  row <- x[[2]]
  
  agg <- aggte(x[[1]], type = "dynamic",na.rm=T)
  
  list(agg=agg, row=row)
})

chart_data <- lapply(models_agg, function(x) {
  row <- x[[2]] %>%
    mutate(coef=x[["agg"]][["overall.att"]],
           se=x[["agg"]][["overall.se"]])
  trow <- t(row)
  list(trow)
})

chart_data_frame <- t(as.data.frame(chart_data))
rownames(chart_data_frame) <- NULL

cpp_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, IPA, OPHO, CPHO, ISM) %>%
  mutate(Outcome="Claims per Patient")



# Create Chart
plot_data <- rbind(fracoffice_data_frame, indoffice_data_frame) %>%
  mutate(mean=ifelse(Outcome=="Exit",.03,NA),
         mean=ifelse(Outcome=="Frac. Patients in Office",.08,mean),
         mean=ifelse(Outcome=="Work in Office",.27,mean),
         mean=ifelse(Outcome=="Number Patients",331,mean),
         mean=ifelse(Outcome=="Claims per Patient",4.09,mean)) %>%
  mutate(Sample=ifelse(IPA==1, "IPA", NA),
         Sample=ifelse(OPHO==1, "OPHO", Sample),
         Sample=ifelse(CPHO==1, "CPHO", Sample),
         Sample=ifelse(ISM==1, "ISM", Sample),
         Sample=ifelse(is.na(Sample),"Main", Sample)) %>%
  select(-IPA, -OPHO, -CPHO, -ISM) %>%
  mutate(estimate_perc=coef/mean) %>%
  mutate(upper=(coef+(1.96*se)),
         lower=(coef-(1.96*se)))
plot_data$Outcome <- factor(plot_data$Outcome, levels=c("Exit", "Frac. Patients in Office", "Work in Office", "Number Patients", "Claims per Patient"))
plot_data$Sample <- factor(plot_data$Sample, levels = c("Main", "IPA", "OPHO", "CPHO", "ISM"))

cbbPalette <- c("#000000", "#E69F00", "#D55E00", "#0072B2", "#009E73")
dodge <- position_dodge(width=0.5)
ggplot(data=plot_data, aes(x=Outcome, y=coef, color=Sample)) + geom_point(position = dodge) +
  geom_errorbar(aes(ymax=upper,ymin=lower, color=Sample),size=.5, position = dodge, width=.2) + 
  scale_colour_manual(values=cbbPalette) + theme_bw() + geom_hline(yintercept=0, linetype="dashed") +
  xlab("\nVariable") + ylab("Estimate and 95% CI\n") + theme(text = element_text(size = 15))

ggsave("Objects/heterog_plot_int_office.pdf", width=8, height=6, units = "in")

