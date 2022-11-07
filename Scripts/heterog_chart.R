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
  filter(ever_rural==1)
Urban_Data <- Physician_Data %>%
  filter(ever_urban==1)
Smallhosp_Data <- Physician_Data %>%
  filter(small==1)
Largehosp_Data <- Physician_Data %>%
  filter(large==1)

data_coms <- crossing(rural=c(TRUE,FALSE), urban=c(TRUE,FALSE), small=c(TRUE,FALSE), large=c(TRUE,FALSE)) %>%
  filter(rural==TRUE & urban==FALSE & small==FALSE & large==FALSE |
           rural==FALSE & urban==TRUE & small==FALSE & large==FALSE |
           rural==FALSE & urban==FALSE & small==TRUE & large==FALSE |
           rural==FALSE & urban==FALSE & small==FALSE & large==TRUE |
           rural==FALSE & urban==FALSE & small==FALSE & large==FALSE)

Het_Data <- list(list(Rural_Data, data_coms[2,]), list(Urban_Data, data_coms[3,]),
                 list(Smallhosp_Data,data_coms[4,]), list(Largehosp_Data, data_coms[5,]),
                 list(Physician_Data,data_coms[1,]))


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
  select(coef, se, rural, urban, small, large) %>%
  mutate(Outcome="Retire")



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
  select(coef, se, rural, urban, small, large) %>%
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
  select(coef, se, rural, urban, small, large)%>%
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
  select(coef, se, rural, urban, small, large) %>%
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
  select(coef, se, rural, urban, small, large) %>%
  mutate(Outcome="Claims per Patient")



# Create Chart
plot_data <- rbind(retire_data_frame, fracoffice_data_frame, indoffice_data_frame,
                   patcount_data_frame, cpp_data_frame) %>%
  mutate(mean=ifelse(Outcome=="Retire",.03,NA),
         mean=ifelse(Outcome=="Frac. Patients in Office",.08,mean),
         mean=ifelse(Outcome=="Work in Office",.27,mean),
         mean=ifelse(Outcome=="Number Patients",331,mean),
         mean=ifelse(Outcome=="Claims per Patient",4.09,mean)) %>%
  mutate(Sample=ifelse(rural==1, "Rural", NA),
         Sample=ifelse(urban==1, "Urban", Sample),
         Sample=ifelse(small==1, "Small Hosp.", Sample),
         Sample=ifelse(large==1, "Large Hosp.", Sample),
         Sample=ifelse(is.na(Sample),"Main", Sample)) %>%
  select(-rural, -urban, -small, -large) %>%
  mutate(estimate_perc=coef/mean) %>%
  mutate(upper=(coef+(1.96*se))/mean,
         lower=(coef-(1.96*se))/mean)
plot_data$Outcome <- factor(plot_data$Outcome, levels=c("Retire", "Frac. Patients in Office", "Work in Office", "Number Patients", "Claims per Patient"))

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
dodge <- position_dodge(width=0.5)
ggplot(data=plot_data, aes(x=Outcome, y=estimate_perc, color=Sample)) + geom_point(position = dodge) +
  geom_errorbar(aes(ymax=upper,ymin=lower, color=Sample),size=.5, position = dodge, width=.2) + 
  scale_colour_manual(values=cbbPalette) + theme_bw() + geom_hline(yintercept=0, linetype="dashed") +
  xlab("\nVariable") + ylab("Estimate and 95% CI\n")

ggsave("Objects/heterog_plot.pdf", width=8, height=5, units = "in")

