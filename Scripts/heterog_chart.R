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
Physician_Data <- readRDS(paste0(created_data_path,"Physician_Data.rds"))


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

chart_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, rural, urban, small, large) 

par(oma=c(1,0,1,1))

# Create plot
labels <- list("Rural", "Urban", "Small Hosp.", "Large Hosp.")

source(paste0(function_path,"spec_chart_function.R"))

schart(chart_data_frame, labels, highlight=5, 
       order="asis", heights=c(1,.75),
       col.est=c("grey70","#E69F00"),
       col.dot=c("grey70", "grey90", "#E69F00", "#E69F00"),
       ylab="Coefficient and 95% C.I.\n")
legend("bottomleft", lwd=2, col=c("#E69F00"), c("Main"), inset=.02)



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

chart_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, rural, urban, small, large) 

par(oma=c(1,0,1,1))

# Create plot
source(paste0(function_path,"spec_chart_function.R"))
schart(chart_data_frame, labels, highlight=5, 
       order="asis", heights=c(1,.75),
       col.est=c("grey70","#E69F00"),
       col.dot=c("grey70", "grey90", "#E69F00", "#E69F00"),
       ylab="Coefficient and 95% C.I.\n")
legend("bottomleft", lwd=2, col=c("#E69F00"), c("Main"), inset=.02)




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

chart_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, rural, urban, small, large) 

par(oma=c(1,0,1,1))

# Create plot
source(paste0(function_path,"spec_chart_function.R"))
schart(chart_data_frame, labels, highlight=5, 
       order="asis", heights=c(1,.75),
       col.est=c("grey70","#E69F00"),
       col.dot=c("grey70", "grey90", "#E69F00", "#E69F00"),
       ylab="Coefficient and 95% C.I.\n")
legend("bottomleft", lwd=2, col=c("#E69F00"), c("Main"), inset=.02)




# CHANGE ZIP ------------------------------------------
models <- lapply(Het_Data, function(x) {
  row <- x[[2]]
  
  all <- att_gt(yname = "change_zip",                # LHS Variable
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

chart_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, rural, urban, small, large) 

par(oma=c(1,0,1,1))

# Create plot
source(paste0(function_path,"spec_chart_function.R"))
schart(chart_data_frame, labels, highlight=5, 
       order="asis", heights=c(1,.75),
       col.est=c("grey70","#E69F00"),
       col.dot=c("grey70", "grey90", "#E69F00", "#E69F00"),
       ylab="Coefficient and 95% C.I.\n")
legend("bottomleft", lwd=2, col=c("#E69F00"), c("Main"), inset=.02)




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

chart_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, rural, urban, small, large) 

par(oma=c(1,0,1,1))

# Create plot
source(paste0(function_path,"spec_chart_function.R"))
schart(chart_data_frame, labels, highlight=5, 
       order="asis", heights=c(1,.75),
       col.est=c("grey70","#E69F00"),
       col.dot=c("grey70", "grey90", "#E69F00", "#E69F00"),
       ylab="Coefficient and 95% C.I.\n")
legend("bottomleft", lwd=2, col=c("#E69F00"), c("Main"), inset=.02)




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

chart_data_frame <- as.data.frame(chart_data_frame) %>%
  select(coef, se, rural, urban, small, large) 

par(oma=c(1,0,1,1))

# Create plot
source(paste0(function_path,"spec_chart_function.R"))
schart(chart_data_frame, labels, highlight=5, 
       order="asis", heights=c(1,.75),
       col.est=c("grey70","#E69F00"),
       col.dot=c("grey70", "grey90", "#E69F00", "#E69F00"),
       ylab="Coefficient and 95% C.I.\n")
legend("bottomleft", lwd=2, col=c("#E69F00"), c("Main"), inset=.02)
