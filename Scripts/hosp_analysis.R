library(readr)
library(dplyr)
library(did)



# read in hospital level data
hosp_data <- readRDS(paste0(created_data_path, "hosp_data.rds"))

# define the first year a hospital implements an EHR
hosp_data <- hosp_data %>%
  mutate(EHR=ifelse(EHLTH==2 | !is.na(EHR_HIMSS1),1,ifelse(EHLTH==0 | EHLTH==1,0,NA))) 

minyr_EHR <- hosp_data %>%
  filter(EHR==1) %>%
  group_by(HospNPI) %>%
  mutate(minyr_EHR = min(year)) %>%
  ungroup() %>%
  distinct(HospNPI, minyr_EHR)

hosp_data <- hosp_data %>%
  left_join(minyr_EHR, by="HospNPI")

hosp_data <- hosp_data %>%
  mutate(minyr_EHR = ifelse(is.na(minyr_EHR),0,minyr_EHR))

# read in HCRIS data
final_hcris_data <- read_csv(paste0(raw_data_path, "/final_hcris_data.csv")) %>%
  select(-fy_start, -fy_end, -date_processed, -date_created, -source)

# merge to main hospital data
hosp_data <- hosp_data %>%
  left_join(final_hcris_data, by=c("MCRNUM"="provider_number", "year"))

# create summary stats by year
means <- hosp_data %>%
  group_by(year) %>%
  summarise_all(mean, na.rm=T)

# create change in behavior variables
hosp_data <- hosp_data %>%
  separate(MADMIN, sep = ",", into = c("admin_name", "other")) %>%
  select(-other) 

hosp_data <- hosp_data %>%
  mutate(count=1) %>%
  group_by(MCRNUM) %>%
  mutate(sum = sum(count)) %>%
  filter(sum>=7) %>%
  mutate(lag_name = dplyr::lag(admin_name)) %>%
  mutate(lag_sys = dplyr::lag(SYSID)) %>%
  ungroup() %>%
  mutate(change_admin = ifelse(admin_name==lag_name,0,1),
         change_sys = ifelse((is.na(SYSID) & !is.na(lag_sys)) | (!is.na(SYSID) & is.na(lag_sys)),1,0),
         change_sys = ifelse(year==2009,0,change_sys)) %>%
  mutate(change_admin = ifelse(is.na(change_admin) & (year==2009 | year==2015),0,change_admin)) %>%
  select(-lag_name, -count, -sum, -lag_sys)

# create variable for operating expenses besides medical records expenses
hosp_data <- hosp_data %>%
  group_by(HospNPI, year) %>%
  mutate(expenses_minus_medrecords = sum(operating_expenses, -medrecords_expenses, na.rm=T)) %>%
  ungroup()

# hospital level analysis with outcomes of other hospital behaviors
varlist <- list("change_admin", "SUBS", "BDTOT", "IPAP", "OPHP", "CPHP", "ISMP", "PHYGP", "capital_cost_buildings", 
                "capital_cost_movableequip", "expenses_minus_medrecords", "IPAHOS", "OPHOSHOS", "CPHOHOS", "ISMHOS", "FTRNTF")

models <- lapply(varlist, function(x){
  all <- att_gt(yname = x,                # LHS Variable
                gname = "minyr_EHR",             # First year a unit is treated. (set to 0 if never treated)
                idname = "HospNPI",               # ID
                tname = "year",                  # Time Variable
                xformla = NULL,                 # No covariates
                data = dplyr::filter(hosp_data, minyr_EHR>0),
                est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
                control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
                clustervars = "HospNPI",          # Cluster Variables          
                anticipation=0,
                base_period = "varying" # can set a number of years to account for anticipation effects
  )
})

models_agg <- lapply(models, function(x){
  agg <- aggte(x, type = "dynamic",na.rm=T)
  p <- x$Wpval
  
  list(agg=agg, p=p)
})

graph_data <- lapply(models_agg, function(x){
  data <- as.data.frame(x[["agg"]][["egt"]]) %>%
    dplyr::rename(year=1) %>%
    cbind(as.data.frame(x[["agg"]][["att.egt"]])) %>%
    dplyr::rename(att=2) %>%
    cbind(as.data.frame(x[["agg"]][["se.egt"]])) %>%
    dplyr::rename(se=3) %>%
    mutate(upper=att+(1.96*se),
           lower=att-(1.96*se),
           group="All",
           year=as.factor(year)
    )
  p <- x[["p"]]
  
  list(data=data, p=p)
})

graphs <- lapply(graph_data, function(x){
  graph <- ggplot(x$data, aes(year, att)) +  
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
    labs(caption=paste0("pre-trends test p-value: ",round(as.numeric(x$p),2))) + 
    theme(plot.caption=element_text(hjust = 0, size=15))
  
  list(graph=graph)
})

# create chart with overall ATT from all outcome variables
chart_data <- data.frame(est=as.numeric(), se=as.numeric() , outcome = as.character())
for (j in 1:length(models_agg)){
  est <- models_agg[[j]][["agg"]][["overall.att"]]
  se <- models_agg[[j]][["agg"]][["overall.se"]]
  outcome <- models_agg[[j]][["agg"]][["DIDparams"]][["yname"]]
  
  chart_data <- chart_data %>%
    add_row(est=est, se=se, outcome=outcome) 
}

# create table showing these results
chart_data <- chart_data %>%
  mutate(outcome = ifelse(outcome=="change_admin","Change Admin",outcome),
         outcome = ifelse(outcome=="SUBS","Owns Subsidiary",outcome),
         outcome = ifelse(outcome=="BDTOT","No. Beds",outcome),
         outcome = ifelse(outcome=="IPAP","No. Physicians IPA",outcome),
         outcome = ifelse(outcome=="OPHP","No. Physicians OPHO",outcome),
         outcome = ifelse(outcome=="CPHP","No. Physicians CPHO",outcome),
         outcome = ifelse(outcome=="ISMP","No. Physicians FIO",outcome),
         outcome = ifelse(outcome=="capital_cost_buildings","Capital Expenses (Buildings)",outcome),
         outcome = ifelse(outcome=="capital_cost_movableequip","Capital Expenses (Equipment)",outcome),
         outcome = ifelse(outcome=="expenses_minus_medrecords","Expenses w/out Med. Records",outcome),
         outcome = ifelse(outcome=="PHYGP", "No. Physicians", outcome),
         outcome = ifelse(outcome=="IPAHOS","IPA",outcome),
         outcome = ifelse(outcome=="OPHOSHOS","OPHO",outcome),
         outcome = ifelse(outcome=="CPHHOS","CPHO",outcome),
         outcome = ifelse(outcome=="ISMHOS","FIO",outcome),
         outcome = ifelse(outcome=="FTRNTF", "No. Nurses", outcome)) %>%
  select(outcome, est, se) 

knitr::kable(chart_data[c(9,10,11,2,1,3,16,8,12,13,14,15),],
             format="latex",
             table.envir="table",
             col.names=c("Outcome","Estimate","SE"),
             digits=2,
             caption="Association between EHR and Other Hosp. Behaviors",
             booktabs=TRUE,
             escape=F,
             align=c("l","c","c"),
             position="h",
             row.names = F) %>%
  kable_styling(full_width=F) %>%
  column_spec(1, width = "20em") %>%
  pack_rows(index = c("Non-EHR Investment/Organizational Changes" = 6, "Workforce" = 6))













