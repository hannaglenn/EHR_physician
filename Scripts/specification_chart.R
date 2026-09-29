library(tidyr)
library(readr)
library(stringr)
library(dplyr)
library(did)
library(ggplot2)


## DATA 3------------------------------------------------ ####
phys_npidata <- read_rds(paste0(created_data_path,"phys_npidata.rds"))
hosp_npidata <- read_rds(paste0(created_data_path,"hosp_npidata.rds"))

temp_npidata <- rbind(phys_npidata, hosp_npidata)

rm(phys_npidata, hosp_npidata)

# Read in MDPPAS NPI list 
npi_list <- read_rds(paste0(created_data_path,"npi_list.rds"))

npi_list <- npi_list %>%
  dplyr::mutate(keep=1)

#  in Shared Patient Data, merge to temp_npi along the way 
for (t in 2009:2015){
  SP.year <- read_csv(paste0(raw_data_path,"/PSPP/pspp",t,"_30.csv"),
                      col_types=cols(npi1=col_character(),
                                     npi2=col_character()
                      ))
  SP.year <- SP.year %>%
    select(npi1,npi2,year,samedaycount) %>%
    distinct()
  
  SP.year<- SP.year %>%
    left_join(temp_npidata, by=c("npi1"="npi")) %>%
    dplyr::rename(PCP1=PCP,hospital1=hospital) 
  
  SP.year <- SP.year %>%
    left_join(temp_npidata, by=c("npi2"="npi")) %>%
    dplyr::rename(PCP2=PCP,hospital2=hospital) %>%
    select(npi1,npi2,samedaycount,year,PCP1,PCP2,hospital1,hospital2)
  
  # filter so that all is left is hospital-physician pairs
  SP.year <- SP.year %>%
    dplyr::filter((PCP1==1 & hospital2==1) | (hospital1==1 & PCP2==1))
  
  assign(paste0("PSPD",t),SP.year)
}

phys_hosp_pairs <- rbind(PSPD2009, PSPD2010, PSPD2011, PSPD2012, PSPD2013, PSPD2014, PSPD2015)
# 12.6 mill observations

rm(PSPD2009, PSPD2010, PSPD2011, PSPD2012, PSPD2013, PSPD2014, PSPD2015, 
   temp_npidata)

# Change to physician and hospital npis instead of npi1 and npi2
phys_hosp_pairs <- phys_hosp_pairs %>%
  mutate(PCP1=ifelse(is.na(PCP1),0,PCP1), PCP2=ifelse(is.na(PCP2),0,PCP2),
         hospital1=ifelse(is.na(hospital1),0,hospital1), hospital2=ifelse(is.na(hospital2),0,hospital2)) %>%
  mutate(HospNPI=ifelse(hospital1==1,npi1,npi2),
         DocNPI=ifelse(PCP1==1,npi1,npi2)) %>%
  select(year,HospNPI,DocNPI,samedaycount)

phys_hosp_pairs <- phys_hosp_pairs %>%
  group_by(year,DocNPI, HospNPI) %>%
  mutate(samedaycount_combine=sum(samedaycount,na.rm=T))

phys_hosp_pairs <- phys_hosp_pairs %>%
  ungroup() %>%
  select(year,HospNPI,DocNPI,samedaycount_combine) %>%
  distinct() %>%
  dplyr::rename(samedaycount=samedaycount_combine)
# 7.1 million obs. 


# Merge to NPI List of Physicians
phys_hosp_pairs <- phys_hosp_pairs %>%
  mutate(DocNPI=as.numeric(DocNPI)) %>%
  left_join(npi_list, by=c("DocNPI"="npi"))

phys_hosp_pairs <- phys_hosp_pairs %>%
  filter(keep==1) %>%
  select(-keep)
# 3.7 mill obs.

rm(npi_list, SP.year)

# Create indicators for which type of restriction to make on hospital-physician pairs
phys_hosp_pairs <- phys_hosp_pairs %>% ungroup() %>%
  mutate(count=1) %>%
  group_by(DocNPI,HospNPI) %>%
  mutate(yrs=sum(count)) %>%
  mutate(sum=sum(samedaycount,na.rm=T)) %>%
  mutate(samedaycount_30=ifelse(sum>30*yrs,1,0),
         samedaycount_10=ifelse(sum>10*yrs,1,0),
         samedaycount_60=ifelse(sum>60*yrs,1,0)) %>%
  select(-sum, -yrs, -count) %>%
  ungroup() %>%
  filter(samedaycount_30==1 | samedaycount_10==1 | samedaycount_60==1)

Physician_Data_10 <- phys_hosp_pairs %>%
  filter(samedaycount_10==1)
Physician_Data_30 <- phys_hosp_pairs %>%
  filter(samedaycount_30==1)
Physician_Data_60 <- phys_hosp_pairs %>%
  filter(samedaycount_60==1)

Data_List <- list(Physician_Data_10, Physician_Data_30, Physician_Data_60)

Data_List <- lapply(Data_List, function(x){
  data <- x %>%
    ungroup() %>%
    group_by(DocNPI,HospNPI) %>%
    mutate(pairID = cur_group_id())
})

### DATA 4 -------------------------------------------------------------------------------------
TAGS <- c("10", "30", "60")   # must match the order of Data_List

## PART 1 -- Reference data, loaded once ----------

##---- Physician Compare --------------------------------------
PhysCompare <- read_csv(paste0(raw_data_path, "DAC_NationalDownloadableFile.csv")) %>%
  dplyr::rename(gender = gndr, grad_year = `Grd_yr`) %>%
  select(NPI, grad_year)

##---- AHA-NPI crosswalk --------------------------------------
AHANPI_cw <- read_rds(paste0(created_data_path, "AHANPI_cw.rds"))

##---- AHA main survey ----------------------------------------
AHAmainsurvey <- read_csv(paste0(raw_data_path, "AHAdata_20052023.csv")) %>%
  select(ID, YEAR, MCRNUM, MNAME, MADMIN, SYSID, SUBS, EHLTH,
         BDTOT, FTMDTF, FTRES, PTMDTF, PTRES, JNTPH, PHYGP,
         IPAHOS, OPHOSHOS, CPHOHOS, ISMHOS, IPAP, OPHP, CPHP, ISMP, FTRNTF) %>%
  filter(!is.na(MCRNUM)) %>%
  mutate(ID = as.character(ID)) %>%
  filter(YEAR <= 2015 & YEAR >= 2009)

##---- HIMSS ---------------------------------------------------
years <- 2009:2015
read_years <- function(years, f) bind_rows(lapply(years, f))

HAEntity <- read_years(years, function(t) {
  read_xlsx(file.path(raw_data_path, "HIMSS", paste0("HAEntity", t, ".xlsx")),
            col_types = "text") %>%
    mutate(year = t) %>%
    select(HAEntityId, SurveyId, ParentId, UniqueId, year,
           HAEntityType, MedicareNumber, Name, City, State) %>%
    mutate(across(c(HAEntityId, SurveyId, ParentId, UniqueId), as.integer),
           MedicareNumber = na_if(trimws(MedicareNumber), ""))
}) %>%
  filter(!is.na(MedicareNumber))

HAEntityApplication <- read_years(years, function(t) {
  read_csv(file.path(raw_data_path, "HIMSS",
                     paste0("HAEntityApplication", t, ".csv")),
           show_col_types = FALSE) %>%
    mutate(year = t) %>%
    select(HAEntityId, year, Application, Category, Status)
}) %>%
  filter(Category == "Electronic Medical Record")

app <- HAEntityApplication %>%
  left_join(HAEntity %>%
              select(HAEntityId, year, UniqueId, SurveyId,
                     HAEntityType, MedicareNumber, Name, State),
            by = c("HAEntityId", "year")) %>%
  mutate(Application = recode(Application,
                              "Clinical Data Repository/EMR" = "Clinical Data Repository"))

status_levels <- c("Not Reported", "Not Automated", "Not Yet Contracted",
                   "Contracted/Not Yet Installed", "Installation in Process",
                   "To be Replaced", "Live and Operational")

app <- app %>%
  mutate(Status = trimws(Status),
         Status = recode(Status,
                         "To Be Replaced"            = "To be Replaced",
                         "Installation In Process"   = "Installation in Process",
                         "NotAutomatedNot Automated" = "Not Automated"))

unmapped <- setdiff(unique(app$Status), status_levels)
if (length(unmapped)) stop("Unmapped Status: ", paste(unmapped, collapse = " | "))

app <- app %>% mutate(Status = factor(Status, levels = status_levels, ordered = TRUE))

collapsed <- app %>%
  group_by(UniqueId, year, Application) %>%
  summarise(HAEntityId     = first(HAEntityId),
            SurveyId       = first(SurveyId),
            HAEntityType   = first(HAEntityType),
            MedicareNumber = first(MedicareNumber),
            Status         = max(Status),
            n_products     = n(),
            .groups = "drop")

himss_hosp <- collapsed %>%
  filter(!is.na(MedicareNumber)) %>%
  mutate(live = Status %in% c("Live and Operational", "To be Replaced"))

app_short <- c(
  "Clinical Data Repository"                     = "cdr",
  "Clinical Decision Support System (CDSS)"      = "cdss",
  "Computerized Practitioner Order Entry (CPOE)" = "cpoe",
  "Order Entry (Includes Order Communications)"  = "order_entry",
  "Physician Documentation"                      = "phys_doc",
  "Physician Portal"                             = "phys_portal",
  "Patient Portal"                               = "patient_portal",
  "Business Intelligence - Clinical"             = "bi_clinical")

himss_hosp <- himss_hosp %>% mutate(app = unname(app_short[Application]))
stopifnot(!any(is.na(himss_hosp$app)))

ccn_app <- himss_hosp %>%
  group_by(MedicareNumber, year, app) %>%
  summarise(live = any(live), n_entities = n_distinct(UniqueId), .groups = "drop")

wide <- ccn_app %>%
  select(MedicareNumber, year, app, live) %>%
  pivot_wider(id_cols = c(MedicareNumber, year),
              names_from = app, values_from = live)

phys_apps      <- c("cpoe", "phys_doc", "phys_portal")
phys_apps_cdss <- c(phys_apps, "cdss")
core_apps      <- c("cdr", "cdss", "cpoe", "order_entry", "phys_doc", "phys_portal")
present <- function(v) intersect(v, names(wide))

himss_analysis <- wide %>%
  mutate(
    n_phys_facing = rowSums(across(all_of(present(phys_apps))), na.rm = TRUE),
    n_any         = rowSums(across(all_of(present(core_apps))), na.rm = TRUE),
    phys_facing_any = n_phys_facing > 0,
    phys_facing_all = n_phys_facing == length(present(phys_apps)),
    any_app         = n_any > 0,
    phys_facing_any_cdss =
      rowSums(across(all_of(present(phys_apps_cdss))), na.rm = TRUE) > 0) %>%
  relocate(MedicareNumber, year)

stopifnot(!any(duplicated(himss_analysis[, c("MedicareNumber", "year")])))

##---- HCRIS HITECH exposure ----------------------------------
hcris <- read_csv(paste0(raw_data_path, "/final_hcris_data.csv")) %>%
  select(-fy_start, -fy_end, -date_processed, -date_created, -source)

exposure <- hcris %>%
  filter(year >= 2006 & year <= 2008) %>%
  distinct(year, provider_number, tot_operating_exp, tot_discharges,
           mcare_beddays, mcaid_beddays, total_beddays) %>%
  group_by(provider_number) %>%
  select(-year) %>%
  summarise_all(list(mean)) %>%
  ungroup() %>%
  mutate(discharges_fun = ifelse(tot_discharges < 1150, 2000000,
                                 ifelse(tot_discharges > 23000, 4370200,
                                        1770000 + (200 * tot_discharges)))) %>%
  mutate(mcaid_exposure = discharges_fun * (mcaid_beddays / total_beddays),
         mcare_exposure = discharges_fun * (mcare_beddays / total_beddays)) %>%
  distinct(provider_number, mcaid_exposure, mcare_exposure)
pairs <- Data_List[[1]]
build_physician_data <- function(pairs, tag, write_files = TRUE,
                                 make_ehr_table = FALSE) {
  
  message("\n===== Building sample: ", tag, " =====")
  
  ##---- Graduation year, drop post-2005 grads ----------------
  data <- pairs %>%
    ungroup() %>%
    left_join(PhysCompare, by = c("DocNPI" = "NPI")) %>%
    distinct() %>%
    filter(grad_year < 2005 | is.na(grad_year))
  
  ##---- AHA crosswalk ----------------------------------------
  data <- data %>%
    mutate(HospNPI = as.double(HospNPI)) %>%
    left_join(AHANPI_cw, by = c("HospNPI" = "NPI")) %>%
    filter(!is.na(AHAID))
  
  ##---- AHA main survey --------------------------------------
  data <- data %>%
    ungroup() %>%
    mutate(AHAID = as.character(AHAID)) %>%
    left_join(AHAmainsurvey, by = c("AHAID" = "ID", "year" = "YEAR"),
              na_matches = "never") %>%
    filter(!is.na(MCRNUM))
  
  ##---- Integration type indicators --------------------------
  data <- data %>%
    group_by(MCRNUM) %>%
    mutate(ever_IPA  = ifelse(IPAHOS   == 1, 1, NA),
           ever_OPHO = ifelse(OPHOSHOS == 1, 1, NA),
           ever_CPHO = ifelse(CPHOHOS  == 1, 1, NA),
           ever_ISM  = ifelse(ISMHOS   == 1, 1, NA)) %>%
    fill(ever_IPA, ever_OPHO, ever_CPHO, ever_ISM, .direction = "downup") %>%
    ungroup() %>%
    group_by(DocNPI) %>%
    mutate(phys_ever_IPA  = ifelse(ever_IPA  == 1, 1, NA),
           phys_ever_OPHO = ifelse(ever_OPHO == 1, 1, NA),
           phys_ever_CPHO = ifelse(ever_CPHO == 1, 1, NA),
           phys_ever_ISM  = ifelse(ever_ISM  == 1, 1, NA)) %>%
    fill(phys_ever_IPA, phys_ever_OPHO, phys_ever_CPHO, phys_ever_ISM,
         .direction = "downup") %>%
    mutate(across(c(phys_ever_IPA, phys_ever_OPHO, phys_ever_CPHO, phys_ever_ISM),
                  ~ ifelse(is.na(.), 0, .))) %>%
    ungroup()
  
  ##---- HIMSS -------------------------------------------------
  data <- data %>%
    left_join(himss_analysis, by = c("year", "MCRNUM" = "MedicareNumber"),
              na_matches = "never")
  
  ##---- Interpolate EHLTH between equal-valued years ---------
  for (lev in 0:2) {
    data <- data %>%
      group_by(AHAID) %>%
      mutate(fy = suppressWarnings(min(year[EHLTH == lev], na.rm = TRUE)),
             ly = suppressWarnings(max(year[EHLTH == lev], na.rm = TRUE)),
             fy = ifelse(is.infinite(fy), NA, fy),
             ly = ifelse(is.infinite(ly), NA, ly),
             EHLTH = ifelse(!is.na(fy) & fy < year & year < ly & is.na(EHLTH),
                            lev, EHLTH)) %>%
      select(-fy, -ly) %>%
      ungroup()
  }
  
  ##---- Define EHR --------------------------------------------
  data <- data %>%
    mutate(any_EHR = case_when(
      EHLTH == 2                ~ 1L,
      phys_facing_any %in% TRUE ~ 1L,
      EHLTH %in% c(0, 1)        ~ 0L,
      TRUE                      ~ NA_integer_))
  
  if (make_ehr_table) build_ehr_table(data, tag)
  
  ##---- Drop VA-only physicians ------------------------------
  VA <- data %>%
    mutate(VA = ifelse(str_detect(MNAME, "Veteran|\\sVA\\s|\\sVHA\\s"), 1, 0)) %>%
    filter(VA == 1) %>%
    distinct(pairID, VA)
  
  data <- data %>%
    left_join(VA, by = "pairID") %>%
    filter(is.na(VA)) %>%
    select(-VA)
  
  ##---- HITECH exposure ---------------------------------------
  data <- data %>%
    left_join(exposure, by = c("MCRNUM" = "provider_number"))
  
  ##---- Drop very small hospitals ----------------------------
  low_beds <- data %>%
    ungroup() %>%
    distinct(HospNPI, year, BDTOT) %>%
    filter(BDTOT < 10) %>%
    distinct(HospNPI) %>%
    mutate(low_beds = 1)
  
  data <- data %>%
    left_join(low_beds, by = "HospNPI") %>%
    filter(is.na(low_beds)) %>%
    select(-low_beds)
  
  data <- data %>%
    group_by(DocNPI, year) %>%
    mutate(avg_beds = round(mean(BDTOT, na.rm = TRUE), 1)) %>%
    ungroup() %>%
    mutate(experience = year - grad_year)
  
  ##---- EHR exposure measures --------------------------------
  data <- data %>%
    group_by(DocNPI, year) %>%
    mutate(num_hospitals        = n(),
           num_hosp_EHR         = sum(any_EHR, na.rm = TRUE),
           num_hosp_anyphys_EHR = sum(phys_facing_any, na.rm = TRUE),
           num_hosp_cpoe_EHR    = sum(cpoe, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(frac_EHR         = ifelse(num_hospitals > 0, num_hosp_EHR / num_hospitals, NA),
           frac_anyphys_EHR = ifelse(num_hospitals > 0, num_hosp_anyphys_EHR / num_hospitals, NA),
           frac_cpoe_EHR    = ifelse(num_hospitals > 0, num_hosp_cpoe_EHR / num_hospitals, NA))
  
  ##---- First year of exposure, three definitions ------------
  first_year <- function(d, fracvar, newname) {
    d %>%
      ungroup() %>%
      distinct(DocNPI, year, .data[[fracvar]]) %>%
      filter(.data[[fracvar]] > 0) %>%
      group_by(DocNPI) %>%
      summarise(!!newname := min(year), .groups = "drop")
  }
  
  data <- data %>%
    ungroup() %>%
    left_join(first_year(data, "frac_EHR",         "minyr_EHR"),         by = "DocNPI") %>%
    left_join(first_year(data, "frac_anyphys_EHR", "minyr_anyphys_EHR"), by = "DocNPI") %>%
    left_join(first_year(data, "frac_cpoe_EHR",    "minyr_cpoe_EHR"),    by = "DocNPI") %>%
    mutate(across(c(minyr_EHR, minyr_anyphys_EHR, minyr_cpoe_EHR),
                  ~ ifelse(is.na(.), 0, .)))
  
  ##---- Hospital system changes ------------------------------
  hosp_sys <- data %>%
    ungroup() %>%
    distinct(HospNPI, year, SYSID) %>%
    arrange(HospNPI, year) %>%
    group_by(HospNPI) %>%
    mutate(prev_sys   = dplyr::lag(SYSID),
           prev_year  = dplyr::lag(year),
           contiguous = !is.na(prev_year) & prev_year == year - 1,
           joined_sys = contiguous &  is.na(prev_sys) & !is.na(SYSID),
           left_sys   = contiguous & !is.na(prev_sys) &  is.na(SYSID),
           switch_sys = contiguous & !is.na(prev_sys) & !is.na(SYSID) & SYSID != prev_sys,
           sys_change = joined_sys | left_sys | switch_sys) %>%
    ungroup() %>%
    select(HospNPI, year, switch_sys) %>%
    group_by(HospNPI) %>%
    summarise(change_years = list(year[switch_sys]), .groups = "drop")
  
  data <- data %>%
    left_join(hosp_sys, by = "HospNPI") %>%
    group_by(DocNPI, year) %>%
    mutate(doc_sc_years = list(sort(unique(unlist(change_years))))) %>%
    ungroup()
  
  ##---- Number of systems ------------------------------------
  count_sys <- data %>%
    ungroup() %>%
    distinct(DocNPI, year, SYSID) %>%
    count(DocNPI, year, name = "num_systems")
  
  data <- data %>% left_join(count_sys, by = c("year", "DocNPI"))
  
  if (write_files)
    saveRDS(data, paste0(created_data_path, "cleaned_pairs_", tag, ".rds"))
  
  ##---- Patient counts ---------------------------------------
  data <- data %>%
    ungroup() %>%
    group_by(DocNPI, year) %>%
    mutate(hosp_patient_count = sum(samedaycount)) %>%
    ungroup()
  
  EHR_hosp <- data %>%
    filter(any_EHR == 1) %>%
    group_by(year, DocNPI) %>%
    summarise(hosp_patient_count_EHRhosp = sum(samedaycount), .groups = "drop")
  
  noEHR_hosp <- data %>%
    filter(any_EHR == 0) %>%
    group_by(year, DocNPI) %>%
    summarise(hosp_patient_count_noEHRhosp = sum(samedaycount), .groups = "drop")
  
  data <- data %>%
    left_join(EHR_hosp,   by = c("year", "DocNPI")) %>%
    left_join(noEHR_hosp, by = c("year", "DocNPI")) %>%
    mutate(across(c(hosp_patient_count_EHRhosp, hosp_patient_count_noEHRhosp),
                  ~ ifelse(is.na(.), 0, .)))
  
  ##---- New NPI introduced -----------------------------------
  ## same == 1 in the year the physician reaches their max
  ## number of affiliations; newnpi flags the year they get there.
  data <- data %>%
    group_by(DocNPI) %>%
    mutate(max_numhosp = max(num_hospitals)) %>%
    ungroup() %>%
    mutate(same = ifelse(num_hospitals == max_numhosp, 1, 0))
  
  same_wide <- data %>%
    distinct(DocNPI, year, same) %>%
    pivot_wider(names_from = year, values_from = same, names_prefix = "same") %>%
    group_by(DocNPI) %>%
    fill(everything(), .direction = "downup") %>%
    ungroup()
  
  data <- data %>% left_join(same_wide, by = "DocNPI")
  
  yrs <- 2010:2015
  data <- data %>% mutate(newnpi = 0)
  for (y in yrs) {
    prev <- paste0("same", y - 1); cur <- paste0("same", y)
    if (all(c(prev, cur) %in% names(data))) {
      data <- data %>%
        mutate(newnpi = ifelse(year == y &
                                 .data[[prev]] == 0 & .data[[cur]] == 1,
                               1, newnpi))
    }
  }
  
  data <- data %>%
    select(-same, -starts_with("same20")) %>%
    group_by(DocNPI) %>%
    mutate(never_newnpi = ifelse(sum(newnpi, na.rm = TRUE) == 0, 1, 0)) %>%
    ungroup()
  
  ##---- Hospital-level file ----------------------------------
  hosp_data <- data %>%
    distinct(year, HospNPI, MCRNUM, MADMIN, EHLTH, cpoe, cdr, cdss, order_entry,
             phys_doc, phys_portal, SYSID, SUBS, BDTOT, FTMDTF, FTRES,
             PTMDTF, PTRES, JNTPH, PHYGP, IPAHOS, OPHOSHOS, CPHOHOS, ISMHOS,
             FTRNTF, IPAP, OPHP, CPHP, ISMP, mcare_exposure, mcaid_exposure,
             change_years, any_EHR)
  
  if (write_files)
    saveRDS(hosp_data, paste0(created_data_path, "hosp_data_", tag, ".rds"))
  
  ##---- Weighted average HITECH exposure ---------------------
  data <- data %>%
    group_by(MCRNUM, year) %>%
    mutate(total_exposure = sum(mcaid_exposure, mcare_exposure)) %>%
    ungroup() %>%
    group_by(DocNPI, year) %>%
    mutate(weightedavg_exposure       = weighted.mean(total_exposure,  samedaycount, na.rm = TRUE),
           weightedavg_mcaid_exposure = weighted.mean(mcaid_exposure,  samedaycount, na.rm = TRUE),
           weightedavg_mcare_exposure = weighted.mean(mcare_exposure,  samedaycount, na.rm = TRUE)) %>%
    ungroup()
  
  ##---- Collapse to physician-year ---------------------------
  Aggregated_Pairs <- data %>%
    distinct(year, DocNPI, grad_year, avg_beds, experience,
             num_hospitals, doc_sc_years,
             frac_EHR, minyr_EHR, frac_anyphys_EHR, frac_cpoe_EHR,
             minyr_anyphys_EHR, minyr_cpoe_EHR,
             num_systems, hosp_patient_count, hosp_patient_count_EHRhosp,
             hosp_patient_count_noEHRhosp,
             newnpi, never_newnpi, weightedavg_exposure,
             weightedavg_mcaid_exposure, weightedavg_mcare_exposure,
             phys_ever_IPA, phys_ever_OPHO, phys_ever_CPHO, phys_ever_ISM)
  
  Aggregated_Pairs <- complete(Aggregated_Pairs, DocNPI, year = 2009:2017) %>%
    group_by(DocNPI) %>%
    fill(minyr_EHR, minyr_anyphys_EHR, minyr_cpoe_EHR, doc_sc_years,
         .direction = "downup") %>%
    fill(grad_year, avg_beds, num_hospitals, .direction = "downup") %>%
    fill(num_systems, weightedavg_exposure, phys_ever_IPA, phys_ever_OPHO,
         phys_ever_CPHO, phys_ever_ISM, .direction = "downup") %>%
    ungroup() %>%
    mutate(experience = year - grad_year,
           anyEHR_exposed = ifelse(minyr_EHR > 0 & year >= minyr_EHR, 1, 0))
  
  if (write_files)
    saveRDS(Aggregated_Pairs,
            file = paste0(created_data_path, "Aggregated_Pairs_", tag, ".rds"))
  
  ##---- Checks -----------------------------------------------
  bal <- Aggregated_Pairs %>% select(year, DocNPI)
  message("  balanced panel: ", is.pbalanced(bal))
  message("  physicians: ", n_distinct(Aggregated_Pairs$DocNPI))
  message("  hospitals:  ", n_distinct(hosp_data$HospNPI))
  print(Aggregated_Pairs %>% distinct(DocNPI, minyr_EHR) %>% count(minyr_EHR))
  
  list(pairs = data, hosp = hosp_data, agg = Aggregated_Pairs, tag = tag)
}


## Run for all three samples-----------------------
Built <- purrr::map2(Data_List, TAGS, function(p, tg) {
  build_physician_data(p, tag = tg,
                       write_files = FALSE,
                       make_ehr_table = FALSE)
})
names(Built) <- TAGS

##---- Side-by-side comparison of the three samples -----------
purrr::map_dfr(Built, function(b) {
  tibble(sample     = b$tag,
         n_phys     = n_distinct(b$agg$DocNPI),
         n_hosp     = n_distinct(b$hosp$HospNPI),
         n_pairs    = nrow(b$pairs),
         share_treated = mean(b$agg$minyr_EHR > 0),
         mean_hosp_per_doc = mean(b$agg$num_hospitals, na.rm = TRUE))
})

#### DATA 5 -------------------------------------------------

# Read in MDPPAS data
for (i in 2009:2017) {
  year <- read_csv(paste0(raw_data_path,"/MDPPAS/PhysicianData_",i,".csv"),
                   guess_max = Inf)
  year <- year %>%
    dplyr::select(-name_middle, -name_first, -spec_broad, -spec_prim_2,
                  -phy_zip7, -claim_count7, -phy_zip8, -claim_count8, -phy_zip9, -claim_count9,
                  -phy_zip10, -claim_count10, -phy_zip11, -claim_count11, -phy_zip12, 
                  -claim_count12, -spec_prim_2_name, -tin1_legal_name, -tin2_legal_name,
                  -group1, -group2)
  assign(paste0("MDPPAS",i),year)
}

MDPPAS <- rbind(MDPPAS2009, MDPPAS2010, MDPPAS2011, MDPPAS2012, MDPPAS2013, MDPPAS2014, MDPPAS2015, MDPPAS2016, MDPPAS2017)

##---- MUP-PHY claim-type shares, 2013-2017 -------------------
MUP_PHY <- map_dfr(2013:2017, function(i) {
  read_csv(paste0(raw_data_path,
                  "/MUP_PHYS/Medicare_Physician_Other_Practitioners_by_Provider_and_Service_",
                  i, ".csv"), guess_max = Inf) %>%
    mutate(year = i)
})

MUP_PHY <- MUP_PHY %>%
  group_by(Rndrng_NPI, year) %>%
  mutate(total_claims = sum(Tot_Srvcs, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(str_detect(HCPCS_Cd, "^992")) %>%
  mutate(group = case_when(
    str_detect(HCPCS_Desc, "Critical care delivery critically ill or injured patient") ~ "crit_care_delivery",
    str_detect(HCPCS_Desc, "Established patient office or other outpatient")           ~ "est_patient_office",
    str_detect(HCPCS_Desc, "Hospital discharge day management")                        ~ "hosp_disch_day",
    str_detect(HCPCS_Desc, "Hospital observation")                                     ~ "hosp_obs_care",
    str_detect(HCPCS_Desc, "Initial hospital inpatient care")                          ~ "initial_hosp_inpat",
    str_detect(HCPCS_Desc, "New patient office or other outpatient visit")             ~ "new_patient_office",
    str_detect(HCPCS_Desc, "Subsequent hospital inpatient care")                       ~ "subs_hosp_inpat",
    str_detect(HCPCS_Desc, "Subsequent observation care")                              ~ "subs_obs_care",
    TRUE                                                                               ~ NA_character_))

MUP_PHY_wide <- MUP_PHY %>%
  filter(!is.na(group)) %>%
  group_by(Rndrng_NPI, group, year) %>%
  mutate(num_services = sum(Tot_Srvcs, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(year, Rndrng_NPI, group, num_services, total_claims,
           Rndrng_Prvdr_State_Abrvtn) %>%
  mutate(frac_services = num_services / total_claims) %>%
  select(-num_services, -total_claims) %>%
  pivot_wider(id_cols = c("Rndrng_NPI", "year", "Rndrng_Prvdr_State_Abrvtn"),
              names_from = "group", values_from = "frac_services") %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

rm(MUP_PHY)

INPAT_THRESH <- c(0.5, 0.7, 0.9)

names(Built) <- TAGS      # "10", "30", "60"
names(Built)                 # confirm

build_analysis_data <- function(Aggregated_Pairs, tag, inpat_thresh = 0.7, write_files = TRUE) {
  
  full_tag <- paste0(tag, "_", inpat_thresh * 100)
  message("\n===== data5 for sample: ", full_tag, " =====")
  
  ##---- Merge MDPPAS, fill time-invariant fields -------------
  Physician_Data <- Aggregated_Pairs %>%
    left_join(MDPPAS, by = c("DocNPI" = "npi", "year" = "Year")) %>%
    group_by(DocNPI) %>%
    fill(sex, birth_dt, frac_EHR, never_newnpi, .direction = "downup") %>%
    ungroup() %>%
    mutate(female = ifelse(sex == 'F', 1, 0))
  
  ##---- MDPPAS coverage flags, BEFORE any NA -> 0 recoding ---
  Physician_Data <- Physician_Data %>%
    mutate(in_mdppas = !is.na(npi_unq_benes)) %>%
    group_by(DocNPI) %>%
    mutate(n_mdppas_yrs    = sum(in_mdppas),
           first_mdppas_yr = ifelse(any(in_mdppas), min(year[in_mdppas]), NA_integer_),
           last_mdppas_yr  = ifelse(any(in_mdppas), max(year[in_mdppas]), NA_integer_),
           missing_early   = any(year %in% c(2009, 2010) & !in_mdppas),
           late_entrant    = first_mdppas_yr > 2009) %>%
    ungroup()
  
  ##---- Age ---------------------------------------------------
  Physician_Data <- Physician_Data %>%
    mutate(birth_year = as.numeric(substr(birth_dt, start = 6, stop = 9)),
           age = year - birth_year) %>%
    group_by(DocNPI) %>%
    mutate(max_age = max(age, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(age != 24)
  
  ##---- Keep hospital-based physicians -----------------------
  Physician_Data <- Physician_Data %>%
    group_by(DocNPI) %>%
    mutate(max_inpat = max(pos_inpat, na.rm = TRUE)) %>%
    filter(max_inpat > inpat_thresh) %>%
    ungroup()
  
  ##---- Claim-type shares and opt-out ------------------------
  Physician_Data <- Physician_Data %>%
    left_join(MUP_PHY_wide, by = c("DocNPI" = "Rndrng_NPI", "year")) 
  
  ##=========================================================
  ## RETIREMENT
  ##=========================================================
  Physician_Data <- Physician_Data %>%
    rowwise() %>%
    mutate(claim_count_total = ifelse(is.na(claim_count1), NA,
                                      sum(c_across(starts_with("claim_count")),
                                          na.rm = TRUE))) %>%
    ungroup()
  
  ## reverse cumulative sum: claims in all strictly future years
  Physician_Data <- Physician_Data %>%
    group_by(DocNPI) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(cc0 = ifelse(is.na(claim_count_total), 0, claim_count_total),
           future_claims = rev(cumsum(rev(cc0))) - cc0) %>%
    select(-cc0) %>%
    ungroup()
  
  minyr_retire <- Physician_Data %>%
    filter(future_claims == 0 & year < 2017) %>%
    group_by(DocNPI) %>%
    summarise(minyr_retire = min(year), .groups = "drop")
  
  Physician_Data <- Physician_Data %>%
    left_join(minyr_retire, by = "DocNPI") %>%
    mutate(minyr_retire = ifelse(!is.na(minyr_retire), minyr_retire + 1, minyr_retire),
           minyr_retire = ifelse(minyr_retire == 2017, NA, minyr_retire),
           retire       = ifelse(is.na(minyr_retire), 0,
                                 ifelse(year == minyr_retire, 1, 0)),
           ever_retire  = ifelse(is.na(minyr_retire), 0, 1),
           retired_ever = ifelse(!is.na(minyr_retire) & year >= minyr_retire, 1, 0))
  ##=========================================================
  ## OFFICE
  ##=========================================================
  Physician_Data <- Physician_Data %>%
    mutate(claim_count_total = ifelse(is.na(claim_count_total), 0, claim_count_total),
           pos_office        = ifelse(is.na(pos_office), 0, pos_office),
           work_in_office    = ifelse(pos_office > 0, 1, 0))
  
  Physician_Data <- Physician_Data %>%
    mutate(office_yearprior = case_when(
      year == minyr_EHR - 1 & work_in_office == 1 ~ 1,
      year == minyr_EHR - 1 & work_in_office == 0 ~ 0,
      TRUE                                        ~ NA_real_)) %>%
    group_by(DocNPI) %>%
    fill(office_yearprior, .direction = "downup") %>%
    ungroup() %>%
    mutate(pos_office_prior   = ifelse(office_yearprior == 1, pos_office, NA),
           pos_office_noprior = ifelse(office_yearprior == 0, pos_office, NA),
           work_in_hosp       = ifelse(pos_inpat > 0, 1, 0))
  
  Physician_Data <- Physician_Data %>%
    group_by(DocNPI) %>%
    mutate(ever_work_in_office = ifelse(sum(work_in_office, na.rm = TRUE) > 0, 1, 0)) %>%
    ungroup()
  
  ##=========================================================
  ## PRODUCTIVITY
  ##=========================================================
  Physician_Data <- Physician_Data %>%
    mutate(npi_unq_benes      = ifelse(is.na(npi_unq_benes), 0, npi_unq_benes),
           total_office       = pos_office * npi_unq_benes,
           total_office_prior   = ifelse(office_yearprior == 1, total_office, NA),
           total_office_noprior = ifelse(office_yearprior == 0, total_office, NA),
           majority_in_office = ifelse(pos_office > pos_inpat, 1, 0),
           claim_per_patient  = claim_count_total / npi_unq_benes,
           only_office        = ifelse(pos_inpat < .05, 1, 0))
  
  ##=========================================================
  ## RELATIVE YEAR INDICATORS
  ##=========================================================
  Physician_Data <- Physician_Data %>%
    mutate(rel_expandyear = ifelse(minyr_EHR > 0, year - minyr_EHR, NA))
  
  for (k in -4:4) {
    nm <- if (k < 0) paste0("rel_m", abs(k)) else paste0("rel_p", k)
    if (k == 0) nm <- "rel_0"
    Physician_Data[[nm]] <- 1 * (Physician_Data$rel_expandyear == k)
  }
  
  ##---- Pre-treatment MDPPAS coverage ------------------------
  Physician_Data <- Physician_Data %>%
    group_by(DocNPI) %>%
    mutate(missing_pretreat = any(minyr_EHR > 0 &
                                    year < minyr_EHR &
                                    year >= first_mdppas_yr &
                                    !in_mdppas)) %>%
    ungroup()
  
  ##---- Keep hospital-based at baseline ----------------------
  Physician_Data <- Physician_Data %>%
    mutate(yearbefore = ifelse(year == minyr_EHR - 1, pos_inpat, NA)) %>%
    group_by(DocNPI) %>%
    fill(yearbefore, .direction = "downup") %>%
    ungroup() %>%
    filter(yearbefore > .5 | is.na(yearbefore)) %>%
    group_by(DocNPI) %>%
    mutate(stable_inpat = all(pos_inpat > 0.7, na.rm = TRUE)) %>%
    fill(Rndrng_Prvdr_State_Abrvtn, .direction = "downup") %>%
    ungroup() 
    
  
  if (write_files)
    saveRDS(Physician_Data,
            file = paste0(created_data_path, "Physician_Data_", full_tag, ".rds"))
  
  ##---- Checks -----------------------------------------------
  message("  physicians: ", n_distinct(Physician_Data$DocNPI))
  message("  rows: ", nrow(Physician_Data))
  message("  ever retire: ",
          n_distinct(Physician_Data$DocNPI[Physician_Data$ever_retire == 1]))
  print(Physician_Data %>% distinct(DocNPI, minyr_EHR) %>% count(minyr_EHR))
  
  Physician_Data
}

grid <- tidyr::expand_grid(sd = TAGS, inpat = INPAT_THRESH)

Analysis <- pmap(grid, function(sd, inpat) {
  build_analysis_data(Built[[sd]]$agg, tag = sd, inpat_thresh = inpat)
})
names(Analysis) <- paste0("sd", grid$sd, "_inpat", grid$inpat * 100)


##=============================================================
## Specification charts, all outcomes
##   36 specifications per outcome:
##     3 shared-patient thresholds (10 / 30 / 60)
##   x 3 inpatient-share thresholds (50 / 70 / 90)
##   x 2 anticipation (0 / 1 year)
##   x 2 year windows (full / 2009-2015)
##
##   Never-treated controls are handled in a separate script.
##
## Requires: Analysis (named list from data5_robustness.R),
##           function_path
##=============================================================

#source("./Scripts/spec_chart_function.R")

SD_TAGS      <- c("10", "30", "60")
INPAT_THRESH <- c(50, 70, 90)

MAIN_SD    <- "30"          # main specification, for highlighting
MAIN_INPAT <- 70

SAVE_PDF <- TRUE            # FALSE to draw in the Plots pane

OUTCOMES <- c(retired_ever      = "Exit from Clinical Practice",
              pos_office        = "Fraction of Patients in Office",
              work_in_office    = "Works in an Office",
              npi_unq_benes     = "Number of Patients",
              claim_per_patient = "Claims per Patient")

##=============================================================
## PART 1 -- Sample preparation
##=============================================================
prep_sample <- function(d, year_spec, yname) {
  
  d <- d %>%
    ungroup() %>%
    ## drop physicians whose first exposure coincides with a
    ## hospital system change
    mutate(bundled = purrr::map2_lgl(doc_sc_years, minyr_EHR,
                                     ~ !is.null(.x) && .y > 0 &&
                                       any(abs(.x - .y) <= 0))) %>%
    group_by(DocNPI) %>%
    mutate(drop_md = any(bundled)) %>%
    ungroup() %>%
    filter(!drop_md) %>%
    filter(first_mdppas_yr == 2009) %>%   # MDPPAS coverage from panel start
    filter(minyr_EHR != 0,                # no never-treated in this script
           minyr_EHR != 2009)             # 2009 cohort has no clean pre-period
  
  ## outcome-specific restrictions, matching the main analysis
  if (yname %in% c("pos_office", "work_in_office"))
    d <- d %>% filter(ever_retire == 0)
  if (yname %in% c("npi_unq_benes", "claim_per_patient"))
    d <- d %>% filter(ever_retire == 0, stable_inpat == 1)
  
  if (year_spec == "limited") d <- d %>% filter(year <= 2015)
  
  d
}

##=============================================================
## PART 2 -- Specification grid
##=============================================================
spec_grid <- expand_grid(sd           = SD_TAGS,
                         inpat        = INPAT_THRESH,
                         anticipation = c(FALSE, TRUE),
                         year_spec    = c("full", "limited")) %>%
  mutate(spec_id  = row_number(),
         data_key = paste0("sd", sd, "_inpat", inpat))

##=============================================================
## PART 3 -- Estimate one specification
##=============================================================
run_spec <- function(sd, inpat, anticipation, year_spec, spec_id, data_key,
                     yname) {
  
  d <- prep_sample(Analysis[[data_key]], year_spec, yname)
  
  mp <- tryCatch(
    att_gt(yname         = yname,
           gname         = "minyr_EHR",
           idname        = "DocNPI",
           tname         = "year",
           xformla       = NULL,
           data          = d,
           est_method    = "dr",
           control_group = "notyettreated",
           clustervars   = "DocNPI",
           anticipation  = as.integer(anticipation),
           base_period   = "universal"),
    error = function(e) {message("   spec ", spec_id, " failed: ", e$message); NULL})
  
  if (is.null(mp))
    return(tibble(spec_id = spec_id, coef = NA_real_, se = NA_real_,
                  n_phys = NA_integer_))
  
  ag <- aggte(mp, type = "dynamic", na.rm = TRUE)
  
  tibble(spec_id = spec_id, coef = ag$overall.att,
         se = ag$overall.se, n_phys = mp$n)
}

##=============================================================
## PART 4 -- Run all specifications for one outcome, then chart
##=============================================================
labels <- list("Perc. Inpatient Threshold: " = c("50% ", "70% ", "90% "),
               "Shared Patient Threshold: "  = c("10 ", "30 ", "60 "),
               "Anticipation: 1 year ",
               "Limited Years (2009-2015) ")

make_spec_chart <- function(yname, label) {
  
  message("\n===== ", label, " =====")
  
  results <- pmap_dfr(spec_grid, run_spec, yname = yname)
  
  cdf <- spec_grid %>%
    left_join(results, by = "spec_id") %>%
    mutate(inpat_50     = as.integer(inpat == 50),
           inpat_70     = as.integer(inpat == 70),
           inpat_90     = as.integer(inpat == 90),
           sp_10        = as.integer(sd == "10"),
           sp_30        = as.integer(sd == "30"),
           sp_60        = as.integer(sd == "60"),
           anticipation = as.integer(anticipation),
           limit_years  = as.integer(year_spec == "limited"))
  
  ## drop failures before anything positional
  n_fail <- sum(is.na(cdf$coef))
  if (n_fail) message("  dropping ", n_fail, " failed specifications")
  cdf <- cdf %>% filter(!is.na(coef))
  
  ## locate the main specification by its characteristics
  main_idx <- which(cdf$sd == MAIN_SD & cdf$inpat == MAIN_INPAT &
                      cdf$anticipation == 0 & cdf$limit_years == 0)
  stopifnot(length(main_idx) == 1)
  
  chart_df <- cdf %>%
    select(coef, se, inpat_50, inpat_70, inpat_90,
           sp_10, sp_30, sp_60, anticipation, limit_years) %>%
    as.data.frame()
  
  ## sort here rather than inside schart, so `highlight` is unambiguous
  srt        <- order(chart_df$coef)
  chart_srt  <- chart_df[srt, ]
  rownames(chart_srt) <- NULL
  main_pos   <- which(srt == main_idx)
  
  message("  main spec: coef = ", sprintf("%.4f", chart_srt$coef[main_pos]),
          ", position ", main_pos, " of ", nrow(chart_srt))
  
  mu   <- mean(chart_srt$coef)
  sd_c <- sd(chart_srt$coef)
  
  if (SAVE_PDF)
    pdf(file = paste0("Objects/spec_chart_", yname, ".pdf"),
        width = 9, height = 6.5)
  
  par(oma = c(1, 0, 1, 1))
  
  schart(chart_srt, labels,
         highlight    = main_pos,
         heights      = c(1, .75),
         band.ref     = c(mu + 1.96 * sd_c, mu - 1.96 * sd_c),
         col.band.ref = "#E8E4E6",
         col.est      = c("grey70", "#16324F"),
         col.dot      = c("grey70", "grey90", "#16324F", "#16324F"),
         ylab         = "Coefficient and 95% C.I.")
  
  legend("bottomright", cex = .8, lwd = 2:2,
         col = c("#E8E4E6", "#16324F"),
         c("Average Conf. Interval", "Main Specification"), inset = .02)
  
  if (SAVE_PDF) dev.off()
  
  ## summary row for the console / a companion table
  tibble(outcome   = label,
         outcome_raw = yname,
         n_specs   = nrow(chart_srt),
         main_coef = chart_srt$coef[main_pos],
         main_se   = chart_srt$se[main_pos],
         min_coef  = min(chart_srt$coef),
         max_coef  = max(chart_srt$coef),
         share_sig = mean(abs(chart_srt$coef / chart_srt$se) > 1.96),
         share_same_sign = mean(sign(chart_srt$coef) ==
                                  sign(chart_srt$coef[main_pos])))
}

##=============================================================
## PART 5 -- Run for every outcome
##=============================================================
if (!SAVE_PDF) while (!is.null(dev.list())) dev.off()

spec_summary <- imap_dfr(OUTCOMES, ~ make_spec_chart(.y, .x))

print(spec_summary, width = Inf)




##=============================================================
## Never-treated comparison group: diagnostics and estimates
##
## Produces:
##   Table A: characteristics of never-treated vs treated
##   Table B: estimates under never-treated vs not-yet-treated
##
## Requires: Analysis (from data5_robustness.R), varlist
##=============================================================

library(dplyr)
library(tidyr)
library(purrr)
library(did)
library(kableExtra)

DATA_KEY <- "sd30_inpat70"     # main sample
varlist <- list("retired_ever", "pos_office", "work_in_office", "npi_unq_benes", "claim_per_patient", "claims_inpat", "claims_office", "claims_opd")
vl <- unlist(varlist)

LABS <- c(retired_ever      = "Exit from Clinical Practice",
          pos_office        = "Fraction of Patients in Office",
          work_in_office    = "Works in an Office",
          npi_unq_benes     = "Number of Patients",
          claim_per_patient = "Claims per Patient")

##=============================================================
## PART 1 -- Build the truncated panel with never-treated units
##=============================================================
base <- Analysis[[DATA_KEY]] %>%
  ungroup() %>%
  mutate(bundled = map2_lgl(doc_sc_years, minyr_EHR,
                            ~ !is.null(.x) && .y > 0 && any(abs(.x - .y) <= 0))) %>%
  group_by(DocNPI) %>%
  mutate(drop_md = any(bundled)) %>%
  ungroup() %>%
  filter(!drop_md, first_mdppas_yr == 2009, minyr_EHR != 2009)

## truncate at 2015 and recode later adopters as never-treated
nt_panel <- base %>%
  filter(year <= 2015) %>%
  mutate(minyr_EHR = ifelse(minyr_EHR > 2015, 0, minyr_EHR),
         group = ifelse(minyr_EHR == 0, "Never treated", "Treated"))

## comparable not-yet-treated panel: same truncation, no never-treated
nyt_panel <- nt_panel %>% filter(minyr_EHR != 0)

##=============================================================
## PART 2 -- Who are the never-treated?
##=============================================================
char_vars <- c("num_hospitals", "num_systems", "avg_beds",
               "max_age", "female", "experience",
               "npi_unq_benes", "pos_inpat", "pos_office")
char_vars <- intersect(char_vars, names(nt_panel))

## one row per physician, measured in the baseline year
char_tab <- nt_panel %>%
  filter(year == 2009) %>%
  select(DocNPI, group, all_of(char_vars)) %>%
  pivot_longer(all_of(char_vars), names_to = "var", values_to = "val") %>%
  group_by(var, group) %>%
  summarise(m = mean(val, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = group, values_from = m)

## difference in means with a t-test
diff_tab <- map_dfr(char_vars, function(v) {
  d <- nt_panel %>% filter(year == 2009)
  tt <- t.test(d[[v]] ~ d$group)
  tibble(var = v,
         diff = unname(diff(rev(tt$estimate))),
         p    = tt$p.value)
})

char_tab <- char_tab %>%
  left_join(diff_tab, by = "var") %>%
  left_join(nt_panel %>% filter(year == 2009) %>%
              count(group) %>%
              pivot_wider(names_from = group, values_from = n,
                          names_prefix = "n_"),
            by = character())

print(char_tab, width = Inf)

n_never   <- n_distinct(nt_panel$DocNPI[nt_panel$minyr_EHR == 0])
n_treated <- n_distinct(nt_panel$DocNPI[nt_panel$minyr_EHR > 0])
cat("\nNever treated: ", n_never, " | Treated: ", n_treated, "\n", sep = "")

##=============================================================
## PART 3 -- Estimates under both comparison groups
##=============================================================
restrict <- function(d, yname) {
  if (yname %in% c("pos_office", "work_in_office"))
    d <- filter(d, ever_retire == 0)
  if (yname %in% c("npi_unq_benes", "claim_per_patient"))
    d <- filter(d, ever_retire == 0, stable_inpat == 1)
  d
}

fit_one <- function(yname, panel, ctrl) {
  d <- restrict(panel, yname)
  mp <- tryCatch(
    att_gt(yname = yname, gname = "minyr_EHR", idname = "DocNPI",
           tname = "year", xformla = NULL, data = d,
           est_method = "dr", control_group = ctrl,
           clustervars = "DocNPI", anticipation = 0,
           base_period = "universal"),
    error = function(e) {message("  ", yname, " / ", ctrl, ": ", e$message); NULL})
  if (is.null(mp)) return(tibble(att = NA_real_, se = NA_real_, n = NA_integer_))
  ag <- aggte(mp, type = "dynamic", na.rm = TRUE)
  tibble(att = ag$overall.att, se = ag$overall.se, n = mp$n)
}

est_tab <- map_dfr(vl, function(y) {
  message("estimating: ", y)
  nyt <- fit_one(y, nyt_panel, "notyettreated")
  nt  <- fit_one(y, nt_panel,  "nevertreated")
  tibble(outcome_raw = y,
         Outcome     = unname(LABS[y]),
         att_nyt = nyt$att, se_nyt = nyt$se, n_nyt = nyt$n,
         att_nt  = nt$att,  se_nt  = nt$se,  n_nt  = nt$n)
})

print(est_tab, width = Inf)

##=============================================================
## PART 4 -- LaTeX tables
##=============================================================
fmt_num <- function(x) ifelse(is.na(x), "---",
                              ifelse(abs(x) >= 100, formatC(x, format = "d", big.mark = ","),
                                     ifelse(abs(x) >= 1,   sprintf("%.2f", x),
                                            sprintf("%.3f", x))))
fmt_p <- function(p) ifelse(is.na(p), "---",
                            ifelse(p < 0.01, "$<$0.01", sprintf("%.2f", p)))

VAR_LABS <- c(num_hospitals = "Hospitals worked with",
              num_systems   = "Systems worked with",
              avg_beds      = "Average hospital beds",
              max_age       = "Age",
              female        = "Female",
              experience    = "Years since graduation",
              npi_unq_benes = "Medicare patients",
              pos_inpat     = "Share of claims, inpatient",
              pos_office    = "Share of claims, office")

tabA <- char_tab %>%
  mutate(Variable = recode(var, !!!VAR_LABS),
         `Never treated` = fmt_num(`Never treated`),
         Treated         = fmt_num(Treated),
         Difference      = fmt_num(diff),
         `$p$-value`     = fmt_p(p)) %>%
  select(Variable, Treated, `Never treated`, Difference, `$p$-value`) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE,
      align = c("l", rep("c", 4)),
      caption = "Characteristics of treated and never-treated hospitalists",
      label = "nt_chars") %>%
  kable_styling(latex_options = "hold_position", full_width = FALSE) %>%
  kableExtra::footnote(general = paste(
    "Means measured in 2009, the first year of the panel.",
    "Never-treated hospitalists are those with no EHR-adopting hospital",
    "affiliation through 2015.",
    "$p$-values are from two-sided $t$-tests of equality of means."),
    threeparttable = TRUE, escape = FALSE)

tabB <- est_tab %>%
  transmute(Outcome,
            `Not-yet-treated` = sprintf("%s (%s)", fmt_num(att_nyt), fmt_num(se_nyt)),
            `N (NYT)`         = formatC(n_nyt, format = "d", big.mark = ","),
            `Never-treated`   = sprintf("%s (%s)", fmt_num(att_nt), fmt_num(se_nt)),
            `N (NT)`          = formatC(n_nt, format = "d", big.mark = ",")) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE,
      align = c("l", rep("c", 4)),
      caption = "Estimates using not-yet-treated and never-treated comparison groups",
      label = "nt_estimates") %>%
  add_header_above(c(" " = 1, "Comparison group" = 4)) %>%
  kable_styling(latex_options = "hold_position", full_width = FALSE) %>%
  kableExtra::footnote(general = paste(
    "Both columns use a panel truncated at 2015.",
    "The not-yet-treated column excludes hospitalists never exposed within",
    "that window; the never-treated column retains them and uses them as the",
    "comparison group. Standard errors in parentheses, clustered at the",
    "physician level."),
    threeparttable = TRUE, escape = FALSE)

writeLines(as.character(tabA), file.path("Table Code", "nt_chars.tex"))
writeLines(as.character(tabB), file.path("Table Code", "nt_estimates.tex"))
cat("\nWrote nt_chars.tex and nt_estimates.tex\n")
