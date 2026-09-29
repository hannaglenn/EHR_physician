library(tidyverse)
library(readr)
library(plm)
library(kableExtra)
library(readxl)


# --------------------    Build Variables Onto Main Hospital-Physician Pair Data --------------------------
#                         Hanna Glenn, Emory University
#                         9/3/2021

# ORDER::::: 4

# This script develops the main dataset to be used in the paper.
# This builds on the physician-hospital pairs created in "data3_pairs.R"
# First, I bring in (raw) Physician Compare for information about medical school graduation and gender. 
# For EHR information, I use the (raw) main and supplement AHA surveys and HIMSS data.
# The final dataset is called "Data.rds"

# Read in physician hospital pairs created in "phys_hosp_pairs.r"
data <- read_rds(paste0(created_data_path, "phys_hosp_pairs.rds"))
# This data does not have any repeats of year, HospNPI, DocNPI
# 1.3 mill obs, 5 variables

data <- data %>%
  dplyr::rename(pairID=ID) 

# Create Experience Variable ------------------------------------------------------------
# Read in Physician Compare
PhysCompare <- read_csv(paste0(raw_data_path,"DAC_NationalDownloadableFile.csv"))

PhysCompare <- PhysCompare %>%
  dplyr::rename(gender=gndr,grad_year=`Grd_yr`) %>%
  select(NPI,grad_year) 

# Merge, then drop any physicians that graduated after 2005
data <- data %>%
  left_join(PhysCompare, by=c("DocNPI"="NPI")) %>%
  distinct() %>%
  filter(grad_year<2005 | is.na(grad_year))

#### Create EHR Variables ---------------------------------------------------------------------- ####
#Read in AHA-NPI crosswalk data
AHANPI_cw <- read_rds(paste0(created_data_path,"AHANPI_cw.rds"))

# Merge crosswalk to main data
data <- data %>%
  mutate(HospNPI=as.double(HospNPI)) %>%
  left_join(AHANPI_cw,by=c("HospNPI"="NPI"))

# See how many unique hospitals in the dataset have an AHAID
num_hosp <- data %>% ungroup() %>%
  filter(!is.na(AHAID)) %>%
  distinct(HospNPI)
# There are 4306 unique hospitals in the data that are not missing AHAID (good)

# Only keep pairs with AHA hospitals since that is where EHR information comes from
data <- data %>%
  filter(!is.na(AHAID))


# Read in main AHA survey data
AHAmainsurvey <- read_csv(paste0(raw_data_path,"AHAdata_20052023.csv"))

AHAmainsurvey <- AHAmainsurvey %>%
  select(ID, YEAR, MCRNUM, MNAME, MADMIN, SYSID, SUBS, EHLTH, 
         BDTOT, FTMDTF, FTRES, PTMDTF, PTRES, JNTPH, PHYGP,
         IPAHOS, OPHOSHOS, CPHOHOS, ISMHOS, IPAP, OPHP, CPHP, ISMP, FTRNTF) %>%
  filter(!is.na(MCRNUM)) %>%
  mutate(ID=as.character(ID)) %>%
  filter(YEAR<=2015 & YEAR>=2009)

data <- data %>% ungroup() %>%
  mutate(AHAID=as.character(AHAID)) %>%
  left_join(AHAmainsurvey,by=c("AHAID"="ID","year"="YEAR"),na_matches="never")

data <- data %>%
  filter(!is.na(MCRNUM))

## BLOCK A: hospital-year integration-form purity
data <- data %>%
  mutate(across(c(IPAHOS, OPHOSHOS, CPHOHOS, ISMHOS), ~ replace_na(., 0))) %>%
  mutate(n_integration_forms = IPAHOS + OPHOSHOS + CPHOHOS + ISMHOS,
         hosp_single_form = case_when(
           n_integration_forms == 1 & IPAHOS   == 1 ~ "IPA only",
           n_integration_forms == 1 & OPHOSHOS == 1 ~ "OPHO only",
           n_integration_forms == 1 & CPHOHOS  == 1 ~ "CPHO only",
           n_integration_forms == 1 & ISMHOS   == 1 ~ "ISM only",
           n_integration_forms == 0                  ~ "None",
           TRUE                                       ~ "Multiple forms"
         ))

# create variables for type of hosp
data <- data %>%
  group_by(MCRNUM) %>%
  mutate(ever_IPA = ifelse(IPAHOS==1,1,NA),
         ever_OPHO = ifelse(OPHOSHOS==1,1,NA),
         ever_CPHO = ifelse(CPHOHOS==1,1,NA),
         ever_ISM = ifelse(ISMHOS==1,1,NA)) %>%
  fill(ever_IPA, ever_OPHO, ever_CPHO, ever_ISM, .direction="downup") %>%
  ungroup()

data <- data %>%
  group_by(DocNPI) %>%
  mutate(phys_ever_IPA = ifelse(ever_IPA==1,1,NA),
         phys_ever_OPHO = ifelse(ever_OPHO==1,1,NA),
         phys_ever_CPHO = ifelse(ever_CPHO==1,1,NA),
         phys_ever_ISM = ifelse(ever_ISM==1,1,NA)) %>%
  fill(phys_ever_IPA, phys_ever_OPHO, phys_ever_CPHO, phys_ever_ISM, .direction="downup") %>%
  mutate(phys_ever_IPA = ifelse(is.na(phys_ever_IPA),0,phys_ever_IPA),
         phys_ever_OPHO = ifelse(is.na(phys_ever_OPHO),0,phys_ever_OPHO),
         phys_ever_CPHO = ifelse(is.na(phys_ever_CPHO),0,phys_ever_CPHO),
         phys_ever_ISM = ifelse(is.na(phys_ever_ISM),0,phys_ever_ISM)) %>%
  ungroup()

# HIMSS
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
})

HAEntity <- HAEntity %>%
  filter(!is.na(MedicareNumber))

AcuteInfo <- read_years(years, function(t) {
  read_xlsx(file.path(raw_data_path, "HIMSS", paste0("AcuteInfo", t, ".xlsx"))) %>%
    mutate(year = t) %>%
    select(HAEntityId, year,
           StructuredPhysDocPerc,
           CPOEHospitalsPerc, CPOEPhysicianPerc, CPOEResidentsPerc,
           CPOEAffiliatedPhPerc, CPOEOtherFTPhPerc,
           any_of(c("PhysDocUsePerc", "CPOEMandated",
                    "CPOEInpatient", "CPOEEmergencyDept")))
})

HAEntityApplication <- read_years(years, function(t) {
  read_csv(file.path(raw_data_path, "HIMSS",
                     paste0("HAEntityApplication", t, ".csv")),
           show_col_types = FALSE) %>%
    mutate(year = t) %>%
    select(HAEntityId, year, Application, Category, Status)
}) %>%
  filter(Category=="Electronic Medical Record")

app <- HAEntityApplication %>%
  left_join(HAEntity %>%
              select(HAEntityId, year, UniqueId, SurveyId,
                     HAEntityType, MedicareNumber, Name, State),
            by = c("HAEntityId", "year"))

app <- app %>%
  mutate(Application = recode(Application,
                              "Clinical Data Repository/EMR" = "Clinical Data Repository"))  # 2014+

status_levels <- c("Not Reported", "Not Automated", "Not Yet Contracted",
                   "Contracted/Not Yet Installed", "Installation in Process",
                   "To be Replaced", "Live and Operational")

app <- app %>%
  mutate(Status = trimws(Status),
         Status = recode(Status,
                         "To Be Replaced"            = "To be Replaced",
                         "Installation In Process"   = "Installation in Process",
                         "NotAutomatedNot Automated" = "Not Automated"))

# factor() silently NAs anything unlisted -- stop instead. All seven levels
# above were observed across 2009-2015, so this should not fire.
unmapped <- setdiff(unique(app$Status), status_levels)
if (length(unmapped)) stop("Unmapped Status: ", paste(unmapped, collapse = " | "))

app <- app %>% mutate(Status = factor(Status, levels = status_levels,
                                      ordered = TRUE))

collapsed <- app %>%
  group_by(UniqueId, year, Application) %>%
  summarise(HAEntityId     = first(HAEntityId),
            SurveyId       = first(SurveyId),
            HAEntityType   = first(HAEntityType),
            MedicareNumber = first(MedicareNumber),
            Status         = max(Status),
            n_products     = n(),
            .groups = "drop")

hosp <- collapsed %>%
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

hosp <- hosp %>% mutate(app = unname(app_short[Application]))
stopifnot(!any(is.na(hosp$app)))     # catches any new or renamed application

ccn_app <- hosp %>%
  group_by(MedicareNumber, year, app) %>%
  summarise(live = any(live), n_entities = n_distinct(UniqueId),
            .groups = "drop")

wide <- ccn_app %>%
  select(MedicareNumber, year, app, live) %>%
  pivot_wider(id_cols = c(MedicareNumber, year),
              names_from = app, values_from = live)

phys_apps      <- c("cpoe", "phys_doc", "phys_portal")
phys_apps_cdss <- c(phys_apps, "cdss")
core_apps      <- c("cdr", "cdss", "cpoe", "order_entry",
                    "phys_doc", "phys_portal")

present <- function(v) intersect(v, names(wide))

analysis <- wide %>%
  mutate(
    n_phys_facing = rowSums(across(all_of(present(phys_apps))), na.rm = TRUE),
    n_any         = rowSums(across(all_of(present(core_apps))), na.rm = TRUE),
    phys_facing_any = n_phys_facing > 0,
    phys_facing_all = n_phys_facing == length(present(phys_apps)),
    any_app         = n_any > 0,
    phys_facing_any_cdss =
      rowSums(across(all_of(present(phys_apps_cdss))), na.rm = TRUE) > 0)

analysis <- analysis %>% relocate(MedicareNumber, year)
stopifnot(!any(duplicated(analysis[, c("MedicareNumber", "year")])))

wide %>% group_by(year) %>%
  summarise(across(all_of(present(core_apps)), ~ mean(is.na(.x))))

# Adoption should climb steadily 2009-2015. Anything non-monotonic points at
# a join or collapse problem rather than a finding.
analysis %>% group_by(year) %>%
  summarise(n_hospitals = n(),
            across(all_of(present(core_apps)), ~ mean(.x, na.rm = TRUE)),
            phys_facing_any = mean(phys_facing_any),
            any_app         = mean(any_app))

# merge HIMSS to data set
data <- data %>%
  left_join(analysis, by=c("year","MCRNUM"="MedicareNumber"), na_matches="never")

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


# Characterize EHR info from the two sources
class_tab <- data %>%
  distinct(pick(year, HospNPI, EHLTH, all_of(core_apps), any_app)) %>%
  ungroup() %>%
  mutate(
    aha_bin   = case_when(is.na(EHLTH) ~ NA_integer_,
                          EHLTH == 2   ~ 1L,
                          TRUE           ~ 0L),
    himss_bin = case_when(is.na(any_app) ~ NA_integer_,
                          any_app==1        ~ 1L,
                          TRUE           ~ 0L),
    category = case_when(
      is.na(aha_bin) & is.na(himss_bin)        ~ "3. both missing",
      is.na(aha_bin) | is.na(himss_bin)        ~ "2. one missing",
      aha_bin == himss_bin                     ~ "1. agree",
      aha_bin == 1 & himss_bin == 0            ~ "4. AHA yes, HIMSS no",
      aha_bin == 0 & himss_bin == 1            ~ "5. AHA no, HIMSS yes"
    )
  ) 

## counts
class_tab %>%
  count(category) %>%
  mutate(share = n / sum(n)) %>%
  arrange(category)

## which source is missing, for category 2
class_tab %>%
  filter(category == "2. one missing") %>%
  count(aha_missing = is.na(aha_bin), himss_missing = is.na(himss_bin))

## by year — coverage usually changes over time
class_tab %>%
  count(year, category) %>%
  tidyr::pivot_wider(names_from = category, values_from = n, values_fill = 0)

apps <- present(core_apps)   # cdr, cdss, cpoe, phys_doc, phys_portal

conflict <- class_tab %>%
  filter(aha_bin == 0, himss_bin == 1)

## how often each application appears
conflict %>%
  summarise(n_hospyr = n(),
            across(all_of(apps), ~ sum(. == 1, na.rm = TRUE))) %>%
  tidyr::pivot_longer(-n_hospyr, names_to = "app", values_to = "n") %>%
  mutate(share = n / n_hospyr) %>%
  arrange(-n)

## how many applications at once
conflict %>%
  mutate(n_apps = rowSums(across(all_of(apps)), na.rm = TRUE)) %>%
  count(n_apps)

## the exact combinations
conflict %>%
  mutate(combo = apply(across(all_of(apps)), 1,
                       function(r) paste(apps[which(r == 1)], collapse = "+"))) %>%
  count(combo, sort = TRUE) %>%
  head(15)

# Define EHR
data <- data %>%
  mutate(any_EHR = case_when(
    EHLTH == 2                      ~ 1L,   # full — decisive
    phys_facing_any %in% TRUE         ~ 1L,   # HIMSS shows physician-facing apps
    EHLTH %in% c(0, 1)              ~ 0L,   # neither source positive
    TRUE                              ~ NA_integer_
  ))

##=============================================================
## ONE COMPACT TABLE: constructing the EHR indicator
##   Panel A -- every AHA x HIMSS combination and its assigned value
##   Panel B -- which HIMSS applications count toward the definition
## Descriptive column headers; no footnote.
##=============================================================

phys_apps <- c("cpoe", "phys_doc", "phys_portal")
back_end  <- c("cdr", "order_entry", "cdss")

hosp <- data %>%
  distinct(pick(year, HospNPI, EHLTH, all_of(core_apps), any_app, phys_facing_any)) 
  

nf <- function(x) formatC(x, format = "d", big.mark = ",")

##---- Panel A: every combination, with its assigned value ----
A <- hosp %>%
  mutate(
    aha = factor(case_when(is.na(EHLTH) ~ "Not reported",
                           EHLTH == 0   ~ "No EHR",
                           EHLTH == 1   ~ "Partial",
                           EHLTH == 2   ~ "Full"),
                 levels = c("Full", "Partial", "No EHR", "Not reported")),
    him = factor(case_when(is.na(phys_facing_any) ~ "Not covered",
                           phys_facing_any        ~ "Physician-facing app",
                           TRUE                   ~ "No physician-facing app"),
                 levels = c("Physician-facing app", "No physician-facing app",
                            "Not covered"))
  ) %>%
  count(aha, him, .drop = FALSE) %>%
  arrange(aha, him) %>%
  mutate(
    assigned = case_when(
      aha == "Full"                   ~ "$1$",
      him == "Physician-facing app"   ~ "$1$",
      aha %in% c("Partial", "No EHR") ~ "$0$",
      TRUE                            ~ "Dropped"),
    label = paste0("\\quad ", as.character(aha), " $\\times$ ", as.character(him)),
    share = sprintf("%.1f\\%%", 100 * n / sum(n)),
    n     = nf(n)
  ) %>%
  select(label, n, share, assigned)

##---- Panel B: which applications count ----------------------
B <- hosp %>%
  summarise(across(all_of(c(phys_apps, back_end)),
                   list(n = ~ sum(. == 1, na.rm = TRUE),
                        s = ~ mean(. == 1, na.rm = TRUE)))) %>%
  pivot_longer(everything(),
               names_to = c("app", ".value"), names_sep = "_(?=[ns]$)") %>%
  mutate(
    label = recode(app,
                   cpoe        = "\\quad Computerized physician order entry",
                   phys_doc    = "\\quad Physician documentation",
                   phys_portal = "\\quad Physician portal",
                   cdr         = "\\quad Clinical data repository",
                   order_entry = "\\quad Order entry (non-physician)",
                   cdss        = "\\quad Clinical decision support"),
    assigned = ifelse(app %in% phys_apps, "Counted", "Not counted"),
    share = sprintf("%.1f\\%%", 100 * s),
    n     = nf(n)
  ) %>%
  arrange(factor(app, levels = c(phys_apps, back_end))) %>%
  select(label, n, share, assigned)

##---- Assemble -----------------------------------------------
tab <- bind_rows(A, B)

out <- kbl(tab, format = "latex", booktabs = TRUE, escape = FALSE,
           col.names = c("", "Hospital-years", "Share", "EHR indicator"),
           align = c("l", "r", "r", "c"),
           caption = "EHR Source Comparison",
           label = "ehr_construct") %>%
  pack_rows("Panel A: AHA reported status $\\times$ HIMSS applications",
            1, nrow(A), escape = FALSE) %>%
  pack_rows("Panel B: HIMSS applications used to define physician-facing use",
            nrow(A) + 1, nrow(tab), escape = FALSE) %>%
  kable_styling(latex_options = "hold_position", font_size = 9,
                full_width = FALSE)

cat(out)

writeLines(as.character(out), file.path("./Table Code", "ehr_construct.tex"))
cat("\nWrote Table Code/ehr_construct.tex\n")


# Get rid of unneeded variables 
data <- data %>% ungroup() %>%
  select(-firstyear_0, -firstyear_1, -firstyear_2, -lastyear_0, -lastyear_1, -lastyear_2)

# Get rid of physicians who only work on VA hospitals
VA <- data %>%
  mutate(VA=ifelse(str_detect(MNAME,"Veteran|\\sVA\\s|\\sVHA\\s"),1,0)) %>%
  filter(VA==1) %>%
  distinct(pairID,VA)

data <- data %>%
  left_join(VA,by="pairID") %>%
  filter(is.na(VA)) %>%
  select(-VA)

# read in HCRIS data to create average exposure of hospitals to HITECH subsidies
hcris <- read_csv(paste0(raw_data_path, "/final_hcris_data.csv")) %>%
  select(-fy_start, -fy_end, -date_processed, -date_created, -source)

exposure <- hcris %>%
  filter(year>=2006 & year<=2008) %>%
  distinct(year, provider_number, tot_operating_exp, tot_discharges, mcare_beddays, mcaid_beddays, total_beddays)

exposure <- exposure %>%
  group_by(provider_number) %>%
  select(-year) %>%
  summarise_all(list(mean)) %>%
  ungroup() %>%
  mutate(discharges_fun = ifelse(tot_discharges<1150, 2000000,
                                 ifelse(tot_discharges>23000,4370200, 1770000+(200*tot_discharges)))) %>%
  mutate(mcaid_exposure = discharges_fun * (mcaid_beddays/total_beddays)) %>%
  mutate(mcare_exposure = discharges_fun * (mcare_beddays/total_beddays)) %>%
  distinct(provider_number, mcaid_exposure, mcare_exposure)

observe <- exposure %>%
  mutate(count=1) %>%
  group_by(provider_number) %>%
  mutate(sum=sum(count)) %>%
  filter(sum>1)

data <- data %>%
  left_join(exposure, by=c("MCRNUM"="provider_number"))


# Create/Clean Variables (Reading in data is complete) --------------------------------------------

### Descriptive Variables to Drop Hospitals ------------------------------------------------------
# Descriptive Variable: Average size of hospital worked with
# Drop hospitals with really low bed count
low_beds <- data %>% ungroup() %>%
  distinct(HospNPI, year, BDTOT) %>%
  filter(BDTOT<10) %>%
  distinct(HospNPI) %>%
  mutate(low_beds=1)
# 55 hospitals

data <- data %>%
  left_join(low_beds, by="HospNPI") %>%
  filter(is.na(low_beds)) %>%
  select(-low_beds)
# 672k

data <- data %>%
  group_by(DocNPI,year) %>%
  mutate(avg_beds=mean(BDTOT,na.rm=T)) %>%
  mutate(avg_beds=round(avg_beds,1)) %>%
  ungroup()

# Descriptive Variable: Years of Experience
data <- data %>% ungroup() %>%
  mutate(experience=year-grad_year)

### EHR VARIABLES --------------------------------------------------------------------------------

# Create measure of number of hospitals a physician works with that use an EHR
# Create number of hospitals variables
data <- data %>%
  group_by(DocNPI,year) %>%
  mutate(count=1) %>%
  mutate(num_hospitals=sum(count)) %>%
  select(-count)

# Create number hospitals with general EHR
data <- data %>% ungroup() %>%
  group_by(DocNPI,year) %>%
  mutate(num_hosp_EHR=sum(any_EHR,na.rm=T)) %>%
  ungroup()

# Create number of hospitals with physician-facing EHR
data <- data %>%
  group_by(DocNPI, year) %>%
  mutate(num_hosp_anyphys_EHR = sum(phys_facing_any, na.rm=T),
         num_hosp_cpoe_EHR = sum(cpoe, na.rm=T)) %>%
  ungroup()

# Variables: fraction of hospitals with EHR
data <- data %>% ungroup() %>%
  mutate(frac_EHR = ifelse(num_hospitals>0, num_hosp_EHR/num_hospitals, NA),
        frac_anyphys_EHR = ifelse(num_hospitals>0,num_hosp_anyphys_EHR/num_hospitals,NA),
         frac_cpoe_EHR = ifelse(num_hospitals>0, num_hosp_cpoe_EHR/num_hospitals, NA))


# Treatment Variable: Indicator for Exposed to EHR in any hospital
# Create variable for first year a doc was exposed to EHR
minyr_EHR <- data %>% ungroup() %>%
  distinct(DocNPI, year, frac_EHR) %>%
  filter(frac_EHR>0) %>%
  group_by(DocNPI) %>%
  mutate(minyr_EHR=min(year)) %>%
  ungroup() %>%
  distinct(DocNPI,minyr_EHR)

minyr_anyphys_EHR <- data %>% ungroup() %>%
  distinct(DocNPI, year, frac_anyphys_EHR) %>%
  filter(frac_anyphys_EHR>0) %>%
  group_by(DocNPI) %>%
  mutate(minyr_anyphys_EHR=min(year)) %>%
  ungroup() %>%
  distinct(DocNPI,minyr_anyphys_EHR)

minyr_cpoe_EHR <- data %>% ungroup() %>%
  distinct(DocNPI, year, frac_cpoe_EHR) %>%
  filter(frac_cpoe_EHR>0) %>%
  group_by(DocNPI) %>%
  mutate(minyr_cpoe_EHR=min(year)) %>%
  ungroup() %>%
  distinct(DocNPI,minyr_cpoe_EHR)

data <- data %>% ungroup() %>%
  left_join(minyr_EHR, by="DocNPI") %>%
  left_join(minyr_anyphys_EHR, by="DocNPI") %>%
  left_join(minyr_cpoe_EHR, by="DocNPI") %>%
  mutate(minyr_EHR=ifelse(is.na(minyr_EHR),0,minyr_EHR)) %>%
  mutate(minyr_anyphys_EHR=ifelse(is.na(minyr_anyphys_EHR),0,minyr_anyphys_EHR)) %>%
  mutate(minyr_cpoe_EHR=ifelse(is.na(minyr_cpoe_EHR),0,minyr_cpoe_EHR))

phys_baseline_hosp <- data %>%
  ungroup() %>%
  distinct(DocNPI, HospNPI, year, minyr_EHR, hosp_single_form) %>%
  filter(minyr_EHR > 0, year == minyr_EHR - 1) %>%
  distinct(DocNPI, HospNPI, minyr_EHR, hosp_single_form)

phys_form_class <- phys_baseline_hosp %>%
  group_by(DocNPI) %>%
  summarise(
    n_hosp_baseline = n_distinct(HospNPI),
    all_same_form   = n_distinct(hosp_single_form) == 1,
    form            = first(hosp_single_form),
    .groups = "drop"
  ) %>%
  mutate(phys_integration_group = case_when(
    all_same_form & form == "IPA only"  ~ "IPA only",
    all_same_form & form == "OPHO only" ~ "OPHO only",
    all_same_form & form == "CPHO only" ~ "CPHO only",
    all_same_form & form == "ISM only"  ~ "ISM only",
    TRUE                                 ~ "Mixed/multiple"
  )) %>%
  select(DocNPI, phys_integration_group)

## merge onto data so it survives into cleaned_pairs.rds
data <- data %>%
  left_join(phys_form_class, by = "DocNPI")

## quick check before moving on
data %>% distinct(DocNPI, phys_integration_group) %>% count(phys_integration_group, sort = TRUE)

# Now create variable for years the hospital was exposed to a system change
hosp_sys <- data %>%
  ungroup() %>%
  distinct(HospNPI, year, SYSID) %>%
  arrange(HospNPI, year) %>%
  group_by(HospNPI) %>%
  mutate(
    prev_sys   = dplyr::lag(SYSID),
    prev_year  = dplyr::lag(year),
    contiguous = !is.na(prev_year) & prev_year == year - 1,
    joined_sys = contiguous &  is.na(prev_sys) & !is.na(SYSID),
    left_sys   = contiguous & !is.na(prev_sys) &  is.na(SYSID),
    switch_sys = contiguous & !is.na(prev_sys) & !is.na(SYSID) & SYSID != prev_sys,
    sys_change = joined_sys | left_sys | switch_sys
  ) %>%
  ungroup() %>%
  select(HospNPI, year, switch_sys) %>%
  group_by(HospNPI) %>%
  summarise(
    change_years = list(year[switch_sys]),
    .groups = "drop"
  )

data <- data %>%
  left_join(hosp_sys, by="HospNPI")

data <- data %>%
  group_by(DocNPI, year) %>%
  mutate(doc_sc_years = list(sort(unique(unlist(change_years))))) %>%
  ungroup()

# More Descriptive Variables --------------------------------------------------------------
# Descriptive Variable: Number of systems worked with
count_sys <- data %>%
  ungroup() %>%
  distinct(DocNPI,year,SYSID) %>%
  count(DocNPI,year,name="num_systems")

data <- data %>%
  left_join(count_sys,by=c("year","DocNPI"))

# save a physician-hospital data set
saveRDS(data, "./CreatedData/cleaned_pairs.rds")

## OUTCOME VARIABLES ----------------------------------------------------------- # 

# Create physician level patient count with hospitals
data <- data %>%
  ungroup() %>%
  group_by(DocNPI,year) %>%
  mutate(hosp_patient_count=sum(samedaycount)) %>%
  ungroup()

# Create physician level patient count with EHR hospitals and merge back to data
EHR_hosp <- data %>%
  filter(any_EHR==1) %>%
  group_by(year, DocNPI) %>%
  mutate(hosp_patient_count_EHRhosp=sum(samedaycount)) %>%
  ungroup() %>%
  distinct(DocNPI,year,hosp_patient_count_EHRhosp)

data <- data %>%
  left_join(EHR_hosp, by=c("year", "DocNPI")) %>%
  mutate(hosp_patient_count_EHRhosp=ifelse(is.na(hosp_patient_count_EHRhosp),0,hosp_patient_count_EHRhosp))

# Create physician level patient count with non-EHR hospitals and merge back to data
noEHR_hosp <- data %>%
  filter(any_EHR==0) %>%
  group_by(year, DocNPI) %>%
  mutate(hosp_patient_count_noEHRhosp=sum(samedaycount)) %>%
  ungroup() %>%
  distinct(DocNPI,year,hosp_patient_count_noEHRhosp)

data <- data %>%
  left_join(noEHR_hosp, by=c("year", "DocNPI")) %>%
  mutate(hosp_patient_count_noEHRhosp=ifelse(is.na(hosp_patient_count_noEHRhosp),0,hosp_patient_count_noEHRhosp))


# Create variable: new NPI introduced
data <- data %>%
  group_by(DocNPI) %>%
  mutate(max_numhosp = max(num_hospitals)) %>%
  ungroup()

data <- data %>%
  mutate(same=ifelse(num_hospitals==max_numhosp,1,0))

data <- data %>%
  mutate(same2009=ifelse(year==2009 & same==1, 1,NA),
         same2009=ifelse(year==2009 & same==0, 0,same2009),
         same2010=ifelse(year==2010 & same==1, 1,NA),
         same2010=ifelse(year==2010 & same==0, 0,same2010),
         same2011=ifelse(year==2011 & same==1, 1,NA),
         same2011=ifelse(year==2011 & same==0, 0,same2011),
         same2012=ifelse(year==2012 & same==1, 1,NA),
         same2012=ifelse(year==2012 & same==0, 0,same2012),
         same2013=ifelse(year==2013 & same==1, 1,NA),
         same2013=ifelse(year==2013 & same==0, 0,same2013),
         same2014=ifelse(year==2014 & same==1, 1,NA),
         same2014=ifelse(year==2014 & same==0, 0,same2014),
         same2015=ifelse(year==2015 & same==1, 1,NA),
         same2015=ifelse(year==2015 & same==0, 0,same2015)) %>%
  group_by(DocNPI) %>%
  fill(same2009,.direction="downup") %>%
  fill(same2010,.direction="downup") %>%
  fill(same2011,.direction="downup") %>%
  fill(same2012,.direction="downup") %>%
  fill(same2013,.direction="downup") %>%
  fill(same2014,.direction="downup") %>%
  fill(same2015,.direction="downup") %>%
  ungroup()

data <- data %>%
  mutate(newnpi=ifelse(year==2010 & same2009==0 & same2010==1,1,NA)) %>%
  mutate(newnpi=ifelse(year==2011 & same2010==0 & same2011==1,1,newnpi)) %>%
  mutate(newnpi=ifelse(year==2012 & same2011==0 & same2012==1,1,newnpi)) %>%
  mutate(newnpi=ifelse(year==2013 & same2012==0 & same2013==1,1,newnpi)) %>%
  mutate(newnpi=ifelse(year==2014 & same2013==0 & same2014==1,1,newnpi)) %>%
  mutate(newnpi=ifelse(year==2015 & same2014==0 & same2015==1,1,newnpi)) %>%
  mutate(newnpi=ifelse(is.na(newnpi),0,newnpi))

data <- data %>%
  select(-same,-same2009,-same2010,-same2011,-same2012,-same2013,-same2014,-same2015)

# Create variable for never adds a new npi
data <- data %>%
  group_by(DocNPI) %>%
  mutate(sum=sum(newnpi)) %>%
  ungroup() %>%
  mutate(never_newnpi=ifelse(sum==0,1,0)) %>%
  select(-sum)

# save a hospital level dataset
hosp_data <- data %>%
  distinct(year, HospNPI, MCRNUM, MADMIN, EHLTH, cpoe, cdr, cdss, order_entry, phys_doc, phys_portal, SYSID, SUBS, BDTOT, FTMDTF, FTRES, 
           PTMDTF, PTRES, JNTPH, PHYGP, IPAHOS, OPHOSHOS, CPHOHOS, ISMHOS, FTRNTF,
           IPAP, OPHP, CPHP, ISMP, mcare_exposure, mcaid_exposure, change_years, any_EHR)

saveRDS(hosp_data, paste0(created_data_path, "hosp_data_NEW.rds"))

# ------------------------------------------------------------------
# Robustness check for reviewer: hospitals/hospitalists with only
# ONE form of physician-hospital integration present
# ------------------------------------------------------------------

# Purity of integration form at the hospital level, using baseline
# (first observed) status per hospital rather than pooling across
# years, since integration status is time-varying
hosp_integration_purity <- hosp_data %>%
  ungroup() %>%
  group_by(HospNPI) %>%
  arrange(year) %>%
  slice(1) %>%                      # baseline (earliest observed) year
  ungroup() %>%
  mutate(across(c(IPAHOS, OPHOSHOS, CPHOHOS, ISMHOS), ~ replace_na(., 0))) %>%
  mutate(n_forms = IPAHOS + OPHOSHOS + CPHOHOS + ISMHOS,
         single_form = case_when(
           n_forms == 1 & IPAHOS   == 1 ~ "IPA only",
           n_forms == 1 & OPHOSHOS == 1 ~ "OPHO only",
           n_forms == 1 & CPHOHOS  == 1 ~ "CPHO only",
           n_forms == 1 & ISMHOS   == 1 ~ "ISM only",
           n_forms == 0                 ~ "None",
           TRUE                          ~ "Multiple forms"
         )) %>%
  select(HospNPI, n_forms, single_form)

# how common is single-form status?
hosp_integration_purity %>% count(single_form) %>% mutate(share = n / sum(n))

# Create average exposure among all hospitals physician works with
data <- data %>%
  group_by(MCRNUM, year) %>%
  mutate(total_exposure = sum(mcaid_exposure, mcare_exposure)) %>%
  ungroup() %>%
  group_by(DocNPI, year) %>%
  mutate(weightedavg_exposure = weighted.mean(total_exposure, samedaycount, na.rm=T)) %>%
  mutate(weightedavg_mcaid_exposure = weighted.mean(mcaid_exposure, samedaycount, na.rm=T)) %>%
  mutate(weightedavg_mcare_exposure = weighted.mean(mcare_exposure, samedaycount, na.rm=T)) %>%
  ungroup()

# Aggregate the data to the physician level -------------------------------------------------------------

Aggregated_Pairs <- data %>%
  distinct(year, DocNPI, grad_year, avg_beds, experience, 
           num_hospitals, doc_sc_years,
           frac_EHR, minyr_EHR, frac_anyphys_EHR, frac_cpoe_EHR, minyr_anyphys_EHR, minyr_cpoe_EHR,
           num_systems, hosp_patient_count, hosp_patient_count_EHRhosp, hosp_patient_count_noEHRhosp, 
           newnpi, never_newnpi, weightedavg_exposure, weightedavg_mcaid_exposure, weightedavg_mcare_exposure,
           phys_ever_IPA, phys_ever_OPHO, phys_ever_CPHO, phys_ever_ISM,
           phys_integration_group)
#724k obs

# Now complete the data
Aggregated_Pairs <- complete(Aggregated_Pairs,DocNPI,year=2009:2017)

Aggregated_Pairs <- Aggregated_Pairs %>%
  group_by(DocNPI) %>%
  fill(minyr_EHR, minyr_anyphys_EHR, minyr_cpoe_EHR, doc_sc_years, .direction="downup") %>%
  fill(grad_year,.direction="downup") %>%
  fill(avg_beds,.direction="downup") %>%
  fill(num_hospitals,.direction="downup") %>%
  fill(num_systems, weightedavg_exposure, phys_ever_IPA, phys_ever_OPHO, phys_ever_CPHO, phys_ever_ISM, phys_integration_group, .direction="downup") %>%
  ungroup() %>%
  mutate(experience=year-grad_year) %>%
  mutate(anyEHR_exposed=ifelse(minyr_EHR>0 & year>=minyr_EHR,1,0))



# Save the Data for Analysis -----------------------------------------------------------------
saveRDS(Aggregated_Pairs,file=paste0(created_data_path,"Aggregated_Pairs_NEW.rds"))


# Balance Check
balance_check <- Aggregated_Pairs %>%
  select(year,DocNPI)

is.pbalanced(balance_check)

