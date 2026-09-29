library(tidyverse)
library(reshape)
library(knitr)
library(kableExtra)
library(ggplot2)
library(readr)
library(showtext)
library(panelView)
library(maps)
library(mapdata)
library(cdlTools)

options(knitr.kable.NA=" ")

# ------------------  Summary Stats and Figures for Third Year Paper -----------------------
#                     Hanna Glenn, Emory University
#                     10/8/2021

# This script creates preliminary summary stats and figures to explore relationships
# between EHR and physician decisions. Some of these will be used in my third year paper.
# The data used is created in "Final_Pairs.R"

# Read in Final_Pairs_Variables.rds
Physician_Data <- read_rds(paste0(created_data_path,"Physician_Data_NEW2.rds"))

Physician_Data <- Physician_Data %>%
  mutate(DocNPI=as.double(DocNPI))

Physician_Data_EHR <- Physician_Data %>%
  ungroup() %>%
  mutate(bundled = purrr::map2_lgl(doc_sc_years, minyr_EHR,
                                   ~ !is.null(.x) && .y > 0 && any(abs(.x - .y) <= 0))) %>%
  group_by(DocNPI) %>%
  mutate(drop_md = any(bundled)) %>%
  ungroup() %>%
  filter(!drop_md)

Physician_Data_EHR <- Physician_Data_EHR %>%
  filter(minyr_EHR!=0 & minyr_EHR!=2009)

Physician_Data_EHR <- Physician_Data_EHR %>%
  filter(first_mdppas_yr==2009) 

Physician_Data_EHR %>%
  group_by(minyr_EHR) %>%
  distinct(DocNPI) %>%
  count()

Physician_Data_EHR <- Physician_Data_EHR %>%
  mutate(retired_ever = ifelse(!is.na(minyr_retire) & year >= minyr_retire, 1, 0)) %>%
  mutate(claims_inpat  = claim_count_total * pos_inpat,
         claims_office = claim_count_total * pos_office,
         claims_opd    = claim_count_total * pos_opd) %>%
  group_by(DocNPI) %>%
  mutate(stable_inpat = all(pos_inpat > 0.7, na.rm = TRUE)) %>%
  ungroup()

# Employer type (billing setting) classification, treated as an
# outcome for the office sample only -- NOT included in the exit
# or productivity samples. Baseline = year before first EHR exposure.
phys_type_baseline <- Physician_Data_EHR %>%
  filter(minyr_EHR > 0, year == minyr_EHR - 1, !is.na(tin1_type)) %>%
  distinct(DocNPI, tin1_type) %>%
  dplyr::rename(phys_employer_type = tin1_type)

Physician_Data_EHR <- Physician_Data_EHR %>%
  select(-any_of("phys_employer_type")) %>%
  left_join(phys_type_baseline, by = "DocNPI") %>%
  mutate(
    phys_hospital_affiliated = as.numeric(
      phys_employer_type %in% c(
        "hospital_direct",
        "integrated_system_group"
      )
    ),
    phys_staffing_company = as.numeric(
      phys_employer_type == "staffing_company"
    ),
    phys_solo_independent = as.numeric(
      phys_employer_type == "solo_independent"
    )
  )
## single source of truth for row order in BOTH tables below
ROW_ORDER <- c("Number of Hospitals Worked With", "Female",
               "Number of Systems Worked With", "Age",
               "Year of EHR Exposure",
               "Ever Exit Clinical Work", "Fraction Patients in Office",
               "Work in an Office", "Number of Patients", "Claims per Patient",
               "Hospital-Direct Employer",
               "Staffing Company Employer", "Solo/Independent Employer",
               "Num. Hospitalists")

order_rows <- function(df) {
  df %>%
    mutate(variable = factor(variable, levels = ROW_ORDER)) %>%
    arrange(variable) %>%
    mutate(variable = as.character(variable))
}


# General Summary Stats Tables:  ----------------------------------------------------------------

# Physician Level
sum_stats_exit <- Physician_Data_EHR %>% ungroup() %>%
  summarise_at(c("Number of Hospitals Worked With"="num_hospitals",
                 "Female"="female", "Number of Systems Worked With"="num_systems",
                 "Age"="age",
                 "Year of EHR Exposure"="minyr_EHR",
                 "Ever Exit Clinical Work"="ever_retire"), 
               list(m=mean,sd=sd), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  tidyr::extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m,sd) %>%
  add_row(variable = "Fraction Patients in Office") %>%
  add_row(variable = "Work in an Office") %>%
  add_row(variable = "Number of Patients") %>%
  add_row(variable = "Claims per Patient") %>%
  add_row(variable = "Hospital-Direct Employer") %>%
  add_row(variable = "Staffing Company Employer") %>%
  add_row(variable = "Solo/Independent Employer")

n_exit <- Physician_Data_EHR %>%
  ungroup() %>%
  distinct(DocNPI) %>%
  nrow()

sum_stats_office <- Physician_Data_EHR %>% ungroup() %>% filter(ever_retire==0) %>%
  summarise_at(c("Number of Hospitals Worked With"="num_hospitals",
                 "Female"="female", "Number of Systems Worked With"="num_systems",
                 "Age"="age",
                 "Year of EHR Exposure"="minyr_EHR",
                 "Fraction Patients in Office"="pos_office",
                 "Work in an Office"="work_in_office",
                 "Hospital-Direct Employer"="phys_hospital_affiliated",
                 "Staffing Company Employer"="phys_staffing_company",
                 "Solo/Independent Employer"="phys_solo_independent"), 
               list(m.office=mean,sd.office=sd), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  tidyr::extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m.office,sd.office)  %>%
  add_row(variable="Ever Exit Clinical Work") %>%
  add_row(variable = "Number of Patients") %>%
  add_row(variable = "Claims per Patient")

n_office <- Physician_Data_EHR %>%
  ungroup() %>%
  filter(ever_retire==0) %>%
  distinct(DocNPI) %>%
  nrow()

sum_stats_prod <- Physician_Data_EHR %>% ungroup() %>% 
  filter(ever_retire==0 & stable_inpat==1) %>%
  summarise_at(c("Number of Hospitals Worked With"="num_hospitals",
                 "Female"="female", "Number of Systems Worked With"="num_systems",
                 "Age"="age",
                 "Number of Patients"="npi_unq_benes",
                 "Year of EHR Exposure"="minyr_EHR",
                 "Claims per Patient"="claim_per_patient"), 
               list(m.prod=mean,sd.prod=sd), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  tidyr::extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m.prod,sd.prod) %>%
  add_row(variable="Ever Exit Clinical Work") %>%
  add_row(variable = "Fraction Patients in Office") %>%
  add_row(variable = "Work in an Office") %>%
  add_row(variable = "Hospital-Direct Employer") %>%
  add_row(variable = "Staffing Company Employer") %>%
  add_row(variable = "Solo/Independent Employer")

n_prod <- Physician_Data_EHR %>%
  ungroup() %>%
  filter(ever_retire==0 & stable_inpat==1) %>%
  distinct(DocNPI) %>%
  nrow()

sum_stats <- sum_stats_exit %>%
  left_join(sum_stats_office, by = "variable") %>%
  left_join(sum_stats_prod, by= "variable") %>%
  add_row(variable = "Num. Hospitalists", m=n_exit, m.office = n_office, m.prod = n_prod)

fmt <- function(x, is_year = FALSE) {
  ifelse(is.na(x), "",
         ifelse(is_year,
                formatC(x, format = "f", digits = 0),
                ifelse(abs(x) >= 100,
                       formatC(x, format = "f", digits = 0, big.mark = ","),
                       formatC(x, format = "f", digits = 2))))
}

sum_stats <- sum_stats %>%
  mutate(is_year = variable == "Year of EHR Exposure") %>%
  mutate(across(where(is.numeric), ~ fmt(.x, is_year = is_year))) %>%
  select(-is_year)

sum_stats <- order_rows(sum_stats)
stopifnot(all(ROW_ORDER %in% sum_stats$variable), nrow(sum_stats) == length(ROW_ORDER))

knitr::kable(sum_stats,
             format="latex",
             table.envir="table",
             col.names=c("Variable","Mean","SD","Mean","SD","Mean","SD"),
             digits=2,
             caption="Summary Statistics",
             booktabs=TRUE,
             escape=F,
             label = "tab:sumstatsnew",
             align=c("l","c","c","c","c","c", "c"),
             position="ht!") %>%
  kable_styling(full_width=F) %>%
  pack_rows(index = c("Physician Characteristics" = 4, "EHR Exposure" = 1,
                      "Outcome Variables" = 5, "Employer Type" = 4, " " = 1)) %>%
  add_header_above(c(" ", "Exit Sample"=2, "Office Sample"=2, "Productivity Sample"=2)) %>%
  save_kable("./Table Code/overall_stats_new.tex")


# Summary Stats of all variables by old vs. young  ---------------------------------------------------------
AGE_VAR <- "max_age"   # see note

vars_exit <- c("Number of Hospitals Worked With"="num_hospitals",
               "Female"="female", "Number of Systems Worked With"="num_systems",
               "Age"="age",
               "Year of EHR Exposure"="minyr_EHR",
               "Ever Exit Clinical Work"="ever_retire")

vars_office <- c("Number of Hospitals Worked With"="num_hospitals",
                 "Female"="female", "Number of Systems Worked With"="num_systems",
                 "Age"="age",
                 "Year of EHR Exposure"="minyr_EHR",
                 "Fraction Patients in Office"="pos_office",
                 "Work in an Office"="work_in_office",
                 "Hospital-Direct Employer"="phys_hospital_affiliated",
                 "Staffing Company Employer"="phys_staffing_company",
                 "Solo/Independent Employer"="phys_solo_independent")

vars_prod <- c("Number of Hospitals Worked With"="num_hospitals",
               "Female"="female", "Number of Systems Worked With"="num_systems",
               "Age"="age",
               "Number of Patients"="npi_unq_benes",
               "Year of EHR Exposure"="minyr_EHR",
               "Claims per Patient"="claim_per_patient")

## mean only, one column, suffix must contain no underscore
means_by <- function(dat, vars, suffix) {
  dat %>% ungroup() %>%
    summarise_at(vars, setNames(list(mean), suffix), na.rm = TRUE) %>%
    mutate_if(is.numeric, ~ifelse(abs(.) == Inf, NA, .)) %>%
    gather(key = var, value = value) %>%
    tidyr::extract(col = "var", into = c("variable", "statistic"),
                   regex = "(.*)_(.*)$") %>%
    spread(key = statistic, value = value)
}

## one sample -> two columns (young, old)
sample_block <- function(dat, vars, tag, add_rows = character(0)) {
  y <- means_by(dplyr::filter(dat, .data[[AGE_VAR]] <  60), vars, paste0(tag, ".y"))
  o <- means_by(dplyr::filter(dat, .data[[AGE_VAR]] >= 60), vars, paste0(tag, ".o"))
  out <- left_join(y, o, by = "variable")
  for (r in add_rows) out <- add_row(out, variable = r)
  out
}

dat_exit   <- Physician_Data_EHR
dat_office <- Physician_Data_EHR %>% filter(ever_retire == 0)
dat_prod   <- Physician_Data_EHR %>% filter(ever_retire == 0 & stable_inpat == 1)

sum_stats_exit <- sample_block(dat_exit, vars_exit, "exit",
                               c("Fraction Patients in Office", "Work in an Office",
                                 "Number of Patients", "Claims per Patient",
                                 "Hospital-Direct Employer",
                                 "Staffing Company Employer", "Solo/Independent Employer"))

sum_stats_office <- sample_block(dat_office, vars_office, "office",
                                 c("Ever Exit Clinical Work", "Number of Patients", "Claims per Patient"))

sum_stats_prod <- sample_block(dat_prod, vars_prod, "prod",
                               c("Ever Exit Clinical Work", "Fraction Patients in Office", "Work in an Office",
                                 "Hospital-Direct Employer", "Integrated System Employer",
                                 "Staffing Company Employer", "Solo/Independent Employer"))

n_doc <- function(dat, young) {
  dat %>% ungroup() %>%
    filter(if (young) .data[[AGE_VAR]] < 60 else .data[[AGE_VAR]] >= 60) %>%
    distinct(DocNPI) %>% nrow()
}

sum_stats <- sum_stats_exit %>%
  left_join(sum_stats_office, by = "variable") %>%
  left_join(sum_stats_prod,   by = "variable") %>%
  add_row(variable  = "Num. Hospitalists",
          exit.y    = n_doc(dat_exit,   TRUE),  exit.o   = n_doc(dat_exit,   FALSE),
          office.y  = n_doc(dat_office, TRUE),  office.o = n_doc(dat_office, FALSE),
          prod.y    = n_doc(dat_prod,   TRUE),  prod.o   = n_doc(dat_prod,   FALSE)) %>%
  relocate(variable, exit.y, exit.o, office.y, office.o, prod.y, prod.o)

sum_stats <- sum_stats %>%
  mutate(is_year = variable == "Year of EHR Exposure") %>%
  mutate(across(where(is.numeric), ~ fmt(.x, is_year = is_year))) %>%
  select(-is_year)

sum_stats <- order_rows(sum_stats)
stopifnot(all(ROW_ORDER %in% sum_stats$variable), nrow(sum_stats) == length(ROW_ORDER))

knitr::kable(sum_stats,
             format = "latex", table.envir = "table",
             col.names = c("Variable", rep(c("$<$60", "$\\geq$60"), 3)),
             digits = 2, caption = "Means by Age Group\\label{tab:means_age}",
             booktabs = TRUE, escape = FALSE,
             align = c("l", rep("c", 6)), position = "h") %>%
  kable_styling(full_width = FALSE) %>%
  pack_rows(index = c("Physician Characteristics" = 4, "EHR Exposure" = 1,
                      "Outcome Variables" = 5, "Employer Type" = 3, " " = 1)) %>%
  add_header_above(c(" ", "Exit Sample" = 2, "Office Sample" = 2,
                     "Productivity Sample" = 2)) %>%
  save_kable("./Table Code/overall_stats_by_age.tex")



##---- Cohort sizes: staggered adoption ------------------------
SAMPLES <- list(
  "Exit"         = Physician_Data_EHR,
  "Office"       = dplyr::filter(Physician_Data_EHR, ever_retire == 0),
  "Productivity" = dplyr::filter(Physician_Data_EHR,
                                 ever_retire == 0, stable_inpat == 1)
)

cohort_data <- purrr::imap_dfr(SAMPLES, function(d, nm) {
  d %>%
    ungroup() %>%
    distinct(DocNPI, minyr_EHR) %>%
    count(minyr_EHR, name = "n_docs") %>%
    mutate(Sample = nm)
}) %>%
  mutate(Sample = factor(Sample, levels = names(SAMPLES)))

pal_sample <- c("Exit"         = "#16324F",
                "Office"       = "#4C7FA6",
                "Productivity" = "#A34A5A")

cohort_plot <- ggplot(cohort_data,
                      aes(x = factor(minyr_EHR), y = n_docs, fill = Sample)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  geom_text(aes(label = formatC(n_docs, format = "d", big.mark = ",")),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3.2, colour = "grey25") +
  scale_fill_manual(values = pal_sample) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12)),
                     breaks = scales::pretty_breaks(n = 6),
                     labels = scales::comma) +
  labs(x = "\nYear of First EHR Exposure", y = "Hospitalists\n") +
  theme_bw() +
  theme(
    text               = element_text(size = 14),
    axis.text          = element_text(size = 13),
    axis.title         = element_text(size = 14),
    legend.title       = element_blank(),
    legend.position    = "bottom",
    legend.text        = element_text(size = 13),
    legend.key.width   = unit(1.6, "lines"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, colour = "grey65"),
    panel.border       = element_rect(colour = "grey30", fill = NA, linewidth = 0.5)
  )

print(cohort_plot)
ggsave("Objects/cohort_sizes.pdf", plot = cohort_plot,
       width = 9, height = 5, units = "in")

## totals, for the figure note
cohort_data %>% group_by(Sample) %>% summarise(total = sum(n_docs))


# summary stats for levels of integration:  ----------------------------------------------------------------

sum_stats_ipa <- Physician_Data_EHR %>% ungroup() %>% filter(phys_ever_IPA==1 & minyr_EHR>0) %>%
  summarise_at(c("Number of Hospitals Worked With"="num_hospitals",
                 "Female"="female", "Number of Systems Worked With"="num_systems",
                 "Age"="age",
                 "Year of EHR Exposure"="minyr_EHR",
                 "Ever Exit Clinical Work"="ever_retire",
                 "Fraction Patients in Office"="pos_office",
                 "Work in an Office"="work_in_office",
                 "Year of EHR Exposure"="minyr_EHR",
                 "Claims per Patient"="claim_per_patient",
                 "Number of Patients"="npi_unq_benes"), 
               list(m=mean,sd=sd), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m,sd) 

sum_stats_opho <- Physician_Data_EHR %>% ungroup() %>% filter(phys_ever_OPHO==1 & minyr_EHR>0) %>%
  summarise_at(c("Number of Hospitals Worked With"="num_hospitals",
                 "Female"="female", "Number of Systems Worked With"="num_systems",
                 "Age"="age",
                 "Year of EHR Exposure"="minyr_EHR",
                 "Ever Exit Clinical Work"="ever_retire",
                 "Fraction Patients in Office"="pos_office",
                 "Work in an Office"="work_in_office",
                 "Year of EHR Exposure"="minyr_EHR",
                 "Claims per Patient"="claim_per_patient",
                 "Number of Patients"="npi_unq_benes"), 
               list(m=mean,sd=sd), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m,sd) 

sum_stats_cpho <- Physician_Data_EHR %>% ungroup() %>% filter(phys_ever_CPHO==1 & minyr_EHR>0) %>%
  summarise_at(c("Number of Hospitals Worked With"="num_hospitals",
                 "Female"="female", "Number of Systems Worked With"="num_systems",
                 "Age"="age",
                 "Year of EHR Exposure"="minyr_EHR",
                 "Ever Exit Clinical Work"="ever_retire",
                 "Fraction Patients in Office"="pos_office",
                 "Work in an Office"="work_in_office",
                 "Year of EHR Exposure"="minyr_EHR",
                 "Claims per Patient"="claim_per_patient",
                 "Number of Patients"="npi_unq_benes"), 
               list(m=mean,sd=sd), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m,sd) 

sum_stats_ism <- Physician_Data_EHR %>% ungroup() %>% filter(phys_ever_ISM==1 & minyr_EHR>0) %>%
  summarise_at(c("Number of Hospitals Worked With"="num_hospitals",
                 "Female"="female", "Number of Systems Worked With"="num_systems",
                 "Age"="age",
                 "Year of EHR Exposure"="minyr_EHR",
                 "Ever Exit Clinical Work"="ever_retire",
                 "Fraction Patients in Office"="pos_office",
                 "Work in an Office"="work_in_office",
                 "Year of EHR Exposure"="minyr_EHR",
                 "Claims per Patient"="claim_per_patient",
                 "Number of Patients"="npi_unq_benes"), 
               list(m=mean,sd=sd), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m,sd) 

n_ipa <- Physician_Data_EHR %>%
  ungroup() %>%
  filter(phys_ever_IPA==1 & minyr_EHR>0) %>%
  distinct(DocNPI) %>%
  nrow()
n_opho <- Physician_Data_EHR %>%
  ungroup() %>%
  filter(phys_ever_OPHO==1 & minyr_EHR>0) %>%
  distinct(DocNPI) %>%
  nrow()
n_cpho <- Physician_Data_EHR %>%
  ungroup() %>%
  filter(phys_ever_CPHO==1 & minyr_EHR>0) %>%
  distinct(DocNPI) %>%
  nrow()
n_ism <- Physician_Data_EHR %>%
  ungroup() %>%
  filter(phys_ever_ISM==1 & minyr_EHR>0) %>%
  distinct(DocNPI) %>%
  nrow()

sum_stats <- sum_stats_ipa %>%
  left_join(sum_stats_opho, by="variable") %>%
  left_join(sum_stats_cpho, by="variable") %>%
  left_join(sum_stats_ism, by="variable") %>%
  add_row(variable = "No. Hospitalists", m.x=n_ipa, m.y=n_opho, m.x.x=n_cpho, m.y.y=n_ism)

sum_stats <- sum_stats %>%
  mutate(is_year = variable == "Year of EHR Exposure") %>%
  mutate(across(where(is.numeric), ~ fmt(.x, is_year = is_year))) %>%
  select(-is_year)


knitr::kable(sum_stats[c(1,4,6,8,10,3,5,9,7,2,11),],
             format="latex",
             table.envir="table",
             col.names=c("Variable","Mean","s.d.", "Mean","s.d.", "Mean","s.d.", "Mean","s.d."),
             digits=2,
             caption="Summary Statistics by Integration Type",
             booktabs=TRUE,
             escape=F,
             align=c("l","c","c","c","c","c"),
             position="h",
             label = "tab:means_int") %>%
  kable_styling(full_width=F) %>%
  pack_rows(index = c("Characteristics" = 5, "Outcomes" = 5, " " = 1)) %>%
  add_header_above(c(" "=1, "IPA"=2, "OPHO"=2, "CPHO"=2, "FIO"=2)) %>%
  save_kable("./Table Code/overall_stats_by_integration.tex")


# # Histogram of treatment and control for each group -------------------------------------------
# neg4 <- Physician_Data %>%
#   filter(minyr_EHR==2014 | minyr_EHR==2015) %>%
#   distinct(DocNPI, minyr_EHR) %>%
#   mutate(treat=ifelse(minyr_EHR==2014,1,0)) %>%
#   group_by(treat) %>%
#   mutate(count=1) %>%
#   mutate(sum=sum(count)) %>%
#   ungroup() %>%
#   distinct(treat,sum) %>%
#   mutate(time=-4)
# 
# neg3 <- Physician_Data %>%
#   filter(minyr_EHR>=2013 & minyr_EHR<=2015) %>%
#   distinct(DocNPI, minyr_EHR) %>%
#   mutate(treat=ifelse(minyr_EHR==2014 | minyr_EHR==2013,1,0),
#          control=ifelse(minyr_EHR>=2014 & minyr_EHR<=2015,1,0)) %>%
#   mutate(count=1) %>%
#   mutate(sum=ifelse(treat==1,sum(count[treat==1]),NA),
#          sum=ifelse(control==1,sum(count[control==1]),sum)) %>%
#   ungroup() %>%
#   distinct(treat,control, sum) %>%
#   filter(!(control==1 & treat==1)) %>%
#   distinct(treat,sum) %>%
#   mutate(time=-3)
# 
# neg2 <- Physician_Data %>%
#   filter(minyr_EHR>=2012 & minyr_EHR<=2015) %>%
#   distinct(DocNPI, minyr_EHR) %>%
#   mutate(treat=ifelse(minyr_EHR>=2012 & minyr_EHR<=2014,1,0),
#          control=ifelse(minyr_EHR>=2013 & minyr_EHR<=2015,1,0)) %>%
#   mutate(count=1) %>%
#   mutate(sum=ifelse(treat==1,sum(count[treat==1]),NA),
#          sum=ifelse(control==1,sum(count[control==1]),sum)) %>%
#   ungroup() %>%
#   distinct(treat,control, sum) %>%
#   filter(!(control==1 & treat==1)) %>%
#   distinct(treat,sum) %>%
#   mutate(time=-2)
# 
# neg1 <- Physician_Data %>%
#   filter(minyr_EHR>=2011 & minyr_EHR<=2015) %>%
#   distinct(DocNPI, minyr_EHR) %>%
#   mutate(treat=ifelse(minyr_EHR>=2011 & minyr_EHR<=2014,1,0),
#          control=ifelse(minyr_EHR>=2012 & minyr_EHR<=2015,1,0)) %>%
#   mutate(count=1) %>%
#   mutate(sum=ifelse(treat==1,sum(count[treat==1]),NA),
#          sum=ifelse(control==1,sum(count[control==1]),sum)) %>%
#   ungroup() %>%
#   distinct(treat,control, sum) %>%
#   filter(!(control==1 & treat==1)) %>%
#   distinct(treat,sum) %>%
#   mutate(time=-1)
# 
# neg <- Physician_Data %>%
#   filter(minyr_EHR>=2010 & minyr_EHR<=2015) %>%
#   distinct(DocNPI, minyr_EHR) %>%
#   mutate(treat=ifelse(minyr_EHR>=2010 & minyr_EHR<=2014,1,0),
#          control=ifelse(minyr_EHR>=2011 & minyr_EHR<=2015,1,0)) %>%
#   mutate(count=1) %>%
#   mutate(sum=ifelse(treat==1,sum(count[treat==1]),NA),
#          sum=ifelse(control==1,sum(count[control==1]),sum)) %>%
#   ungroup() %>%
#   distinct(treat,control, sum) %>%
#   filter(!(control==1 & treat==1)) %>%
#   distinct(treat,sum) %>%
#   mutate(time=0)
# 
# pos1 <- Physician_Data %>%
#   filter(minyr_EHR>=2010 & minyr_EHR<=2015) %>%
#   distinct(DocNPI, minyr_EHR) %>%
#   mutate(treat=ifelse(minyr_EHR>=2010 & minyr_EHR<=2013,1,0),
#          control=ifelse(minyr_EHR>=2012 & minyr_EHR<=2015,1,0)) %>%
#   mutate(count=1) %>%
#   mutate(sum=ifelse(treat==1,sum(count[treat==1]),NA),
#          sum=ifelse(control==1,sum(count[control==1]),sum)) %>%
#   ungroup() %>%
#   distinct(treat,control, sum) %>%
#   filter(!(control==1 & treat==1)) %>%
#   distinct(treat,sum) %>%
#   mutate(time=1)
# 
# pos2 <- Physician_Data %>%
#   filter(minyr_EHR!=0) %>%
#   distinct(DocNPI, minyr_EHR) %>%
#   mutate(treat=ifelse(minyr_EHR>=2010 & minyr_EHR<=2012,1,0),
#          control=ifelse(minyr_EHR>=2013 & minyr_EHR<=2015,1,0)) %>%
#   mutate(count=1) %>%
#   mutate(sum=ifelse(treat==1,sum(count[treat==1]),NA),
#          sum=ifelse(control==1,sum(count[control==1]),sum)) %>%
#   ungroup() %>%
#   distinct(treat,control, sum) %>%
#   filter(!(control==1 & treat==1)) %>%
#   distinct(treat,sum) %>%
#   mutate(time=2)
# 
# pos3 <- Physician_Data %>%
#   filter(minyr_EHR!=2012 & minyr_EHR!=2013) %>%
#   distinct(DocNPI, minyr_EHR) %>%
#   mutate(treat=ifelse(minyr_EHR==2010 | minyr_EHR==2011,1,0),
#          control=ifelse(minyr_EHR==2014 | minyr_EHR==2015,1,0)) %>%
#   mutate(count=1) %>%
#   mutate(sum=ifelse(treat==1,sum(count[treat==1]),NA),
#          sum=ifelse(control==1,sum(count[control==1]),sum)) %>%
#   ungroup() %>%
#   distinct(treat,control, sum) %>%
#   filter(!(control==1 & treat==1)) %>%
#   distinct(treat,sum) %>%
#   mutate(time=3)
# 
# pos4 <- Physician_Data %>%
#   filter(minyr_EHR==2010 & minyr_EHR==2015) %>%
#   distinct(DocNPI, minyr_EHR) %>%
#   mutate(treat=ifelse(minyr_EHR==2010,1,0),
#          control=ifelse(minyr_EHR==2015,1,0)) %>%
#   mutate(count=1) %>%
#   mutate(sum=ifelse(treat==1,sum(count[treat==1]),NA),
#          sum=ifelse(control==1,sum(count[control==1]),sum)) %>%
#   ungroup() %>%
#   distinct(treat,control, sum) %>%
#   filter(!(control==1 & treat==1)) %>%
#   distinct(treat,sum) %>%
#   mutate(time=4)
# 
# control_hist_data <- rbind(neg4, neg3, neg2, neg1, neg, pos1, pos2, pos3, pos4) %>%
#   mutate(treat=ifelse(treat==1,"Treatment", "Control")) %>%
#   dplyr::rename(Group=treat) %>%
#   mutate(time=as.factor(time))
# 
# ggplot(control_hist_data, aes(x=time, y=sum, fill=Group)) + 
#  geom_bar(stat='identity') + theme_bw() +
#   theme_light() + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))  +
#   ylab("Count of Physicians") + xlab("Event Time")
# 
# ggsave(plot=last_plot(), file="/Objects/control_histogram.pdf", height=7, width=10, units="in")


## plot distribution of where retiring physicians who retire are located ############
zipcw <- read_excel(paste0(raw_data_path, "ZIPCodetoZCTACrosswalk2010UDS.xls"))

zipcw <- zipcw %>%
  distinct(ZIP, StateAbbr)

Physician_Data_EHR <- Physician_Data_EHR %>%
  mutate(phy_zip1=as.character(phy_zip1)) %>%
  left_join(zipcw, by=c("phy_zip1"="ZIP"))

Physician_Data_EHR <- Physician_Data_EHR %>%
  group_by(DocNPI) %>%
  fill(StateAbbr, .direction="downup") %>%
  ungroup()

all_states <- tibble(StateAbbr = c(state.abb, "DC"))

retire_data <- Physician_Data_EHR %>%
  filter(ever_retire == 1) %>%
  distinct(DocNPI, StateAbbr) %>%
  count(StateAbbr, name = "n") %>%
  filter(!is.na(StateAbbr)) %>%
  right_join(all_states, by = "StateAbbr") %>%
  mutate(n = tidyr::replace_na(n, 0)) %>%
  arrange(StateAbbr)

ggplot(retire_data, aes(x=StateAbbr, y=n)) + geom_bar(stat = "identity")

# make it into a map
states_map <- map_data("state") %>%
  mutate(StateAbbr = state.abb[match(region, tolower(state.name))])

map_data_full <- states_map %>%
  left_join(retire_data, by = "StateAbbr") %>%
  mutate(n = tidyr::replace_na(n, 0))

p <- ggplot(map_data_full, aes(long, lat, group = group, fill = n)) +
  geom_polygon(colour = "grey40", linewidth = 0.2) +
  coord_map("albers", lat0 = 29.5, lat1 = 45.5) +
  scale_fill_gradient(low = "#E8E4E6", high = "#16324F",
                      name = "Exiting Hospitalists",
                      breaks = scales::pretty_breaks(n = 5),
                      guide = guide_colourbar(
                        title.position = "top",
                        title.hjust = 0.5,
                        barwidth  = unit(12, "lines"),
                        barheight = unit(0.7, "lines"),
                        frame.colour = "grey40",
                        ticks.colour = "grey40")) +
  theme_void() +
  theme(
    text            = element_text(size = 14),
    legend.title    = element_text(size = 13),
    legend.text     = element_text(size = 12),
    legend.position = "bottom",
    legend.margin   = margin(t = -5),
    plot.margin     = margin(2, 2, 2, 2)
  )

ggsave("Objects/retire_map.pdf", plot = p,
       width = 7.5, height = 4.8, units = "in")
