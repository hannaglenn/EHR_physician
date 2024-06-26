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
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



# ------------------  Summary Stats and Figures for Third Year Paper -----------------------
#                     Hanna Glenn, Emory University
#                     10/8/2021

# This script creates preliminary summary stats and figures to explore relationships
# between EHR and physician decisions. Some of these will be used in my third year paper.
# The data used is created in "Final_Pairs.R"

# Read in Final_Pairs_Variables.rds
Physician_Data <- read_rds(paste0(created_data_path,"Physician_Data.rds"))



# General Summary Stats Tables:  ----------------------------------------------------------------

# Physician Level
sum_stats_exit <- Physician_Data %>% ungroup() %>% filter(minyr_EHR>0) %>%
  summarise_at(c("Number of Hospitals Worked With"="num_hospitals",
                 "Female"="female", "Number of Systems Worked With"="num_systems",
                 "Age"="age",
                 "Year of EHR Exposure"="minyr_EHR",
                 "Ever Exit Clinical Work"="ever_retire",
                 "Works w/ IPA Hosp."="phys_ever_IPA",
                 "Works w/ OPHO Hosp."="phys_ever_OPHO",
                 "Works w/ CPHO Hosp."="phys_ever_CPHO",
                 "Works w/ FIO Hosp."="phys_ever_ISM"), 
               list(m=mean,sd=sd), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m,sd) %>%
  add_row(variable = "Fraction Patients in Office") %>%
  add_row(variable = "Work in an Office") %>%
  add_row(variable = "Number of Patients") %>%
  add_row(variable = "Claims per Patient")

n_exit <- Physician_Data %>%
  ungroup() %>%
  filter(minyr_EHR>0) %>%
  distinct(DocNPI) %>%
  nrow()

sum_stats_office <- Physician_Data %>% ungroup() %>% filter(ever_retire==0) %>%
  filter(minyr_EHR>0) %>%
  summarise_at(c("Number of Hospitals Worked With"="num_hospitals",
                 "Female"="female", "Number of Systems Worked With"="num_systems",
                 "Age"="age",
                 "Year of EHR Exposure"="minyr_EHR",
                 "Fraction Patients in Office"="pos_office",
                 "Work in an Office"="work_in_office",
                 "Works w/ IPA Hosp."="phys_ever_IPA",
                 "Works w/ OPHO Hosp."="phys_ever_OPHO",
                 "Works w/ CPHO Hosp."="phys_ever_CPHO",
                 "Works w/ FIO Hosp."="phys_ever_ISM"), 
               list(m.office=mean,sd.office=sd), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m.office,sd.office)  %>%
  add_row(variable="Ever Exit Clinical Work") %>%
  add_row(variable = "Number of Patients") %>%
  add_row(variable = "Claims per Patient")

n_office <- Physician_Data %>%
  ungroup() %>%
  filter(minyr_EHR>0 & ever_retire==0) %>%
  distinct(DocNPI) %>%
  nrow()

sum_stats_prod <- Physician_Data %>% ungroup() %>% 
  filter(ever_retire==0 & never_newnpi==1) %>%
  filter(minyr_EHR>0) %>%
  summarise_at(c("Number of Hospitals Worked With"="num_hospitals",
                 "Female"="female", "Number of Systems Worked With"="num_systems",
                 "Age"="age",
                 "Number of Patients"="npi_unq_benes",
                 "Year of EHR Exposure"="minyr_EHR",
                 "Claims per Patient"="claim_per_patient",
                 "Works w/ IPA Hosp."="phys_ever_IPA",
                 "Works w/ OPHO Hosp."="phys_ever_OPHO",
                 "Works w/ CPHO Hosp."="phys_ever_CPHO",
                 "Works w/ FIO Hosp."="phys_ever_ISM"), 
               list(m.prod=mean,sd.prod=sd), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m.prod,sd.prod) %>%
  add_row(variable="Ever Exit Clinical Work") %>%
  add_row(variable = "Fraction Patients in Office") %>%
  add_row(variable = "Work in an Office") 

n_prod <- Physician_Data %>%
  ungroup() %>%
  filter(minyr_EHR>0 & ever_retire==0 & never_newnpi==1) %>%
  distinct(DocNPI) %>%
  nrow()

sum_stats <- sum_stats_exit %>%
  left_join(sum_stats_office, by = "variable") %>%
  left_join(sum_stats_prod, by= "variable") %>%
  add_row(variable = "Num. Hospitalists", m=n_exit, m.office = n_office, m.prod = n_prod)



knitr::kable(sum_stats[c(1,3,4,5,8,9,6,7,10,2,11,12,13,14,15),],
             format="latex",
             table.envir="table",
             col.names=c("Variable","Mean","SD","Mean","SD","Mean","SD"),
             digits=2,
             caption="Summary Statistics",
             booktabs=TRUE,
             escape=F,
             align=c("l","c","c","c","c","c", "c"),
             position="h") %>%
  kable_styling(full_width=F) %>%
  pack_rows(index = c("Physician Characteristics" = 4, "Integration Characteristics"=4, "EHR Exposure" = 1, "Outcome Variables" = 5, " ")) %>%
  add_header_above(c(" ", "Exit Sample"=2, "Office Sample"=2, "Productivity Sample"=2))


# Summary Stats of all variables by old vs. young  ---------------------------------------------------------
means_old <- Physician_Data %>% ungroup() %>%
  filter(minyr_EHR>0) %>%
  filter(max_age>59) %>%
  summarise_at(c("Number of Hospitals Worked With"="num_hospitals",
                 "Female"="female", "Number of Systems Worked With"="num_systems",
                 "Age"="age",
                 "Number of Patients"="npi_unq_benes",
                 "Year of EHR Exposure"="minyr_EHR",
                 "Fraction Patients in Office"="pos_office",
                 "Ever Retire"="ever_retire",
                 "Work in an Office"="work_in_office",
                 "Claims Per Patient"="claim_per_patient"), list(mean), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.)) %>%
  gather(key=var,value=value) %>%
  dplyr::rename(value_old=value)

means_young <- Physician_Data %>% ungroup() %>%
  filter(minyr_EHR>0) %>%
  filter(max_age<=59) %>%
  summarise_at(c("Number of Hospitals Worked With"="num_hospitals",
                 "Female"="female", "Number of Systems Worked With"="num_systems",
                 "Age"="age",
                 "Number of Patients"="npi_unq_benes",
                 "Year of EHR Exposure"="minyr_EHR",
                 "Fraction Patients in Office"="pos_office",
                 "Ever Retire"="ever_retire",
                 "Work in an Office"="work_in_office",
                 "Claims Per Patient"="claim_per_patient"), list(mean), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.)) %>%
  gather(key=var,value=value) %>%
  dplyr::rename(value_young=value)

means_bind <- means_young %>%
  left_join(means_old,by="var") 

knitr::kable(means_bind[c(8,9,7,5,10,6,4,2,1,3),], "latex",
             col.names=c("Variable","Age $<=$ 60", "Age $>$ 60"),
             digits=2,
             caption="Means by Age Sample",
             booktabs=TRUE,
             escape=F,
             align=c("l","c","c","c"),
             position="h") %>%
  kable_styling(full_width=F) %>%
  pack_rows(index = c("Outcomes" = 5, "Treatment" = 1, "Characteristics" = 4))


# EHR Info at the Physician Level (by year) -----------------------------------------------------------------
sum_stats_year <- Physician_Data %>% filter(minyr_EHR>0) %>% group_by(year) %>%
  summarise_at(c("Percent of Hospitals using EHR"="frac_EHR", 
                 "Percent of Hospitalists Exposed to EHR"="anyEHR_exposed"), list(mean), na.rm=T) 


# Create a dataframe out of the summary stats to put in a ggplot
sum_stats_year <- as.data.frame(sum_stats_year)
sum_stats_year <- melt(sum_stats_year, id.vars = "year", measure.vars = c("Percent of Hospitals using EHR",
                                                                          "Percent of Hospitalists Exposed to EHR"))
sum_stats_year <- sum_stats_year %>%
  dplyr::rename("Variable"="variable")

ggplot(sum_stats_year,aes(x=year,y=value,color=Variable, linetype=Variable)) + 
  geom_line(size=1.2) + geom_point(size=2) + labs(x="\nYear", y=" ") + 
  scale_colour_manual(values=cbbPalette) + ylim(0,1)  + xlim(2009,2015) + theme_bw() +
  theme(legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(4, "line"),
        legend.position = "bottom",
        legend.title = element_blank(),
        text=element_text(size=20)) 

ggsave("Objects/sum_stats_year.pdf", width=10, height=7, units="in")


# summary stats for levels of integration:  ----------------------------------------------------------------

sum_stats_ipa <- Physician_Data %>% ungroup() %>% filter(phys_ever_IPA==1 & minyr_EHR>0) %>%
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

sum_stats_opho <- Physician_Data %>% ungroup() %>% filter(phys_ever_OPHO==1 & minyr_EHR>0) %>%
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

sum_stats_cpho <- Physician_Data %>% ungroup() %>% filter(phys_ever_CPHO==1 & minyr_EHR>0) %>%
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

sum_stats_ism <- Physician_Data %>% ungroup() %>% filter(phys_ever_ISM==1 & minyr_EHR>0) %>%
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

n_ipa <- Physician_Data %>%
  ungroup() %>%
  filter(phys_ever_IPA==1 & minyr_EHR>0) %>%
  distinct(DocNPI) %>%
  nrow()
n_opho <- Physician_Data %>%
  ungroup() %>%
  filter(phys_ever_OPHO==1 & minyr_EHR>0) %>%
  distinct(DocNPI) %>%
  nrow()
n_cpho <- Physician_Data %>%
  ungroup() %>%
  filter(phys_ever_CPHO==1 & minyr_EHR>0) %>%
  distinct(DocNPI) %>%
  nrow()
n_ism <- Physician_Data %>%
  ungroup() %>%
  filter(phys_ever_ISM==1 & minyr_EHR>0) %>%
  distinct(DocNPI) %>%
  nrow()

sum_stats <- sum_stats_ipa %>%
  left_join(sum_stats_opho, by="variable") %>%
  left_join(sum_stats_cpho, by="variable") %>%
  left_join(sum_stats_ism, by="variable") %>%
  add_row(variable = "No. Hospitalists", m.x=n_ipa, m.y=n_opho, m.x.x=n_cpho, m.y.y=n_ism)


knitr::kable(sum_stats[c(1,4,6,8,10,3,5,9,7,2,11),],
             format="latex",
             table.envir="table",
             col.names=c("Variable","Mean","s.d.", "Mean","s.d.", "Mean","s.d.", "Mean","s.d."),
             digits=2,
             caption="Summary Statistics by Integration Type",
             booktabs=TRUE,
             escape=F,
             align=c("l","c","c","c","c","c"),
             position="h") %>%
  kable_styling(full_width=F) %>%
  pack_rows(index = c("Characteristics" = 5, "Outcomes" = 5, " " = 1)) %>%
  add_header_above(c(" "=1, "IPA"=2, "OPHO"=2, "CPHO"=2, "FIO"=2))


# Create graph showing the distributions of important variables---------------------------------------------------------
# year of retirement
retire_dis <- ggplot(filter(Physician_Data,minyr_EHR>0 & year==2013), aes(x=minyr_retire)) + ylab("Number of Physicians\n") + xlab("\nYear of Retirement")+ geom_histogram(binwidth=1, colour="black", fill="white") 

ggplot(filter(Physician_Data,minyr_EHR>0 & year==2013),
       aes(x=minyr_EHR,y=minyr_retire)) + geom_bin2d(binwidth=c(1,1)) + theme_bw() + xlim(2008.5,2015.5) +
  scale_fill_viridis_c() + xlab("\nYear Exposed to EHR") + ylab("Year of Retirement\n")

ggplot(filter(Physician_Data,minyr_EHR>0&year==2013),
       aes(x=minyr_EHR,y=work_in_office)) + geom_bin2d(binwidth=c(1,.7)) + theme_bw() + xlim(2008.5,2015.5) +
  scale_fill_viridis_c() + xlab("\nYear Exposed to EHR") + ylab("Office\n")


# Number of hospitals with positive same day count
ggplot(Physician_Data, aes(x=num_hospitals)) + geom_histogram(binwidth=1, colour="black", fill="white") +
  xlim(0,30)

# Number of systems
ggplot(Physician_Data, aes(x=num_systems)) + geom_histogram(binwidth=1, colour="black", fill="white") +
  xlim(0,30)

# Fraction of hospitals with EHR (do one plot for each year)
ggplot(Physician_Data, aes(x=frac_EHR,fill=as.character(year))) + geom_histogram(binwidth=.15) + 
  facet_grid(cols=vars(year)) + scale_colour_manual(values=cbbPalette) + xlim(0,1)

# Fraction of hospitals with EHR conditional on more than one hospital (do one plot for each year)
ggplot(filter(Physician_Data, num_hospitals>1), aes(x=frac_EHR,fill=as.character(year))) + geom_histogram(binwidth=.15) + 
  facet_grid(cols=vars(year)) + scale_colour_manual(values=cbbPalette) + xlim(0,1)

# year of expansion 
ggplot(Physician_Data, aes(x=minyr_EHR)) + geom_histogram(binwidth=1) + 
   scale_fill_manual(values=cbbPalette) + xlim(2008,2016) + ylim(0,1000000)

# year of expansion for physicians with more than one hospital
ggplot(filter(Physician_Data, num_hospitals>1), aes(x=minyr_EHR)) + geom_histogram(binwidth=1) + 
  scale_fill_manual(values=cbbPalette) + xlim(2008,2016) + ylim(0,1000000)





## Create hospital level implementation graph ---------------------------------------------------
AHAmainsurvey <- read_csv(paste0(raw_data_path,"AHA_mainsurvey.csv"))
phys_hosp_pairs <- readRDS(paste0(created_data_path,"phys_hosp_pairs.rds"))
AHANPI_cw <- read_rds(paste0(created_data_path,"AHANPI_cw.rds"))

AHAmainsurvey <- AHAmainsurvey %>%
  mutate(ID=as.character(ID)) %>%
  mutate(low_integration=ifelse(IPAHOS==1 | OPHOHOS==1 | CPHOHOS==1,1,0),
         high_integration=ifelse(GPWWHOS==1 | MSOHOS==1 | ISMHOS==1 | EQMODHOS==1,1,0)) %>%
  dplyr::rename(Hospital_name=MNAME, year=YEAR, full_year=FYR, 
         joint_phys=JNTPH, total_physicians=FTMT, phys_owned=PHYGP, beds=BDTOT ) %>%
  select(-IPAHOS, -OPHOHOS, -CPHOHOS, -GPWWHOS, -MSOHOS, -ISMHOS, -EQMODHOS) %>%
  select(-IPAP, -GPWP, -OPHP, -CPHP, -ISMP, -EQMP, -FNDP, -full_year, -EHLTHI, -EHLTHS,
         -total_physicians, -joint_phys, -phys_owned)


hosp_keep <- phys_hosp_pairs %>% ungroup() %>%
  distinct(HospNPI) %>%
  mutate(keep=1) 

AHANPI_cw <- AHANPI_cw %>%
  mutate(NPI=as.character(NPI))

hosp_keep <- hosp_keep %>%
  left_join(AHANPI_cw,by=c("HospNPI"="NPI")) %>%
  mutate(AHAID=as.character(AHAID))

AHAmainsurvey <- AHAmainsurvey %>%
  left_join(hosp_keep,by=c("ID"="AHAID")) %>%
  filter(keep==1) %>%
  select(-keep)



# Find out how many hospitals are missing an answer in every year
num_always_missing_EHR <- AHAmainsurvey %>% ungroup() %>%
  filter(is.na(EHLTH)) %>%
  mutate(EHLTH=ifelse(is.na(EHLTH),10,EHLTH)) %>%
  group_by(ID) %>%
  mutate(always_missing=ifelse(sum(EHLTH)==60,1,0)) %>%
  filter(always_missing==1) %>%
  ungroup() %>%
  distinct(ID,always_missing)

# Fill in missing year for EHR if it's between two years that have the same answer for EHR question
AHAmainsurvey <-AHAmainsurvey %>% group_by(ID) %>%
  dplyr::mutate(firstyear_0=min(year[EHLTH==0],na.rm=T),lastyear_0=max(year[EHLTH==0],na.rm=T)) %>%
  mutate(firstyear_0=ifelse(is.infinite(firstyear_0),NA,firstyear_0),lastyear_0=ifelse(is.infinite(lastyear_0),NA,lastyear_0)) %>%
  mutate(EHLTH=ifelse(firstyear_0<year & year<lastyear_0 & is.na(EHLTH),0,EHLTH))


AHAmainsurvey <- AHAmainsurvey %>% group_by(ID) %>%
  mutate(firstyear_1=min(year[EHLTH==1],na.rm=T),lastyear_1=max(year[EHLTH==1],na.rm=T)) %>%
  mutate(firstyear_1=ifelse(is.infinite(firstyear_1),NA,firstyear_1),lastyear_1=ifelse(is.infinite(lastyear_1),NA,lastyear_1)) %>%
  mutate(EHLTH=ifelse(firstyear_1<year & year<lastyear_1 & is.na(EHLTH),1,EHLTH))


AHAmainsurvey <- AHAmainsurvey %>% group_by(ID) %>%
  mutate(firstyear_2=min(year[EHLTH==2],na.rm=T),lastyear_2=max(year[EHLTH==2],na.rm=T)) %>%
  mutate(firstyear_2=ifelse(is.infinite(firstyear_2),NA,firstyear_2),lastyear_2=ifelse(is.infinite(lastyear_2),NA,lastyear_2)) %>%
  mutate(EHLTH=ifelse(firstyear_2<year & year<lastyear_2 & is.na(EHLTH),2,EHLTH)) %>%
  ungroup()

# Get rid of unneeded variables 
AHAmainsurvey <- AHAmainsurvey %>% ungroup() %>%
  select(-firstyear_0, -firstyear_1, -firstyear_2, -lastyear_0, -lastyear_1, -lastyear_2)

AHAmainsurvey <- AHAmainsurvey %>%
  mutate(EHR=ifelse(EHLTH==2,1,ifelse(EHLTH==0 | EHLTH==1,0,NA)))

# In some cases, 2009 or 2015 can be filled in to not have a missing value. 
# Create dataset to fill in 2009 or 2015 conditionally
fill_in <- AHAmainsurvey %>%
  filter((year==2010 & EHR==0) | (year==2014 & EHR==1)) %>%
  mutate(change2009=ifelse(year==2010,1,0),
         change2015=ifelse(year==2014,1,0)) %>%
  select(ID,year,EHR,change2009,change2015) %>%
  distinct(ID,year,EHR,change2009,change2015)

fill_in <- fill_in %>%
  group_by(ID) %>%
  mutate(change2009=ifelse(sum(change2009)>0,1,0),
         change2015=ifelse(sum(change2015)>0,1,0)) %>%
  ungroup() %>%
  distinct(ID,change2009,change2015)

# Merge it back to original
AHAmainsurvey <- AHAmainsurvey %>%
  left_join(fill_in,by="ID")

# Fill in the qualifying missing values
AHAmainsurvey <- AHAmainsurvey %>%
  mutate(EHR=ifelse(year==2009 & change2009==1 & is.na(EHR),0,EHR)) %>%
  mutate(EHR=ifelse(year==2015 & change2015==1 & is.na(EHR),1,EHR)) %>%
  select(-change2009, -change2015) %>%
  ungroup()

AHAmainsurvey <- AHAmainsurvey %>%
  left_join(num_always_missing_EHR,by="ID") %>%
  filter(is.na(always_missing)) %>%
  select(-always_missing)

sample <- AHAmainsurvey %>%
  distinct(HospNPI) %>%
  sample_frac(.02, replace=FALSE) %>%
  mutate(sample=1)


AHA_sample <- AHAmainsurvey %>%
  left_join(sample,by="HospNPI") %>%
  filter(sample==1) %>%
  select(-sample) %>%
  mutate(EHR_partial=ifelse(EHLTH==1 | EHLTH==2,1,0))




panelview(AHA_sample, Y=NULL,D="EHR_partial", index=c("HospNPI","year"), axis.lab="time", by.timing=TRUE) +
  theme(text=element_text(size=15))

ggsave("Objects/hosp_treat.pdf", width=10, height=12, units = "in")





# Histogram of treatment and control for each group -------------------------------------------
neg4 <- Physician_Data %>%
  filter(minyr_EHR==2014 | minyr_EHR==2015) %>%
  distinct(DocNPI, minyr_EHR) %>%
  mutate(treat=ifelse(minyr_EHR==2014,1,0)) %>%
  group_by(treat) %>%
  mutate(count=1) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  distinct(treat,sum) %>%
  mutate(time=-4)

neg3 <- Physician_Data %>%
  filter(minyr_EHR>=2013 & minyr_EHR<=2015) %>%
  distinct(DocNPI, minyr_EHR) %>%
  mutate(treat=ifelse(minyr_EHR==2014 | minyr_EHR==2013,1,0),
         control=ifelse(minyr_EHR>=2014 & minyr_EHR<=2015,1,0)) %>%
  mutate(count=1) %>%
  mutate(sum=ifelse(treat==1,sum(count[treat==1]),NA),
         sum=ifelse(control==1,sum(count[control==1]),sum)) %>%
  ungroup() %>%
  distinct(treat,control, sum) %>%
  filter(!(control==1 & treat==1)) %>%
  distinct(treat,sum) %>%
  mutate(time=-3)

neg2 <- Physician_Data %>%
  filter(minyr_EHR>=2012 & minyr_EHR<=2015) %>%
  distinct(DocNPI, minyr_EHR) %>%
  mutate(treat=ifelse(minyr_EHR>=2012 & minyr_EHR<=2014,1,0),
         control=ifelse(minyr_EHR>=2013 & minyr_EHR<=2015,1,0)) %>%
  mutate(count=1) %>%
  mutate(sum=ifelse(treat==1,sum(count[treat==1]),NA),
         sum=ifelse(control==1,sum(count[control==1]),sum)) %>%
  ungroup() %>%
  distinct(treat,control, sum) %>%
  filter(!(control==1 & treat==1)) %>%
  distinct(treat,sum) %>%
  mutate(time=-2)

neg1 <- Physician_Data %>%
  filter(minyr_EHR>=2011 & minyr_EHR<=2015) %>%
  distinct(DocNPI, minyr_EHR) %>%
  mutate(treat=ifelse(minyr_EHR>=2011 & minyr_EHR<=2014,1,0),
         control=ifelse(minyr_EHR>=2012 & minyr_EHR<=2015,1,0)) %>%
  mutate(count=1) %>%
  mutate(sum=ifelse(treat==1,sum(count[treat==1]),NA),
         sum=ifelse(control==1,sum(count[control==1]),sum)) %>%
  ungroup() %>%
  distinct(treat,control, sum) %>%
  filter(!(control==1 & treat==1)) %>%
  distinct(treat,sum) %>%
  mutate(time=-1)

neg <- Physician_Data %>%
  filter(minyr_EHR>=2010 & minyr_EHR<=2015) %>%
  distinct(DocNPI, minyr_EHR) %>%
  mutate(treat=ifelse(minyr_EHR>=2010 & minyr_EHR<=2014,1,0),
         control=ifelse(minyr_EHR>=2011 & minyr_EHR<=2015,1,0)) %>%
  mutate(count=1) %>%
  mutate(sum=ifelse(treat==1,sum(count[treat==1]),NA),
         sum=ifelse(control==1,sum(count[control==1]),sum)) %>%
  ungroup() %>%
  distinct(treat,control, sum) %>%
  filter(!(control==1 & treat==1)) %>%
  distinct(treat,sum) %>%
  mutate(time=0)

pos1 <- Physician_Data %>%
  filter(minyr_EHR>=2010 & minyr_EHR<=2015) %>%
  distinct(DocNPI, minyr_EHR) %>%
  mutate(treat=ifelse(minyr_EHR>=2010 & minyr_EHR<=2013,1,0),
         control=ifelse(minyr_EHR>=2012 & minyr_EHR<=2015,1,0)) %>%
  mutate(count=1) %>%
  mutate(sum=ifelse(treat==1,sum(count[treat==1]),NA),
         sum=ifelse(control==1,sum(count[control==1]),sum)) %>%
  ungroup() %>%
  distinct(treat,control, sum) %>%
  filter(!(control==1 & treat==1)) %>%
  distinct(treat,sum) %>%
  mutate(time=1)

pos2 <- Physician_Data %>%
  filter(minyr_EHR!=0) %>%
  distinct(DocNPI, minyr_EHR) %>%
  mutate(treat=ifelse(minyr_EHR>=2010 & minyr_EHR<=2012,1,0),
         control=ifelse(minyr_EHR>=2013 & minyr_EHR<=2015,1,0)) %>%
  mutate(count=1) %>%
  mutate(sum=ifelse(treat==1,sum(count[treat==1]),NA),
         sum=ifelse(control==1,sum(count[control==1]),sum)) %>%
  ungroup() %>%
  distinct(treat,control, sum) %>%
  filter(!(control==1 & treat==1)) %>%
  distinct(treat,sum) %>%
  mutate(time=2)

pos3 <- Physician_Data %>%
  filter(minyr_EHR!=2012 & minyr_EHR!=2013) %>%
  distinct(DocNPI, minyr_EHR) %>%
  mutate(treat=ifelse(minyr_EHR==2010 | minyr_EHR==2011,1,0),
         control=ifelse(minyr_EHR==2014 | minyr_EHR==2015,1,0)) %>%
  mutate(count=1) %>%
  mutate(sum=ifelse(treat==1,sum(count[treat==1]),NA),
         sum=ifelse(control==1,sum(count[control==1]),sum)) %>%
  ungroup() %>%
  distinct(treat,control, sum) %>%
  filter(!(control==1 & treat==1)) %>%
  distinct(treat,sum) %>%
  mutate(time=3)

pos4 <- Physician_Data %>%
  filter(minyr_EHR==2010 & minyr_EHR==2015) %>%
  distinct(DocNPI, minyr_EHR) %>%
  mutate(treat=ifelse(minyr_EHR==2010,1,0),
         control=ifelse(minyr_EHR==2015,1,0)) %>%
  mutate(count=1) %>%
  mutate(sum=ifelse(treat==1,sum(count[treat==1]),NA),
         sum=ifelse(control==1,sum(count[control==1]),sum)) %>%
  ungroup() %>%
  distinct(treat,control, sum) %>%
  filter(!(control==1 & treat==1)) %>%
  distinct(treat,sum) %>%
  mutate(time=4)

control_hist_data <- rbind(neg4, neg3, neg2, neg1, neg, pos1, pos2, pos3, pos4) %>%
  mutate(treat=ifelse(treat==1,"Treatment", "Control")) %>%
  dplyr::rename(Group=treat) %>%
  mutate(time=as.factor(time))

ggplot(control_hist_data, aes(x=time, y=sum, fill=Group)) + 
 geom_bar(stat='identity') + theme_bw() +
  theme_light() + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))  +
  ylab("Count of Physicians") + xlab("Event Time")

ggsave(plot=last_plot(), file="/Objects/control_histogram.pdf", height=7, width=10, units="in")


## plot distribution of where retiring physicians who retire are located ############
zipcw <- read_excel(paste0(raw_data_path, "ZIPCodetoZCTACrosswalk2010UDS.xls"))

zipcw <- zipcw %>%
  distinct(ZIP, StateAbbr)

Physician_Data <- Physician_Data %>%
  mutate(phy_zip1=as.character(phy_zip1)) %>%
  left_join(zipcw, by=c("phy_zip1"="ZIP"))

Physician_Data <- Physician_Data %>%
  group_by(DocNPI) %>%
  fill(StateAbbr, .direction="downup") %>%
  ungroup()

retire_data <- Physician_Data %>%
  filter(ever_retire==1) %>%
  distinct(DocNPI, StateAbbr) %>%
  group_by(StateAbbr) %>%
  count() %>%
  ungroup() %>%
  filter(!is.na(StateAbbr)) %>%
  add_row(StateAbbr="CN", n=0) %>%
  add_row(StateAbbr="ME", n=0) %>%
  add_row(StateAbbr="MA", n=0) %>%
  add_row(StateAbbr="NH", n=0) %>%
  add_row(StateAbbr="NJ", n=0) %>%
  add_row(StateAbbr="RI", n=0) %>%
  add_row(StateAbbr="VT", n=0)

observe <- Physician_Data %>%
  filter(ever_retire==1) %>%
  distinct(DocNPI)

sum <- sum(retire_data$n)

ggplot(retire_data, aes(x=StateAbbr, y=n)) + geom_bar(stat = "identity")

# make it into a map
usa <- map_data('usa')
state <- map_data("state")

state <- state %>%
  mutate(fips = fips(region, to="FIPS"))
retire_data <- retire_data %>%
  mutate(fips=fips(StateAbbr, to="FIPS"))

retire_data <- retire_data %>%
  left_join(state, by="fips")

retire_data <- retire_data %>%
  mutate(n_group = cut(n, breaks = c(0, 10, 20, 30, 40, 50, 60, 100, 150))) %>%
  mutate(n_group=as.character(n_group)) %>%
  mutate(n_group = ifelse(is.na(n_group), "0", n_group)) %>%
  mutate(n_group = factor(n_group, levels = c("0", "(0,10]", "(10,20]", "(20,30]", "(30,40]", "(40,50]", "(50,60]", "(100,150]"))) %>%
  dplyr::rename("# Retiring Docs"=n_group)

ggplot(data=retire_data, aes(x=long, y=lat, fill=`# Retiring Docs`, group=group)) + 
  geom_polygon(color = "white") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  coord_fixed(1.3) +
  theme(text=element_text(size=15)) + theme_bw()

ggsave(filename = "Objects/retire_dist_map.pdf", width=8, height=5, units="in")
