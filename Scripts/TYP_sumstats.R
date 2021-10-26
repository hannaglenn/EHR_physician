library(tidyverse)
library(reshape)
library(knitr)
library(kableExtra)
library(ggplot2)

options(knitr.kable.NA=" ")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



# ------------------  Summary Stats and Figures for Third Year Paper -----------------------
#                     Hanna Glenn, Emory University
#                     10/8/2021

# This script creates preliminary summary stats and figures to explore relationships
# between EHR and physician decisions. Some of these will be used in my third year paper.
# The data used is created in "Final_Pairs.R"

# Read in Final_Pairs_Variables.rds
Physician_Data <- read_rds(paste0(created_data_path,"Physician_data.rds"))

# General Summary Stats Tables:  ----------------------------------------------------------------

# Physician Level
sum_stats_fullsample <- Physician_Data %>% ungroup() %>%
  summarise_at(c("Number of Hospitals Worked With"="num_hospitals",
                 "Female"="female", "Number of Systems Worked With"="num_systems",
                 "Years since Graduating"="experience",
                 "Total Patients Billed with Hospitals"="phys_working_hosp","Fraction of Hospitals with EHR"="frac_EHR",
                 "Average Size of Hospitals Worked With (Beds)"="avg_beds", 
                 "Average Hospital Operating Days"="avg_oper_days",
                 "Fraction of Hospital Patients at EHR Hospital"="frac_EHR_patients"), 
               list(m=mean,sd=sd,min=min,max=max,n=~sum(!is.na(.))), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,n,m,sd,min,max)

# Hospitalist Level
sum_stats_hospitalist <- Physician_Data %>% ungroup() %>% filter(num_hospitals==1) %>%
  summarise_at(c("Number of Hospitals Worked With"="num_hospitals",
                 "Female"="female", "Number of Systems Worked With"="num_systems",
                 "Years since Graduating"="experience",
                 "Total Patients Billed with Hospitals"="phys_working_hosp","Fraction of Hospitals with EHR"="frac_EHR",
                 "Average Size of Hospitals Worked With (Beds)"="avg_beds", 
                 "Average Hospital Operating Days"="avg_oper_days",
                 "Fraction of Hospital Patients at EHR Hospital"="frac_EHR_patients"), 
               list(m=mean,sd=sd,min=min,max=max,n=~sum(!is.na(.))), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,n,m,sd,min,max)

# Merge the two summary stats tables
sum_stats <- sum_stats_fullsample %>%
  left_join(sum_stats_hospitalist, by="variable")

knitr::kable(sum_stats, "latex",
             col.names=c("Variable", "Number Physicians", "Mean", "Std. Dev.", "Min", "Max","Number Physicians", "Mean", "Std. Dev.", "Min", "Max"),
             digits=2,
             caption="Physician Level Variables",
             booktabs=T,
             escape=F,
             label=NA,
             align=c("l", "c","c","c","c","c","c","c","c","c","c"),
             position="h",
             linesep = "\\addlinespace",
             format.args = list(big.mark = ",")) %>%
  kable_styling( full_width=F, latex_options=c("scale_down") ) %>%
  add_header_above(c(" "=1, "Full Sample"=5, "Works with Only 1 Hospital" =5)) %>%
  save_kable("objects/sumstats.pdf", density=300)



# AHA EHR Info at the Physician Level (by year) -----------------------------------------------------------------
sum_stats_year <- Physician_Data %>% group_by(year) %>%
  summarise_at(c("Fraction of Physicians Working"="working_ind", "Fraction of Hospitals using EHR"="frac_EHR", 
                 "Fraction of Physicians Exposed to an EHR"="exposed"), list(m=mean), na.rm=T) %>%
  dplyr::rename("Fraction of Physicians Working"="Fraction of Physicians Working_m", 
                "Fraction of Hospitals using EHR"="Fraction of Hospitals using EHR_m",
                "Fraction of Physicians Exposed to an EHR"="Fraction of Physicians Exposed to an EHR_m")


# Create a dataframe out of the summary stats to put in a ggplot
sum_stats_year <- as.data.frame(sum_stats_year)
sum_stats_year <- melt(sum_stats_year, id.vars = "year", measure.vars = c("Fraction of Hospitals \nusing EHR",
                                                                          "Fraction of Physicians \nExposed to an EHR"))
sum_stats_year <- sum_stats_year %>%
  dplyr::rename("Variable"="variable")

ggplot(sum_stats_year,aes(x=year,y=value,shape=Variable,color=Variable)) + 
  geom_line() +geom_point() + labs(x="\nYear", y="Percent\n", 
                                   title="\nEHR use Over Time\n") + 
  scale_colour_manual(values=cbbPalette) + ylim(.2,1)  + theme(legend.key.size=unit(.3,'cm'),legend.key.height = unit(.4, 'cm'),legend.key.width = unit(.3, 'cm'))

ggsave("objects/sum_stats_year.pdf", width=8, height=5, units="in")

# Graph of treatment physician with continuous labor variable-------------------------------------------

EHR_treatment_cont <- Physician_Data %>%
  filter(minyr_EHR>0) %>%
  group_by(rel_exposedyr) %>%
  mutate(labor=mean(phys_working)) %>%
  distinct(rel_exposedyr,labor) 

# Relative Year Treatment Plot
ggplot(EHR_treatment_cont,aes(x=rel_exposedyr,y=labor)) + geom_point() + geom_line() + labs(x="\nYear Relative to EHR Exposure", y="Labor Variable\n", 
                                                                                                                             title="\nPhysician Labor in Year Relative to EHR Exposure\n") + 
  scale_colour_manual(values=cbbPalette)  + theme(legend.key.size=unit(.3,'cm'),legend.key.height = unit(.4, 'cm'),legend.key.width = unit(.3, 'cm')) +
  guides(linetype=guide_legend(nrow=8)) 

ggsave("objects/relyear_treatment_cont.pdf", width=8, height=5, units="in")


# Graph of treatment physician with indicator labor variable-------------------------------------------
EHR_treatment_ind <- Physician_Data %>%
  filter(minyr_EHR>0) %>%
  group_by(rel_exposedyr) %>%
  mutate(labor=mean(working_ind)) %>%
  distinct(rel_exposedyr,labor) 

# Relative Year Treatment Plot
ggplot(EHR_treatment_ind,aes(x=rel_exposedyr,y=labor)) + geom_point() + geom_line() + labs(x="\nYear Relative to EHR Exposure", y="Labor Variable\n", 
                                                                                            title="\nPhysician Labor in Year Relative to EHR Exposure\n") + 
  scale_colour_manual(values=cbbPalette)  + theme(legend.key.size=unit(.3,'cm'),legend.key.height = unit(.4, 'cm'),legend.key.width = unit(.3, 'cm')) +
  guides(linetype=guide_legend(nrow=8)) 

ggsave("objects/relyear_treatment_ind.pdf", width=8, height=5, units="in")


# Create graph showing the distributions of important variables---------------------------------------------------------
# Patients billed with hospitals
ggplot(filter(Physician_Data,working_allyears_hosp==1), aes(x=phys_working_hosp)) + geom_histogram(binwidth=1000, colour="black", fill="white") 

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



