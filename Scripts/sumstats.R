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
  summarise_at(c("Number of Hospitals Worked With"="num_hosp_total",
                 "Female"="female", "Number of Systems Worked With"="num_systems",
                 "Years since Graduating"="experience",
                 "Number of Patients"="hosp_count","Fraction of Hospitals with EHR"="frac_EHR",
                 "Average Size of Hospitals Worked With (Beds)"="avg_beds", 
                 "Average Hospital Operating Days"="avg_oper_days",
                 "Fraction of Hospital Patients at EHR Hospital"="frac_EHR_patients"), 
               list(m=mean,sd=sd,min=min,max=max,n=~sum(!is.na(.))), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,n,m,sd,min,max) %>%
  mutate(n=round(n/7))


# AHA EHR Info at the Physician Level (by year) -----------------------------------------------------------------
sum_stats_year <- Physician_Data %>% group_by(year) %>%
  summarise_at(c("Fraction of Hospitals using EHR"="frac_EHR", 
                 "Fraction of Physicians Exposed to an EHR"="anyEHR_exposed"), list(m=mean), na.rm=T) %>%
  dplyr::rename("Hospitals using EHR"="Fraction of Hospitals using EHR_m",
                "Physicians Exposed to an EHR"="Fraction of Physicians Exposed to an EHR_m")


# Create a dataframe out of the summary stats to put in a ggplot
sum_stats_year <- as.data.frame(sum_stats_year)
sum_stats_year <- melt(sum_stats_year, id.vars = "year", measure.vars = c("Hospitals using EHR",
                                                                          "Physicians Exposed to an EHR"))
sum_stats_year <- sum_stats_year %>%
  dplyr::rename("Variable"="variable")

ggplot(sum_stats_year,aes(x=year,y=value,shape=Variable,color=Variable)) + 
  geom_line() +geom_point() + labs(x="\nYear", y="Proportion\n", 
                                   title="\nEHR Use Over Time\n") + 
  scale_colour_manual(values=cbbPalette) + ylim(.2,1)  + theme(legend.key.size=unit(.3,'cm'),legend.key.height = unit(.4, 'cm'),legend.key.width = unit(.3, 'cm'))

ggsave("objects/sum_stats_year.pdf", width=8, height=5, units="in")

# Graph continuous labor variable with lines for each year treated ------------------------------------
# I don't think I want to include this graph in the paper
cont_treatment_graph <- data.frame(year=double(), labor=double(), Treatment=character())
for (i in 2010:2013){
year <- Physician_Data %>%
  filter(working_allyears==1) %>%
  filter(minyr_EHR==i) %>%
  group_by(year) %>%
  mutate(labor=mean(hosp_count)) %>%
  distinct(year,labor) %>%
  mutate(Treatment=paste0("Treatment Year: ",i))

cont_treatment_graph <- rbind(cont_treatment_graph, year)
}

never_treated <- Physician_Data %>%
  filter(working_allyears==1) %>%
  filter(minyr_EHR==0) %>%
  group_by(year) %>%
  mutate(labor=mean(hosp_count)) %>%
  distinct(year,labor) %>%
  mutate(Treatment=paste0("Never Treated"))

cont_treatment_graph <- rbind(cont_treatment_graph, never_treated)

cont_treatment_graph <- cont_treatment_graph %>%
  filter(2009<year & year<2015)

ggplot(cont_treatment_graph, aes(x=year, y=labor, color=Treatment, shape=Treatment)) +geom_point() + geom_line() +
  labs(x="\nYear", y="Avergae Number of Patients in Hospitals\n", 
       title="\nHospital Patient Count by Treatment\n") +
  scale_colour_manual(values=cbbPalette) 
  
ggsave("objects/cont_treatment_graph.pdf", width=8, height=5, units="in")


# Graph indicator labor variable with lines for each year treated ------------------------------------
ind_treatment_graph <- data.frame(year=double(), labor=double(), Treatment=character())
for (i in 2010:2013){
  year <- Physician_Data %>%
    filter(minyr_EHR==i) %>%
    group_by(year) %>%
    mutate(labor=mean(nonhosp_ind)) %>%
    distinct(year,labor) %>%
    mutate(Treatment=paste0("Treatment Year: ",i))
  
  ind_treatment_graph <- rbind(ind_treatment_graph, year)
}

never_treated_ind <- Physician_Data %>%
  filter(minyr_EHR==0) %>%
  group_by(year) %>%
  mutate(labor=mean(nonhosp_ind)) %>%
  distinct(year,labor) %>%
  mutate(Treatment=paste0("Never Treated"))

ind_treatment_graph <- rbind(ind_treatment_graph, never_treated_ind)

ind_treatment_graph <- ind_treatment_graph %>%
  filter(2009<year)

ggplot(ind_treatment_graph, aes(x=year, y=labor, color=Treatment, shape=Treatment)) +geom_point() + geom_line() +
  labs(x="\nYear", y="Hospital Patients\n", 
       title="\nProbability of Positive Patients only Outside of Hospitals\n") +
  scale_colour_manual(values=cbbPalette) 

ggsave("objects/ind_treatment_graph.pdf", width=8, height=5, units="in")




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



