library(tidyverse)
library(reshape)
library(knitr)
library(kableExtra)
library(ggplot2)
library(magick)
library(webshot)

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
                 "Age"="age",
                 "Number of Patients"="claim_count_total","Fraction of Hospitals with EHR"="frac_EHR",
                 "Exposure to an EHR"="anyEHR_exposed",
                 "Exposure to an EHR (Low Integration)"="anyEHR_LI_exposed",
                 "Fraction Patients in Office"="pos_office",
                 "Ever Retire"="ever_retire",
                 "Work in an Office"="work_in_office",
                 "Change Zip Codes"="change_zip"), 
               list(m=mean,sd=sd,min=min,max=max,n=~sum(!is.na(.))), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,n,m,sd,min,max) 




knitr::kable(sum_stats_fullsample[c(3,8,12,2,10,4,5,7,1,6,9,11),],
             format="latex",
             table.envir="table",
             col.names=c("Variable","N","Mean","Std. Dev.", "Min", "Max"),
             digits=2,
             caption="Summary Statistics",
             booktabs=TRUE,
             escape=F,
             align=c("l","r","r","r","r","r"),
             position="h") %>%
  kable_styling(full_width=F) %>%
  pack_rows(index = c("Outcomes" = 5, "Treatment" = 3, "Characteristics" = 4))


# Summary Stats broken down by age of physician and year ---------------------------------------------------------
means_old <- Physician_Data %>% 
  filter(max_age>45) %>% 
  group_by(year) %>%
  summarize_at(c("hosp_count",
                 "frac_EHR",
                 "anyEHR_exposed"), list(mean,sd), na.rm=TRUE) 
  filter(year >= 1986) %>%
  select(year,hosp_count_fn1,hosp_count_fn2,frac_EHR_fn1,frac_EHR_fn2,anyEHR_exposed_fn1,anyEHR_exposed_fn2) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.)) 

means_young <- Physician_Data %>% filter(experience<=35) %>% ungroup() %>%
  group_by(year) %>%
  summarize_at(c("hosp_count",
                 "frac_EHR",
                 "anyEHR_exposed"), list(mean,sd), na.rm=TRUE) %>%
  filter(year >= 1986) %>%
  select(year,hosp_count_fn1,hosp_count_fn2,frac_EHR_fn1,frac_EHR_fn2,anyEHR_exposed_fn1,anyEHR_exposed_fn2) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.)) %>%
  select(-year)

means_bind <- cbind(means_old,means_young)

knitr::kable(means_bind, "latex",
             col.names=c("Year","Mean","Std. Dev.", "Mean","Std. Dev.", "Mean", "Std. Dev." ,"Mean","Std. Dev.",
                         "Mean","Std. Dev.","Mean","Std. Dev."),
             digits=2,
             caption="Summary Stats",
             booktabs=TRUE,
             escape=F,
             align=c("l","c","c","c","c","c","c","c","c","c","c","c","c"),
             position="h") %>%
  kable_styling(full_width=F) %>%
  add_header_above(c(" "=1, "Number of Patients"=2, "Frac. of Hospitals with EHR" =2, "Exposure to EHR"=2,
                     "Number of Patients"=2, "Frac. of Hospitals with EHR" =2, "Exposure to EHR"=2)) %>%
  add_header_above(c(" "=1, "Senior Physicians"=6, "Young Physicians"=6))


# AHA EHR Info at the Physician Level (by year) -----------------------------------------------------------------
sum_stats_year <- Physician_Data %>% group_by(year) %>%
  summarise_at(c("Fraction of Hospitals using EHR"="frac_EHR_constant", 
                 "Fraction of Physicians Exposed to an EHR"="anyEHR_exposed"), list(m=mean), na.rm=T) %>%
  dplyr::rename("Fraction of Hospitals using EHR"="Fraction of Hospitals using EHR_m",
                "Physicians Exposed to an EHR"="Fraction of Physicians Exposed to an EHR_m")


# Create a dataframe out of the summary stats to put in a ggplot
sum_stats_year <- as.data.frame(sum_stats_year)
sum_stats_year <- melt(sum_stats_year, id.vars = "year", measure.vars = c("Fraction of Hospitals using EHR",
                                                                          "Physicians Exposed to an EHR"))
sum_stats_year <- sum_stats_year %>%
  dplyr::rename("Variable"="variable")

ggplot(sum_stats_year,aes(x=year,y=value,shape=Variable,color=Variable)) + 
  geom_line() +geom_point() + labs(x="\nYear", y=" " 
                                   ) + 
  scale_colour_manual(values=cbbPalette) + ylim(.2,1)  + theme(legend.key.size=unit(.3,'cm'),legend.key.height = unit(.4, 'cm'),legend.key.width = unit(.3, 'cm'))

ggsave("Objects/sum_stats_year.pdf", width=8, height=5, units="in")

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



