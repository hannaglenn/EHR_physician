library(tidyverse)
library(reshape)
library(knitr)
library(kableExtra)
library(ggplot2)

options(knitr.kable.NA=" ")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



# ------------------  Summary Stats and Figures for Third Year Paper -----------------------
#                     Hanna Glenn, Emory University
#                     8/27/2021

# This script creates preliminary summary stats and figures to explore relationships
# between EHR and physician decisions. Some of these will be used in my third year paper.
# The data used is created in "Final_Pairs_Variables.R"

# Read in Final_Pairs_Variables.rds
Final_Pairs_Variables <- read_rds(paste0(created_data_path,"Final_Pairs_Variables.rds"))

# General Summary Stats Tables: Separate for Physician, Hospital, Pair level ----------------------------------------------------------------

# Physician Level
TYP_sumstats_physician <- Final_Pairs_Variables %>% ungroup() %>%
  distinct(DocNPI,year,.keep_all = T) %>%
  summarise_at(c("Total Services (Part B)"="total_services","Number of Hospitals Worked With"="num_hospitals",
                 "Female"="female", "Number of Systems Worked With"="num_systems",
                 "Concentration Index"="hhi","Years since Graduating"="yrssince_grad"), 
               list(m=mean,sd=sd,min=min,max=max,n=~sum(!is.na(.))), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,n,m,sd,min,max) %>%
  mutate(n=n/7)

knitr::kable(TYP_sumstats_physician, "latex",
             col.names=c("Variable", "Physicians", "Mean", "Std. Dev.", "Min", "Max"),
             digits=2,
             caption="Physician Level Variables",
             booktabs=T,
             escape=F,
             label=NA,
             align=c("l", "c","c","c","c","c"),
             position="h",
             format.args = list(big.mark = ",")) %>%
  kable_styling( full_width=F, latex_options=c("scale_down") ) %>%
  save_kable("objects/sumstats_physician_table.pdf", density=300)

# Hospital Level
TYP_sumstats_hospital <- Final_Pairs_Variables %>% ungroup() %>%
  distinct(HospNPI,year,.keep_all=T) %>%
  summarise_at(c("Beds"="beds","Documentation Index"= "documentation_index","Decision Index"="decision_index",
                 "Uses an EHR"="usesEHR","Days Operating in the Year"="days_hosp_operating",
                 "Recieved Meaningful Use Subsidy"="getsubsidy", "Recieved Subsidy: Stage 1"="stage1"), 
               list(m=mean,sd=sd,min=min,max=max,n=~sum(!is.na(.))), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,n,m,sd,min,max) %>%
  mutate(n=n/7) 

knitr::kable(TYP_sumstats_hospital, "latex",
             col.names=c("Variable", "Hospitals", "Mean", "Std. Dev.", "Min", "Max"),
             digits=2,
             caption="Hospital Level Variables",
             booktabs=T,
             label=NA,
             escape=F,
             align=c("l", "c","c","c","c","c"),
             position="h") %>%
  kable_styling(full_width=F, latex_options="scale_down")  %>%
  save_kable("objects/sumstats_hospital_table.pdf",density=300)

# Pair Level
TYP_sumstats_pair <- Final_Pairs_Variables %>% ungroup() %>%
  summarise_at(c("Shared Patients Same Day"="samedaycount","Percent Shared Patients"="share_samedaycount"), 
               list(m=mean,sd=sd,min=min,max=max,n=~sum(!is.na(.))), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,n,m,sd,min,max) 

knitr::kable(TYP_sumstats_pair, "latex",
             col.names=c("Variable", "N", "Mean", "Std. Dev.", "Min", "Max"),
             digits=2,
             caption="Physician-Hospital Level Variables",
             booktabs=T,
             escape=F,
             label=NA,
             align=c("l", "c","c","c","c","c"),
             position="h",
             format.args = list(big.mark = ",")) %>%
  kable_styling(full_width=F, latex_options="scale_down")  %>%
  save_kable("objects/sumstats_pair_table.pdf",density=300)


# AHA EHR Info at the Hospital Level (by year) -----------------------------------------------------------------
TYP_sumstats_hospEHR_year <- Final_Pairs_Variables %>% ungroup() %>% group_by(year) %>%
  distinct(HospNPI,.keep_all=T) %>%
  summarise_at(c("Uses EHR for Documentation"="usesEHRdoc", "Uses EHR for Decision Making"="usesEHRdec", 
                 "Uses EHR"="usesEHR", 
                 "Recieved Meaningful Use Subsidy"="getsubsidy", "Recieved Subsidy: Stage 1"="stage1"), list(m=mean), na.rm=T) %>%
  dplyr::rename("Uses EHR for Documentation"="Uses EHR for Documentation_m", 
                "Uses EHR for Decision Making"="Uses EHR for Decision Making_m",
                "Uses EHR"="Uses EHR_m")


# Create a dataframe out of the summary stats to put in a ggplot
TYP_plot_hospEHR_year <- as.data.frame(TYP_sumstats_hospEHR_year)
TYP_plot_hospEHR_year <- melt(TYP_plot_hospEHR_year, id.vars = "year", measure.vars = c("Uses full EHR for Documentation","Uses full EHR for Decision Making",
                                                                                    "Uses full EHR"))
TYP_plot_hospEHR_year <- TYP_plot_hospEHR_year %>%
  dplyr::rename("Variable"="variable")

ggplot(TYP_plot_hospEHR_year,aes(x=year,y=value,shape=Variable,color=Variable)) + 
  geom_line() +geom_point() + labs(x="\nYear", y="Percent\n", 
                                   title="\nPercentage Hospitals that Use EHRs (AHA Data)\n") + 
  scale_colour_manual(values=cbbPalette) + ylim(0,1)  + theme(legend.key.size=unit(.3,'cm'),legend.key.height = unit(.4, 'cm'),legend.key.width = unit(.3, 'cm'))

ggsave("objects/TYP_plot_hospEHR_year.pdf", width=8, height=5, units="in")


# Meaningful Use at the Hospital Level (by year) ----------------------------------------------------------------------

# Create summary stats for these for distinct hospitals by year
TYP_sumstats_hospmeanuse_year <- Final_Pairs_Variables %>% ungroup() %>% group_by(year) %>%
  distinct(HospNPI,.keep_all=T) %>%
  summarise_at(c("getsubsidy","stage1","stage2"), list(m=mean,sd=sd,min=min,max=max), na.rm=T)  %>%
  dplyr::rename("Receives any Subsidy"="getsubsidy_m", "Is in Stage 1"="stage1_m", "Is in Stage 2"="stage2_m")

# Create a dataframe out of the summary stats to put in a ggplot
TYP_plot_hospmeanuse_year <- as.data.frame(TYP_sumstats_hospmeanuse_year)
TYP_plot_hospmeanuse_year <- melt(TYP_plot_hospmeanuse_year, id.vars = "year", measure.vars = c("Receives any Subsidy","Is in Stage 1","Is in Stage 2"))

TYP_plot_hospmeanuse_year <- TYP_plot_hospmeanuse_year %>%
  dplyr::rename("Variable"="variable")

ggplot(TYP_plot_hospmeanuse_year,aes(x=year,y=value,shape=Variable,color=Variable)) + 
  geom_line() +geom_point() + labs(x="\nYear", y="Percent\n", 
                                   title="\nPercentage Hospitals With Meaningful Use\n") + 
  scale_colour_manual(values=cbbPalette) + ylim(0,1)  + theme(legend.key.size=unit(.3,'cm'),legend.key.height = unit(.4, 'cm'),legend.key.width = unit(.3, 'cm'))

ggsave("objects/TYP_plot_meanuse_year.pdf", width=8, height=5, units="in")


# Show a graph that represents the spirit of DD (4 lines on graph: mainhosp & smallhosp x mainhospshare & smallhospshare) -----------------------
main_main_data <- Final_Pairs_Variables %>%
  filter(firstyear_mainhosp_usesEHR>0) %>%
  group_by(rel_expandyear_main) %>%
  mutate(avg_share=mean(mainhosp_share)) %>%
  distinct(rel_expandyear_main,avg_share) %>%
  mutate(Category="\nShare at Main Hospital\n when Main Hospital Adopts EHR\n") %>%
  rename(rel_expandyear=rel_expandyear_main)

main_small_data <- Final_Pairs_Variables %>%
  filter(firstyear_mainhosp_usesEHR>0)   %>%
  group_by(rel_expandyear_main) %>%
  mutate(avg_share=mean(smallhosp_share)) %>%
  distinct(rel_expandyear_main,avg_share) %>%
  mutate(Category="Share at Small Hospital\n when Main Hospital Adopts EHR\n") %>%
  rename(rel_expandyear=rel_expandyear_main)

small_small_data <- Final_Pairs_Variables %>%
  filter(firstyear_smallhosp_usesEHR>0) %>%
  group_by(rel_expandyear_small) %>%
  mutate(avg_share=mean(smallhosp_share)) %>%
  distinct(rel_expandyear_small,avg_share) %>%
  mutate(Category="Share at Small Hospital\n when Small Hospital Adopts EHR\n") %>%
  rename(rel_expandyear=rel_expandyear_small)

small_main_data <- Final_Pairs_Variables %>%
  filter(firstyear_smallhosp_usesEHR>0)   %>%
  group_by(rel_expandyear_small) %>%
  mutate(avg_share=mean(mainhosp_share)) %>%
  distinct(rel_expandyear_small,avg_share) %>%
  mutate(Category="Share at Main Hospital\n when Small Hospital Adopts EHR\n") %>%
  rename(rel_expandyear=rel_expandyear_small)

relyear_mainvssmall_data <- rbind(main_main_data, main_small_data, small_main_data, small_small_data)

ggplot(relyear_mainvssmall_data,aes(x=rel_expandyear,y=avg_share,color=Category, shape=Category)) + geom_point() + geom_line() + labs(x="\nYear Relative to EHR Adoption", y="Average Share\n", 
                                                                                                                    title="\nHow Share of Patients Changes with EHR Adoption\n") + 
  scale_colour_manual(values=cbbPalette)  + theme(legend.key.size=unit(.3,'cm'),legend.key.height = unit(.4, 'cm'),legend.key.width = unit(.3, 'cm')) +
  guides(linetype=guide_legend(nrow=8))

ggsave("objects/4linegraph.pdf", width=8, height=5, units="in")

# Create graphs that show averages when main or small hospital never adopts EHR
main_control_data <- Final_Pairs_Variables %>%
  filter(main_neveruses_EHR==1) %>%
  group_by(year) %>%
  mutate(avg_share=mean(mainhosp_share)) %>%
  distinct(year,avg_share) %>%
  mutate(Category="Main Never Adopts EHR\n")

small_control_data <- Final_Pairs_Variables %>%
  filter(small_neveruses_EHR==1) %>%
  group_by(year) %>%
  mutate(avg_share=mean(smallhosp_share)) %>%
  distinct(year,avg_share) %>%
  mutate(Category="Small Never Adopts EHR\n")

share_mainvssmall_control <- rbind(main_control_data,small_control_data)

ggplot(share_mainvssmall_control,aes(x=year,y=avg_share,color=Category, shape=Category)) + geom_point() + geom_line() + labs(x="\nYear", y="Average Share\n", 
                                                                                                                                      title="\nAverage Share in Hospitals that Never Adopt EHRs\n") + 
  scale_colour_manual(values=cbbPalette)  + theme(legend.key.size=unit(.3,'cm'),legend.key.height = unit(.4, 'cm'),legend.key.width = unit(.3, 'cm')) +
  guides(linetype=guide_legend(nrow=8))

ggsave("objects/mainsmall_control.pdf", width=8, height=5, units="in")


# Graph of treatment vs control physician (ever exposed vs. never exposed)-------------------------------------------
EHR_control <- Final_Pairs_Variables %>%
  filter(doc_usesEHR_ever==0) %>%
  group_by(year) %>%
  mutate(avg_share=mean(share_samedaycount)) %>%
  distinct(year,avg_share) %>%
  mutate(Category="Never Exposed to EHR\n")

EHR_treatment_relyear <- Final_Pairs_Variables %>%
  filter(firstyear_usesEHR>0) %>%
  group_by(rel_expandyear_any) %>%
  mutate(avg_share=mean(share_samedaycount)) %>%
  distinct(rel_expandyear_any,avg_share) 


# Relative Year Treatment Plot
ggplot(EHR_treatment_relyear,aes(x=rel_expandyear_any,y=avg_share)) + geom_point() + geom_line() + labs(x="\nYear Relative to EHR Exposure", y="Average Share\n", 
                                                                                                                             title="\nAverage Share in Hospitals that Adopt EHRs\n") + 
  scale_colour_manual(values=cbbPalette)  + theme(legend.key.size=unit(.3,'cm'),legend.key.height = unit(.4, 'cm'),legend.key.width = unit(.3, 'cm')) +
  guides(linetype=guide_legend(nrow=8)) + ylim(0,.75)

ggsave("objects/relyear_treatment.pdf", width=8, height=5, units="in")

# Never Exposed to EHR Plot
ggplot(EHR_control,aes(x=year,y=avg_share,color=Category, shape=Category)) + geom_point() + geom_line() + labs(x="\nYear", y="Average Share\n", 
                                                                                                                                       title="\nAverage Share in Hospitals that Never Adopt EHRs\n") + 
  scale_colour_manual(values=cbbPalette)  + theme(legend.key.size=unit(.3,'cm'),legend.key.height = unit(.4, 'cm'),legend.key.width = unit(.3, 'cm')) +
  guides(linetype=guide_legend(nrow=8)) + ylim(0,1)

ggsave("objects/physician_control.pdf", width=8, height=5, units="in")



# Create graph showing the distributions of important variables---------------------------------------------------------
# Number of hospitals with positive same day count
ggplot(Final_Pairs_Variables, aes(x=num_hospitals_positive)) + geom_histogram(binwidth=1, colour="black", fill="white") +
  xlim(0,30)

# Number of systems
ggplot(Final_Pairs_Variables, aes(x=num_systems)) + geom_histogram(binwidth=1, colour="black", fill="white") +
  xlim(0,30)

# Number of hospitals with EHR (do one plot for each year)
ggplot(Final_Pairs_Variables, aes(x=num_hospitals_EHR,fill=as.character(year))) + geom_histogram(binwidth=1) + 
  facet_grid(cols=vars(year)) + scale_colour_manual(values=cbbPalette) + xlim(0,20)

# Share of hospitals with an EHR
ggplot(Final_Pairs_Variables, aes(x=share_hosp_EHR,fill=as.character(year))) + geom_histogram(binwidth=.1) + 
  facet_grid(cols=vars(year)) + scale_fill_manual(values=cbbPalette) + xlim(0,1) + ylim(0,150000) 

# year of expansion (why is 2009 not showing up?)
ggplot(Final_Pairs_Variables, aes(x=firstyear_usesEHR)) + geom_histogram(binwidth=1) + 
   scale_fill_manual(values=cbbPalette) + xlim(2008,2016) + ylim(0,1000000)


