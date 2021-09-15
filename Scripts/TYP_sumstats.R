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
  summarise_at(c("Total Services (Part B)"="total_services","Number of Hospitals Worked With"="num_hospitals",
                 "Main Hospital Uses an EHR"="mainhosp_EHR", "Number of Systems Worked With"="num_systems",
                 "Graduation Year"="grad_year", "Female"="female", "Concentration Index"="hhi",
                 "Share at Main Hospital"="mainhosp_share"), 
               list(m=mean,sd=sd,min=min,max=max,n=~sum(!is.na(.))), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,n,m,sd,min,max) %>%
  mutate(m=ifelse(variable=="First Year of Main Hospital Using EHR",NA,m),
         sd=ifelse(variable=="First Year of Main Hospital Using EHR",NA,sd))

knitr::kable(TYP_sumstats_physician, "latex",
             col.names=c("Variable", "N", "Mean", "Std. Dev.", "Min", "Max"),
             digits=2,
             caption="Physician Level Variables",
             booktabs=T,
             escape=F,
             align=c("l", "c","c","c","c","c"),
             position="h",
             format.args = list(big.mark = ",")) %>%
  kable_styling( full_width=F, latex_options=c("scale_down") ) %>%
  save_kable("objects/sumstats_physician_table.pdf", density=300)

# Hospital Level
# Note: divide by 7 to get number of UNIQUE hospitals (divide out years)
TYP_sumstats_hospital <- Final_Pairs_Variables %>% ungroup() %>%
  distinct(HospNPI,year,.keep_all=T) %>%
  summarise_at(c("Beds"="beds","Documentation Index"= "documentation_index","Decision Index"="decision_index",
                 "Uses an EHR for Documentation"="usesEHRdoc", "Uses an EHR for Decision Making"="usesEHRdec", 
                 "Uses an EHR"="usesEHR",  "Days Operating in the Year"="days_hosp_operating",
                 "Recieved Meaningful Use Subsidy"="getsubsidy", "Recieved Subsidy: Stage 1"="stage1"), 
               list(m=mean,sd=sd,min=min,max=max,n=~sum(!is.na(.))), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,n,m,sd,min,max) %>%
  mutate(n=n/7)

knitr::kable(TYP_sumstats_hospital, "latex",
             col.names=c("Variable", "Num Hospitals", "Mean", "Std. Dev.", "Min", "Max"),
             digits=2,
             caption="Hospital Level Variables",
             booktabs=T,
             escape=F,
             align=c("l", "c","c","c","c","c"),
             position="h",
             format.args = list(big.mark = ",")) %>%
  kable_styling(full_width=F, latex_options="scale_down")  %>%
  save_kable("objects/sumstats_hospital_table.pdf",density=300)

# Pair Level
TYP_sumstats_pair <- Final_Pairs_Variables %>% ungroup() %>%
  summarise_at(c("Shared Patients Same Day"="samedaycount", "Percent of Shared Patients"="share_samedaycount"), 
               list(m=mean,sd=sd,min=min,max=max,n=~sum(!is.na(.))), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,n,m,sd,min,max) 

knitr::kable(TYP_sumstats_pair, "latex",
             col.names=c("Variable", "N", "Mean", "Std. Dev.", "Min", "Max"),
             digits=2,
             caption="Summary Statistics",
             booktabs=T,
             escape=F,
             align=c("l", "c","c","c","c","c"),
             position="h",
             format.args = list(big.mark = ",")) %>%
  kable_styling(full_width=F, latex_options="scale_down")  %>%
  save_kable("objects/sumstats_pair_table.pdf",density=300)


# AHA EHR Info at the Hospital Level (by year) ----------------------------------------
TYP_sumstats_hospEHR_year <- Final_Pairs_Variables %>% ungroup() %>% group_by(year) %>%
  distinct(HospNPI,.keep_all=T) %>%
  summarise_at(c("Uses an EHR for Documentation"="usesEHRdoc", "Uses an EHR for Decision Making"="usesEHRdec", 
                 "Uses an EHR"="usesEHR"), list(m=mean), na.rm=T) %>%
  dplyr::rename("Uses an EHR for Documentation"="Uses an EHR for Documentation_m", 
                "Uses an EHR for Decision Making"="Uses an EHR for Decision Making_m",
                "Uses an EHR"="Uses an EHR_m")


# Create a dataframe out of the summary stats to put in a ggplot
TYP_plot_hospEHR_year <- as.data.frame(TYP_sumstats_hospEHR_year)
TYP_plot_hospEHR_year <- melt(TYP_plot_hospEHR_year, id.vars = "year", measure.vars = c("Uses an EHR for Documentation","Uses an EHR for Decision Making",
                                                                                    "Uses an EHR"))
TYP_plot_hospEHR_year <- TYP_plot_hospEHR_year %>%
  dplyr::rename("Variable"="variable")

hospEHR_year_plot <- ggplot(TYP_plot_hospEHR_year,aes(x=year,y=value,shape=Variable,color=Variable)) + 
  geom_line() +geom_point() + labs(x="\nYear", y="Percent\n", 
                                   title="\nFigure 1: Percentage Hospitals that Use EHRs (AHA Data)\n") + 
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

hospmeanuse_year_plot <- ggplot(TYP_plot_hospmeanuse_year,aes(x=year,y=value,shape=Variable,color=Variable)) + 
  geom_line() +geom_point() + labs(x="\nYear", y="Percent\n", 
                                   title="\nFigure 1: Percentage Hospitals With Meaningful Use\n") + 
  scale_colour_manual(values=cbbPalette) + ylim(0,1)  + theme(legend.key.size=unit(.3,'cm'),legend.key.height = unit(.4, 'cm'),legend.key.width = unit(.3, 'cm'))

ggsave("objects/TYP_plot_meanuse_year.pdf", width=8, height=5, units="in")




# Show a graph that represents the spirit of DD (this one uses average HHI) ------------------------
hhi_control_data <- Final_Pairs_Variables %>%
  filter(firstyear_mainhosp_usesEHR==0) %>%
  group_by(year) %>%
  mutate(avg_hhi=mean(hhi)) %>%
  distinct(year,avg_hhi) %>%
  mutate(Category="Control")

hhi_treatment_data <- Final_Pairs_Variables %>%
  filter(firstyear_mainhosp_usesEHR>0)   %>%
  group_by(year) %>%
  mutate(avg_hhi=mean(hhi)) %>%
  distinct(year,avg_hhi) %>%
  mutate(Category="Treatment")

avg_hhi_data <- rbind(hhi_control_data, hhi_treatment_data)

ggplot(avg_hhi_data,aes(x=year,y=avg_hhi,color=Category, shape=Category)) + geom_point() +geom_line() + labs(x="\nYear", y="Average Concentration Index\n", 
                                                                                                            title="\nFigure 2: Change in Concentration Over Time: Treatment and Control\n") + 
  scale_colour_manual(values=cbbPalette)  + theme(legend.key.size=unit(.3,'cm'),legend.key.height = unit(.4, 'cm'),legend.key.width = unit(.3, 'cm'))

ggsave("objects/hhi_treatvscontrol.pdf", width=8, height=5, units="in")



# Show a graph that represents the spirit of DD (this one uses main hospital share) -----------------------
main_control_data <- Final_Pairs_Variables %>%
  filter(firstyear_mainhosp_usesEHR==0) %>%
  group_by(year) %>%
  mutate(avg_mainhosp_share=mean(mainhosp_share)) %>%
  distinct(year,avg_mainhosp_share) %>%
  mutate(category="Control")

main_treatment_data <- Final_Pairs_Variables %>%
  filter(firstyear_mainhosp_usesEHR>0)   %>%
  group_by(year) %>%
  mutate(avg_mainhosp_share=mean(mainhosp_share)) %>%
  distinct(year,avg_mainhosp_share) %>%
  mutate(category="Treatment")

mainhosp_avg_share_data <- rbind(main_control_data, main_treatment_data)

ggplot(mainhosp_avg_share_data,aes(x=year,y=avg_mainhosp_share,color=category)) + geom_point() + geom_line() + labs(x="\nYear", y="Average Share at Main Hospital\n", 
                                                                                                            title="\nFigure 3: Change in Main Hospital Share Over Time: Treatment and Control\n") + 
  scale_colour_manual(values=cbbPalette)  + theme(legend.key.size=unit(.3,'cm'),legend.key.height = unit(.4, 'cm'),legend.key.width = unit(.3, 'cm'))

ggsave("objects/mainhosp_share_treatvscontrol.pdf", width=8, height=5, units="in")

 # Create a spirit of DD graph that uses total_services
year_limited_data <- Final_Pairs_Variables %>%
  filter(year>2011) %>%
  filter(firstyear_mainhosp_usesEHR>2012 | firstyear_mainhosp_usesEHR==0)

services_control_data <- year_limited_data %>%
  filter(firstyear_mainhosp_usesEHR==2015) %>%
  group_by(year) %>%
  mutate(avg_services=mean(total_services, na.rm=T)) %>%
  distinct(year,avg_services) %>%
  mutate(category="Control")

services_treatment_data <- year_limited_data %>%
  filter(firstyear_mainhosp_usesEHR>00) %>%
  group_by(year) %>%
  mutate(avg_services=mean(total_services, na.rm=T)) %>%
  distinct(year,avg_services) %>%
  mutate(category="Treatment")

services_avg_data <- rbind(services_control_data, services_treatment_data)

ggplot(services_avg_data,aes(x=year,y=avg_services,color=category)) + geom_point() + geom_line() + labs(x="\nYear", y="Average Total Services\n", 
                                                                                                                    title="\nFigure 3: Change in Main Hospital Share Over Time: Treatment and Control\n") + 
  scale_colour_manual(values=cbbPalette)  + theme(legend.key.size=unit(.3,'cm'),legend.key.height = unit(.4, 'cm'),legend.key.width = unit(.3, 'cm'))

ggsave("objects/mainhosp_share_treatvscontrol.pdf", width=8, height=5, units="in")




