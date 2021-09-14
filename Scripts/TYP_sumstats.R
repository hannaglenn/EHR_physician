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
# The data used is created in "ThirdYearPaper.R"

# Read in ThirdYearPaper.rds
ThirdYearPaper <- read_rds(paste0(created_data_path,"ThirdYearPaper.rds"))

# General Summary Stats Tables: Separate for Physician, Hospital, Pair level ----------------------------------------------------------------

# Physician Level
TYP_sumstats_physician <- ThirdYearPaper %>% ungroup() %>%
  summarise_at(c("Total Services (Part B)"="total_services","Number of Hospitals Worked With"="num_hospitals"), 
               list(m=mean,sd=sd,min=min,max=max,n=~sum(!is.na(.))), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,n,m,sd,min,max) 

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
TYP_sumstats_hospital <- ThirdYearPaper %>% ungroup() %>%
  summarise_at(c("Beds"="BDTOT","Documentation Index"= "documentation_index","Decision Index"="decision_index",
                 "Uses any EHR for Documentation"="usesanyEHRdoc", "Uses any EHR for Decision Making"="usesanyEHRdec", 
                 "Uses any EHR"="usesanyEHR", "Uses full EHR for Documentation"="usesfullEHRdoc", 
                 "Uses full EHR for Decision Making"="usesfullEHRdec", "Uses full EHR"="usesfullEHR",
                 "Recieved Meaningful Use Subsidy"="getsubsidy", "Recieved Subsidy: Stage 1"="stage1"), 
               list(m=mean,sd=sd,min=min,max=max,n=~sum(!is.na(.))), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,n,m,sd,min,max) 

knitr::kable(TYP_sumstats_hospital, "latex",
             col.names=c("Variable", "N", "Mean", "Std. Dev.", "Min", "Max"),
             digits=2,
             caption="Summary Statistics",
             booktabs=T,
             escape=F,
             align=c("l", "c","c","c","c","c"),
             position="h",
             format.args = list(big.mark = ",")) %>%
  kable_styling(full_width=F, latex_options="scale_down")  %>%
  save_kable("objects/sumstats_hospital_table.pdf",density=300)

# Pair Level
TYP_sumstats_pair <- ThirdYearPaper %>% ungroup() %>%
  summarise_at(c("Year"="year",
                 "Shared Patients Same Day"="samedaycount"), 
               list(m=mean,sd=sd,min=min,max=max,n=~sum(!is.na(.))), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,n,m,sd,min,max) 

knitr::kable(TYP_sumstats_hospital, "latex",
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
TYP_sumstats_hospEHR_year <- ThirdYearPaper %>% ungroup() %>% group_by(year) %>%
  distinct(HospNPI,.keep_all=T) %>%
  summarise_at(c("Uses any EHR for Documentation"="usesanyEHRdoc", "Uses any EHR for Decision Making"="usesanyEHRdec", 
                 "Uses any EHR"="usesanyEHR", "Uses full EHR for Documentation"="usesfullEHRdoc", 
                 "Uses full EHR for Decision Making"="usesfullEHRdec", "Uses full EHR"="usesfullEHR",
                 "Recieved Meaningful Use Subsidy"="getsubsidy", "Recieved Subsidy: Stage 1"="stage1"), list(m=mean), na.rm=T) %>%
  dplyr::rename("Uses full EHR for Documentation"="Uses full EHR for Documentation_m", 
                "Uses full EHR for Decision Making"="Uses full EHR for Decision Making_m",
                "Uses full EHR"="Uses full EHR_m")


# Create a dataframe out of the summary stats to put in a ggplot
TYP_plot_hospEHR_year <- as.data.frame(TYP_sumstats_hospEHR_year)
TYP_plot_hospEHR_year <- melt(TYP_plot_hospEHR_year, id.vars = "year", measure.vars = c("Uses full EHR for Documentation","Uses full EHR for Decision Making",
                                                                                    "Uses full EHR"))
TYP_plot_hospEHR_year <- TYP_plot_hospEHR_year %>%
  dplyr::rename("Variable"="variable")

hospEHR_year_plot <- ggplot(TYP_plot_hospEHR_year,aes(x=year,y=value,shape=Variable,color=Variable)) + 
  geom_line() +geom_point() + labs(x="\nYear", y="Percent\n", 
                                   title="\nFigure 1: Percentage Hospitals that Use EHRs (AHA Data)\n") + 
  scale_colour_manual(values=cbbPalette) + ylim(0,1)  + theme(legend.key.size=unit(.3,'cm'),legend.key.height = unit(.4, 'cm'),legend.key.width = unit(.3, 'cm'))

ggsave("objects/TYP_plot_hospEHR_year.pdf", width=8, height=5, units="in")


# Meaningful Use at the Hospital Level (by year) ----------------------------------------------------------------------

# Create summary stats for these for distinct hospitals by year
TYP_sumstats_hospmeanuse_year <- ThirdYearPaper %>% ungroup() %>% group_by(year) %>%
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

ggsave("objects/TYP_plot_hospmeanuse_year.pdf", width=8, height=5, units="in")


# Createa graph that shows variation in the dependent variable

ggplot(ThirdYearPaper, aes(x=year,y=perc_sharedpatients)) + geom_point()

# Show a graph that represents the spirit of DD








save(TYP_sumstats_overall,TYP_plot_hospEHR_year,TYP_plot_hospmeanuse_year,file="~/important.TYP.RData")








