library(tidyverse)
library(readr)
library(plm)
library(lfe)
library(dotwhisker)
library(stargazer)
library(estimatr)
library(stats)
library(did)

# --------------------    EHR-Physician Analysis  --------------------------
#                         Hanna Glenn, Emory University
#                         10/25/2021

# This Script reads in the data for my third year paper, "Physician_Data.rds" and uses it to analyze the relationship
# between EHR adoption and physician decisions at the intensive margin.

Physician_Data <- read_rds(paste0(created_data_path,"Physician_Data.rds"))


# Limit to Hospitalists only ---------------------------------------------------------------------------
hospitalist_data <- Physician_Data %>%
  filter(num_hospitals==1)

# Full sample of hospitalists
event_hosp_fullsample <- felm(phys_working_hosp ~ rel_m6 + rel_m5 + rel_m4 + rel_m3 + rel_m2 + 
                                rel_0 + rel_p1 + rel_p2 + rel_p3 + 
                                rel_p4 + rel_p5 + rel_p6 + avg_beds + avg_oper_days |DocNPI + year|0|DocNPI,
                              data=filter(hospitalist_data,working_allyears_hosp==1))

event_hosp_fullsample_coef <- as_tibble(event_hosp_fullsample$coefficients, rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(estimate=phys_working_hosp)

event_hosp_fullsample_ci <- as_tibble(confint.default(event_hosp_fullsample), rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(conf.low='2.5 %', conf.high='97.5 %')

extra_row <- tibble(term="rel_m1", estimate=0, conf.low=0, conf.high=0, rel_year=-1)

event_hosp_fullsample_table <- event_hosp_fullsample_coef %>%
  left_join(event_hosp_fullsample_ci, by="term") %>%
  mutate(rel_year=c(-6,-5,-4,-3,-2,0,1,2,3,4,5,6)) %>%
  bind_rows(extra_row) %>%
  arrange(rel_year)

event_hosp_fullsample_plot <- dwplot(event_hosp_fullsample_table, vline=geom_vline(xintercept=0, linetype=2),
                     whisker_args=list(color="black", size=1.1),
                     vars_order =c("rel_p5", "rel_p4","rel_p3","rel_p2","rel_p1",
                                   "rel_0","rel_m1","rel_m2","rel_m3","rel_m4","rel_m5","rel_m6"),
                     dot_args=list(color="black")) + xlim(-150,150) +
  coord_flip() + theme_bw() + theme(legend.position="none") + labs(y="\nEvent Time\n", x="\nEstimate and 95% CI\n") +
  scale_y_discrete(labels=c("rel_p5"="t+5",
                            "rel_p4"="t+4",
                            "rel_p3"="t+3",
                            "rel_p2"="t+2",
                            "rel_p1"="t+1",
                            "rel_0"="0",
                            "rel_m1"="t-1",
                            "rel_m2"="t-2",
                            "rel_m3"="t-3",
                            "rel_m4"="t-4",
                            "rel_m5"="t-5",
                            "rel_m6"="t-6")) + ggtitle("\nEffect of EHR Implementation on Patients Billed in Hospital (Only Hospitalists)\n")
 
event_hosp_fullsample_plot
ggsave("Objects/event_hosp_fullsample.pdf")


# Limit to old hospitalists
event_hosp_oldsample <- felm(phys_working_hosp ~ rel_m6 + rel_m5 + rel_m4 + rel_m3 + rel_m2 + 
                                rel_0 + rel_p1 + rel_p2 + rel_p3 + 
                                rel_p4 + rel_p5 + rel_p6 + avg_beds + avg_oper_days |DocNPI + year|0|DocNPI,
                              data=filter(hospitalist_data,working_allyears_hosp==1 & experience>35))

event_hosp_oldsample_coef <- as_tibble(event_hosp_oldsample$coefficients, rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(estimate=phys_working_hosp)

event_hosp_oldsample_ci <- as_tibble(confint.default(event_hosp_oldsample), rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(conf.low='2.5 %', conf.high='97.5 %')

extra_row <- tibble(term="rel_m1", estimate=0, conf.low=0, conf.high=0, rel_year=-1)

event_hosp_oldsample_table <- event_hosp_oldsample_coef %>%
  left_join(event_hosp_oldsample_ci, by="term") %>%
  mutate(rel_year=c(-6,-5,-4,-3,-2,0,1,2,3,4,5,6)) %>%
  bind_rows(extra_row) %>%
  arrange(rel_year)

event_hosp_oldsample_plot <- dwplot(event_hosp_oldsample_table, vline=geom_vline(xintercept=0, linetype=2),
                                     whisker_args=list(color="black", size=1.1),
                                     vars_order =c("rel_p5", "rel_p4","rel_p3","rel_p2","rel_p1",
                                                   "rel_0","rel_m1","rel_m2","rel_m3","rel_m4","rel_m5","rel_m6"),
                                     dot_args=list(color="black")) +
  coord_flip() + theme_bw() + theme(legend.position="none") + labs(y="\nEvent Time\n", x="\nEstimate and 95% CI\n") +
  scale_y_discrete(labels=c(
    "rel_p5"="t+5",
    "rel_p4"="t+4",
    "rel_p3"="t+3",
    "rel_p2"="t+2",
    "rel_p1"="t+1",
    "rel_0"="0",
    "rel_m1"="t-1",
    "rel_m2"="t-2",
    "rel_m3"="t-3",
    "rel_m4"="t-4",
    "rel_m5"="t-5",
    "rel_m6"="t-6")) + ggtitle("\nEffect of EHR Implementation on Patients Billed in Hospital (Only Senior Hospitalists)\n") 
  

event_hosp_oldsample_plot
ggsave("Objects/event_hosp_oldsample.pdf")

# Limit to doctors who work in multiple hospitals Only -------------------------------------------------------------
nonhospitalist_data <- Physician_Data %>%
  filter(num_hospitals>1)

# Full sample of doctors who work in multiple hospitals
event_nonhosp_fullsample <- felm(phys_working_hosp ~ rel_m6 + rel_m5 + rel_m4 + rel_m3 + rel_m2 + 
                                rel_0 + rel_p1 + rel_p2 + rel_p3 + 
                                rel_p4 + rel_p5 + rel_p6 + avg_beds + avg_oper_days |DocNPI + year|0|DocNPI,
                              data=filter(nonhospitalist_data,working_allyears_hosp==1))

event_nonhosp_fullsample_coef <- as_tibble(event_nonhosp_fullsample$coefficients, rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(estimate=phys_working_hosp)

event_nonhosp_fullsample_ci <- as_tibble(confint.default(event_nonhosp_fullsample), rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(conf.low='2.5 %', conf.high='97.5 %')

extra_row <- tibble(term="rel_m1", estimate=0, conf.low=0, conf.high=0, rel_year=-1)

event_nonhosp_fullsample_table <- event_nonhosp_fullsample_coef %>%
  left_join(event_nonhosp_fullsample_ci, by="term") %>%
  mutate(rel_year=c(-6,-5,-4,-3,-2,0,1,2,3,4,5,6)) %>%
  bind_rows(extra_row) %>%
  arrange(rel_year)

event_nonhosp_fullsample_plot <- dwplot(event_nonhosp_fullsample_table, vline=geom_vline(xintercept=0, linetype=2),
                                     whisker_args=list(color="black", size=1.1),
                                     vars_order =c("rel_p5", "rel_p4","rel_p3","rel_p2","rel_p1",
                                                   "rel_0","rel_m1","rel_m2","rel_m3","rel_m4","rel_m5","rel_m6"),
                                     dot_args=list(color="black")) + xlim(-150,150) +
  coord_flip() + theme_bw() + theme(legend.position="none") + labs(y="\nEvent Time\n", x="\nEstimate and 95% CI\n") +
  scale_y_discrete(labels=c(
    "rel_p5"="t+5",
    "rel_p4"="t+4",
    "rel_p3"="t+3",
    "rel_p2"="t+2",
    "rel_p1"="t+1",
    "rel_0"="0",
    "rel_m1"="t-1",
    "rel_m2"="t-2",
    "rel_m3"="t-3",
    "rel_m4"="t-4",
    "rel_m5"="t-5",
    "rel_m6"="t-6")) + ggtitle("\nEffect of EHR Implementation on Patients Billed in Hospital (Work in Multiple Hospitals)\n")
event_nonhosp_fullsample_plot
ggsave("Objects/event_nonhosp_fullsample.pdf")


# Sample of old doctors who work in multiple hospitals
event_nonhosp_oldsample <- felm(phys_working_hosp ~ rel_m6 + rel_m5 + rel_m4 + rel_m3 + rel_m2 + 
                                   rel_0 + rel_p1 + rel_p2 + rel_p3 + 
                                   rel_p4 + rel_p5 + rel_p6 + avg_beds + avg_oper_days |DocNPI + year|0|DocNPI,
                                 data=filter(nonhospitalist_data,working_allyears_hosp==1 & experience>35))

event_nonhosp_oldsample_coef <- as_tibble(event_nonhosp_oldsample$coefficients, rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(estimate=phys_working_hosp)

event_nonhosp_oldsample_ci <- as_tibble(confint.default(event_nonhosp_oldsample), rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(conf.low='2.5 %', conf.high='97.5 %')

extra_row <- tibble(term="rel_m1", estimate=0, conf.low=0, conf.high=0, rel_year=-1)

event_nonhosp_oldsample_table <- event_nonhosp_oldsample_coef %>%
  left_join(event_nonhosp_oldsample_ci, by="term") %>%
  mutate(rel_year=c(-6,-5,-4,-3,-2,0,1,2,3,4,5,6)) %>%
  bind_rows(extra_row) %>%
  arrange(rel_year)

event_nonhosp_oldsample_plot <- dwplot(event_nonhosp_oldsample_table, vline=geom_vline(xintercept=0, linetype=2),
                                        whisker_args=list(color="black", size=1.1),
                                        vars_order =c("rel_p5", "rel_p4","rel_p3","rel_p2","rel_p1",
                                                      "rel_0","rel_m1","rel_m2","rel_m3","rel_m4","rel_m5","rel_m6"),
                                        dot_args=list(color="black")) +
  coord_flip() + theme_bw() + theme(legend.position="none") + labs(y="\nEvent Time\n", x="\nEstimate and 95% CI\n") +
  scale_y_discrete(labels=c(
    "rel_p5"="t+5",
    "rel_p4"="t+4",
    "rel_p3"="t+3",
    "rel_p2"="t+2",
    "rel_p1"="t+1",
    "rel_0"="0",
    "rel_m1"="t-1",
    "rel_m2"="t-2",
    "rel_m3"="t-3",
    "rel_m4"="t-4",
    "rel_m5"="t-5",
    "rel_m6"="t-6")) + ggtitle("\nEffect of EHR Implementation on Patients Billed in Hospital (Multiple Hospitals, Senior Doctors)\n")
event_nonhosp_oldsample_plot
ggsave("Objects/event_nonhosp_oldsample.pdf")
