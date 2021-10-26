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

# Event Study, Full Sample ------------------------------------------------------------
event_fullsample <- felm(nonhosp_ind ~ rel_m6 + rel_m5 + rel_m4 + rel_m3 + rel_m2 + 
                           rel_0 + rel_p1 + rel_p2 + rel_p3 + 
                           rel_p4 + rel_p5 + rel_p6 |DocNPI + year|0|DocNPI,
                         data=Physician_Data)

event_fullsample_coef <- as_tibble(event_fullsample$coefficients, rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(estimate=nonhosp_ind)

event_fullsample_ci <- as_tibble(confint.default(event_fullsample), rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(conf.low='2.5 %', conf.high='97.5 %')

extra_row <- tibble(term="rel_m1", estimate=0, conf.low=0, conf.high=0, rel_year=-1)

event_fullsample_table <- event_fullsample_coef %>%
  left_join(event_fullsample_ci, by="term") %>%
  mutate(rel_year=c(-6,-5,-4,-3,-2,0,1,2,3,4,5,6)) %>%
  bind_rows(extra_row) %>%
  arrange(rel_year) %>%
  mutate(model="All")

event_fullsample_plot <- dwplot(event_fullsample_table, vline=geom_vline(xintercept=0, linetype=2),
                                whisker_args=list(color="black", size=1.1),
                                vars_order =c("rel_p5", "rel_p4","rel_p3","rel_p2","rel_p1",
                                              "rel_0","rel_m1","rel_m2","rel_m3","rel_m4","rel_m5","rel_m6"),
                                dot_args=list(color="black")) + #xlim(-150,150) +
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
                            "rel_m6"="t-6")) + ggtitle("\nEffect of EHR Implementation on Patients Billed in Hospital\n")

event_fullsample_plot
ggsave("Objects/event_fullsample_office_to_hosp.pdf")

# Event Study, Old Doctors
event_oldsample <- felm(nonhosp_ind ~ rel_m6 + rel_m5 + rel_m4 + rel_m3 + rel_m2 + 
                          rel_0 + rel_p1 + rel_p2 + rel_p3 + 
                          rel_p4 + rel_p5 + rel_p6 |DocNPI + year|0|DocNPI,
                        data=filter(Physician_Data,experience>35))

event_oldsample_coef <- as_tibble(event_oldsample$coefficients, rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(estimate=nonhosp_ind)

event_oldsample_ci <- as_tibble(confint.default(event_oldsample), rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(conf.low='2.5 %', conf.high='97.5 %')

extra_row <- tibble(term="rel_m1", estimate=0, conf.low=0, conf.high=0, rel_year=-1)

event_oldsample_table <- event_oldsample_coef %>%
  left_join(event_oldsample_ci, by="term") %>%
  mutate(rel_year=c(-6,-5,-4,-3,-2,0,1,2,3,4,5,6)) %>%
  bind_rows(extra_row) %>%
  arrange(rel_year) %>%
  mutate(model="Experience>35")

event_oldsample_plot <- dwplot(event_oldsample_table, vline=geom_vline(xintercept=0, linetype=2),
                               whisker_args=list(color="black", size=1.1),
                               vars_order =c("rel_p5", "rel_p4","rel_p3","rel_p2","rel_p1",
                                             "rel_0","rel_m1","rel_m2","rel_m3","rel_m4","rel_m5","rel_m6"),
                               dot_args=list(color="black")) + #xlim(-150,150) +
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
                            "rel_m6"="t-6")) + ggtitle("\nEffect of EHR Implementation on Patients Billed in Hospital\n")

event_oldsample_plot
ggsave("Objects/event_oldsample_office_to_hosp.pdf")

# Put both full sample event studies on the same graph
event_joined_table <- bind_rows(event_fullsample_table, event_oldsample_table) 
event_plot <- dwplot(event_joined_table, vline=geom_vline(xintercept=0, linetype=2),
                     whisker_args=list(size=1.1),
                     vars_order =c("rel_p5", "rel_p4","rel_p3","rel_p2","rel_p1",
                                   "rel_0","rel_m1","rel_m2","rel_m3","rel_m4","rel_m5","rel_m6")) +
  coord_flip()  + labs(y="\nEvent Time\n", x="\nEstimate and 95% CI\n") +
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
                            "rel_m6"="t-6")) + ggtitle("\nEffect of EHR Implementation on Probability of Working, \nbut not in Hospitals\n")

event_plot
ggsave("Objects/event_plot_office_to_hosp.pdf")


# Limit to Hospitalists only ---------------------------------------------------------------------------
hospitalist_data <- Physician_Data %>%
  filter(num_hospitals==1)

# Full sample of hospitalists
event_hosp_fullsample <- felm(nonhosp_ind ~ rel_m6 + rel_m5 + rel_m4 + rel_m3 + rel_m2 + 
                                rel_0 + rel_p1 + rel_p2 + rel_p3 + 
                                rel_p4 + rel_p5 + rel_p6 |DocNPI + year|0|DocNPI,
                              data=hospitalist_data)

event_hosp_fullsample_coef <- as_tibble(event_hosp_fullsample$coefficients, rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(estimate=nonhosp_ind)

event_hosp_fullsample_ci <- as_tibble(confint.default(event_hosp_fullsample), rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(conf.low='2.5 %', conf.high='97.5 %')

extra_row <- tibble(term="rel_m1", estimate=0, conf.low=0, conf.high=0, rel_year=-1)

event_hosp_fullsample_table <- event_hosp_fullsample_coef %>%
  left_join(event_hosp_fullsample_ci, by="term") %>%
  mutate(rel_year=c(-6,-5,-4,-3,-2,0,1,2,3,4,5,6)) %>%
  bind_rows(extra_row) %>%
  arrange(rel_year) %>%
  mutate(model="All")

event_hosp_fullsample_plot <- dwplot(event_hosp_fullsample_table, vline=geom_vline(xintercept=0, linetype=2),
                                     whisker_args=list(color="black", size=1.1),
                                     vars_order =c("rel_p5", "rel_p4","rel_p3","rel_p2","rel_p1",
                                                   "rel_0","rel_m1","rel_m2","rel_m3","rel_m4","rel_m5","rel_m6"),
                                     dot_args=list(color="black")) + #xlim(-150,150) +
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
                            "rel_m6"="t-6")) + ggtitle("\nEffect of EHR Implementation on Probability of Working, but not in hospitals\n")

event_hosp_fullsample_plot
ggsave("Objects/event_hosp_fullsample_office_to_hosp.pdf")

# Limit to old hospitalists
event_hosp_oldsample <- felm(nonhosp_ind ~ rel_m6 + rel_m5 + rel_m4 + rel_m3 + rel_m2 + 
                               rel_0 + rel_p1 + rel_p2 + rel_p3 + 
                               rel_p4 + rel_p5 + rel_p6 |DocNPI + year|0|DocNPI,
                             data=filter(hospitalist_data,experience>35))

event_hosp_oldsample_coef <- as_tibble(event_hosp_oldsample$coefficients, rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(estimate=nonhosp_ind)

event_hosp_oldsample_ci <- as_tibble(confint.default(event_hosp_oldsample), rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(conf.low='2.5 %', conf.high='97.5 %')

extra_row <- tibble(term="rel_m1", estimate=0, conf.low=0, conf.high=0, rel_year=-1)

event_hosp_oldsample_table <- event_hosp_oldsample_coef %>%
  left_join(event_hosp_oldsample_ci, by="term") %>%
  mutate(rel_year=c(-6,-5,-4,-3,-2,0,1,2,3,4,5,6)) %>%
  bind_rows(extra_row) %>%
  arrange(rel_year) %>%
  mutate(model="Experience>35")

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
ggsave("Objects/event_hosp_oldsample_office_to_hosp.pdf")

# Put two event studies on one graph
event_joined_hospitalist_table <- bind_rows(event_hosp_fullsample_table, event_hosp_oldsample_table) 
event_hosp_plot <- dwplot(event_joined_hospitalist_table, vline=geom_vline(xintercept=0, linetype=2),
                          whisker_args=list(size=1.1),
                          vars_order =c("rel_p5", "rel_p4","rel_p3","rel_p2","rel_p1",
                                        "rel_0","rel_m1","rel_m2","rel_m3","rel_m4","rel_m5","rel_m6")) +
  coord_flip()  + labs(y="\nEvent Time\n", x="\nEstimate and 95% CI\n") +
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
                            "rel_m6"="t-6")) + ggtitle("\nEffect of EHR Implementation on Probability of Working, \nbut not in hospitals (Hospitalist)\n")

event_hosp_plot
ggsave("Objects/event_hosp_plot_hosp_to_office.pdf")






