library(tidyverse)
library(readr)
library(plm)
library(lfe)
library(dotwhisker)

# --------------------    EHR-Physician Analysis  --------------------------
#                         Hanna Glenn, Emory University
#                         10/11/2021

# This Script reads in the data for my third year paper, "Physician_Data.rds" and uses it to analyze the relationship
# between EHR adoption and physician labor market decisions. 

# Event Studies for Entire Sample of Doctors ------------------------------------------------------
# Event Study for Continuous Working Variable
event_reg <- felm(phys_working ~ rel_m6 + rel_m5 + rel_m4 + rel_m3 + rel_m2 + rel_0 + rel_p1 + rel_p2 + rel_p3 + 
                  rel_p4 + rel_p5 + rel_p6 + experience + female + avg_beds + avg_oper_days | year,
                  data=Physician_Data)

event_reg_coef <- as_tibble(event_reg$coefficients, rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(estimate=phys_working)

event_reg_ci <- as_tibble(confint(event_reg), rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(conf.low='2.5 %', conf.high='97.5 %')

extra_row <- tibble(term="rel_m1", estimate=0, conf.low=0, conf.high=0, rel_year=-1)

event_plot_table <- event_reg_coef %>%
  left_join(event_reg_ci, by="term") %>%
  mutate(rel_year=c(-6,-5,-4,-3,-2,0,1,2,3,4,5,6)) %>%
  bind_rows(extra_row) %>%
  arrange(rel_year)

event_plot <- dwplot(event_plot_table, vline=geom_vline(xintercept=0, linetype=2),
                     order_vars=c("rel_p6", "rel_p5", "rel_p4", "rel_p3", "rel_p2", "rel_p1", "rel_0",
                                  "rel_m1", "rel_m2", "rel_m3", "rel_m4", "rel_m5", "rel_m6"),
                     whisker_args=list(color="black", size=1.1),
                     dot_args=list(color="black")) +
  coord_flip() + theme_bw() + theme(legend.position="none") + labs(y="Year", x="Estimate and 95% CI") +
  scale_y_discrete(labels=c("rel_p6"="t+6",
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
                            "rel_m6"="t-6"))
event_plot


# Event Study for Indicator Working Variable
event_reg_ind <- felm(working_ind ~ rel_m6 + rel_m5 + rel_m4 + rel_m3 + rel_m2 + rel_0 + rel_p1 + rel_p2 + rel_p3 + 
                    rel_p4 + rel_p5 + rel_p6 + experience + female + avg_beds + avg_oper_days | year,
                  data=Physician_Data)

event_reg_coef_ind <- as_tibble(event_reg_ind$coefficients, rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(estimate=working_ind)

event_reg_ci_ind <- as_tibble(confint(event_reg_ind), rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(conf.low='2.5 %', conf.high='97.5 %')

event_plot_ind_table <- event_reg_coef_ind %>%
  left_join(event_reg_ci_ind, by="term") %>%
  mutate(rel_year=c(-6,-5,-4,-3,-2,0,1,2,3,4,5,6)) %>%
  bind_rows(extra_row) %>%
  arrange(rel_year)

event_plot_ind <- dwplot(event_plot_ind_table, vline=geom_vline(xintercept=0, linetype=2),
                     order_vars=c("rel_p6", "rel_p5", "rel_p4", "rel_p3", "rel_p2", "rel_p1", "rel_0",
                                  "rel_m1", "rel_m2", "rel_m3", "rel_m4", "rel_m5", "rel_m6"),
                     whisker_args=list(color="black", size=1.1),
                     dot_args=list(color="black")) +
  coord_flip() + theme_bw() + theme(legend.position="none") + labs(y="Year", x="Estimate and 95% CI") +
  scale_y_discrete(labels=c("rel_p6"="t+6",
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
                            "rel_m6"="t-6"))
event_plot_ind

# Repeat Event Studies for Old Doctors ----------------------------------------------------------------
# Event Study for Continuous Working Variable (Old Doctors)
event_reg_old <- felm(phys_working ~ rel_m6 + rel_m5 + rel_m4 + rel_m3 + rel_m2 + rel_0 + rel_p1 + rel_p2 + rel_p3 + 
                    rel_p4 + rel_p5 + rel_p6 + experience + female + avg_beds + avg_oper_days | year,
                  data=filter(Physician_Data, experience>=40))

event_reg_coef_old <- as_tibble(event_reg_old$coefficients, rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(estimate=phys_working)

event_reg_ci_old <- as_tibble(confint(event_reg_old), rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(conf.low='2.5 %', conf.high='97.5 %')

extra_row <- tibble(term="rel_m1", estimate=0, conf.low=0, conf.high=0, rel_year=-1)

event_plot_table_old <- event_reg_coef_old %>%
  left_join(event_reg_ci_old, by="term") %>%
  mutate(rel_year=c(-6,-5,-4,-3,-2,0,1,2,3,4,5,6)) %>%
  bind_rows(extra_row) %>%
  arrange(rel_year)

event_plot_old <- dwplot(event_plot_table_old, vline=geom_vline(xintercept=0, linetype=2),
                     order_vars=c("rel_p6", "rel_p5", "rel_p4", "rel_p3", "rel_p2", "rel_p1", "rel_0",
                                  "rel_m1", "rel_m2", "rel_m3", "rel_m4", "rel_m5", "rel_m6"),
                     whisker_args=list(color="black", size=1.1),
                     dot_args=list(color="black")) +
  coord_flip() + theme_bw() + theme(legend.position="none") + labs(y="Year", x="Estimate and 95% CI") +
  scale_y_discrete(labels=c("rel_p6"="t+6",
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
                            "rel_m6"="t-6"))
event_plot_old


# Event Study for Indicator Working Variable (Old Doctors)
event_reg_ind_old <- felm(working_ind ~ rel_m6 + rel_m5 + rel_m4 + rel_m3 + rel_m2 + rel_0 + rel_p1 + rel_p2 + rel_p3 + 
                        rel_p4 + rel_p5 + rel_p6 + experience + female + avg_beds + avg_oper_days | year,
                      data=filter(Physician_Data, experience>=40))

event_reg_coef_ind_old <- as_tibble(event_reg_ind_old$coefficients, rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(estimate=working_ind)

event_reg_ci_ind_old<- as_tibble(confint(event_reg_ind_old), rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(conf.low='2.5 %', conf.high='97.5 %')

event_plot_ind_table_old <- event_reg_coef_ind_old %>%
  left_join(event_reg_ci_ind_old, by="term") %>%
  mutate(rel_year=c(-6,-5,-4,-3,-2,0,1,2,3,4,5,6)) %>%
  bind_rows(extra_row) %>%
  arrange(rel_year)

event_plot_ind_old <- dwplot(event_plot_ind_table_old, vline=geom_vline(xintercept=0, linetype=2),
                         order_vars=c("rel_p6", "rel_p5", "rel_p4", "rel_p3", "rel_p2", "rel_p1", "rel_0",
                                      "rel_m1", "rel_m2", "rel_m3", "rel_m4", "rel_m5", "rel_m6"),
                         whisker_args=list(color="black", size=1.1),
                         dot_args=list(color="black")) +
  coord_flip() + theme_bw() + theme(legend.position="none") + labs(y="Year", x="Estimate and 95% CI") +
  scale_y_discrete(labels=c("rel_p6"="t+6",
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
                            "rel_m6"="t-6"))
event_plot_ind_old



