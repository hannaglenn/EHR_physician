library(tidyverse)
library(readr)
library(plm)
library(lfe)
library(dotwhisker)
library(stargazer)
library(estimatr)

# --------------------    EHR-Physician Analysis  --------------------------
#                         Hanna Glenn, Emory University
#                         10/11/2021

# This Script reads in the data for my third year paper, "Physician_Data.rds" and uses it to analyze the relationship
# between EHR adoption and physician labor market decisions.

# Read in data
Physician_Data <- read_rds(paste0(created_data_path,"Physician_Data.rds"))

# Event Studies for Entire Sample of Doctors ----------------------------------------------------------------
# Continuous Number of Patients Variable
event_reg <- felm(phys_working ~ rel_m6 + rel_m5 + rel_m4 + rel_m3 + rel_m2 + rel_0 + rel_p1 + rel_p2 + rel_p3 + 
                  rel_p4 + rel_p5 + rel_p6 + female + experience + avg_beds + avg_oper_days |year,
                  data=Physician_Data)

event_reg_coef <- as_tibble(event_reg$coefficients, rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(estimate=phys_working)

event_reg_ci <- as_tibble(confint.default(event_reg), rownames="term") %>%
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
                     whisker_args=list(color="black", size=1.1),
                     vars_order =c("rel_p6", "rel_p5", "rel_p4","rel_p3","rel_p2","rel_p1","rel_0","rel_m1","rel_m2","rel_m3","rel_m4","rel_m5","rel_m6"),
                     dot_args=list(color="black")) +
  coord_flip() + theme_bw() + theme(legend.position="none") + labs(y="\nEvent Time\n", x="\nEstimate and 95% CI\n") +
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
                            "rel_m6"="t-6")) + ggtitle("\nDependent Variable: Total Patients\n")
event_plot
ggsave("Objects/cont_ES_fullsample.pdf")


# Event Study for Indicator Working Variable
event_reg_ind <- felm(working_ind ~ rel_m6 + rel_m5 + rel_m4 + rel_m3 + rel_m2 + rel_0 + rel_p1 + rel_p2 + rel_p3 + 
                    rel_p4 + rel_p5 + rel_p6 + experience + female + avg_beds + avg_oper_days | year,
                  data=Physician_Data)

event_reg_coef_ind <- as_tibble(event_reg_ind$coefficients, rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(estimate=working_ind)

event_reg_ci_ind <- as_tibble(confint.default(event_reg_ind), rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(conf.low='2.5 %', conf.high='97.5 %')

event_plot_ind_table <- event_reg_coef_ind %>%
  left_join(event_reg_ci_ind, by="term") %>%
  mutate(rel_year=c(-6,-5,-4,-3,-2,0,1,2,3,4,5,6)) %>%
  bind_rows(extra_row) %>%
  arrange(rel_year)

event_plot_ind <- dwplot(event_plot_ind_table, vline=geom_vline(xintercept=0, linetype=2),
                     vars_order=c("rel_p6", "rel_p5", "rel_p4", "rel_p3", "rel_p2", "rel_p1", "rel_0",
                                  "rel_m1", "rel_m2", "rel_m3", "rel_m4", "rel_m5", "rel_m6"),
                     whisker_args=list(color="black", size=1.1),
                     dot_args=list(color="black")) +
  coord_flip() + theme_bw() + theme(legend.position="none") + labs(y="\nEvent Time\n", x="\nEstimate and 95% CI\n") +
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
                            "rel_m6"="t-6")) + ggtitle("\nDependent Variable: Indicator for Working\n")
event_plot_ind
ggsave("Objects/ind_ES_fullsample.pdf")

# Repeat Event Studies for Senior Doctors ----------------------------------------------------------------
# Event Study for Continuous Working Variable (Old Doctors)
event_reg_old <- felm(phys_working ~ rel_m6 + rel_m5 + rel_m4 + rel_m3 + rel_m2 + rel_0 + rel_p1 + rel_p2 + rel_p3 + 
                    rel_p4 + rel_p5 + rel_p6 + female + avg_beds + avg_oper_days | year,
                  data=filter(Physician_Data, experience>=40))

event_reg_coef_old <- as_tibble(event_reg_old$coefficients, rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(estimate=phys_working)

event_reg_ci_old <- as_tibble(confint.default(event_reg_old), rownames="term") %>%
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
                     vars_order =c("rel_p6", "rel_p5", "rel_p4", "rel_p3", "rel_p2", "rel_p1", "rel_0",
                                  "rel_m1", "rel_m2", "rel_m3", "rel_m4", "rel_m5", "rel_m6"),
                     whisker_args=list(color="black", size=1.1),
                     dot_args=list(color="black")) +
  coord_flip() + theme_bw() + theme(legend.position="none") + labs(y="\nEvent Time\n", x="\nEstimate and 95% CI\n") +
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
                            "rel_m6"="t-6")) + ggtitle("\nDependent Variable: Total Patients (Senior Physician Sample)\n")
event_plot_old
ggsave("Objects/cont_ES_oldsample.pdf")

# Event Study for Indicator Working Variable (Old Doctors)
event_reg_ind_old <- felm(working_ind ~ rel_m6 + rel_m5 + rel_m4 + rel_m3 + rel_m2 + rel_0 + rel_p1 + rel_p2 + rel_p3 + 
                        rel_p4 + rel_p5 + rel_p6 + female + avg_beds + avg_oper_days | year,
                      data=filter(Physician_Data, experience>=40))

event_reg_coef_ind_old <- as_tibble(event_reg_ind_old$coefficients, rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(estimate=working_ind)

event_reg_ci_ind_old<- as_tibble(confint.default(event_reg_ind_old), rownames="term") %>%
  filter(term %in% c("rel_m6", "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5", "rel_p6")) %>%
  dplyr::rename(conf.low='2.5 %', conf.high='97.5 %')

event_plot_ind_table_old <- event_reg_coef_ind_old %>%
  left_join(event_reg_ci_ind_old, by="term") %>%
  mutate(rel_year=c(-6,-5,-4,-3,-2,0,1,2,3,4,5,6)) %>%
  bind_rows(extra_row) %>%
  arrange(rel_year)

event_plot_ind_old <- dwplot(event_plot_ind_table_old, vline=geom_vline(xintercept=0, linetype=2),
                         vars_order=c("rel_p6", "rel_p5", "rel_p4", "rel_p3", "rel_p2", "rel_p1", "rel_0",
                                      "rel_m1", "rel_m2", "rel_m3", "rel_m4", "rel_m5", "rel_m6"),
                         whisker_args=list(color="black", size=1.1),
                         dot_args=list(color="black")) +
  coord_flip() + theme_bw() + theme(legend.position="none") + labs(y="\nEvent Time\n", x="\nEstimate and 95% CI\n") +
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
                            "rel_m6"="t-6")) + ggtitle("\nDependent Variable: Indicator for Working (Senior Physician Sample)\n")
event_plot_ind_old
ggsave("Objects/ind_ES_oldsample.pdf")


# Event Studies for Sample of Doctors excluding 2009 treatment ------------------------------------------------------
event_reg_subset <- felm(phys_working ~ rel_m5 + rel_m4 + rel_m3 + rel_m2 + rel_0 + rel_p1 + rel_p2 + rel_p3 + 
                    rel_p4 + rel_p5 + female + experience + avg_beds + avg_oper_days |year,
                  data=filter(Physician_Data,minyr_EHR>2009))

event_reg_coef_subset <- as_tibble(event_reg_subset$coefficients, rownames="term") %>%
  filter(term %in% c("rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5")) %>%
  dplyr::rename(estimate=phys_working)

event_reg_ci_subset <- as_tibble(confint.default(event_reg_subset), rownames="term") %>%
  filter(term %in% c("rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5")) %>%
  dplyr::rename(conf.low='2.5 %', conf.high='97.5 %')

extra_row <- tibble(term="rel_m1", estimate=0, conf.low=0, conf.high=0, rel_year=-1)

event_plot_table_subset <- event_reg_coef_subset %>%
  left_join(event_reg_ci_subset, by="term") %>%
  mutate(rel_year=c(-5,-4,-3,-2,0,1,2,3,4,5)) %>%
  bind_rows(extra_row) %>%
  arrange(rel_year)

event_plot_subset <- dwplot(event_plot_table_subset, vline=geom_vline(xintercept=0, linetype=2),
                     whisker_args=list(color="black", size=1.1),
                     vars_order =c("rel_p5", "rel_p4","rel_p3","rel_p2","rel_p1","rel_0","rel_m1","rel_m2","rel_m3","rel_m4","rel_m5"),
                     dot_args=list(color="black")) +
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
                            "rel_m5"="t-5")) + ggtitle("\nDependent Variable: Total Patients (Exclude treated in 2009)\n")
event_plot_subset
ggsave("Objects/cont_ES_subset2009.pdf")


# Event Study for Indicator Working Variable
event_reg_ind_subset <- felm(working_ind ~ rel_m5 + rel_m4 + rel_m3 + rel_m2 + rel_0 + rel_p1 + rel_p2 + rel_p3 + 
                        rel_p4 + rel_p5 + experience + female + avg_beds + avg_oper_days | year,
                      data=filter(Physician_Data, minyr_EHR>2009))

event_reg_coef_ind_subset <- as_tibble(event_reg_ind_subset$coefficients, rownames="term") %>%
  filter(term %in% c("rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5")) %>%
  dplyr::rename(estimate=working_ind)

event_reg_ci_ind_subset <- as_tibble(confint.default(event_reg_ind_subset), rownames="term") %>%
  filter(term %in% c( "rel_m5", "rel_m4", "rel_m3", "rel_m2", "rel_0", "rel_p1",
                     "rel_p2", "rel_p3", "rel_p4", "rel_p5")) %>%
  dplyr::rename(conf.low='2.5 %', conf.high='97.5 %')

event_plot_ind_table_subset <- event_reg_coef_ind_subset %>%
  left_join(event_reg_ci_ind_subset, by="term") %>%
  mutate(rel_year=c(-5,-4,-3,-2,0,1,2,3,4,5)) %>%
  bind_rows(extra_row) %>%
  arrange(rel_year)

event_plot_ind <- dwplot(event_plot_ind_table_subset, vline=geom_vline(xintercept=0, linetype=2),
                         vars_order=c("rel_p5", "rel_p4", "rel_p3", "rel_p2", "rel_p1", "rel_0",
                                      "rel_m1", "rel_m2", "rel_m3", "rel_m4", "rel_m5"),
                         whisker_args=list(color="black", size=1.1),
                         dot_args=list(color="black")) +
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
                            "rel_m5"="t-5")) + ggtitle("\nDependent Variable: Indicator for Working (Exclude treated in 2009)\n")
event_plot_ind
ggsave("Objects/ind_ES_subset2009.pdf")







# Look more closely at what is happening each year -------------------------------------------

# Treated in 2010 ----------------------------------------------------------------------------
# Look at some graphs (comparing treated in 2010 to never treated or treated after 2010)
ggplot(filter(Physician_Data, working_allyears==1 & (minyr_EHR==0 | minyr_EHR>2009)), aes(year,phys_working,color=factor(exposed_2010))) +
  stat_summary(geom='line') + geom_vline(xintercept=2010) 
ggplot(filter(Physician_Data, minyr_EHR==0 | minyr_EHR>2009), aes(year,working_ind,color=factor(exposed_2010))) +
  stat_summary(geom='line') + geom_vline(xintercept=2010)
  # Neither of these graphs point to anything substantial happening in 2010. For the working variable, the lines stay
  # parallel for all of the years. For the working indicator, the lines dont break until 2015. 

# Look at some graphs(comparing treated in 2010 to never treated only)
ggplot(filter(Physician_Data, working_allyears==1 & (minyr_EHR==0 | minyr_EHR==2010)), aes(year,phys_working,color=factor(exposed_2010))) +
  stat_summary(geom='line') + geom_vline(xintercept=2010) 
ggplot(filter(Physician_Data, minyr_EHR==0 | minyr_EHR==2010), aes(year,working_ind,color=factor(exposed_2010))) +
  stat_summary(geom='line') + geom_vline(xintercept=2010)
# Neither of these graphs point to anything substantial happening in 2010. For the working variable, the lines stay
# parallel for all of the years. The working indicator seems to break a little bit after 2010

# DiD with comparison as never treated or treated after 2010
cont_dd_2010 <- lm(phys_working ~ exposed_2010 + post_2010 + exposed_2010*post_2010 + experience + female + 
                     avg_beds + avg_oper_days, data=filter(Physician_Data, working_allyears==1 & (minyr_EHR==0 | minyr_EHR>2009)))
summary(cont_dd_2010)

ind_dd_2010 <- lm(working_ind ~ exposed_2010 + post_2010 + exposed_2010*post_2010 + experience + female + 
                    avg_beds + avg_oper_days, data=filter(Physician_Data, minyr_EHR==0 | minyr_EHR>2009))
summary(ind_dd_2010)

# DiD with comparison as never treated or treated after 2010
cont_dd_2010_never <- lm(phys_working ~ exposed_2010 + post_2010 + exposed_2010*post_2010 + experience + female + 
                     avg_beds + avg_oper_days, data=filter(Physician_Data, working_allyears==1 & (minyr_EHR==0 | minyr_EHR==2010)))
summary(cont_dd_2010_never)

ind_dd_2010_never <- lm(working_ind ~ exposed_2010 + post_2010 + exposed_2010*post_2010 + experience + female + 
                    avg_beds + avg_oper_days, data=filter(Physician_Data, minyr_EHR==0 | minyr_EHR==2010))
summary(ind_dd_2010_never)

# Did with compairson as never treated, only for older physicians
cont_dd_2010_never_old <- lm(phys_working ~ exposed_2010 + post_2010 + exposed_2010*post_2010 + experience + female + 
                           avg_beds + avg_oper_days, data=filter(Physician_Data, experience>=35 & working_allyears==1 & (minyr_EHR==0 | minyr_EHR==2010)))
summary(cont_dd_2010_never_old)

ind_dd_2010_never_old <- lm(working_ind ~ exposed_2010 + post_2010 + exposed_2010*post_2010 + experience + female + 
                          avg_beds + avg_oper_days, data=filter(Physician_Data, experience>=35 & (minyr_EHR==0 | minyr_EHR==2010)))
summary(ind_dd_2010_never_old)

# Treated in 2011 ----------------------------------------------------------------------------
# Look at some graphs (comparing treated 2011 to never treated or treated after 2011)
ggplot(filter(Physician_Data, working_allyears==1 & (minyr_EHR==0 | minyr_EHR>2010)), aes(year,phys_working,color=factor(exposed_2011))) +
  stat_summary(geom='line') + geom_vline(xintercept=2011) 
ggplot(filter(Physician_Data, minyr_EHR==0 | minyr_EHR>2010), aes(year,working_ind,color=factor(exposed_2011))) +
  stat_summary(geom='line') + geom_vline(xintercept=2011)
# Neither of these graphs point to anything substantial happening in 2011. For the working variable, the lines stay
# parallel for all of the years. For the working indicator, never exposed line is slightly steeper after 2011 

# Look at some graphs (comparing treated 2011 to never treated only)
ggplot(filter(Physician_Data,working_allyears==1 & (minyr_EHR==0 | minyr_EHR==2011)), aes(year,phys_working,color=factor(exposed_2011))) +
  stat_summary(geom='line') + geom_vline(xintercept=2011) 
ggplot(filter(Physician_Data, minyr_EHR==0 | minyr_EHR==2011), aes(year,working_ind,color=factor(exposed_2011))) +
  stat_summary(geom='line') + geom_vline(xintercept=2011)
  # Similar story with these graphs

# DiD (comparison group is never treated or treated after 2011)
cont_dd_2011 <- lm(phys_working ~ exposed_2011 + post_2011 + exposed_2011*post_2011 + experience + female + 
                     avg_beds + avg_oper_days, data=filter(Physician_Data, working_allyears==1 & (minyr_EHR==0 | minyr_EHR>2010)))
summary(cont_dd_2011)

ind_dd_2011 <- lm(working_ind ~ exposed_2011 + post_2011 + exposed_2011*post_2011 + experience + female + 
                    avg_beds + avg_oper_days, data=filter(Physician_Data, minyr_EHR==0 | minyr_EHR>2010))
summary(ind_dd_2011)

# DiD (comparison group is never treated only)
cont_dd_2011_never <- lm(phys_working ~ exposed_2011 + post_2011 + exposed_2011*post_2011 + experience + female + 
                     avg_beds + avg_oper_days, data=filter(Physician_Data, working_allyears==1 & (minyr_EHR==0 | minyr_EHR==2011)))
summary(cont_dd_2011_never)

ind_dd_2011_never <- lm(working_ind ~ exposed_2011 + post_2011 + exposed_2011*post_2011 + experience + female + 
                    avg_beds + avg_oper_days, data=filter(Physician_Data, minyr_EHR==0 | minyr_EHR==2011))
summary(ind_dd_2011_never)

# DiD with compairson as never treated, only for older physicians
cont_dd_2011_never_old <- lm(phys_working ~ exposed_2011 + post_2011 + exposed_2011*post_2011 + experience + female + 
                               avg_beds + avg_oper_days, data=filter(Physician_Data, experience>=35 & working_allyears==1 & (minyr_EHR==0 | minyr_EHR==2011)))
summary(cont_dd_2011_never_old)

ind_dd_2011_never_old <- lm(working_ind ~ exposed_2011 + post_2011 + exposed_2011*post_2011 + experience + female + 
                              avg_beds + avg_oper_days, data=filter(Physician_Data, experience>=35 & (minyr_EHR==0 | minyr_EHR==2011)))
summary(ind_dd_2011_never_old)

# Treated in 2012 ----------------------------------------------------------------------------
# Look at some graphs (comparing treated 2011 to never treated or treated after 2011)
ggplot(filter(Physician_Data, working_allyears==1 & (minyr_EHR==0 | minyr_EHR>2011)), aes(year,phys_working,color=factor(exposed_2012))) +
  stat_summary(geom='line') + geom_vline(xintercept=2012) 
ggplot(filter(Physician_Data, minyr_EHR==0 | minyr_EHR>2011), aes(year,working_ind,color=factor(exposed_2012))) +
  stat_summary(geom='line') + geom_vline(xintercept=2012)
# Neither of these graphs point to anything substantial happening in 2012. 

# Look at some graphs (comparing treated 2011 to never treated only)
ggplot(filter(Physician_Data, working_allyears==1 & (minyr_EHR==0 | minyr_EHR==2012)), aes(year,phys_working,color=factor(exposed_2012))) +
  stat_summary(geom='line') + geom_vline(xintercept=2012) 
ggplot(filter(Physician_Data, minyr_EHR==0 | minyr_EHR==2012), aes(year,working_ind,color=factor(exposed_2012))) +
  stat_summary(geom='line') + geom_vline(xintercept=2012)
# Similar story with these graphs (indicator may show something)

# DiD (comparison group is never treated or treated after 2011)
cont_dd_2012 <- lm(phys_working ~ exposed_2012 + post_2012 + exposed_2012*post_2012 + experience + female + 
                     avg_beds + avg_oper_days, data=filter(Physician_Data, working_allyears==1 & (minyr_EHR==0 | minyr_EHR>2011)))
summary(cont_dd_2012)

ind_dd_2012 <- lm(working_ind ~ exposed_2012 + post_2012+ exposed_2012*post_2012 + experience + female + 
                    avg_beds + avg_oper_days, data=filter(Physician_Data, minyr_EHR==0 | minyr_EHR>2011))
summary(ind_dd_2012)

# DiD (comparison group is never treated only)
cont_dd_2012_never <- lm(phys_working ~ exposed_2012 + post_2012 + exposed_2012*post_2012 + experience + female + 
                           avg_beds + avg_oper_days, data=filter(Physician_Data, working_allyears==1 & (minyr_EHR==0 | minyr_EHR==2012)))
summary(cont_dd_2012_never)

ind_dd_2012_never <- lm(working_ind ~ exposed_2012 + post_2012+ exposed_2012*post_2012 + experience + female + 
                          avg_beds + avg_oper_days, data=filter(Physician_Data, minyr_EHR==0 | minyr_EHR==2012))
summary(ind_dd_2012_never)
  # Second indicator DD is the only one that shows something happening

# Did with compairson as never treated, only for older physicians
cont_dd_2012_never_old <- lm(phys_working ~ exposed_2012 + post_2012 + exposed_2012*post_2012 + experience + female + 
                               avg_beds + avg_oper_days, data=filter(Physician_Data, experience>=35 & working_allyears==1 & (minyr_EHR==0 | minyr_EHR==2012)))
summary(cont_dd_2012_never_old)

ind_dd_2012_never_old <- lm(working_ind ~ exposed_2012 + post_2012 + exposed_2012*post_2012 + experience + female + 
                              avg_beds + avg_oper_days, data=filter(Physician_Data, experience>=35 & (minyr_EHR==0 | minyr_EHR==2012)))
summary(ind_dd_2012_never_old)



# Treated in 2013 ----------------------------------------------------------------------------
# Look at some graphs (comparing treated 2013 to never treated or treated after 2013)
ggplot(filter(Physician_Data, working_allyears==1 & (minyr_EHR==0 | minyr_EHR>2012)), aes(year,phys_working,color=factor(exposed_2013))) +
  stat_summary(geom='line') + geom_vline(xintercept=2013) 
ggplot(filter(Physician_Data, minyr_EHR==0 | minyr_EHR>2012), aes(year,working_ind,color=factor(exposed_2013))) +
  stat_summary(geom='line') + geom_vline(xintercept=2013)

# Look at some graphs (comparing treated 2011 to never treated only)
ggplot(filter(Physician_Data, working_allyears==1 & (minyr_EHR==0 | minyr_EHR==2013)), aes(year,phys_working,color=factor(exposed_2013))) +
  stat_summary(geom='line') + geom_vline(xintercept=2013) 
ggplot(filter(Physician_Data, minyr_EHR==0 | minyr_EHR==2013), aes(year,working_ind,color=factor(exposed_2013))) +
  stat_summary(geom='line') + geom_vline(xintercept=2013)

# DiD (comparison group is never treated or treated after 2011)
cont_dd_2013 <- lm(phys_working ~ exposed_2013 + post_2013 + exposed_2013*post_2013 + experience + female + 
                     avg_beds + avg_oper_days, data=filter(Physician_Data, working_allyears==1 & (minyr_EHR==0 | minyr_EHR>2012)))
summary(cont_dd_2013)

ind_dd_2013 <- lm(working_ind ~ exposed_2013 + post_2013+ exposed_2013*post_2013 + experience + female + 
                    avg_beds + avg_oper_days, data=filter(Physician_Data, minyr_EHR==0 | minyr_EHR>2012))
summary(ind_dd_2013)

# DiD (comparison group is never treated only)
cont_dd_2013_never <- lm(phys_working ~ exposed_2013 + post_2013 + exposed_2013*post_2013 + experience + female + 
                           avg_beds + avg_oper_days, data=filter(Physician_Data, working_allyears==1 & (minyr_EHR==0 | minyr_EHR==2013)))
summary(cont_dd_2013_never)

ind_dd_2013_never <- lm(working_ind ~ exposed_2013 + post_2013+ exposed_2013*post_2013 + experience + female + 
                          avg_beds + avg_oper_days, data=filter(Physician_Data, minyr_EHR==0 | minyr_EHR==2013))
summary(ind_dd_2013_never)

# Did with compairson as never treated, only for older physicians
cont_dd_2013_never_old <- lm(phys_working ~ exposed_2013 + post_2013 + exposed_2013*post_2013 + experience + female + 
                               avg_beds + avg_oper_days, data=filter(Physician_Data, experience>=37 & working_allyears==1 & (minyr_EHR==0 | minyr_EHR==2013)))
summary(cont_dd_2013_never_old)

ind_dd_2013_never_old <- lm(working_ind ~ exposed_2013 + post_2013 + exposed_2013*post_2013 + experience + female + 
                              avg_beds + avg_oper_days, data=filter(Physician_Data, experience>=37 & (minyr_EHR==0 | minyr_EHR==2013)))
summary(ind_dd_2013_never_old)



# Treated in 2014 ----------------------------------------------------------------------------
# Look at some graphs (comparing treated 2014 to never treated or treated after 2014)
ggplot(filter(Physician_Data, working_allyears==1 & (minyr_EHR==0 | minyr_EHR>2013)), aes(year,phys_working,color=factor(exposed_2014))) +
  stat_summary(geom='line') + geom_vline(xintercept=2014) 
ggplot(filter(Physician_Data, minyr_EHR==0 | minyr_EHR>2013), aes(year,working_ind,color=factor(exposed_2014))) +
  stat_summary(geom='line') + geom_vline(xintercept=2014)

# Look at some graphs (comparing treated 2011 to never treated only)
ggplot(filter(Physician_Data, working_allyears==1 & (minyr_EHR==0 | minyr_EHR==2014)), aes(year,phys_working,color=factor(exposed_2014))) +
  stat_summary(geom='line') + geom_vline(xintercept=2014) 
ggplot(filter(Physician_Data, minyr_EHR==0 | minyr_EHR==2014), aes(year,working_ind,color=factor(exposed_2014))) +
  stat_summary(geom='line') + geom_vline(xintercept=2014)

# DiD (comparison group is never treated or treated after 2011)
cont_dd_2014 <- lm(phys_working ~ exposed_2014 + post_2014 + exposed_2014*post_2014 + experience + female + 
                     avg_beds + avg_oper_days, data=filter(Physician_Data, working_allyears==1 & (minyr_EHR==0 | minyr_EHR>2013)))
summary(cont_dd_2014)

ind_dd_2014 <- lm(working_ind ~ exposed_2014 + post_2014+ exposed_2014*post_2014 + experience + female + 
                    avg_beds + avg_oper_days, data=filter(Physician_Data, minyr_EHR==0 | minyr_EHR>2013))
summary(ind_dd_2014)

# DiD (comparison group is never treated only)
cont_dd_2014_never <- lm(phys_working ~ exposed_2014 + post_2014 + exposed_2014*post_2014 + experience + female + 
                           avg_beds + avg_oper_days, data=filter(Physician_Data, working_allyears==1 & (minyr_EHR==0 | minyr_EHR==2014)))
summary(cont_dd_2014_never)

ind_dd_2014_never <- lm(working_ind ~ exposed_2014 + post_2014+ exposed_2014*post_2014 + experience + female + 
                          avg_beds + avg_oper_days, data=filter(Physician_Data, minyr_EHR==0 | minyr_EHR==2014))
summary(ind_dd_2014_never)

# Did with compairson as never treated, only for older physicians
cont_dd_2014_never_old <- lm(phys_working ~ exposed_2014 + post_2014 + exposed_2014*post_2014 + experience + female + 
                               avg_beds + avg_oper_days, data=filter(Physician_Data, experience>=37 & working_allyears==1 & (minyr_EHR==0 | minyr_EHR==2014)))
summary(cont_dd_2014_never_old)

ind_dd_2014_never_old <- lm(working_ind ~ exposed_2014 + post_2014 + exposed_2014*post_2014 + experience + female + 
                              avg_beds + avg_oper_days, data=filter(Physician_Data, experience>=37 & (minyr_EHR==0 | minyr_EHR==2014)))
summary(ind_dd_2014_never_old)




# Save regressions in stargazer table based on depenent variable-----------------------------------
stargazer(cont_dd_2010_never, cont_dd_2011_never, cont_dd_2012_never, cont_dd_2013_never,
          cont_dd_2014_never, 
          se = starprep(cont_dd_2010_never, cont_dd_2011_never, cont_dd_2012_never, cont_dd_2013_never,
                                 cont_dd_2014_never, se_type = "HC1"),
          p = starprep(cont_dd_2010_never, cont_dd_2011_never, cont_dd_2012_never, cont_dd_2013_never,
                               cont_dd_2014_never, stat = "p.value"),
          title="Continuous Dep. Variable Results",
          dep.var.labels=c("Total Patients (Conditional on Staying in the Job)"),
          type='latex' , 
          out="Objects/continuous.tex")

stargazer(ind_dd_2010_never, ind_dd_2011_never, ind_dd_2012_never, ind_dd_2013_never,
          ind_dd_2014_never, 
          se = starprep(ind_dd_2010_never, ind_dd_2011_never, ind_dd_2012_never, ind_dd_2013_never,
                        ind_dd_2014_never, se_type = "HC1"),
          p = starprep(ind_dd_2010_never, ind_dd_2011_never, ind_dd_2012_never, ind_dd_2013_never,
                       ind_dd_2014_never, stat = "p.value"),
          title="Indicator for Working Dep. Variable Results",
          dep.var.labels=c("Working"),
          type='latex' , 
          out="Objects/indicator.tex")

stargazer(cont_dd_2010_never_old, cont_dd_2011_never_old, cont_dd_2012_never_old, cont_dd_2013_never_old,
          cont_dd_2014_never_old, 
          se = starprep(cont_dd_2010_never_old, cont_dd_2011_never_old, cont_dd_2012_never_old, cont_dd_2013_never_old,
                        cont_dd_2014_never_old, se_type = "HC1"),
          p = starprep(cont_dd_2010_never_old, cont_dd_2011_never_old, cont_dd_2012_never_old, cont_dd_2013_never_old,
                       cont_dd_2014_never_old, stat = "p.value"),
          title="Difference in Difference Results (limited to older physicians)",
          dep.var.labels=c("Total Patients (Conditional on Staying in the Job)"),
          type='latex' , 
          out="Objects/continuous_old.tex")

stargazer(ind_dd_2010_never_old, ind_dd_2011_never_old, ind_dd_2012_never_old, ind_dd_2013_never_old,
          ind_dd_2014_never_old, 
          se = starprep(ind_dd_2010_never_old, ind_dd_2011_never_old, ind_dd_2012_never_old, ind_dd_2013_never_old,
                        ind_dd_2014_never_old, se_type = "HC1"),
          p = starprep(ind_dd_2010_never_old, ind_dd_2011_never_old, ind_dd_2012_never_old, ind_dd_2013_never_old,
                       ind_dd_2014_never_old, stat = "p.value"),
          title="Difference in Difference Linear Probability Model Results (limited to older physicians)",
          dep.var.labels=c("Working"),
          type='latex' , 
          out="Objects/indicator_old.tex")





