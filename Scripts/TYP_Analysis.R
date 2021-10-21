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
#                         10/11/2021

# This Script reads in the data for my third year paper, "Physician_Data.rds" and uses it to analyze the relationship
# between EHR adoption and physician labor market decisions.

# Read in data
Physician_Data <- read_rds(paste0(created_data_path,"Physician_Data.rds"))

# Event Studies for Entire Sample of Doctors ----------------------------------------------------------------
# Continuous Number of Patients Variable
event_reg <- felm(phys_working ~ rel_m6 + rel_m5 + rel_m4 + rel_m3 + rel_m2 + rel_0 + rel_p1 + rel_p2 + rel_p3 + 
                  rel_p4 + rel_p5 + rel_p6 + female + experience + avg_beds + avg_oper_days |year,
                  data=filter(Physician_Data,working_allyears==1))

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
                  data=filter(Physician_Data, working_allyears==1 & experience>=37))

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
                      data=filter(Physician_Data, experience>=37))

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
                  data=filter(Physician_Data,working_allyears==1 & minyr_EHR>2009))

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

# Create yearly datasets so the variables can have the same names
data_2010 <- Physician_Data %>%
  filter(minyr_EHR==2010 | minyr_EHR==0) %>%
  rename(treated=exposed_2010, post=post_2010)
data_2011 <- Physician_Data %>%
  filter(minyr_EHR==2011 | minyr_EHR==0) %>%
  rename(treated=exposed_2011, post=post_2011)
data_2012 <- Physician_Data %>%
  filter(minyr_EHR==2012 | minyr_EHR==0) %>%
  rename(treated=exposed_2012, post=post_2012)
data_2013 <- Physician_Data %>%
  filter(minyr_EHR==2013 | minyr_EHR==0) %>%
  rename(treated=exposed_2013, post=post_2013)
data_2014 <- Physician_Data %>%
  filter(minyr_EHR==2014 | minyr_EHR==0) %>%
  rename(treated=exposed_2014, post=post_2014)
  

# Treated in 2010 ----------------------------------------------------------------------------

# Look at some graphs(comparing treated in 2010 to never treated only)
ggplot(filter(data_2010, working_allyears==1), aes(year,phys_working,color=factor(treated))) +
  stat_summary(geom='line') + geom_vline(xintercept=2010) 
ggplot(data_2010, aes(year,working_ind,color=factor(treated))) +
  stat_summary(geom='line') + geom_vline(xintercept=2010)
# Neither of these graphs point to anything substantial happening in 2010. For the working variable, the lines stay
# parallel for all of the years. The working indicator seems to break a little bit after 2010

# DiD with comparison as never treated
cont_dd_2010_never <- lm(phys_working ~ treated + post + treated*post + experience + female + 
                     avg_beds + avg_oper_days, data=filter(data_2010, working_allyears==1))
summary(cont_dd_2010_never)

ind_dd_2010_never <- lm(working_ind ~ treated + post + treated*post + experience + female + 
                    avg_beds + avg_oper_days, data_2010)
summary(ind_dd_2010_never)

# Did with compairson as never treated, only for older physicians
cont_dd_2010_never_old <- lm(phys_working ~ treated + post + treated*post + experience + female + 
                           avg_beds + avg_oper_days, data=filter(data_2010, experience>=35 & working_allyears==1))
summary(cont_dd_2010_never_old)

ind_dd_2010_never_old <- lm(working_ind ~ treated + post + treated*post + experience + female + 
                          avg_beds + avg_oper_days, data=filter(data_2010, experience>=35))
summary(ind_dd_2010_never_old)

# Treated in 2011 ----------------------------------------------------------------------------

# Look at some graphs(comparing treated in 2010 to never treated only)
ggplot(filter(data_2011, working_allyears==1), aes(year,phys_working,color=factor(treated))) +
  stat_summary(geom='line') + geom_vline(xintercept=2011) 
ggplot(data_2011, aes(year,working_ind,color=factor(treated))) +
  stat_summary(geom='line') + geom_vline(xintercept=2011)

# DiD with comparison as never treated
cont_dd_2011_never <- lm(phys_working ~ treated + post + treated*post + experience + female + 
                           avg_beds + avg_oper_days, data=filter(data_2011, working_allyears==1))
summary(cont_dd_2011_never)

ind_dd_2011_never <- lm(working_ind ~ treated + post + treated*post + experience + female + 
                          avg_beds + avg_oper_days, data_2011)
summary(ind_dd_2011_never)

# Did with comparison as never treated, only for older physicians
cont_dd_2011_never_old <- lm(phys_working ~ treated + post + treated*post + experience + female + 
                               avg_beds + avg_oper_days, data=filter(data_2011, experience>=35 & working_allyears==1))
summary(cont_dd_2011_never_old)

ind_dd_2011_never_old <- lm(working_ind ~ treated + post + treated*post + experience + female + 
                              avg_beds + avg_oper_days, data=filter(data_2011, experience>=35))
summary(ind_dd_2011_never_old)


# Treated in 2012 ----------------------------------------------------------------------------
# Look at some graphs(comparing treated in 2010 to never treated only)
ggplot(filter(data_2012, working_allyears==1), aes(year,phys_working,color=factor(treated))) +
  stat_summary(geom='line') + geom_vline(xintercept=2012) 
ggplot(data_2012, aes(year,working_ind,color=factor(treated))) +
  stat_summary(geom='line') + geom_vline(xintercept=2012)

# DiD with comparison as never treated
cont_dd_2012_never <- lm(phys_working ~ treated + post + treated*post + experience + female + 
                           avg_beds + avg_oper_days, data=filter(data_2012, working_allyears==1))
summary(cont_dd_2012_never)

ind_dd_2012_never <- lm(working_ind ~ treated + post + treated*post + experience + female + 
                          avg_beds + avg_oper_days, data_2012)
summary(ind_dd_2012_never)

# Did with comparison as never treated, only for older physicians
cont_dd_2012_never_old <- lm(phys_working ~ treated + post + treated*post + experience + female + 
                               avg_beds + avg_oper_days, data=filter(data_2012, experience>=35 & working_allyears==1))
summary(cont_dd_2012_never_old)

ind_dd_2012_never_old <- lm(working_ind ~ treated + post + treated*post + experience + female + 
                              avg_beds + avg_oper_days, data=filter(data_2012, experience>=35))
summary(ind_dd_2012_never_old)


# Treated in 2013 ----------------------------------------------------------------------------
# Look at some graphs(comparing treated in 2010 to never treated only)
ggplot(filter(data_2013, working_allyears==1), aes(year,phys_working,color=factor(treated))) +
  stat_summary(geom='line') + geom_vline(xintercept=2013) 
ggplot(data_2013, aes(year,working_ind,color=factor(treated))) +
  stat_summary(geom='line') + geom_vline(xintercept=2013)

# DiD with comparison as never treated
cont_dd_2013_never <- lm(phys_working ~ treated + post + treated*post + experience + female + 
                           avg_beds + avg_oper_days, data=filter(data_2013, working_allyears==1))
summary(cont_dd_2013_never)

ind_dd_2013_never <- lm(working_ind ~ treated + post + treated*post + experience + female + 
                          avg_beds + avg_oper_days, data_2013)
summary(ind_dd_2013_never)

# Did with comparison as never treated, only for older physicians
cont_dd_2013_never_old <- lm(phys_working ~ treated + post + treated*post + experience + female + 
                               avg_beds + avg_oper_days, data=filter(data_2013, experience>=35 & working_allyears==1))
summary(cont_dd_2013_never_old)

ind_dd_2013_never_old <- lm(working_ind ~ treated + post + treated*post + experience + female + 
                              avg_beds + avg_oper_days, data=filter(data_2013, experience>=35))
summary(ind_dd_2013_never_old)


# Treated in 2014 ----------------------------------------------------------------------------
# Look at some graphs(comparing treated in 2010 to never treated only)
ggplot(filter(data_2014, working_allyears==1), aes(year,phys_working,color=factor(treated))) +
  stat_summary(geom='line') + geom_vline(xintercept=2014) 
ggplot(data_2014, aes(year,working_ind,color=factor(treated))) +
  stat_summary(geom='line') + geom_vline(xintercept=2014)

# DiD with comparison as never treated
cont_dd_2014_never <- lm(phys_working ~ treated + post + treated*post + experience + female + 
                           avg_beds + avg_oper_days, data=filter(data_2014, working_allyears==1))
summary(cont_dd_2014_never)

ind_dd_2014_never <- lm(working_ind ~ treated + post + treated*post + experience + female + 
                          avg_beds + avg_oper_days, data_2014)
summary(ind_dd_2014_never)

# Did with comparison as never treated, only for older physicians
cont_dd_2014_never_old <- lm(phys_working ~ treated + post + treated*post + experience + female + 
                               avg_beds + avg_oper_days, data=filter(data_2014, experience>=35 & working_allyears==1))
summary(cont_dd_2014_never_old)

ind_dd_2014_never_old <- lm(working_ind ~ treated + post + treated*post + experience + female + 
                              avg_beds + avg_oper_days, data=filter(data_2014, experience>=35))
summary(ind_dd_2014_never_old)

# Save regressions in stargazer table based on dependent variable-----------------------------------
stargazer(cont_dd_2010_never, cont_dd_2011_never, cont_dd_2012_never, cont_dd_2013_never,
          cont_dd_2014_never, 
          se = starprep(cont_dd_2010_never, cont_dd_2011_never, cont_dd_2012_never, cont_dd_2013_never,
                                 cont_dd_2014_never, se_type = "HC1"),
          p = starprep(cont_dd_2010_never, cont_dd_2011_never, cont_dd_2012_never, cont_dd_2013_never,
                               cont_dd_2014_never, stat = "p.value"),
          title="Difference in Difference by Year: Full Sample of Doctors",
          dep.var.labels=c("Dep. Variable: Total Patients (Conditional on Staying in the Job)"),
          type='latex' , 
          out="Objects/continuous.tex", 
          column.sep.width = "0pt", 
          font.size = "scriptsize",
          keep.stat=c("n","adj.rsq"))

stargazer(ind_dd_2010_never, ind_dd_2011_never, ind_dd_2012_never, ind_dd_2013_never,
          ind_dd_2014_never, 
          se = starprep(ind_dd_2010_never, ind_dd_2011_never, ind_dd_2012_never, ind_dd_2013_never,
                        ind_dd_2014_never, se_type = "HC1"),
          p = starprep(ind_dd_2010_never, ind_dd_2011_never, ind_dd_2012_never, ind_dd_2013_never,
                       ind_dd_2014_never, stat = "p.value"),
          title="Difference in Difference Linear Probability Model by Year; Full Sample of Doctors",
          dep.var.labels=c("Working"),
          type='latex' , 
          out="Objects/indicator.tex",
          column.sep.width = "0pt", 
          font.size = "scriptsize",
          keep.stat=c("n","adj.rsq"))

stargazer(cont_dd_2010_never_old, cont_dd_2011_never_old, cont_dd_2012_never_old, cont_dd_2013_never_old,
          cont_dd_2014_never_old, 
          se = starprep(cont_dd_2010_never_old, cont_dd_2011_never_old, cont_dd_2012_never_old, cont_dd_2013_never_old,
                        cont_dd_2014_never_old, se_type = "HC1"),
          p = starprep(cont_dd_2010_never_old, cont_dd_2011_never_old, cont_dd_2012_never_old, cont_dd_2013_never_old,
                       cont_dd_2014_never_old, stat = "p.value"),
          title="Difference in Difference by Year (limited to older physicians)",
          dep.var.labels=c("Total Patients (Conditional on Staying in the Job)"),
          type='latex' , 
          out="Objects/continuous_old.tex",
          column.sep.width = "0pt", 
          font.size = "scriptsize",
          keep.stat=c("n","adj.rsq"))

stargazer(ind_dd_2010_never_old, ind_dd_2011_never_old, ind_dd_2012_never_old, ind_dd_2013_never_old,
          ind_dd_2014_never_old, 
          se = starprep(ind_dd_2010_never_old, ind_dd_2011_never_old, ind_dd_2012_never_old, ind_dd_2013_never_old,
                        ind_dd_2014_never_old, se_type = "HC1"),
          p = starprep(ind_dd_2010_never_old, ind_dd_2011_never_old, ind_dd_2012_never_old, ind_dd_2013_never_old,
                       ind_dd_2014_never_old, stat = "p.value"),
          title="Difference in Difference Linear Probability Model by Year (limited to older physicians)",
          dep.var.labels=c("Working"),
          type='latex' , 
          out="Objects/indicator_old.tex",
          column.sep.width = "0pt", 
          font.size = "scriptsize",
          keep.stat=c("n","adj.rsq"))



#### Now try event studies for each year (only intensive margin) -------------------------------

# Treated in 2010


#### Callaway and Sant'Anna analysis -------------------------------------------------------
reg_data <- Physician_Data %>%
  mutate(DocNPI=as.numeric(DocNPI)) %>%
  filter(!is.na(female), !is.na(experience), !is.na(avg_oper_days), !is.na(avg_beds))

cont_es <- att_gt(yname = "phys_working_hosp",
              gname = "minyr_EHR",
              idname = "DocNPI",
              tname = "year",
              xformla = ~female + experience + avg_oper_days + avg_beds,
              data = reg_data,
              control_group="notyettreated",
              est_method = "reg"
)
summary(cont_es)
ggdid(cont_es)
dynamic_es <- aggte(cont_es, type = "dynamic", na.rm=T)
ggdid(dynamic_es)
