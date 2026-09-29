library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(kableExtra)

##---- 1. Read and classify --------------------------------------
mu <- read_csv(paste0(raw_data_path, "/MU_REPORT.csv"), guess_max = Inf)

mu2 <- mu %>%
  filter(!is.na(Product_Setting)) %>%
  mutate(setting     = Product_Setting,      # "Ambulatory" / "Inpatient"
         attest_year = Program_Year,          # NOT Attestation_Year (filing lag)
         stage       = Provider_Stage_Number)

##---- 2. Stage 2 penetration share, by setting and calendar year ----
stage_tab <- mu2 %>%
  distinct(NPI, setting, attest_year, stage) %>%
  count(setting, attest_year, stage) %>%
  group_by(setting, attest_year) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

## check 2017 before including -- earlier inspection suggested
## it may rest on very few attestations
stage_tab %>% filter(attest_year == 2017)

## keep years where Stage 2 was a live option and the cell counts
## looked stable (2014-2016); drop 2011-2013 (Stage 2 did not yet
## exist) and 2017 unless the check above shows it's solid
KEEP_YEARS <- 2014:2016

stage_wide <- stage_tab %>%
  filter(attest_year %in% KEEP_YEARS) %>%
  select(setting, attest_year, stage, share) %>%
  pivot_wider(names_from = setting, values_from = share) %>%
  arrange(attest_year, stage)

print(stage_wide)

##---- 3. Formal test at first availability (2014) ----------------
tab_2014 <- mu2 %>%
  filter(attest_year == 2014) %>%
  distinct(NPI, setting, stage) %>%
  count(setting, stage) %>%
  pivot_wider(names_from = stage, values_from = n)

prop_test_2014 <- prop.test(
  x = c(tab_2014$`Stage 2`[tab_2014$setting == "Inpatient"],
        tab_2014$`Stage 2`[tab_2014$setting == "Ambulatory"]),
  n = c(sum(tab_2014[tab_2014$setting == "Inpatient", -1]),
        sum(tab_2014[tab_2014$setting == "Ambulatory", -1]))
)
print(prop_test_2014)

gap_2014 <- stage_wide %>%
  filter(attest_year == 2014, stage == "Stage 2") %>%
  mutate(gap_pp = 100 * (Inpatient - Ambulatory))
print(gap_2014)

##---- 4. Table for the paper -------------------------------------
tab_out <- stage_wide %>%
  filter(stage == "Stage 2") %>%
  transmute(
    Year                 = attest_year,
    `Inpatient (\\%)`    = sprintf("%.1f", 100 * Inpatient),
    `Ambulatory (\\%)`   = sprintf("%.1f", 100 * Ambulatory),
    `Gap (p.p.)`         = sprintf("%.1f", 100 * (Inpatient - Ambulatory))
  ) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE,
      align = c("c", "c", "c", "c"),
      caption = paste0("Share of Meaningful Use attestations at Stage 2, ",
                       "by product setting and program year"),
      label = "mu_stage2") %>%
  kable_styling(latex_options = "hold_position", full_width = FALSE) %>%
  footnote(general = paste(
    "Sample restricted to attestations using a product certified for",
    "either the Inpatient or Ambulatory setting. Program year reflects",
    "the reporting period covered by the attestation, not the year the",
    "attestation was filed. A test of equal Stage 2 shares in 2014",
    sprintf("rejects equality ($\\\\chi^2$ = %.1f, $p$ %s).",
            prop_test_2014$statistic,
            format.pval(prop_test_2014$p.value, digits = 2, eps = 0.001))),
    threeparttable = TRUE, escape = FALSE)

writeLines(as.character(tab_out), file.path("./Table Code", "mu_stage2.tex"))
cat("Wrote Table Code/mu_stage2.tex\n")

##---- 5. Figure ----------------------------------------------------
pal_setting <- c("Inpatient" = "#16324F", "Ambulatory" = "#A34A5A")

fig_data <- stage_tab %>%
  filter(attest_year %in% KEEP_YEARS, stage == "Stage 2")

p <- ggplot(fig_data, aes(attest_year, share, colour = setting)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.4) +
  scale_colour_manual(values = pal_setting) +
  scale_x_continuous(breaks = KEEP_YEARS) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = "\nProgram Year", y = "Share of attestations at Stage 2\n",
       colour = NULL) +
  theme_bw() +
  theme(text = element_text(size = 14),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey30", fill = NA, linewidth = 0.5))

print(p)
ggsave("Objects/mu_stage2_gap.pdf", plot = p, width = 7, height = 5, units = "in")

##---- 6. One-line summary for text ---------------------------------
cat(sprintf(
  "In %d, %.1f%% of inpatient attestations vs %.1f%% of ambulatory\nattestations had reached Stage 2 (gap: %.1f p.p., chi-sq p %s).\n",
  gap_2014$attest_year, 100 * gap_2014$Inpatient, 100 * gap_2014$Ambulatory,
  gap_2014$gap_pp, format.pval(prop_test_2014$p.value, digits = 2, eps = 0.001)
))



### HIMSS DATA with ambulatory kept ###########################################
# HIMSS
years <- 2009:2015

read_years <- function(years, f) bind_rows(lapply(years, f))

HAEntity <- read_years(years, function(t) {
  read_xlsx(file.path(raw_data_path, "HIMSS", paste0("HAEntity", t, ".xlsx")),
            col_types = "text") %>%
    mutate(year = t) %>%
    select(HAEntityId, SurveyId, ParentId, UniqueId, year,
           HAEntityType, MedicareNumber, Name, City, State) %>%
    mutate(across(c(HAEntityId, SurveyId, ParentId, UniqueId), as.integer),
           MedicareNumber = na_if(trimws(MedicareNumber), ""))
})

AcuteInfo <- read_years(years, function(t) {
  read_xlsx(file.path(raw_data_path, "HIMSS", paste0("AcuteInfo", t, ".xlsx"))) %>%
    mutate(year = t) %>%
    select(HAEntityId, year,
           StructuredPhysDocPerc,
           CPOEHospitalsPerc, CPOEPhysicianPerc, CPOEResidentsPerc,
           CPOEAffiliatedPhPerc, CPOEOtherFTPhPerc,
           any_of(c("PhysDocUsePerc", "CPOEMandated",
                    "CPOEInpatient", "CPOEEmergencyDept")))
})

HAEntityApplication_all <- read_years(years, function(t) {
  read_csv(file.path(raw_data_path, "HIMSS",
                     paste0("HAEntityApplication", t, ".csv")),
           show_col_types = FALSE) %>%
    mutate(year = t) %>%
    select(HAEntityId, year, Application, Category, Status)
})

app_all <- HAEntityApplication_all %>%
  left_join(HAEntity %>%
              select(HAEntityId, year, UniqueId, SurveyId,
                     HAEntityType, MedicareNumber, Name, State),
            by = c("HAEntityId", "year"))

status_levels <- c("Not Reported", "Not Automated", "Not Yet Contracted",
                   "Contracted/Not Yet Installed", "Installation in Process",
                   "To be Replaced", "Live and Operational")

app_all <- app_all %>%
  mutate(Status = trimws(Status),
         Status = recode(Status,
                         "To Be Replaced"            = "To be Replaced",
                         "Installation In Process"   = "Installation in Process",
                         "NotAutomatedNot Automated" = "Not Automated"),
         live = Status %in% c("Live and Operational", "To be Replaced"))

## does the comparable_app logic actually match anything?
app_all %>%
  filter(live) %>%
  mutate(comparable_app = case_when(
    HAEntityType == "Hospital"   & Category == "Electronic Medical Record" ~ TRUE,
    HAEntityType == "Ambulatory" & Application == "Ambulatory EMR"         ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  count(comparable_app)

## if ambulatory entities exist, do they have application records
## in HAEntityApplication (the same file the hospital-side pull uses)?
amb_ids <- HAEntity %>% filter(str_detect(tolower(HAEntityType), "ambul|clinic|physician|group")) %>%
  pull(HAEntityId)

length(amb_ids)

n_apps_by_entity <- app_all %>%
  filter(live) %>%
  mutate(comparable_app = case_when(
    HAEntityType == "Hospital"   & Category == "Electronic Medical Record" ~ TRUE,
    HAEntityType == "Ambulatory" & Application == "Ambulatory EMR"         ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  filter(comparable_app) %>%
  distinct(HAEntityType, HAEntityId, year, Application) %>%
  count(HAEntityType, HAEntityId, year, name = "n_live_apps")

n_apps_by_entity %>%
  filter(HAEntityType %in% c("Hospital", "Ambulatory")) %>%
  group_by(HAEntityType) %>%
  summarise(mean_apps   = mean(n_live_apps),
            median_apps = median(n_live_apps),
            n_entities  = n(), .groups = "drop")

##---- 1. Share of ambulatory entities with Ambulatory EMR live, by year
amb_emr_by_year <- app_all %>%
  filter(HAEntityType == "Ambulatory", Application == "Ambulatory EMR") %>%
  distinct(HAEntityId, year, live) %>%
  group_by(year) %>%
  summarise(share_live = mean(live), n_entities = n(), .groups = "drop")

print(amb_emr_by_year)

##---- 2. Share of hospitals with at least one EMR component live, by year
hosp_emr_by_year <- app_all %>%
  filter(HAEntityType == "Hospital", Category == "Electronic Medical Record") %>%
  group_by(HAEntityId, year) %>%
  summarise(any_live = any(live), .groups = "drop") %>%
  group_by(year) %>%
  summarise(share_live = mean(any_live), n_entities = n(), .groups = "drop")

print(hosp_emr_by_year)


##---- 3. Figure: adoption curves side by side --------------------------
timing_comp <- bind_rows(
  amb_emr_by_year  %>% mutate(setting = "Ambulatory"),
  hosp_emr_by_year %>% mutate(setting = "Hospital")
)

pal_setting <- c("Hospital" = "#16324F", "Ambulatory" = "#A34A5A")

p <- ggplot(timing_comp, aes(year, share_live, colour = setting)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.4) +
  scale_colour_manual(values = pal_setting) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = "\nYear", y = "Share with EMR live and operational\n",
       colour = NULL) +
  theme_bw() +
  theme(text = element_text(size = 14),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey30", fill = NA, linewidth = 0.5))

print(p)
ggsave("Objects/himss_emr_timing.pdf", plot = p, width = 7, height = 5, units = "in")

##---- 4. Functional depth among hospitals, over time --------------------
## among hospitals with ANY EMR component live, how many of the
## 6 tracked components do they have, on average, by year?
hosp_depth_by_year <- app_all %>%
  filter(HAEntityType == "Hospital", Category == "Electronic Medical Record", live) %>%
  distinct(HAEntityId, year, Application) %>%
  count(HAEntityId, year, name = "n_live") %>%
  group_by(year) %>%
  summarise(mean_components = mean(n_live), n_entities = n(), .groups = "drop")

print(hosp_depth_by_year)

##---- 5. Gap summary for text/table -------------------------------------
gap_tab <- hosp_emr_by_year %>%
  select(year, share_hosp = share_live) %>%
  left_join(amb_emr_by_year %>% select(year, share_amb = share_live), by = "year") %>%
  mutate(gap_pp = 100 * (share_hosp - share_amb))

print(gap_tab)

cat(sprintf(
  "In 2009, %.1f%% of hospitals had EMR live vs %.1f%% of ambulatory\nentities (gap: %.1f p.p.). By 2015 the gap had narrowed to %.1f p.p.\n",
  100*gap_tab$share_hosp[1], 100*gap_tab$share_amb[1], gap_tab$gap_pp[1],
  gap_tab$gap_pp[nrow(gap_tab)]
))

##---- 6. Table for appendix ----------------------------------------------
library(kableExtra)

tab_out <- gap_tab %>%
  transmute(Year = year,
            `Hospital (\\%)`   = sprintf("%.1f", 100*share_hosp),
            `Ambulatory (\\%)` = sprintf("%.1f", 100*share_amb),
            `Gap (p.p.)`       = sprintf("%.1f", gap_pp)) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE,
      align = c("c","c","c","c"),
      caption = "Share of HIMSS-surveyed entities with EMR live and operational, by setting",
      label = "himss_timing") %>%
  kable_styling(latex_options = "hold_position", full_width = FALSE) %>%
  footnote(general = paste(
    "Hospital share is based on any of six clinical EMR sub-components",
    "(clinical data repository, clinical decision support, computerized",
    "physician order entry, order entry, patient portal, or physician",
    "documentation) reported live and operational.",
    "Ambulatory share is based on the Ambulatory EMR application, the",
    "only comparable clinical documentation system HIMSS tracks for",
    "this setting."),
    threeparttable = TRUE, escape = FALSE)

writeLines(as.character(tab_out), file.path("./Table Code", "himss_timing.tex"))
cat("Wrote Table Code/himss_timing.tex\n")


##=============================================================
## Two-panel figure: EHR timing evidence
##   (a) HIMSS: share with EMR live and operational, by setting
##   (b) ONC MU: share of attestations at Stage 2, by setting
##
##   Requires: gap_tab (HIMSS) and stage_wide (ONC), built earlier
##=============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

pal_setting <- c("Hospital" = "#16324F", "Ambulatory" = "#A34A5A")

##---- Shared theme, matching other figures in the paper ----------------
base_theme <- theme_bw() +
  theme(
    text               = element_text(size = 14),
    axis.text          = element_text(size = 13),
    axis.title         = element_text(size = 14),
    strip.text         = element_text(size = 12.5, lineheight = 1.1),
    legend.title       = element_blank(),
    legend.position    = "bottom",
    legend.text        = element_text(size = 13),
    legend.key.width   = unit(1.6, "lines"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, colour = "grey65"),
    panel.border       = element_rect(colour = "grey30", fill = NA, linewidth = 0.5)
  )

##---- Panel (a): HIMSS EMR live share, by setting -----------------------
himss_long <- gap_tab %>%
  select(year, Hospital = share_hosp, Ambulatory = share_amb) %>%
  pivot_longer(-year, names_to = "setting", values_to = "share")

##---- Panel (b): ONC MU Stage 2 share, by setting -----------------------
onc_long <- stage_wide %>%
  filter(stage == "Stage 2") %>%
  select(year = attest_year, Hospital = Inpatient, Ambulatory) %>%
  pivot_longer(-year, names_to = "setting", values_to = "share")

p1 <- ggplot(himss_long, aes(year, share, colour = setting, shape = setting)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.4) +
  scale_colour_manual(values = pal_setting) +
  scale_shape_manual(values = c("Hospital" = 16, "Ambulatory" = 17)) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  facet_wrap(~ "HIMSS Analytics: EMR Adoption") +
  labs(x = NULL, y = "Share with EMR\nLive and Operational\n") +
  base_theme

p2 <- ggplot(onc_long, aes(year, share, colour = setting, shape = setting)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.4) +
  scale_colour_manual(values = pal_setting) +
  scale_shape_manual(values = c("Hospital" = 16, "Ambulatory" = 17)) +
  scale_x_continuous(breaks = 2014:2016) +           # fixes the decimal issue too
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  facet_wrap(~ "ONC Meaningful Use: Stage 2 Attestation") +
  labs(x = "\nYear", y = "Share of Attestations\nat Stage 2\n") +
  base_theme


##---- Combine, one legend, shared x-axis --------------------------------
p_combined <- p1 / p2 +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(p_combined)
ggsave("Objects/timing_evidence_panels.pdf", plot = p_combined,
       width = 7.5, height = 9, units = "in")







