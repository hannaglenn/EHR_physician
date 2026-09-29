library(tidyr)
library(readr)
library(stringr)
library(dplyr)
library(plm)
library(janitor)
library(lubridate)
library(did)
library(ggplot2)
library(readxl)
library(dplyr)



### HETEROGENEITY GRAPHS ------------------------------------------- ####

# Read in the main data created in "data5_MDPPAS.R"
Physician_Data <- readRDS(paste0(created_data_path,"Physician_Data_NEW2.rds")) %>%
  mutate(DocNPI=as.numeric(DocNPI))

Physician_Data_EHR <- Physician_Data %>%
  ungroup() %>%
  mutate(bundled = purrr::map2_lgl(doc_sc_years, minyr_EHR,
                                   ~ !is.null(.x) && .y > 0 && any(abs(.x - .y) <= 0))) %>%
  group_by(DocNPI) %>%
  mutate(drop_md = any(bundled)) %>%
  ungroup() %>%
  filter(!drop_md)

Physician_Data_EHR <- Physician_Data_EHR %>%
  filter(minyr_EHR!=0 & minyr_EHR!=2009)

Physician_Data_EHR <- Physician_Data_EHR %>%
  filter(first_mdppas_yr==2009) 

Physician_Data_EHR %>%
  group_by(minyr_EHR) %>%
  distinct(DocNPI) %>%
  count()

Physician_Data_EHR <- Physician_Data_EHR %>%
  mutate(retired_ever = ifelse(!is.na(minyr_retire) & year >= minyr_retire, 1, 0)) %>%
  mutate(claims_inpat  = claim_count_total * pos_inpat,
         claims_office = claim_count_total * pos_office,
         claims_opd    = claim_count_total * pos_opd) %>%
  group_by(DocNPI) %>%
  mutate(stable_inpat = all(pos_inpat > 0.7, na.rm = TRUE)) %>%
  ungroup()


# Read in rural zip code data
RUCA2010zipcode <- read_excel(paste0(raw_data_path,"/RUCA2010zipcode.xlsx"), 
                              sheet = "Data")

Physician_Data_EHR <- Physician_Data_EHR %>%
  mutate(phy_zip1=as.character(phy_zip1)) %>%
  dplyr::left_join(RUCA2010zipcode, by=c("phy_zip1"="ZIP_CODE"))


# Create rural and urban indicators
Physician_Data_EHR <- Physician_Data_EHR %>%
  mutate(rural=ifelse(RUCA1==6 | RUCA1==7 | RUCA1==8 | RUCA1==9 | RUCA1==10,1,0),
         urban=ifelse(RUCA1==1 | RUCA1==2 | RUCA1==3 | RUCA1==4 | RUCA1==5,1,0),
         small=ifelse(avg_beds<278,1,0),
         large=ifelse(avg_beds>=278,1,0))

# Create "ever urban" and "ever rural"
Physician_Data_EHR <- Physician_Data_EHR %>%
  dplyr::group_by(DocNPI) %>%
  dplyr::mutate(sum=sum(rural, na.rm=TRUE),
                sum1=sum(urban, na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(ever_rural=ifelse(sum>0,1,0),
                ever_urban=ifelse(sum1>0,1,0)) %>%
  dplyr::select(-sum,-sum1)

# Define different data sets
Rural_Data <- Physician_Data_EHR %>%
  dplyr::filter(ever_rural==1)
Urban_Data <- Physician_Data_EHR %>%
  dplyr::filter(ever_urban==1)
Smallhosp_Data <- Physician_Data_EHR %>%
  dplyr::filter(small==1)
Largehosp_Data <- Physician_Data_EHR %>%
  dplyr::filter(large==1)
onehosp_data <- Physician_Data_EHR %>%
  dplyr::filter(num_systems==1)
manyhosp_data <- Physician_Data_EHR %>%
  dplyr::filter(num_systems>1)

data_coms <- tidyr::crossing(rural=c(TRUE,FALSE), urban=c(TRUE,FALSE), small=c(TRUE,FALSE), large=c(TRUE,FALSE), one=c(TRUE, FALSE), many=c(TRUE,FALSE)) %>%
  filter(rural==TRUE & urban==FALSE & small==FALSE & large==FALSE & one == FALSE & many == FALSE |
           rural==FALSE & urban==TRUE & small==FALSE & large==FALSE & one == FALSE & many == FALSE |
           rural==FALSE & urban==FALSE & small==TRUE & large==FALSE & one == FALSE & many == FALSE |
           rural==FALSE & urban==FALSE & small==FALSE & large==TRUE & one == FALSE & many == FALSE |
           rural==FALSE & urban==FALSE & small==FALSE & large==FALSE & one == FALSE & many == FALSE |
           rural==FALSE & urban==FALSE & small==FALSE & large==FALSE & one == TRUE & many == FALSE |
           rural==FALSE & urban==FALSE & small==FALSE & large==FALSE & one == FALSE & many == TRUE)

Het_Data <- list(list(Rural_Data, data_coms[7,]), list(Urban_Data, data_coms[6,]),
                 list(Smallhosp_Data,data_coms[5,]), list(Largehosp_Data, data_coms[4,]),
                 list(Physician_Data_EHR,data_coms[1,]), list(onehosp_data,data_coms[3,]),
                 list(manyhosp_data,data_coms[2,]))


# create another dataset for different levels of integration with the hospital
IPA_data <- Physician_Data_EHR %>%
  filter(phys_ever_IPA==1)
OPHO_data <- Physician_Data_EHR %>%
  filter(phys_ever_OPHO==1) 
CPHO_data <- Physician_Data_EHR %>%
  filter(phys_ever_CPHO==1) 
ISM_data <- Physician_Data_EHR %>%
  filter(phys_ever_ISM==1) 

data_coms_int <- crossing(IPA=c(TRUE,FALSE), OPHO=c(TRUE,FALSE), CPHO=c(TRUE,FALSE), ISM=c(TRUE,FALSE)) %>%
  filter(IPA==TRUE & OPHO==FALSE & CPHO==FALSE & ISM==FALSE |
           IPA==FALSE & OPHO==TRUE & CPHO==FALSE & ISM==FALSE |
           IPA==FALSE & OPHO==FALSE & CPHO==TRUE & ISM==FALSE |
           IPA==FALSE & OPHO==FALSE & CPHO==FALSE & ISM==TRUE |
           IPA==FALSE & OPHO==FALSE & CPHO==FALSE & ISM==FALSE)

Het_Data_int <- list(list(IPA_data, data_coms_int[5,]), list(OPHO_data, data_coms_int[4,]),
                 list(CPHO_data,data_coms_int[3,]), list(ISM_data, data_coms_int[2,]),
                 list(Physician_Data_EHR,data_coms_int[1,]))


##=============================================================
## Heterogeneity chart
##   One specification function for all outcomes, so settings
##   cannot drift between them.
##   Adds a pre-trends Wald test for every outcome x subsample.
##=============================================================

##---- Shared specification settings --------------------------
XFORMLA     <- NULL        # set to NULL for no covariates
BASE_PERIOD <- "universal"       # universal is the convention used in the main text
EST_METHOD  <- "dr"
CONTROL_GRP <- "notyettreated"

##---- Sample restrictions by outcome -------------------------
restrict <- function(d, yname) {
  d <- dplyr::filter(d, minyr_EHR > 0)
  if (yname %in% c("pos_office", "work_in_office"))
    d <- dplyr::filter(d, ever_retire == 0)
  if (yname %in% c("npi_unq_benes", "claim_per_patient"))
    d <- dplyr::filter(d, ever_retire == 0, stable_inpat == 1)
  d
}

##---- Wald pre-trends test -----------------------------------
wald_pre <- function(mp, max_lag = Inf) {
  pre <- which(mp$t < mp$group & !is.na(mp$se) & mp$se > 0 &
                 (mp$group - mp$t) <= max_lag)
  if (!length(pre)) return(c(W = NA, df = NA, n_cells = 0, p = NA))
  a  <- mp$att[pre]
  V  <- crossprod(as.matrix(mp$inffunc[, pre, drop = FALSE])) / mp$n^2
  W  <- as.numeric(t(a) %*% MASS::ginv(V) %*% a)
  df <- qr(V)$rank
  c(W = W, df = df, n_cells = length(pre), p = 1 - pchisq(W, df))
}

##---- Baseline mean: treated units, year before treatment ----
pre_mean <- function(d, yname) {
  d %>%
    ungroup() %>%
    dplyr::filter(minyr_EHR > 0, year == minyr_EHR - 1) %>%
    dplyr::summarise(m = mean(.data[[yname]], na.rm = TRUE)) %>%
    dplyr::pull(m)
}

##---- Fit one outcome across all subsamples ------------------
OUTCOMES <- c(retired_ever      = "Exit",
              pos_office        = "Frac. in Office",
              work_in_office    = "Work in Office",
              npi_unq_benes     = "Number Patients",
              claim_per_patient = "Claims per Patient")

fit_one <- function(yname, label) {
  map_dfr(Het_Data, function(x) {
    d   <- restrict(x[[1]], yname)
    row <- x[[2]]
    
    mp <- did::att_gt(
      yname         = yname,
      gname         = "minyr_EHR",
      idname        = "DocNPI",
      tname         = "year",
      xformla       = XFORMLA,
      data          = d,
      est_method    = EST_METHOD,
      control_group = CONTROL_GRP,
      clustervars   = "DocNPI",
      anticipation  = 0,
      base_period   = BASE_PERIOD
    )
    
    ag <- did::aggte(mp, type = "dynamic", na.rm = TRUE)
    w  <- wald_pre(mp)
    
    tibble(
      Outcome = label,
      coef    = ag$overall.att,
      se      = ag$overall.se,
      p       = 2 * pnorm(-abs(ag$overall.att / ag$overall.se)),
      p_pre   = unname(w["p"]),
      df_pre  = unname(w["df"]),
      n_doc   = n_distinct(d$DocNPI),
      mean    = pre_mean(d, yname)
    ) %>%
      bind_cols(as_tibble(row))
  })
}

plot_data <- imap_dfr(OUTCOMES, ~ fit_one(.y, .x))

##---- Label subsamples, scale by the mean --------------------
plot_data <- plot_data %>%
  mutate(
    Sample = case_when(rural == 1 ~ "Rural",
                       urban == 1 ~ "Urban",
                       small == 1 ~ "Small Hosp.",
                       large == 1 ~ "Large Hosp.",
                       one   == 1 ~ "Works w/ 1 Sys.",
                       many  == 1 ~ "Works w/ >1 Sys.",
                       TRUE       ~ "Main"),
    estimate_perc = coef / mean,
    upper = (coef + 1.96 * se) / mean,
    lower = (coef - 1.96 * se) / mean,
    Outcome = factor(Outcome, levels = unname(OUTCOMES)),
    Sample  = factor(Sample, levels = c("Main", "Small Hosp.", "Large Hosp.",
                                        "Rural", "Urban", "Works w/ 1 Sys.",
                                        "Works w/ >1 Sys."))
  ) %>%
  select(-rural, -urban, -small, -large, -one, -many) %>%
  arrange(Outcome, Sample)

##---- Pre-trends table (one row per outcome x subsample) -----
pretrend_tab <- plot_data %>%
  mutate(p_pre = ifelse(is.na(p_pre), "---", sprintf("%.2f", p_pre))) %>%
  select(Outcome, Sample, p_pre) %>%
  pivot_wider(names_from = Sample, values_from = p_pre)

print(pretrend_tab, width = Inf)

##---- Chart --------------------------------------------------
pal <- c("Main"             = "#000000",
         "Small Hosp."      = "#16324F",
         "Large Hosp."      = "#4C7FA6",
         "Rural"            = "#A34A5A",
         "Urban"            = "#D08C97",
         "Works w/ 1 Sys."  = "#3D7068",
         "Works w/ >1 Sys." = "#7FA99B")

dodge <- position_dodge(width = 0.6)

plot_data <- plot_data %>%
  mutate(upper_raw = coef + 1.96 * se,
         lower_raw = coef - 1.96 * se)

## baseline mean per facet, for the strip labels
facet_labs <- plot_data %>%
  filter(Sample == "Main") %>%
  mutate(lab = sprintf("%s\n(baseline: %s)", Outcome,
                       ifelse(mean >= 100, formatC(mean, format = "d", big.mark = ","),
                              ifelse(mean >= 1,   sprintf("%.2f", mean),
                                     sprintf("%.3f", mean))))) %>%
  select(Outcome, lab)

plot_data <- plot_data %>%
  left_join(facet_labs, by = "Outcome") %>%
  mutate(lab = factor(lab, levels = facet_labs$lab))

p <- ggplot(plot_data, aes(x = Sample, y = coef, colour = Sample)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") +
  geom_errorbar(aes(ymin = lower_raw, ymax = upper_raw),
                linewidth = 0.6, width = 0.25) +
  geom_point(size = 2) +
  facet_wrap(~ lab, scales = "free_y", nrow = 1) +
  scale_colour_manual(values = pal) +
  labs(x = NULL, y = "Point Estimate and 95% CI\n") +
  theme_bw() +
  scale_y_continuous(expand = expansion(mult = 0.15)) +
  theme(
    text          = element_text(size = 14),
    axis.text.y   = element_text(size = 13),
    axis.title.y  = element_text(size = 14),
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    strip.text    = element_text(size = 12.5, lineheight = 1.1),
    legend.text   = element_text(size = 13),
    legend.title  = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1.6, "lines"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )

print(p)
ggsave("Objects/heterog_plot.pdf", plot = p,
       width = 11, height = 5.5, units = "in")

##=============================================================
## Appendix table for the heterogeneity figure
##   Rows: outcome x subsample
##   Cols: physicians, baseline mean, estimate, 95% CI, pre-trends p
##
## Requires `plot_data` from the heterogeneity script, which must
## contain: Outcome, Sample, n_doc, mean, coef, se, p, p_pre
##=============================================================

fmt_num <- function(x) ifelse(is.na(x), "---",
                              ifelse(abs(x) >= 100, formatC(x, format = "d", big.mark = ","),
                                     ifelse(abs(x) >= 1,   sprintf("%.2f", x),
                                            sprintf("%.3f", x))))
fmt_p <- function(p) ifelse(is.na(p), "---",
                            ifelse(p < 0.01, "$<$0.01", sprintf("%.2f", p)))

tab <- plot_data %>%
  ungroup() %>%
  arrange(Outcome, Sample) %>%
  mutate(
    stars    = case_when(p < 0.01 ~ "$^{***}$",
                         p < 0.05 ~ "$^{**}$",
                         p < 0.10 ~ "$^{*}$", TRUE ~ ""),
    label    = paste0("\\quad ", gsub(">", "$>$", as.character(Sample), fixed = TRUE)),
    N        = formatC(n_doc, format = "d", big.mark = ","),
    Mean     = fmt_num(mean),
    Estimate = paste0(fmt_num(coef), stars),
    CI       = sprintf("[%s, %s]", fmt_num(coef - 1.96 * se),
                       fmt_num(coef + 1.96 * se)),
    Ppre     = fmt_p(p_pre)
  ) %>%
  select(Outcome, label, N, Mean, Estimate, CI, Ppre)

blocks <- tab %>% count(Outcome)

out <- tab %>%
  select(-Outcome) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE,
      col.names = c("", "Physicians", "Baseline mean", "Estimate",
                    "95\\% CI", "Pre-trends $p$"),
      align = c("l", rep("c", 5)),
      caption = "Estimates underlying the heterogeneity figure",
      label = "heterog_appendix") %>%
  kable_styling(latex_options = "hold_position",
                full_width = FALSE)

start <- 1
for (i in seq_len(nrow(blocks))) {
  o <- as.character(blocks$Outcome[i]); n <- blocks$n[i]
  out <- pack_rows(out, o, start, start + n - 1, escape = FALSE)
  start <- start + n
}

out <- out %>%
  kableExtra::footnote(general = paste(
    "Each row reports the overall average treatment effect on the treated",
    "from Callaway and Sant'Anna (2021), aggregated over event time and",
    "estimated separately on the indicated subsample.",
    "Physicians is the number contributing to each estimate.",
    "Baseline mean is the mean of the outcome among treated physicians in the",
    "year before first EHR exposure, computed within each subsample.",
    "The final column reports a Wald test of the null that all pre-treatment",
    "group-time effects equal zero.",
    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."),
    threeparttable = TRUE, escape = FALSE)

cat(out)

writeLines(as.character(out), file.path("Table Code", "heterog_appendix.tex"))
cat("\nWrote Table Code/heterog_appendix.tex\n")




##=============================================================
## Heterogeneity by integration level
##   One specification function for all outcomes.
##   Produces: faceted figure (raw units) + appendix table
##             with physician counts and pre-trends tests.
##=============================================================
fit_one <- function(yname, label) {
  map_dfr(Het_Data_int, function(x) {
    d   <- restrict(x[[1]], yname)
    row <- x[[2]]
    
    mp <- did::att_gt(yname = yname, gname = "minyr_EHR", idname = "DocNPI",
                      tname = "year", xformla = XFORMLA, data = d,
                      est_method = EST_METHOD, control_group = CONTROL_GRP,
                      clustervars = "DocNPI", anticipation = 0,
                      base_period = BASE_PERIOD)
    
    ag <- did::aggte(mp, type = "dynamic", na.rm = TRUE)
    w  <- wald_pre(mp)
    
    tibble(Outcome = label,
           coef   = ag$overall.att,
           se     = ag$overall.se,
           p      = 2 * pnorm(-abs(ag$overall.att / ag$overall.se)),
           p_pre  = unname(w["p"]),
           n_doc  = mp$n,
           mean   = pre_mean(d, yname)) %>%
      bind_cols(as_tibble(row))
  })
}

res <- imap_dfr(OUTCOMES, ~ fit_one(.y, .x)) %>%
  mutate(Sample = case_when(IPA  == 1 ~ "IPA",
                            OPHO == 1 ~ "OPHO",
                            CPHO == 1 ~ "CPHO",
                            ISM  == 1 ~ "ISM",
                            TRUE      ~ "Main"),
         Sample  = factor(Sample, levels = c("Main", "IPA", "OPHO", "CPHO", "ISM")),
         Outcome = factor(Outcome, levels = unname(OUTCOMES)),
         upper   = coef + 1.96 * se,
         lower   = coef - 1.96 * se) %>%
  select(-IPA, -OPHO, -CPHO, -ISM) %>%
  arrange(Outcome, Sample)

##=============================================================
## FIGURE -- faceted, raw units, baseline in strip label
##=============================================================
fmt_base <- function(x) ifelse(x >= 100, formatC(x, format = "d", big.mark = ","),
                               ifelse(x >= 1,   sprintf("%.2f", x),
                                      sprintf("%.3f", x)))

facet_labs <- res %>%
  filter(Sample == "Main") %>%
  transmute(Outcome, lab = sprintf("%s\n(baseline: %s)", Outcome, fmt_base(mean)))

res <- res %>%
  left_join(facet_labs, by = "Outcome") %>%
  mutate(lab = factor(lab, levels = facet_labs$lab))

pal <- c("Main" = "#000000",
         "IPA"  = "#16324F",
         "OPHO" = "#4C7FA6",
         "CPHO" = "#A34A5A",
         "ISM"  = "#3D7068")

p <- ggplot(res, aes(x = Sample, y = coef, colour = Sample)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                linewidth = 0.6, width = 0.25) +
  geom_point(size = 2) +
  facet_wrap(~ lab, scales = "free_y", nrow = 1) +
  scale_colour_manual(values = pal) +
  labs(x = NULL, y = "Point Estimate and 95% CI\n") +
  theme_bw() +
  scale_y_continuous(expand = expansion(mult = 0.15)) +
  theme(
    text          = element_text(size = 14),
    axis.text.y   = element_text(size = 13),
    axis.title.y  = element_text(size = 14),
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    strip.text    = element_text(size = 12.5, lineheight = 1.1),
    legend.text   = element_text(size = 13),
    legend.title  = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1.6, "lines"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )

print(p)
ggsave("Objects/heterog_plot_int.pdf", plot = p,
       width = 11, height = 5.5, units = "in")

##=============================================================
## APPENDIX TABLE
##=============================================================
fmt_num <- function(x) ifelse(is.na(x), "---",
                              ifelse(abs(x) >= 100, formatC(x, format = "d", big.mark = ","),
                                     ifelse(abs(x) >= 1,   sprintf("%.2f", x),
                                            sprintf("%.3f", x))))
fmt_p <- function(p) ifelse(is.na(p), "---",
                            ifelse(p < 0.01, "$<$0.01", sprintf("%.2f", p)))

tab <- res %>%
  ungroup() %>%
  mutate(stars    = case_when(p < 0.01 ~ "$^{***}$",
                              p < 0.05 ~ "$^{**}$",
                              p < 0.10 ~ "$^{*}$", TRUE ~ ""),
         label    = paste0("\\quad ", as.character(Sample)),
         N        = formatC(n_doc, format = "d", big.mark = ","),
         Mean     = fmt_num(mean),
         Estimate = paste0(fmt_num(coef), stars),
         CI       = sprintf("[%s, %s]", fmt_num(lower), fmt_num(upper)),
         Ppre     = fmt_p(p_pre)) %>%
  select(Outcome, label, N, Mean, Estimate, CI, Ppre)

blocks <- tab %>% count(Outcome)

out <- tab %>%
  select(-Outcome) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE,
      col.names = c("", "Physicians", "Baseline mean", "Estimate",
                    "95\\% CI", "Pre-trends $p$"),
      align = c("l", rep("c", 5)),
      caption = "Effect of EHR exposure by physician-hospital integration level",
      label = "het_int") %>%
  kable_styling(latex_options = "hold_position",
                full_width = FALSE)

start <- 1
for (i in seq_len(nrow(blocks))) {
  o <- as.character(blocks$Outcome[i]); n <- blocks$n[i]
  out <- pack_rows(out, o, start, start + n - 1, escape = FALSE)
  start <- start + n
}

out <- out %>%
  kableExtra::footnote(general = paste(
    "Each row reports the overall average treatment effect on the treated",
    "from Callaway and Sant'Anna (2021), aggregated over event time and",
    "estimated separately on physicians affiliated with hospitals at the",
    "indicated level of physician-hospital integration.",
    "IPA is independent practice association, OPHO and CPHO are open and",
    "closed physician-hospital organizations, and ISM is integrated salary",
    "model. Physicians is the number contributing to each estimate.",
    "Baseline mean is the mean of the outcome among treated physicians in",
    "the year before first EHR exposure, computed within each subsample.",
    "The final column reports a Wald test of the null that all pre-treatment",
    "group-time effects equal zero.",
    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."),
    threeparttable = TRUE, escape = FALSE)

writeLines(as.character(out), file.path("Table Code", "het_int.tex"))
cat("\nWrote Table Code/het_int.tex and Objects/heterog_plot_int.pdf\n")


##=============================================================
## Step 2: Does combined_type change around minyr_EHR?
## Step 3: Re-run heterogeneity estimation by combined_type.
##
## Requires: Physician_Data_EHR (with combined_type merged on),
##           minyr_EHR, ever_retire, stable_inpat
##=============================================================

library(dplyr)
library(tidyr)
library(purrr)
library(did)
library(ggplot2)

##=============================================================
## STEP 2: Stability of combined_type around treatment
##=============================================================

## baseline (year before exposure) vs. later post-treatment years
stability_check <- Physician_Data_EHR %>%
  filter(minyr_EHR > 0, !is.na(combined_type)) %>%
  mutate(period = case_when(
    year == minyr_EHR - 1                        ~ "baseline",
    year >= minyr_EHR + 2 & year <= minyr_EHR + 4 ~ "post"
  )) %>%
  filter(!is.na(period)) %>%
  distinct(DocNPI, period, combined_type) %>%
  group_by(DocNPI, period) %>%
  slice(1) %>%   # in case of any residual duplicates
  ungroup() %>%
  pivot_wider(names_from = period, values_from = combined_type)

cat("== Share with the SAME combined_type at baseline and post ==\n")
stability_check %>%
  filter(!is.na(baseline), !is.na(post)) %>%
  summarise(n = n(), same_type = mean(baseline == post)) %>%
  print()

cat("\n== Baseline -> Post transition matrix ==\n")
stability_check %>%
  filter(!is.na(baseline), !is.na(post)) %>%
  count(baseline, post) %>%
  group_by(baseline) %>%
  mutate(share_within_baseline = n / sum(n)) %>%
  arrange(baseline, desc(n)) %>%
  print(n = 60)

##=============================================================
## STEP 3: Heterogeneity estimation by combined_type (baseline)
##=============================================================

## classify physicians by their BASELINE combined_type (year before
## exposure), same logic as phys_integration_group before
phys_type_baseline <- Physician_Data_EHR %>%
  filter(minyr_EHR > 0, year == minyr_EHR - 1, !is.na(combined_type)) %>%
  distinct(DocNPI, combined_type) %>%
  rename(phys_employer_type = combined_type)

Physician_Data_EHR <- Physician_Data_EHR %>%
  select(-any_of("phys_employer_type")) %>%
  left_join(phys_type_baseline, by = "DocNPI")

Physician_Data_EHR %>%
  distinct(DocNPI, phys_employer_type) %>%
  count(phys_employer_type, sort = TRUE)

##---- Estimation setup ---------------------------------------------------------
GROUPS_TO_TEST <- c("integrated_system_group", "hospital_direct",
                    "staffing_company", "mixed")

restrict_sample <- function(d, yname) {
  if (yname == "retired_ever") {
    d
  } else if (yname %in% c("work_in_office", "pos_office")) {
    d %>% filter(ever_retire == 0)
  } else if (yname %in% c("npi_unq_benes", "claim_per_patient")) {
    d %>% filter(ever_retire == 0, stable_inpat == 1)
  }
}

fit_one <- function(yname, group_filter = NULL) {
  d <- Physician_Data_EHR
  if (!is.null(group_filter)) d <- d %>% filter(phys_employer_type == group_filter)
  d <- restrict_sample(d, yname)
  
  n_docs <- n_distinct(d$DocNPI)
  message(yname, " / ", ifelse(is.null(group_filter), "Main", group_filter),
          ": ", n_docs, " physicians")
  if (n_docs < 50) { message("  -- too few, skipping"); return(NULL) }
  
  tryCatch(
    att_gt(yname = yname, gname = "minyr_EHR", idname = "DocNPI",
           tname = "year", xformla = NULL, data = d,
           est_method = "dr", control_group = "notyettreated",
           clustervars = "DocNPI", anticipation = 0, base_period = "universal"),
    error = function(e) { message("  -- failed: ", e$message); NULL }
  )
}

OUT_LABS <- c(retired_ever      = "Exit",
              work_in_office    = "Work in Office",
              pos_office        = "Frac. Patients in Office",
              npi_unq_benes     = "Number Patients",
              claim_per_patient = "Claims per Patient")

SPECS <- tibble(Sample = c("Main", "Integrated System", "Hospital Direct",
                           "Staffing Co.", "Mixed"),
                group_filter = c(NA, GROUPS_TO_TEST))

res <- expand.grid(yname = names(OUT_LABS), spec_row = seq_len(nrow(SPECS)),
                   stringsAsFactors = FALSE) %>%
  left_join(SPECS %>% mutate(spec_row = row_number()), by = "spec_row") %>%
  pmap_dfr(function(yname, spec_row, Sample, group_filter) {
    mp <- fit_one(yname, if (is.na(group_filter)) NULL else group_filter)
    if (is.null(mp)) return(tibble(Outcome = OUT_LABS[[yname]], Sample = Sample,
                                   coef = NA, se = NA, n = NA))
    a <- aggte(mp, type = "dynamic", na.rm = TRUE)
    tibble(Outcome = OUT_LABS[[yname]], Sample = Sample,
           coef = a$overall.att, se = a$overall.se, n = mp$n)
  })

print(res)

##---- Figure, matching your heterogeneity style --------------------------------
main_means <- purrr::map_dfr(names(OUT_LABS), function(y) {
  d <- restrict_sample(Physician_Data_EHR, y) %>%
    filter(minyr_EHR > 0, year == minyr_EHR - 1)
  tibble(Outcome = OUT_LABS[[y]], mean = mean(d[[y]], na.rm = TRUE))
})

fmt_base <- function(x) ifelse(x >= 100, formatC(x, format = "d", big.mark = ","),
                               ifelse(x >= 1,   sprintf("%.2f", x),
                                      sprintf("%.3f", x)))

res <- res %>%
  left_join(main_means, by = "Outcome") %>%
  mutate(lower = coef - 1.96 * se, upper = coef + 1.96 * se)

facet_labs <- res %>%
  filter(Sample == "Main") %>%
  transmute(Outcome, lab = sprintf("%s\n(baseline: %s)", Outcome, fmt_base(mean)))

res <- res %>%
  left_join(facet_labs, by = "Outcome") %>%
  mutate(lab = factor(lab, levels = facet_labs$lab),
         Sample = factor(Sample, levels = SPECS$Sample),
         Outcome = factor(Outcome, levels = unname(OUT_LABS)))

pal <- c("Main" = "#000000", "Integrated System" = "#4C7FA6",
         "Hospital Direct" = "#A34A5A", "Staffing Co." = "#3D7068",
         "Mixed" = "#B8862E")

p <- ggplot(res, aes(x = Sample, y = coef, colour = Sample)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                linewidth = 0.6, width = 0.25) +
  geom_point(size = 2) +
  facet_wrap(~ lab, scales = "free_y", nrow = 1) +
  scale_colour_manual(values = pal) +
  labs(x = NULL, y = "Point Estimate and 95% CI\n") +
  theme_bw() +
  scale_y_continuous(expand = expansion(mult = 0.15)) +
  theme(
    text          = element_text(size = 14),
    axis.text.y   = element_text(size = 13),
    axis.title.y  = element_text(size = 14),
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    strip.text    = element_text(size = 12.5, lineheight = 1.1),
    legend.text   = element_text(size = 13),
    legend.title  = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1.6, "lines"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )

print(p)
ggsave("Objects/heterog_plot_employertype.pdf", plot = p,
       width = 12, height = 5.5, units = "in")

res %>% distinct(Outcome, Sample, n) %>% arrange(Outcome, Sample) %>% print(n = 30)