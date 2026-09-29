library(showtext)
library(ggplot2)
library(did)
library(ggpubr)
library(fixest)
library(did2s)
library(readxl)
library(HonestDiD)
library(mvtnorm)
library(eventstudyr)
library(dplyr)
library(modelsummary)
library(leebounds)


### --------------------  Sensitivity Analysis --------------------------
#                         Hanna Glenn, Emory University
#                         4/19/2022

# # ORDER :::::::::: 6

# This script reads in "Physician_Data.rds" from data5, the final dataset used in my third year paper. 
# The first portion of the script considers different potential estimators to use as the main specification for the paper.
# In this portion, I only consider "retire" as the dependent outcome and I compare estimators. 

# Read in the main data created in "data5_MDPPAS.R"
Physician_Data <- readRDS(paste0(created_data_path,"Physician_Data_NEW2.rds")) %>%
  mutate(DocNPI=as.numeric(DocNPI))

Physician_Data <- Physician_Data %>%
  mutate(DocNPI=as.double(DocNPI))

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
  group_by(DocNPI) %>%
  mutate(stable_inpat = all(pos_inpat > 0.7, na.rm = TRUE)) %>%
  ungroup()

avg_coef <- function(m, terms) {
  b <- coef(m)
  V <- vcov(m)                      # clustered, as specified in feols()
  keep <- terms[terms %in% names(b)]
  if (!length(keep)) stop("None of `terms` found. Available: ",
                          paste(names(b), collapse = ", "))
  if (length(keep) < length(terms))
    warning("Dropped: ", paste(setdiff(terms, keep), collapse = ", "))
  w   <- rep(1 / length(keep), length(keep))
  est <- sum(w * b[keep])
  se  <- sqrt(drop(t(w) %*% V[keep, keep, drop = FALSE] %*% w))
  c(est = est, se = se, t = est / se, k = length(keep))
}


## DIFFERENT ESTIMATORS ############################################################################################
varlist <- list("retired_ever", "pos_office", "work_in_office", "npi_unq_benes", "claim_per_patient")

# Stacked Regression ###
##---- Sample restrictions, matching the main specification ---
restrict <- function(d, yname) {
  if (yname %in% c("pos_office", "work_in_office"))
    d <- dplyr::filter(d, ever_retire == 0)
  if (yname %in% c("npi_unq_benes", "claim_per_patient"))
    d <- dplyr::filter(d, ever_retire == 0, stable_inpat==1)
  d
}

##---- Build the stacked panel for a given sample -------------
build_stack <- function(d) {
  purrr::map_dfr(2010:2015, function(dd) {
    d %>%
      mutate(stack          = dd,
             treated_unit   = ifelse(minyr_EHR == dd, 1, 0),
             clean_control  = ifelse(minyr_EHR >  dd, 1, 0),
             years_included = ifelse(year >= dd - 1 & year <= dd + 4, 1, 0),
             inclusion      = years_included * (treated_unit + clean_control)) %>%
      filter(inclusion == 1)
  })
}

##---- Estimate, one stack per outcome ------------------------
POST_STK <- c("treated_unit:rel_0", "treated_unit:rel_p1",
              "treated_unit:rel_p2", "treated_unit:rel_p3",
              "treated_unit:rel_p4")

models_stacked <- lapply(unlist(varlist), function(x) {
  d <- build_stack(restrict(Physician_Data_EHR, x))
  feols(xpd(..lhs ~ treated_unit:rel_0 + treated_unit:rel_p1 +
              treated_unit:rel_p2 + treated_unit:rel_p3 + treated_unit:rel_p4 |
              DocNPI^stack + year^stack, ..lhs = x),
        cluster = "DocNPI", data = d)
})
names(models_stacked) <- unlist(varlist)

ATT_stacked <- purrr::imap(models_stacked, function(m, x) {
  a <- avg_coef(m, POST_STK)
  c(x, round(unname(a["est"]), 5), round(unname(a["se"]), 5),
    "Stacked Regression")
})

stacked_values <- as.data.frame(do.call(rbind, ATT_stacked)) %>%
  dplyr::rename(Variable=V1, ATT=V2, SE=V3, Specification=V4) %>%
  mutate(Variable=ifelse(Variable=="retired_ever", "Exit",Variable),
         Variable=ifelse(Variable=="pos_office", "Fraction in Office",Variable),
         Variable=ifelse(Variable=="work_in_office", "Work in Office",Variable),
         Variable=ifelse(Variable=="npi_unq_benes", "Number Patients",Variable),
         Variable=ifelse(Variable=="claim_per_patient", "Claims per Patient",Variable))


# 2sDid #####
models_2sdid <- lapply(unlist(varlist), function(x) {
  did2s(restrict(Physician_Data_EHR, x),
        yname        = x,
        first_stage  = ~ 0 | DocNPI + year,
        second_stage = ~ rel_m4 + rel_m3 + rel_m2 + rel_0 + rel_p1 +
          rel_p2 + rel_p3 + rel_p4,
        treatment    = "anyEHR_exposed",
        cluster_var  = "DocNPI",
        bootstrap    = TRUE,
        n_bootstraps = 250)
})
names(models_2sdid) <- unlist(varlist)

agg_2sdid <- lapply(models_2sdid, function(x) aggregate(x, agg = "(rel_p)"))

LABS_SHORT <- c(retired_ever      = "Exit",
                pos_office        = "Fraction in Office",
                work_in_office    = "Work in Office",
                npi_unq_benes     = "Number Patients",
                claim_per_patient = "Claims per Patient")

ATT_2sdid <- purrr::imap(agg_2sdid, function(a, x) {
  c(unname(LABS_SHORT[x]), round(a[[1]], 5), round(a[[2]], 5), "Two-Stage DiD")
})

tsdid_values <- as.data.frame(do.call(rbind, ATT_2sdid)) %>%
  dplyr::rename(Variable = V1, ATT = V2, SE = V3, Specification = V4)

##=============================================================
## TWFE: aggregate post-treatment coefficients into one ATT
##=============================================================

POST <- c("rel_p1", "rel_p2", "rel_p3", "rel_p4")
PRE  <- c("rel_m4", "rel_m3", "rel_m2")
VAR_MAP <- c(retired_ever      = "Exit",
             pos_office        = "Fraction in Office",
             work_in_office    = "Work in Office",
             npi_unq_benes     = "Number Patients",
             claim_per_patient = "Claims per Patient")

## Wald test that a set of coefficients are jointly zero
wald_terms <- function(b, V, terms) {
  terms <- intersect(terms, names(b))
  if (!length(terms)) return(c(W = NA, df = NA, p = NA))
  bb <- b[terms]; VV <- V[terms, terms, drop = FALSE]
  W  <- as.numeric(t(bb) %*% MASS::ginv(VV) %*% bb)
  df <- qr(VV)$rank
  c(W = W, df = df, p = 1 - pchisq(W, df))
}

twfe_models <- lapply(unlist(varlist), function(x) {
  feols(xpd(..lhs ~ rel_m4 + rel_m3 + rel_m2 + rel_0 + rel_p1 +
              rel_p2 + rel_p3 + rel_p4 | DocNPI + year, ..lhs = x),
        cluster = "DocNPI",
        data = restrict(Physician_Data_EHR, x))
})
names(twfe_models) <- unlist(varlist)

twfe_values <- purrr::imap_dfr(twfe_models, function(m, x) {
  a <- avg_coef(m, POST)
  tibble(Variable  = unname(VAR_MAP[x]),
         coef      = unname(a["est"]),
         se        = unname(a["se"]),
         Estimator = "TWFE")
})

##=============================================================
## Specification chart: alternative staggered DiD estimators
##   Main (CS) estimates and baseline means pulled from the
##   fitted objects rather than hardcoded.
##
## Requires: models, models_agg, varlist, Physician_Data_EHR,
##           stacked_values, tsdid_values


##---- Map estimation variable -> display label ---------------
VAR_MAP <- c(retired_ever      = "Exit",
             pos_office        = "Fraction in Office",
             work_in_office    = "Work in Office",
             npi_unq_benes     = "Number Patients",
             claim_per_patient = "Claims per Patient")

##---- Baseline means -----------------------------------------
var_means <- imap_dfr(VAR_MAP, function(label, yname) {
  m <- Physician_Data_EHR %>%
    ungroup() %>%
    restrict(yname) %>%
    dplyr::filter(year == minyr_EHR - 1) %>%
    dplyr::summarise(m = mean(.data[[yname]], na.rm = TRUE)) %>%
    dplyr::pull(m)
  tibble(Variable = label, mean = m)
})

##---- Main (CS) estimates, pulled from models_agg ------------
## models_agg[[i]] is list(all=, young=, old=); take `all`.
vl <- unlist(varlist)

cs_values <- map_dfr(seq_along(Models_agg), function(i) {
  yname <- vl[i]
  if (!yname %in% names(VAR_MAP)) return(NULL)   # skip outcomes not charted
  a <- Models_agg[[i]]$all$agg
  tibble(Variable = unname(VAR_MAP[yname]),
         coef     = a$overall.att,
         se       = a$overall.se,
         Estimator = "Main (CS)")
})

##---- Combine and scale --------------------------------------
alt_values <- bind_rows(
  stacked_values %>%
    dplyr::rename(coef = ATT, se = SE, Estimator = Specification) %>%
    mutate(coef = as.numeric(coef), se = as.numeric(se)),
  tsdid_values %>%
    dplyr::rename(coef = ATT, se = SE, Estimator = Specification) %>%
    mutate(coef = as.numeric(coef), se = as.numeric(se)),
  twfe_values,
  cs_values
) %>%
  mutate(Variable = as.character(Variable)) 

##---- Combine and scale --------------------------------------
estimators <- alt_values %>%
  left_join(var_means, by = "Variable") %>%
  mutate(estimate_perc = coef / mean,
         upper = (coef + 1.96 * se) / mean,
         lower = (coef - 1.96 * se) / mean,
         Variable  = factor(Variable, levels = unname(VAR_MAP)),
         Estimator = factor(Estimator,
                            levels = c("Main (CS)", "TWFE",
                                       "Stacked Regression", "Two-Stage DiD"))) %>%
  arrange(Variable, Estimator)


dodge <- position_dodge(width=0.5)
##---- Baseline in the strip label ----------------------------
fmt_base <- function(x) ifelse(x >= 100, formatC(x, format = "d", big.mark = ","),
                               ifelse(x >= 1,   sprintf("%.2f", x),
                                      sprintf("%.3f", x)))

facet_labs <- var_means %>%
  mutate(lab = sprintf("%s\n(baseline: %s)", Variable, fmt_base(mean))) %>%
  arrange(factor(Variable, levels = unname(VAR_MAP)))

estimators <- estimators %>%
  left_join(select(facet_labs, Variable, lab), by = "Variable") %>%
  mutate(lab = factor(lab, levels = facet_labs$lab))

##---- Plot ---------------------------------------------------
pal <- c("Main (CS)"          = "#000000",
         "TWFE"               = "#16324F",
         "Stacked Regression" = "#A34A5A",
         "Two-Stage DiD"      = "#3D7068")

shp <- c("Main (CS)"          = 16,   # filled circle
         "TWFE"               = 17,   # filled triangle
         "Stacked Regression" = 15,   # filled square
         "Two-Stage DiD"      = 18)   # filled diamond

p <- ggplot(estimators, aes(x = Estimator, y = coef,
                            colour = Estimator, shape = Estimator)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") +
  geom_errorbar(aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se),
                linewidth = 0.6, width = 0.25) +
  geom_point(size = 2.4) +
  facet_wrap(~ lab, scales = "free_y", nrow = 1) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values = shp) +
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
ggsave("Objects/estimators_plot.pdf", plot = p,
       width = 11, height = 5, units = "in")



## Roth Prettrends Stuff ---------------------------------- ####

#(Need results from analysis loaded)

##=============================================================
## HonestDiD sensitivity: relative magnitude restrictions
##   Table 1 (main text): breakdown Mbar by outcome
##   Table 2 (appendix):  full grid of robust CIs
##
## Requires: models (list of att_gt fits with $all/$young/$old),
##           varlist, HonestDiD
##=============================================================
library(HonestDiD)

vl <- unlist(varlist)

LABS <- c(retired_ever      = "Exit",
             pos_office        = "Fraction in Office",
             work_in_office    = "Work in Office",
             npi_unq_benes     = "Number Patients",
             claim_per_patient = "Claims per Patient")

## Event window used for the sensitivity exercise. Deeper leads
## make the pre-treatment covariance ill-conditioned.
MIN_E <- -3
MAX_E <-  4

## Grid: fine enough to locate breakdown, coarse enough to print
MBAR_FINE  <- seq(0.05, 2, by = 0.05)     # for finding breakdown
MBAR_TABLE <- c(0.25, 0.5, 0.75, 1, 1.5, 2)  # for the appendix table
E_POST <- 2

GROUPS <- c(all = "All", young = "Age $<$ 60", old = "Age $\\geq$ 60")

##---- Run HonestDiD for one aggregated object ----------------
run_honest <- function(mp, mbarvec, e = E_POST) {
  
  ag <- tryCatch(did::aggte(mp, type = "dynamic", na.rm = TRUE,
                            min_e = MIN_E, max_e = MAX_E),
                 error = function(err) NULL)
  if (is.null(ag)) return(NULL)
  
  inf <- ag$inf.function$dynamic.inf.func.e
  if (is.null(inf)) return(NULL)
  n   <- nrow(inf)
  V   <- t(inf) %*% inf / (n * n)
  
  npre  <- sum(ag$egt < 0)
  npost <- nrow(V) - npre
  if (npre < 1 || npost <= e) return(NULL)
  
  lvec <- HonestDiD::basisVector(index = e + 1, size = npost)
  
  ## search grid scaled to the outcome, not hardcoded to [-1, 1]
  span <- max(abs(ag$att.egt), na.rm = TRUE) +
    10 * max(ag$se.egt,  na.rm = TRUE)
  
  orig <- tryCatch(
    HonestDiD::constructOriginalCS(betahat = ag$att.egt, sigma = V,
                                   numPrePeriods = npre,
                                   numPostPeriods = npost, l_vec = lvec),
    error = function(err) NULL)
  if (is.null(orig)) return(NULL)
  
  rob <- tryCatch(
    HonestDiD::createSensitivityResults_relativeMagnitudes(
      betahat = ag$att.egt, sigma = V,
      numPrePeriods = npre, numPostPeriods = npost,
      method = "C-LF", l_vec = lvec, Mbarvec = mbarvec,
      alpha = 0.05, gridPoints = 1000,
      grid.lb = -span, grid.ub = span),
    error = function(err) NULL)
  if (is.null(rob)) return(NULL)
  
  ## flag any bound sitting on the grid edge (would be truncated)
  edge <- any(abs(abs(c(rob$lb, rob$ub)) - span) < 1e-6, na.rm = TRUE)
  
  list(robust_ci = rob,
       benchmark = max(abs(ag$att.egt[ag$egt < 0]), na.rm = TRUE),
       orig_lb   = as.numeric(orig$lb),
       orig_ub   = as.numeric(orig$ub),
       npre      = npre,
       grid_edge = edge)
}

##---- Breakdown: smallest Mbar whose robust CI covers zero ---
breakdown <- function(rob) {
  d <- rob %>%
    filter(Mbar > 0) %>%
    arrange(Mbar) %>%
    mutate(covers0 = lb <= 0 & ub >= 0)
  if (all(is.na(d$lb))) return(c(bd = NA, last_sig = NA))
  if (!any(d$covers0, na.rm = TRUE))
    return(c(bd = NA, last_sig = max(d$Mbar, na.rm = TRUE)))
  first_cover <- d$Mbar[which(d$covers0)[1]]
  below <- d$Mbar[d$Mbar < first_cover & !d$covers0]
  c(bd = first_cover, last_sig = if (length(below)) max(below) else NA)
}

##---- Loop over outcomes -------------------------------------
res <- map_dfr(seq_along(models), function(i) {
  
  yname <- vl[i]
  if (!yname %in% names(LABS)) return(NULL)
  
  fine <- run_honest(models[[i]]$all, MBAR_FINE)
  tab  <- run_honest(models[[i]]$all, MBAR_TABLE)
  
  if (is.null(fine) || is.null(tab)) {
    return(tibble(outcome_raw = yname,
                  Outcome     = unname(LABS[yname]),
                  orig_lb = NA_real_, orig_ub = NA_real_,
                  benchmark = NA_real_, npre = NA_integer_,
                  bd = NA_real_, last_sig = NA_real_,
                  grid_edge = NA, grid = list(NULL)))
  }
  
  b <- breakdown(fine$robust_ci)
  
  tibble(outcome_raw = yname,
         Outcome     = unname(LABS[yname]),
         orig_lb     = fine$orig_lb,
         orig_ub     = fine$orig_ub,
         benchmark   = fine$benchmark,
         npre        = fine$npre,
         bd          = unname(b["bd"]),
         last_sig    = unname(b["last_sig"]),
         grid_edge   = fine$grid_edge,
         grid        = list(tab$robust_ci))
}) %>%
  mutate(Outcome = factor(Outcome, levels = unname(LABS))) %>%
  arrange(Outcome)

##---- Formatting helpers -------------------------------------
fmt_num <- function(x) ifelse(is.na(x), "---",
                              ifelse(abs(x) >= 100, formatC(x, format = "d", big.mark = ","),
                                     ifelse(abs(x) >= 1,   sprintf("%.2f", x),
                                            sprintf("%.4f", x))))

##=============================================================
## TABLE 1 (main text): breakdown values
##=============================================================
main_tab <- res %>%
  transmute(Outcome,
            `Original 95\\% CI` = sprintf("[%s, %s]", fmt_num(orig_lb), fmt_num(orig_ub)),
            `Largest pre-treatment violation` = fmt_num(benchmark),
            `Breakdown $\\bar{M}$` = ifelse(is.na(bd), "$>2$", sprintf("%.2f", bd)))

out1 <- kbl(main_tab, format = "latex", booktabs = TRUE, escape = FALSE,
            align = c("l", "c", "c", "c"),
            caption = "Robustness to violations of parallel trends",
            label = "honest_breakdown") %>%
  kable_styling(latex_options = "hold_position", full_width = FALSE) %>%
  kableExtra::footnote(general = paste(
    "Sensitivity analysis follows Rambachan and Roth (2023).",
    "The restriction bounds any post-treatment violation of parallel trends",
    "at $\\bar{M}$ times the largest violation observed before treatment.",
    "The breakdown value is the smallest $\\bar{M}$ at which the robust",
    "confidence interval includes zero; values are searched on a grid of",
    "width 0.05 up to $\\bar{M}=2$.",
    sprintf("The event window is restricted to $e \\\\in [%d, %d]$.", MIN_E, MAX_E)),
    threeparttable = TRUE, escape = FALSE)

##=============================================================
## TABLE 2 (appendix): full grid
##=============================================================
grid_tab <- res %>%
  filter(!map_lgl(grid, is.null)) %>%
  select(Outcome, orig_lb, orig_ub, grid) %>%
  mutate(rows = map(grid, ~ .x %>%
                      filter(Mbar > 0) %>%
                      transmute(Mbar, ci = sprintf("[%s, %s]",
                                                   fmt_num(lb), fmt_num(ub))))) %>%
  select(-grid) %>%
  unnest(rows) %>%
  mutate(Mbar = sprintf("%.2f", Mbar)) %>%
  select(Outcome, orig_lb, orig_ub, Mbar, ci) %>%
  pivot_wider(names_from = Mbar, values_from = ci) %>%
  mutate(Original = sprintf("[%s, %s]", fmt_num(orig_lb), fmt_num(orig_ub))) %>%
  select(Outcome, Original, everything(), -orig_lb, -orig_ub)

out2 <- kbl(grid_tab, format = "latex", booktabs = TRUE, escape = FALSE,
            align = c("l", rep("c", ncol(grid_tab) - 1)),
            caption = "Robust confidence intervals under relative magnitude restrictions",
            label = "honest_grid") %>%
  add_header_above(c(" " = 2,
                     "$\\\\bar{M}$" = ncol(grid_tab) - 2), escape = FALSE) %>%
  kable_styling(latex_options = c("scale_down", "hold_position"),
                font_size = 9, full_width = FALSE) %>%
  kableExtra::footnote(general = paste(
    "Robust 95\\% confidence intervals from Rambachan and Roth (2023),",
    "computed under the restriction that any post-treatment violation of",
    "parallel trends is at most $\\bar{M}$ times the largest pre-treatment",
    "violation. The Original column reports the conventional confidence",
    "interval, which assumes parallel trends holds exactly.",
    sprintf("The event window is restricted to $e \\\\in [%d, %d]$.", MIN_E, MAX_E)),
    threeparttable = TRUE, escape = FALSE)

##---- Write ---------------------------------------------------
writeLines(as.character(out1), file.path("Table Code", "honest_breakdown.tex"))
writeLines(as.character(out2), file.path("Table Code", "honest_grid.tex"))




# LEE (2009) BOUNDS ------------------------------------------------------------------------------------ #### 

library(leebounds)

# work in office indicator
group_list <- c(2010, 2011, 2012, 2013, 2014)

office_lee <- lapply(group_list, function(x){
    leedata <- Physician_Data_EHR %>%
      filter(minyr_EHR>=x) %>%
      mutate(treat = ifelse(minyr_EHR==x,1,0)) %>%
      mutate(selection = ifelse(ever_retire==1,0,1))
    
    leedata <- data.frame(treat=leedata$treat,selection=leedata$selection,outcome=leedata$work_in_office)
    
    return(GetBounds(leebounds(leedata)))
})

office_frac_lee <- lapply(group_list, function(x){
  leedata <- Physician_Data_EHR %>%
    filter(minyr_EHR>=x) %>%
    mutate(treat = ifelse(minyr_EHR==x,1,0)) %>%
    mutate(selection = ifelse(ever_retire==1,0,1))
  
  leedata <- data.frame(treat=leedata$treat,selection=leedata$selection,outcome=leedata$pos_office)
  
  return(GetBounds(leebounds(leedata)))
})

pat_count_lee <- lapply(group_list, function(x){
  leedata <- Physician_Data_EHR %>%
    filter(minyr_EHR>=x) %>%
    mutate(treat = ifelse(minyr_EHR==x,1,0)) %>%
    mutate(selection = ifelse(ever_retire==1 & stable_inpat==1,0,1))
  
  leedata <- data.frame(treat=leedata$treat,selection=leedata$selection,outcome=leedata$npi_unq_benes)
  
  return(GetBounds(leebounds(leedata)))
})

claim_lee <- lapply(group_list, function(x){
  leedata <- Physician_Data_EHR %>%
    filter(minyr_EHR>=x) %>%
    mutate(treat = ifelse(minyr_EHR==x,1,0)) %>%
    mutate(selection = ifelse(ever_retire==1 & stable_inpat==1,0,1)) %>%
    filter(!is.na(claim_per_patient))
  
  leedata <- data.frame(treat=leedata$treat,selection=leedata$selection,outcome=leedata$claim_per_patient)
  
  return(GetBounds(leebounds(leedata)))
})

office_ind_lower <- mean(c(office_lee[[1]][1], office_lee[[2]][1], office_lee[[3]][1],office_lee[[4]][1], office_lee[[5]][1]))
office_ind_upper <- mean(c(office_lee[[1]][2], office_lee[[2]][2], office_lee[[3]][2],office_lee[[4]][2], office_lee[[5]][2]))

office_frac_lower <- mean(c(office_frac_lee[[1]][1], office_frac_lee[[2]][1], office_frac_lee[[3]][1],office_frac_lee[[4]][1], office_frac_lee[[5]][1]))
office_frac_upper <- mean(c(office_frac_lee[[1]][2], office_frac_lee[[2]][2], office_frac_lee[[3]][2],office_frac_lee[[4]][2], office_frac_lee[[5]][2]))

pat_lower <- mean(c(pat_count_lee[[1]][1], pat_count_lee[[2]][1], pat_count_lee[[3]][1],pat_count_lee[[4]][1], pat_count_lee[[5]][1]))
pat_upper <- mean(c(pat_count_lee[[1]][2], pat_count_lee[[2]][2], pat_count_lee[[3]][2],pat_count_lee[[4]][2], pat_count_lee[[5]][2]))

claim_lower <- mean(c(claim_lee[[1]][1], claim_lee[[2]][1], claim_lee[[3]][1],claim_lee[[4]][1], claim_lee[[5]][1]))
claim_upper <- mean(c(claim_lee[[1]][2], claim_lee[[2]][2], claim_lee[[3]][2],claim_lee[[4]][2], claim_lee[[5]][2]))

lower <- as.data.frame(c(office_frac_lower, pat_lower, claim_lower))
upper <- as.data.frame(c(office_frac_upper, pat_upper, claim_upper))

lee_plot <- cbind(lower,upper)
colnames(lee_plot) <- c("lower", "upper")

lee_table <- lee_plot %>%
  mutate(variable = c("Fraction in Office", "Num Patients", "Claims per Patient")) %>%
  select(variable, lower, upper)

lee_table_code <- knitr::kable(lee_table,
                               format="latex",
                               table.envir="table",
                               caption="\\label{leebouns}Lee Bounds for Selection",
                               col.names=c("Variable","Lower","Upper"),
                               digits=3,
                               booktabs=TRUE,
                               escape=F,
                               align=c("l","c","c"),
                               position="ht!") 
write(lee_table_code, "Table Code/leebounds.tex")





