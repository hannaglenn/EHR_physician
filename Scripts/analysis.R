library(did)
library(dplyr)
library(ggplot2)
library(readr)
library(fixest)
library(did2s)
library(ggpubr)
library(tidyr)
library(extrafont)
library(purrr)
library(kableExtra)

#remotes::install_version("did", version = "2.1.2", type = "source",
#                         repos = "https://cran.r-project.org")

# ------------------------------------- ANALYSIS  ------------------------------------
#                                       Hanna Glenn, Emory University
#                                       1/31/2022


# This script reads in "Physician_Data.rds" from data5, the final dataset used in my third year paper. 
# The first portion of the script considers different potential estimators to use as the main specification for the paper.

# Make sure to have "models" and "models_agg" from analysis loaded 

# Read in the main data created in "data5_MDPPAS.R"
Physician_Data <- readRDS(paste0(created_data_path,"Physician_Data_NEW2.rds")) %>%
  ungroup() 

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
  mutate(claims_inpat  = claim_count_total * pos_inpat,
         claims_office = claim_count_total * pos_office,
         claims_opd    = claim_count_total * pos_opd) %>%
  group_by(DocNPI) %>%
  mutate(stable_inpat = all(pos_inpat > 0.7, na.rm = TRUE)) %>%
  ungroup()



## ANALYSIS ---------------------------------------------- #########
## Write results for each variable in a loop and create graphs ----- ##

varlist <- list("retired_ever", "pos_office", "work_in_office", "npi_unq_benes", "claim_per_patient", "claims_inpat", "claims_office", "claims_opd")

# Get results for ATTGT 
models <- lapply(varlist, function(x) {
  base <- if (x == "retired_ever" | x=="retire") dplyr::filter(Physician_Data_EHR)
  else if (x %in% c("pos_office", "work_in_office", "non_hosp_employer", "system_employer", "solo_employer"))
    dplyr::filter(Physician_Data_EHR, ever_retire == 0)
  else dplyr::filter(Physician_Data_EHR, ever_retire == 0 & stable_inpat==1)
  
  fit <- function(d) att_gt(
    yname = x, gname = "minyr_EHR", idname = "DocNPI", tname = "year",
    xformla = NULL, data = d, est_method = "dr",
    control_group = "notyettreated", clustervars = "DocNPI",
    anticipation = 0, base_period = "universal"
  )
  
  list(all   = fit(base),
       young = fit(dplyr::filter(base, max_age <  60)),
       old   = fit(dplyr::filter(base, max_age >= 60)))
})
names(models) <- unlist(varlist)

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

fmt_p <- function(p) if (is.na(p)) "n/a" else if (p < 0.01) "<0.01" else sprintf("%.2f", p)

# Aggregate the previous results
Models_agg <- lapply(models, function(x) {
  lapply(list(all = x$all, young = x$young, old = x$old), function(m) {
    agg <- aggte(m, type = "dynamic", na.rm = TRUE, min_e = -4, max_e = 4)
    w   <- wald_pre(m)
    list(agg = agg, p = unname(w["p"]), df = unname(w["df"]),
         att = agg$overall.att, se = agg$overall.se,
         p_att = 2 * pnorm(-abs(agg$overall.att / agg$overall.se)))
  })
})

summ <- do.call(rbind, lapply(names(Models_agg), function(v)
  do.call(rbind, lapply(c("all", "young", "old"), function(g) {
    m <- Models_agg[[v]][[g]]
    data.frame(outcome = v, group = g,
               att = round(m$att, 3), se = round(m$se, 3),
               p = round(m$p_att, 3))
  }))))

print(summ)

## Baselines for figure subtitles
SUB_TYPE <- c(retired_ever = "cumulative", retire = "cumulative",
              pos_office = "level", work_in_office = "level",
              npi_unq_benes = "level", claim_per_patient = "level",
              claims_inpat = "level", claims_office = "level", claims_opd = "level",
              non_hosp_employer = "level", system_employer = "level",
              solo_employer = "level")

## how each outcome's baseline should be printed
fmt_base <- function(x, out_name) {
  if (out_name %in% c("retired_ever", "retire"))    sprintf("%.1f%%", 100 * x)
  else if (out_name %in% c("npi_unq_benes"))        formatC(x, format = "d", big.mark = ",")
  else if (out_name == "claim_per_patient")         sprintf("%.2f", x)
  else                                              sprintf("%.3f", x)
}

## same sample restrictions as the att_gt call
base_sample <- function(out_name) {
  if (out_name %in% c("retired_ever", "retire")) Physician_Data_EHR
  else if (out_name %in% c("pos_office", "work_in_office", "non_hosp_employer", "solo_employer"))
    dplyr::filter(Physician_Data_EHR, ever_retire == 0)
  else dplyr::filter(Physician_Data_EHR, ever_retire == 0 & stable_inpat==1)
}

## one baseline per outcome x group
get_baseline <- function(out_name, grp, agg, horizon = 4) {
  d <- base_sample(out_name) %>%
    ungroup() %>%
    dplyr::filter(if (grp == "young") max_age <  60
                  else if (grp == "old") max_age >= 60
                  else TRUE)
  
  if (SUB_TYPE[[out_name]] == "cumulative") {
    realized <- d %>%
      dplyr::filter(minyr_EHR > 0, year - minyr_EHR == horizon) %>%
      dplyr::summarise(m = mean(.data[[out_name]], na.rm = TRUE)) %>% dplyr::pull(m)
    att_h <- agg$att.egt[agg$egt == horizon]
    if (!length(att_h)) return(NA_real_)
    realized - att_h
  } else {
    d %>%
      dplyr::filter(minyr_EHR > 0, year == minyr_EHR - 1) %>%
      dplyr::summarise(m = mean(.data[[out_name]], na.rm = TRUE)) %>% dplyr::pull(m)
  }
}

baselines <- lapply(seq_along(varlist), function(i) {
  v <- varlist[[i]]
  vapply(c("all", "young", "old"),
         function(g) get_baseline(v, g, Models_agg[[i]][[g]]$agg),
         numeric(1))
})

## subtitle text
make_subtitle <- function(out_name, vals, groups = "all") {
  cum <- SUB_TYPE[[out_name]] == "cumulative"
  lead <- if (cum) "Counterfactual mean at year 4: " else "Pre-treatment mean: "
  if (identical(groups, "all")) {
    paste0(lead, fmt_base(vals[["all"]], out_name))
  } else {
    paste0(lead,
           fmt_base(vals[["young"]], out_name), " (age < 60); ",
           fmt_base(vals[["old"]],   out_name), " (age >= 60)")
  }
}

graph_data <- lapply(seq_along(Models_agg), function(i) {
  x <- Models_agg[[i]]
  v <- varlist[[i]]
  labs_map <- c(all = "All", young = "Age < 60", old = "Age >= 60")
  
  dat <- dplyr::bind_rows(lapply(names(labs_map), function(g) {
    a <- x[[g]]$agg
    data.frame(year = a$egt, att = a$att.egt, se = a$se.egt,
               group = labs_map[[g]])
  })) %>%
    dplyr::mutate(upper = att + 1.96 * se,
                  lower = att - 1.96 * se)
  
  ## restrict plotted window; appendix tables keep the full range
  dat <- dplyr::filter(dat, year >= -5, year <= 4)
  
  # ## drop degenerate leads for the older group only
  # if (v %in% c("retired_ever", "retire")) {
  #   dat <- dplyr::filter(dat, !(group == "Age >= 60" & year < -3))
  # }
  
  dat <- dat %>%
    dplyr::mutate(year = factor(year, levels = sort(unique(year))))
  
  list(data = dat, p_all = x$all$p, p_young = x$young$p, p_old = x$old$p)
})

dodge <- position_dodge(width=0.3) 
pal <- c("Age < 60" = "#16324F", "Age >= 60" = "#A34A5A")

base_theme <- theme_light() +
  theme(
    text                = element_text(size = 15),
    axis.text           = element_text(size = 13),
    axis.title          = element_text(size = 14),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor    = element_blank(),
    panel.grid.major.y  = element_line(linewidth = 0.3, colour = "grey65"),
    panel.border        = element_rect(colour = "grey30", fill = NA, linewidth = 0.5),
    plot.subtitle       = element_text(size = 13, colour = "grey25"),
    plot.caption        = element_text(hjust = 0, size = 11),
    legend.position     = "bottom",
    legend.title        = element_blank(),
    legend.text         = element_text(size = 15)
  )

graphs <- lapply(seq_along(graph_data), function(i) {
  
  x  <- graph_data[[i]]
  v  <- varlist[[i]]
  bl <- baselines[[i]]
  
  ## common y-limits across both panels
  yr <- range(c(x$data$lower, x$data$upper), na.rm = TRUE)
  yr <- yr + c(-1, 1) * 0.04 * diff(yr)
  
  all_data  <- dplyr::filter(x$data, group == "All")
  ages_data <- dplyr::filter(x$data, group != "All")
  
  all <- ggplot(all_data, aes(year, att)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") +
    geom_vline(xintercept = "0", linetype = "dashed", colour = "grey30") +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  width = 0, linewidth = 1.1, colour = "black") +
    geom_point(size = 2.5, colour = "black") +
    scale_y_continuous(limits = yr, breaks = scales::pretty_breaks(n = 8)) +
    labs(x = NULL, y = "Point Estimate and 95% CI\n",
         subtitle = make_subtitle(v, bl, "all"),
         caption = paste0("Pre-trends test p-value: ", fmt_p(x$p_all))) +
    base_theme + theme(legend.position = "none")
  
  ages <- ggplot(ages_data, aes(year, att, colour = group, shape = group)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") +
    geom_vline(xintercept = "0", linetype = "dashed", colour = "grey30") +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  width = 0, linewidth = 1.1, position = dodge) +
    geom_point(size = 3, position = dodge) +
    scale_colour_manual(values = pal) +
    scale_shape_manual(values = c("Age < 60" = 16, "Age >= 60" = 17)) +
    scale_y_continuous(limits = yr, breaks = scales::pretty_breaks(n = 8)) +
    labs(x = NULL, y = "Point Estimate and 95% CI\n",
         subtitle = make_subtitle(v, bl, "groups"),
         caption = paste0("Pre-trends test p-value (age < 60): ",  fmt_p(x$p_young),
                          "\nPre-trends test p-value (age >= 60): ", fmt_p(x$p_old))) +
    base_theme
  
  list(all = all, ages = ages)
})
names(graphs) <- unlist(varlist)

# Save plots 
FILE_STUB <- c(retired_ever      = "retire",
               pos_office        = "officefrac",
               work_in_office    = "officeind",
               npi_unq_benes     = "patient",
               claim_per_patient = "claim_per_patient")

for (v in names(graphs)) {
  stub <- if (v %in% names(FILE_STUB)) FILE_STUB[[v]] else v
  ggsave(sprintf("Objects/%s_plot_all_new.pdf",  stub),
         plot = graphs[[v]][["all"]],  width = 9, height = 4,   units = "in")
  ggsave(sprintf("Objects/%s_plot_ages_new.pdf", stub),
         plot = graphs[[v]][["ages"]], width = 9, height = 4.5, units = "in")
}



# Create table showing actual values for the appendix

vl <- unlist(varlist)

GROUP_LABS  <- c(all = "All", young = "Age $<$ 60", old = "Age $\\geq$ 60")
GROUP_ORDER <- unname(GROUP_LABS)
OUT_LABS <- c(
  retired_ever   = "Exit from Clinical Practice",
  retire         = "Exit from Clinical Practice (Annual)",
  pos_office     = "Fraction of Patients Seen in Office",
  work_in_office = "Works in an Office",
  npi_unq_benes  = "Number of Patients",
  claim_per_patient = "Claims per Patient"
)


est_tab <- imap_dfr(models, function(x, i) {
  out_name <- vl[as.integer(i)]
  imap_dfr(list(all = x$all, young = x$young, old = x$old), function(m, g) {
    a <- aggte(m, type = "dynamic", na.rm = TRUE)
    tibble(outcome    = out_name,
           group      = GROUP_LABS[[g]],
           event_time = a$egt,
           att        = a$att.egt,
           se         = a$se.egt)
  })
}) %>%
  mutate(base  = is.na(se),
         lower = att - 1.96 * se,
         upper = att + 1.96 * se)

count_outcome <- "retired_ever"

counts <- bind_rows(
  Physician_Data_EHR %>% ungroup() %>% mutate(grp = GROUP_LABS[["all"]]),
  Physician_Data_EHR %>% ungroup() %>% filter(max_age <  60) %>%
    mutate(grp = GROUP_LABS[["young"]]),
  Physician_Data_EHR %>% ungroup() %>% filter(max_age >= 60) %>%
    mutate(grp = GROUP_LABS[["old"]])
) %>%
  filter(minyr_EHR > 0) %>%
  mutate(event_time = year - minyr_EHR) %>%
  group_by(group = grp, event_time) %>%
  summarise(n_docs   = n_distinct(DocNPI),
            n_events = sum(.data[[count_outcome]], na.rm = TRUE),
            .groups  = "drop")

make_ci_table <- function(out_name, with_counts = FALSE) {
  
  d <- est_tab %>%
    filter(outcome == out_name, !base) %>%
    mutate(group = factor(group, levels = GROUP_ORDER)) %>%
    arrange(group, event_time)
  
  if (with_counts) {
    d <- d %>% left_join(counts, by = c("group", "event_time"))
  }
  
  wide <- d %>%
    mutate(
      est = sprintf("%.3f", att),
      ci  = sprintf("[%.3f, %.3f]", lower, upper),
      wid = sprintf("%.3f", upper - lower)
    ) %>%
    mutate(across(any_of(c("n_docs", "n_events")),
                  ~ formatC(., format = "d", big.mark = ","))) %>%
    select(group, event_time, est, ci, wid,
           any_of(c("n_docs", "n_events"))) %>%
    pivot_longer(-c(group, event_time),
                 names_to = "row_type", values_to = "val") %>%
    mutate(row_type = factor(
      row_type,
      levels = c("est", "ci", "wid", "n_docs", "n_events"),
      labels = c("Estimate", "95\\% CI", "CI width",
                 "Physicians", "Events"))) %>%
    filter(!is.na(val)) %>%
    pivot_wider(names_from = event_time, values_from = val) %>%
    arrange(group, row_type)
  
  et_cols <- setdiff(names(wide), c("group", "row_type"))
  
  body <- wide %>%
    mutate(label = as.character(row_type)) %>%
    select(label, all_of(et_cols))
  
  blocks <- wide %>% count(group)
  
  lab <- if (out_name %in% names(OUT_LABS)) OUT_LABS[[out_name]] else out_name
  
  tab <- kbl(body, format = "latex", booktabs = TRUE, escape = FALSE,
             col.names = c("", et_cols),
             align = c("l", rep("c", length(et_cols))),
             caption = paste0("Event-time estimates and confidence intervals: ", lab),
             label = paste0("ci_", out_name)) 
  
  start <- 1
  for (i in seq_len(nrow(blocks))) {
    g <- as.character(blocks$group[i]); n <- blocks$n[i]
    tab <- pack_rows(tab, g, start, start + n - 1, escape = FALSE)
    start <- start + n
  }
  
  tab <- tab %>%
    add_header_above(c(" " = 1,
                       "Event time relative to first EHR exposure" = length(et_cols))) %>%
    kable_styling(latex_options = "hold_position", font_size = 9) %>%
    landscape()
  
  return(tab)
}

for (v in vl) {
  wc  <- v %in% c("retire", "retired_ever")
  tex <- make_ci_table(v, with_counts = wc)
  writeLines(as.character(tex),
             file.path("./Table Code", paste0("ci_", v, ".tex")))
}

cat("Wrote", length(vl), "tables to Table Code/\n")


# Save group plots for appendix
ggdid_theme <- theme_bw() +
  theme(
    text               = element_text(size = 14, colour = "black"),
    axis.text          = element_text(size = 12),
    axis.title         = element_text(size = 14),
    strip.text         = element_text(size = 12),
    strip.background   = element_rect(fill = "grey92", colour = "grey40"),
    plot.title         = element_text(size = 15, hjust = 0),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, colour = "grey65"),
    panel.border       = element_rect(colour = "grey30", fill = NA, linewidth = 0.5),
    legend.position    = "none"
  )

pal_prepost <- c("#A34A5A", "#16324F")   # pre = burgundy, post = navy

save_ggdid <- function(mp, title, file, w = 9, h = 11) {
  p <- ggdid(mp) +
    scale_colour_manual(values = pal_prepost, name = "") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
    labs(title = title, y = "Point Estimate and 95% CI\n", x = "\nYear") +
    ggdid_theme
  ggsave(filename = file, plot = p, width = w, height = h, units = "in")
  p
}

group_plots <- list(
  list(mp = models[[1]]$old,
       title = "Dis-aggregated Effects of EHR Exposure on Exiting Clinical Work",
       file = "Objects/retire_group.pdf"),
  list(mp = models[[2]]$all,
       title = "Dis-aggregated Effects of EHR Exposure on Fraction of Patients Seen in Office",
       file = "Objects/officefrac_group.pdf"),
  list(mp = models[[3]]$all,
       title = "Dis-aggregated Effects of EHR Exposure on Working in an Office",
       file = "Objects/officeind_group.pdf"),
  list(mp = models[[4]]$all,
       title = "Dis-aggregated Effects of EHR Exposure on Patient Count",
       file = "Objects/patient_group.pdf"),
  list(mp = models[[5]]$all,
       title = "Dis-aggregated Effects of EHR Exposure on Claims per Patient",
       file = "Objects/claimperpatient_group.pdf")
)

invisible(lapply(group_plots, function(g) save_ggdid(g$mp, g$title, g$file)))



## Claim-type analysis, 2013-2015---------------------------


varlist <- list("initial_hosp_inpat", "subs_hosp_inpat", "hosp_disch_day",
                "new_patient_office", "est_patient_office", "hosp_obs_care",
                "subs_obs_care", "crit_care_delivery")
vl <- unlist(varlist)

LABS <- c(initial_hosp_inpat = "Initial Care: Hospital",
          subs_hosp_inpat    = "Subsequent Care: Hospital",
          hosp_disch_day     = "Discharge: Hospital",
          hosp_obs_care      = "Observation: Hospital",
          subs_obs_care      = "Subsequent Observation: Hospital",
          crit_care_delivery = "Critical Care Delivery",
          new_patient_office = "New Patient: Office",
          est_patient_office = "Established Patient: Office")

base_sample <- Physician_Data %>%
  ungroup() %>%
  mutate(bundled = map2_lgl(doc_sc_years, minyr_EHR,
                            ~ !is.null(.x) && .y > 0 && any(abs(.x - .y) <= 0))) %>%
  group_by(DocNPI) %>%
  mutate(drop_md = any(bundled)) %>%
  ungroup() %>%
  filter(!drop_md,
         first_mdppas_yr == 2009,
         ever_retire == 0,
         year >= 2012, year <= 2015) %>%
  group_by(DocNPI) %>%
  mutate(stable_inpat = all(pos_inpat > 0.7, na.rm = TRUE)) %>%
  ungroup()

SAMPLES <- list(
  "All hospitalists"      = base_sample,
  "Remain hospital-based" = filter(base_sample, stable_inpat)
)

fit_one <- function(d, yname) {
  mp <- tryCatch(
    att_gt(yname = yname, gname = "minyr_EHR", idname = "DocNPI",
           tname = "year", xformla = NULL, data = d,
           est_method = "dr", control_group = "nevertreated",
           clustervars = "DocNPI", anticipation = 0,
           base_period = "universal"),
    error = function(e) {message("  ", yname, ": ", e$message); NULL})
  if (is.null(mp)) return(tibble(est = NA_real_, se = NA_real_, n = NA_integer_))
  a <- aggte(mp, type = "dynamic", na.rm = TRUE)
  tibble(est = a$overall.att, se = a$overall.se, n = mp$n)
}

chart_data <- imap_dfr(SAMPLES, function(d, panel) {
  message("=== ", panel, " ===")
  map_dfr(vl, function(y) {
    fit_one(d, y) %>% mutate(outcome_raw = y, Panel = panel)
  })
}) %>%
  mutate(outcome = recode(outcome_raw, !!!LABS),
         upper   = est + 1.96 * se,
         lower   = est - 1.96 * se,
         sig     = ifelse(!is.na(est) & abs(est / se) > 1.96,
                          "p < 0.05", "n.s."),
         Panel   = factor(Panel, levels = names(SAMPLES)))

SETTING <- c(initial_hosp_inpat = "Hospital",
             subs_hosp_inpat    = "Hospital",
             hosp_disch_day     = "Hospital",
             hosp_obs_care      = "Hospital",
             subs_obs_care      = "Hospital",
             crit_care_delivery = "Hospital",
             new_patient_office = "Office",
             est_patient_office = "Office")

## order: hospital codes first (sorted by effect), then office
ord <- chart_data %>%
  filter(Panel == names(SAMPLES)[1]) %>%
  mutate(setting = recode(outcome_raw, !!!SETTING)) %>%
  arrange(desc(setting), est) %>%      # Office block on top
  pull(outcome)

chart_data <- chart_data %>%
  mutate(outcome = factor(outcome, levels = ord),
         setting = factor(recode(outcome_raw, !!!SETTING),
                          levels = c("Office", "Hospital")))

pal_sig <- c("p < 0.05" = "#A34A5A", "n.s." = "#16324F")
shp_sig <- c("p < 0.05" = 17,        "n.s." = 16)

chart <- ggplot(chart_data, aes(y = outcome, x = est,
                                colour = sig, shape = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30") +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                linewidth = 0.7, width = 0.25) +
  geom_point(size = 2.4) +
  facet_wrap(~ Panel, nrow = 1) +
  scale_colour_manual(values = pal_sig) +
  scale_shape_manual(values = shp_sig) +
  scale_x_continuous(expand = expansion(mult = 0.12),
                     breaks = scales::pretty_breaks(n = 5)) +
  labs(x = "\nPoint Estimate and 95% CI", y = NULL) +
  theme_bw() +
  theme(
    text               = element_text(size = 14),
    axis.text          = element_text(size = 13),
    axis.title         = element_text(size = 14),
    strip.text         = element_text(size = 12.5),
    legend.title       = element_blank(),
    legend.position    = "bottom",
    legend.text        = element_text(size = 13),
    legend.key.width   = unit(1.6, "lines"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.3, colour = "grey65"),
    panel.border       = element_rect(colour = "grey30", fill = NA,
                                      linewidth = 0.5)
  ) + scale_x_continuous(expand = expansion(mult = 0.15),
                         breaks = scales::pretty_breaks(n = 4)) +
  geom_hline(yintercept = 2.5, colour = "grey50", linewidth = 0.4) 

print(chart)
ggsave("Objects/claim_type_results.pdf", plot = chart,
       width = 11, height = 5.5, units = "in")

chart_data %>%
  select(Panel, outcome, n, est, se) %>%
  arrange(Panel, est) %>%
  print(n = 20)


### Calcuate share of patients going to other hospitalists -------------
varlist <- list("retired_ever", "pos_office", "work_in_office",
                "npi_unq_benes", "claim_per_patient")
i_exit <- which(unlist(varlist) == "retired_ever")
i_pat  <- which(unlist(varlist) == "npi_unq_benes")
a_exit <- Models_agg[[i_exit]]$all$agg
a_pat  <- Models_agg[[i_pat]]$all$agg
ybar <- 336

data.frame(e = a_exit$egt, s = a_exit$att.egt) %>%
  filter(e >= 0) %>%
  mutate(implied = ybar * s / (1 - s)) %>%
  left_join(data.frame(e = a_pat$egt, observed = a_pat$att.egt), by = "e") %>%
  mutate(share = 100 * implied / observed)



## Never-treated comparison group--------------------------------------


## outcomes actually estimated in the paper
NT_OUTCOMES <- c(retired_ever      = "Exit from Clinical Practice",
                 pos_office        = "Fraction of Patients in Office",
                 work_in_office    = "Works in an Office",
                 npi_unq_benes     = "Number of Patients")

base <- Physician_Data %>%
  ungroup() %>%
  mutate(DocNPI = as.double(DocNPI),
         bundled = map2_lgl(doc_sc_years, minyr_EHR,
                            ~ !is.null(.x) && .y > 0 && any(abs(.x - .y) <= 0))) %>%
  group_by(DocNPI) %>%
  mutate(drop_md = any(bundled)) %>%
  ungroup() %>%
  filter(!drop_md,
         first_mdppas_yr == 2009,
         minyr_EHR != 2009) %>%          # keep 0, drop the 2009 cohort
  mutate(retired_ever = ifelse(!is.na(minyr_retire) & year >= minyr_retire, 1, 0)) %>%
  group_by(DocNPI) %>%
  mutate(stable_inpat = all(pos_inpat > 0.7, na.rm = TRUE)) %>%
  ungroup()

## truncate at 2015 and recode later adopters as never-treated
nt_panel <- base %>%
  filter(year <= 2015) %>%
  mutate(minyr_EHR = ifelse(minyr_EHR > 2015, 0, minyr_EHR),
         group     = ifelse(minyr_EHR == 0, "Never treated", "Treated")) %>%
  filter(year>=2009 & year<=2015)

## comparable not-yet-treated panel: same truncation, no never-treated
nyt_panel <- nt_panel %>% filter(minyr_EHR != 0)


char_vars <- c("num_hospitals", "num_systems", "avg_beds",
               "max_age", "female", "experience",
               "npi_unq_benes", "pos_inpat", "pos_office")
char_vars <- intersect(char_vars, names(nt_panel))

base_yr <- nt_panel %>% filter(year == 2009)

char_tab <- base_yr %>%
  select(DocNPI, group, all_of(char_vars)) %>%
  pivot_longer(all_of(char_vars), names_to = "var", values_to = "val") %>%
  group_by(var, group) %>%
  summarise(m = mean(val, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = group, values_from = m)

diff_tab <- map_dfr(char_vars, function(v) {
  tt <- t.test(base_yr[[v]] ~ base_yr$group)
  tibble(var  = v,
         diff = unname(diff(rev(tt$estimate))),
         p    = tt$p.value)
})

char_tab <- left_join(char_tab, diff_tab, by = "var")

n_never   <- n_distinct(nt_panel$DocNPI[nt_panel$minyr_EHR == 0])
n_treated <- n_distinct(nt_panel$DocNPI[nt_panel$minyr_EHR > 0])
cat("Never treated: ", n_never, " | Treated: ", n_treated, "\n", sep = "")
print(char_tab, width = Inf)


restrict_nt <- function(d, yname) {
  if (yname %in% c("pos_office", "work_in_office"))
    d <- filter(d, ever_retire == 0)
  if (yname %in% c("npi_unq_benes", "claim_per_patient"))
    d <- filter(d, ever_retire == 0, stable_inpat==1)
  d
}

fit_one <- function(yname, panel, ctrl) {
  d <- restrict_nt(panel, yname)
  mp <- tryCatch(
    att_gt(yname = yname, gname = "minyr_EHR", idname = "DocNPI",
           tname = "year", xformla = NULL, data = d,
           est_method = "dr", control_group = ctrl,
           clustervars = "DocNPI", anticipation = 0,
           base_period = "universal"),
    error = function(e) {message("  ", yname, " / ", ctrl, ": ", e$message); NULL})
  if (is.null(mp)) return(tibble(att = NA_real_, se = NA_real_, n = NA_integer_))
  ag <- aggte(mp, type = "dynamic", na.rm = TRUE)
  tibble(att = ag$overall.att, se = ag$overall.se, n = mp$n)
}

est_tab <- imap_dfr(NT_OUTCOMES, function(lab, y) {
  message("estimating: ", y)
  nyt <- fit_one(y, nyt_panel, "notyettreated")
  nt  <- fit_one(y, nt_panel,  "nevertreated")
  tibble(outcome_raw = y, Outcome = lab,
         att_nyt = nyt$att, se_nyt = nyt$se, n_nyt = nyt$n,
         att_nt  = nt$att,  se_nt  = nt$se,  n_nt  = nt$n)
})

## drop any outcome that failed to estimate under either approach
est_tab <- est_tab %>% filter(!is.na(att_nyt), !is.na(att_nt))
print(est_tab, width = Inf)


fmt_num <- function(x) ifelse(is.na(x), "---",
                              ifelse(abs(x) >= 100, formatC(x, format = "d", big.mark = ","),
                                     ifelse(abs(x) >= 1,   sprintf("%.2f", x),
                                            sprintf("%.3f", x))))
fmt_p <- function(p) ifelse(is.na(p), "---",
                            ifelse(p < 0.01, "$<$0.01", sprintf("%.2f", p)))

VAR_LABS <- c(num_hospitals = "Hospitals worked with",
              num_systems   = "Systems worked with",
              avg_beds      = "Average hospital beds",
              max_age       = "Age",
              female        = "Female",
              experience    = "Years since graduation",
              npi_unq_benes = "Medicare patients",
              pos_inpat     = "Share of claims, inpatient",
              pos_office    = "Share of claims, office")

tabA <- char_tab %>%
  mutate(Variable        = recode(var, !!!VAR_LABS),
         Treated         = fmt_num(Treated),
         `Never treated` = fmt_num(`Never treated`),
         Difference      = fmt_num(diff),
         `$p$-value`     = fmt_p(p)) %>%
  select(Variable, Treated, `Never treated`, Difference, `$p$-value`) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE,
      align = c("l", rep("c", 4)),
      caption = "Characteristics of treated and never-treated hospitalists",
      label = "nt_chars") %>%
  kable_styling(latex_options = "hold_position", full_width = FALSE) %>%
  kableExtra::footnote(general = paste(
    "Means measured in 2009, the first year of the panel.",
    "Never-treated hospitalists are those with no EHR-adopting hospital",
    "affiliation through 2015.",
    "$p$-values are from two-sided $t$-tests of equality of means."),
    threeparttable = TRUE, escape = FALSE)

tabB <- est_tab %>%
  transmute(Outcome,
            `Not-yet-treated` = sprintf("%s (%s)", fmt_num(att_nyt), fmt_num(se_nyt)),
            `N (NYT)`         = formatC(n_nyt, format = "d", big.mark = ","),
            `Never-treated`   = sprintf("%s (%s)", fmt_num(att_nt), fmt_num(se_nt)),
            `N (NT)`          = formatC(n_nt, format = "d", big.mark = ",")) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE,
      align = c("l", rep("c", 4)),
      caption = "Estimates using not-yet-treated and never-treated comparison groups",
      label = "nt_estimates") %>%
  add_header_above(c(" " = 1, "Comparison group" = 4)) %>%
  kable_styling(latex_options = "hold_position", full_width = FALSE) %>%
  kableExtra::footnote(general = paste(
    "Both columns use a panel truncated at 2015.",
    "The not-yet-treated column excludes hospitalists never exposed within",
    "that window; the never-treated column retains them and uses them as the",
    "comparison group. Standard errors in parentheses, clustered at the",
    "physician level."),
    threeparttable = TRUE, escape = FALSE)

dir.create("Table Code", showWarnings = FALSE)
writeLines(as.character(tabA), file.path("Table Code", "nt_chars.tex"))
writeLines(as.character(tabB), file.path("Table Code", "nt_estimates.tex"))
cat("\nWrote nt_chars.tex and nt_estimates.tex\n")


### RESULTS WITH EMPLOYER TYPE AS OUTCOME------------------------------------


##---- 1. Identify office-work shifters 
office_shift_ids <- Physician_Data_EHR %>%
  filter(minyr_EHR > 0, ever_retire == 0) %>%
  mutate(period = case_when(
    year == minyr_EHR - 1                        ~ "baseline",
    year >= minyr_EHR + 2 & year <= minyr_EHR + 4 ~ "post"
  )) %>%
  filter(!is.na(period)) %>%
  group_by(DocNPI, period) %>%
  summarise(work_in_office = max(work_in_office, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = work_in_office,
              names_prefix = "office_") %>%
  filter(!is.na(office_baseline), !is.na(office_post)) %>%
  mutate(shifter = office_baseline == 0 & office_post == 1) %>%
  filter(shifter) %>%
  pull(DocNPI)

length(office_shift_ids)

##---- 2. Restrict the panel to these physicians, keep full event-time data 
d_shifters <- Physician_Data_EHR %>%
  filter(DocNPI %in% office_shift_ids, ever_retire == 0)

n_distinct(d_shifters$DocNPI)

##---- 3. Construct indicators
d_shifters <- d_shifters %>%
  mutate(
    emp_hospital_affiliated = case_when(
      is.na(tin1_type) ~ NA_real_,
      tin1_type %in% c("hospital_direct", "integrated_system_group") ~ 1,
      TRUE ~ 0
    ),
    emp_staffing_company = case_when(
      is.na(tin1_type) ~ NA_real_,
      tin1_type == "staffing_company" ~ 1,
      TRUE ~ 0
    ),
    emp_solo_independent = case_when(
      is.na(tin1_type) ~ NA_real_,
      tin1_type == "solo_independent" ~ 1,
      TRUE ~ 0
    )
  )

##---- 4. Estimate each employer-type category, WITHIN shifters only
CATEGORIES <- c("hospital_affiliated", "staffing_company", "solo_independent")

fit_category_shifters <- function(cat) {
  varname <- paste0("emp_", cat)
  d <- d_shifters %>% filter(!is.na(.data[[varname]]))
  
  n_docs <- n_distinct(d$DocNPI)
  message(cat, ": ", n_docs, " physicians")
  if (n_docs < 30) { message("  -- too few, skipping"); return(NULL) }
  
  mp <- tryCatch(
    att_gt(yname = varname, gname = "minyr_EHR", idname = "DocNPI",
           tname = "year", xformla = NULL, data = d,
           est_method = "dr", control_group = "notyettreated",
           clustervars = "DocNPI", anticipation = 0, base_period = "universal"),
    error = function(e) { message("  -- failed: ", e$message); NULL }
  )
  if (is.null(mp)) return(NULL)
  
  a <- aggte(mp, type = "dynamic", na.rm = TRUE)
  tibble(category = cat, att = a$overall.att, se = a$overall.se, n = mp$n)
}

results_shifters <- map_dfr(CATEGORIES, fit_category_shifters)
print(results_shifters)

cat("\nSum of ATTs (should be ~0 if categories are exhaustive/exclusive):\n")
sum(results_shifters$att, na.rm = TRUE)


## re-run fit_category_shifters but keep the full mp/aggte objects this time,
## not just the summary row, so we have event-time data to plot
fit_category_shifters_full <- function(cat) {
  varname <- paste0("emp_", cat)
  d <- d_shifters %>% filter(!is.na(.data[[varname]]))
  
  n_docs <- n_distinct(d$DocNPI)
  if (n_docs < 30) return(NULL)
  
  mp <- tryCatch(
    att_gt(yname = varname, gname = "minyr_EHR", idname = "DocNPI",
           tname = "year", xformla = NULL, data = d,
           est_method = "dr", control_group = "notyettreated",
           clustervars = "DocNPI", anticipation = 0, base_period = "universal"),
    error = function(e) NULL
  )
  if (is.null(mp)) return(NULL)
  
  a <- aggte(mp, type = "dynamic", na.rm = TRUE)
  tibble(event_time = a$egt, att = a$att.egt, se = a$se.egt) %>%
    mutate(lower = att - 1.96*se, upper = att + 1.96*se, category = cat)
}

CATEGORIES_TO_PLOT <- c("hospital_affiliated", "staffing_company", "solo_independent")

CAT_LABS <- c(hospital_affiliated = "Hospital-Affiliated",
              staffing_company    = "Staffing Company",
              solo_independent    = "Solo/Independent")

##---- Pre-treatment means, within the shifter sample 
pretreat_means_shifters <- d_shifters %>%
  filter(minyr_EHR > 0, year == minyr_EHR - 1) %>%
  summarise(across(all_of(paste0("emp_", CATEGORIES_TO_PLOT)),
                   ~ mean(., na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "varname", values_to = "pretreat_mean") %>%
  mutate(category = CAT_LABS[str_remove(varname, "emp_")])

##---- Build the figure, with baseline means in the facet labels 
plot_data_shifters <- map_dfr(CATEGORIES_TO_PLOT, fit_category_shifters_full) %>%
  filter(event_time >= -4, event_time <= 4) %>%
  mutate(category = CAT_LABS[category]) %>%
  left_join(pretreat_means_shifters %>% select(category, pretreat_mean),
            by = "category") %>%
  mutate(lab = sprintf("%s\n(baseline: %.1f%%)", category, 100 * pretreat_mean))

p_shifters <- ggplot(plot_data_shifters, aes(event_time, att)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") +
  geom_vline(xintercept = -0.5, linetype = "dashed", colour = "grey30") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, linewidth = 1.1, colour = "black") +
  geom_point(size = 2.5, colour = "black") +
  facet_wrap(~ lab, scales = "free_y", nrow = 1) +
  scale_x_continuous(breaks = seq(-4, 4, 1)) +
  labs(x = "\nEvent time", y = "Point Estimate and 95% CI\n") +
  base_theme +
  theme(legend.position = "none",
        strip.text = element_text(size = 14, colour = "black"),
        strip.background = element_rect(fill = "grey90", colour = "grey30"))

print(p_shifters)
ggsave("Objects/employer_types_shifters_eventstudy_appendix.pdf", plot = p_shifters,
       width = 12, height = 4.5, units = "in")


## Baseline Employer Type Split for Office-Fraction Outcomes-------------------

baseline_employer <- Physician_Data_EHR %>%
  filter(minyr_EHR > 0) %>%
  filter(year == minyr_EHR - 1) %>%
  distinct(DocNPI, .keep_all = FALSE, tin1_type) %>%
  dplyr::rename(baseline_employer_type = tin1_type) %>%
  select(DocNPI, baseline_employer_type)

## check coverage / missingness before merging
baseline_employer %>% count(baseline_employer_type, sort = TRUE)

## merge onto main analysis data
Physician_Data_EHR <- Physician_Data_EHR %>%
  left_join(baseline_employer, by = "DocNPI")

## collapse to broader groups for power, consistent with the paper's
## finding that hospital-direct and integrated-system behave similarly
Physician_Data_EHR <- Physician_Data_EHR %>%
  mutate(baseline_employer_group = case_when(
    baseline_employer_type %in% c("hospital_direct", "integrated_system_group") ~ "hospital_affiliated",
    baseline_employer_type == "staffing_company" ~ "staffing_company",
    baseline_employer_type == "solo_independent" ~ "solo_independent",
    TRUE ~ NA_character_  # drops academic/government/unclassified from this analysis
  ))

## sample sizes by group -- check power before running att_gt on each
Physician_Data_EHR %>%
  filter(!is.na(baseline_employer_group)) %>%
  distinct(DocNPI, baseline_employer_group) %>%
  count(baseline_employer_group)


## ---- 2. Fit att_gt for pos_office / work_in_office by employer group 

office_vars <- c("pos_office", "work_in_office")

fit_by_employer <- function(x) {
  base <- Physician_Data_EHR %>%
    dplyr::filter(ever_retire == 0, !is.na(baseline_employer_group))
  
  fit <- function(d) att_gt(
    yname = x, gname = "minyr_EHR", idname = "DocNPI", tname = "year",
    xformla = NULL, data = d, est_method = "dr",
    control_group = "notyettreated", clustervars = "DocNPI",
    anticipation = 0, base_period = "universal"
  )
  
  list(
    hospital_affiliated = fit(dplyr::filter(base, baseline_employer_group == "hospital_affiliated")),
    staffing_company     = fit(dplyr::filter(base, baseline_employer_group == "staffing_company")),
    solo_independent     = fit(dplyr::filter(base, baseline_employer_group == "solo_independent"))
  )
}

models_by_employer <- lapply(office_vars, fit_by_employer)
names(models_by_employer) <- office_vars


## ---- 3. Aggregate, reusing your existing wald_pre() helper 

Models_agg_employer <- lapply(models_by_employer, function(x) {
  lapply(x, function(m) {
    agg <- aggte(m, type = "dynamic", na.rm = TRUE, min_e = -4, max_e = 4)
    w   <- wald_pre(m)
    list(agg = agg, p = unname(w["p"]), df = unname(w["df"]),
         att = agg$overall.att, se = agg$overall.se,
         p_att = 2 * pnorm(-abs(agg$overall.att / agg$overall.se)))
  })
})


## ---- 4. Baselines (pre-treatment means) by employer group 

get_baseline_employer <- function(out_name, grp) {
  Physician_Data_EHR %>%
    dplyr::filter(ever_retire == 0, baseline_employer_group == grp,
                  minyr_EHR > 0, year == minyr_EHR - 1) %>%
    dplyr::summarise(m = mean(.data[[out_name]], na.rm = TRUE)) %>%
    dplyr::pull(m)
}

baselines_employer <- lapply(office_vars, function(v) {
  vapply(c("hospital_affiliated", "staffing_company", "solo_independent"),
         function(g) get_baseline_employer(v, g),
         numeric(1))
})
names(baselines_employer) <- office_vars


## ---- 5. Build graph data (same structure as age-split graphs) 

labs_map_emp <- c(hospital_affiliated = "Hospital-Affiliated",
                  staffing_company    = "Staffing Company",
                  solo_independent    = "Solo/Independent")

graph_data_employer <- lapply(seq_along(Models_agg_employer), function(i) {
  x <- Models_agg_employer[[i]]
  v <- office_vars[[i]]
  
  dat <- dplyr::bind_rows(lapply(names(labs_map_emp), function(g) {
    a <- x[[g]]$agg
    data.frame(year = a$egt, att = a$att.egt, se = a$se.egt,
               group = labs_map_emp[[g]])
  })) %>%
    dplyr::mutate(upper = att + 1.96 * se,
                  lower = att - 1.96 * se)
  
  dat <- dplyr::filter(dat, year >= -5, year <= 4) %>%
    dplyr::mutate(year = factor(year, levels = sort(unique(year))))
  
  list(data = dat,
       p_hosp     = x$hospital_affiliated$p,
       p_staffing = x$staffing_company$p,
       p_solo     = x$solo_independent$p)
})
names(graph_data_employer) <- office_vars


## ---- 6. Plot, reusing base_theme and dodge from main script 

pal_emp   <- c("Hospital-Affiliated" = "#16324F",
               "Staffing Company"    = "#A34A5A",
               "Solo/Independent"    = "#5A8F5A")
shape_emp <- c("Hospital-Affiliated" = 16,
               "Staffing Company"    = 17,
               "Solo/Independent"    = 15)

fmt_base_office <- function(x) sprintf("%.3f", x)

make_subtitle_employer <- function(bl) {
  paste0("Pre-treatment mean: ",
         fmt_base_office(bl[["hospital_affiliated"]]), " (hosp.-affil.); ",
         fmt_base_office(bl[["staffing_company"]]),    " (staffing); ",
         fmt_base_office(bl[["solo_independent"]]),    " (solo)")
}

graphs_employer <- lapply(seq_along(graph_data_employer), function(i) {
  x  <- graph_data_employer[[i]]
  v  <- office_vars[[i]]
  bl <- baselines_employer[[i]]
  
  yr <- range(c(x$data$lower, x$data$upper), na.rm = TRUE)
  yr <- yr + c(-1, 1) * 0.04 * diff(yr)
  
  ggplot(x$data, aes(year, att, colour = group, shape = group)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") +
    geom_vline(xintercept = "0", linetype = "dashed", colour = "grey30") +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  width = 0, linewidth = 1.1, position = dodge) +
    geom_point(size = 3, position = dodge) +
    scale_colour_manual(values = pal_emp) +
    scale_shape_manual(values = shape_emp) +
    scale_y_continuous(limits = yr, breaks = scales::pretty_breaks(n = 8)) +
    labs(x = NULL, y = "Point Estimate and 95% CI\n",
         subtitle = make_subtitle_employer(bl),
         ) +
    base_theme
})
names(graphs_employer) <- office_vars

ggsave("Objects/pos_office_by_type.pdf", plot = graphs_employer$pos_office,
       width = 10, height = 5, units = "in")
ggsave("Objects/work_in_office_by_type.pdf", plot = graphs_employer$work_in_office,
       width = 10, height = 5, units = "in")


## Balanced Cohort Sample------------------

## ---- 1. Define eligible cohorts
MIN_YEAR <- 2009
MAX_YEAR <- 2017
WINDOW   <- 3

eligible_cohorts <- (MIN_YEAR + WINDOW):(MAX_YEAR - WINDOW)
eligible_cohorts   # 2012 2013 2014

## ---- 2. Fit att_gt on the FULL sample with TRUE treatment timing 
## No recoding. control_group = "notyettreated" already handles comparisons
## correctly using each unit's real minyr_EHR.

varlist_balanced <- list("pos_office", "work_in_office", "npi_unq_benes",
                         "claim_per_patient", "retired_ever")

fit_full <- function(x) {
  base <- if (x %in% c("retired_ever")) Physician_Data_EHR
  else if (x %in% c("pos_office", "work_in_office"))
    dplyr::filter(Physician_Data_EHR, ever_retire == 0)
  else dplyr::filter(Physician_Data_EHR, ever_retire == 0 & stable_inpat == 1)
  
  att_gt(
    yname = x, gname = "minyr_EHR", idname = "DocNPI", tname = "year",
    xformla = NULL, data = base, est_method = "dr",
    control_group = "notyettreated", clustervars = "DocNPI",
    anticipation = 0, base_period = "universal"
  )
}

models_full <- lapply(varlist_balanced, fit_full)
names(models_full) <- unlist(varlist_balanced)

## ---- 3. Manually aggregate ONLY the eligible cohorts, by event time 

aggregate_eligible <- function(mp, eligible, window) {
  event_time <- mp$t - mp$group
  keep <- which(mp$group %in% eligible &
                  event_time >= -window & event_time <= window &
                  !is.na(mp$se))
  
  if (!length(keep)) return(NULL)
  
  et_vals <- sort(unique(event_time[keep]))
  
  out <- lapply(et_vals, function(e) {
    idx <- keep[event_time[keep] == e]
    
    ## weight each group-time cell by its group size (standard did approach
    ## uses group weights proportional to group size; here we approximate
    ## with equal weighting across cohorts unless you have group sizes handy)
    a_vec <- mp$att[idx]
    inf_mat <- as.matrix(mp$inffunc[, idx, drop = FALSE])
    
    ## simple average across cohorts contributing to this event time
    w <- rep(1 / length(idx), length(idx))
    att_e <- sum(w * a_vec)
    
    ## variance of the weighted average using influence functions
    inf_e <- inf_mat %*% w   # combined influence function for this event time
    se_e  <- sqrt(sum(inf_e^2) / mp$n^2)
    
    tibble(event_time = e, att = att_e, se = se_e, n_cohorts = length(idx))
  })
  
  bind_rows(out)
}

models_agg_eligible <- lapply(models_full, aggregate_eligible,
                              eligible = eligible_cohorts, window = WINDOW)
names(models_agg_eligible) <- unlist(varlist_balanced)

## ---- 4. Pre-trends test restricted to eligible cohorts 
wald_pre_eligible <- function(mp, eligible, max_lag = Inf) {
  event_time <- mp$t - mp$group
  pre <- which(mp$group %in% eligible &
                 event_time < 0 & event_time >= -max_lag &
                 !is.na(mp$se) & mp$se > 0)
  if (!length(pre)) return(c(W = NA, df = NA, n_cells = 0, p = NA))
  a  <- mp$att[pre]
  V  <- crossprod(as.matrix(mp$inffunc[, pre, drop = FALSE])) / mp$n^2
  W  <- as.numeric(t(a) %*% MASS::ginv(V) %*% a)
  df <- qr(V)$rank
  c(W = W, df = df, n_cells = length(pre), p = 1 - pchisq(W, df))
}

pretrend_results <- lapply(models_full, wald_pre_eligible,
                           eligible = eligible_cohorts, max_lag = WINDOW)
names(pretrend_results) <- unlist(varlist_balanced)
print(pretrend_results)

## ---- 5. Comparison table
comparison_table <- tibble(
  outcome    = unlist(varlist_balanced),
  overall_att = vapply(models_agg_eligible, function(x) {
    if (is.null(x)) return(NA_real_)
    ## simple overall average across post event-times, unweighted
    post <- dplyr::filter(x, event_time >= 0)
    mean(post$att)
  }, numeric(1)),
  pretrend_p = vapply(pretrend_results, function(x) unname(x["p"]), numeric(1))
)
print(comparison_table)

## ---- 6. Plot event studies, restricted to eligible cohorts
plot_eligible <- function(v) {
  dat <- models_agg_eligible[[v]] %>%
    mutate(upper = att + 1.96 * se, lower = att - 1.96 * se,
           event_time = factor(event_time, levels = sort(unique(event_time))))
  
  p_val <- fmt_p(unname(pretrend_results[[v]]["p"]))
  
  ggplot(dat, aes(event_time, att)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") +
    geom_vline(xintercept = as.character(0), linetype = "dashed", colour = "grey30") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, linewidth = 1.1, colour = "black") +
    geom_point(size = 2.5, colour = "black") +
    labs(x = NULL, y = "Point Estimate and 95% CI\n",
         subtitle = paste0("Balanced cohort sample, cohorts ",
                           min(eligible_cohorts), "-", max(eligible_cohorts)),
         caption = paste0("Pre-trends test p-value: ", p_val)) +
    base_theme + theme(legend.position = "none")
}

balanced_plots <- lapply(unlist(varlist_balanced), plot_eligible)
names(balanced_plots) <- unlist(varlist_balanced)


## Comparison Table: Main Specification vs. Balanced Cohort Sample

## ---- 1. Pull main-specification overall ATT/SE for each outcome 

main_results <- tibble(
  outcome    = unlist(varlist),
  att_main   = vapply(Models_agg, function(x) x$all$att, numeric(1)),
  se_main    = vapply(Models_agg, function(x) x$all$se, numeric(1))
) %>%
  filter(outcome %in% unlist(varlist_balanced))  # keep only outcomes used in balanced check

## ---- 2. Pull balanced-sample overall ATT/SE + pre-trends p-value

get_balanced_n <- function(x) {
  base <- if (x %in% c("retired_ever")) 
    dplyr::filter(Physician_Data_EHR, minyr_EHR %in% eligible_cohorts | !(minyr_EHR %in% eligible_cohorts))
  # ^ note: N should reflect only the TREATED docs actually being evaluated,
  # i.e., those in eligible_cohorts -- see explanation below
  
  d <- if (x %in% c("retired_ever")) Physician_Data_EHR
  else if (x %in% c("pos_office", "work_in_office"))
    dplyr::filter(Physician_Data_EHR, ever_retire == 0)
  else dplyr::filter(Physician_Data_EHR, ever_retire == 0 & stable_inpat == 1)
  
  d %>%
    dplyr::filter(minyr_EHR %in% eligible_cohorts) %>%
    dplyr::distinct(DocNPI) %>%
    nrow()
}

n_docs_by_outcome <- vapply(unlist(varlist_balanced), get_balanced_n, numeric(1))
n_docs_by_outcome

balanced_results <- tibble(
  outcome       = unlist(varlist_balanced),
  att_balanced  = vapply(models_agg_eligible, function(x) {
    if (is.null(x)) return(NA_real_)
    post <- dplyr::filter(x, event_time >= 0)
    mean(post$att)
  }, numeric(1)),
  se_balanced   = vapply(models_agg_eligible, function(x) {
    if (is.null(x)) return(NA_real_)
    post <- dplyr::filter(x, event_time >= 0)
    sqrt(mean(post$se^2) / nrow(post))
  }, numeric(1)),
  pretrend_p    = vapply(pretrend_results, function(x) unname(x["p"]), numeric(1)),
  n_docs        = n_docs_by_outcome[unlist(varlist_balanced)]
)

## ---- 3. Combine into one comparison table 
comparison_table_final <- main_results %>%
  left_join(balanced_results, by = "outcome") %>%
  mutate(
    outcome = case_when(
      outcome == "pos_office"        ~ "Fraction Patients in Office",
      outcome == "work_in_office"    ~ "Work in an Office",
      outcome == "npi_unq_benes"     ~ "Number of Patients",
      outcome == "claim_per_patient" ~ "Claims per Patient",
      outcome == "retired_ever"      ~ "Ever Exit Clinical Work",
      TRUE ~ outcome
    )
  ) %>%
  select(outcome, att_main, se_main, att_balanced, se_balanced, pretrend_p, n_docs)

print(comparison_table_final)

## ---- 4. Format as a LaTeX table via kableExtra 
library(kableExtra)

comparison_table_final %>%
  mutate(
    `Main Sample`      = sprintf("%.3f (%.3f)", att_main, se_main),
    `Balanced Sample`  = sprintf("%.3f (%.3f)", att_balanced, se_balanced),
    `Pre-Trends p`     = vapply(pretrend_p, fmt_p, character(1)),
    `N Physicians`     = formatC(n_docs, format = "d", big.mark = ",")
  ) %>%
  select(Outcome = outcome, `Main Sample`, `Balanced Sample`, `Pre-Trends p`, `N Physicians`) %>%
  kbl(format = "latex", booktabs = TRUE, escape = TRUE,
      caption = "Robustness to Balanced Cohort Sample (Cohorts 2012--2014, Event Window $-3$ to $+3$)",
      label = "tab:balanced_robustness") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(general = paste0(
    "Main Sample column reports overall ATT (SE) from the full estimation sample, ",
    "pooling all cohorts. Balanced Sample column restricts to hospitalists first ",
    "exposed to EHRs in 2012--2014, so that a full event window of three years ",
    "before and after exposure is observed for every included physician. ",
    "Pre-Trends p reports the p-value from a joint Wald test that pre-treatment ",
    "event-study coefficients within the balanced sample are equal to zero."
  ), footnote_as_chunk = TRUE) %>%
  cat(file = "Table Code/balanced_robustness.tex")



# INVESTIGATE HOSPITAL LEVEL CLUSTERING ---------------------------------------
cleaned_pairs <- readRDS(paste0(created_data_path, "cleaned_pairs.rds"))

adopting_hosp_candidates <- cleaned_pairs %>%
  ungroup() %>%
  filter(minyr_EHR > 0, year == minyr_EHR) %>%
  filter(any_EHR == 1) %>%   ## restrict to the hospital(s) that actually had an EHR that year
  group_by(DocNPI, HospNPI) %>%
  summarise(shared_patients = sum(samedaycount, na.rm = TRUE), .groups = "drop")

## how many physicians have more than one "adopting" hospital in their treatment year?
adopting_hosp_candidates %>%
  count(DocNPI) %>%
  count(n, name = "n_physicians")

phys_adopting_hosp <- adopting_hosp_candidates %>%
  group_by(DocNPI) %>%
  slice_max(shared_patients, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(DocNPI, cluster_HospNPI_adopting = HospNPI)

n_distinct(phys_adopting_hosp$DocNPI) == nrow(phys_adopting_hosp)

## coverage check within your actual analysis sample
Physician_Data_EHR <- Physician_Data_EHR %>%
  select(-any_of("cluster_HospNPI_adopting")) %>%
  left_join(phys_adopting_hosp, by = "DocNPI")

Physician_Data_EHR %>%
  summarise(n = n(), n_missing = sum(is.na(cluster_HospNPI_adopting)))

fit_hosp_cluster <- function(x) {
  base <- if (x %in% c("retired_ever")) Physician_Data_EHR
  else if (x %in% c("pos_office", "work_in_office"))
    dplyr::filter(Physician_Data_EHR, ever_retire == 0)
  else dplyr::filter(Physician_Data_EHR, ever_retire == 0 & stable_inpat == 1)
  
  base <- base %>% filter(!is.na(cluster_HospNPI_adopting))
  
  att_gt(
    yname = x, gname = "minyr_EHR", idname = "DocNPI", tname = "year",
    xformla = NULL, data = base, est_method = "dr",
    control_group = "notyettreated", clustervars = "cluster_HospNPI_adopting",
    anticipation = 0, base_period = "universal"
  )
}

varlist_main <- list("retired_ever", "pos_office", "work_in_office",
                     "npi_unq_benes", "claim_per_patient")

models_hosp_cluster <- lapply(varlist_main, fit_hosp_cluster)
names(models_hosp_cluster) <- unlist(varlist_main)
names(Models_agg) <- varlist

comparison_cluster <- tibble(
  outcome = unlist(varlist_main),
  se_physician_cluster = vapply(varlist_main, function(x) Models_agg[[x]]$all$se, numeric(1)),
  se_hospital_cluster  = vapply(models_hosp_cluster, function(m) {
    aggte(m, type = "dynamic", na.rm = TRUE)$overall.se
  }, numeric(1))
)
print(comparison_cluster)

## Pull point estimates alongside both SE versions to check significance
set.seed(1234)

comparison_cluster_final <- tibble(
  outcome = unlist(varlist_main),
  att = vapply(varlist_main, function(x) Models_agg[[x]]$all$att, numeric(1)),
  se_physician_cluster = vapply(varlist_main, function(x) Models_agg[[x]]$all$se, numeric(1)),
  se_hospital_cluster  = vapply(models_hosp_cluster, function(m) {
    aggte(m, type = "dynamic", na.rm = TRUE)$overall.se
  }, numeric(1))
) %>%
  mutate(
    t_hospital  = att / se_hospital_cluster,
    p_hospital  = 2 * pnorm(-abs(t_hospital))
  )

print(comparison_cluster_final)

fmt_p_table <- function(p) ifelse(p < 0.01, "$<$0.01", sprintf("%.2f", p))
fmt_est <- function(x) sprintf("%.3f", x)

clustering_tab <- comparison_cluster_final %>%
  mutate(
    outcome_label = case_when(
      outcome == "retired_ever"      ~ "Ever Exit Clinical Work",
      outcome == "pos_office"        ~ "Fraction Patients in Office",
      outcome == "work_in_office"    ~ "Work in an Office",
      outcome == "npi_unq_benes"     ~ "Number of Patients",
      outcome == "claim_per_patient" ~ "Claims per Patient",
      TRUE ~ outcome
    ),
    p_physician = 2 * pnorm(-abs(att / se_physician_cluster)),
    `Estimate`            = fmt_est(att),
    `SE (Physician)`      = fmt_est(se_physician_cluster),
    `p-value (Physician)` = vapply(p_physician, fmt_p_table, character(1)),
    `SE (Hospital)`       = fmt_est(se_hospital_cluster),
    `p-value (Hospital)`  = vapply(p_hospital, fmt_p_table, character(1))
  ) %>%
  select(outcome_label, Estimate, `SE (Physician)`, `p-value (Physician)`,
         `SE (Hospital)`, `p-value (Hospital)`)

out <- kbl(clustering_tab, format = "latex", booktabs = TRUE, escape = FALSE,
           col.names = c("Outcome", "Estimate",
                         "\\makecell{SE\\\\(Physician)}", "\\makecell{$p$-value\\\\(Physician)}",
                         "\\makecell{SE\\\\(Hospital)}", "\\makecell{$p$-value\\\\(Hospital)}"),
           align = c("l", "c", "c", "c", "c", "c"),
           caption = "Robustness to Hospital-Level Clustering",
           label = "tab:cluster_robustness") %>%
  kable_styling(latex_options = "hold_position", full_width = FALSE)

out_full <- paste0(
  "\\begin{table}[ht!]\n\\centering\n",
  "\\caption{\\label{tab:cluster_robustness}Robustness to Hospital-Level Clustering}\n",
  "\\begin{threeparttable}\n",
  sub(".*(\\\\begin\\{tabular\\}.*\\\\end\\{tabular\\}).*", "\\1", out, perl = TRUE),
  "\n\\begin{tablenotes}\n",
  "\\item \\textit{Note: } Estimate reports the overall average treatment effect on the treated from the main specification. ",
  "SE (Physician) reports the standard error under the main specification's physician-level clustering; ",
  "SE (Hospital) reports the standard error when instead clustering by the hospital responsible for the physician's ",
  "EHR exposure timing, identified as the hospital with EHR adoption in the physician's treatment year with the most ",
  "shared patients. $p$-values in each column are calculated using the corresponding standard error.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}"
)

cat(out_full)
writeLines(out_full, file.path("Table Code", "cluster_robustness.tex"))
cat("\nWrote Table Code/cluster_robustness.tex\n")


## Hospital-clustered estimates, split by age

fit_hosp_cluster_age <- function(x, age_filter) {
  base <- if (x %in% c("retired_ever")) Physician_Data_EHR
  else if (x %in% c("pos_office", "work_in_office"))
    dplyr::filter(Physician_Data_EHR, ever_retire == 0)
  else dplyr::filter(Physician_Data_EHR, ever_retire == 0 & stable_inpat == 1)
  
  base <- base %>%
    filter(!is.na(cluster_HospNPI_adopting)) %>%
    filter(if (age_filter == "young") max_age < 60
           else if (age_filter == "old") max_age >= 60
           else TRUE)
  
  att_gt(
    yname = x, gname = "minyr_EHR", idname = "DocNPI", tname = "year",
    xformla = NULL, data = base, est_method = "dr",
    control_group = "notyettreated", clustervars = "cluster_HospNPI_adopting",
    anticipation = 0, base_period = "universal"
  )
}

varlist_main <- list("retired_ever", "pos_office", "work_in_office",
                     "npi_unq_benes", "claim_per_patient")

set.seed(1234)

models_hosp_cluster_age <- lapply(varlist_main, function(x) {
  list(
    young = fit_hosp_cluster_age(x, "young"),
    old   = fit_hosp_cluster_age(x, "old")
  )
})
names(models_hosp_cluster_age) <- unlist(varlist_main)

## ---- Build comparison table: physician vs hospital clustering, by age 
comparison_cluster_age <- tibble(
  outcome     = rep(unlist(varlist_main), each = 2),
  group       = rep(c("young", "old"), times = length(varlist_main)),
  att         = unlist(lapply(varlist_main, function(x) c(
    Models_agg[[x]]$young$att, Models_agg[[x]]$old$att))),
  se_physician_cluster = unlist(lapply(varlist_main, function(x) c(
    Models_agg[[x]]$young$se, Models_agg[[x]]$old$se))),
  se_hospital_cluster  = unlist(lapply(varlist_main, function(x) {
    c(aggte(models_hosp_cluster_age[[x]]$young, type = "dynamic", na.rm = TRUE)$overall.se,
      aggte(models_hosp_cluster_age[[x]]$old,   type = "dynamic", na.rm = TRUE)$overall.se)
  }))
) %>%
  mutate(
    p_physician = 2 * pnorm(-abs(att / se_physician_cluster)),
    p_hospital  = 2 * pnorm(-abs(att / se_hospital_cluster))
  )

print(comparison_cluster_age, n = 20)
