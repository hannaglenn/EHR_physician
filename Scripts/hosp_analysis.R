library(readr)
library(dplyr)
library(did)
library(purrr)
library(kableExtra)


# read in hospital level data
hosp_data <- readRDS(paste0(created_data_path, "hosp_data_NEW.rds"))

minyr_EHR <- hosp_data %>%
  filter(any_EHR==1) %>%
  group_by(HospNPI) %>%
  mutate(minyr_EHR = min(year)) %>%
  ungroup() %>%
  distinct(HospNPI, minyr_EHR)

hosp_data <- hosp_data %>%
  left_join(minyr_EHR, by="HospNPI")

hosp_data <- hosp_data %>%
  mutate(minyr_EHR = ifelse(is.na(minyr_EHR),0,minyr_EHR))

# Remove observations that happened during a system change 
hosp_data <- hosp_data %>%
  ungroup() %>%
  mutate(bundled = purrr::map2_lgl(change_years, minyr_EHR,
                                   ~ !is.null(.x) && .y > 0 && any(abs(.x - .y) <= 0))) %>%
  group_by(HospNPI) %>%
  mutate(drop_md = any(bundled)) %>%
  ungroup() %>%
  filter(!drop_md)

# read in HCRIS data
final_hcris_data <- read_csv(paste0(raw_data_path, "/final_hcris_data.csv")) %>%
  select(-fy_start, -fy_end, -date_processed, -date_created, -source)

# merge to main hospital data
hosp_data <- hosp_data %>%
  left_join(final_hcris_data, by=c("MCRNUM"="provider_number", "year"))

# create summary stats by year
means <- hosp_data %>%
  group_by(year) %>%
  summarise_all(mean, na.rm=T)

# create change in behavior variables
hosp_data <- hosp_data %>%
  separate(MADMIN, sep = ",", into = c("admin_name", "other")) %>%
  select(-other) 

hosp_data <- hosp_data %>%
  mutate(count=1) %>%
  group_by(MCRNUM) %>%
  mutate(sum = sum(count)) %>%
  filter(sum>=7) %>%
  mutate(lag_name = dplyr::lag(admin_name)) %>%
  mutate(lag_sys = dplyr::lag(SYSID)) %>%
  ungroup() %>%
  mutate(change_admin = ifelse(admin_name==lag_name,0,1),
         change_sys = ifelse((is.na(SYSID) & !is.na(lag_sys)) | (!is.na(SYSID) & is.na(lag_sys)),1,0),
         change_sys = ifelse(year==2009,0,change_sys)) %>%
  mutate(change_admin = ifelse(is.na(change_admin) & (year==2009 | year==2015),0,change_admin)) %>%
  select(-lag_name, -count, -sum, -lag_sys)

# fix integration variables (NA means 0 physicians in that category, not actually missing)
hosp_data <- hosp_data %>%
  mutate(IPAP = ifelse(IPAHOS==0 & is.na(IPAP),0,IPAP),
         OPHP = ifelse(OPHOSHOS==0 & is.na(OPHP),0,OPHP),
         CPHP = ifelse(CPHOHOS==0 & is.na(CPHP),0,CPHP),
         ISMP = ifelse(ISMHOS==0 & is.na(ISMP),0,ISMP))

# hospital level analysis with outcomes of other hospital behaviors
varlist <- list("change_admin", "SUBS", "BDTOT", "IPAP", "OPHOSHOS", "CPHP", "ISMP", "PHYGP", "capital_cost_buildings", 
                "capital_cost_movableequip", "IPAHOS", "OPHOSHOS", "CPHOHOS", "ISMHOS", "FTRNTF", "IPAHOS", "CPHOHOS", "ISMHOS")
vl       <- unlist(varlist)


models <- lapply(varlist, function(x){
  all <- att_gt(yname = x,                # LHS Variable
                gname = "minyr_EHR",             # First year a unit is treated. (set to 0 if never treated)
                idname = "HospNPI",               # ID
                tname = "year",                  # Time Variable
                xformla = NULL,                 # No covariates
                data = dplyr::filter(hosp_data, minyr_EHR>0 & minyr_EHR!=2015),
                est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
                control_group = "notyettreated", # Set the control group to notyettreated or nevertreated
                clustervars = "HospNPI",          # Cluster Variables          
                anticipation=0,
                base_period = "universal" # can set a number of years to account for anticipation effects
  )
})

models_agg <- lapply(models, function(x){
  agg <- aggte(x, type = "dynamic",na.rm=T)
  p <- x$Wpval
  
  list(agg=agg, p=p)
})

graph_data <- lapply(models_agg, function(x){
  data <- as.data.frame(x[["agg"]][["egt"]]) %>%
    dplyr::rename(year=1) %>%
    cbind(as.data.frame(x[["agg"]][["att.egt"]])) %>%
    dplyr::rename(att=2) %>%
    cbind(as.data.frame(x[["agg"]][["se.egt"]])) %>%
    dplyr::rename(se=3) %>%
    mutate(upper=att+(1.96*se),
           lower=att-(1.96*se),
           group="All",
           year=as.factor(year)
    )
  p <- x[["p"]]
  
  list(data=data, p=p)
})

graphs <- lapply(graph_data, function(x){
  graph <- ggplot(x$data, aes(year, att)) +  
    geom_vline(xintercept="0", linetype="dashed", colour="red") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width=.0, size=1.1) +
    geom_point(size=2.5) +
    theme_bw() +
    theme_light() +
    geom_hline(yintercept=0, linetype="dashed") +
    theme(legend.position = "none", text = element_text(size = 15)) +
    xlab("") + ylab("Point Estimate and 95% CI\n") +
    geom_vline(xintercept=0, linetype="dashed", color="red") +
    theme(panel.grid.major.x = element_blank() ,
          panel.grid.major.y = element_line(size=.05, color="lightgray" )) +
    labs(caption=paste0("pre-trends test p-value: ",round(as.numeric(x$p),2))) + 
    theme(plot.caption=element_text(hjust = 0, size=15))
  
  list(graph=graph)
})

##=============================================================
## Hospital-level outcomes table
##   - overall ATT with 95% CI and BH-adjusted stars
##   - pre-treatment mean
##   - unadjusted and Benjamini-Hochberg p-values
##   - pre-trends test p-value
##
## NOTE: base_dat must use the SAME filter as the att_gt call
##       that produced `models`.
## <<< keep this identical to the data= argument in att_gt >>>
base_dat <- dplyr::filter(hosp_data, minyr_EHR > 0 & minyr_EHR != 2015 & minyr_EHR!=2009)

##---- Wald pre-trends test -----------------------------------
## pseudo-inverse; drops base periods and zero-variance cells
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

##---- Pre-treatment means and sample sizes -------------------
pre_means <- base_dat %>%
  ungroup() %>%
  filter(year == minyr_EHR - 1) %>%
  summarise(across(all_of(vl), ~ mean(., na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "outcome_raw", values_to = "m")

n_hosp <- map_dfr(vl, function(v) {
  d <- base_dat %>% ungroup() %>% filter(!is.na(.data[[v]]))
  tibble(outcome_raw = v, n_hosp = n_distinct(d$HospNPI))
})

pre_means <- left_join(pre_means, n_hosp, by = "outcome_raw")

##---- Assemble estimates -------------------------------------
chart_data <- map2_dfr(models, models_agg, function(mp, m) {
  a <- m$agg
  z <- a$overall.att / a$overall.se
  w <- wald_pre(mp)
  tibble(outcome_raw = a$DIDparams$yname,
         est    = a$overall.att,
         se     = a$overall.se,
         lower  = a$overall.att - 1.96 * a$overall.se,
         upper  = a$overall.att + 1.96 * a$overall.se,
         p      = 2 * pnorm(-abs(z)),
         p_pre  = unname(w["p"]),
         df_pre = unname(w["df"]))
}) %>%
  mutate(p_bh = p.adjust(p, method = "BH")) %>%
  left_join(pre_means, by = "outcome_raw")

##---- Labels and formatting ----------------------------------
LABS <- c(change_admin = "Change Admin",
          SUBS = "Owns Subsidiary",
          BDTOT = "No. Beds",
          FTRNTF = "No. Nurses",
          PHYGP = "No. Physicians",
          capital_cost_buildings    = "Capital Expenses: Buildings",
          capital_cost_movableequip = "Capital Expenses: Equipment",
          IPAHOS   = "Has IPA",
          OPHOSHOS = "Has OPHO",
          CPHOHOS  = "Has CPHO",
          ISMHOS   = "Has FIO",
          IPAP = "No. Physicians, IPA",
          CPHP = "No. Physicians, CPHO",
          ISMP = "No. Physicians, FIO")

fmt_num <- function(x) ifelse(is.na(x), "---",
                              ifelse(abs(x) >= 1000, formatC(x, format = "d", big.mark = ","),
                                     sprintf("%.2f", x)))
fmt_p   <- function(p) ifelse(is.na(p), "---",
                              ifelse(p < 0.01, "$<$0.01", sprintf("%.2f", p)))

chart_tab <- chart_data %>%
  mutate(
    outcome  = recode(outcome_raw, !!!LABS),
    stars    = case_when(is.na(p_bh)  ~ "",
                         p_bh < 0.01  ~ "$^{***}$",
                         p_bh < 0.05  ~ "$^{**}$",
                         p_bh < 0.10  ~ "$^{*}$",
                         TRUE         ~ ""),
    ## estimate (with stars) on the first line, CI on the second
    EstCI    = sprintf("\\makecell{%s%s\\\\{[}%s, %s{]}}",
                       fmt_num(est), stars, fmt_num(lower), fmt_num(upper)),
    PreMean  = fmt_num(m),
    Nhosp    = formatC(n_hosp, format = "d", big.mark = ","),
    Pbh      = fmt_p(p_bh),
    Ppre     = fmt_p(p_pre)
  ) %>%
  select(outcome, PreMean, Nhosp, EstCI, Pbh, Ppre)

##---- Row order ----------------------------------------------
ord <- c("capital_cost_buildings", "capital_cost_movableequip",
         "SUBS", "change_admin", "BDTOT",
         "FTRNTF", "PHYGP",
         "IPAHOS", "OPHOSHOS", "CPHOHOS", "ISMHOS",
         "IPAP", "CPHP", "ISMP")

stopifnot(all(recode(ord, !!!LABS) %in% chart_tab$outcome))
chart_tab <- chart_tab[match(recode(ord, !!!LABS), chart_tab$outcome), ]

##---- Table --------------------------------------------------
out <- kbl(chart_tab, format = "latex", booktabs = TRUE, escape = FALSE,
           col.names = c("Outcome",
                         "\\makecell{Pre-treatment\\\\mean}",
                         "Hospitals",
                         "\\makecell{Estimate\\\\{[}95\\% CI{]}}",
                         "\\makecell{BH\\\\$p$-value}",
                         "Pre-trends"),
           align = c("l", "c", "c", "c", "c", "c"),
           caption = "EHR Adoption and Other Hospital Behaviors",
           label = "hosp_outcomes", position = "ht!", row.names = FALSE) %>%
  kable_styling(full_width = FALSE) %>%
  column_spec(1, width = "15em") %>%
  column_spec(4, width = "8em") %>%
  pack_rows(index = c("Non-EHR Investment/Organizational Changes" = 5,
                      "Workforce" = 2,
                      "Physician Arrangements" = 7)) %>%
  footnote(general = paste(
    "Each row reports the overall average treatment effect on the treated from",
    "Callaway and Sant'Anna (2021), aggregated over event time, with the",
    "95\\% confidence interval in brackets.",
    "$p$-values are adjusted across all outcomes in the table using the",
    "Benjamini--Hochberg procedure to control the false discovery rate.",
    "The pre-trends column reports a Wald test of the null that all",
    "pre-treatment group-time effects equal zero.",
    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."),
    threeparttable = TRUE, escape = FALSE)
cat(out)
writeLines(as.character(out), file.path("Table Code", "hosp_analysis.tex"))

