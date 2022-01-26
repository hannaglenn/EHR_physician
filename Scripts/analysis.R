library(did)

no_retire_data <- Physician_Data %>%
  dplyr::filter(ever_retire==0)


out <- att_gt(yname = "hosp_patient_count",
              gname = "minyr_EHR",
              idname = "DocNPI",
              tname = "year",
              xformla = ~age,
              data = no_retire_data,
              est_method = "reg",
              control_group = "notyettreated"
)
ggdid(out)

out_dyn <- aggte(out, type = "dynamic",balance_e=1)
ggdid(out_dyn)
