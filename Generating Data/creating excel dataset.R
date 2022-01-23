# This script will save the dataset generated for this cell
# This script is nested within 'GGUM_SIM.R" and feeds into 
# 'creating jags files.R'

# Format data to use in jags
jags_rsp <- responses %>%
  dplyr::select(id, item, rsp, rt) %>%
  dplyr::mutate(rsp = rsp + 1)%>%
  tidyr::pivot_wider(values_from=c(rsp, rt), names_from=item) %>%
  dplyr::select(starts_with("rsp"))  %>%
  mutate(across(everything(), as.numeric))

jags_rt <- responses %>%
  dplyr::select(id, item, rsp, rt) %>%
  dplyr::mutate(rsp = rsp + 1)%>%
  tidyr::pivot_wider(values_from=c(rsp, rt), names_from=item) %>%
  dplyr::select(starts_with("rt"))  %>%
  mutate(across(everything(), as.numeric))

jags_b <- item_sample$delta
jags_a <- item_sample$alpha

jags_theta <- thetas

jags_tau <- thresholds[,1:num_cats]

#tau[ 1,2] ~ dlnorm( 1.098,1);
jags_tau_priors <- tau_priors %>%
  dplyr::mutate(item = row_number(),
                tau_0 = NA) %>%
  tidyr::pivot_longer(!item, names_to = "index", values_to = "prior") %>%
  dplyr::mutate(index = as.numeric(str_remove(index, "tau_"))+1,
                save = paste0("tau[", item, ",", index, "] ~ dlnorm(", round(prior,digits=6),
                              ", 1);")
  )%>%
  drop_na()%>%
  pull(save)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "resp")
openxlsx::addWorksheet(wb, "b true")
openxlsx::addWorksheet(wb, "a true")
openxlsx::addWorksheet(wb, "tau true")
openxlsx::addWorksheet(wb, "theta true")
openxlsx::addWorksheet(wb, "rt true")
openxlsx::addWorksheet(wb, "b start")
openxlsx::addWorksheet(wb, "a start")
openxlsx::addWorksheet(wb, "tau start")
openxlsx::addWorksheet(wb, "theta start")
openxlsx::addWorksheet(wb, "tau priors")

openxlsx::writeData(wb, "resp", jags_rsp, colNames=FALSE, rowNames=FALSE)
openxlsx::writeData(wb, "b true", jags_b, colNames=FALSE, rowNames=FALSE)
openxlsx::writeData(wb, "a true", jags_a, colNames=FALSE, rowNames=FALSE)
openxlsx::writeData(wb, "tau true", jags_tau, colNames=FALSE, rowNames=FALSE)
openxlsx::writeData(wb, "theta true", jags_theta, colNames=FALSE, rowNames=FALSE)
openxlsx::writeData(wb, "rt true", jags_rt, colNames=FALSE, rowNames=FALSE)

openxlsx::writeData(wb, "b start", start_delta, colNames=FALSE, rowNames=FALSE)
openxlsx::writeData(wb, "a start", start_alpha, colNames=FALSE, rowNames=FALSE)
openxlsx::writeData(wb, "tau start", start_tau, colNames=FALSE, rowNames=FALSE)
openxlsx::writeData(wb, "theta start", start_theta, colNames=FALSE, rowNames=FALSE)

openxlsx::writeData(wb, "tau priors", jags_tau_priors, colNames=FALSE, rowNames=FALSE)

openxlsx::saveWorkbook(wb, file = paste0(file_prefix, "_DATASET.xlsx"), overwrite=TRUE)