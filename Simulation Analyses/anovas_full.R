#' RUN GGUM IN JAGS
#'
#' @description
#' This script runs JAGs for the GGUM portion of the simulation and outputs results
#' Requires user to enter cell+rep to run
#'
#' @CELL the cell to be run
#' @REP the replication to be run

library(tidyverse)
library(psych)
library(readxl)
library(broom)
library(lsr)

# source("./Simulation Analysis/analysis_functions.R")

CELL_ALL <- c(1:8)
REP_ALL <- c(1:10)

compare_df <- data.frame()
rmsd2 <- data.frame()

for (c in 1:length(CELL_ALL)) {
  for (reps in 1:length(REP_ALL)) {
    # for (c in 8) {
    # for (reps in 10) {
    CELL <- CELL_ALL[c]
    REP <- REP_ALL[reps]

    # Call appropriate directories
    starting.directory <- data.frame(folders = c(list.dirs(path = "Simulation"))) %>%
      filter(str_detect(folders, paste0("CELL", !!CELL)) &
        str_detect(folders, paste0("REP", !!REP, "$"))) %>%
      dplyr::pull()

    file_prefix <- paste0(starting.directory, "/", str_sub(starting.directory, 12), "_")

    print(file_prefix)

    # Import start values for RT I forgot to save
    # rt_start <- read.delim(paste0(file_prefix, "MODEL GGUM-RT.R"), col.names = "V1") %>%
    #   dplyr::filter(grepl("^rt_b*", V1)) %>%
    #   dplyr::mutate(
    #     ParameterID = stringr::str_extract(V1, "\\w{4}\\d"),
    #     truth = as.double(stringr::str_extract(V1, "[^(]([0-9])+.([0-9])+"))
    #  )

    rt_start <- data.frame(
      ParameterID = c("rt_b0", "rt_b1", "rt_b2"),
      # truth = c(4456, 308, -11.5)
      truth = c(4500, 400, -20)
    )

    rt_se <- readxl::read_excel(
      path = paste0(starting.directory, "/GGUMRT/", str_sub(starting.directory, 12), "_SE ESTIMATES.xlsx")
    ) %>%
      rename(
        ParameterID = parameter,
        ggumrt_estimate = estimate,
        ggumrt_se_naive = se_naive,
        ggumrt_se_timeseries = se_timeseries
      ) %>%
      dplyr::mutate(
        ggumrt_estimate = as.double(ggumrt_estimate),
        ggumrt_se_naive = as.double(ggumrt_se_naive),
        ggumrt_se_timeseries = as.double(ggumrt_se_timeseries)
      ) %>%
      dplyr::select(-ggumrt_estimate)

    # Import Estimates
    ggumrt <- readxl::read_excel(
      path = paste0(starting.directory, "/GGUMRT/", str_sub(starting.directory, 12), "_ESTIMATES GGUM-RT.xlsx"),
      sheet = "comparison", col_names = c("ParameterID", "ggumrt_estimate", "truth", "family")
    ) %>%
      #  replace_na(list(family = "RT")) %>%
      dplyr::mutate(
        family = case_when(
          is.na(family) & ParameterID == "rt_b0" ~ "RT_0",
          is.na(family) & ParameterID == "rt_b1" ~ "RT_1",
          is.na(family) & ParameterID == "rt_b2" ~ "RT_2",
          TRUE ~ family
        ),
        truth =
          case_when(
            is.na(truth) & ParameterID == "rt_b0" ~ rt_start[rt_start$ParameterID == "rt_b0", ]$truth,
            is.na(truth) & ParameterID == "rt_b1" ~ rt_start[rt_start$ParameterID == "rt_b1", ]$truth,
            is.na(truth) & ParameterID == "rt_b2" ~ rt_start[rt_start$ParameterID == "rt_b2", ]$truth,
            !is.na(truth) ~ truth
          )
      ) %>%
      dplyr::left_join(rt_se, by = c("ParameterID"))

    ggum_se <- readxl::read_excel(
      path = paste0(file_prefix, "SE ESTIMATES.xlsx")
    ) %>%
      rename(
        ParameterID = parameter,
        ggum_estimate = estimate,
        ggum_se_naive = se_naive,
        ggum_se_timeseries = se_timeseries
      ) %>%
      dplyr::filter(ParameterID != "parameter") %>%
      dplyr::mutate(
        ggum_estimate = round(as.double(ggum_estimate), digits = 3),
        ggum_se_naive = as.double(ggum_se_naive),
        ggum_se_timeseries = as.double(ggum_se_timeseries)
      ) %>%
      dplyr::select(-ggum_estimate)

    ggum <- try(readxl::read_excel(
      path = paste0(file_prefix, "ESTIMATES.xlsx"),
      sheet = "comparison", col_names = c("ParameterID", "ggum_estimate", "truth", "family")
    ) %>%
      dplyr::filter(ParameterID != "parameter") %>%
      dplyr::mutate(
        ggum_estimate = as.double(ggum_estimate),
        truth = as.double(truth)
      )) %>%
      dplyr::left_join(ggum_se, by = c("ParameterID"))

    compare <- try(readxl::read_excel(
      path = paste0(file_prefix, "ESTIMATES.xlsx"),
      sheet = "comparison", col_names = c("ParameterID", "estimate", "truth", "family")
    ) %>%
      dplyr::filter(ParameterID != "parameter") %>%
      dplyr::mutate(
        estimate = as.double(estimate),
        truth = as.double(truth)
      ) %>%
      dplyr::select(-estimate, -family) %>%
      dplyr::full_join(ggumrt, by = c("ParameterID", "truth")) %>%
      dplyr::full_join(ggum, by = c("ParameterID", "truth", "family")))

    # adj <- compare %>%
    #   dplyr::filter(family == "theta") %>%
    #   dplyr::summarize(ggum_constant = coef(summary(lm(truth ~ ggum_estimate, data=.)))[1],
    #                    ggum_scale = coef(summary(lm(truth ~ ggum_estimate, data=.)))[2],
    #                    ggumrt_constant = coef(summary(lm(truth ~ ggumrt_estimate, data=.)))[1],
    #                    ggumrt_scale = coef(summary(lm(truth ~ ggumrt_estimate, data=.)))[2])

    adj0 <- compare %>%
      dplyr::filter(family == "theta") %>%
      dplyr::summarize(
        ggum_constant = coef(summary(lm(ggum_estimate ~ truth, data = .)))[1],
        ggum_scale = coef(summary(lm(ggum_estimate ~ truth, data = .)))[2],
        ggumrt_constant = coef(summary(lm(ggumrt_estimate ~ truth, data = .)))[1],
        ggumrt_scale = coef(summary(lm(ggumrt_estimate ~ truth, data = .)))[2],
        ggum_flip = ifelse(ggum_scale < 0, "Y", "N"),
        ggumrt_flip = ifelse(ggumrt_scale < 0, "Y", "N")
      )

    ggum_flip <- adj0$ggum_flip
    ggumrt_flip <- adj0$ggumrt_flip

    adj <- compare %>%
      dplyr::mutate(
        ggumrt_estimate =
          case_when(
            family %in% c("b", "theta") & !!ggumrt_flip == "Y" ~
              ggumrt_estimate * -1,
            TRUE ~ ggumrt_estimate
          ),
        ggum_estimate =
          case_when(
            family %in% c("b", "theta") & !!ggum_flip == "Y" ~
              ggum_estimate * -1,
            TRUE ~ ggum_estimate
          )
      ) %>%
      dplyr::filter(family == "theta") %>%
      dplyr::summarize(
        ggum_constant = coef(summary(lm(ggum_estimate ~ truth, data = .)))[1],
        ggum_scale = coef(summary(lm(ggum_estimate ~ truth, data = .)))[2],
        ggumrt_constant = coef(summary(lm(ggumrt_estimate ~ truth, data = .)))[1],
        ggumrt_scale = coef(summary(lm(ggumrt_estimate ~ truth, data = .)))[2],
        ggum_flip = ifelse(ggum_scale < 0, "Y", "N"),
        ggumrt_flip = ifelse(ggumrt_scale < 0, "Y", "N")
      )

    compare <- compare %>%
      dplyr::mutate(
        ggum_constant = adj$ggum_constant,
        ggum_scale = adj$ggum_scale,
        ggumrt_constant = adj$ggumrt_constant,
        ggumrt_scale = adj$ggumrt_scale,
        ggumrt_estimate =
          case_when(
            family %in% c("b", "theta") & !!ggumrt_flip == "Y" ~
              ggumrt_estimate * -1,
            TRUE ~ ggumrt_estimate
          ),
        ggum_estimate =
          case_when(
            family %in% c("b", "theta") & !!ggum_flip == "Y" ~
              ggum_estimate * -1,
            TRUE ~ ggum_estimate
          )
      ) %>%
      dplyr::mutate(
        ggumrt_estimate =
          case_when(
            family %in% c("a") ~ ggumrt_estimate * ggumrt_scale, # 8-18-23: Changing a to be multiplied by the scale value not divided
            family %in% c("tau") ~ ggumrt_estimate / ggumrt_scale,
            family %in% c("b", "theta") ~ (ggumrt_estimate - ggumrt_constant) / ggumrt_scale,
            family == "RT_0" ~ ggumrt_estimate,
            family == "RT_1" ~ ggumrt_estimate * ggumrt_scale^2,
            family == "RT_2" ~ ggumrt_estimate * ggumrt_scale^4
          ),
        ggum_estimate =
          case_when(
            family %in% c("a") ~ ggum_estimate * ggum_scale, # 8-18-23: Changing a to be multiplied by the scale value not divided
            family %in% c("tau") ~ ggum_estimate / ggum_scale,
            family %in% c("b", "theta") ~ (ggum_estimate - ggum_constant) / ggum_scale,
            family == "RT_0" ~ ggum_estimate,
            family == "RT_1" ~ ggum_estimate * ggum_scale^2,
            family == "RT_2" ~ ggum_estimate * ggum_scale^4
          )
      )


    rmsd2_0 <- compare %>%
      dplyr::mutate(
        # ggumrt_num = (abs(ggumrt_estimate) - abs(truth))^2,
        # ggum_num = (abs(ggum_estimate) - abs(truth))^2
        ggumrt_num = (ggumrt_estimate - truth)^2,
        ggum_num = (ggum_estimate - truth)^2
      ) %>%
      dplyr::group_by(family) %>%
      dplyr::summarize(
        t = n(),
        ggum_sum = sum(ggum_num),
        ggum_rmsd = (sum(ggum_num) / n())^(1 / 2), # corrected from ^2
        ggumrt_sum = sum(ggumrt_num),
        ggumrt_rmsd = (sum(ggumrt_num) / n())^(1 / 2), # corrected from ^2
        ggum_se_naive = mean(ggum_se_naive),
        ggum_se_timeseries = mean(ggum_se_timeseries),
        ggumrt_se_naive = mean(ggumrt_se_naive),
        ggumrt_se_timeseries = mean(ggumrt_se_timeseries),
        truth_mean = mean(truth),
        truth_sd = sd(truth),
        ggum_mean = mean(ggum_estimate),
        ggum_sd = sd(ggum_estimate),
        ggumrt_mean = mean(ggumrt_estimate),
        ggumrt_sd = sd(ggumrt_estimate),
        ggum_constant = mean(ggum_constant),
        ggum_scale = abs(mean(ggum_scale)),
        ggumrt_constant = mean(ggumrt_constant),
        ggumrt_scale = abs(mean(ggumrt_scale))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        cell = paste0("Cell_", CELL),
        rep = paste0("Rep_", REP),
        ggumrt_flip = !!ggumrt_flip,
        ggum_flip = !!ggum_flip
      ) %>%
      relocate(cell, rep)

    compare_df <- bind_rows(compare_df, compare %>%
      dplyr::mutate(
        cell = paste0("Cell_", CELL),
        rep = paste0("Rep_", REP)
      ) %>%
      relocate(cell, rep))

    if (is.data.frame(rmsd2_0) == TRUE) {
      rmsd2 <- bind_rows(rmsd2, rmsd2_0)
    }
  }
}

rmsd2_s <- rmsd2 %>%
  dplyr::mutate(
    diff = ggum_rmsd - ggumrt_rmsd,
    better = ifelse(ggum_rmsd > ggumrt_rmsd, "GGUM-RT", "GGUM")
  )

# making sure I have all my parameters
table(rmsd2_s$rep, rmsd2_s$family, rmsd2_s$cell, useNA = "ifany")

# checking which did better
## overall
table(rmsd2_s$better)
table(rmsd2_s$better) %>% prop.table()

## by parameter
table(rmsd2_s$family, rmsd2_s$better)
table(rmsd2_s$family, rmsd2_s$better) %>% prop.table(1)

# ANOVA prep
# Sim structure
sim_str <- data.frame(
  cell = paste0("Cell_", c(1:8)),
  num_cats = as.factor(rep(c(6, 2), each = 2, times = 2)),
  test_length = as.factor(rep(c(10, 30), 4)),
  sample_size = as.factor(rep(c(500, 2000), each = 4))
)

df <- rmsd2 %>%
  dplyr::left_join(sim_str, by = "cell")
