library(tidyverse)
library(psych)
library(readxl)
library(broom)
library(lsr)

# get data
source("./Simulation Analysis/anovas_full.R")

# GGUM-RT #####

## Table X – Mean RMSD of Parameter Estimates ####
bind_rows(
  (df %>%
    dplyr::group_by(family, sample_size) %>%
    dplyr::summarize(ggumrt_rmsd = mean(as.numeric(ggumrt_rmsd))) %>%
    ungroup() %>%
    pivot_wider(id_cols = sample_size, names_from = family, values_from = ggumrt_rmsd)),
  (df %>%
    dplyr::group_by(family, test_length) %>%
    dplyr::summarize(ggumrt_rmsd = mean(as.numeric(ggumrt_rmsd))) %>%
    ungroup() %>%
    pivot_wider(id_cols = test_length, names_from = family, values_from = ggumrt_rmsd)),
  (df %>%
    dplyr::group_by(family, num_cats) %>%
    dplyr::summarize(ggumrt_rmsd = mean(as.numeric(ggumrt_rmsd))) %>%
    ungroup() %>%
    pivot_wider(id_cols = num_cats, names_from = family, values_from = ggumrt_rmsd))
) %>%
  dplyr::relocate(a, b, tau, theta, RT_0, RT_1, RT_2) %>%
  dplyr::mutate(
    RT_0 = RT_0,
    RT_1 = RT_1,
    RT_2 = RT_2
  ) %>%
  mutate(across(c(a, b, tau, theta, RT_0, RT_1, RT_2), ~ round(., digits = 3))) %>%
  clipr::write_clip()

## GGUM-RT: SEVEN Between-replications factorial ANOVAs
# Main effects: Sample size, test length, number of response categories
# Seven, one per parameter:

# Item discriminations
m1_rt_rmsd <- sim.aov(df, "ggumrt", "rmsd", 0.007, "a")
m1_rt_se <- sim.aov(df, "ggumrt", "se", 0.007, "a")

# Item locations
m2_rt_rmsd <- sim.aov(df, "ggumrt", "rmsd", 0.007, "b")
m2_rt_se <- sim.aov(df, "ggumrt", "se", 0.007, "b")

# SRC Thresholds
m3_rt_rmsd <- sim.aov(df, "ggumrt", "rmsd", 0.007, "tau")
m3_rt_se <- sim.aov(df, "ggumrt", "se", 0.007, "tau")

# Thetas
m4_rt_rmsd <- sim.aov(df, "ggumrt", "rmsd", 0.007, "theta")
m4_rt_se <- sim.aov(df, "ggumrt", "se", 0.007, "theta")

# b0
m5_rt_rmsd <- sim.aov(df, "ggumrt", "rmsd", 0.007, "RT_0")
m5_rt_se <- sim.aov(df, "ggumrt", "se", 0.007, "RT_0")

# b1
m6_rt_rmsd <- sim.aov(df, "ggumrt", "rmsd", 0.007, "RT_1")
m6_rt_se <- sim.aov(df, "ggumrt", "se", 0.007, "RT_1")

# b2
m7_rt_rmsd <- sim.aov(df, "ggumrt", "rmsd", 0.007, "RT_2")
m7_rt_se <- sim.aov(df, "ggumrt", "se", 0.007, "RT_2")

# compile results
termOrder <- c(
  "sample_size", "test_length", "num_cats",
  "test_length:sample_size", "num_cats:sample_size", "num_cats:test_length", "num_cats:test_length:sample_size"
)

## Table X - Eta^2 values for ANOVA effects ####
dplyr::full_join(m1_rt_rmsd, m2_rt_rmsd, by = "term") %>%
  dplyr::full_join(m3_rt_rmsd, by = "term") %>%
  dplyr::full_join(m4_rt_rmsd, by = "term") %>%
  dplyr::full_join(m5_rt_rmsd, by = "term") %>%
  dplyr::full_join(m6_rt_rmsd, by = "term") %>%
  dplyr::full_join(m7_rt_rmsd, by = "term") %>%
  .[order(match(.$term, termOrder)), ] %>%
  clipr::write_clip()


# ## Plotting interaction effects ####
df %>%
  dplyr::filter(family == "a") %>%
  dplyr::group_by(sample_size, num_cats) %>%
  dplyr::summarize(ggumrt_rmsd = mean(ggumrt_rmsd)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = sample_size, y = ggumrt_rmsd, color = num_cats, group = num_cats)) +
  geom_line(size = 1) +
  xlab("Sample Size") +
  ylab("Mean RMSD") +
  labs(color = "Response Categories")

## RMSD components: rmse_calculations_corrected.R ####

# GGUM ####
## Table X – Mean RMSD of Parameter Estimates ####
bind_rows(
  (df %>%
    dplyr::group_by(family, sample_size) %>%
    dplyr::summarize(ggum_rmsd = mean(as.numeric(ggum_rmsd))) %>%
    ungroup() %>%
    pivot_wider(id_cols = sample_size, names_from = family, values_from = ggum_rmsd)),
  (df %>%
    dplyr::group_by(family, test_length) %>%
    dplyr::summarize(ggum_rmsd = mean(as.numeric(ggum_rmsd))) %>%
    ungroup() %>%
    pivot_wider(id_cols = test_length, names_from = family, values_from = ggum_rmsd)),
  (df %>%
    dplyr::group_by(family, num_cats) %>%
    dplyr::summarize(ggum_rmsd = mean(as.numeric(ggum_rmsd))) %>%
    ungroup() %>%
    pivot_wider(id_cols = num_cats, names_from = family, values_from = ggum_rmsd))
) %>%
  dplyr::relocate(a, b, tau, theta) %>%
  mutate(across(c(a, b, tau, theta), ~ round(., digits = 3))) %>%
  clipr::write_clip()


## GGUM: FOUR Between-replications factorial ANOVAs
# Main effects: Sample size, test length, number of response categories
# Four, one per parameter:

# Item discriminations
m1_og <- sim.aov(df, "ggum", "rmsd", 0.0125, "a")

# Item locations
m2_og <- sim.aov(df, "ggum", "rmsd", 0.0125, "b")

# SRC Thresholds
m3_og <- sim.aov(df, "ggum", "rmsd", 0.0125, "tau")

# Thetas
m4_og <- sim.aov(df, "ggum", "rmsd", 0.0125, "theta")

## Table X - Eta^2 values for ANOVA effects ####
dplyr::full_join(m1_og, m2_og, by = "term") %>%
  dplyr::full_join(m3_og, by = "term") %>%
  dplyr::full_join(m4_og, by = "term") %>%
  .[order(match(.$term, termOrder)), ] %>%
  clipr::write_clip()

## Plotting interaction effects ####
df %>%
  dplyr::filter(family == "a") %>%
  dplyr::group_by(sample_size, num_cats) %>%
  dplyr::summarize(ggum_rmsd = mean(ggum_rmsd)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = sample_size, y = ggum_rmsd, color = num_cats, group = num_cats)) +
  geom_line(size = 1) +
  xlab("Sample Size") +
  ylab("Mean RMSD") +
  labs(color = "Response Categories")

df %>%
  dplyr::filter(family == "b") %>%
  dplyr::group_by(test_length, num_cats) %>%
  dplyr::summarize(ggum_rmsd = mean(ggum_rmsd)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = test_length, y = ggum_rmsd, color = num_cats, group = num_cats)) +
  geom_line(size = 1) +
  xlab("Test Length") +
  ylab("Mean RMSD") +
  labs(color = "Response Categories")

df %>%
  dplyr::filter(family == "tau") %>%
  dplyr::group_by(test_length, num_cats) %>%
  dplyr::summarize(ggum_rmsd = mean(ggum_rmsd)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = test_length, y = ggum_rmsd, color = num_cats, group = num_cats)) +
  geom_line(size = 1) +
  xlab("Test Length") +
  ylab("Mean RMSD") +
  labs(color = "Response Categories")

## RMSD components: rmse_calculations_corrected.R ####

# Comparison of GGUM to GGUM-RT: FOUR Split-plot ANOVAs ####
# Four, one per parameter:
# Item discriminations
#

# I ended up doing this in SPSS ##
library(haven)
#write_sav(df %>% dplyr::filter(!(family %in% c("RT_0", "RT_1", "RT_2"))), "Simulation Analysis/SPSS/20230818/202300818 anovas_df.sav")

## Table X – Mean SE of Parameter Estimates ####
## GGUM values
bind_rows(
  (df %>%
    dplyr::group_by(family, sample_size) %>%
    dplyr::summarize(ggum_se_timeseries = mean(as.numeric(ggum_se_timeseries))) %>%
    ungroup() %>%
    pivot_wider(id_cols = sample_size, names_from = family, values_from = ggum_se_timeseries)),
  (df %>%
    dplyr::group_by(family, test_length) %>%
    dplyr::summarize(ggum_se_timeseries = mean(as.numeric(ggum_se_timeseries))) %>%
    ungroup() %>%
    pivot_wider(id_cols = test_length, names_from = family, values_from = ggum_se_timeseries)),
  (df %>%
    dplyr::group_by(family, num_cats) %>%
    dplyr::summarize(ggum_se_timeseries = mean(as.numeric(ggum_se_timeseries))) %>%
    ungroup() %>%
    pivot_wider(id_cols = num_cats, names_from = family, values_from = ggum_se_timeseries))
) %>%
  dplyr::relocate(a, b, tau, theta) %>%
  mutate(across(c(a, b, tau, theta), ~ round(., digits = 3))) %>%
  select(-RT_0, -RT_1, -RT_2) %>%
  clipr::write_clip()

## GGUM-RT values
bind_rows(
  (df %>%
    dplyr::group_by(family, sample_size) %>%
    dplyr::summarize(ggumrt_se_timeseries = mean(as.numeric(ggumrt_se_timeseries))) %>%
    ungroup() %>%
    pivot_wider(id_cols = sample_size, names_from = family, values_from = ggumrt_se_timeseries)),
  (df %>%
    dplyr::group_by(family, test_length) %>%
    dplyr::summarize(ggumrt_se_timeseries = mean(as.numeric(ggumrt_se_timeseries))) %>%
    ungroup() %>%
    pivot_wider(id_cols = test_length, names_from = family, values_from = ggumrt_se_timeseries)),
  (df %>%
    dplyr::group_by(family, num_cats) %>%
    dplyr::summarize(ggumrt_se_timeseries = mean(as.numeric(ggumrt_se_timeseries))) %>%
    ungroup() %>%
    pivot_wider(id_cols = num_cats, names_from = family, values_from = ggumrt_se_timeseries))
) %>%
  dplyr::relocate(a, b, tau, theta) %>%
  mutate(across(c(a, b, tau, theta), ~ round(., digits = 3))) %>%
  select(-RT_0, -RT_1, -RT_2) %>%
  clipr::write_clip()

## Plotting interaction effects ####
## num_cats, test_length, sample_size
df %>%
  dplyr::filter(family == "tau") %>%
  dplyr::group_by(num_cats) %>%
  dplyr::summarize(
    ggum_se = mean(ggum_se_timeseries),
    ggumrt_se = mean(ggumrt_se_timeseries)
  ) %>%
  dplyr::ungroup() %>%
  pivot_longer(cols = c("ggum_se", "ggumrt_se"), names_to = "method", values_to = "se") %>%
  dplyr::mutate(method = ifelse(method == "ggum_se", "GGUM", "GGUM-RT")) %>%
  ggplot(aes(x = num_cats, y = se, color = method, group = method)) +
  geom_line(size = 1) +
  xlab("Response Categories") +
  ylab("Mean SE") +
  labs(color = "Method")

df %>%
  dplyr::filter(family == "theta") %>%
  dplyr::group_by(test_length) %>%
  dplyr::summarize(
    ggum_rmsd = mean(ggum_rmsd),
    ggumrt_rmsd = mean(ggumrt_rmsd)
  ) %>%
  dplyr::ungroup() %>%
  pivot_longer(cols = c("ggum_rmsd", "ggumrt_rmsd"), names_to = "method", values_to = "rmsd") %>%
  dplyr::mutate(method = ifelse(method == "ggum_rmsd", "GGUM", "GGUM-RT")) %>%
  ggplot(aes(x = test_length, y = rmsd, color = method, group = method)) +
  geom_line(size = 1) +
  xlab("Test Length") +
  ylab("Mean RMSD") +
  labs(color = "Method")

df %>%
  dplyr::filter(family == "tau") %>%
  dplyr::group_by(num_cats) %>%
  dplyr::summarize(
    ggum_se = mean(ggum_se_timeseries),
    ggumrt_se = mean(ggumrt_se_timeseries)
  ) %>%
  dplyr::ungroup() %>%
  pivot_longer(cols = c("ggum_se", "ggumrt_se"), names_to = "method", values_to = "se") %>%
  dplyr::mutate(method = ifelse(method == "ggum_se", "GGUM", "GGUM-RT")) %>%
  ggplot(aes(x = num_cats, y = se, color = method, group = method)) +
  geom_line(size = 1) +
  xlab("Response Categories") +
  ylab("Mean SE") +
  labs(color = "Method")

df %>%
  dplyr::filter(family == "theta") %>%
  # dplyr::group_by(sample_size) %>%
  # dplyr::summarize(
  #   ggum_se = mean(ggum_se_timeseries),
  #   ggumrt_se = mean(ggumrt_se_timeseries)
  # ) %>%
  # dplyr::ungroup() %>%
  pivot_longer(cols = c("ggum_se", "ggumrt_se"), names_to = "method", values_to = "se") %>%
  dplyr::mutate(method = ifelse(method == "ggum_se", "GGUM", "GGUM-RT")) %>%
  ggplot(aes(x = sample_size, y = se, color = method, group = method)) +
  geom_line(size = 1) +
  xlab("Sample Size") +
  ylab("Mean SE") +
  labs(color = "Method")


p1 <- df %>%
  dplyr::filter(family == "a" &
                  num_cats ==2) %>%
  dplyr::group_by(test_length) %>%
  dplyr::summarize(
    ggum_rmsd = mean(ggum_rmsd),
    ggumrt_rmsd = mean(ggumrt_rmsd)
  ) %>%
  dplyr::ungroup() %>%
  pivot_longer(cols = c("ggum_rmsd", "ggumrt_rmsd"), names_to = "method", values_to = "rmsd") %>%
  dplyr::mutate(method = ifelse(method == "ggum_rmsd", "GGUM", "GGUM-RT")) %>%
  ggplot(aes(x = test_length, y = rmsd, color = method, group = method)) +
  geom_line(size = 1) +
  xlab("Test Length") +
  ylab("Mean RMSD") +
  labs(color = "Method") +
  ggtitle("Response Categories = 2")

p2 <- df %>%
  dplyr::filter(family == "a" &
                  num_cats == 6) %>%
  dplyr::group_by(test_length) %>%
  dplyr::summarize(
    ggum_rmsd = mean(ggum_rmsd),
    ggumrt_rmsd = mean(ggumrt_rmsd)
  ) %>%
  dplyr::ungroup() %>%
  pivot_longer(cols = c("ggum_rmsd", "ggumrt_rmsd"), names_to = "method", values_to = "rmsd") %>%
  dplyr::mutate(method = ifelse(method == "ggum_rmsd", "GGUM", "GGUM-RT")) %>%
  ggplot(aes(x = test_length, y = rmsd, color = method, group = method)) +
  geom_line(size = 1) +
  xlab("Test Length") +
  ylab("Mean RMSD") +
  labs(color = "Method") +
  ggtitle("Response Categories = 6")

df %>%
  dplyr::filter(family == "a" ) %>%
  dplyr::group_by(test_length) %>%
  dplyr::summarize(
    ggum_rmsd = mean(ggum_rmsd),
    ggumrt_rmsd = mean(ggumrt_rmsd)
  ) %>%
  dplyr::ungroup() %>%
  pivot_longer(cols = c("ggum_rmsd", "ggumrt_rmsd"), names_to = "method", values_to = "rmsd") %>%
  dplyr::mutate(method = ifelse(method == "ggum_rmsd", "GGUM", "GGUM-RT")) %>%
  ggplot(aes(x = test_length, y = rmsd, color = method, group = method)) +
  geom_line(size = 1) +
  xlab("Test Length") +
  ylab("Mean RMSD") +
  labs(color = "Method") 

library(ggpubr)
ggarrange(p1, p2,  ncol=2, common.legend = TRUE, legend="bottom")
