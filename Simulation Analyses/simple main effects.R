library(broom)

# for split plots with SE
tukey_table1=function(x) {
  x.1=aov(se~method,data=x)
  tidy(TukeyHSD(x.1))
}

## num_cats, test_length, sample_size

# Theta: Sample Size x Estimation Method
df %>%
  dplyr::filter(family == "theta") %>%
  pivot_longer(cols = c("ggum_se_timeseries", "ggumrt_se_timeseries"), names_to = "method", values_to = "se") %>%
  dplyr::mutate(method = ifelse(method == "ggum_se_timeseries", "GGUM", "GGUM-RT")) %>%
  nest(-sample_size) %>%
  mutate(tukey=map(data,tukey_table1)) %>% 
  unnest(tukey)

# Theta: Response Categories x Estimation Method
df %>%
  dplyr::filter(family == "theta") %>%
  pivot_longer(cols = c("ggum_se_timeseries", "ggumrt_se_timeseries"), names_to = "method", values_to = "se") %>%
  dplyr::mutate(method = ifelse(method == "ggum_se_timeseries", "GGUM", "GGUM-RT")) %>%
  nest(-num_cats) %>%
  mutate(tukey=map(data,tukey_table1)) %>% 
  unnest(tukey)

# Item Discrimination: Test Length x Estimation Method
df %>%
  dplyr::filter(family == "a") %>%
  pivot_longer(cols = c("ggum_se_timeseries", "ggumrt_se_timeseries"), names_to = "method", values_to = "se") %>%
  dplyr::mutate(method = ifelse(method == "ggum_se_timeseries", "GGUM", "GGUM-RT")) %>%
  nest(-test_length) %>%
  mutate(tukey=map(data,tukey_table1)) %>% 
  unnest(tukey)

# for split plots with RMSD
tukey_table2=function(x) {
  x.1=aov(rmsd~method,data=x)
  tidy(TukeyHSD(x.1))
}

## num_cats, test_length, sample_size

# Theta: Test Length x Estimation Method
df %>%
  dplyr::filter(family == "b") %>%
  pivot_longer(cols = c("ggum_rmsd", "ggumrt_rmsd"), names_to = "method", values_to = "rmsd") %>%
  dplyr::mutate(method = ifelse(method == "ggum_rmsd", "GGUM", "GGUM-RT")) %>%
  nest(-test_length) %>%
  mutate(tukey=map(data,tukey_table2)) %>% 
  unnest(tukey)

# Theta: Response Categories x Estimation Method
df %>%
  dplyr::filter(family == "theta") %>%
  pivot_longer(cols = c("ggum_rmsd", "ggumrt_rmsd"), names_to = "method", values_to = "rmsd") %>%
  dplyr::mutate(method = ifelse(method == "ggum_rmsd", "GGUM", "GGUM-RT")) %>%
  nest(-num_cats) %>%
  mutate(tukey=map(data,tukey_table2)) %>% 
  unnest(tukey)

# Item Discrimination: Test Length x Estimation Method
df %>%
  dplyr::filter(family == "a") %>%
  pivot_longer(cols = c("ggum_rmsd", "ggumrt_rmsd"), names_to = "method", values_to = "rmsd") %>%
  dplyr::mutate(method = ifelse(method == "ggum_rmsd", "GGUM", "GGUM-RT")) %>%
  nest(-test_length) %>%
  mutate(tukey=map(data,tukey_table2)) %>% 
  unnest(tukey)
