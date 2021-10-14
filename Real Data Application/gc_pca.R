## load libraries #----
library(tidyverse)
library(ggplot2)
library(readxl)
library(GGUM)
library(psych)

## load data prepped in data_prep.R #----
data <- readRDS("data.rds")
questions <- readRDS("questions.rds")

## PCA to determine dimensionality of the dataset #----
#fa.parallel(data[,c(2:64)], fa="pc")
pc <- psych::principal(data[,c(2:64)], nfactors=2)

# Discard any items with a communality < 0.3
communalities <- data.frame(pc$communality) %>% rownames_to_column() %>%
  dplyr::mutate(removal_flag = case_when(
    pc.communality < 0.3 ~ 1,
    pc.communality >= 0.3 ~ 0,
    TRUE ~ NA_real_
  )
  )

rc <- pc$loadings[1:63, 1:2]

plot(rc)

keep <- communalities %>% 
 # dplyr::filter(removal_flag == 0) %>%
  dplyr::pull(rowname)

table(communalities$removal_flag, useNA="ifany")

# 16/63 items seems like a lot to remove?
which <- communalities %>%
  dplyr::left_join(questions, by=c("rowname" = "Gun"))

# Remove items closest to center of circumplex - likely 34, 28, 43 (investigate)