library(bmggum)

Data <- as.matrix(Jordan_GGUM_data)
deli <- matrix(nrow=2, ncol=20)
deli[1,] <- c(1:20)
deli[2,] <- c(rep(0,20))
ind <- c(rep(1,20))
ind <- t(ind)
cova <- c(0.70, -1.25)

mod <- bmggum(GGUM.Data=Data,
              delindex=deli,
              trait=1,
              ind=ind,
              option=4,
              iter=15000,
              chains=1,
              warmup = 10000)
library(tidyverse)
library(ggplot2)
library(gridExtra)

#mod$Cor.est

a_estimates <- as.data.frame(mod$Alpha.est) %>%
  tibble::rownames_to_column() %>%
  dplyr::select(parameter=rowname, estimate = mean) %>%
  dplyr::mutate(parameter = paste0("a[", parameter, "]"))

b_estimates <- as.data.frame(mod$Delta.est) %>%
  tibble::rownames_to_column() %>%
  dplyr::select(parameter=rowname, estimate = mean) %>%
  dplyr::mutate(parameter = paste0("b[", parameter, "]"))

tau_estimates <- as.data.frame(mod$Tau.est) %>%
  tibble::rownames_to_column() %>%
  dplyr::select(parameter=rowname, estimate = mean)

theta_estimates <- as.data.frame(mod$Theta.est) %>%
  tibble::rownames_to_column() %>%
  dplyr::select(parameter=rowname, estimate = mean)

estimates <- bind_rows(a_estimates, b_estimates, tau_estimates, theta_estimates)

fileName = "Jordan_GGUM_data.xlsx"

library(openxlsx)

a <- data.frame(truth = readWorkbook(fileName, sheet="a", colNames=FALSE, sep=",")[,1]) %>%
  dplyr::mutate(parameter = paste0("a[", row_number(), "]"),
                family = "a") %>% relocate(parameter)

b <- data.frame(truth = readWorkbook(fileName, sheet="b", colNames=FALSE, sep=",")[,1]) %>%
  dplyr::mutate(parameter = paste0("b[", row_number(), "]"),
                family = "b") %>% relocate(parameter)

tau <- as.data.frame(readWorkbook(fileName, sheet="tau", colNames=FALSE, sep=",")[,1:4]) %>%
  tibble::rownames_to_column() %>%
  pivot_longer(!rowname, names_to = "parameter", values_to = "truth") %>%
  dplyr::mutate(parameter= paste0("tau_raw[", rowname, ",", str_split(parameter, "X", simplify = TRUE)[ , 2],"]"),
                family="tau") %>%
  dplyr::select(-rowname)

theta <- data.frame(truth = readWorkbook(fileName, sheet="theta", colNames=FALSE, sep=",")[,1]) %>%
  dplyr::mutate(parameter = paste0("theta[", row_number(), ",1]"),
                family="theta") %>% relocate(parameter)

df <- estimates %>% 
  dplyr::left_join(bind_rows(a, b, tau, theta), by="parameter") %>%
  dplyr::mutate(estimate = round(estimate, digits=4),
                truth = round(truth, digits = 4))

library(gridExtra)
grid.arrange(qplot(estimate, truth, data = (df %>% dplyr::filter(family == "a")), main="a"),
             qplot(estimate, truth, data = (df %>% dplyr::filter(family == "b")), main="b"),
             qplot(estimate, truth, data = (df %>% dplyr::filter(family == "tau")), main="tau"),
             qplot(estimate, truth, data = (df %>% dplyr::filter(family == "theta")), main="theta"))

