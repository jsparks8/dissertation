library(rjags)
library(coda)
library(tidyverse)
library(tictoc)

# Packages required -----
packages <- c("tidyverse", "ggplot2", "openxlsx", "rstan", "tictoc", "gridExtra")

# Install packages not yet installed -----
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load packages -----
invisible(lapply(packages, library, character.only = TRUE))


source("~/R/dissertation/StanGGUM-master/JAGS testing/data.R")
source("StanGGUM-master/JAGS testing/Model Script.R")

model1.spec<-textConnection(model)

tic()
tic()
# Load model and run adaption iterations
jags <- jags.model(model1.spec, data=data_list, inits = starts,
                   n.chains=1)
toc(log = TRUE)

tic()
# Extra burn-in iterations
update(jags, n.iter=4000, by=100, progress.bar="text")
toc(log = TRUE)

tic()
# Retained iterations
samples <- coda.samples(
  jags,
  c('a', 'b', 'tau', 'theta'),
  5000
)
toc(log = TRUE)
toc(log = TRUE)


# store how long it took to run -----
log.txt <- tic.log(format = TRUE)

estimates <- summary(samples)$statistics %>%
  as.data.frame()%>%
  tibble::rownames_to_column() %>%
  dplyr::select(parameter = rowname, estimate = Mean)


# Specify working directory -----
starting.directory <- getwd()
fileName <- file.path(starting.directory, "2021-10-25_Jordan_GGUM_data.xlsx", fsep = "/")
date <- Sys.Date()
output_dir <- file.path(starting.directory, date, fsep = "/")

# Create output directory if it does not exist ----
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
} else {
  print("Dir already exists!")
}

a <- data.frame(truth = readWorkbook(fileName, sheet = "a", colNames = FALSE, sep = ",")[, 1]) %>%
  dplyr::mutate(
    parameter = paste0("a[", row_number(), "]"),
    family = "a"
  ) %>%
  relocate(parameter)

b <- data.frame(truth = readWorkbook(fileName, sheet = "b", colNames = FALSE, sep = ",")[, 1]) %>%
  dplyr::mutate(
    parameter = paste0("b[", row_number(), "]"),
    family = "b"
  ) %>%
  relocate(parameter)

tau <- as.data.frame(readWorkbook(fileName, sheet = "tau", colNames = FALSE, sep = ",")[, 1:4]) %>%
  tibble::rownames_to_column() %>%
  pivot_longer(!rowname, names_to = "parameter", values_to = "truth") %>%
  dplyr::mutate(
    parameter = paste0("tau[", rowname, ",", str_split(parameter, "X", simplify = TRUE)[, 2], "]"),
    family = "tau"
  ) %>%
  dplyr::select(-rowname)

theta <- data.frame(truth = readWorkbook(fileName, sheet = "theta", colNames = FALSE, sep = ",")[, 1]) %>%
  dplyr::mutate(
    parameter = paste0("theta[", row_number(), "]"),
    family = "theta"
  ) %>%
  relocate(parameter)

df <- estimates %>%
  dplyr::left_join(bind_rows(a, b, tau, theta), by = "parameter") %>%
  dplyr::mutate(
    estimate = round(estimate, digits = 4),
    truth = round(truth, digits = 4)
  )

# save comparison plot ----
g <- arrangeGrob(
  qplot(estimate, truth, data = (df %>% dplyr::filter(family == "a")), main = "a"),
  qplot(estimate, truth, data = (df %>% dplyr::filter(family == "b")), main = "b"),
  qplot(estimate, truth, data = (df %>% dplyr::filter(family == "tau")), main = "tau"),
  qplot(estimate, truth, data = (df %>% dplyr::filter(family == "theta")), main = "theta")
)

ggsave(paste0(
  output_dir, "/", date, "_GGUM1_Plot_JAGS.png"
), g)

# save environment ----
save.image(file = paste0(
  output_dir, "/", date, "_GGUM1_JAGS_Environment.RData"
))

# print plot for easy reference ----
grid.arrange(
  qplot(estimate, truth, data = (df %>% dplyr::filter(family == "a")), main = "a"),
  qplot(estimate, truth, data = (df %>% dplyr::filter(family == "b")), main = "b"),
  qplot(estimate, truth, data = (df %>% dplyr::filter(family == "tau")), main = "tau"),
  qplot(estimate, truth, data = (df %>% dplyr::filter(family == "theta")), main = "theta")
)

