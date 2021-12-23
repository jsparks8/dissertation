# Packages required -----
packages <- c("tidyverse", "ggplot2", "openxlsx", "rstan", "tictoc", "gridExtra")
library(bmggum)
# Install packages not yet installed -----
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load packages -----
invisible(lapply(packages, library, character.only = TRUE))

# Set `rstan` settings -----
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Specify working directory -----
starting.directory <- getwd()
date <- Sys.Date()
output_dir <- file.path(starting.directory, date, fsep = "/")

# Create output directory if it does not exist ----
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
} else {
  print("Dir already exists!")
}

# Specify output paths ----

fileName <- file.path(starting.directory, "2021-10-25_Jordan_GGUM_data.xlsx", fsep = "/")
stan_file <- file.path(starting.directory, "StanGGUM-master", "ggum_new.stan", fsep = "/")

# GGUM -----
resp <- readWorkbook(fileName, sheet = "resp", colNames = FALSE, sep = ",")
resp <- as.matrix(resp)
I <- dim(resp)[1]
J <- dim(resp)[2]
K <- 4 # number of categories
GGUMdata <- list(n_sub = I, n_item = J, K = K, r = resp)

# initial values  -----

initfun <- function() {
  list(
    a = as.list(rep(0.5, 20)),
    b = as.list(readWorkbook(fileName, sheet = "b", colNames = FALSE, sep = ",")[, 1]),
    tau = as.matrix(readWorkbook(fileName, sheet = "tau", colNames = FALSE, sep = ",")[, 1:4]),
    theta = as.list(readWorkbook(fileName, sheet = "theta", colNames = FALSE, sep = ",")[, 1])
  )
}

# Estimation specs -----
chains <- 1
iter <- 10000
warmup <- 5000

df <- GGUMdata[["r"]]

delindex <- c(1:20, rep(0,20))
delindex <- matrix(delindex,nrow = 2, byrow=TRUE)
ind <- c(rep(1,20))
ind <- t(ind)
tic("Additional iterations")

mod <- bmggum(GGUM.Data=df, delindex=delindex, trait=1, ind=ind, option=4,
              init=initfun, model="UM8", iter = iter, warmup = warmup,chains=1)
toc(log = TRUE)
# store how long it took to run -----
log.txt <- tic.log(format = TRUE)

write.table(slot(get_stanmodel(mod[["Fit"]]), "model_code"),
            file = paste0(output_dir, "/", date, "_ggum.stan"),
            sep = "\t",
            quote = F, col.names = FALSE,
            row.names = FALSE
)

# compare estimates to true values ----
estimates <- as.data.frame(summary(mod[["Fit"]])$summary) %>%
  tibble::rownames_to_column() %>%
  dplyr::select(parameter = rowname, estimate = mean)

a <- data.frame(truth = readWorkbook(fileName, sheet = "a", colNames = FALSE, sep = ",")[, 1]) %>%
  dplyr::mutate(
    parameter = paste0("alpha[", row_number(), "]"),
    family = "alpha"
  ) %>%
  relocate(parameter)

b <- data.frame(truth = readWorkbook(fileName, sheet = "b", colNames = FALSE, sep = ",")[, 1]) %>%
  dplyr::mutate(
    parameter = paste0("delta[", row_number(), "]"),
    family = "delta"
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
    parameter = paste0("theta[", row_number(), ",1]"),
    family = "theta"
  ) %>%
  relocate(parameter)

df <- estimates %>%
  dplyr::left_join(bind_rows(a, b, tau, theta), by = "parameter") %>%
  dplyr::mutate(
    estimate = round(estimate, digits = 4),
    truth = round(truth, digits = 4)
  )

# print plot for easy reference ----
grid.arrange(
  qplot(estimate, truth, data = (df %>% dplyr::filter(family == "alpha")), main = "a"),
  qplot(estimate, truth, data = (df %>% dplyr::filter(family == "delta")), main = "b"),
  qplot(estimate, truth, data = (df %>% dplyr::filter(family == "tau")), main = "tau"),
  qplot(estimate, truth, data = (df %>% dplyr::filter(family == "theta")), main = "theta")
)


# #check mod[["Fit"]]el convergence using r package "shinystan"
# library(shinystan)
# conv_check<-launch_shinystan(mod[["Fit"]])
#
# #check mod[["Fit"]]el convergence using r package "ggmcmc"
# library(ggmcmc)
# GGUM_conv<-ggs(mod[["Fit"]])
#
# #this generates a pdf file "ggmcmc-output.pdf"
# ggmcmc(GGUM_conv,family="beta_free")

