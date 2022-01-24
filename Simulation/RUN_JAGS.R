#' RUN GGUM IN JAGS
#' 
#' @description 
#' This script runs JAGs for the GGUM portion of the simulation and outputs results
#' Requires user to enter cell+rep to run
#' 
#' @CELL the cell to be run
#' @REP the replication to be run

CELL = 4
REP = 10

# Packages required -----
packages <- c("tidyverse", "ggplot2", "openxlsx", "rjags", "coda", "tictoc", "gridExtra", "R2jags",
              "openxlsx", "stringr")

# Install packages not yet installed -----
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load packages -----
invisible(lapply(packages, library, character.only = TRUE))

# Call appropriate directories
starting.directory <- data.frame(folders = c(list.dirs(path="Simulation")))  %>%
  filter(str_detect(folders, paste0("CELL", !!CELL)) & 
           str_detect(folders, paste0("REP", !!REP, "$"))) %>%
  dplyr::pull()

file_prefix <- paste0(starting.directory, "/", str_sub(starting.directory, 12), "_")

source(paste0(file_prefix, "DATA.R"))
source(paste0(file_prefix, "MODEL.R"))

model1.spec<-textConnection(model)

tic()
tic()
# Load model and run adaption iterations
jags <- jags.model(model1.spec, data=data_list, inits = starts,
                   n.chains=3)
toc(log = TRUE)

tic()
# Extra burn-in iterations
update(jags, n.iter=9000, by=100, progress.bar="text")
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
fileName <- file.path(file_prefix, "DATASET.xlsx", fsep = "")
output_dir <- file.path(starting.directory, fsep = "/")

# Create output directory if it does not exist ----
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
} else {
  print("Dir already exists!")
}

a <- data.frame(truth = readWorkbook(fileName, sheet = "a true", colNames = FALSE, sep = ",")[, 1]) %>%
  dplyr::mutate(
    parameter = paste0("a[", row_number(), "]"),
    family = "a"
  ) %>%
  relocate(parameter)

b <- data.frame(truth = readWorkbook(fileName, sheet = "b true", colNames = FALSE, sep = ",")[, 1]) %>%
  dplyr::mutate(
    parameter = paste0("b[", row_number(), "]"),
    family = "b"
  ) %>%
  relocate(parameter)

tau <- as.data.frame(readWorkbook(fileName, sheet = "tau true", colNames = FALSE, sep = ",")[, 1:6]) %>%
  tibble::rownames_to_column() %>%
  pivot_longer(!rowname, names_to = "parameter", values_to = "truth") %>%
  dplyr::mutate(
    parameter = paste0("tau[", rowname, ",", str_split(parameter, "X", simplify = TRUE)[, 2], "]"),
    family = "tau"
  ) %>%
  dplyr::select(-rowname)

theta <- data.frame(truth = readWorkbook(fileName, sheet = "theta true", colNames = FALSE, sep = ",")[, 1]) %>%
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

# save estimates ----

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "estimates")
openxlsx::addWorksheet(wb, "comparison")

openxlsx::writeData(wb, "resp", jags_rsp, colNames=FALSE, rowNames=FALSE)

openxlsx::saveWorkbook(wb, file = paste0(file_prefix, "_ESTIMATES.xlsx"), overwrite=TRUE)

# save comparison plot ----
g <- arrangeGrob(
  qplot(estimate, truth, data = (df %>% dplyr::filter(family == "a")), main = "a"),
  qplot(estimate, truth, data = (df %>% dplyr::filter(family == "b")), main = "b"),
  qplot(estimate, truth, data = (df %>% dplyr::filter(family == "tau")), main = "tau"),
  qplot(estimate, truth, data = (df %>% dplyr::filter(family == "theta")), main = "theta")
)

ggsave(paste0(
  file_prefix, "comparison_plot.png"
), g)

# save environment ----
save.image(file = paste0(
  file_prefix, "Environment.RData"
))

# save trace plots --- 
pdf(paste0(
  file_prefix, "trace_plot.pdf"
))  
R2jags::traceplot(samples)
dev.off()		

# save autocorrelation plots ---
pdf(paste0(
  file_prefix, "autocorrelation_plot.pdf"
))
coda::autocorr.plot(samples)
dev.off()

# save timer ---
sink(paste0(
  file_prefix, "tictoc.txt"
))
tic.log(format = TRUE)
Sys.time()
sink(file=NULL)

# save Gelman-Rubin index ---
sink(paste0(
  file_prefix, "gelman.txt"
))
gelman.diag(samples, multivariate = FALSE)
sink(file=NULL)
sink(file=NULL)

# print plot for easy reference ----
grid.arrange(
  qplot(estimate, truth, data = (df %>% dplyr::filter(family == "a")), main = "a"),
  qplot(estimate, truth, data = (df %>% dplyr::filter(family == "b")), main = "b"),
  qplot(estimate, truth, data = (df %>% dplyr::filter(family == "tau")), main = "tau"),
  qplot(estimate, truth, data = (df %>% dplyr::filter(family == "theta")), main = "theta")
)

print(Sys.time())
