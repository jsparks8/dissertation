## load libraries #----
library(tidyverse)
library(ggplot2)
library(GGUM)
library(psych)

## load data prepped in data_prep.R #----
data <- readRDS("data.rds")
questions <- readRDS("questions.rds")

## GGUM modeling #----
ggum_data <- data %>%
  dplyr::select(
    !!keep
  )

ggumdata <- data.matrix(ggum_data) - matrix(1, nrow =  nrow(ggum_data), ncol = ncol(ggum_data))

## Exporting to run in GGUM2004 #----
export.GGUM2004(ggumdata, data.file = "ggumdata", data.dir = paste(getwd()))
write.GGUM2004(I = ncol(ggum_data), C = 3, model = "GGUM", cmd.file = "ggumcmd", data.file = "ggumdata", data.dir = paste(getwd())) # change spurious comma to .#

## Modify the command file #----
cmd <- readLines('ggumcmd.txt')
cmd[16] <- "500 NUMBER OF OUTER CYCLES"
cmd <- cmd[1:20]
cmd <- c(cmd, "Y WANT TO PLOT",
"20 NUMBER OF PLOT GROUPS",
"200 NUMBER OF THETA-DELTA PAIR GROUPS",
"Y WANT FIT STATISTICS",
"20 NUMBER OF FIT GROUPS",
"N PRINT FIT FOR EVERY PERSON",
"2.576 ITEM T-VALUE CUTOFF",
"0.01 ITEM CHI-SQUARE PROBABILITY CUTOFF",
"3.291 PERSON T-VALUE CUTOFF",
"0.001 PERSON CHI-SQUARE PROBABILITY CUTOFF",
"2.576 PERSON LOCALIZED T-VALUE CUTOFF")

writeLines(cmd,"ggumcmd.txt")

# Run 'GGUM2004' #----
