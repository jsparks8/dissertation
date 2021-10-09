# load libraries

library(tidyverse)
library(ggplot2)
library(readxl)
library(GGUM)

# load merged data
data.raw <- readxl::read_excel(path = "6pt_attdata.xlsx")

# prep gun control dataset
data <- data.raw %>%
  dplyr::filter(Session == "1" & guncorr > 0.7) %>%
  dplyr::select(
    Subject,
    Gq1, Gq2, Gq3, Gq4, Gq5, Gq6, Gq7, Gq8, Gq9, Gq10,
    Gq11, Gq12, Gq13, Gq14, Gq15, Gq16, Gq17, Gq18, Gq19, Gq20,
    Gq21, Gq22, Gq23, Gq24, Gq25, Gq26, Gq27, Gq28, Gq29, Gq30,
    Gq31, Gq32, Gq33, Gq34, Gq35, Gq36, Gq37, Gq38, Gq39, Gq40,
    Gq41, Gq42, Gq43, Gq44, Gq45, Gq46, Gq47, Gq48, Gq49, Gq50,
    Gq51, Gq52, Gq53, Gq54, Gq55, Gq56, Gq57, Gq58, Gq59, Gq60,
    Gq61, Gq62, Gq63,
    RTG1, RTG2, RTG3, RTG4, RTG5, RTG6, RTG7, RTG8, RTG9, RTG10,
    RTG11, RTG12, RTG13, RTG14, RTG15, RTG16, RTG17, RTG18, RTG19, RTG20,
    RTG21, RTG22, RTG23, RTG24, RTG25, RTG26, RTG27, RTG28, RTG29, RTG30,
    RTG31, RTG32, RTG33, RTG34, RTG35, RTG36, RTG37, RTG38, RTG39, RTG40,
    RTG41, RTG42, RTG43, RTG44, RTG45, RTG46, RTG47, RTG48, RTG49, RTG50,
    RTG51, RTG52, RTG53, RTG54, RTG55, RTG56, RTG57, RTG58, RTG59, RTG60,
    RTG61, RTG62, RTG63
  ) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(Subject = as.numeric(row.names(.)))

# GGUM modeling
ggum_data <- data %>%
  dplyr::select(
    Gq1, Gq2, Gq3, Gq4, Gq5, Gq6, Gq7, Gq8, Gq9, Gq10,
    Gq11, Gq12, Gq13, Gq14, Gq15, Gq16, Gq17, Gq18, Gq19, Gq20,
    Gq21, Gq22, Gq23, Gq24, Gq25, Gq26, Gq27, Gq28, Gq29, Gq30,
    Gq31, Gq32, Gq33, Gq34, Gq35, Gq36, Gq37, Gq38, Gq39, Gq40,
    Gq41, Gq42, Gq43, Gq44, Gq45, Gq46, Gq47, Gq48, Gq49, Gq50,
    Gq51, Gq52, Gq53, Gq54, Gq55, Gq56, Gq57, Gq58, Gq59, Gq60,
    Gq61, Gq62, Gq63
  )

ggumdata <- data.matrix(ggum_data) - matrix(1, nrow = 852, ncol = 63)

# Exporting to run in GGUM2004
export.GGUM2004(ggumdata, data.file = "ggumdata", data.dir = "C:/Users/Jordan Sparks/Documents/attdiss")
write.GGUM2004(I = 63, C = 5, model = "GGUM", cmd.file = "ggumcmd", data.file = "ggumdata", data.dir = "C:/Users/Jordan Sparks/Documents/attdiss") # change spurious comma to .#

# Saving data for future use
saveRDS(data, file = "data.rds")
