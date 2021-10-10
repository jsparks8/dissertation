## load libraries #----

library(tidyverse)
library(ggplot2)
library(readxl)
library(GGUM)
library(psych)

## load merged data #----
data.raw <- readxl::read_excel(path = "6pt_attdata.xlsx")

## load question test #----
questions <- readxl::read_excel(path = "Attitude_Questions.xlsx") %>%
  unite("text", c(Statement1, Statement2, Statement3), na.rm=TRUE, sep = " ") %>%
  dplyr::mutate(Gun = paste0("Gq", Gun))
  

## prep gun control dataset #----
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
  dplyr::mutate(Subject = as.numeric(row.names(.))) %>%
  # Collapse 1-2 and 5-6 responses
  mutate(across(starts_with("Gq"),
                ~recode(., "1"=1, "2"= 2, "3"=2, "4"=3, "5"=3, "6"=4)))

## PCA to determine diminesality of the dataset #----
pc <- psych::principal(data[,c(2:64)], nfactors=2)
fa.parallel(data[,c(2:64)], fa="pc")

# Discard any items with a communality < 0.3
communalities <- data.frame(pc$communality) %>% rownames_to_column() %>%
  dplyr::mutate(removal_flag = case_when(
                  pc.communality < 0.3 ~ 1,
                  pc.communality >= 0.3 ~ 0,
                  TRUE ~ NA_real_
                    )
  )

keep <- communalities %>% 
  dplyr::filter(removal_flag == 0) %>%
  dplyr::pull(rowname)

table(communalities$removal_flag, useNA="ifany")

# 17/63 items seems like a lot to remove?
which <- communalities %>%
  dplyr::left_join(questions, by=c("rowname" = "Gun"))

## GGUM modeling #----
ggum_data <- data %>%
  dplyr::select(
    !!keep
  )

ggumdata <- data.matrix(ggum_data) - matrix(1, nrow =  nrow(ggum_data), ncol = ncol(ggum_data))

## Exporting to run in GGUM2004 #----
export.GGUM2004(ggumdata, data.file = "ggumdata", data.dir = paste(getwd()))
write.GGUM2004(I = ncol(ggum_data), C = 3, model = "GGUM", cmd.file = "ggumcmd", data.file = "ggumdata", data.dir = paste(getwd())) # change spurious comma to .#

# Run 'GGUM2004' #----
res.GGUM2004 <- run.GGUM2004(
  cmd.file = "ggumcmd",
  data.file = "ggumdata",
  datacmd.dir = paste(getwd()),
  prog.dir = "C:/Ggum")

# Examine item locations
locations <- data.frame(question = keep, delta = res.GGUM2004[["delta"]])
stripchart(locations$delta, pch = "+")

## Saving data for future use #----
saveRDS(data, file = "data.rds")
