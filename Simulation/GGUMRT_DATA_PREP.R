#' PREPARE GGUM DATA FOR GGUM-RT SIM
#' 
#' @description 
#' This script collects results from the GGUM simulation and prepares 
#' them for the GGUM-RT SIM
#' 
#' @CELL the cell to be run (to eventually be passed from 'RUN_JAGS_GGUMRT.R')
#' @REP the replication to be run  (to eventually be passed from 'RUN_JAGS_GGUMRT.R')

CELL = 1
REP_ALL = c(1)

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

#for (reps in 1:length(REP_ALL)) {
  
 # REP <- REP_ALL[reps]

REP <- 1
  
  # Call appropriate directories
  starting.directory <- data.frame(folders = c(list.dirs(path="Simulation")))  %>%
    filter(str_detect(folders, paste0("CELL", !!CELL)) & 
             str_detect(folders, paste0("REP", !!REP, "$"))) %>%
    dplyr::pull()
  
  file_prefix <- paste0(starting.directory, "/", str_sub(starting.directory, 12), "_")
  
  print(file_prefix)
  
  source(paste0(file_prefix, "DATA.R"))
  
  rt_true <- openxlsx::readWorkbook(paste0(file_prefix, "DATASET.xlsx"), sheet="rt true",
                                            colNames = FALSE) %>%
    dplyr::mutate(ID = row_number())
  
  # specify parameters
  # N is sample size
  # J is number of items
  # K is response categories
  # D is number of dimensions
  theta_delta_sqd <- data.frame()
 for (N_use in 1:length(start_theta)){
    for(J_use in 1:length(start_delta)){
      theta <- start_theta[N_use]
      delta <- start_delta[J_use]
      temp <- data.frame(ID = N_use, Item = paste0("X", J_use),
                                    distance_sqd = (theta-delta)^2)
      theta_delta_sqd <- bind_rows(theta_delta_sqd, temp)
    }
 }
  
  rt_data <- rt_true %>% pivot_longer(cols=starts_with("X"), names_to="Item", values_to="RT") %>%
    dplyr::left_join(theta_delta_sqd, by=c("ID", "Item"))

      
      poly1 <- lm(RT ~ distance_sqd + I(distance_sqd^2), data=rt_data)
      
      VAR=sum(poly1$residuals**2)/(length(poly1$residuals)-1)
      
      rt_starts0 <- data.frame(ID = N_use,
                              B0 = poly1$coefficients[1], 
                              B1 = poly1$coefficients[2], 
                              B2 = poly1$coefficients[3],
                              E_PREC = 1/VAR)
  rt_starts<- rt_starts0 %>%
    dplyr::select(-ID) %>%
    as.matrix()

  # Save JAGS RT Data file ----
  rt_list <- readr::format_delim(rt_true %>% dplyr::select(-ID) %>% round(digits=2),
                                  delim=",",
                                  eol=",
                                
                                ",
                                col_names = FALSE)%>%
    str_trim()%>%
    str_sub(.,1,nchar(.)-1)
  
  file.create(paste0(file_prefix, "DATA_RT.R"))
  writeLines(strwrap(paste0("datart <- c(", rt_list, ")"), width=60), 
             con = paste0(file_prefix, "DATA_RT.R"), sep = "\n", useBytes = FALSE)
  
  source(paste0(file_prefix, "DATA_RT.R"))
  