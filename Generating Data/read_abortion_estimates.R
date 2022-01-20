# This file will read in five intervals of abortion attitude item
# estimates derived from a unidimensional GGUM by Thompson (2014)

library(readr)
MMAPint1 <- read_table("Generating Data/MMAPest/MMAPint1.dat", 
                       col_names = 
                         c("var", 
                           "delta_6", "alpha_6", "tau0_6", "tau1_6", "tau2_6", "tau3_6", "tau4_6", "tau5_6", 
                           "delta_5", "alpha_5", "tau0_5", "tau1_5", "tau2_5", "tau3_5", "tau4_5",
                           "delta_4", "alpha_4", "tau0_4", "tau1_4", "tau2_4", "tau3_4", 
                           "delta_3", "alpha_3", "tau0_3", "tau1_3", "tau2_3", 
                           "delta_2", "alpha_2", "tau0_2", "tau1_2"), col_types = cols())

MMAPint2 <- read_table("Generating Data/MMAPest/MMAPint2.dat", 
                       col_names = 
                         c("var", 
                           "delta_6", "alpha_6", "tau0_6", "tau1_6", "tau2_6", "tau3_6", "tau4_6", "tau5_6", 
                           "delta_5", "alpha_5", "tau0_5", "tau1_5", "tau2_5", "tau3_5", "tau4_5",
                           "delta_4", "alpha_4", "tau0_4", "tau1_4", "tau2_4", "tau3_4", 
                           "delta_3", "alpha_3", "tau0_3", "tau1_3", "tau2_3", 
                           "delta_2", "alpha_2", "tau0_2", "tau1_2"), col_types = cols())

MMAPint3 <- read_table("Generating Data/MMAPest/MMAPint3.dat", 
                       col_names = 
                         c("var", 
                           "delta_6", "alpha_6", "tau0_6", "tau1_6", "tau2_6", "tau3_6", "tau4_6", "tau5_6", 
                           "delta_5", "alpha_5", "tau0_5", "tau1_5", "tau2_5", "tau3_5", "tau4_5",
                           "delta_4", "alpha_4", "tau0_4", "tau1_4", "tau2_4", "tau3_4", 
                           "delta_3", "alpha_3", "tau0_3", "tau1_3", "tau2_3", 
                           "delta_2", "alpha_2", "tau0_2", "tau1_2"), col_types = cols())

MMAPint4 <- read_table("Generating Data/MMAPest/MMAPint4.dat", 
                       col_names = 
                         c("var", 
                           "delta_6", "alpha_6", "tau0_6", "tau1_6", "tau2_6", "tau3_6", "tau4_6", "tau5_6", 
                           "delta_5", "alpha_5", "tau0_5", "tau1_5", "tau2_5", "tau3_5", "tau4_5",
                           "delta_4", "alpha_4", "tau0_4", "tau1_4", "tau2_4", "tau3_4", 
                           "delta_3", "alpha_3", "tau0_3", "tau1_3", "tau2_3", 
                           "delta_2", "alpha_2", "tau0_2", "tau1_2"), col_types = cols())

MMAPint5 <- read_table("Generating Data/MMAPest/MMAPint5.dat", 
                       col_names = 
                         c("var", 
                           "delta_6", "alpha_6", "tau0_6", "tau1_6", "tau2_6", "tau3_6", "tau4_6", "tau5_6", 
                           "delta_5", "alpha_5", "tau0_5", "tau1_5", "tau2_5", "tau3_5", "tau4_5",
                           "delta_4", "alpha_4", "tau0_4", "tau1_4", "tau2_4", "tau3_4", 
                           "delta_3", "alpha_3", "tau0_3", "tau1_3", "tau2_3", 
                           "delta_2", "alpha_2", "tau0_2", "tau1_2"), col_types = cols())
