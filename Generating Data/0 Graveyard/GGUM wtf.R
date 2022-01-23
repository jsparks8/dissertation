# This script will generate item responses to the GGUM

# Vary three factors: ----
# Total sample size: 500, 2000
# Number of items: 10, 30
# Number of response categories: 2, 6
# = 2x2x2 between-subjects design with 8 cells

# Need to simulate 10 datasets for each cell

# True Parameter Values ----
# Theta - Randomly sampled from a normal distribution
# Item discriminations - Randomly sampled w/ replacement from Thompson (2014)
# Item locations - Randomly sampled w/ replacement from Thompson (2014)
# Item thresholds - Randomly sampled w/ replacement from Thompson (2014)

# Divide estimates from Thompson (2014) into five intervals across the continuum
# Then equally sample

# RT Generation ----
# Average response time used to generate values and the amount of noise
# Based off descriptives using the real data example

library(tidyverse)
library(GGUM)
library(vegan) # DCA

# Read abortion estimates from Thompson (2014)
source("Generating Data/read_abortion_estimates.R")

# What am I sampling?
num_items <- 10 # c(10, 30)
num_people <- 500 # c(500, 2000)
num_cats <- 6 # c(2, 6)
reps_cell <- 10

# Make sure values are reproducible
set.seed(2)

# Generating thetas- randomly sampled from a normal distribution
thetas <- rnorm(num_people, mean = 0, sd = 1)

# Sample items with replacement
item_sample <- bind_rows(
  MMAPint1[sample(1:nrow(MMAPint1), num_items/5, replace=TRUE),], # Int 1
  MMAPint2[sample(1:nrow(MMAPint2), num_items/5, replace=TRUE),], # Int 2
  MMAPint3[sample(1:nrow(MMAPint3), num_items/5, replace=TRUE),], # Int 3
  MMAPint4[sample(1:nrow(MMAPint4), num_items/5, replace=TRUE),], # Int 4
  MMAPint5[sample(1:nrow(MMAPint5), num_items/5, replace=TRUE),]  # Int 5
) %>%
  # Now select cols for estimates acc to # of resp categories
  dplyr::select(var_real=var, ends_with(paste0("_", num_cats))) %>%
  dplyr::mutate(var_sim = row_number()) %>%
  dplyr::relocate(var_real, var_sim) %>% 
  rename_with(~str_remove(., paste0("_", num_cats)))
   
thresholds <- item_sample %>% dplyr::select(starts_with("tau")) %>%
  dplyr::mutate(
    tau6=0,
    tau7=-1*tau5,
    tau8=-1*tau4,
    tau9=-1*tau3,
    tau10=-1*tau2,
    tau11=-1*tau1
  ) %>%
  as.matrix()%>%
  unname()

responses <- data.frame()
# Using item and person parameters to generate responses
for (i in 1:length(thetas)) {
  theta <- thetas[i]
  for (j in 1:length(item_sample)) {
    a <- item_sample$alpha[j]
    b <- item_sample$delta[j]

    tausum_0 <- thresholds[j, 1]
    tausum_1 <- thresholds[j, 1] + thresholds[j, 2]
    tausum_2 <- thresholds[j, 1] + thresholds[j, 2] + thresholds[j, 3]
    tausum_3 <- thresholds[j, 1] + thresholds[j, 2] + thresholds[j, 3] + thresholds[j, 4]
    tausum_4 <- thresholds[j, 1] + thresholds[j, 2] + thresholds[j, 3] + thresholds[j, 4] + thresholds[j, 5]
    tausum_5 <- thresholds[j, 1] + thresholds[j, 2] + thresholds[j, 3] + thresholds[j, 4] + thresholds[j, 5] + thresholds[j, 6]
    
    num0 <- (exp(1)^(a * (0 * (theta - b) + tausum_0))) +
      (exp(1)^(a * (11 * (theta - b) + tausum_0)))

    num1 <- (exp(1)^(a * (1 * (theta - b) + tausum_1))) +
      (exp(1)^(a * (10 * (theta - b) + tausum_1)))

    num2 <- (exp(1)^(a * (2 * (theta - b) + tausum_2))) +
      (exp(1)^(a * (9 * (theta - b) + tausum_2)))

    num3 <- (exp(1)^(a * (3 * (theta - b) + tausum_3))) +
      (exp(1)^(a * (8 * (theta - b) + tausum_3)))
    
    num4 <- (exp(1)^(a * (4 * (theta - b) + tausum_4))) +
      (exp(1)^(a * (7 * (theta - b) + tausum_4)))
    
    num5 <- (exp(1)^(a * (5 * (theta - b) + tausum_5))) +
      (exp(1)^(a * (6 * (theta - b) + tausum_5)))

    denom <- num0 + num1 + num2 + num3 + num4 + num5
    p0 <- num0 / denom # probabilities
    p1 <- num1 / denom
    p2 <- num2 / denom
    p3 <- num3 / denom
    p4 <- num4 / denom
    p5 <- num5 / denom

    set.seed(1 * i * j)
    raw <- runif(1)
    rsp <- (raw > p0) + 
      (raw > (p0 + p1)) + 
      (raw > (p0 + p1 + p2)) + 
      (raw > (p0 + p1 + p2 + p3)) + 
      (raw > (p0 + p1 + p2 + p3 + p4)) + 
      (raw > (p0 + p1 + p2 + p3 + p4 + p5))

    # Generating response times
    rt <- (7500 - (abs(1 - abs(theta - b)) * .75)) + (500 * (-1 + (1 - (-1)) * runif(1)))

    prob <- data.frame(id = i, item = j, p0, p1, p2, p3, raw, rsp, rt)

    responses <- bind_rows(responses, prob)
  }
}

table(responses$rsp)
hist(responses$rsp)

#---- Now obtain starting values ----

# Starting Values ---
# Starting values for item locations and thetas will be obtained by DCA
# and retaining first dimension

data <- responses %>% 
  dplyr:: select(id, item, rsp) %>%
  dplyr::mutate(rsp = rsp+1) %>%
  tidyr::pivot_wider(id_cols=id, names_from=item, names_prefix="Q", values_from=rsp) %>%
  dplyr::select(-id)

## DCA ##
emo.dca <- decorana(data, ira=0)  # Runs the DCA
emo.dca                           # Outputs eigenvalues

# Save DCA starts
start_delta <- scores(emo.dca, display="species")[,1]
start_theta <- scores(emo.dca, display="sites")[,1]
start_alpha <- rep(1, num_items)

# Thresholds will be obtained using regression equation from King (2017):
# Taui0 = 0
# O_i=1.002+0.449|δ ̃_i |-0.093V_i	
# ∆_i=0.921+0.058|δ ̃_i |-0.129V_i	
# where:
# V_i = the number of response categories for the ith item,
# δ ̃_i = the starting value of the ith item on the latent continuum.
#The initial values for the kth threshold of the ith item will then be calculated 
# from the following equation:
#  τ ̃_ik=O_i+∆_i (C_i-k_i )	

start_tau0 <- data.frame()
for (k in 1:num_cats-1){
for (j in 1:length(item_sample)) {
O_i <- 1.002+ 0.449*abs(start_delta[j]) - 0.093*num_cats
DELTA_i <- 0.921 + 0.058*abs(start_delta[j]) - 0.129*num_cats
k_i = k
i=j
start_tau0 <- bind_rows(start_tau0, data.frame(i=i, O_i = O_i, DELTA_i = DELTA_i, k_i = k_i))  }
}

start_tau <- start_tau0 %>%
  dplyr::mutate(C = num_cats-1,
                start_tau = O_i + DELTA_i*(C - k_i),
                start_tau = case_when(
                  start_tau < 0.1 ~ 0.1,
                  start_tau >= 0.1 ~ start_tau
                )) %>%
  dplyr::select(i, k_i, start_tau) %>%
  pivot_wider(id_cols=i, names_from=k_i, names_prefix="tau_", values_from=start_tau)%>%
  dplyr::mutate(tau_0 = 0) %>%
  dplyr::relocate(tau_0) %>%
  dplyr::select(-i)

# Generating tau priors ----
tau_priors <- start_tau0 %>%
  dplyr::mutate(C = num_cats-1,
                start_tau = O_i + DELTA_i*(C - k_i),
                start_tau = case_when(
                  start_tau < 0.1 ~ 0.1,
                  start_tau >= 0.1 ~ start_tau
                ),
                tau_prior = log(start_tau) - 0.5) %>%
  pivot_wider(id_cols=i, names_from=k_i, names_prefix="tau_", values_from=tau_prior)%>%
  dplyr::mutate(tau_0 = 0) %>%
  dplyr::relocate(tau_0) %>%
  dplyr::select(-i)

# Format data to use in stan
stan_rsp <- responses %>%
  dplyr::select(id, item, rsp, rt) %>%
  dplyr::mutate(rsp = rsp + 1)%>%
  tidyr::pivot_wider(values_from=c(rsp, rt), names_from=item) %>%
  dplyr::select(starts_with("rsp"))  %>%
  mutate(across(everything(), as.numeric))

stan_b <- item_sample$delta
stan_a <- item_sample$alpha

stan_theta <- thetas

stan_tau <- thresholds[,1:num_cats]

#tau[ 1,2] ~ dlnorm( 1.098,1);
jags_tau_priors <- tau_priors %>%
  dplyr::mutate(item = row_number(),
                tau_0 = NA) %>%
  tidyr::pivot_longer(!item, names_to = "index", values_to = "prior") %>%
  dplyr::mutate(index = as.numeric(str_remove(index, "tau_"))+1,
                save = paste0("tau[", item, ",", index, "] ~ dlnorm(", prior,
                              ", 1);")
  )%>%
  drop_na()%>%
  pull(save)
  
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "resp")
openxlsx::addWorksheet(wb, "b true")
openxlsx::addWorksheet(wb, "a true")
openxlsx::addWorksheet(wb, "tau true")
openxlsx::addWorksheet(wb, "theta true")
openxlsx::addWorksheet(wb, "b start")
openxlsx::addWorksheet(wb, "a start")
openxlsx::addWorksheet(wb, "tau start")
openxlsx::addWorksheet(wb, "theta start")
openxlsx::addWorksheet(wb, "tau priors")

openxlsx::writeData(wb, "resp", stan_rsp, colNames=FALSE, rowNames=FALSE)
openxlsx::writeData(wb, "b true", stan_b, colNames=FALSE, rowNames=FALSE)
openxlsx::writeData(wb, "a true", stan_a, colNames=FALSE, rowNames=FALSE)
openxlsx::writeData(wb, "tau true", stan_tau, colNames=FALSE, rowNames=FALSE)
openxlsx::writeData(wb, "theta true", stan_theta, colNames=FALSE, rowNames=FALSE)

openxlsx::writeData(wb, "b start", start_delta, colNames=FALSE, rowNames=FALSE)
openxlsx::writeData(wb, "a start", start_alpha, colNames=FALSE, rowNames=FALSE)
openxlsx::writeData(wb, "tau start", start_tau, colNames=FALSE, rowNames=FALSE)
openxlsx::writeData(wb, "theta start", start_theta, colNames=FALSE, rowNames=FALSE)

openxlsx::writeData(wb, "tau priors", jags_tau_priors, colNames=FALSE, rowNames=FALSE)

openxlsx::saveWorkbook(wb, file = paste(Sys.Date(), "GGUM data.xlsx"), overwrite=FALSE)
