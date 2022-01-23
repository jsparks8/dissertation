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
library(stringr)

# Read abortion estimates from Thompson (2014)
source("Generating Data/read_abortion_estimates.R")

# What am I sampling?
num_items_sim <- c(10, 30)
num_people_sim <- c(500, 2000)
num_cats_sim <- c(6, 2)
reps_cell_sim <- c(1:10)
cell <- 1
# Loop starts here # 

for (people_iterate in 1:length(num_people)){
  for (cats_iterate in 1:length(num_cats)){
for (item_iterate in 1:length(num_items)){
      for (rep_iterate in 1:length(reps_cell)){
        print(paste0("CELL", cell, "_CAT", num_cats_sim[cats_iterate], "_I", 
               num_items_sim[item_iterate], "_N", num_people_sim[people_iterate],
               "_REP", reps_cell_sim[rep_iterate]))
        
        file_prefix <- paste0("Simulation/CELL", cell, "_CAT", num_cats_sim[cats_iterate], "_I", 
                              num_items_sim[item_iterate], "_N", num_people_sim[people_iterate],
                              "_REP", reps_cell_sim[rep_iterate],
                              "/CELL", cell, "_CAT", num_cats_sim[cats_iterate], "_I", 
                              num_items_sim[item_iterate], "_N", num_people_sim[people_iterate],
                              "_REP", reps_cell_sim[rep_iterate])
        
        num_items <- num_items_sim[item_iterate]
        num_people <-num_people_sim[people_iterate]
        num_cats <- num_cats_sim[cats_iterate]
        reps_cell <- reps_cell_sim[rep_iterate]
        
        # Make sure values are reproducible
        set.seed(num_items+num_people+num_cats+reps_cell)
        
        
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
            b0 <- 4500
            b1 <- 350
            b2 <- -14
            
            Dist <- theta-b
            Dist_sqd <- (theta-b)^2
            
            rt <- (b0 + 
                     b1*Dist_sqd + 
                     b2*(Dist_sqd^2)) + rnorm(1, mean=0, sd=2800)
            
            rt <- ifelse(rt < 200, 200, rt)
            
            
            prob <- data.frame(id = i, item = j, p0, p1, p2, p3, raw, rsp, rt)
            
            responses <- bind_rows(responses, prob)
          }
        }
        
        print(hist(responses$rsp, 
                   main=paste0("CELL", cell, "_CAT", num_cats_sim[cats_iterate], "_I", 
                                              num_items_sim[item_iterate], "_N", num_people_sim[people_iterate],
                                              "_REP", reps_cell_sim[rep_iterate])))
        
        
        
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
        
# Create the excel file
        source("Generating Data/creating excel dataset.R")
        
        # Create the JAGs files
        source("Generating Data/creating jags files.R")
        
      }
  cell <- cell+1
}
  }
}


#table(responses$rsp)
