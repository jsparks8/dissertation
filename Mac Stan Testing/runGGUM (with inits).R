library(ggplot2)
library(openxlsx)
###load Rstan package
library(rstan)
library(tictoc)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(janitor)
library(tidyverse)
library(gridExtra)


#set up working directory 
#setwd('')
##################################################################################

# GGUM --------------------------------------------------------------
fileName = "Mac Stan Testing/Jordan_GGUM_data.xlsx"
resp <- readWorkbook(fileName, sheet="resp", colNames=FALSE, sep=",")
resp <- as.matrix(resp)
I <- dim(resp)[1]
J <- dim(resp)[2]
K<-4              #number of categories
GGUMdata <- list(n_sub =I, n_item=J, K=K, r=resp)

# initial values
initfun <- function(){
  list(
    a = as.list(rep(0.5,20)),
    b = as.list(readWorkbook(fileName, sheet="b", colNames=FALSE, sep=",")[,1]),
    tau = as.matrix(readWorkbook(fileName, sheet="tau", colNames=FALSE, sep=",")[,1:4]),
    theta = as.list(readWorkbook(fileName, sheet="theta", colNames=FALSE, sep=",")[,1])
  )
}

chains <- 3
iter <- 15000
warmup <- 10000

#run 1 chain to diagnose whether the stan code works properly
tic("Test (1 Chain, 50 Iterations)")
GGUM = stan(file = "Mac Stan Testing/ggum.stan", data = GGUMdata, init = initfun, chains = 1, iter =50)
toc(log = TRUE)

#run additional 2000 iterations with 3 chains
tic("Additional iterations")
GGUM1 <- stan(fit=GGUM, data=GGUMdata, init = initfun, chains = chains, iter=iter, warmup=warmup)
#GGUM1 <- stan(fit=GGUM, data=GGUMdata, init = initfun, chains = 3, iter=2000)
toc(log = TRUE)

#toc(log = TRUE)
log.txt <- tic.log(format = TRUE)
write.csv(summary(GGUM1),paste0(Sys.Date(), "_GGUM1_summary_", chains, 
"chain_", iter, "_bmggum.csv"))
saveRDS(GGUM1, paste0(Sys.Date(), "_GGUM1_summary_", chains, 
                      "chain_", iter, "_bmggum.csv"))

estimates <- as.data.frame(summary(GGUM1)$summary) %>%
  tibble::rownames_to_column() %>%
  dplyr::select(parameter=rowname, estimate = mean)

a <- data.frame(truth = readWorkbook(fileName, sheet="a", colNames=FALSE, sep=",")[,1]) %>%
  dplyr::mutate(parameter = paste0("a[", row_number(), "]"),
                family = "a") %>% relocate(parameter)

b <- data.frame(truth = readWorkbook(fileName, sheet="b", colNames=FALSE, sep=",")[,1]) %>%
  dplyr::mutate(parameter = paste0("b[", row_number(), "]"),
                family = "b") %>% relocate(parameter)

tau <- as.data.frame(readWorkbook(fileName, sheet="tau", colNames=FALSE, sep=",")[,1:4]) %>%
tibble::rownames_to_column() %>%
  pivot_longer(!rowname, names_to = "parameter", values_to = "truth") %>%
  dplyr::mutate(parameter= paste0("tau[", rowname, ",", str_split(parameter, "X", simplify = TRUE)[ , 2],"]"),
                family="tau") %>%
  dplyr::select(-rowname)

theta <- data.frame(truth = readWorkbook(fileName, sheet="theta", colNames=FALSE, sep=",")[,1]) %>%
  dplyr::mutate(parameter = paste0("theta[", row_number(), "]"),
                family="theta") %>% relocate(parameter)

df <- estimates %>% 
  dplyr::left_join(bind_rows(a, b, tau, theta), by="parameter") %>%
  dplyr::mutate(estimate = round(estimate, digits=4),
                truth = round(truth, digits = 4))

grid.arrange(qplot(estimate, truth, data = (df %>% dplyr::filter(family == "a")), main="a"),
             qplot(estimate, truth, data = (df %>% dplyr::filter(family == "b")), main="b"),
             qplot(estimate, truth, data = (df %>% dplyr::filter(family == "tau")), main="tau"),
             qplot(estimate, truth, data = (df %>% dplyr::filter(family == "theta")), main="theta"))

#save
g <- arrangeGrob(qplot(estimate, truth, data = (df %>% dplyr::filter(family == "a")), main="a"),
                 qplot(estimate, truth, data = (df %>% dplyr::filter(family == "b")), main="b"),
                 qplot(estimate, truth, data = (df %>% dplyr::filter(family == "tau")), main="tau"),
                 qplot(estimate, truth, data = (df %>% dplyr::filter(family == "theta")), main="theta"))
#generates g
ggsave(paste0(Sys.Date(), "_GGUM1_Plot_", chains, 
              "chain_", iter, "_bmggum.png"), g)

#write.csv(summary(GGUM1),"GGUM1_summary_2k.csv")

#print(GGUM1, par=c("a","b", "theta", "tau"))
#traceplot(GGUM1, par=c("a"))

#check model convergence using r package "shinystan"
#library(shinystan)
#conv_check<-launch_shinystan(GGUM1)

#check model convergence using r package "ggmcmc"
#library(ggmcmc)
#GGUM_conv<-ggs(GGUM1)

#this generates a pdf file "ggmcmc-output.pdf"
#ggmcmc(GGUM_conv,family="beta_free")

