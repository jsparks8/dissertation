library(ggplot2)
library(openxlsx)
###load Rstan package
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#set up working directory 
#setwd('')
##################################################################################

# GGUM --------------------------------------------------------------
fileName = "GGUMdata.xlsx"
resp <- readWorkbook(fileName, sheet="resp", colNames=FALSE, sep=",")
resp <- as.matrix(resp)
I <- dim(resp)[1]
J <- dim(resp)[2]
K<-4              #number of categories
GGUMdata <- list(n_sub =I, n_item=J, K=K, r=resp)

# initial values (not set)

#run 1 chain to diagnose whether the stan code works properly
GGUM = stan(file = "ggum.stan", data = GGUMdata, chains = 1, iter =50)

#run additional 2000 iterations with 3 chains
GGUM1 <- stan(fit=GGUM, data=GGUMdata, chains = 3, iter=2000)

print(GGUM1, par=c("a","b", "theta", "tau"))
traceplot(GGUM1, par=c("a","b", "tau","theta"))
summary(GGUM1)
write.csv(summary(GGUM1),"GGUM1_summary.csv")

#check model convergence using r package "shinystan"
library(shinystan)
conv_check<-launch_shinystan(GGUM1)

#check model convergence using r package "ggmcmc"
library(ggmcmc)
GGUM_conv<-ggs(GGUM1)
#this generates a pdf file "ggmcmc-output.pdf"
ggmcmc(GGUM_conv,family="beta_free")

