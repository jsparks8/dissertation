library(rjags)

source("~/R/dissertation/StanGGUM-master/JAGS testing/data.R")
source("StanGGUM-master/JAGS testing/Model Script.R")

model1.spec<-textConnection(model)

jags <- jags.model(model1.spec, data=data_list, inits = starts,
                   n.chains=10)
