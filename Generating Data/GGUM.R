# This script will generate item responses to the GGUM

library(tidyverse)

# Make sure values are reproducible
set.seed(1)

# Generating thetas
thetas <- rnorm(1000, mean=0, sd=1)

# Generating item locations
locations <- NA

 for (k in 1:20){
   locations[k] <-  -1 + (k-1)*0.1
  
 }

# Generating item discriminations
discriminations <- rnorm(20, mean=0, sd=1)

# Generating item thresholds
thresholds <- matrix(nrow = 20, ncol=8)

for (h in 1:20){
  thresholds[h,1]=0 
  set.seed(1*h)
  thresholds[h,2]=(-0.25 * runif(1))/2
  set.seed(2*h)
  thresholds[h,3]=-0.5 * runif(1) + thresholds[h,2]
  thresholds[h,4]=thresholds[h,3]-1
  thresholds[h,5]= -1 * thresholds[h,4]
  thresholds[h,6]= -1  * thresholds[h,3]
  thresholds[h,7]= -1 * thresholds[h,2]
  thresholds[h,8]= -1 * thresholds[h,1]
  }

responses <- data.frame()
# Using item and person parameters to generate responses
for (i in 1:length(thetas)){
  theta <- thetas[i]
  for (j in 1:length(locations)){
    a <- discriminations[j]
    b <- locations[j]
    
    num0 <- (exp(1)^(a*(0*(theta - b)+(thresholds[j,1])))) + 
      (exp(1)^(a*(7*(theta - b)+(thresholds[j,1]+thresholds[j,2]+thresholds[j,3]+thresholds[j,4]
                                           +thresholds[j,5]+thresholds[j,6]+thresholds[j,7]+thresholds[j,8]))))
    
    num1=(exp(1)^(a*(1*(theta - b)+(thresholds[j,1]+thresholds[j,2])))) + 
      (exp(1)^(a*(6*(theta - b)+(thresholds[j,1]+thresholds[j,2]+thresholds[j,3]+thresholds[j,4]
                                           +thresholds[j,5]+thresholds[j,6]+thresholds[j,7]))))
    
    num2=(exp(1)^(a*(2*(theta - b)+(thresholds[j,1]+thresholds[j,2]+thresholds[j,3])))) + 
      (exp(1)^(a*(5*(theta - b)+(thresholds[j,1]+thresholds[j,2]+thresholds[j,3]+thresholds[j,4]
                                           +thresholds[j,5]+thresholds[j,6]))))
    
    num3=(exp(1)^(a*(3*(theta - b)+(thresholds[j,1]+thresholds[j,2]+thresholds[j,3]+thresholds[j,4])))) + 
      (exp(1)^(a*(4*(theta - b)+(thresholds[j,1]+thresholds[j,2]+thresholds[j,3]+thresholds[j,4]
                                           +thresholds[j,5]))))
    
    denom=num0+num1+num2+num3
    p0=num0/denom #probabilities
    p1=num1/denom
    p2=num2/denom
    p3=num3/denom
    
set.seed(1*i*j)
    raw <- runif(1)
 rsp <- (raw > p0) + (raw > (p0+p1)) +(raw > (p0+p1+p2)) +(raw > (p0+p1+p2+p3))
 
 prob <- data.frame(theta = i, item = j, p0, p1, p2, p3, raw, rsp)
 
 responses <- bind_rows(responses, prob)
 
 
  }
}

