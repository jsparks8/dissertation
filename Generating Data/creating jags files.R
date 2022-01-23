# This file drops the starting values and priors into two JAGs files
# This file is sourced by 'GGUM_SIM.R'

# Save JAGS Data file ----
rsp_list <- readr::format_delim(jags_rsp,
                                delim=",",
                                eol=",
                                
                                ",
                                col_names = FALSE)%>%
  str_trim()%>%
  str_sub(.,1,nchar(.)-1)

file.create(paste0(file_prefix, "_DATA.R"))
writeLines(strwrap(paste0("data <- c(", rsp_list, ")"), width=60), 
           con = paste0(file_prefix, "_DATA.R"), sep = "\n", useBytes = FALSE)

# Create JAGS Model  file ----

file.create(paste0(file_prefix, "_MODEL.R"))
jags_script <- paste('model <- "
model                                                                                                          
{  
       
# specify prior distributions   
                                                                                                           
for (j in 1:J) {		

	b[j] ~ dnorm(0,0.25);
	
	a[j] ~ dnorm(0,4);
	
	tau[j,1] <- 0;
	
}		

',
paste0(jags_tau_priors, collapse=" "))

writeLines(strwrap(jags_script, width=35), 
           con = paste0(file_prefix, "_MODEL.R"), sep = "\n", useBytes = FALSE)

next1 <- paste(
  '
  #specify model
for (i in 1:N) {				
  theta[i] ~ dnorm(0,1);
  for (j in 1:J) {	
    for (z in 1:K){
      cp[i,j,z]<- exp(a[j]*((z-1)*(theta[i]-b[j] )+ sum(tau[j,1:z])))+ exp(a[j]*(((2*K)-z)*(theta[i]-b[j] )+ sum(tau[j,1:z])));
    }
    den[i,j] <- sum(cp[i,j,1:K]);
    for (z in 1:K){
      prob[i,j,z] <- cp[i,j,z]/den[i,j];	
    } 
    r[i,j] ~ dcat(prob[i,j,]);	
  }
}
}   "          
 
# specify parameters
# N is sample size
# J is number of items
# K is response categories
# D is number of dimensions
                                  
resp <- matrix(data, nrow=', num_people, ', ncol=', num_items, ', byrow=TRUE)

N <-', num_people, '

J <-', num_items, '

K <-', num_cats, '

data_list <- list("r" = resp, "N" = N, "J" = J, "K" = K)

# specify starting values for all estimated parameters

starts <-  list(a=
c(',
paste0(start_alpha, collapse=", "),
'
),
b=c(',
readr::format_delim(as.data.frame(start_delta),
                          delim=",",
                          eol=", 
                    ",
                    col_names = FALSE)%>%
        str_trim()%>%
        str_sub(.,1,nchar(.)-1),
'),
tau=matrix(c(',
readr::format_delim(start_tau %>%
                     dplyr::mutate(tau_0 = NA),
                    delim=", ",
                    eol=", 
                    ",
                    col_names = FALSE,
                    na="NA")%>%
  str_trim()%>%
  str_sub(.,1,nchar(.)-1),
'), ncol=',
num_cats,

', nrow=',

num_items,

', byrow=TRUE),
theta = c(
',
readr::format_delim(as.data.frame(start_theta),
                    delim=", ",
                    eol=", 
                    ",
                    col_names = FALSE)%>%
  str_trim()%>%
  str_sub(.,1,nchar(.)-1),
'
)
)
'
)

write(next1,
      file=paste0(file_prefix, "_MODEL.R"),append=TRUE)

