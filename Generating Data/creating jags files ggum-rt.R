# This file drops the starting values and priors into two JAGs files
# This file is sourced by 'GGUMRT_DATA_PREP.R'

jags_tau_priors <- openxlsx::readWorkbook(paste0(file_prefix, "DATASET.xlsx"), sheet="tau priors",
                                  colNames = FALSE) %>%
  as.matrix()

start_alpha <- openxlsx::readWorkbook(paste0(file_prefix, "ESTIMATES.xlsx"), sheet="comparison",
                                      colNames = FALSE) %>%
  dplyr::filter(X4 == "a") %>%
  dplyr::select(X2) %>%
  as.matrix()

start_delta <- openxlsx::readWorkbook(paste0(file_prefix, "ESTIMATES.xlsx"), sheet="comparison",
                                      colNames = FALSE) %>%
  dplyr::filter(X4 == "b") %>%
  dplyr::select(X2) %>%
  as.matrix()

start_tau <- openxlsx::readWorkbook(paste0(file_prefix, "ESTIMATES.xlsx"), sheet="comparison",
                                      colNames = FALSE) %>%
  dplyr::filter(X4 == "tau") %>%
  dplyr::select(X2) %>%
  as.matrix()%>%
  matrix(nrow=length(start_alpha), ncol=length(unique(data))) %>%
  as.data.frame()

start_theta <- openxlsx::readWorkbook(paste0(file_prefix, "ESTIMATES.xlsx"), sheet="comparison",
                                    colNames = FALSE) %>%
  dplyr::filter(X4 == "theta") %>%
  dplyr::select(X2) %>%
  as.matrix()

#infer parameters
num_people <- length(start_theta)
num_items <- length(start_alpha) 
num_cats <- length(unique(data))

# Create JAGS Model  file ----

file.create(paste0(file_prefix, "MODEL GGUM-RT.R"))
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
           con = paste0(file_prefix, "MODEL GGUM-RT.R"), sep = "\n", useBytes = FALSE)

next1 <- paste(
  '
  # RT stuff starts here

   rt_b0 ~ dnorm(4545,100); #rt_b0
   rt_b1 ~ dnorm(459,10); #rt_b1
   rt_b2 ~ dnorm(-31,1); #rt_b2
   e ~ dunif( 5.0E+2 , 1.0E+3 );
   
for (i in 1:N) {
 	for (j in 1:J) {
 	 			rt0[i,j] <- rt_b0 + rt_b1*pow((theta[i]-b[j]),2)+
 						rt_b2*pow(pow((theta[i]-b[j]),2),2);
 						
 						rt[i,j] ~ dnorm(rt0[i,j],1/(e^2))	;
						
						
 	}
 	}


# ending RT stuff
  
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

rt <- matrix(datart, nrow=', num_people, ', ncol=', num_items, ', byrow=TRUE)

N <-', num_people, '

J <-', num_items, '

K <-', num_cats, '

data_list <- list("r" = resp, "rt" = rt, "N" = N, "J" = J, "K" = K)

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
                     dplyr::mutate(V1 = NA),
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

#  rt_e[i] 
rt_b0=c(',
readr::format_delim(as.data.frame(rt_starts[,1])%>%round(digits=3),
                    delim=",",
                    eol=", 
                    ",
                    col_names = FALSE)%>%
  str_trim()%>%
  str_sub(.,1,nchar(.)-1),
'),

rt_b1=c(',
readr::format_delim(as.data.frame(rt_starts[,2])%>%round(digits=3),
                    delim=",",
                    eol=", 
                    ",
                    col_names = FALSE)%>%
  str_trim()%>%
  str_sub(.,1,nchar(.)-1),
'),

rt_b2=c(',
readr::format_delim(as.data.frame(rt_starts[,3])%>%round(digits=3),
                    delim=",",
                    eol=", 
                    ",
                    col_names = FALSE)%>%
  str_trim()%>%
  str_sub(.,1,nchar(.)-1),
'),

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
      file=paste0(file_prefix, "MODEL GGUM-RT.R"),append=TRUE)

