data {
  int <lower=0> K;        // number of observed categories (equal to C + 1)
  int <lower=0> n_sub;    // number of subjects
  int <lower=0> n_item;   // number of items
  int <lower=1, upper=K> r[n_sub, n_item]; // array of responses
}

parameters {
  vector<lower=0>[K-1]   tau_raw[n_item];
  //  vector[K] tau[n_item];   // free step parameters parameter
  real b[n_item];          // item difficulty parameter
  real<lower=0, upper=4> a[n_item]; // item discrimination
  vector[n_sub] theta;     // latent trait
}

transformed parameters{
  
  vector<lower=0>[K] tau[n_item];
  
  for (i in 1:n_item){
    tau[i,1]=0;
  }
  
  for (k in 2:K){
    tau[,k]=tau_raw[,(k-1)];
  }
}


model {
  vector[K] prob[n_sub, n_item];
  real numerator[n_sub, n_item, K];
  real den[n_sub, n_item];
  
  theta ~ normal(1.2, 1);
  b ~ normal(0.3, 1);  
  a ~ normal(1.2, 0.5);
  
  tau_raw[,1] ~ normal(1, 1);
  tau_raw[,2] ~ normal(1, 1);
  tau_raw[,3] ~ normal(1, 1);
  
  
  for (j in 1:n_sub){
    
    for (i in 1:n_item) {
      
      for (k in 1:K) {
        
        // numerator[k]=exp(a[j]*((k-1)*(theta[i]-b[j])-sum(tau[j, 1:k])))+
        // exp(a[j]*(((2*K)-k)*(theta[i]-b[j])-sum(tau[j,1:k])));
        
            
      numerator[j, i, k]=exp(a[i]*((k-1)*(theta[j]-b[i])+sum(tau[i, 1:k])))+
      exp(a[i]*(((2*K)-k)*(theta[j]-b[i])+sum(tau[i,1:k])));
            
      }
      
      den[j, i] = sum(numerator[j, i, 1:K]);
      
      for (k in 1:K) {
        prob[j, i, k]=numerator[j, i, k]/den[j, i];
      }
      
      r[j, i] ~ categorical(prob[j, i, ]);
      
    }
  }
}
