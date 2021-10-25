data {
  int <lower=0> K;        // number of observed categories (equal to C + 1)
  int <lower=0> n_sub;    // number of subjects
  int <lower=0> n_item;   // number of items
  int <lower=1, upper=K> r[n_sub, n_item]; // array of responses
 }

parameters {
  vector[K-1]   tau_raw[n_item];
//  vector[K] tau[n_item];   // free step parameters parameter
  real b[n_item];          // item difficulty parameter
  real<lower=0, upper=4> a[n_item]; // item discrimination
  vector[n_sub] theta;     // latent trait
}


transformed parameters{
  
  vector[K] tau[n_item];
  
  for (i in 1:n_item){
    tau[i,1]=0;
  }
  
  for (d in 2:K){
    tau[,d]=tau_raw[,(d-1)];
  }
}


model {
  theta ~ normal(0, 1);
  b ~ normal(0, 2);  
  a ~ normal(0, 0.5);
  
    tau_raw[,1] ~ normal(-3, 2);
    tau_raw[,2] ~ normal(-2, 2);
    tau_raw[,3] ~ normal(-1, 2);


  for (i in 1:n_sub){
     for (j in 1:n_item) {
        //
    vector[K] prob;
    
    vector[K] nominator;
    
    for (k in 1:K) {
      
      nominator[k]=exp(a[j]*((k-1)*(theta[i]-b[j])-sum(tau[j, 1:k])))+
      exp(a[j]*(((2*K)-k)*(theta[i]-b[j])-sum(tau[j,1:k])));
      
    }
     
    prob=nominator/sum(nominator);
     
    r[i,j] ~ categorical(prob);
    //
     }
  }
  }

